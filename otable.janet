(defn- entry/take
  ```
  Take the specified entry out of the linked list, detaching it from any
  adjacent entries and updating the table's head/tail pointers.
  ```
  [tbl entry]
  # Retrieve the previous and next entries in the table.
  (def [previous next]
    [(entry :previous)
     (entry :next)])

  # Detach this entry from the previous one.
  (when previous
    (put entry :previous nil)
    (put previous :next next))

  # Detach this entry from the next one.
  (when next
    (put entry :next nil)
    (put next :previous previous))

  # Remove the entry from the head or tail of the list.
  (if (= entry (tbl :first))
    (put tbl :first next))
  (if (= entry (tbl :last))
    (put tbl :last previous))

  entry)

(defn- entry/put
  ```
  Put an entry relative to another entry in the linked list, updating the
  appropriate adjacent pointers and updating the table's head/tail pointers.
  ```
  [tbl entry &keys {:before before :after after}]
  (cond
    # Add this entry directly before another.
    before
    (let [previous (before :previous)]
      # Connect the preceding entry to this one.
      (when previous
        (put previous :next entry)
        (put entry :previous previous))

      # Connect this entry to the following one.
      (put entry :next before)
      (put before :previous entry)

      # When being placed before the first entry, this entry is now first.
      (if (= before (tbl :first))
        (put tbl :first entry)))

    # Add this entry directly after another.
    after
    (let [next (after :next)]
      # Connect this entry to the following one.
      (when next
        (put next :previous entry)
        (put entry :next next))

      # Connect the preceding entry to this one.
      (put after :next entry)
      (put entry :previous after)

      # When being placed after the final entry, this entry is now last.
      (if (= after (tbl :last))
        (put tbl :last entry)))

    # If this function is not being called with :before or :after, it is assumed
    # that this entry is being added as the first entry in the table.
    (each i [:first :last]
      (put tbl i entry)))

  entry)

(def OTable
  ```
  Prototype defining methods used to manipulate the contents of an ordered table.
  ```
  @{:_name "OTable"})

(defn otable?
  ```
  Check if x is an ordered table.
  ```
  [x]
  (var [result proto] [false x])
  (if (table? x)
    (while (set proto (table/getproto proto))
      (if (set result (= proto OTable))
        (break))))
  result)

(defn- otable/put
  ```
  Add the provided key and value to the table. By default, if the key already
  exists in the table, the value will replace the existing value; otherwise,
  the key/value pair will be appended to the end of the table.

  The behavior of this method may be configured with the following options:

    :before   If the value of this option is a key contained in the table, place
              new entry directly before this key.

    :after    If the value of this option is a key contained in the table, place
              new entry directly after this key.

    :prepend  If truthy, place the entry at the beginning of the table instead
              of the end.

    :reorder  If truthy, and the key already exists in the table, its entry will
              be repositioned as if being added as a new entry, instead of being
              updated in-place.
  ```
  [tbl k v &keys {:before before :after after :prepend prepend :reorder reorder}]
  # Nil or NaN keys cannot be added to a table.
  (if (or (nil? k) (nan? k)) (break))

  # False is a valid table key, so ordinary truthiness checks will not work.
  (def [before? after? entries]
    [(not (or (nil? before) (nan? before)))
     (not (or (nil? after) (nan? after)))
     (get tbl :entries @{})])

  # Allow the values of before and after to be modified.
  (var [before after]
    [(if before? before)
     (if after? after)])

  # This is done as otherwise the behavior would be arbitrary.
  (if (and before? after?)
    (error "options :before and :after must not be specified at the same time"))

  # Retrieve the entry from the table if it exists.
  (var entry (entries k))
  (def exists (and entry (not reorder)))

  # If the value is nil or the entry is being reordered, remove it from the
  # entry list. In the case of being reordered, it will be added later.
  (if (and entry (or reorder (nil? v)))
    (entry/take tbl entry))

  # If the value is nil, remove the entry from the entry table and return early.
  (when (nil? v)
    (put entries k nil)
    (break tbl))

  # Determine the position in the list to insert the entry at.
  (unless exists
    (cond
      before? (set before (entries before))
      after? (set after (entries after)))
    (if-not (or before after)
      (if prepend
        (set before (tbl :first))
        (set after (tbl :last)))))

  # Set the entry's key and value to the new value. This creates a new entry
  # object if one does not already exist.
  (if-not entry
    (set entry @{:kv [k v]})
    (put entry :kv [k v]))

  # If the entry was not already present within the list, or :reorder is true,
  # add the entry into the list at the appropriate position.
  (if-not exists
    (entry/put tbl entry :before before :after after))

  # Add the entry to the entry table, and add the entry table to the main table.
  (put entries k entry)
  (put tbl :entries entries))

(defn- otable/get
  ```
  Get the value corresponding to the provided key, or dflt if no value is found.
  ```
  [tbl key &opt dflt]
  (if-let [entries (tbl :entries)
           entry (entries key)
           kv (entry :kv)]
    (kv 1) dflt))

(defn- otable/each-entry
  ```
  Evaluate body for each entry within the ordered table. Returns nil.
  ```
  [x tbl body item]
  (with-syms [$tbl entry]
    ~(do
       (def ,$tbl ,tbl)
       (,assert (,otable? ,$tbl) "expected tbl to have OTable prototype")
       (var ,entry (,$tbl :first))
       (while ,entry
         (def ,x
           ,(case item
              :pair ~(,entry :kv)
              :key ~((,entry :kv) 0)
              :value ~((,entry :kv) 1)))
         ,;body
         (set ,entry (,entry :next))))))

(defmacro otable/each
  ```
  Evaluate body for each [k v] pair within the ordered table. Returns nil.
  ```
  [x tbl & body]
  (otable/each-entry x tbl body :pair))

(defmacro otable/eachk
  ```
  Evaluate body for each key within the ordered table. Returns nil.
  ```
  [x tbl & body]
  (otable/each-entry x tbl body :key))

(defmacro otable/eachv
  ```
  Evaluate body for each value within the ordered table. Returns nil.
  ```
  [x tbl & body]
  (otable/each-entry x tbl body :value))

(defn- otable/kvs
  ```
  Return an array of the ordered table's keys and values.
  ```
  [tbl]
  (def result @[])
  (otable/each pair tbl (array/concat result pair))
  result)

(defn- otable/keys
  ```
  Return an array of the ordered table's keys.
  ```
  [tbl]
  (def result @[])
  (otable/eachk key tbl (array/push result key))
  result)

(defn- otable/values
  ```
  Return an array of the ordered table's keys.
  ```
  [tbl]
  (def result @[])
  (otable/eachv value tbl (array/push result value))
  result)

(defn- otable/pairs
  ```
  Return an array of the ordered table's keys and values, paired into tuples.
  ```
  [tbl]
  (def result @[])
  (otable/each pair tbl (array/push result pair))
  result)

(defn- otable/clear
  ```
  Clear all entries from the ordered table.
  ```
  [tbl]
  (put tbl :entries nil)
  (put tbl :first nil)
  (put tbl :last nil))

(defn otable
  ```
  Create an ordered table with optional initialization parameters. If specified,
  kvs shall be a series of keys and values used to initialize the table.
  ```
  [& kvs]
  (if-not (even? (length kvs))
    (error "expected even number of arguments"))
  (let [ot (table/setproto (table/new 3) OTable)]
    (each [k v] (partition 2 kvs)
      (:put ot k v))
    ot))

(put OTable :put otable/put)
(put OTable :get otable/get)
(put OTable :kvs otable/kvs)
(put OTable :keys otable/keys)
(put OTable :values otable/values)
(put OTable :pairs otable/pairs)
(put OTable :clear otable/clear)
