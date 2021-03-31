(defn- entry/take
  ```
  Take the specified entry out of the linked list, detaching it from any
  adjacent entries and updating the table's head/tail pointers.
  ```
  [tbl entry]

  # Retrieve the previous and next entries in the table.
  (def [previous next]
    [(in entry :previous)
     (in entry :next)])

  # Detach this entry from the previous one.
  (when previous
    (put entry :previous nil)
    (put previous :next next))

  # Detach this entry from the next one.
  (when next
    (put entry :next nil)
    (put next :previous previous))

  # Remove the entry from the head or tail of the list.
  (if (= entry (in tbl :first))
    (put tbl :first next))
  (if (= entry (in tbl :last))
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
    (let [previous (in before :previous)]

      # Connect the preceding entry to this one.
      (when previous
        (put previous :next entry)
        (put entry :previous previous))

      # Connect this entry to the following one.
      (put entry :next before)
      (put before :previous entry)

      # When being placed before the first entry, this entry is now first.
      (if (= before (in tbl :first))
        (put tbl :first entry)))

    # Add this entry directly after another.
    after
    (let [next (in after :next)]

      # Connect this entry to the following one.
      (when next
        (put next :previous entry)
        (put entry :next next))

      # Connect the preceding entry to this one.
      (put after :next entry)
      (put entry :previous after)

      # When being placed after the final entry, this entry is now last.
      (if (= after (in tbl :last))
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

  # To be an ordered table, x must at least be a table!
  (if (table? x)

    # Check if one of x's recursive prototypes is OTable.
    (while (and (not result) proto)
      (set proto (table/getproto proto))
      (set result (= proto OTable))))

  # If the above checks succeed, x is an ordered table.
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
  (if (or (nil? k) (nan? k)) (break tbl))

  # False is a valid table key, so ordinary truthiness checks will not work.
  (def [before? after? entries]
    [(not (or (nil? before) (nan? before)))
     (not (or (nil? after) (nan? after)))
     (in tbl :entries @{})])

  # Allow the values of before and after to be modified.
  (var [before after]
    [(if before? before)
     (if after? after)])

  # This is done as otherwise the behavior would be arbitrary.
  (if (and before? after?)
    (error "options :before and :after must not be specified at the same time"))

  # Retrieve the entry from the table if it exists.
  (var entry (in entries k))
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
      before? (set before (in entries before))
      after? (set after (in entries after)))
    (if-not (or before after)
      (if prepend
        (set before (in tbl :first))
        (set after (in tbl :last)))))

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

  # Attempt to retrieve an entry corresponding to the provided key.
  (if-let [entries (in tbl :entries)
           entry (in entries key)
           kv (in entry :kv)]

    # If the entry was successfully retrieved, return the entry's value.
    # Otherwise, return the provided default value (nil if not provided).
    (in kv 1) dflt))

(defn- otable/init
  ```
  Return the value of key in ds. If the key is not present, a new ordered table
  will be returned and set as the value of key in ds. If init-fn is provided,
  its return value will be the default value instead of a new ordered table.
  ```
  [tbl key &opt init-fn]

  # Nil or NaN keys cannot be added to a table.
  (if (or (nil? key) (nan? key)) (break))

  # Retrieve the entry table if it exists.
  (def entries (in tbl :entries @{}))

  # Return the entry from the table if it exists.
  (if-let [entry (in entries key)
           kv (in entry :kv)]
    (break (in kv 1)))

  # Use init-fn to initialize the new value, or otable constructor as a fallback.
  (def value
    (if init-fn (init-fn)
      (table/setproto (table/new 3) OTable)))

  # Unless init-fn returned nil, add the key/value pair as a new entry.
  (unless (nil? value)
    (def entry @{:kv [key value]})
    (put entries key entry)
    (entry/put tbl entry :after (in tbl :last))
    (put tbl :entries entries))
  value)

(defn- otable/next
  ```
  Obtain the key of the entry following the provided key.
  If no key is provided, returns the first key in the table.
  ```
  [tbl &opt key]
  (if-not (nil? key)
    (get-in tbl [:entries key :next :kv 0])
    (get-in tbl [:first :kv 0])))

(defn- otable/prev
  ```
  Obtain the key of the entry preceding the provided key.
  If no key is provided, returns the last key in the table.
  ```
  [tbl &opt key]
  (if-not (nil? key)
    (get-in tbl [:entries key :previous :kv 0])
    (get-in tbl [:last :kv 0])))

(defmacro- otable/collect
  ```
  Internal macro used by kvs, keys, values, and pairs.
  ```
  [variant]

  # Usually the result length is equal to the number of entries, but for kvs,
  # both the key and value of an entry are added to the result array.
  (def result-len
    (if (= variant :kvs)
      ~(* 2 (length entries))
      ~(length entries)))

  # Construct an array of the keys and/or values of the table, or return an
  # empty array if the table does not contain anything.
  ~(if-not (def entries (in tbl :entries)) @[]
     (let [result (array/new ,result-len)]
       (var entry (in tbl :first))
       (while entry
         ,(case variant
            :kvs ~(array/concat result (in entry :kv))
            :keys ~(array/push result (in (in entry :kv) 0))
            :values ~(array/push result (in (in entry :kv) 1))
            :pairs ~(array/push result (in entry :kv)))
         (set entry (in entry :next)))
       result)))

(defn- otable/kvs
  ```
  Return an array of the ordered table's keys and values.
  ```
  [tbl]
  (otable/collect :kvs))

(defn- otable/keys
  ```
  Return an array of the ordered table's keys.
  ```
  [tbl]
  (otable/collect :keys))

(defn- otable/values
  ```
  Return an array of the ordered table's keys.
  ```
  [tbl]
  (otable/collect :values))

(defn- otable/pairs
  ```
  Return an array of the ordered table's keys and values, paired into tuples.
  ```
  [tbl]
  (otable/collect :pairs))

(defn- otable/clear
  ```
  Clear all entries from the ordered table.
  ```
  [tbl]
  (put tbl :entries nil)
  (put tbl :first nil)
  (put tbl :last nil))

(defn- otable/compare
  ```
  Polymorphic comparison operator for ordered tables.

  Returns -1, 0, 1 for x < y, x = y, x > y respectively. Comparison is performed
  in the order in which key/value pairs were inserted into the table, with keys
  compared before values.
  ```
  [x y]
  (cond
    # Check for pointer equality between the two values, which obviates the need
    # for deep comparison as the contents are necessarily equal.
    (= x y) 0

    # When x and y are both ordered tables, perform a deep comparison.
    (otable? y)
    (do (var [x y] [(in x :first) (in y :first)])
      (var result 0)
      (while (= result 0)
        (cond
          # The key/value pairs of each table are equal, and neither table has
          # more elements than the other. Therefore, the tables are equivalent.
          (not (or x y)) (break)

          # If the preceding values are equal, but table x has more values than
          # table y, table x is considered greater than table y.
          (and x (not y)) (break (set result 1))

          # If the preceding values are equal, but table y has more values than
          # table x, table x is considered less than table y.
          (and y (not x)) (break (set result -1))

          # Compare the keys of the currently selected key/value pairs.
          (->> (compare (in (in x :kv) 0)
                        (in (in y :kv) 0))
               (set result) (= 0))

          # If the keys compared above are equal, compare the values.
          (->> (compare (in (in x :kv) 1)
                        (in (in y :kv) 1))
               (set result)))

        # Access the next entry of each table.
        (set x (in x :next))
        (set y (in y :next)))

      # Return the final result of the deep comparison.
      result)))

(defmacro- otable/transform
  ```
  Internal macro used by clone, reverse, and reverse!.
  ```
  [variant]
  (def [first next]
    (if (= variant :clone)
      [:first :next]
      [:last :previous]))
  ~(if (var entry (in tbl ,first))
     (do ,;(if (= variant :reverse!)
             ~[] ~[(def tbl (table/clone tbl))])
       (def entries
         (->> (in tbl :entries) length table/new (set (tbl :entries))))
       (def kv (in entry :kv))
       (var prev-entry @{:kv kv})
       (put tbl :first prev-entry)
       (put entries (in kv 0) prev-entry)
       (set entry (in entry ,next))
       (while entry
         (def kv (in entry :kv))
         (def new-entry @{:kv kv :previous prev-entry})
         (put entries (in kv 0) new-entry)
         (put prev-entry :next new-entry)
         (set entry (in entry ,next))
         (set prev-entry new-entry))
       (put tbl :last prev-entry))
     ,(if (= variant :reverse!)
        'tbl '(table/clone tbl))))

(defn- otable/clone
  ```
  Return a copy of the ordered table that can be mutated without updating the
  original table.
  ```
  [tbl]
  (otable/transform :clone))

(defn- otable/reverse
  ```
  Return a copy of the ordered table that can be mutated without updating the
  original table, with the order of key/value pairs reversed.
  ```
  [tbl]
  (otable/transform :reverse))

(defn- otable/reverse!
  ```
  Reverse the key/value pairs of the ordered table in-place. Returns the modified
  table.
  ```
  [tbl]
  (otable/transform :reverse!))

(defn- otable/length
  ```
  Return the number of entries within an ordered table.
  ```
  [tbl]
  (if (def entries (in tbl :entries))
    (length entries) 0))

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

(put OTable :jvs/get otable/get)
(put OTable :jvs/put otable/put)
(put OTable :jvs/init otable/init)
(put OTable :jvs/next otable/next)
(put OTable :jvs/clone otable/clone)
(put OTable :jvs/reverse otable/reverse)
(put OTable :jvs/reverse! otable/reverse!)
(put OTable :jvs/length otable/length)
(put OTable :put otable/put)
(put OTable :get otable/get)
(put OTable :next otable/next)
(put OTable :prev otable/prev)
(put OTable :kvs otable/kvs)
(put OTable :keys otable/keys)
(put OTable :values otable/values)
(put OTable :pairs otable/pairs)
(put OTable :clear otable/clear)
(put OTable :compare otable/compare)
(put OTable :clone otable/clone)
(put OTable :reverse otable/reverse)
(put OTable :reverse! otable/reverse!)
(put OTable :length otable/length)
