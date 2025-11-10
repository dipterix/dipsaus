# Defines abstract queue class

This class is inspired by <https://cran.r-project.org/package=txtq>. The
difference is `AbstractQueue` introduce an abstract class that can be
extended and can queue not only text messages, but also arbitrary R
objects, including expressions and environments. All the queue types in
this package inherit this class.

## Abstract Public Methods

Methods start with `@...` are not thread-safe. Most of them are not used
directly by users. However, you might want to override them if you
inherit this abstract class. Methods marked as "(override)" are not
implemented, meaning you are supposed to implement the details. Methods
marked as "(optional)" usually have default alternatives.

- `initialize(...)` (override):

  The constructor. Usually three things to do during the process: 1. set
  `get_locker` `free_locker` if you don't want to use the default
  lockers. 2. set lock file (if using default lockers). 3. call
  `self$connect(...)`

- `get_locker()`, `free_locker()` (optional):

  Default is `NULL` for each methods, and queue uses an internal
  `private$default_get_locker` and `private$default_free_locker`. These
  two methods are for customized locker, please implement these two
  methods as functions during `self$initialization` `get_locker` obtains
  and lock access (exclusive), and `free_locker` frees the locker. Once
  implemented, `private$exclusive` will take care the rest. Type:
  function; parameters: none; return: none

- `@get_head()`, `@set_head(v)` (override):

  Get head so that we know where we are in the queue `self$@get_head()`
  should return a integer indicating where we are at the queue
  `self$@set_head(v)` stores that integer. Parameter `v` is always
  non-negative, this is guaranteed. Users are not supposed to call these
  methods directly, use `self$head` and `self$head<-` instead. However,
  if you inherit this class, you are supposed to override the methods.

- `@get_total()`, `@set_total(v)` (override):

  Similar to `@get_head` and `@set_head`, defines the total items ever
  stored in the queue. total-head equals current items in the queue.

- `@inc_total(n=1)` (optional):

  Increase total, usually this doesn't need to be override, unless you
  are using files to store total and want to decrease number of file
  connections

- `@append_header(msg, ...)` (override):

  `msg` will be vector of strings, separated by "\|", containing encoded
  headers: \`time\`, \`key\`, \`hash\`, and \`message\`. to decode
  what's inside, you can use
  `self$print_items(stringr::str_split_fixed(msg, '\|', 4))`. **Make
  sure** to return a number, indicating number of items stored. Unless
  handled elsewhere, usually `return(length(msg))`.

- `@store_value(value, key)` (override):

  Defines how to store value. \`key\` is unique identifier generated
  from time, queue ID, and value. Usually I use it as file name or key
  ID in database. value is an arbitrary R object to store. you need to
  store value somewhere and return a string that will be passed as
  \`hash\` in `self$restore_value`.

- `restore_value(hash, key, preserve = FALSE)` (override):

  Method to restore value from given combination of \`hash\` and
  \`key\`. \`hash\` is the string returned by `@store_value`, and
  \`key\` is the same as key in `@store_value`. preserve is a indicator
  of whether to preserve the value for future use. If set to `FALSE`,
  then you are supposed to free up the resource related to the value.
  (such as free memory or disk space)

- `@log(n = -1, all = FALSE) (override)`:

  get `n` items from what you saved to during `@append_header`. `n` less
  equal than 0 means listing all possible items. If `all=TRUE`, return
  all items (number of rows should equals to `self$total`), including
  popped items. If `all=FALSE`, only return items in the queue (number
  of rows is `self$count`). The returned value should be a `n x 4`
  matrix. Usually I use `stringr::str_split_fixed(..., '\|', 4)`. Please
  see all other types implemented for example.

- `@reset(...)` (override):

  Reset queue, remove all items and reset head, total to be 0.

- `@clean()` (override):

  Clean the queue, remove all the popped items.

- `@validate()` (override):

  Validate the queue. Stop if the queue is broken.

- `@connect(con, ...)` (override):

  Set up connection. Usually should be called at the end of
  `self$initialization` to connect to a database, a folder, or an
  existing queue you should do checks whether the connection is new or
  it's an existing queue.

- `connect(con, ...)` (optional):

  Thread-safe version. sometimes you need to override this function
  instead of `@connect`, because `private$exclusive` requires `lockfile`
  to exist and to be locked. If you don't have lockers ready, or need to
  set lockers during the connection, override this one.

- `destroy()` (optional):

  Destroy a queue, free up space and call
  `delayedAssign('.lockfile', {stop(...)}, assign.env=private)` to raise
  error if a destroyed queue is called again later.

## Public Methods

Usually don't need to override unless you know what you are doing.

- `push(value, message='',...)`:

  Function to push an arbitrary R object to queue. `message` is a string
  giving notes to the pushed item. Usually message is stored with
  header, separated from values. The goal is to describe the value.
  `...` is passed to `@append_header`

- `pop(n = 1, preserve = FALSE)`:

  Pop `n` items from the queue. `preserve` indicates whether not to free
  up the resources, though not always guaranteed.

- `print_item(item)`, `print_items(items)`:

  To decode matrix returned by
  [`log()`](https://rdrr.io/r/base/Log.html), returning named list or
  data frame with four heads: \`time\`, \`key\`, \`hash\`, and
  \`message\`.

- `list(n=-1)`:

  List items in the queue, decoded. If `n` is less equal than 0, then
  list all results. The result is equivalent to
  `self$print_items(self$log(n))`

- `log(n=-1,all=FALSE)`:

  List items in the queue, encoded. This is used with
  `self$print_items`. When `all=TRUE`, result will list the records ever
  pushed to the queue since the last time queue is cleaned. When
  `all=FALSE`, results will be items in the queue. `n` is the number of
  items.

## Public Active Bindings

- `id`:

  Read-only property. Returns unique ID of current queue.

- `lockfile`:

  The lock file.

- `head`:

  Integer, total number of items popped, i.e. inactive items.

- `total`:

  Total number of items ever pushed to the queue since last cleaned,
  integer.

- `count`:

  Integer, read-only, equals to total - head, number of active items in
  the queue

## Private Methods or properties

- `.id`:

  Don't use directly. Used to store queue ID.

- `.lockfile`:

  Location of lock file.

- `lock`:

  Preserve the file lock.

- `exclusive(expr,...)`:

  Function to make sure the methods are thread-safe

- `default_get_locker()`:

  Default method to lock a queue

- `default_free_locker`:

  Default method to free a queue
