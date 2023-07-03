(in-package #:org.shirakumo.depot)

;; conditions.lisp
(docs:define-docs
  (type depot-condition
    "Base condition for all conditions in the context of depots.

See OBJECT")
  
  (function object
    "Returns the object that the condition relates to.

See DEPOT-CONDITION")
  
  (type no-such-entry
    "Error signalled if a requested entry does not exist.

See DEPOT-CONDITION
See ID")

  (type no-such-attribute
    "Error signalled if a requested attribute does not exist.

See DEPOT-CONDITION
See NAME")

  (function name
    "Returns the name of the attribute.

See NO-SUCH-ATTRIBUTE")
  
  (type permission-denied
    "Error signalled if an action cannot be performed due to security constraints.

See DEPOT-CONDITION")
  
  (type depot-full
    "Error signalled if a depot cannot accept further content.

See DEPOT-CONDITION")
  
  (type entry-already-exists
    "Error signalled if the requested entry already exists.

See DEPOT-CONDITION
See ATTRIBUTES")

  (type entry-does-not-exist
    "Error signalled if the entry to operate on no longer exists.

See DEPOT-CONDITION")
  
  (type unsupported-operation
    "Error signalled when attempting an operation that is not supported by the depot implementation.

See DEPOT-CONDITION
See OPERATION")
  
  (function operation
    "Returns the name of the operation that failed.

See UNSUPPORTED-OPERATION")
  
  (type transaction-aborted
    "Error signalled when a transaction has been aborted and can't be used.

See DEPOT-CONDITION")
  
  (type write-conflict
    "Error signalled when a write cannot be commited due to an existing conflict.

This usually happens when a write has been committed between the time
the transaction was opened and the current commit was attempted.

See TRANSACTION-ABORTED")
  
  (type read-invalidated
    "Error signalled when a read cannot be completed due to a conflict.

This usually happens when a write has been committed between the time
the transaction was opened and the current read was attempted.

See TRANSACTION-ABORTED")
  
  (type not-a-depot
    "Error signalled when an object cannot be coerced to a depot.

See DEPOT-CONDITION"))

;; protocol.lisp
(docs:define-docs
  (type realizer
    "Superclass for objects implementing the realizer protocol.

See REALIZE-ENTRY
See REGISTER-REALIZER
See REMOVE-REALIZER
See DEFINE-REALIZER")
  
  (type depot
    "Superclass for objects implementing the depot protocol.

When implementing a new depot subclass, the following generic
functions SHOULD be implemented:

  - LIST-ENTRIES
  - MAKE-ENTRY

All other depot operations are implemented using fallback operations.
You may however implement other, specialised methods as well should
there be more efficient ways of implementing them.

See LIST-ENTRIES
See QUERY-ENTRIES
See QUERY-ENTRY
See ENTRY
See ENTRY-EXISTS-P
See MAKE-ENTRY
See OPEN-P
See ENSURE-DEPOT")
  
  (type entry
    "Superclass for objects implementing the entry protocol.

When implementing a new entry subclass, the following generic
functions SHOULD be implemented:

  - ATTRIBUTES
  - (SETF ATTRIBUTES)
  - DELETE-ENTRY
  - OPEN-ENTRY

Any other entry operations are implemented using fallback operations.
You may however implement other, specialised methods as well should
there be more efficient ways of implementing them.

See DELETE-ENTRY
See ENTRY-MATCHES-P
See ATTRIBUTES
See ATTRIBUTE
See ID
See DEPOT
See ENSURE-DEPOT
See OPEN-ENTRY
See WRITE-TO
See READ-FROM")
  
  (type transaction
    "Superclass for objects implementing the transaction protocol.

When implementing a new transaction subclass, the following generic
functions SHOULD be implemented:

  - SIZE
  - INDEX
  - ABORT
  - COMMIT
  - WRITE-TO
  - READ-FROM

See TARGET
see WRITE-TO
See READ-FROM
See SIZE
See INDEX
See TO-STREAM
See COMMIT
See ABORT")
  
  (function target
    "Returns the entry that the transaction is targeting.

See ENTRY
See TRANSACTION")
  
  (function list-entries
    "Returns a fresh list of all entries is the depot.

See ENTRY
See DEPOT")
  
  (function query-entries
    "Returns a list of entries in the depot that match the queried attributes.

The only required attribute that a depot must be able to query is ID.
Other attributes are dependent on the depot and possibly entry being
queried.

The order of returned entries is not required to be stable.

See ENTRY
See DEPOT")
  
  (function query-entry
    "Returns the matching entry in the depot.

If no matching entry is found, NIL is returned. If multiple entries
match, any one of them may be returned.

See QUERY-ENTRIES
See ENTRY
See DEPOT")
  
  (function entry
    "Returns the entry with the given ID from the depot.

If no such entry exists in the depot, a NO-SUCH-ENTRY error is
signalled.

See ENTRY
See DEPOT")
  
  (function entry-exists-p
    "Returns true if an entry with the given ID exists in the depot.

See DEPOT")
  
  (function make-entry
    "Creates a new entry in the depot.

If the depot does not support creating an entry, an error of type
UNSUPPORTED-OPERATION is signalled.

If the depot is full and no further entries can be created due to
storage constraints, an error of type DEPOT-FULL is signalled.

If creating a new entry with the requested attributes would overwrite
an existing entry, an error of type ENTRY-ALREADY-EXISTS is signalled.

If creating the requested entry would violate security restrictions,
an error of type PERMISSION-DENIED is signalled.

The attributes that can be passed as keyword arguments are entirely
dependent on the depot.

Depending on the depot, calling MAKE-ENTRY may not persist the entry
yet, and may only do so once the entry has been written to. As such,
an entry created with this function may not immediately be returned by
LIST-ENTRIES, QUERY-ENTRIES, and so forth.

See DEPOT
See UNSUPPORTED-OPERATION
See DEPOT-FULL
See ENTRY-ALREADY-EXISTS
See PERMISSION-DENIED")

  (function open-p
    "Returns true if the depot is considered \"open\" and ready for reading and writing.

A depot may become \"closed\" by calling CLOSE on it. Once a depot is
closed, its data may no longer be up to date, and writing and reading
from any of its entries may fail until ENSURE-DEPOT is called on it
to open it back up again.

See DEPOT
See ENSURE-DEPOT")
  
  (function delete-entry
    "Deletes the entry from its depot.

If the depot does not support deleting an entry, an error of type
UNSUPPORTED-OPERATION is signalled.

If deleting the entry would violate security restrictions, an error of
type PERMISSION-DENIED is signalled.

If the entry does not exist (and thus cannot be deleted), an error of
type ENTRY-DOES-NOT-EXIST is signalled.

After an entry has been deleted, operating on it in any way leads to
undefined behaviour.

See ENTRY")
  
  (function entry-matches-p
    "Returns true if the entry matches the specified attribute query.

The available attributes are dependent on the entry and depot used.

If the requested attribute does not exist, an error of type
NO-SUCH-ATTRIBUTE may be signalled.

See ENTRY")
  
  (function attributes
    "Accesses a plist of attributes of the entry.

Which attributes are given depends on the depot/entry used. The only
attribute that is required is the :ID attribute. The following
attributes are nevertheless standardised in their purpose:

  :NAME       --- The name of the entry. This excludes the type
                  extension of the path, if there is such a concept.
  :TYPE       --- The content type of the entry as a string.
  :VERSION    --- The version of the entry, in case versioning
                  information is kept.
  :WRITE-DATE --- The universal-time timestamp at which the entry was
                  last modified.
  :AUTHOR     --- The user (as a string) that authored the entry.
  :SIZE       --- The size of the readable data from the entry in
                  octets.
  :COMMENT    --- An arbitrary human-readable string of metadata on
                  the entry.
  :CRC-32     --- The CRC-32 checksum of the entry's contents as an
                  unsigned-byte 32.

When setting this place, only the attributes in the provided plist
should be changed. Attributes are not removed using this
method. Attributes other than the ones provided in the set plist may
change as well, however. Some attributes may also not be changed. If
an attribute cannot be changed, an error of type PERMISSION-DENIED may
be signalled.

See ENTRY
See ATTRIBUTE")
  
  (function attribute
    "Accesses a single attribute of the entry.

Which attributes are available depend on the depot/entry used. The
only attribute that is required is the :ID attribute.

If the requested attribute does not exist, an error of type
NO-SUCH-ATTRIBUTE is signalled.

When setting this place, If the attribute cannot be changed, an error
of type PERMISSION-DENIED may be signalled.

See ENTRY
See ATTRIBUTES")
  
  (function id
    "Returns the ID of the entry.

The ID uniquely identifies the entry within the depot.

See ENTRY")
  
  (function depot
    "Returns the depot the entry is a part of.

See ENTRY
See DEPOT")
  
  (function realize-entry
    "Attempts to turn the entry into another entry or depot of another backend.

This allows you to access the contents of a depot that's encoded
within another depot.

You may either pass a realizer instance, or T. If T is passed, each of
the known realizers is tried in turn until one that passes is found,
whose result is returned. If no realizer accepts the entry, NIL is
returned. If a realizer instance is passed directly and it fails at
realizing the entry, an error is signalled.

When successful the result may be an ENTRY or a DEPOT.

See ENTRY
See REALIZER")
  
  (function ensure-depot
    "Attempts to convert the given object to a depot.

If this conversion should not be possible, an error of type
NOT-A-DEPOT is signalled.

If a DEPOT is passed, the depot is returned. If an ENTRY is passed,
the entry's originating DEPOT is returned.

See ENTRY
See DEPOT")
  
  (function open-entry
    "Open the entry for a read or write transaction.

Returns a live TRANSACTION instance.

DIRECTION must be either :INPUT or :OUTPUT.
ELEMENT-TYPE must be a valid type that the entry supports. The set of
supported element types must in the very least include
(UNSIGNED-BYTE 8) and CHARACTER.

Depending on the entry type, additional arguments may be accepted that
designate transaction properties.

You MUST complete the transaction by use of COMMIT or ABORT at some
point, as resources may be allocated in the back that could be
exhausted if staying live for too long.

A transaction object may be invalidated by an intermittent write
operation to the same entry either through the same process, or
through an external mechanism of some kind.

Any changes made to an entry by either WRITE-TO, or by external
processes, must not be visible until the transaction has been
successfully COMMITTed.

If a transaction is ABORTed, any changes made via WRITE-TO must be
discarded as if they had never happened.

See WITH-OPEN
See TRANSACTION
See ENTRY
See COMMIT
See ABORT
See WRITE-TO
See READ-FROM")
  
  (function write-to
    "Write a sequence to a transaction.

TRANSACTION must be an object obtained through OPEN-ENTRY with the
DIRECTION set to :OUTPUT, and may not have been aborted, committed, or
invalidated.

SEQUENCE must be a lisp sequence type. START and END may be indices
into this sequence designating a subsequence to write.

If another write was committed after the transaction was created, but
before this call an error of type WRITE-CONFLICT may be signalled.

You may also call this on an ENTRY directly, in which case it will
construct a transaction, write the sequence, and immediately attempt
to COMMIT it.

See OPEN-ENTRY
See TRANSACTION")
  
  (function read-from
    "Read a sequence from a transaction.

TRANSACTION must be an object obtained through OPEN-ENTRY with the
DIRECTION set to :INPUT, and may not have been aborted, committed, or
invalidated.

SEQUENCE must be a lisp sequence type. START and END may be indices
into this sequence designating a subsequence to write.

The sequence is filled starting from the current INDEX of the
transaction until either the end of the designated sequence is
reached, or the end of the transaction is reached.

Returns the index of the last element in the sequence that was
modified.

If another write was committed after the transaction was created, but
before this call an error of type READ-INVALIDATED may be signalled.

You may also call this on an ENTRY directly, in which case it will
construct a transaction, read the sequence, and immediately attempt
to COMMIT. In this case you may also pass either 'BYTE or 'CHARACTER
as arguments to the second argument, in which case a vector will be
constructed for you and returned instead of the last modified
index. The vector will have element type of (UNSIGNED-BYTE 8) or
CHARACTER, respectively.

See OPEN-ENTRY
See TRANSACTION")
  
  (function size
    "Returns the size (in elements) of the backing storage the transaction represents.

If the TRANSACTION was not obtained through OPEN-ENTRY with the
DIRECTION set to :INPUT, the consequences of this function are
implementation defined.

See TRANSACTION")
  
  (function index
    "Accesses the index at which in the backing storage read or write operations are performed.

If the TRANSACTION was obtained through OPEN-ENTRY with the DIRECTION
set to :OUTPUT, and the index is set to an index higher than the
current index, the consequences are undefined.

See TRANSACTION")
  
  (function to-stream
    "Turns the transaction object into a STREAM object that can be passed to other agnostic libraries.

Note that many operations on the stream can be significantly more
expensive than if done through transactions' READ-FROM/WRITE-TO
functions, and some stream operations may not be entirely supported.

See TRANSACTION")
  
  (function commit
    "Commits the transaction and completes any pending operations.

If the underlying entry was created new, but in the meantime the same
entry was already created successfully, an error of type
ENTRY-ALREADY-EXISTS may be signalled.

If the underlying entry was deleted before the commit, an error of
type ENTRY-DOES-NOT-EXIST may be signalled.

If the underlying depot is full and cannot support the amount of data
to be committed, an error of type DEPOT-FULL may be signalled.

If the operating user lacks the permissions to complete the operation,
an error  of type PERMISSION-DENIED may be signalled.

If another write was committed after this transaction was created, but
before this call to commit occurred, an error of type WRITE-CONFLICT
may be signalled for :OUTPUT transactions, or READ-INVALIDATED for
:INPUT transactions.

If the commit completes successfully, the transaction is closed and
the changes must become visible to new READ transactions, as well as
to other external processes interacting with the same entry.

See OPEN-ENTRY
See TRANSACTION")
  
  (function abort
    "Aborts the transaction and rolls back any changes it may have made.

The transaction is closed and any changes it may have made will be
discarded and thus will not be visible to READ transactions (present
or future), or other external processes interacting with the same
entry.

It is thus as if the transaction had never been opened in the first
place.

See OPEN-ENTRY
See TRANSACTION")

  (function register-realizer
    "Register a realizer.

You can pass a symbol naming the class of a realizer, a realizer
subclass, or a realizer instance.

See REALIZER
See REMOVE-REALIZER
See DEFINE-REALIZER")

  (function remove-realizer
    "Deregisters the realizer.

You can pass a symbol naming the class of a realizer, a realizer
subclass, or a realizer instance.

See REALIZER
See REGISTER-REALIZER
See DEFINE-REALIZER")

  (function define-realizer
    "Define a new realizer.

This does the following:

If no class named NAME exists yet, it is defined as a subclass of
REALIZER. For each of the DISPATCHERS in the body it defines a method
on REALIZE-ENTRY, with the structure of each dispatcher being as
follows:

  ((var class) . body)

CLASS naming a class that the entry must be a subclass of, and VAR
being the variable to which the entry is bound. BODY being the forms
that are executed when a matching entry is being realised.

See REALIZER
See REGISTER-REALIZER
See REMOVE-REALIZER
See REALIZE-ENTRY")

  (function entry*
    "Resolves a chain of depot entries.

This is as if calling ENTRY on each passed ID and replacing the depot
with the result. The last depot is returned.

This is useful for hierarchical depots.

See ENTRY")

  (function with-open
    "Macro to conveniently handle a transaction.

Will call OPEN-ENTRY on the passed arguments, bind the resulting
transaction to TRANSACTION, and evaluate the BODY. During execution of
the BODY, an ABORT restart is established. This restart will invoke
ABORT on the transaction object and exit the body.

If the body is executed to completion, the transaction is
COMMITTed. If the body should exit abnormally, the transaction is
ABORTed.

See OPEN-ENTRY
See ABORT
See COMMIT
See TRANSACTION")
  
  (type stream-transaction
    "Superclass for transactions that operate on a stream.

Automatically handles the implementation of SIZE and INDEX for
file-streams. 

For all streams, ABORT and COMMIT will simply close the stream. It is
up to the subclass to establish the proper ABORT and COMMIT
semantics.

For all streams, WRITE-TO and READ-FROM are implemented using
WRITE-SEQUENCE and READ-SEQUENCE, respectively.

TO-STREAM is also implemented to simply return the underlying stream.

See TRANSACTION
See TO-STREAM")
  
  (type transaction-stream
    "Gray stream wrapper around a transaction object.

Attempts to implement all gray stream methods and translate them to
depot transaction operations.

See TO-STREAM
See TRANSACTION"))

;; pathname.lisp
(docs:define-docs
  (variable *os-depot*
    "Contains the base depot for the operating system according to the implementation.

See OS-DEPOT")
  
  (type os-depot
    "Base class for the operating system's file system depot.

See *OS-DEPOT*
See DEPOT")

  (type host
    "Base class for pathname hosts.

These correspond to pathname-host types.

See DEPOT")

  (type device
    "Base class for pathname devices.

These correspond to pathname-device types.

See DEPOT")

  (function to-pathname
    "Returns a pathname that this entry represents.

This operation may also be used for entries that do not come from a
file system depot, but the success of the operation is not
guaranteed, as it is subject to implementation pathname constraints
that may not apply to the depot or entry in question.

See FROM-PATHNAME")

  (function from-pathname
    "Returns an entry corresponding to the given pathname.

This may signal an error of type NO-SUCH-ENTRY if the entry does not
exist.

If CREATE-DIRECTORIES is true, inexistent entries along the pathname's
directories will be created. If it is :PRETEND (the default), then the 
directories are not actually created on the filesystem, but instead
are virtual.

Returns a FILE or DIRECTORY.

See TO-PATHNAME
See FILE
See DIRECTORY")

  (type directory
    "Class representing directories on a standard operating system hierarchical file system.

This is both a DEPOT and an ENTRY.

See DEPOT
See ENTRY")

  (type file
    "Class representing files on a standard operating system hierarchical file system.

See ENTRY")

  (type file-write-transaction
    "Class representing a write transaction to a file.

The atomicity of the write is established by writing to a temporary
file, which is then renamed to the existing file on commit.

See FILE
See OPEN-ENTRY
See TRANSACTION")

  (type file-read-transaction
    "Class representing a read transaction from a file.

See FILE
See OPEN-ENTRY
See TRANSACTION"))
