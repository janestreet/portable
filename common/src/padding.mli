@@ portable

(** Depending on the object, either creates a shallow clone of it or returns it as is.
    When cloned, the clone will have extra padding words added after the last used word.

    This is designed to help avoid
    {{:https://en.wikipedia.org/wiki/False_sharing} false sharing}. False sharing has a
    negative impact on multicore performance. Accesses of both atomic and non-atomic
    locations, whether read-only or read-write, may suffer from false sharing.

    The intended use case for this is to pad all long lived objects that are being
    accessed highly frequently (read or written).

    Many kinds of objects can be padded, for example:

    {[
      let padded_atomic = Padding.copy_as_padded (Atomic.make 101)
      let padded_ref = Padding.copy_as_padded (ref 42)
      let padded_record = Padding.copy_as_padded { number = 76; pointer = [ 1; 2; 3 ] }
      let padded_variant = Padding.copy_as_padded (Some 1)
    ]}

    Note that padding potentially changes the length of an array, which means that you
    likely want to control the allocation of arrays more explicitly to ensure that your
    index and length calculations are correct. *)
val copy_as_padded : 'a -> 'a

(** [copy_as x] by default simply returns [x]. When [~padded:true] is explicitly
    specified, returns {{!copy_as_padded} [copy_as_padded x]}. *)
val copy_as : ?padded:bool @ local -> 'a -> 'a
