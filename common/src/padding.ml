let words_per_cache_line = 8
let num_padding_words = words_per_cache_line - 1

let copy_as_padded (o : 'a) : 'a =
  let o = Obj.repr o in
  if Obj.is_block o
  then (
    let original_size = Obj.size o in
    let padded_size =
      if original_size <= num_padding_words
      then num_padding_words
      else original_size + num_padding_words
    in
    if original_size <> padded_size
    then (
      let t = Obj.tag o in
      if Sys.word_size = 64 && t != Obj.double_array_tag
      then (
        let n = Obj.new_block t padded_size in
        Array.blit (Obj.obj o) 0 (Obj.obj n) 0 original_size;
        Obj.obj n)
      else Obj.obj o)
    else Obj.obj o)
  else Obj.obj o
;;

let copy_as ?padded x =
  match padded with
  | None | Some false -> x
  | Some true -> copy_as_padded x
;;
