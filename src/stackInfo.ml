type t

type id = int

external alloc : unit -> t = "caml_alloc_stack_info"

external get : t -> unit = "caml_stack_info"

external id : t -> int = "caml_stack_info_handler"

external parent : t -> bool = "caml_stack_info_parent"
