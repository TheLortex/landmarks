(* the type for stack information *)
type t

type id = int

(* allocate an empty stack information *)
val alloc : unit -> t

(* mutate the value with the current stack information *)
val get : t -> unit

(* get the stack ID *)
val id : t -> id

(* replace [t] with the parent. Returns false if it's the root stack. *)
val parent : t -> bool
