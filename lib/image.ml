type metadata = { image_id : int; assignment_id : int; endpoint : string }
(** This type represents the essential information for identifying an assigned image *)

(** [string_of_metadata m] creates a serialized version of the work ID record. *)
let string_of_metadata { image_id; assignment_id; endpoint } : string =
  "{" ^ "image_id: " ^ string_of_int image_id ^ "; " ^ "assignment_id: "
  ^ string_of_int assignment_id
  ^ "; " ^ "endpoint: " ^ endpoint ^ "}"
