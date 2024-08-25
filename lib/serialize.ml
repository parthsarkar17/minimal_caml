(** [target_attrs_from_json j] is a record of type Target.t that is the internal
representation of a target with given attributes. *)
let target_attrs_from_json : Yojson.Basic.t -> Target.t = function
  | `Assoc target ->
      target
      |> List.map (fun (attr, attr_json) ->
             match attr_json with
             | `String s -> (attr, s)
             | _ -> raise (Failure "Can't resolve attribute to a string"))
      |> fun t ->
      let get_field of_string str = of_string (List.assoc str t) in
      let open Target in
      {
        shape = get_field Target.ShapeString.of_string "shape";
        shape_color = get_field ColorString.of_string "shapeColor";
        alpha = get_field AlphaString.of_string "alpha";
        alpha_color = get_field ColorString.of_string "alphaColor";
      }
  | _ -> raise (Failure "Can't resolve target to an association list")

(** [target_attrs_from_json j] is an association list, mapping a target ID integer
  to a Target.t representation of the target. *)
let target_ids_from_json : Yojson.Basic.t -> (int * Target.t) list = function
  | `Assoc targets ->
      List.map
        (fun (id_str, target_json) ->
          (int_of_string id_str, target_attrs_from_json target_json))
        targets
  | _ -> raise (Failure "Can't find association between target id and target")

(** [metadata_from_json j] extracts image metadata from image metadata Json. *)
let metadata_from_json : Yojson.Basic.t -> Image.metadata = function
  | `Assoc data ->
      let assignment_id =
        match List.assoc "id" data with
        | `Int id -> id
        | _ -> raise (Failure "Couldn't locate assignment id.")
      in
      let endpoint =
        match List.assoc "image" data with
        | `Assoc img_data -> (
            match List.assoc "imageUrl" img_data with
            | `String url -> url
            | _ -> raise (Failure "Couldn't locate image url."))
        | _ -> raise (Failure "Couldn't locate `image` field.")
      in
      let image_id =
        match List.assoc "image" data with
        | `Assoc img_data -> (
            match List.assoc "id" img_data with
            | `Int id -> id
            | _ -> raise (Failure "Could't locate image id."))
        | _ -> raise (Failure "Couldn't locate `image` field.")
      in
      let metadata : Image.metadata = { assignment_id; image_id; endpoint } in
      metadata
  | _ -> raise (Failure "Couldn't resolve work json to a metadata construct")
