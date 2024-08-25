(* These bind and map operations are taken from Async, so we can get them as
   infix functions without opening up all of Async. *)
let ( >>= ), ( >>| ) =
  let open Async in
  (( >>= ), ( >>| ))

(** An interface for the IP Address, the port, and endpoints for GS. *)
module type SOCKET = sig
  val socket : string
  val attributes_endp : string
  val work_endp : string
  val client_id : Cohttp.Header.t option
end

module Mission (Socket : SOCKET) = struct
  let attribute_failure = Failure "Could not acquire target attributes."

  let work_failure (s : Cohttp.Code.status_code) : exn =
    Failure
      ("Unable to fetch response code. Status: "
      ^ Cohttp.Code.string_of_status s)

  (** [url_of_targets s e] is a string that represents the url from which to query GS.
  E.g., returns [http://127.0.0.1:9000/api/v1/targets/all]. *)
  let url_of_targets (endpoint : string) : string =
    "http://" ^ Socket.socket ^ "/" ^ endpoint

  (** [await_target_attributes ()] is a promise for a Yojson.Basic Json type, 
  whose contents are target IDs and each of their attributes. *)
  let target_attributes () : (int * Target.t) list Async.Deferred.t =
    let uri = Socket.attributes_endp |> url_of_targets |> Uri.of_string in
    Async.try_with (fun () -> Cohttp_async.Client.get uri)
    >>= (fun result ->
          match result with
          | Ok (_, body) -> Cohttp_async.Body.to_string body
          | Error _ -> raise attribute_failure)
    >>| Yojson.Basic.from_string >>| Serialize.target_ids_from_json

  (** [fetch_work ()] is a promise for an Image.metadata instance, which reveals
  the URL from which to fetch the image, along with ID information. *)
  let fetch_work () : Image.metadata Async.Deferred.t =
    let uri = Socket.work_endp |> url_of_targets |> Uri.of_string in
    let rec query_for_work () =
      Async.try_with (fun () ->
          Cohttp_async.Client.post ?headers:Socket.client_id uri)
      >>= fun result ->
      match result with
      | Ok (response, body) -> (
          match response.status with
          | `No_content ->
              print_endline "Waiting for image.";
              Unix.sleep 2 |> query_for_work
          | `OK -> Cohttp_async.Body.to_string body
          | strange_occurrence -> raise (work_failure strange_occurrence))
      | Error _ -> raise (Failure "Failure in acquiring work.")
    in
    query_for_work () >>| Yojson.Basic.from_string
    >>| Serialize.metadata_from_json
end
