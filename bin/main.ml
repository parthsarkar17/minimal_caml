open Minimal_caml
open Async

module Socket : Client.SOCKET = struct
  let socket = "127.0.0.1:9000"
  let attributes_endp = "api/v1/targets/all"
  let work_endp = "api/v1/assignment/work"
  let client_id = Some (Cohttp.Header.init_with "Username" "adlc")
end

module Mission = Client.Mission (Socket)

let print_deferred (string_of : 'a -> string) (d : 'a Deferred.t) () :
    unit Deferred.t =
  d >>| fun a -> a |> string_of |> print_endline

(* let () =
   Async.Command.async ~summary:"get targets"
     (Command.Param.return
        (print_deferred
           (fun lst ->
             lst
             |> List.map (fun (i, t) ->
                    "id: " ^ string_of_int i ^ ", " ^ Target.string_of_target t
                    ^ "\n")
             |> String.concat "")
           (Mission.target_attributes ())))
   |> Command_unix.run *)

let () =
  Async.Command.async ~summary:"get work metadata"
    (Command.Param.return
       (print_deferred Image.string_of_metadata (Mission.fetch_work ())))
  |> Command_unix.run
