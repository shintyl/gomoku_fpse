open Core

type message_object = { message : string } [@@deriving yojson]

let () =
  Dream.run @@ Dream.logger
  (* @@ Dream.origin_referer_check *)
  @@ Dream.router
       [
         Dream.post "/" (fun request ->
             let%lwt body = Dream.body request in
             let message_object =
               match
                 body |> Yojson.Safe.from_string |> message_object_of_yojson
               with
               | Error _ ->
                   failwith
                     "JSON deserialization failure, could not find \"message\" \
                      field"
               | Ok res -> res
             in
             `String message_object.message |> Yojson.Safe.to_string
             |> Dream.json);
       ]
  @@ Dream.not_found