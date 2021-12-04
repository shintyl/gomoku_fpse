open Core

type message_object = { message : string } [@@deriving yojson]

module Foo = Board.Board

let deserialize_message body =
  
  `String
    (match body |> Yojson.Safe.from_string |> message_object_of_yojson with
    | Error _ ->
        failwith
          "JSON deserialization failure, could not find \"message\" field"
    | Ok res -> res)
      .message |> Yojson.Safe.to_string
  |> fun x ->
  String.drop_prefix x 1 |> fun x -> String.drop_suffix x 1

let () =
  let user_id_to_game_id = Hashtbl.create (module String) in
  let game_id_to_game_board = Hashtbl.create (module String) in

  Dream.run @@ Dream.logger @@ Dream.memory_sessions
  @@ Dream.router
       [
         Dream.post "/create_game" (fun request ->
             let%lwt body = Dream.body request in
             let other_user_id = deserialize_message body in
             let game_id = other_user_id ^ Dream.session_id request in
             Hashtbl.add_exn user_id_to_game_id ~key:other_user_id ~data:game_id;
             Hashtbl.add_exn user_id_to_game_id ~key:(Dream.session_id request)
               ~data:game_id;
             Hashtbl.add_exn game_id_to_game_board ~key:game_id ~data:1;
             Dream.json game_id);
         Dream.get "/new_session" (fun request ->
             let%lwt () = Dream.invalidate_session request in
             Dream.json (Dream.session_id request));
       ]
  @@ Dream.not_found
