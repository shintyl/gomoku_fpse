open Core

type message_object = { message : string } [@@deriving yojson]

type coords_object = { x : int; y : int } [@@deriving yojson]

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

let deserialize_coords body =
  match body |> Yojson.Safe.from_string |> coords_object_of_yojson with
  | Error _ ->
      failwith
        "JSON deserialization failure, could not find \"x\" and \"y\" field"
  | Ok res -> res

let six_digit_random () =
  let rec gen len =
    if len = 6 then "" else Int.to_string (Random.int 10) ^ gen (len + 1)
  in
  gen 0

type game_state = {
  board : Board.Board.pieces;
  is_black_turn : bool;
  b_id : string;
  w_id : string;
  winner_is_black : bool option;
  b_id_ws : Dream.websocket option;
  w_id_ws : Dream.websocket option;
}

let () =
  Random.self_init ();

  let sessions = Hash_set.create (module String) in
  let session_id_to_game_id = Hashtbl.create (module String) in
  let game_id_to_game_state = Hashtbl.create (module String) in

  Dream.run @@ Dream.logger @@ Dream.memory_sessions
  @@ Dream.router
       [
         Dream.get "/websocket" (fun request ->
             Dream.websocket (fun websocket ->
                 let session_id =
                   match Dream.session "session" request with
                   | Some s -> s
                   | None -> failwith "Could not find session"
                 in
                 let game_id =
                   Hashtbl.find_exn session_id_to_game_id session_id
                 in
                 let game_state =
                   Hashtbl.find_exn game_id_to_game_state game_id
                 in
                 Dream.log "%s" game_state.b_id;
                 Dream.log "%s" game_state.w_id;
                 Dream.log "%s" session_id;
                 Hashtbl.set game_id_to_game_state ~key:game_id
                   ~data:
                     {
                       board = game_state.board;
                       is_black_turn = game_state.is_black_turn;
                       b_id = game_state.b_id;
                       w_id = game_state.w_id;
                       winner_is_black = game_state.winner_is_black;
                       b_id_ws =
                         (if String.equal game_state.b_id session_id then
                          Some websocket
                         else game_state.b_id_ws);
                       w_id_ws =
                         (if String.equal game_state.w_id session_id then
                          Some websocket
                         else game_state.w_id_ws);
                     };
                 let rec loop () =
                   match%lwt Dream.receive websocket with
                   | Some a ->
                       Dream.log "%s" a;
                       loop ()
                   | None -> Dream.close_websocket websocket
                 in
                 loop ()));
         (* let x, _ = game_state.is_black_turn_signal in
            let send = S.map (Dream.log "%b") x in
            let%lwt () = Dream.send websocket "Good-bye!" in
            Dream.close_websocket websocket)); *)
         Dream.scope "/session" []
           [
             Dream.post "/refresh" (fun request ->
                 let rand_num = six_digit_random () in
                 Hash_set.add sessions rand_num;
                 match Dream.session "session" request with
                 | Some s ->
                     Hash_set.remove sessions s;
                     let%lwt () = Dream.invalidate_session request in
                     let%lwt () =
                       Dream.put_session "session" rand_num request
                     in
                     Dream.json rand_num
                 | None ->
                     let%lwt () =
                       Dream.put_session "session" rand_num request
                     in
                     Dream.json rand_num);
             Dream.get "/get" (fun request ->
                 match Dream.session "session" request with
                 | Some s -> Dream.json s
                 | None -> failwith "Could not find session");
           ];
         Dream.scope "/game" []
           [
             Dream.post "/create" (fun request ->
                 let session_id =
                   match Dream.session "session" request with
                   | Some s -> s
                   | None -> failwith "Could not find session"
                 in
                 let%lwt body = Dream.body request in
                 let other_session_id =
                   let temp = deserialize_message body in
                   match Hash_set.mem sessions temp with
                   | false -> failwith "Could not find other client's session"
                   | true -> temp
                 in
                 let game_id = other_session_id ^ session_id in
                 Hashtbl.add_exn session_id_to_game_id ~key:other_session_id
                   ~data:game_id;
                 Hashtbl.add_exn session_id_to_game_id ~key:session_id
                   ~data:game_id;
                 Hashtbl.add_exn game_id_to_game_state ~key:game_id
                   ~data:
                     {
                       board = Board.Board.Vect2Map.empty;
                       is_black_turn = true;
                       b_id = session_id;
                       w_id = other_session_id;
                       winner_is_black = None;
                       b_id_ws = None;
                       w_id_ws = None;
                     };
                 Dream.json game_id);
             Dream.post "/make_move" (fun request ->
                 let%lwt body = Dream.body request in
                 let coords = deserialize_coords body in
                 let session_id =
                   match Dream.session "session" request with
                   | Some s -> s
                   | None -> failwith "Could not find session"
                 in
                 let game_id =
                   Hashtbl.find_exn session_id_to_game_id session_id
                 in
                 let game_state =
                   Hashtbl.find_exn game_id_to_game_state game_id
                 in
                 match game_state.winner_is_black with
                 | Some _ -> failwith "Game completed"
                 | None -> (
                     let color =
                       match
                         ( game_state.is_black_turn,
                           String.equal game_state.b_id session_id )
                       with
                       | true, true -> 'b'
                       | false, false -> 'w'
                       | _, _ -> failwith "Invalid turn"
                     in
                     match
                       Board.Board.insert game_state.board (coords.x, coords.y)
                         color
                     with
                     | Error _ -> failwith "Invalid move"
                     | Ok new_board -> (
                         Hashtbl.set game_id_to_game_state ~key:game_id
                           ~data:
                             {
                               board = new_board;
                               is_black_turn = not game_state.is_black_turn;
                               b_id = game_state.b_id;
                               w_id = game_state.w_id;
                               winner_is_black =
                                 (match
                                    Board.Board.insert_check game_state.board
                                      [ (1, 0); (1, 1); (0, 1); (-1, 1) ]
                                      (coords.x, coords.y) color
                                  with
                                 | Error _ -> failwith "Invalid move"
                                 | Ok true -> Some game_state.is_black_turn
                                 | Ok false -> None);
                               b_id_ws = game_state.b_id_ws;
                               w_id_ws = game_state.w_id_ws;
                             };
                         match (game_state.b_id_ws, game_state.w_id_ws) with
                         | Some ws_b, Some ws_w ->
                             let turn =
                               if not game_state.is_black_turn then "b" else "w"
                             in
                             let%lwt () = Dream.send ws_b turn in
                             let%lwt () = Dream.send ws_w turn in
                             Dream.json (Board.Board.yojson_of_pieces new_board)
                         | _ -> failwith "WS connection not found")));
             Dream.get "winner" (fun request ->
                 let session_id =
                   match Dream.session "session" request with
                   | Some s -> s
                   | None -> failwith "Could not find session"
                 in
                 let game_id =
                   Hashtbl.find_exn session_id_to_game_id session_id
                 in
                 let game_state =
                   Hashtbl.find_exn game_id_to_game_state game_id
                 in
                 match game_state.winner_is_black with
                 | Some true -> Dream.json "b"
                 | Some false -> Dream.json "w"
                 | None -> Dream.json "none");
             Dream.get "board" (fun request ->
                 let session_id =
                   match Dream.session "session" request with
                   | Some s -> s
                   | None -> failwith "Could not find session"
                 in
                 let game_id =
                   Hashtbl.find_exn session_id_to_game_id session_id
                 in
                 let game_state =
                   Hashtbl.find_exn game_id_to_game_state game_id
                 in
                 Dream.json (Board.Board.yojson_of_pieces game_state.board));
           ];
       ]
  @@ Dream.not_found
