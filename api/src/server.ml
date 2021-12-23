open Core

type message_object = { message : string } [@@deriving yojson]

type coords_object = { x : int; y : int } [@@deriving yojson]

let serialize_message msg =
  { message = msg } |> message_object_to_yojson |> Yojson.Safe.to_string
  |> Dream.json

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
}

let () =
  Random.self_init ();

  let sessions = Hash_set.create (module String) in
  let session_id_to_ws = Hashtbl.create (module String) in
  let session_id_to_game_id = Hashtbl.create (module String) in
  let game_id_to_game_state = Hashtbl.create (module String) in

  let session_id_from_request request =
    match Dream.session "session" request with
    | Some s -> s
    | None -> failwith "Could not find session"
  in

  let session_id_lookup request =
    let session_id =
      match Dream.session "session" request with
      | Some s -> s
      | None -> failwith "Could not find session"
    in
    let game_id = Hashtbl.find_exn session_id_to_game_id session_id in
    (session_id, game_id, Hashtbl.find_exn game_id_to_game_state game_id)
  in

  let session_refresh request =
    let rand_num = six_digit_random () in
    Hash_set.add sessions rand_num;
    match Dream.session "session" request with
    | Some s ->
        Hash_set.remove sessions s;
        let%lwt () = Dream.invalidate_session request in
        let%lwt () = Dream.put_session "session" rand_num request in
        serialize_message rand_num
    | None ->
        let%lwt () = Dream.put_session "session" rand_num request in
        serialize_message rand_num
  in

  let session_get request = session_id_from_request request |> Dream.json in

  let game_connect request =
    Dream.websocket (fun websocket ->
        let session_id = session_id_from_request request in
        Hashtbl.set session_id_to_ws ~key:session_id ~data:websocket;
        let rec loop () =
          match%lwt Dream.receive websocket with
          | Some _ -> loop ()
          | None -> Dream.close_websocket websocket
        in
        loop ())
  in

  let game_create request =
    let session_id = session_id_from_request request in
    let%lwt body = Dream.body request in
    let other_session_id =
      let temp = deserialize_message body in
      match Hash_set.mem sessions temp with
      | false -> failwith "Could not find other client's session"
      | true -> temp
    in
    let game_id = other_session_id ^ session_id in
    Hashtbl.add_exn session_id_to_game_id ~key:other_session_id ~data:game_id;
    Hashtbl.add_exn session_id_to_game_id ~key:session_id ~data:game_id;
    Hashtbl.add_exn game_id_to_game_state ~key:game_id
      ~data:
        {
          board = Board.Board.Vect2Map.empty;
          is_black_turn = true;
          b_id = session_id;
          w_id = other_session_id;
          winner_is_black = None;
        };
    match
      ( Hashtbl.find_exn session_id_to_ws other_session_id,
        Hashtbl.find_exn session_id_to_ws session_id )
    with
    | ws_b, ws_w ->
        let%lwt () = Dream.send ws_b "game_start" in
        let%lwt () = Dream.send ws_w "game_start" in
        serialize_message "OK"
  in

  let game_create_ai_opponent request =
    let session_id = session_id_from_request request in
    let game_id = session_id in
    Hashtbl.add_exn session_id_to_game_id ~key:session_id ~data:game_id;
    Hashtbl.add_exn game_id_to_game_state ~key:game_id
      ~data:
        {
          board = Board.Board.Vect2Map.empty;
          is_black_turn = true;
          b_id = session_id;
          w_id = "";
          winner_is_black = None;
        };
    match Hashtbl.find_exn session_id_to_ws session_id with
    | ws_b ->
        let%lwt () = Dream.send ws_b "game_start" in
        serialize_message "OK"
  in

  let game_make_move request =
    let get_color game_state session_id =
      match
        (game_state.is_black_turn, String.equal game_state.b_id session_id)
      with
      | true, true -> 'b'
      | false, false -> 'w'
      | _, _ -> failwith "Invalid turn"
    in
    let get_winner game_state coords color =
      match
        Board.Board.insert_check game_state.board
          [ (1, 0); (1, 1); (0, 1); (-1, 1) ]
          (coords.x, coords.y) color
      with
      | Error _ -> failwith "Invalid move"
      | Ok true -> Some game_state.is_black_turn
      | Ok false -> None
    in
    let get_board_after_insertion game_state coords color =
      match Board.Board.insert game_state.board (coords.x, coords.y) color with
      | Error _ -> failwith "Invalid move"
      | Ok new_board -> new_board
    in
    let update_game_state game_state game_id coords color winner_is_black =
      Hashtbl.set game_id_to_game_state ~key:game_id
        ~data:
          {
            board = get_board_after_insertion game_state coords color;
            is_black_turn = not game_state.is_black_turn;
            b_id = game_state.b_id;
            w_id = game_state.w_id;
            winner_is_black;
          }
    in
    let send_ws_notif game_state winner_is_black =
      match
        ( Hashtbl.find_exn session_id_to_ws game_state.b_id,
          Hashtbl.find_exn session_id_to_ws game_state.w_id )
      with
      | ws_b, ws_w ->
          let turn = if not game_state.is_black_turn then "b" else "w" in
          let is_winner =
            match winner_is_black with Some _ -> true | None -> false
          in
          let msg = if is_winner then "game_complete" else turn in
          let%lwt () = Dream.send ws_b msg in
          let%lwt () = Dream.send ws_w msg in
          serialize_message "OK"
    in
    let%lwt body = Dream.body request in
    let coords = deserialize_coords body in
    let session_id, game_id, game_state = session_id_lookup request in
    match game_state.winner_is_black with
    | Some _ -> failwith "Game completed"
    | None ->
        let color = get_color game_state session_id in
        let winner_is_black = get_winner game_state coords color in
        update_game_state game_state game_id coords color winner_is_black;
        send_ws_notif game_state winner_is_black
  in

  let game_make_move_ai request =
    let%lwt body = Dream.body request in
    let coords = deserialize_coords body in
    let _, game_id, game_state = session_id_lookup request in
    let ws_b = Hashtbl.find_exn session_id_to_ws game_state.b_id in
    let board_after_insertion =
      match Board.Board.insert game_state.board (coords.x, coords.y) 'b' with
      | Error _ -> failwith "Invalid move"
      | Ok new_board -> new_board
    in
    match
      Board.Board.insert_check game_state.board
        [ (1, 0); (1, 1); (0, 1); (-1, 1) ]
        (coords.x, coords.y) 'b'
    with
    | Error _ -> failwith "Invalid move"
    | Ok true ->
        Hashtbl.set game_id_to_game_state ~key:game_id
          ~data:
            {
              board = board_after_insertion;
              is_black_turn = not game_state.is_black_turn;
              b_id = game_state.b_id;
              w_id = game_state.w_id;
              winner_is_black = Some true;
            };
        let%lwt () = Dream.send ws_b "game_complete" in
        serialize_message "OK"
    | Ok false -> (
        Hashtbl.set game_id_to_game_state ~key:game_id
          ~data:
            {
              board = board_after_insertion;
              is_black_turn = not game_state.is_black_turn;
              b_id = game_state.b_id;
              w_id = game_state.w_id;
              winner_is_black = game_state.winner_is_black;
            };
        let%lwt () = Dream.send ws_b "w" in
        let _, ai_coord =
          Ai.advantage_max board_after_insertion
            [ (1, 0); (1, 1); (0, 1); (-1, 1) ]
            (19, 19) 'w' 'b'
        in
        let board_after_ai_insertion =
          match Board.Board.insert board_after_insertion ai_coord 'w' with
          | Error _ -> failwith "Invalid move"
          | Ok new_board -> new_board
        in
        match
          Board.Board.insert_check board_after_insertion
            [ (1, 0); (1, 1); (0, 1); (-1, 1) ]
            ai_coord 'w'
        with
        | Error _ -> failwith "Invalid move"
        | Ok true ->
            Hashtbl.set game_id_to_game_state ~key:game_id
              ~data:
                {
                  board = board_after_ai_insertion;
                  is_black_turn = not game_state.is_black_turn;
                  b_id = game_state.b_id;
                  w_id = game_state.w_id;
                  winner_is_black = Some false;
                };
            let%lwt () = Dream.send ws_b "game_complete" in
            serialize_message "OK"
        | Ok false ->
            Hashtbl.set game_id_to_game_state ~key:game_id
              ~data:
                {
                  board = board_after_ai_insertion;
                  is_black_turn = not game_state.is_black_turn;
                  b_id = game_state.b_id;
                  w_id = game_state.w_id;
                  winner_is_black = game_state.winner_is_black;
                };
            let%lwt () = Dream.send ws_b "b" in
            serialize_message "OK")
  in

  let game_winner request =
    let _, _, game_state = session_id_lookup request in
    let msg =
      match game_state.winner_is_black with
      | Some true -> "b"
      | Some false -> "w"
      | None -> "none"
    in
    serialize_message msg
  in

  let game_board request =
    let _, _, game_state = session_id_lookup request in
    Dream.json (Board.Board.yojson_of_pieces game_state.board)
  in

  let game_color request =
    let session_id, _, game_state = session_id_lookup request in
    let msg =
      match String.( = ) session_id game_state.w_id with
      | true -> "w"
      | false -> "b"
    in
    serialize_message msg
  in

  Dream.run ~interface:"0.0.0.0"
    (Dream.logger @@ Dream.memory_sessions
    @@ Dream.router
         [
           Dream.scope "/session" []
             [
               Dream.post "/refresh" session_refresh;
               Dream.get "/get" session_get;
             ];
           Dream.scope "/game" []
             [
               Dream.get "/connect" game_connect;
               Dream.post "/create" game_create;
               Dream.post "/make_move" game_make_move;
               Dream.get "/winner" game_winner;
               Dream.get "/board" game_board;
               Dream.get "/color" game_color;
               Dream.post "/make_move_ai" game_make_move_ai;
               Dream.post "/create_ai_opponent" game_create_ai_opponent;
             ];
         ]
    @@ Dream.not_found)
