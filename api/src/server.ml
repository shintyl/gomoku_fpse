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
       if len = 5 then Int.to_string (Random.int 10)
       else Int.to_string (Random.int 10) ^ gen (len + 1)
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
     let session_id_to_game_id = Hashtbl.create (module String) in
     let game_id_to_game_state = Hashtbl.create (module String) in

     Dream.run @@ Dream.logger @@ Dream.memory_sessions
     @@ Dream.router
          [
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

                        let _foo =
                          match
                            Board.Board.insert_check game_state.board
                              [ (1, 0); (1, 1); (0, 1); (-1, 1) ]
                              (coords.x, coords.y) color
                          with
                          | Error _ -> failwith "Invalid move"
                          | Ok true -> Dream.log "%b" game_state.is_black_turn
                          | Ok false -> Dream.log "None"
                        in

                        match
                          Board.Board.insert game_state.board (coords.x, coords.y)
                            color
                        with
                        | Error _ -> failwith "Invalid move"
                        | Ok new_board ->
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
                                };
                            Dream.json (Board.Board.yojson_of_pieces new_board)));
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
            Dream.get "/no_op" (fun _ -> Dream.json "no op");
          ]
     @@ Dream.not_found
