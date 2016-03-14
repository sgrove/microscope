open Lwt.Infix
module Opi = Opium.Std
open Opium.Std
module Websocket = Websocket_lwt

let section = Lwt_log.Section.make "microscope"
let setup_logging () =
  let open Lwt_log in
  default := channel
               ~close_mode:`Keep
               ~channel:Lwt_io.stderr
               ~template:"$(name): $(message)"
               ();
  Section.set_level section Debug

let infof = Lwt_log.ign_info_f ~section

type user_id = int [@@deriving yojson]

type post_id = int [@@deriving yojson]

type comment_id = int [@@deriving yojson]

type date = int [@@deriving yojson]

type user =
    {
      id: user_id;
      username: string;
      email: string;
      password: string;
      created_at: date;
    } [@@deriving yojson]

type post =
    {
      id: post_id;
      title: string;
      body: string;
      user_id: user_id;
      created_at: date;
    } [@@deriving yojson]

type comment =
    {
      id: comment_id;
      body: string;
      user_id: user_id;
      post_id: post_id;
      created_at: date;
    } [@@deriving yojson]

let a_user =
  { id = 1;
    username = "dww";
    email = "dwwoelfel@gmail.com";
    password = "password";
    created_at = 0;
  }

let a_post = {
  id = 1;
  title = "Microscope in OCaml";
  body = "A new post";
  user_id = 1;
  created_at = 1;
}

let a_comment = {
  id = 1;
  body = "First psot1";
  user_id = 1;
  post_id = 1;
  created_at = 2;
}

let a_posts = 
  [a_post
  ;
    {
      id = 1;
      title = "Microscope in OCaml";
      body = "A new post";
      user_id = 1;
      created_at = 1;
    }
  ; {
    id = 1;
    title = "How to build a <strong onclick='alert(\"wow\")'>js_of_ocaml</strong> client";
    body = "A new post";
    user_id = 1;
    created_at = 1;
  }
  ]

let a_users =
  [a_user]

let () =
  Printf.printf "%s: %s by %s\n" a_post.title a_post.body a_user.username

type person = {
  name: string;
  age: int;
} [@@deriving yojson]

let ws_connections = Hashtbl.create 17

type ws_opt = string [@@deriving yojson] 

type ws_data =
    {
      posts: post list;
    } [@@deriving yojson]

type ws_message =
    {
      op: ws_opt;
      data: ws_data;
    } [@@deriving yojson]

let ws_handler id req recv send =
  (try
      Hashtbl.find ws_connections id
    with Not_found ->
      Hashtbl.add ws_connections id ();
      infof "New websocket connection (id=%d)" id;
      Lwt.async(fun () ->
                let msg = {
                  op = "initial_data";
                  data = {
                    posts = a_posts;
                  };
                } in
                send @@ Websocket.Frame.create ~content:(Yojson.Safe.to_string (ws_message_to_yojson msg)) ()
               )
  );
  let rec recv_forever () =
    let open Websocket.Frame in
    let react fr =
      Lwt_log.debug_f ~section "<- %s" (Websocket.Frame.show fr) >>= fun () ->
      match fr.opcode with
      | Opcode.Ping ->
         send @@ Websocket.Frame.create ~opcode:Opcode.Pong ~content:fr.content ()

      | Opcode.Close ->
         Lwt_log.info_f ~section "Client %d sent a close frame" id >>= fun () ->
         (* Immediately echo and pass this last message to the user *)
         (if String.length fr.content >= 2 then
            send @@ Websocket.Frame.create ~opcode:Opcode.Close
                                           ~content:(String.sub fr.content 0 2) ()
          else send @@ Websocket.Frame.close 1000) >>= fun () ->
         Lwt.fail Exit

      | Opcode.Pong -> Lwt.return_unit

      | Opcode.Text
      | Opcode.Binary -> send @@ Websocket.Frame.create ~content:fr.content ()

      | _ ->
         send @@ Websocket.Frame.close 1002 >>= fun () ->
         Lwt.fail Exit
    in
    recv () >>= react >>= recv_forever
  in
  try%lwt
        recv_forever ()
  with exn ->
    Lwt_log.info_f ~section "Connection to client %d lost" id >>= fun () ->
    Hashtbl.remove ws_connections id;
    Lwt.fail exn

let main uri () =
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp ->
  Conduit_lwt_unix.(endp_to_server ~ctx:default_ctx endp >>= fun server ->
                    Websocket.establish_server ~ctx:default_ctx ~mode:server ws_handler)

let json_of_post { id ; title ; body ; user_id ; created_at } =
  let open Ezjsonm in
  dict [ "id", (int id)
       ; "title", (string title)
       ; "body", (string body)
       ; "user_id", (int user_id)
       ; "created_at", (int created_at)
       ]

let html_of_post { id ; title ; body ; user_id ; created_at } =
  let user = List.find ~f:(fun (user : user) -> user.id = user_id) a_users in
  Printf.sprintf "<div><h1>%s <small>- by %s</small></h1><p>%s</p>" title user.username body

let print_param = put "/hello/:name" begin fun req ->
                                           `String ("Hello " ^ param req "name") |> respond'
                                     end

let get_post = 
  get "/posts/:post_id" begin 
        fun req ->
        try
          let post_id = "post_id" |> param req |> int_of_string in
          let post = List.find ~f:(fun (post : post) -> post.id = post_id) a_posts in
          `Json (post |> json_of_post |> Ezjsonm.wrap) |> respond'
        with
        | Not_found -> (Opium_kernel.Rock.Handler.not_found req)
      end

let get_index =
  get "/" begin
        fun req ->
        `String (Printf.sprintf "<!DOCTYPE HTML>\n<html><head><link href='/css/style.css' rel='stylesheet'></head><body><div class='container'><div id='main'><div class='posts page'><div class='wrapper'><div id='main-area'></div></div></div></div></div><script src='https://cdnjs.cloudflare.com/ajax/libs/react/0.14.7/react-with-addons.js' type='text/javascript'></script><script src='https://cdnjs.cloudflare.com/ajax/libs/react/0.14.7/react-dom.js' type='text/javascript'></script><script src='/js/client.js' type='text/javascript'></script></body></html>")  |> respond'
      end

let _ =
  setup_logging ();

  infof "starting websocket server";

  let port = ref 3000 in
  let ws_port = !port + 1 in
  let address = ref "localhost" in
  let ws_uri = Printf.sprintf "http://%s:%d" !address ws_port in

  Lwt.async @@ main @@ Uri.of_string ws_uri;

  infof "starting http server";

  let static =
    Middleware.static ~local_path:"./resources/public" ~uri_prefix:"/" in
  App.empty
  |> App.port !port
  |> print_param
  |> get_post
  |> get_index
  |> middleware static
  |> App.run_command
