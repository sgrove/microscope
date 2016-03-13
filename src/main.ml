open Lwt.Infix
module Opi = Opium.Std
open Opium.Std

type user_id = int

type post_id = int

type comment_id = int

type date = int

type user =
    {
      id: user_id;
      username: string;
      email: string;
      password: string;
      created_at: date;
    }

type post =
    {
      id: post_id;
      title: string;
      body: string;
      user_id: user_id;
      created_at: date;
    }

type comment =
    {
      id: comment_id;
      body: string;
      user_id: user_id;
      post_id: post_id;
      created_at: date;
    }

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
}

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
  let static =
    Middleware.static ~local_path:"./resources/public" ~uri_prefix:"/" in
  App.empty
  |> print_param
  |> get_post
  |> get_index
  |> middleware static
  |> App.run_command
