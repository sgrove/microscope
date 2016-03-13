module Html = Dom_html

let react =
  Js.Unsafe.variable "React"

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
  [| a_post
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
    |]

let a_users =
  [a_user]

let () =
  print_endline "TESTING";
  Printf.printf "%s: %s by %s\n" a_post.title a_post.body a_user.username

let my_add a b =
  a + b

let () =
  print_endline "Hello world";
  print_endline "Just works?";
  print_endline "Pretty fast at this stage..."

let () =
  let x = 10 in
  let y = 20 in
  Printf.printf "Sum of %d + %d = %d\n" x y (my_add x y)

let to_obj l = Js.Unsafe.obj @@ Array.of_list l
let jss s = Js.string s
let inj o = Js.Unsafe.inject o
let ins s = inj @@ jss s


let console =
  Js.Unsafe.variable "console"

let log msg = 
  Js.Unsafe.meth_call console "log" [| inj @@ msg |]

(* TODO: Handle decodeURIComponent, etc. - see https://ocsigen.org/js_of_ocaml/api/Js#2_StandardJavascriptfunctions *)
let page_params =
  let url = (Js.to_string Html.window##.location##.href) in
  let query_string = (String.concat "?" (List.tl (Regexp.split (Regexp.regexp "\\?") url))) in
  let kv_pairs = (Regexp.split (Regexp.regexp "&") query_string) in
  List.fold_left (fun run next ->
                  match (Regexp.split (Regexp.regexp "=") next) with
                  | key::value -> List.append run [(key, (String.concat "" value))]
                  | _ -> run)
                 [] kv_pairs

let get_query_param params ?default:(default="") key =
  (try
      List.assoc key page_params
    with
    | Not_found -> default)

module type REACT = sig
    type component

    type value_t = unit

    val component : (value_t -> component) -> (value_t -> component)
    val element_of_tag : string -> (string * Js.Unsafe.any) list -> string -> component
    val root : component -> Dom_html.element Js.t -> unit
    val render : component -> Dom_html.element Js.t -> unit
  end

module React:REACT = struct
  let react = (Js.Unsafe.variable "React")
  let react_impl = (Js.Unsafe.variable "ReactDOM")

  type component = Js.Unsafe.any
  type value_t = unit

  let component renderer =
    let rfun this _ =
      let props = Js.Unsafe.get this "props" in
      let value = Js.Unsafe.get props "value" in

      renderer value
    in
    let opts = to_obj [("render", inj @@ Js.wrap_meth_callback rfun)] in
    let comp = Js.Unsafe.meth_call react "createClass" [| opts |] in
    let r value =
      let opts = to_obj [("value", inj value)] in
      Js.Unsafe.meth_call react "createElement" [| comp; opts |]
    in
    r

  let element_of_tag tag opts children =
    Js.Unsafe.meth_call react "createElement"
                        [| inj @@ jss "div";
                           inj @@ to_obj opts;
                           inj @@ jss children |]

  let root comp node =
    let el = Js.Unsafe.meth_call react "createElement" [| inj @@ comp |] in
    print_endline "Rooting component:";
    ignore(log el);
    Js.Unsafe.meth_call react_impl "render" [| inj el; inj node |]

  let render comp node =
    Js.Unsafe.meth_call react_impl "render" [| inj comp; inj node |]
end

let box = 
  React.component
    (fun v ->
     React.element_of_tag "div" [("className", inj @@ jss "commentBox")] "This is a new commentBox")

let () =
  Js.Unsafe.set Html.window "myboxfn" box

type person = 
    {
      name : string
    ; age : int
    }

let colors =
  [|"blue" ; "green" ; "red" ; "purple" ; "pink" |]

let rand_color () =
  let n = Random.int (Array.length colors) in
  Array.get colors n


let js_get =
  Js.Unsafe.get

let el tag ?p:(props=[]) children =
  let mapped_children = Array.map (fun child -> inj child) children in
  let args = Array.append [| ins tag; inj @@ to_obj props |] mapped_children in
  Js.Unsafe.meth_call react "createElement" args

let div ?p:(p=[]) children =
  el "div" ~p children

let a ?p:(p=[]) children =
  el "a" ~p children

let span ?p:(p=[]) children =
  el "span" ~p children

let h3 ?p:(p=[]) children =
  el "h3" ~p children

let p ?props:(props=[]) children =
  el "p" ~p:props children

let post_preview_comp = 
  object%js (self)
    val displayName = jss "PostPreview"
    method render =
      let props = js_get self "props" in
      let post =  js_get props "post" in
      let url = "/" in
      let domain = "www.example.com" in
      div ~p:[("className", ins "post")]
          [|
            (a ~p:[("href", ins "#");
                   ("className", ins "upvote btn btn-default {{upvotedClass}}");]
               [| ins "⬆" |])

          ; (div ~p:[("className", ins "post-content");]
                 [| (h3 [|
                         (a ~p:[("href", ins url);] [| ins post.title |]) 
                       ; (span [| ins domain |])
                        |])
                  ; (p [|
                         ins ((string_of_int 10) ^ " Votes, submitted by " ^ "SOME USER,")
                       ; (a ~p:[("href", ins "/posts/1")] [|
                              (ins "99 comments")
                            |])
                        |])
                   (* TODO: Add edit link if current user *)
                   |])
          ; (a ~p:[("href", ins "/posts/1")
                  ; ("className", ins "discuss btn btn-default")]
               [|
                 ins "Discuss"
                |])
           |]
  end

(* 
{h2 | class="Whatever" post.title}

 *)

let start _:(bool Js.t) =
  Printf.printf "Start\n";
  let react = Js.Unsafe.variable "React" in
  let entry_div = Dom_html.getElementById "main-area" in
  Printf.printf "Rendering component:\n";
  let rclass = Js.Unsafe.meth_call react "createClass" [|(inj post_preview_comp)|] in
  let make_post post = Js.Unsafe.meth_call react "createElement" [| inj @@ rclass; inj @@ to_obj ["post", inj post] |] in
  let posts = Array.map make_post a_posts in
  let container = div posts in
  React.render container entry_div;
  Printf.printf "Finished initial rendering\n";
  Js._false


let () =
  let my_obj = object%js (self)
                 val x = 3
                 val name = "Sean Grove"
               end
  in
  print_endline "Ok, starting up";
  Js.Unsafe.set Html.window "my_obj" my_obj;
  Printf.printf "value: %s\n" my_obj##.name;
  Js.Unsafe.set Html.window "onload" (Dom.handler start)
