open Tylr_core;

let insert: (Zipper.t, string) => Zipper.t =
  (z, str) => {
    switch (Edit.perform(Insert(str), z)) {
    | None =>
      print_endline("WARNING: Store.insert failed");
      z;
    | Some(r) => r
    };
  };

let parse = (str: string): Zipper.t =>
  str
  |> Labeler.label
  |> List.map((x: Token.Unmolded.t) => x.text)
  |> List.fold_left(insert, Zipper.empty);

let serialize = z => z |> Zipper.sexp_of_t |> Sexplib.Sexp.to_string;

let deserialize = (str: string): Zipper.t =>
  try(str |> Sexplib.Sexp.of_string |> Zipper.t_of_sexp) {
  | _ => failwith("WARNING: Store.deserialize: exception during parse")
  };

let save_syntax_key: int => string =
  save_idx => "SAVE" ++ string_of_int(save_idx);

let save_syntax = (save_idx: int, z: Zipper.t) =>
  LocalStorage.set(save_syntax_key(save_idx), z |> serialize);

let editor_defaults = [
  serialize(Zipper.empty),
  serialize(parse("2")),
  serialize(parse("3 + 3")),
  serialize(parse("(4)")),
  serialize(parse("(5 + 5) * 5")),
  serialize(parse("let x = 6 in x")),
  serialize(parse("case 7\n| x => 7")),
  serialize(parse("let (a, b) =\n(8*9<6, 17==6) in\n(a,(a, b))")),
  serialize(parse("let f = fun z -> 9 in f(9)")),
  serialize(parse("0")),
  //serialize(parse("let x = 3 in x")),
  //serialize(parse("let f = fun x -> 4 in f(4)")),
  //serialize(parse("case 5 | x => 5")),
];

let load_default_syntax: int => Zipper.t =
  save_idx =>
    switch (List.nth_opt(editor_defaults, save_idx)) {
    | None => Zipper.empty
    | Some(str) => deserialize(str)
    };

let load_syntax: int => Zipper.t =
  save_idx =>
    switch (LocalStorage.get(save_syntax_key(save_idx))) {
    | None => load_default_syntax(save_idx)
    | Some(str) => deserialize(str)
    };

//let unparse: Zipper.t => string = z => z |> Zipper.zip |> Cell.tokens;

let _editor_defaults = [
  "",
  "",
  "fun center, p ->
  let x1, y1 = center in
  let x2, y2 = p in
  let r = sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2)) in
  circle(center, r)
  ",
  "",
  "shapes
  |> map(rotate(pi / 4))
  |> map(translate(6, 7))
  |> filter(fun shape -> area(shape) < 50)
  |> map(dilate(5))
  ",
  "",
  "fun square, p1, p2 ->
  if square then
  let mark =
  fun center ->
  let x, y = center in
  rect(x - 2, y - 2, 4, 4)
  in
  [mark(p1); line(p1, p2); mark(p2)]
  else
  let mark =
  fun center ->
  let r = 4 in
  circle(center, 4)
  in
  [mark(p1); line(p1, p2); mark(p2)]
  ",
  "let ss1 = observe(msg, map_rotate(map_dilate(shapes))) in
  let ss2 = map_brighten(shapes) in
  [ss1; ss2]
  ",
  "let foo =
  fun taz ->
  case taz of
  | (2, torb) -> bargle + 7*torb
  | (blee, 5) -> krunk ? blee : 66
  in foo(0!)",
  "let foo = fun taz -> (fun bar -> (taz + 2*bar)) in foo(1!)",
];
