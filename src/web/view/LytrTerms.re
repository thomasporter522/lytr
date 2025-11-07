open Virtual_dom.Vdom;
open Tylr_core;

/* Create styled view nodes that mimic the rich token system */
let mk_styled_token = (~text, ~classes, ()) => {
  Node.span(
    ~attrs=[Attr.classes(["token", "lytr-token", ...classes])],
    [Node.text(text)],
  );
};

let mk_operator_token = (~text, ()) =>
  mk_styled_token(~text, ~classes=["operator", "tile"], ());

let mk_paren_token = (~text, ()) =>
  mk_styled_token(~text, ~classes=["paren", "tile"], ());

let mk_atom_token = (~text, ()) =>
  mk_styled_token(~text, ~classes=["atom", "tile"], ());

let mk_unlexed_token = (~text, ()) =>
  mk_styled_token(~text, ~classes=["atom", "tile", "unlexed"], ());

let mk_grout = (~font, ()) => {
  /* Create a simple inline grout symbol without complex positioning */
  Node.span(
    ~attrs=[Attr.classes(["token", "lytr-token", "hole", "lytr-grout"])],
    [
      Node.text(Util.Unicode.nbsp),
      Dec.Box.mk(
        ~font,
        ~loc={
          row: 0,
          col: 0,
        },
        [
          Dec.Token.hexagon(
            Dec.Token.Style.{
              sort: Sort.root,
              shape: (Tip.Conc, Tip.Conc),
              sil: false,
            },
            1,
          ),
        ],
      ),
    ],
  );
};

let mk_hole = (~font, ()) => {
  /* Create a simple inline grout symbol without complex positioning */
  Node.span(
    ~attrs=[Attr.classes(["token", "lytr-token", "hole", "lytr-grout"])],
    [
      Node.text(Util.Unicode.nbsp),
      Dec.Box.mk(
        ~font,
        ~loc={
          row: 0,
          col: 0,
        },
        [
          Dec.Token.hexagon(
            Dec.Token.Style.{
              sort: Sort.root,
              shape: (Tip.Conv, Tip.Conv),
              sil: false,
            },
            1,
          ),
        ],
      ),
    ],
  );
};

let mk_error_token = (~text, ()) =>
  mk_styled_token(~text, ~classes=["error", "grout"], ());

/* Helper function to intersperse grout between terms */
let rec intersperse_grout = (~font, ~cursor: int, ~text: string, nodes: list(Node.t)): list(Node.t) =>
  switch (nodes) {
  | [] => []
  | [single] => [single]
  | [first, ...rest] => [
      first,
      mk_grout(~font, ()),
      ...intersperse_grout(~font, rest),
    ]
  };

/* Convert LytrParser terms to styled nodes WITHOUT grout interspersion */
let rec view_lytr_terms_no_grout =
        (~font, ~cursor: int, ~text: string, terms: LytrParser.terms): list(Node.t) =>
  switch (terms) {
  | LytrParser.Nil => []
  | LytrParser.Cons(rest, sharded) =>
    view_lytr_terms_no_grout(~font, rest)
    @ [view_lytr_sharded(~font, sharded)]
  }

/* Convert LytrParser terms to styled nodes WITH grout interspersion */
and view_lytr_terms = (~font, ~cursor: int, ~text: string, terms: LytrParser.terms): list(Node.t) => {
  let nodes = view_lytr_terms_no_grout(~font, terms);
  if (List.length(nodes) == 0) {
    [mk_hole(~font, ())];
  } else {
    intersperse_grout(~font, nodes);
  };
}

and view_lytr_sharded =
    (~font, ~cursor: int, ~text: string, sharded: LytrParser.sharded(LytrParser.term)): Node.t =>
  switch (sharded) {
  | LytrParser.Shard(token) =>
    let text = LytrToken.string_of_token(token);
    mk_error_token(~text, ());
  | LytrParser.Form(term) => view_lytr_term(~font, term)
  }

and view_lytr_term = (~font, ~cursor: int, ~text: string, term: LytrParser.term): Node.t =>
  switch (term) {
  | LytrParser.Parens(inner_terms) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-parens")],
      [mk_paren_token(~text="(", ())]
      @ view_lytr_terms(~font, inner_terms)
      @ [mk_paren_token(~text=")", ())],
    )
  | LytrParser.Times(left, right) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-times")],
      [
        view_lytr_child(~font, left),
        mk_operator_token(~text="*", ()),
        view_lytr_child(~font, right),
      ],
    )
  | LytrParser.Negative(child) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-negative")],
      [mk_operator_token(~text="-", ()), view_lytr_child(~font, child)],
    )
  | LytrParser.Minus(left, right) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-minus")],
      [
        view_lytr_child(~font, left),
        mk_operator_token(~text="-", ()),
        view_lytr_child(~font, right),
      ],
    )
  | LytrParser.Atom(atom) =>
    switch (atom) {
    | Unlexed(s) => mk_unlexed_token(~text=s, ())
    | _ =>
      let text = LytrToken.string_of_atom(atom);
      mk_atom_token(~text, ());
    }

  | LytrParser.DEBUG => mk_error_token(~text="DEBUG", ())
  }

and view_lytr_child = (~font, ~cursor: int, ~text: string, child: LytrParser.child): Node.t =>
  switch (child) {
  | LytrParser.Hole => mk_hole(~font, ())
  | LytrParser.Term(term) => view_lytr_term(~font, term)
  };

/* Rich view function using styled tokens */
let view_lytr_text = (~font, ~cursor: int, ~text: string, terms: LytrParser.terms): Node.t => {
  let styled_line = view_lytr_terms(~font, terms);
  Node.div(
    ~attrs=[Attr.classes(["block", "lytr-block"])],
    [Node.span(~attrs=[Attr.class_("line")], styled_line)],
  );
};

/* Fallback simple view function */
let rec view_lytr_terms = (~font, terms: LytrParser.terms): list(Node.t) =>
  switch (terms) {
  | LytrParser.Nil => []
  | LytrParser.Cons(rest, sharded) =>
    view_lytr_terms(~font, rest) @ [view_lytr_sharded(~font, sharded)]
  }

and view_lytr_sharded =
    (~font, sharded: LytrParser.sharded(LytrParser.term)): Node.t =>
  switch (sharded) {
  | LytrParser.Shard(token) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-shard")],
      [Node.text("💥" ++ LytrToken.string_of_token(token) ++ "💥")],
    )
  | LytrParser.Form(term) => view_lytr_term(~font, term)
  }

and view_lytr_term = (~font, term: LytrParser.term): Node.t =>
  switch (term) {
  | LytrParser.Parens(inner_terms) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-parens")],
      [Node.text("(")]
      @ view_lytr_terms(~font, inner_terms)
      @ [Node.text(")")],
    )
  | LytrParser.Times(left, right) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-times")],
      [
        view_lytr_child(~font, left),
        Node.text("*"),
        view_lytr_child(~font, right),
      ],
    )
  | LytrParser.Negative(child) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-negative")],
      [Node.text("-"), view_lytr_child(~font, child)],
    )
  | LytrParser.Minus(left, right) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-minus")],
      [
        view_lytr_child(~font, left),
        Node.text("-"),
        view_lytr_child(~font, right),
      ],
    )
  | LytrParser.Atom(atom) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-atom")],
      [Node.text(LytrToken.string_of_atom(atom))],
    )
  | LytrParser.DEBUG =>
    Node.span(~attrs=[Attr.class_("lytr-debug")], [Node.text("DEBUG")])
  }

and view_lytr_child = (~font, child: LytrParser.child): Node.t =>
  switch (child) {
  | LytrParser.Hole =>
    Node.span(~attrs=[Attr.class_("lytr-hole")], [Node.text("?")])
  | LytrParser.Term(term) => view_lytr_term(~font, term)
  };
