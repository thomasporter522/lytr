open Virtual_dom.Vdom;
open Tylr_core;
open LytrGrammar;
open LytrAbstractor;

let string_of_secondary_token = (s: secondary_token) =>
  switch (s) {
  | Whitespace(s) => s
  | Unlexed(s) => s
  };

let string_of_atom = (a: atom) =>
  switch (a) {
  | Numlit(n) => string_of_int(n)
  | Identifier(s) => s
  };

let string_of_primary_token = (t: primary_token) =>
  switch (t) {
  | BOF => "#"
  | EOF => "#"
  | TOP => "("
  | TCP => ")"
  | TAtom(a) => string_of_atom(a)
  | TPlus => "+"
  | TMinus => "-"
  | TTimes => "*"
  | TDivide => "/"
  | TDoubleDivide => "//"
  | TModulo => "%"
  | TFun => "fun"
  | TArrow => "->"
  | TLet => "let"
  | TEquals => "="
  | TIn => "in"
  | TType => "type"
  | TCase => "case"
  | TPipe => "|"
  | TDoubleArrow => "=>"
  | TEnd => "end"
  | TIf => "if"
  | TThen => "then"
  | TElse => "else"
  | TColon => ":"
  | TComma => ","
  };

let string_of_token = (t: token) =>
  switch (t) {
  | Primary(t) => string_of_primary_token(t)
  | Secondary(s) => string_of_secondary_token(s)
  };

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

let mk_grout_dot = () => {
  Node.span(
    ~attrs=[Attr.class_("lytr-grout")],
    [mk_styled_token(~text="·", ~classes=["hole", "lytr-grout"], ())],
  );
};

let mk_grout = (~font as _, ()) => {
  mk_grout_dot();
};

let mk_hole_circ = () => {
  Node.span(
    ~attrs=[Attr.class_("lytr-grout")],
    [mk_styled_token(~text="○", ~classes=["hole", "lytr-grout"], ())],
  );
};

let mk_hole = (~font as _, ()) => {
  mk_hole_circ();
};

let mk_error_token = (~text, ()) =>
  mk_styled_token(~text, ~classes=["error", "grout"], ());

/* Helper function to intersperse grout between terms */
let rec intersperse_grout = (~font, nodes: list(Node.t)): list(Node.t) =>
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
let rec view_lytr_terms_no_grout = (~font, terms: terms): list(Node.t) =>
  switch (terms) {
  | Nil => []
  | Cons(rest, sharded) =>
    view_lytr_terms_no_grout(~font, rest)
    @ [view_lytr_sharded(~font, sharded)]
  }

/* Convert LytrParser terms to styled nodes WITH grout interspersion */
and view_lytr_terms = (~font, terms: terms): list(Node.t) => {
  let nodes = view_lytr_terms_no_grout(~font, terms);
  if (List.length(nodes) == 0) {
    [mk_hole(~font, ())];
  } else {
    intersperse_grout(~font, nodes);
  };
}

and view_lytr_sharded = (~font, sharded: LytrParser.sharded(term)): Node.t =>
  switch (sharded) {
  | Secondary(token) =>
    let text = string_of_secondary_token(token);
    mk_atom_token(~text, ());
  | Shard(token) =>
    let text = string_of_primary_token(token);
    mk_error_token(~text, ());
  | Form(term) => view_lytr_term(~font, term)
  }

and view_lytr_term = (~font, term: term): Node.t =>
  switch (term) {
  | Tuple(inner_term_list) =>
    let tuple_elements =
      List.fold_right(
        (terms, acc) =>
          [Node.span(~attrs=[], view_lytr_terms(~font, terms)), ...acc],
        inner_term_list,
        [],
      );
    let interspersed_elements =
      List.fold_right(
        (elem, acc) =>
          switch (acc) {
          | [] => [elem]
          | _ => [elem, mk_operator_token(~text=",", ()), ...acc]
          },
        tuple_elements,
        [],
      );
    Node.span(
      ~attrs=[Attr.class_("lytr-tuple")],
      [mk_paren_token(~text="(", ())]
      @ interspersed_elements
      @ [mk_paren_token(~text=")", ())],
    );
  | Binop(binop, left, right) =>
    let op_text =
      switch (binop) {
      | Plus => "+"
      | Minus => "-"
      | Times => "*"
      | Divide => "/"
      | DoubleDivide => "//"
      | Modulo => "%"
      };
    Node.span(
      ~attrs=[Attr.class_("lytr-binop")],
      [
        view_lytr_child(~font, left),
        mk_operator_token(~text=op_text, ()),
        view_lytr_child(~font, right),
      ],
    );
  | Unop(unop, child) =>
    let op_text =
      switch (unop) {
      | Minus => "-"
      };
    Node.span(
      ~attrs=[Attr.class_("lytr-unop")],
      [mk_operator_token(~text=op_text, ()), view_lytr_child(~font, child)],
    );
  | Atom(atom) =>
    let text = string_of_atom(atom);
    mk_atom_token(~text, ());
  | Fun(params, body) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-fun")],
      [mk_atom_token(~text="fun", ())]
      @ view_lytr_terms(~font, params)
      @ [mk_atom_token(~text="->", ())]
      @ [view_lytr_child(~font, body)],
    )
  | Ap(func, args) =>
    let arg_elements =
      List.fold_right(
        (terms, acc) =>
          [Node.span(~attrs=[], view_lytr_terms(~font, terms)), ...acc],
        args,
        [],
      );
    let interspersed_args =
      List.fold_right(
        (elem, acc) =>
          switch (acc) {
          | [] => [elem]
          | _ => [elem, mk_operator_token(~text=",", ()), ...acc]
          },
        arg_elements,
        [],
      );
    Node.span(
      ~attrs=[Attr.class_("lytr-ap")],
      [view_lytr_child(~font, func)]
      @ [mk_paren_token(~text="(", ())]
      @ interspersed_args
      @ [mk_paren_token(~text=")", ())],
    );
  | Let(bindings, body_terms, body) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-let")],
      [mk_atom_token(~text="let", ())]
      @ view_lytr_terms(~font, bindings)
      @ [mk_atom_token(~text="=", ())]
      @ view_lytr_terms(~font, body_terms)
      @ [mk_atom_token(~text="in", ())]
      @ [view_lytr_child(~font, body)],
    )
  | Type(type_params, type_body, body) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-type")],
      [mk_atom_token(~text="type", ())]
      @ view_lytr_terms(~font, type_params)
      @ [mk_operator_token(~text="=", ())]
      @ view_lytr_terms(~font, type_body)
      @ [mk_atom_token(~text="in", ())]
      @ [view_lytr_child(~font, body)],
    )
  | Case(scrutinee, branches) =>
    let branch_nodes =
      List.map(
        ((pattern, body)) =>
          Node.span(
            ~attrs=[Attr.class_("lytr-case-branch")],
            [mk_operator_token(~text="|", ())]
            @ view_lytr_terms(~font, pattern)
            @ [mk_operator_token(~text="=>", ())]
            @ view_lytr_terms(~font, body),
          ),
        branches,
      );
    Node.span(
      ~attrs=[Attr.class_("lytr-case")],
      [mk_atom_token(~text="case", ())]
      @ view_lytr_terms(~font, scrutinee)
      @ branch_nodes
      @ [mk_atom_token(~text="end", ())],
    );
  | If(terms1, terms2, child) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-if")],
      [mk_atom_token(~text="if", ())]
      @ view_lytr_terms(~font, terms1)
      @ [mk_atom_token(~text="then", ())]
      @ view_lytr_terms(~font, terms2)
      @ [mk_atom_token(~text="else", ())]
      @ [view_lytr_child(~font, child)],
    )
  | Asc(c1, c2) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-asc")],
      [view_lytr_child(~font, c1)]
      @ [mk_atom_token(~text=":", ())]
      @ [view_lytr_child(~font, c2)],
    )
  | Arrow(c1, c2) =>
    Node.span(
      ~attrs=[Attr.class_("lytr-asc")],
      [view_lytr_child(~font, c1)]
      @ [mk_atom_token(~text="->", ())]
      @ [view_lytr_child(~font, c2)],
    )
  | DEBUG => mk_error_token(~text="DEBUG", ())
  }

and view_lytr_child = (~font, child: child): Node.t =>
  switch (child) {
  | Hole => mk_hole(~font, ())
  | Term(term) => view_lytr_term(~font, term)
  };

/* Rich view function using styled tokens */
let view_lytr_text = (~font, terms: terms): Node.t => {
  let styled_line = view_lytr_terms(~font, terms);
  Node.div(
    ~attrs=[Attr.classes(["block", "lytr-block"])],
    [Node.span(~attrs=[Attr.class_("line")], styled_line)],
  );
};

/* Fallback simple view function */
let rec view_lytr_terms = (~font, terms: terms): list(Node.t) =>
  switch (terms) {
  | Nil => []
  | Cons(rest, sharded) =>
    view_lytr_terms(~font, rest) @ [view_lytr_sharded(~font, sharded)]
  };
