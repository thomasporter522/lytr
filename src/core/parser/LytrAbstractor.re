open LytrGrammar;
open LytrParser;

/* abstraction phase */

type binop =
  | Plus
  | Minus
  | Times
  | Divide
  | DoubleDivide
  | Modulo;

type unop =
  | Minus
  | Factorial;

type terms = listr(sharded(term))

and left_child =
  | Hole
  | Term(term, secondaries)

and right_child =
  | Hole
  | Term(secondaries, term)

and term =
  | Tuple(list(terms))
  | List(list(terms))
  | Atom(atom)
  | InfixBinop(left_child, binop, right_child)
  | PrefixUnop(unop, right_child)
  | PostfixUnop(left_child, unop)
  | Fun(terms, right_child)
  | Ap(left_child, list(terms))
  | Let(terms, terms, right_child)
  | Type(terms, terms, right_child)
  | Case(terms, list((terms, terms)))
  | If(terms, terms, right_child)
  | Asc(left_child, right_child)
  | Arrow(left_child, right_child)
  | DEBUG;

let rec terms_all_secondary = terms => {
  switch (terms) {
  | Nil => true
  | Cons(terms, Secondary(_)) => terms_all_secondary(terms)
  | Cons(_, _) => false
  };
};

let rec abstract_terms = (fs: listr(sharded(open_form))): terms =>
  mapr(abstract_sharded, fs)

and abstract_left_child =
    (form: option(open_form), se: secondaries): left_child =>
  switch (form) {
  | None => Hole
  | Some(f) => Term(abstract_form(f), se)
  }

and abstract_right_child =
    (se: secondaries, form: option(open_form)): right_child =>
  switch (form) {
  | None => Hole
  | Some(f) => Term(se, abstract_form(f))
  }

and abstract_case_branches =
    (form: closed_form, is: listr(sharded(open_form))) => {
  switch (form) {
  | Head(TCase) => (abstract_terms(is), [])
  | Match(Match(form, is1, TPipe), is2, TDoubleArrow) =>
    let (scrutinee, cases) = abstract_case_branches(form, is1);
    (scrutinee, cases @ [(abstract_terms(is2), abstract_terms(is))]);
  | _ => failwith("impossible; ill structured case form")
  };
}

and abstract_tuple = (form: closed_form) => {
  switch (form) {
  | Head(TOP) => []
  | Match(form, is, TCommaTuple) =>
    abstract_tuple(form) @ [abstract_terms(is)]
  | _ => failwith("impossible; ill structured tuple form")
  };
}

and abstract_list = (form: closed_form) => {
  switch (form) {
  | Head(TOSB) => []
  | Match(form, is, TCommaList) =>
    abstract_list(form) @ [abstract_terms(is)]
  | _ => failwith("impossible; ill structured list form")
  };
}

/* precondition: the input begins, ends, and matches validly */
and abstract_form = (form: open_form): term =>
  switch (form) {
  // tuples (including unit and ap)
  | OForm(None, Nil, Match(Head(TOP), Nil, TCP), Nil, None) => Tuple([])
  | OForm(None, Nil, Match(form, is, TCP), Nil, None) =>
    Tuple(abstract_tuple(form) @ [abstract_terms(is)])
  | OForm(Some(l), se, Match(Head(TOP), Nil, TCP), Nil, None) =>
    Ap(abstract_left_child(Some(l), se), [])
  | OForm(Some(l), se, Match(form, is, TCP), Nil, None) =>
    Ap(
      abstract_left_child(Some(l), se),
      abstract_tuple(form) @ [abstract_terms(is)],
    )
  // lists
  | OForm(None, Nil, Match(Head(TOSB), Nil, TCSB), Nil, None) => List([])
  | OForm(None, Nil, Match(form, is, TCSB), Nil, None) =>
    List(abstract_list(form) @ [abstract_terms(is)])

  // atoms
  | OForm(None, Nil, Head(TAtom(a)), Nil, None) => Atom(a)
  // unary operations
  | OForm(None, Nil, Head(TMinus), se, r) =>
    PrefixUnop(Minus, abstract_right_child(se, r))
  | OForm(l, se, Head(TFactorial), Nil, None) =>
    PostfixUnop(abstract_left_child(l, se), Factorial)
  // binary operations
  | OForm(l, se1, Head(TPlus), se2, r) =>
    InfixBinop(
      abstract_left_child(l, se1),
      Plus,
      abstract_right_child(se2, r),
    )
  | OForm(l, se1, Head(TMinus), se2, r) =>
    InfixBinop(
      abstract_left_child(l, se1),
      Minus,
      abstract_right_child(se2, r),
    )
  | OForm(l, se1, Head(TTimes), se2, r) =>
    InfixBinop(
      abstract_left_child(l, se1),
      Times,
      abstract_right_child(se2, r),
    )
  | OForm(l, se1, Head(TDivide), se2, r) =>
    InfixBinop(
      abstract_left_child(l, se1),
      Divide,
      abstract_right_child(se2, r),
    )
  | OForm(l, se1, Head(TDoubleDivide), se2, r) =>
    InfixBinop(
      abstract_left_child(l, se1),
      DoubleDivide,
      abstract_right_child(se2, r),
    )
  | OForm(l, se1, Head(TModulo), se2, r) =>
    InfixBinop(
      abstract_left_child(l, se1),
      Modulo,
      abstract_right_child(se2, r),
    )
  // other infix forms
  | OForm(l, se1, Head(TColon), se2, r) =>
    Asc(abstract_left_child(l, se1), abstract_right_child(se2, r))
  | OForm(l, se1, Head(TArrow), se2, r) =>
    Arrow(abstract_left_child(l, se1), abstract_right_child(se2, r))

  // keyword forms
  | OForm(None, Nil, Match(Head(TFun), is, TArrow), se2, r) =>
    Fun(abstract_terms(is), abstract_right_child(se2, r))
  | OForm(
      None,
      Nil,
      Match(Match(Head(TLet), is1, TEquals), is2, TIn),
      se,
      r,
    ) =>
    Let(
      abstract_terms(is1),
      abstract_terms(is2),
      abstract_right_child(se, r),
    )
  | OForm(
      None,
      Nil,
      Match(Match(Head(TType), is1, TEquals), is2, TIn),
      se,
      r,
    ) =>
    Type(
      abstract_terms(is1),
      abstract_terms(is2),
      abstract_right_child(se, r),
    )
  | OForm(None, Nil, Match(form, is, TEnd), Nil, None) =>
    let (scrutinee, cases) = abstract_case_branches(form, is);
    Case(scrutinee, cases);
  | OForm(None, Nil, Match(Match(Head(TIf), is1, TThen), is2, TElse), se, r) =>
    If(
      abstract_terms(is1),
      abstract_terms(is2),
      abstract_right_child(se, r),
    )

  | _ => DEBUG /* impossible fallthrough */
  }

and abstract_sharded = (sof: sharded(open_form)): sharded(term) =>
  switch (sof) {
  | Secondary(s) => Secondary(s)
  | Shard(t) => Shard(t)
  | Form(f) => Form(abstract_form(f))
  };

let go = (tokens: list(token)): terms =>
  abstract_terms(operatorize(match_parse(tokens)));

// let rec string_of_sharded = (st: sharded(term)): string =>
//   switch (st) {
//   | Shard(t) => "💥" ++ string_of_token(t) ++ "💥"
//   | Form(t) => string_of_term(t)
//   }

// and string_of_terms = (ts: terms): string =>
//   switch (ts) {
//   | Nil => ""
//   | Cons(Nil, t) => string_of_sharded(t)
//   | Cons(rest, t) => string_of_terms(rest) ++ "·" ++ string_of_sharded(t)
//   }

// and string_of_child = (c: child): string =>
//   switch (c) {
//   | Hole => "?"
//   | Term(t) => string_of_term(t)
//   }

// and string_of_term = (t: term): string =>
//   switch (t) {
//   | Parens(ts) => "(" ++ string_of_terms(ts) ++ ")"
//   | Times(l, r) => string_of_child(l) ++ "*" ++ string_of_child(r)
//   | Negative(c) => "-" ++ string_of_child(c)
//   | Minus(l, r) => string_of_child(l) ++ "-" ++ string_of_child(r)
//   | Atom(a) => string_of_atom(a)
//   | DEBUG => "DEBUG"
//   };
