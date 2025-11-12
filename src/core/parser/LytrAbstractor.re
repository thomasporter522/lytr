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
  | Minus;

type terms = listr(sharded(term))

and child =
  | Hole
  | Term(term)

and term =
  | Parens(terms)
  | Unit
  | Atom(atom)
  | Binop(binop, child, child)
  | Unop(unop, child)
  | Fun(terms, child)
  | Ap(child, terms)
  | Let(terms, terms, child)
  | Type(terms, terms, child)
  | Case(terms, list((terms, terms)))
  | If(terms, terms, child)
  | DEBUG;

let rec abstract_terms = (fs: listr(sharded(open_form))): terms =>
  map_r(abstract_sharded, fs)

and abstract_child = (form: option(open_form)): child =>
  switch (form) {
  | None => Hole
  | Some(f) => Term(abstract_form(f))
  }

and extract_case_branches =
    (form: closed_form, is: listr(sharded(open_form))) => {
  switch (form) {
  | Head(TCase) => (abstract_terms(is), [])
  | Match(Match(form, is1, TPipe), is2, TDoubleArrow) =>
    let (scrutinee, cases) = extract_case_branches(form, is1);
    (scrutinee, cases @ [(abstract_terms(is2), abstract_terms(is))]);
  | _ => failwith("impossible; uneven case form")
  };
}

/* precondition: the input begins, ends, and matches validly */
and abstract_form = (form: open_form): term =>
  switch (form) {
  // parens (including unit and ap)
  | OForm(None, Match(Head(TOP), Nil, TCP), None) => Unit
  | OForm(None, Match(Head(TOP), is, TCP), None) =>
    Parens(abstract_terms(is))
  | OForm(Some(l), Match(Head(TOP), is, TCP), None) =>
    Ap(abstract_child(Some(l)), abstract_terms(is))

  // atoms
  | OForm(None, Head(TAtom(a)), None) => Atom(a)

  // unary minus
  | OForm(None, Head(TMinus), r) => Unop(Minus, abstract_child(r))

  // binary operations
  | OForm(l, Head(TPlus), r) =>
    Binop(Plus, abstract_child(l), abstract_child(r))
  | OForm(l, Head(TMinus), r) =>
    Binop(Minus, abstract_child(l), abstract_child(r))
  | OForm(l, Head(TTimes), r) =>
    Binop(Times, abstract_child(l), abstract_child(r))
  | OForm(l, Head(TDivide), r) =>
    Binop(Divide, abstract_child(l), abstract_child(r))
  | OForm(l, Head(TDoubleDivide), r) =>
    Binop(DoubleDivide, abstract_child(l), abstract_child(r))
  | OForm(l, Head(TModulo), r) =>
    Binop(Modulo, abstract_child(l), abstract_child(r))

  // keyword forms
  | OForm(None, Match(Head(TFun), is, TArrow), r) =>
    Fun(abstract_terms(is), abstract_child(r))
  | OForm(None, Match(Match(Head(TLet), is1, TEquals), is2, TIn), r) =>
    Let(abstract_terms(is1), abstract_terms(is2), abstract_child(r))
  | OForm(None, Match(Match(Head(TType), is1, TEquals), is2, TIn), r) =>
    Type(abstract_terms(is1), abstract_terms(is2), abstract_child(r))

  // todo: cases
  | OForm(None, Match(form, is, TEnd), None) =>
    let (scrutinee, cases) = extract_case_branches(form, is);
    Case(scrutinee, cases);

  | OForm(None, Match(Match(Head(TIf), is1, TThen), is2, TElse), r) =>
    If(abstract_terms(is1), abstract_terms(is2), abstract_child(r))

  | _ => DEBUG /* impossible fallthrough */
  }

and abstract_sharded = (sof: sharded(open_form)): sharded(term) =>
  switch (sof) {
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
