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
  | Term(term, unforms)

and right_child =
  | Hole
  | Term(unforms, term)

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

and abstract_left_child = (form: option(open_form), se: unforms): left_child =>
  switch (form) {
  | None => Hole
  | Some(f) => Term(abstract_form(f), se)
  }

and abstract_right_child = (u: unforms, form: option(open_form)): right_child =>
  switch (form) {
  | None => Hole
  | Some(f) => Term(u, abstract_form(f))
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
  | OForm(Some(l), u, Match(Head(TOP), Nil, TCP), Nil, None) =>
    Ap(abstract_left_child(Some(l), u), [])
  | OForm(Some(l), u, Match(form, is, TCP), Nil, None) =>
    Ap(
      abstract_left_child(Some(l), u),
      abstract_tuple(form) @ [abstract_terms(is)],
    )
  // lists
  | OForm(None, Nil, Match(Head(TOSB), Nil, TCSB), Nil, None) => List([])
  | OForm(None, Nil, Match(form, is, TCSB), Nil, None) =>
    List(abstract_list(form) @ [abstract_terms(is)])

  // atoms
  | OForm(None, Nil, Head(TAtom(a)), Nil, None) => Atom(a)
  // unary operations
  | OForm(None, Nil, Head(TMinus), u, r) =>
    PrefixUnop(Minus, abstract_right_child(u, r))
  | OForm(l, u, Head(TFactorial), Nil, None) =>
    PostfixUnop(abstract_left_child(l, u), Factorial)
  // binary operations
  | OForm(l, u1, Head(TPlus), u2, r) =>
    InfixBinop(
      abstract_left_child(l, u1),
      Plus,
      abstract_right_child(u2, r),
    )
  | OForm(l, u1, Head(TMinus), u2, r) =>
    InfixBinop(
      abstract_left_child(l, u1),
      Minus,
      abstract_right_child(u2, r),
    )
  | OForm(l, u1, Head(TTimes), u2, r) =>
    InfixBinop(
      abstract_left_child(l, u1),
      Times,
      abstract_right_child(u2, r),
    )
  | OForm(l, u1, Head(TDivide), u2, r) =>
    InfixBinop(
      abstract_left_child(l, u1),
      Divide,
      abstract_right_child(u2, r),
    )
  | OForm(l, u1, Head(TDoubleDivide), u2, r) =>
    InfixBinop(
      abstract_left_child(l, u1),
      DoubleDivide,
      abstract_right_child(u2, r),
    )
  | OForm(l, u1, Head(TModulo), u2, r) =>
    InfixBinop(
      abstract_left_child(l, u1),
      Modulo,
      abstract_right_child(u2, r),
    )
  // other infix forms
  | OForm(l, u1, Head(TColon), u2, r) =>
    Asc(abstract_left_child(l, u1), abstract_right_child(u2, r))
  | OForm(l, u1, Head(TArrow), u2, r) =>
    Arrow(abstract_left_child(l, u1), abstract_right_child(u2, r))

  // keyword forms
  | OForm(None, Nil, Match(Head(TFun), is, TArrow), u2, r) =>
    Fun(abstract_terms(is), abstract_right_child(u2, r))
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
      u,
      r,
    ) =>
    Type(
      abstract_terms(is1),
      abstract_terms(is2),
      abstract_right_child(u, r),
    )
  | OForm(None, Nil, Match(form, is, TEnd), Nil, None) =>
    let (scrutinee, cases) = abstract_case_branches(form, is);
    Case(scrutinee, cases);
  | OForm(None, Nil, Match(Match(Head(TIf), is1, TThen), is2, TElse), u, r) =>
    If(
      abstract_terms(is1),
      abstract_terms(is2),
      abstract_right_child(u, r),
    )

  | _ => DEBUG /* impossible fallthrough */
  }

and abstract_sharded = (sof: sharded(open_form)): sharded(term) =>
  switch (sof) {
  | Unform(u) => Unform(u)
  | Form(f) => Form(abstract_form(f))
  };

let go = (tokens: list(token)): terms =>
  abstract_terms(operatorize(match_parse(tokens)));
