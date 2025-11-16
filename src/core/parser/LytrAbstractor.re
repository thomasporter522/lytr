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

and assoc_entry =
  | Empty(unforms)
  | Term(unforms, term, unforms)

and term =
  | Tuple(list(terms))
  | List(list(terms))
  | Atom(atom)
  | InfixBinop(left_child, binop, right_child)
  | AssocBinop(left_child, binop, list(assoc_entry), right_child)
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

and abstract_associative = (token, binop, f: open_form): term => {
  switch (f) {
  | OForm(lc, lu, Head(t), ru, rc) when t == token =>
    let (left_child, left_extension): (left_child, list(assoc_entry)) =
      switch (lc) {
      | Some(l) =>
        switch (abstract_associative(token, binop, l)) {
        | AssocBinop(a1, binop2, a2, a3) when binop2 == binop =>
          switch (a3) {
          | Hole => (a1, a2 @ [Empty(lu)])
          | Term(b1, b2) => (a1, a2 @ [Term(b1, b2, lu)])
          }
        | lc2 => (Term(lc2, lu), [])
        }
      | None => (Hole, [])
      };
    let (right_extension, right_child): (list(assoc_entry), right_child) =
      switch (rc) {
      | Some(r) =>
        switch (abstract_associative(token, binop, r)) {
        | AssocBinop(a1, binop2, a2, a3) when binop2 == binop =>
          switch (a1) {
          | Hole => ([Empty(ru), ...a2], a3)
          | Term(b1, b2) => ([Term(ru, b1, b2), ...a2], a3)
          }
        | rc2 => ([], Term(ru, rc2))
        }
      | None => ([], Hole)
      };
    AssocBinop(
      left_child,
      binop,
      left_extension @ right_extension,
      right_child,
    );
  | _ => abstract_form(f)
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
  // associative binary operations
  | OForm(_, _, Head(TPlus), _, _) =>
    abstract_associative(TPlus, Plus, form)
  | OForm(_, _, Head(TTimes), _, _) =>
    abstract_associative(TTimes, Times, form)
  // binary operations
  | OForm(l, u1, Head(TMinus), u2, r) =>
    InfixBinop(
      abstract_left_child(l, u1),
      Minus,
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
