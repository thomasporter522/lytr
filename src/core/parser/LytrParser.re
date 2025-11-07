open LytrToken;
open LytrGrammar;

type listr('a) =
  | Nil
  | Cons(listr('a), 'a);

let sing = a => Cons(Nil, a);

/* Helper functions for right-cons lists */
let rec append = (l1: listr('a), l2: listr('a)): listr('a) =>
  switch (l2) {
  | Nil => l1
  | Cons(l2_rest, a) => Cons(append(l1, l2_rest), a)
  };

let rec map_r = (f: 'a => 'b, l: listr('a)): listr('b) =>
  switch (l) {
  | Nil => Nil
  | Cons(rest, a) => Cons(map_r(f, rest), f(a))
  };

let token_of = (te: token_entry): token => te;

type sharded('a) =
  | Shard(token)
  | Form('a);

type partial_form =
  | Head(token_entry)
  | Match(partial_form, listr(sharded(partial_form)), token_entry);

type closed_form =
  | Head(token_entry)
  | Match(closed_form, listr(sharded(open_form)), token_entry)

and open_form =
  | OForm(option(open_form), closed_form, option(open_form))

and half_open_form =
  | HOForm(option(open_form), closed_form);

let rec head_of = (cf: closed_form): token_entry =>
  switch (cf) {
  | Head(t) => t
  | Match(f, _is, _t) => head_of(f)
  };

let rec head_of_partial_form = (pf: partial_form): token_entry =>
  switch (pf) {
  | Head(t) => t
  | Match(f, _is, _t) => head_of_partial_form(f)
  };

let face_of_partial_form = (pf: partial_form): token_entry =>
  switch (pf) {
  | Head(t) => t
  | Match(_f, _is, t) => t
  };

let face_of_form = (cf: closed_form): token_entry =>
  switch (cf) {
  | Head(t) => t
  | Match(_f, _is, t) => t
  };

let face_of_half_open_form = (hof: half_open_form): token =>
  switch (hof) {
  | HOForm(_, f) => face_of_form(f)
  };

/* matching phase */

let rec shatter_partial_form =
        (pf: partial_form): listr(sharded(partial_form)) =>
  switch (pf) {
  | Head(te) => sing(Shard(token_of(te)))
  | Match(f, is, te) =>
    append(shatter_partial_form(f), Cons(is, Shard(token_of(te))))
  };

let flatten_partial_form = (f: partial_form): listr(sharded(partial_form)) =>
  switch (close_token(face_of_partial_form(f))) {
  | Closed => sing(Form(f))
  | Unclosed => shatter_partial_form(f)
  };

let rec flatten =
        (spfs: list(sharded(partial_form))): listr(sharded(partial_form)) =>
  switch (spfs) {
  | [] => Nil
  | [Shard(t), ...s] => append(sing(Shard(t)), flatten(s))
  | [Form(f), ...s] => append(flatten_partial_form(f), flatten(s))
  };

type match_stack_result =
  | NoMatch
  | Match(listr(sharded(partial_form)));

let rec match_stack =
        (
          s: listr(sharded(partial_form)),
          t: token,
          s_prime: list(sharded(partial_form)),
        )
        : match_stack_result =>
  switch (s) {
  | Nil => NoMatch
  | Cons(rest, Shard(t_prime)) =>
    match_stack(rest, t, [Shard(t_prime), ...s_prime])
  | Cons(rest, Form(f)) =>
    switch (match_token(face_of_partial_form(f), t)) {
    | NoMatch => match_stack(rest, t, [Form(f), ...s_prime])
    | Match => Match(Cons(rest, Form(Match(f, flatten(s_prime), t))))
    }
  };

let match_stack_init =
    (s: listr(sharded(partial_form)), t: token): match_stack_result =>
  match_stack(s, t, []);

let match_push =
    (s: listr(sharded(partial_form)), t: token)
    : listr(sharded(partial_form)) =>
  switch (match_stack_init(s, t)) {
  | Match(s_prime) => s_prime
  | NoMatch =>
    switch (init_token(t)) {
    | NoInit => Cons(s, Shard(t))
    | Init => Cons(s, Form(Head(t)))
    }
  };

let rec match_pushes =
        (s: listr(sharded(partial_form)), ts: list(token))
        : listr(sharded(partial_form)) =>
  switch (ts) {
  | [] => s
  | [t, ...rest] => match_pushes(match_push(s, t), rest)
  };

let match_parse = (ts: list(token)): listr(sharded(partial_form)) => {
  let tokens = [BOF, ...List.append(ts, [EOF])];
  let result = match_pushes(Nil, tokens);
  switch (result) {
  | Cons(Nil, Form(Match(Head(BOF), is, EOF))) => is
  | _ => Nil /* impossible */
  };
};

/* operatorize phase */

type op_state =
  | OS(listr(open_form), list(half_open_form));

let rec op_state_roll =
        (os: op_state, acc: option(open_form)): listr(open_form) =>
  switch (os, acc) {
  | (OS(fs, []), None) => fs
  | (OS(fs, []), Some(acc)) => Cons(fs, acc)
  | (OS(fs, [HOForm(l, f), ...s]), acc) =>
    op_state_roll(OS(fs, s), Some(OForm(l, f, acc)))
  };

/* the top "with" clause might be unnecessary if acc is not nothing */
let rec op_parse =
        (os: op_state, acc: option(open_form), f: closed_form): op_state =>
  switch (os) {
  | OS(fs, []) =>
    switch (wants_left_child(head_of(f)), acc) {
    | (Yes, _) => OS(fs, [HOForm(acc, f)])
    | (No, None) => OS(fs, [HOForm(None, f)])
    | (No, Some(acc)) => OS(Cons(fs, acc), [HOForm(None, f)])
    }
  | OS(fs, [f1, ...s]) =>
    switch (compare_tokens(face_of_half_open_form(f1), head_of(f))) {
    | Shift => OS(fs, [HOForm(acc, f), f1, ...s])
    | Reduce =>
      switch (f1) {
      | HOForm(l, f1_inner) =>
        op_parse(OS(fs, s), Some(OForm(l, f1_inner, acc)), f)
      }
    | Roll =>
      OS(op_state_roll(OS(fs, [f1, ...s]), acc), [HOForm(None, f)])
    }
  };

type sharded_op_state =
  | SOS(listr(sharded(open_form)), op_state);

let sharded_op_state_roll =
    (sos: sharded_op_state): listr(sharded(open_form)) =>
  switch (sos) {
  | SOS(fs, s) =>
    append(fs, map_r(form => Form(form), op_state_roll(s, None)))
  };

let rec close_partial_form = (pf: partial_form): closed_form =>
  switch (pf) {
  | Head(t) => Head(t)
  | Match(f, fs, t) => Match(close_partial_form(f), operatorize(fs), t)
  }

and close_sharded_partial_form =
    (spf: sharded(partial_form)): sharded(closed_form) =>
  switch (spf) {
  | Shard(t) => Shard(t)
  | Form(f) => Form(close_partial_form(f))
  }

and op_parse_step =
    (s: sharded_op_state, scf: sharded(closed_form)): sharded_op_state =>
  switch (scf) {
  | Shard(t) => SOS(Cons(sharded_op_state_roll(s), Shard(t)), OS(Nil, []))
  | Form(f) =>
    switch (s) {
    | SOS(fs, os) => SOS(fs, op_parse(os, None, f))
    }
  }

and op_parse_steps =
    (s: sharded_op_state, spfs: listr(sharded(partial_form)))
    : sharded_op_state =>
  switch (spfs) {
  | Nil => s
  | Cons(l, f) =>
    op_parse_step(op_parse_steps(s, l), close_sharded_partial_form(f))
  }

and operatorize =
    (fs: listr(sharded(partial_form))): listr(sharded(open_form)) =>
  sharded_op_state_roll(op_parse_steps(SOS(Nil, OS(Nil, [])), fs));

/* abstraction phase */

type terms = listr(sharded(term))

and child =
  | Hole
  | Term(term)

and term =
  | Parens(terms)
  | Times(child, child)
  | Negative(child)
  | Minus(child, child)
  | Atom(atom)
  | DEBUG;

let rec abstract_terms = (fs: listr(sharded(open_form))): terms =>
  map_r(abstract_sharded, fs)

and abstract_child = (form: option(open_form)): child =>
  switch (form) {
  | None => Hole
  | Some(f) => Term(abstract_form(f))
  }

/* precondition: the input begins, ends, and matches validly */
and abstract_form = (form: open_form): term =>
  switch (form) {
  | OForm(l, Head(TTimes), r) =>
    Times(abstract_child(l), abstract_child(r))
  | OForm(None, Head(TMinus), r) => Negative(abstract_child(r))
  | OForm(Some(f), Head(TMinus), r) =>
    Minus(Term(abstract_form(f)), abstract_child(r))
  | OForm(None, Match(Head(TOP), is, TCP), None) =>
    Parens(abstract_terms(is))
  | OForm(None, Head(TAtom(a)), None) => Atom(a)
  | _ => DEBUG /* impossible fallthrough */
  }

and abstract_sharded = (sof: sharded(open_form)): sharded(term) =>
  switch (sof) {
  | Shard(t) => Shard(t)
  | Form(f) => Form(abstract_form(f))
  };

let parse = (tokens: list(token)): terms =>
  abstract_terms(operatorize(match_parse(tokens)));

let rec string_of_sharded = (st: sharded(term)): string =>
  switch (st) {
  | Shard(t) => "💥" ++ string_of_token(t) ++ "💥"
  | Form(t) => string_of_term(t)
  }

and string_of_terms = (ts: terms): string =>
  switch (ts) {
  | Nil => ""
  | Cons(Nil, t) => string_of_sharded(t)
  | Cons(rest, t) => string_of_terms(rest) ++ "·" ++ string_of_sharded(t)
  }

and string_of_child = (c: child): string =>
  switch (c) {
  | Hole => "?"
  | Term(t) => string_of_term(t)
  }

and string_of_term = (t: term): string =>
  switch (t) {
  | Parens(ts) => "(" ++ string_of_terms(ts) ++ ")"
  | Times(l, r) => string_of_child(l) ++ "*" ++ string_of_child(r)
  | Negative(c) => "-" ++ string_of_child(c)
  | Minus(l, r) => string_of_child(l) ++ "-" ++ string_of_child(r)
  | Atom(a) => string_of_atom(a)
  | DEBUG => "DEBUG"
  };
