type atom =
  | Zero
  | Unlexed(string);

let string_of_atom = (a: atom) =>
  switch (a) {
  | Zero => "0"
  | Unlexed(s) => s
  };

type token =
  | BOF
  | EOF
  | TOP
  | TCP
  | TTimes
  | TMinus
  | TAtom(atom);

let string_of_token = (t: token) =>
  switch (t) {
  | BOF => "#"
  | EOF => "#"
  | TOP => "("
  | TCP => ")"
  | TTimes => "*"
  | TMinus => "-"
  | TAtom(a) => string_of_atom(a)
  };

type token_entry = token;

type init_token_result =
  | NoInit
  | Init;

let init_token = (t: token): init_token_result =>
  switch (t) {
  | BOF => Init
  | EOF => NoInit
  | TOP => Init
  | TCP => NoInit
  | TTimes => Init
  | TMinus => Init
  | TAtom(_) => Init
  };

type match_token_result =
  | Match
  | NoMatch;

let match_token = (te1: token_entry, te2: token_entry): match_token_result =>
  switch (te1, te2) {
  | (BOF, EOF) => Match
  | (TOP, TCP) => Match
  | (_, _) => NoMatch /* fallthrough */
  };

type close_token_result =
  | Closed
  | Unclosed;

let close_token = (te: token_entry): close_token_result =>
  switch (te) {
  | BOF => Unclosed
  | EOF => Closed
  | TOP => Unclosed
  | TCP => Closed
  | TTimes => Closed
  | TMinus => Closed
  | TAtom(_) => Closed
  };

type compare_tokens_result =
  | Shift
  | Reduce
  | Roll;

/* need only consider when t1 ends a form and t2 starts a form */
let compare_tokens = (t1: token, t2: token): compare_tokens_result =>
  switch (t1, t2) {
  | (TCP, TOP) => Roll
  | (TCP, TTimes) => Reduce
  | (TCP, TMinus) => Reduce
  | (TCP, TAtom(_)) => Roll
  | (TTimes, TOP) => Shift
  | (TTimes, TTimes) => Shift /* right assoc */
  | (TTimes, TMinus) => Reduce /* * binds tighter */
  | (TTimes, TAtom(_)) => Shift
  | (TMinus, TOP) => Shift
  | (TMinus, TTimes) => Shift
  | (TMinus, TMinus) => Reduce /* left assoc */
  | (TMinus, TAtom(_)) => Shift
  | (TAtom(_), TOP) => Roll
  | (TAtom(_), TTimes) => Reduce
  | (TAtom(_), TMinus) => Reduce
  | (TAtom(_), TAtom(_)) => Roll
  | (_, _) => Roll /* impossible fallthrough */
  };

type wants_left_child_result =
  | Yes
  | No;

/* need only consider when t starts a form */
let wants_left_child = (t: token): wants_left_child_result =>
  switch (t) {
  | TOP => No
  | TTimes => Yes
  | TMinus => Yes
  | TAtom(_) => No
  | _ => No /* impossible fallthrough */
  };

let token_of = (te: token_entry): token => te;

type sharded('a) =
  | Shard(token)
  | Form('a);

type partial_form =
  | Head(token_entry)
  | Match(partial_form, list(sharded(partial_form)), token_entry);

type closed_form =
  | Head(token_entry)
  | Match(closed_form, list(sharded(open_form)), token_entry)

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
        (pf: partial_form): list(sharded(partial_form)) =>
  switch (pf) {
  | Head(te) => [Shard(token_of(te))]
  | Match(f, is, te) =>
    List.append(
      shatter_partial_form(f),
      List.append(is, [Shard(token_of(te))]),
    )
  };

let flatten_partial_form = (f: partial_form): list(sharded(partial_form)) =>
  switch (close_token(face_of_partial_form(f))) {
  | Closed => [Form(f)]
  | Unclosed => shatter_partial_form(f)
  };

let rec flatten =
        (spfs: list(sharded(partial_form))): list(sharded(partial_form)) =>
  switch (spfs) {
  | [] => []
  | [Shard(t), ...s] => List.append([Shard(t)], flatten(s))
  | [Form(f), ...s] => List.append(flatten_partial_form(f), flatten(s))
  };

type match_stack_result =
  | NoMatch
  | Match(list(sharded(partial_form)));

let rec match_stack =
        (
          s: list(sharded(partial_form)),
          t: token,
          s_prime: list(sharded(partial_form)),
        )
        : match_stack_result =>
  switch (s) {
  | [] => NoMatch
  | [Shard(t_prime), ...rest] =>
    match_stack(rest, t, [Shard(t_prime), ...s_prime])
  | [Form(f), ...rest] =>
    switch (match_token(face_of_partial_form(f), t)) {
    | NoMatch => match_stack(rest, t, [Form(f), ...s_prime])
    | Match => Match([Form(Match(f, flatten(s_prime), t)), ...rest])
    }
  };

let match_stack_init =
    (s: list(sharded(partial_form)), t: token): match_stack_result =>
  match_stack(s, t, []);

let match_push =
    (s: list(sharded(partial_form)), t: token)
    : list(sharded(partial_form)) =>
  switch (match_stack_init(s, t)) {
  | Match(s_prime) => s_prime
  | NoMatch =>
    switch (init_token(t)) {
    | NoInit => [Shard(t), ...s]
    | Init => [Form(Head(t)), ...s]
    }
  };

let rec match_pushes =
        (s: list(sharded(partial_form)), ts: list(token))
        : list(sharded(partial_form)) =>
  switch (ts) {
  | [] => s
  | [t, ...rest] => match_pushes(match_push(s, t), rest)
  };

let match_parse = (ts: list(token)): list(sharded(partial_form)) => {
  let tokens = [BOF, ...List.append(ts, [EOF])];
  let result = match_pushes([], tokens);
  switch (result) {
  | [Form(Match(Head(BOF), is, EOF))] => is
  | _ => [] /* impossible */
  };
};

/* operatorize phase */

type op_state =
  | OS(list(open_form), list(half_open_form));

let rec op_state_roll =
        (os: op_state, acc: option(open_form)): list(open_form) =>
  switch (os, acc) {
  | (OS(fs, []), None) => fs
  | (OS(fs, []), Some(acc)) => [acc, ...fs]
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
    | (No, Some(acc)) => OS([acc, ...fs], [HOForm(None, f)])
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
  | SOS(list(sharded(open_form)), op_state);

let sharded_op_state_roll =
    (sos: sharded_op_state): list(sharded(open_form)) =>
  switch (sos) {
  | SOS(fs, s) =>
    List.append(fs, List.map(form => Form(form), op_state_roll(s, None)))
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
  | Shard(t) => SOS([Shard(t), ...sharded_op_state_roll(s)], OS([], []))
  | Form(f) =>
    switch (s) {
    | SOS(fs, os) => SOS(fs, op_parse(os, None, f))
    }
  }

and op_parse_steps =
    (s: sharded_op_state, spfs: list(sharded(partial_form)))
    : sharded_op_state =>
  switch (spfs) {
  | [] => s
  | [hd, ...tl] =>
    op_parse_step(op_parse_steps(s, tl), close_sharded_partial_form(hd))
  }

and operatorize =
    (fs: list(sharded(partial_form))): list(sharded(open_form)) =>
  sharded_op_state_roll(op_parse_steps(SOS([], OS([], [])), fs));

/* abstraction phase */

type terms = list(sharded(term))

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

let rec abstract_terms = (fs: list(sharded(open_form))): terms =>
  List.map(abstract_sharded, fs)

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
