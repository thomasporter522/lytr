open LytrToken;

type token_entry = token;

type init_token_result =
  | NoInit
  | Init;

let non_initial_tokens = [EOF, TCP, TEquals, TIn, TPipe, TDoubleArrow, TEnd];

let init_token = (t: token): init_token_result =>
  List.mem(t, non_initial_tokens) ? NoInit : Init;

type match_token_result =
  | Match
  | NoMatch;

let match_token = (te1: token_entry, te2: token_entry): match_token_result =>
  switch (te1, te2) {
  | (BOF, EOF) => Match
  | (TOP, TCP) => Match
  | (TFun, TArrow) => Match
  | (TLet, TEquals) => Match
  | (TEquals, TIn) => Match
  | (TCase, TEnd) => Match
  | (TCase, TPipe) => Match
  | (TPipe, TDoubleArrow) => Match
  | (TDoubleArrow, TPipe) => Match
  | (TDoubleArrow, TEnd) => Match
  | (_, _) => NoMatch /* fallthrough */
  };

type close_token_result =
  | Closed
  | Unclosed;

let unclosed_tokens = [BOF, TOP, TLet, TEquals, TType, TCase, TPipe, TDoubleArrow];

let close_token = (te: token_entry): close_token_result =>
    List.mem(te, unclosed_tokens) ? Unclosed : Closed;

type prec = 
    | Interior // never relevent, always matches over
    | Uninterested // can't have a child
    | Precedence(float); 

let prec (t : token) : (prec, prec) =
  switch (t) {
  | BOF => (Uninterested, Interior)
  | EOF => (Interior, Uninterested)
  | TOP => (Precedence(3.), Interior)
  | TCP => (Interior, Uninterested)
  | TAtom(_) => (Uninterested, Uninterested)
  | TPlus => (Precedence(0.9), Precedence(1.1))
  | TMinus => (Precedence(0.9), Precedence(1.1))
  | TTimes => (Precedence(2.1), Precedence(1.9))
  | TDivide => (Precedence(2.1), Precedence(1.9))
  | TDoubleDivide => (Precedence(2.1), Precedence(1.9))
  | TModulo => (Precedence(2.1), Precedence(1.9))
  | TFun => (Uninterested, Interior)
  | TArrow => (Interior, Interior)
  | TLet => (Uninterested, Interior)
  | TEquals => (Interior, Interior)
  | TIn => (Interior, Precedence(0.))
  | TType => (Uninterested, Interior)
  | TCase => (Uninterested, Interior)
  | TPipe => (Interior, Interior)
  | TDoubleArrow => (Interior, Interior)
  | TEnd => (Interior, Uninterested)
  };

type compare_tokens_result =
  | Shift // first thing wants the second as a child
  | Reduce // second thing wants the first as a child
  | Roll; // neither can be the other's child
  // Fuse // for associative operators

/* Precondition: t1 ends a form and t2 starts a form */
let compare_tokens = (t1: token, t2: token): compare_tokens_result => {
    let (_, prec1_r) = prec(t1);
    let (prec2_l, _) = prec(t2);
    switch (prec1_r, prec2_l) {
    | (Precedence(p1), Precedence(p2)) =>
        if (p1 < p2) {
            Shift
        } else if (p1 > p2) {
            Reduce
        } else {
            // Roll?
            failwith("impossible: precedence collision")
        }
    | (Uninterested, Precedence(_)) => Reduce
    | (Precedence(_), Uninterested) => Shift
    | (Uninterested, Uninterested) => Roll
    | (Interior, _)
    | (_, Interior) => failwith("impossible: precondition violated")
    }
}

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
