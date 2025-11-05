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
