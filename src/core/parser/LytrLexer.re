open LytrToken;

/* Helper function to check if a character is whitespace */
let is_whitespace = (c: char): bool =>
  switch (c) {
  | ' '
  | '\t'
  | '\n'
  | '\r' => true
  | _ => false
  };

/* Helper function to check if a character is a digit */
let is_digit = (c: char): bool => c >= '0' && c <= '9';

/* Helper function to check if a character is a letter */
let is_letter = (c: char): bool =>
  c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z';

/* Helper function to check if a character is alphanumeric */
let is_alphanum = (c: char): bool =>
  c >= 'a'
  && c <= 'z'
  || c >= 'A'
  && c <= 'Z'
  || c >= '0'
  && c <= '9'
  || c == '_';

/* Tokenize a single character or sequence */
let rec lex_chars = (chars: list(char)): list(token) =>
  switch (chars) {
  | [] => []
  | [' ' | '\t' | '\n' | '\r', ...rest] => lex_chars(rest) /* skip whitespace */
  | ['(', ...rest] => [TOP, ...lex_chars(rest)]
  | [')', ...rest] => [TCP, ...lex_chars(rest)]
  | ['*', ...rest] => [TTimes, ...lex_chars(rest)]
  | ['-', ...rest] => [TMinus, ...lex_chars(rest)]
  | ['0', ...rest] => [TAtom(Zero), ...lex_chars(rest)]
  | [c, ...rest] when is_digit(c) =>
    /* Collect numeric sequence */
    let (num, remaining) = collect_number([c], rest);
    let num_str = String.of_seq(List.to_seq(List.rev(num)));
    switch (num_str) {
    | "0" => [TAtom(Zero), ...lex_chars(remaining)]
    | _ => [TAtom(Unlexed(num_str)), ...lex_chars(remaining)]
    };
  | [c, ...rest] when is_letter(c) =>
    /* Collect identifier sequence */
    let (id, remaining) = collect_word([c], rest);
    let id_str = String.of_seq(List.to_seq(List.rev(id)));
    [TAtom(Unlexed(id_str)), ...lex_chars(remaining)];
  | [c, ...rest] =>
    /* Unknown character - treat as unlexed atom */
    [TAtom(Unlexed(String.make(1, c))), ...lex_chars(rest)]
  }

/* Helper function to collect a numeric sequence */
and collect_number =
    (acc: list(char), chars: list(char)): (list(char), list(char)) =>
  switch (chars) {
  | [] => (acc, [])
  | [c, ...rest] when is_digit(c) => collect_number([c, ...acc], rest)
  | _ => (acc, chars)
  }

/* Helper function to collect a word (alphanumeric sequence) */
and collect_word =
    (acc: list(char), chars: list(char)): (list(char), list(char)) =>
  switch (chars) {
  | [] => (acc, [])
  | [c, ...rest] when is_alphanum(c) => collect_word([c, ...acc], rest)
  | _ => (acc, chars)
  };

/* Main lexer function */
let lex = (s: string): list(token) => {
  let chars = String.to_seq(s) |> List.of_seq;
  lex_chars(chars);
};
