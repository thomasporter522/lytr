let test_lexer = () => {
  let tokens = LytrLexer.lex("(* 0 - hello)");
  List.iter(t => print_endline(LytrToken.string_of_token(t)), tokens);
};
test_lexer();
