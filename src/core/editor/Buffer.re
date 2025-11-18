type id = int;

type character = {
  text: char,
  id,
};

type t = {
  text: list(character),
  cursor: int,
};

let empty: t = {
  text: [],
  cursor: 0,
};
