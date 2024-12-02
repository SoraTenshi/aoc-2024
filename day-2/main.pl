:- initialization(main).

main :-
  exists_file("input.txt"),
  parse_file("input.txt", Reports),
  print_matrix(Reports),
  include(valid_row, Reports, ValidReports),
  length(ValidReports, Count),
  format("Valid: ~w~n", [Count]),
  include(valid_safety_dampened, Reports, SafetyDampenedReports),
  print_matrix(SafetyDampenedReports),
  length(SafetyDampenedReports, SDCount),
  format("Valid (Safety Dampened): ~w~n", [SDCount]).

parse_file(FileName, Reports) :-
  open(FileName, read, Stream),
  read_lines(Stream, Lines),
  close(Stream),
  maplist(parse_line, Lines, Reports).

read_lines(Stream, []) :-
  at_end_of_stream(Stream).
read_lines(Stream, [X|Y]) :-
  \+ at_end_of_stream(Stream),
  read_line_to_string(Stream, X),
  read_lines(Stream, Y).

parse_line(Line, Numbers) :-
  split_string(Line, ' ', '', StrNumbers),
  maplist(number_string, Numbers, StrNumbers).

print_matrix([]).
print_matrix([X|Y]) :-
  write(X), write(' '), 
  ( valid_row(X)
  -> write("OK")
  ; write("INVALID")
  ), nl,
  print_matrix(Y).

valid_row([X, Y | Rest]) :-
  trend([X, Y | Rest], none).

trend([], _).
trend([_], _).

trend([X, Y | Rest], ascending) :-
  X < Y, abs(X - Y) =< 3, abs(X - Y) > 0,
  trend([Y | Rest], ascending).

trend([X, Y | Rest], descending) :-
  X > Y, abs(X - Y) =< 3, abs(X - Y) > 0,
  trend([Y | Rest], descending).

trend([X, Y | Rest], none) :-
  abs(X - Y) =< 3, abs(X - Y) > 0,
  % Generally, the ( $statement ; $statement ) syntax represents somewhat of a logical or
  % Looks super fancy imo
  (  X < Y -> trend([Y | Rest], ascending)
  ;  X > Y -> trend([Y | Rest], descending) % ; = logical or in prolog
  ).

remove_one([_|T], T).
remove_one([H|T], [H|T2]) :-
  remove_one(T, T2).

safety_dampened(X, Removed) :-
  findall(Xs, remove_one(X, Xs), Removed).

valid_safety_dampened(X) :-
  ( valid_row(X)
  -> true
  ; safety_dampened(X, Xs),
    include(valid_row, Xs, Xss),
    Xss \= [] % thou shall not be empty!
  ).

