% Factorial example
% Demonstrates arithmetic and recursion

factorial(0, 1).
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% Query: What is 5 factorial?
?- factorial(5, X), write(X), nl.
