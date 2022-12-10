:- use_module(library(dcgs)).
:- use_module(library(charsio)).

integer(I)              --> signed_digits(Ds), { number_chars(I, Ds) }.
signed_digits(['-'|Ds]) --> "-", digits(Ds).
signed_digits(Ds)       --> digits(Ds).
digits([D|Ds])          --> digit(D), maybe_digits(Ds).
maybe_digits([])        --> [].
maybe_digits([D|Ds])    --> digit(D), maybe_digits(Ds).
digit(D)                --> [D], { char_type(D, decimal_digit) }.
