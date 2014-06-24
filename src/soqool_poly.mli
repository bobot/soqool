open Soqool_core.Formula

(** comparison operator are implemented for all relevant datatype
    (I don't know what relevant means)
    Warning it is the comparison operator of posgresql.
    It must be the same than ocaml for predefined type in Ty.
    But certainly not for the one created with Ty.from_sexp
*)
val (=): 'p term -> 'p term -> formula
val (<): 'p term -> 'p term -> formula
val (>): 'p term -> 'p term -> formula
val (<=): 'p term -> 'p term -> formula
val (>=): 'p term -> 'p term -> formula
val (<>): 'p term -> 'p term -> formula
