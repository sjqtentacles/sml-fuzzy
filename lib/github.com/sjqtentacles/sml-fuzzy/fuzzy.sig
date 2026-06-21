(* fuzzy.sig

   Fuzzy string matching for Standard ML: edit distances (Levenshtein and
   Damerau, the latter adding adjacent transposition), a normalized
   similarity ratio, deterministic best-first ranking, a BK-tree index over
   the Levenshtein metric, and classic American Soundex. Pure and
   deterministic: no FFI, threads, or clock. *)

signature FUZZY =
sig
  (* Levenshtein edit distance: minimum number of single-character
     insertions, deletions, and substitutions to turn the first string into
     the second. *)
  val levenshtein : string * string -> int

  (* Damerau (optimal string alignment) distance: like Levenshtein but also
     counts a transposition of two adjacent characters as a single edit. *)
  val damerau : string * string -> int

  (* Similarity in [0.0, 1.0], where 1.0 is an exact match. Computed as
     1 - levenshtein(a, b) / max(|a|, |b|); two empty strings score 1.0. *)
  val ratio : string * string -> real

  (* Rank candidates against a query by descending similarity ratio. The
     result is sorted best-first; ties keep the candidates' input order. *)
  val rank : {query : string, candidates : string list} -> (string * real) list

  (* A Burkhard-Keller tree over the Levenshtein metric, for retrieving all
     indexed strings within a given edit distance of a query. *)
  structure BKTree :
  sig
    type tree
    val empty  : tree
    val insert : tree -> string -> tree
    (* search t q k = every indexed string within Levenshtein distance k of q. *)
    val search : tree -> string -> int -> string list
  end

  (* Classic American Soundex code: an initial letter followed by three
     digits (e.g. soundex "Robert" = "R163"). *)
  val soundex : string -> string
end
