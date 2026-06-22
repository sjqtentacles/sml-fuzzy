# sml-fuzzy

Fuzzy string matching in pure Standard ML: Levenshtein and Damerau edit
distances, a normalized similarity ratio, deterministic best-first ranking, a
BK-tree index, and classic Soundex. Pure and deterministic — no FFI, threads,
or clock — and it builds with both **MLton** and **Poly/ML**.

## Relationship to `sml-diff`

[`sml-diff`](https://github.com/sjqtentacles/sml-diff) ships a single
`editDistance` that counts only insertions and deletions (the LCS-style
metric). `sml-fuzzy` is the fuller fuzzy-matching toolkit: edit distances that
also include **substitution** (Levenshtein) and adjacent **transposition**
(Damerau), plus similarity **ranking**, a **BK-tree** for nearest-neighbour
lookups, and **Soundex** phonetic coding. Reach for `sml-diff` when you want a
line/sequence diff; reach for `sml-fuzzy` when you want approximate string
matching, spell-checking, or ranking.

## API (`signature FUZZY`)

```sml
val levenshtein : string * string -> int        (* insert / delete / substitute *)
val damerau     : string * string -> int        (* + adjacent transposition *)
val ratio       : string * string -> real        (* similarity in [0.0, 1.0] *)
val jaro        : string * string -> real        (* Jaro similarity *)
val jaroWinkler : string * string -> real        (* + common-prefix boost *)
val rank        : {query : string, candidates : string list}
                    -> (string * real) list       (* best-first, stable ties *)

structure BKTree :
sig
  type tree
  val empty  : tree
  val insert : tree -> string -> tree
  val search : tree -> string -> int -> string list   (* within edit distance *)
end

val soundex : string -> string                    (* e.g. "Robert" -> "R163" *)
```

## Usage

```sml
val d1 = Fuzzy.levenshtein ("kitten", "sitting")   (* => 3 *)
val d2 = Fuzzy.damerau ("ca", "ac")                (* => 1 (transposition) *)
val r  = Fuzzy.ratio ("kitten", "sitting")         (* => ~0.571 *)

val best = Fuzzy.rank
  {query = "kitten", candidates = ["sitting", "mitten", "kitten", "dog"]}
(* => [("kitten", 1.0), ("mitten", 0.833...), ("sitting", 0.571...), ("dog", 0.0)] *)

val tree = List.foldl (fn (w, t) => Fuzzy.BKTree.insert t w)
             Fuzzy.BKTree.empty ["book", "books", "boo", "cook", "boon"]
val near = Fuzzy.BKTree.search tree "book" 1       (* all words within distance 1 *)

val code = Fuzzy.soundex "Ashcraft"                (* => "A261" *)
```

`ratio (a, b) = 1 - levenshtein (a, b) / max (|a|, |b|)`, with two empty
strings scoring `1.0`. `rank` sorts by descending `ratio` and breaks ties by
the candidates' input order (a stable sort), so its output is deterministic.

### Jaro and Jaro-Winkler

`jaro` is the Jaro similarity in `[0.0, 1.0]`, counting matching characters
within a sliding window of `floor(max(|a|,|b|)/2) - 1` and their
half-transpositions. `jaroWinkler` boosts that score by a common prefix of up
to four characters with scaling factor `p = 0.1`, so strings that agree at the
start rank higher:

```sml
val j  = Fuzzy.jaro ("MARTHA", "MARHTA")          (* => ~0.944 *)
val jw = Fuzzy.jaroWinkler ("MARTHA", "MARHTA")   (* => ~0.961 *)
val _  = Fuzzy.jaroWinkler ("DWAYNE", "DUANE")    (* => ~0.840 *)
```

## Installation

```sh
smlpkg add github.com/sjqtentacles/sml-fuzzy
smlpkg sync
```

Reference `lib/github.com/sjqtentacles/sml-fuzzy/fuzzy.mlb` from your own
`.mlb` (MLton / MLKit), or feed `sources.mlb` to `tools/polybuild` (Poly/ML).

## Example

`make example` builds and runs [`examples/demo.sml`](examples/demo.sml), a
small dependency-free "fuzzy finder" that ranks a few queries against a word
list and prints the top matches as text:

```
Query: "smiton"  (soundex S535)
  rank  score   word
  1.    0.714   smitten
  2.    0.500   mitten
  3.    0.429   sitting
```

(A graphical PNG version of the demo can come later.)

## Layout

```
sml.pkg                                       smlpkg manifest
Makefile                                      MLton + Poly/ML targets
.github/workflows/ci.yml                      CI: MLton + Poly/ML
lib/github.com/sjqtentacles/sml-fuzzy/
  fuzzy.sig/.sml   the FUZZY structure
  sources.mlb      ordered source list
  fuzzy.mlb        public basis
test/
  harness.sml  shared assertion harness
  test.sml     TDD suite (45 checks)
  entry.sml / main.sml
examples/demo.sml  dependency-free fuzzy finder
tools/polybuild    Poly/ML build wrapper
```

## Tests

45 deterministic checks covering Levenshtein and Damerau (optimal string
alignment) distances, the similarity ratio, Jaro / Jaro-Winkler similarity
against canonical published vectors, stable best-first ranking, BK-tree
retrieval within an edit-distance bound, and the standard Soundex vectors
(`Robert`/`Rupert` -> `R163`, `Rubin` -> `R150`, `Ashcraft` -> `A261`,
`Tymczak` -> `T522`, `Honeyman` -> `H555`).

```sh
make test       # MLton
make test-poly  # Poly/ML
make all-tests  # both
```

## License

MIT. See [LICENSE](LICENSE).
