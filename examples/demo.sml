(* sml-fuzzy demo: a tiny dependency-free "fuzzy finder". For each query we
   rank a fixed word list by similarity (Fuzzy.rank) and print the best
   matches as text, then show the Soundex code of the query.

   A graphical (PNG) version of this demo can come later; this round stays
   dependency-free and prints plain text. *)

val dictionary =
  [ "kitten", "mitten", "sitting", "bitten", "fitting"
  , "knitting", "smitten", "written", "rotten", "dog" ]

fun pad (s, w) =
  if String.size s >= w then s
  else s ^ CharVector.tabulate (w - String.size s, fn _ => #" ")

fun showRatio r =
  let
    val scaled = Real.round (r * 1000.0)
    val whole = scaled div 1000
    val frac = scaled mod 1000
    fun three n =
      let val s = Int.toString n
      in CharVector.tabulate (3 - String.size s, fn _ => #"0") ^ s end
  in
    Int.toString whole ^ "." ^ three frac
  end

fun findFor query =
  let
    val ranked = Fuzzy.rank {query = query, candidates = dictionary}
    val top = List.take (ranked, Int.min (5, length ranked))
  in
    print ("\nQuery: \"" ^ query ^ "\"  (soundex " ^ Fuzzy.soundex query ^ ")\n");
    print "  rank  score   word\n";
    ignore (List.foldl
      (fn ((w, r), i) =>
         ( print ("  " ^ pad (Int.toString i ^ ".", 5) ^ " "
                  ^ showRatio r ^ "   " ^ w ^ "\n")
         ; i + 1 ))
      1 top)
  end

val () = print "sml-fuzzy fuzzy finder\n======================\n"
val () = List.app findFor ["kitten", "writting", "smiton"]
