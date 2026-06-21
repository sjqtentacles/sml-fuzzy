(* Tests for sml-fuzzy: Levenshtein and Damerau (OSA) edit distances, the
   normalized similarity ratio, deterministic best-first ranking, the BK-tree
   index, and classic American Soundex (Knuth, TAOCP vol. 3 vectors). *)

structure FuzzyTests =
struct
  open Harness

  (* Portable stable insertion sort of strings, ascending. Used only to make
     set-membership assertions order-independent. *)
  fun sortStr xs =
    let
      fun insert (x, []) = [x]
        | insert (x, y :: ys) = if x <= y then x :: y :: ys else y :: insert (x, ys)
    in
      foldr insert [] xs
    end

  fun run () =
    let
      val () = section "Levenshtein"
      val () = checkInt "kitten/sitting" (3, Fuzzy.levenshtein ("kitten", "sitting"))
      val () = checkInt "flaw/lawn" (2, Fuzzy.levenshtein ("flaw", "lawn"))
      val () = checkInt "empty/abc" (3, Fuzzy.levenshtein ("", "abc"))
      val () = checkInt "abc/empty" (3, Fuzzy.levenshtein ("abc", ""))
      val () = checkInt "identical" (0, Fuzzy.levenshtein ("same", "same"))
      val () = checkInt "both empty" (0, Fuzzy.levenshtein ("", ""))
      val () = checkInt "single substitution" (1, Fuzzy.levenshtein ("cat", "cot"))

      val () = section "Damerau (adjacent transposition)"
      val () = checkInt "ca/ac transposition" (1, Fuzzy.damerau ("ca", "ac"))
      val () = checkInt "levenshtein ca/ac is 2" (2, Fuzzy.levenshtein ("ca", "ac"))
      val () = checkInt "damerau identical" (0, Fuzzy.damerau ("abc", "abc"))
      val () = checkInt "damerau empty/abc" (3, Fuzzy.damerau ("", "abc"))
      val () = checkInt "damerau kitten/sitting" (3, Fuzzy.damerau ("kitten", "sitting"))

      val () = section "Ratio"
      val () = check "identical is 1.0" (Real.== (Fuzzy.ratio ("abc", "abc"), 1.0))
      val () = check "both empty is 1.0" (Real.== (Fuzzy.ratio ("", ""), 1.0))
      val () = check "disjoint is 0.0" (Real.== (Fuzzy.ratio ("abc", "xyz"), 0.0))
      val () = check "kitten/sitting ~ 0.571"
                 (Real.abs (Fuzzy.ratio ("kitten", "sitting") - (4.0 / 7.0)) < 1.0E~9)
      val () = check "ratio in [0,1]"
                 (let val r = Fuzzy.ratio ("flaw", "lawn")
                  in r >= 0.0 andalso r <= 1.0 end)

      val () = section "Rank"
      val ranked = Fuzzy.rank
                     { query = "kitten"
                     , candidates = ["sitting", "mitten", "kitten", "dog"] }
      val () = checkStringList "ranked order"
                 (["kitten", "mitten", "sitting", "dog"], map #1 ranked)
      val () = check "best is exact match (ratio 1.0)"
                 (Real.== (#2 (hd ranked), 1.0))
      val () = checkString "best word" ("kitten", #1 (hd ranked))
      val () = check "ranks descending"
                 (let val rs = map #2 ranked
                  in ListPair.all (fn (a, b) => a >= b) (rs, tl rs @ [0.0]) end)
      (* Deterministic tie-break: equal ratios keep input order. *)
      val tie = Fuzzy.rank { query = "aa", candidates = ["xx", "yy", "zz"] }
      val () = checkStringList "tie-break preserves input order"
                 (["xx", "yy", "zz"], map #1 tie)

      val () = section "BK-tree"
      val dict = ["book", "books", "boo", "cook", "cake", "cape", "boon", "back"]
      val tree = foldl (fn (w, t) => Fuzzy.BKTree.insert t w) Fuzzy.BKTree.empty dict
      val () = checkStringList "within distance 1 of book"
                 (["boo", "book", "books", "boon", "cook"],
                  sortStr (Fuzzy.BKTree.search tree "book" 1))
      val () = checkStringList "distance 0 finds exact only"
                 (["book"], Fuzzy.BKTree.search tree "book" 0)
      val () = check "missing word at distance 0 finds nothing"
                 (null (Fuzzy.BKTree.search tree "zzz" 0))
      val () = check "empty tree finds nothing"
                 (null (Fuzzy.BKTree.search Fuzzy.BKTree.empty "anything" 5))
      val () = check "every result is within distance"
                 (List.all (fn w => Fuzzy.levenshtein ("cake", w) <= 2)
                    (Fuzzy.BKTree.search tree "cake" 2))

      val () = section "Soundex (Knuth TAOCP vol. 3)"
      val () = checkString "Robert" ("R163", Fuzzy.soundex "Robert")
      val () = checkString "Rupert" ("R163", Fuzzy.soundex "Rupert")
      val () = checkString "Rubin" ("R150", Fuzzy.soundex "Rubin")
      val () = checkString "Ashcraft" ("A261", Fuzzy.soundex "Ashcraft")
      val () = checkString "Tymczak" ("T522", Fuzzy.soundex "Tymczak")
      val () = checkString "Honeyman" ("H555", Fuzzy.soundex "Honeyman")
      val () = checkString "Robert/Rupert agree"
                 (Fuzzy.soundex "Robert", Fuzzy.soundex "Rupert")
    in
      ()
    end
end
