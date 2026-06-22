(* fuzzy.sml

   Implementations:
     - levenshtein/damerau: standard dynamic-programming edit distance, the
       latter (optimal string alignment) also rewarding adjacent
       transpositions.
     - ratio: 1 - distance / max length.
     - rank: stable (input-order tie-break) merge sort by descending ratio.
     - BKTree: Burkhard-Keller tree keyed on the Levenshtein metric, using the
       triangle inequality to prune the search.
     - soundex: classic American Soundex (Knuth, TAOCP vol. 3). *)

structure Fuzzy :> FUZZY =
struct
  fun min3 (a, b, c) = Int.min (a, Int.min (b, c))

  (* Edit-distance DP over two int-indexed vectors of character codes. The
     [transpose] flag adds the optimal-string-alignment transposition rule. *)
  fun editDistance transpose (a, b) =
    let
      val sa = Vector.fromList (map Char.ord (String.explode a))
      val sb = Vector.fromList (map Char.ord (String.explode b))
      val m = Vector.length sa
      val n = Vector.length sb
      (* d is an (m+1) x (n+1) matrix flattened row-major. *)
      val d = Array.array ((m + 1) * (n + 1), 0)
      fun idx (i, j) = i * (n + 1) + j
      fun get (i, j) = Array.sub (d, idx (i, j))
      fun set (i, j, v) = Array.update (d, idx (i, j), v)
      val () = Array.appi (fn (i, _) => set (i, 0, i)) (Array.array (m + 1, 0))
      val () = Array.appi (fn (j, _) => set (0, j, j)) (Array.array (n + 1, 0))
      fun fillRow i =
        if i > m then ()
        else
          let
            fun fillCol j =
              if j > n then ()
              else
                let
                  val ca = Vector.sub (sa, i - 1)
                  val cb = Vector.sub (sb, j - 1)
                  val cost = if ca = cb then 0 else 1
                  val base = min3 ( get (i - 1, j) + 1       (* deletion *)
                                  , get (i, j - 1) + 1       (* insertion *)
                                  , get (i - 1, j - 1) + cost (* substitution *) )
                  val v =
                    if transpose
                       andalso i > 1 andalso j > 1
                       andalso ca = Vector.sub (sb, j - 2)
                       andalso Vector.sub (sa, i - 2) = cb
                    then Int.min (base, get (i - 2, j - 2) + 1)
                    else base
                in
                  set (i, j, v); fillCol (j + 1)
                end
          in
            fillCol 1; fillRow (i + 1)
          end
      val () = fillRow 1
    in
      get (m, n)
    end

  fun levenshtein args = editDistance false args
  fun damerau args = editDistance true args

  fun ratio (a, b) =
    let
      val maxLen = Int.max (String.size a, String.size b)
    in
      if maxLen = 0 then 1.0
      else 1.0 - Real.fromInt (levenshtein (a, b)) / Real.fromInt maxLen
    end

  (* Jaro similarity. Characters of [a] and [b] match if they are equal and
     positioned within [matchWindow] of each other; [t] is the number of
     transpositions (matched characters that occur out of order), counted in
     halves. The formula is (m/|a| + m/|b| + (m-t)/m) / 3. *)
  fun jaro (a, b) =
    let
      val va = Vector.fromList (String.explode a)
      val vb = Vector.fromList (String.explode b)
      val la = Vector.length va
      val lb = Vector.length vb
    in
      if la = 0 andalso lb = 0 then 1.0
      else if la = 0 orelse lb = 0 then 0.0
      else
        let
          (* Window is at least 0; standard definition uses max/2 - 1. *)
          val matchWindow = Int.max (Int.max (la, lb) div 2 - 1, 0)
          val aMatched = Array.array (la, false)
          val bMatched = Array.array (lb, false)
          (* First pass: greedily mark matched characters in [b] for each
             character of [a], scanning [b] left to right within the window. *)
          fun markMatches i =
            if i >= la then ()
            else
              let
                val lo = Int.max (0, i - matchWindow)
                val hi = Int.min (i + matchWindow + 1, lb)
                fun scan j =
                  if j >= hi then ()
                  else if not (Array.sub (bMatched, j))
                          andalso Vector.sub (va, i) = Vector.sub (vb, j)
                  then (Array.update (aMatched, i, true);
                        Array.update (bMatched, j, true))
                  else scan (j + 1)
              in
                scan lo; markMatches (i + 1)
              end
          val () = markMatches 0
          val m = Array.foldl (fn (b, acc) => if b then acc + 1 else acc) 0 aMatched
        in
          if m = 0 then 0.0
          else
            let
              (* Second pass: walk matched characters of [a] and [b] in order;
                 each position where they disagree is a half-transposition. *)
              fun collect (arr, vec) =
                let
                  val n = Array.length arr
                  fun go (k, acc) =
                    if k >= n then List.rev acc
                    else if Array.sub (arr, k)
                    then go (k + 1, Vector.sub (vec, k) :: acc)
                    else go (k + 1, acc)
                in
                  go (0, [])
                end
              val matchedA = collect (aMatched, va)
              val matchedB = collect (bMatched, vb)
              val halfTranspositions =
                ListPair.foldl
                  (fn (ca, cb, acc) => if ca = cb then acc else acc + 1)
                  0 (matchedA, matchedB)
              val t = Real.fromInt halfTranspositions / 2.0
              val mr = Real.fromInt m
            in
              (mr / Real.fromInt la
               + mr / Real.fromInt lb
               + (mr - t) / mr) / 3.0
            end
        end
    end

  (* Jaro-Winkler: boost the Jaro score by the length of the common prefix
     (capped at 4) scaled by p = 0.1. *)
  fun jaroWinkler (a, b) =
    let
      val j = jaro (a, b)
      val maxPrefix = 4
      fun commonPrefix (i, limit) =
        if i >= limit then i
        else if String.sub (a, i) = String.sub (b, i) then commonPrefix (i + 1, limit)
        else i
      val limit = Int.min (maxPrefix, Int.min (String.size a, String.size b))
      val l = Real.fromInt (commonPrefix (0, limit))
      val p = 0.1
    in
      j + l * p * (1.0 - j)
    end

  (* Stable merge sort: equal keys retain their original relative order, which
     gives the deterministic tie-break required by [rank]. *)
  fun stableSort cmp xs =
    let
      fun merge ([], ys) = ys
        | merge (xs, []) = xs
        | merge (x :: xs, y :: ys) =
            (case cmp (x, y) of
                 GREATER => y :: merge (x :: xs, ys)
               | _       => x :: merge (xs, y :: ys))  (* keep x first on ties *)
      (* Contiguous split (first half / second half) keeps the sort stable. *)
      fun split ys =
        let
          val n = length ys div 2
          fun take (0, rest) = ([], rest)
            | take (_, []) = ([], [])
            | take (k, z :: zs) =
                let val (l, r) = take (k - 1, zs) in (z :: l, r) end
        in
          take (n, ys)
        end
      fun sort [] = []
        | sort [x] = [x]
        | sort ys =
            let val (l, r) = split ys
            in merge (sort l, sort r) end
    in
      sort xs
    end

  fun rank {query, candidates} =
    let
      val scored = map (fn c => (c, ratio (query, c))) candidates
      (* Descending by ratio; ties fall through to stable input order. *)
      fun cmp ((_, r1), (_, r2)) =
        if r1 > r2 then LESS
        else if r1 < r2 then GREATER
        else EQUAL
    in
      stableSort cmp scored
    end

  structure BKTree =
  struct
    (* A node holds a word and children keyed by their Levenshtein distance to
       it; Empty is the unindexed tree. *)
    datatype tree = Empty | Node of string * (int * tree) list

    val empty = Empty

    fun insert Empty w = Node (w, [])
      | insert (node as Node (word, children)) w =
          let
            val dist = levenshtein (word, w)
          in
            if dist = 0 then node  (* duplicate: already indexed *)
            else
              let
                fun go [] = [(dist, insert Empty w)]
                  | go ((d, child) :: rest) =
                      if d = dist then (d, insert child w) :: rest
                      else (d, child) :: go rest
              in
                Node (word, go children)
              end
          end

    fun search Empty _ _ = []
      | search (Node (word, children)) q k =
          let
            val dist = levenshtein (word, q)
            val here = if dist <= k then [word] else []
            (* Triangle inequality: only children within [dist-k, dist+k] can
               hold a match. *)
            val matches =
              List.concat
                (map (fn (d, child) =>
                        if d >= dist - k andalso d <= dist + k
                        then search child q k
                        else [])
                     children)
          in
            here @ matches
          end
  end

  (* Classic American Soundex (Knuth, TAOCP vol. 3, section 6.1). h and w are
     transparent (do not break a run of equal codes); the vowels a,e,i,o,u and
     y are separators that do break such a run. *)
  fun soundex name =
    let
      fun upper c = Char.toUpper c
      val letters = List.filter Char.isAlpha (map upper (String.explode name))
    in
      case letters of
          [] => ""
        | first :: rest =>
            let
              (* digit code for a coded consonant; NONE for vowels/h/w. *)
              fun code c =
                case c of
                    #"B" => SOME #"1" | #"F" => SOME #"1"
                  | #"P" => SOME #"1" | #"V" => SOME #"1"
                  | #"C" => SOME #"2" | #"G" => SOME #"2"
                  | #"J" => SOME #"2" | #"K" => SOME #"2"
                  | #"Q" => SOME #"2" | #"S" => SOME #"2"
                  | #"X" => SOME #"2" | #"Z" => SOME #"2"
                  | #"D" => SOME #"3" | #"T" => SOME #"3"
                  | #"L" => SOME #"4"
                  | #"M" => SOME #"5" | #"N" => SOME #"5"
                  | #"R" => SOME #"6"
                  | _ => NONE
              fun isSep c =
                c = #"A" orelse c = #"E" orelse c = #"I" orelse c = #"O"
                orelse c = #"U" orelse c = #"Y"
              (* prev: code of the previous coded/seen letter, "0" after a
                 separator, used to collapse adjacent equal codes. *)
              fun loop ([], _, acc) = acc
                | loop (c :: cs, prev, acc) =
                    if c = #"H" orelse c = #"W" then
                      loop (cs, prev, acc)  (* transparent: prev unchanged *)
                    else if isSep c then
                      loop (cs, #"0", acc)
                    else
                      (case code c of
                           SOME d =>
                             if d = prev then loop (cs, d, acc)
                             else loop (cs, d, d :: acc)
                         | NONE => loop (cs, prev, acc))
              val startPrev = case code first of SOME d => d | NONE => #"0"
              val digits = List.rev (loop (rest, startPrev, []))
              val three = List.take (digits @ [#"0", #"0", #"0"], 3)
            in
              String.implode (first :: three)
            end
    end
end
