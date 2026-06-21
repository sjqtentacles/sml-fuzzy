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
