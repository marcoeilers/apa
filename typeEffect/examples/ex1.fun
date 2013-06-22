let f = fn x => (x + 1)
in let g = fn y => (y * 2)
   in let h = fn z => (if z < 0 then f else g)
      in (case (Pair(f, (fn i => (f i)))) of Pair(x1, x2) => Cons(x2, Nil))

