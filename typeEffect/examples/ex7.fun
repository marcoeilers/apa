let f = fn x => (true)
in let g = fn k => (case Nil of Cons(y, ys) => k or (fn y => (false)))
   in (g f)
