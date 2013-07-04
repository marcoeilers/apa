let f = fn x => (true)
in let g = fn k => (case Nil of Cons(y, ys) => (fn y => (false)) or k)
   in (g f)
