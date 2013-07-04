let f = fn x => (true)
in let g = fn k => (if (f 0) then (fn y => (false)) else k)
   in (g f)
