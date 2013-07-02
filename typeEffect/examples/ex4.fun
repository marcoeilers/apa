let f = fn x => (true)
in let g = fn k => (if (f 0) then k else (fn y => (false)))
   in (g f)
