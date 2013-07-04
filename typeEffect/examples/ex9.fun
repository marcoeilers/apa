let snd = fn p => (case p of Pair(x1, x2) => x2)
in (snd Pair(true, 2))
