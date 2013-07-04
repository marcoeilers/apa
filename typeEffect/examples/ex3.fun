let map = 
         (fun m f1 => 
             (fn l => 
                 (case l of 
                 Cons(x, xs) => (Cons((f1 x), ((m f1) xs))) 
                 or (Nil))))
in map

