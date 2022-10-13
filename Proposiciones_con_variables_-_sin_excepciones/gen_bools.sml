(* Solución directa y natural.
   Idea original:
   Solución inductiva, mapear (true ::) y (false ::) sobre las listas con 2^(n-1) arreglos booleanos
   *)

fun cons x xs = x :: xs
;

fun gen_bools 0 = [[]]
|   gen_bools n = let val anterior = gen_bools (n - 1)
                  in (map (cons true) anterior) @ (map (cons false) anterior)
                  end
;

fun pegar car hil = car ^ hil
;

fun gen_bins 0 = [""]
|   gen_bins n = let val anterior = gen_bins (n - 1)
                  in (map (pegar "1") anterior) @ (map (pegar "0") anterior)
                  end
;
