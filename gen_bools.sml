(* Solución directa y natural.
   Idea original:
   Solución inductiva, mapear (true ::) y (false ::) sobre las listas con 2^(n-1) arreglos booleanos
   *)

(* construcción de listas 'curryficada' *)
fun cons x xs = x :: xs
;

(* combinaciones de valores booleanos *)
fun gen_bools 0 = [[]]
|   gen_bools n = let val anterior = gen_bools (n - 1)
                  in (map (cons true) anterior) @ (map (cons false) anterior)
                  end
;


(* concatenación de hileras 'curryficada'*)
fun pegar car hil = car ^ hil
;

(* generación de números binarios de n dígitos *)
fun gen_bins 0 = [""]
|   gen_bins n = let val anterior = gen_bins (n - 1)
                  in (map (pegar "0") anterior) @ (map (pegar "1") anterior)
                  end
;
