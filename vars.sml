(*
	Creador: Ignacio Trejos Zelaya

	1. La función vars determina la lista de las distintas variables proposicionales que
	aparecen en una fórmula lógica (proposición). La lista no debe tener elementos repetidos.
*)


(* filter: filtra una lista de acuerdo con un predicado p *)
fun filter p []      = []
|   filter p (x::xs) = if p x then x :: filter p xs else filter p xs
;


(* nub: obtiene una lista sin duplicados a partir de una lista arbitraria *)
fun nub []      = []
|   nub (x::xs) = x :: (nub (filter (fn y => x <> y) xs))
;


(* Extraer variables en una proposición *)
(* Estrategia:
   - sacar variables una a una en listas unitarias,
   - concatenar listas cuando hay conectivos lógicos,
   - eliminar duplicados (si lo hay) en la lista final *)

fun vars prop =
let
	fun las_vars prop =
	  case prop of
	    constante _
	       => []
	  | variable var
	       => [var]
	  | negacion prop1
	       => las_vars prop1
	  | conjuncion (prop1, prop2)
	       => let val vars1 = las_vars prop1
	              and vars2 = las_vars prop2
	          in  vars1 @ vars2
	          end
	  | disyuncion (prop1, prop2)
	       => let val vars1 = las_vars prop1
	              and vars2 = las_vars prop2
	          in  vars1 @ vars2
	          end
	  | implicacion (prop1, prop2)
	       => let val vars1 = las_vars prop1
	              and vars2 = las_vars prop2
	          in  vars1 @ vars2
	          end
	  | equivalencia (prop1, prop2)
	       => let val vars1 = las_vars prop1
	              and vars2 = las_vars prop2
	          in  vars1 @ vars2
	          end
in
    nub (las_vars prop) (* elimina valores repetidos *)
end
;

