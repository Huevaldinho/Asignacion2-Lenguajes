(* Lenguaje de proposiciones con constantes. No tiene variables *)

(* Aqui definimos la sintaxis abstracta de nuestro pequenno
   lenguaje de proposiciones con constantes *)

datatype PropConst =
  constante    of bool
| negacion     of PropConst
| conjuncion   of PropConst * PropConst
| disyuncion   of PropConst * PropConst
| implicacion  of PropConst * PropConst
| equivalencia of PropConst * PropConst
;

nonfix ~:
val ~: = negacion

infix 7 :&&:
val (op :&&:) = conjuncion

infix 6 :||:
val (op :||:) = disyuncion

infixr 5 :=>:
val (op :=>:) = implicacion

infix 4 :<=>:
val (op :<=>:) = equivalencia

infix 4 :++:
val (op :++:) = fn (p, q) => ~: (p :<=>: q)

;