(* Evaluador de proposiciones.

   Hay un caso para cada variante de proposición.
*)

fun min (x,y) = if x < y then x else y
;

fun max (x,y) = if x > y then x else y
;

fun bool2int true  = 1
|   bool2int false = 0


fun evalPropNum prop =
  case prop of
    constante valor
       => bool2int valor
  | negacion prop1
       => 1 - (evalPropNum prop1)
  | conjuncion (prop1, prop2)
       => let val valor1 = evalPropNum prop1
              and valor2 = evalPropNum prop2
          in  min (valor1, valor2)
          end
  | disyuncion (prop1, prop2)
       => let val valor1 = evalPropNum prop1
              and valor2 = evalPropNum prop2
          in  max (valor1, valor2)
          end
  | implicacion (prop1, prop2)
       => let val valor1 = evalPropNum prop1
              and valor2 = evalPropNum prop2
          in  bool2int (valor1 <= valor2)
          end
  | equivalencia (prop1, prop2)
       => let val valor1 = evalPropNum prop1
              and valor2 = evalPropNum prop2
          in  bool2int (valor1 = valor2)
          end
;


