(* Evaluador de proposiciones.

   Hay un caso para cada variante de proposición.
*)

fun evalProp prop =
  case prop of
    constante valor
       => valor
  | negacion prop1
       => not (evalProp prop1)
  | conjuncion (prop1, prop2)
       => let val valor1 = evalProp prop1
              and valor2 = evalProp prop2
          in  valor1 andalso valor2
          end
  | disyuncion (prop1, prop2)
       => let val valor1 = evalProp prop1
              and valor2 = evalProp prop2
          in  valor1 orelse valor2
          end
  | implicacion (prop1, prop2)
       => let val valor1 = evalProp prop1
              and valor2 = evalProp prop2
          in  case (valor1, valor2) of
                (true, false) => false
              | _             => true
          end
  | equivalencia (prop1, prop2)
       => let val valor1 = evalProp prop1
              and valor2 = evalProp prop2
          in  valor1 = valor2
          end
;


