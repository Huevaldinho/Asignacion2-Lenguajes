(* tipos_resultados.sml *)
(* declaración de un tipos de datos para todas las variantes de los resultados por generar *)

(* Excepciones provenientes del programa original
exception NoEstaEnElDominio of Identificador
exception NoEsUnaTautologia of (Identificador * bool) list
*)

datatype 'a Resultados =
          Bien              of 'a
     |    NoEstaEnElDominio of Identificador
     |    NoEsUnaTautologia of (Identificador * bool) list
;
