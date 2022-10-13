(* Ambientes. *)
(* Los ambientes son representados como listas de pares de objetos *)

(* Por ahora no lo implementamos como un abstype.

   Podria ser asi:

   type (''a,'b) Ambiente = (''a * 'b) list

   Pero lo hacemos asi: *)

type Identificador = string

type 'a Ambiente = (Identificador * 'a) list


(* Las siguientes declaraciones implementan la busqueda en el ambiente. 
   Cuando un identificador no esta definido en el ambiente, se levanta 
   una excepcion. *)

exception NoEstaEnElDominio of Identificador

fun busca ident []
    = raise NoEstaEnElDominio ident
|   busca ident ((ident',valor)::ambiente)
    = if ident = ident'
      then valor
      else busca ident ambiente


(* Evaluador de proposiciones con variables.
   Hay un caso para cada variante de proposición.
*)

fun evalProp ambiente prop =
  case prop of
    constante valor
       => valor
  | variable var
       => busca var ambiente
  | negacion prop1
       => not (evalProp ambiente prop1)
  | conjuncion (prop1, prop2)
       => let val valor1 = evalProp ambiente prop1
              and valor2 = evalProp ambiente prop2
          in  valor1 andalso valor2
          end
  | disyuncion (prop1, prop2)
       => let val valor1 = evalProp ambiente prop1
              and valor2 = evalProp ambiente prop2
          in  valor1 orelse valor2
          end
  | implicacion (prop1, prop2)
       => let val valor1 = evalProp ambiente prop1
              and valor2 = evalProp ambiente prop2
          in  case (valor1, valor2) of
                (true, false) => false
              | _             => true
          end
  | equivalencia (prop1, prop2)
       => let val valor1 = evalProp ambiente prop1
              and valor2 = evalProp ambiente prop2
          in  valor1 = valor2
          end
;


