(* Ambientes. *)
(* Los ambientes son representados como listas de pares de objetos *)

(* Por ahora no lo implementamos como un abstype.

   Podria ser asi:

   type (''a,'b) Ambiente = (''a * 'b) list

   Pero lo hacemos asi: *)

(* type Identificador = string : pasamos esta declaración al componente sintax.sml *)

type 'a Ambiente = (Identificador * 'a) list


(* Las siguientes declaraciones implementan la busqueda en el ambiente. 
   Cuando un identificador no esta definido en el ambiente, se levanta 
   una excepcion. *)

(* exception NoEstaEnElDominio of Identificador *)

fun busca ident []
    = NoEstaEnElDominio ident
|   busca ident ((ident',valor)::ambiente)
    = if ident = ident'
      then Bien valor
      else busca ident ambiente


(* Evaluador de proposiciones con variables.
   Hay un caso para cada variante de proposición.
*)

fun evalProp asignacion prop =
  case prop of
    constante valor
       => Bien valor
  | variable var
       => busca var asignacion
  | negacion prop1
       => (* not (evalProp asignacion prop1) *)
          (* Es necesario poner paréntesis alrededor del case, para que no se tome conjuncion (prop1, prop2) como parte del case *)
          (
          case evalProp asignacion prop1 of
            Bien valor => Bien (not valor)
          | otro       => otro
          ) 
  | conjuncion (prop1, prop2)
       => let val valor1 = evalProp asignacion prop1
              and valor2 = evalProp asignacion prop2
          in  (* valor1 andalso valor2 *)
            case (valor1, valor2) of
              (Bien v1, Bien v2) => Bien (v1 andalso v2)
            | (Bien v1, otro)    => otro
            | (otro,    _)       => otro (* damos preferencia a reportar el error de la izquierda *)
          end
  | disyuncion (prop1, prop2)
       => let val valor1 = evalProp asignacion prop1
              and valor2 = evalProp asignacion prop2
          in  (* valor1 orelse valor2 *)
            case (valor1, valor2) of
              (Bien v1, Bien v2) => Bien (v1 orelse v2)
            | (Bien v1, otro)    => otro
            | (otro,    _)       => otro (* damos preferencia a reportar el error de la izquierda *)
          end
  | implicacion (prop1, prop2)
       => let val valor1 = evalProp asignacion prop1
              and valor2 = evalProp asignacion prop2
          in  (* case (valor1, valor2) of
                   (true, false) => false
              | _                => true    *)
            case (valor1, valor2) of
              (Bien true , Bien false) => Bien false
            | (Bien v1, Bien v2)       => Bien true
            | (Bien v1, otro)          => otro
            | (otro,    _)             => otro (* damos preferencia a reportar el error de la izquierda *)
          end
  | equivalencia (prop1, prop2)
       => let val valor1 = evalProp asignacion prop1
              and valor2 = evalProp asignacion prop2
          in  (* valor1 = valor2 *)
            case (valor1, valor2) of
              (Bien v1, Bien v2) => Bien (v1 = v2)
            | (Bien v1, otro)    => otro
            | (otro,    _)       => otro (* damos preferencia a reportar el error de la izquierda *)
          end
;
