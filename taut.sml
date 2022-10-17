(*
	Creador: Ignacio Trejos Zelaya
	
	5. La función taut determina si una proposición lógica es una tautología, esto es, 
	una fórmula lógica que evalúa a verdadera (true), para toda posible asignación 
	de valores de verdad a las variables presentes en la fórmula.

	- Si la proposición lógica sí es una tautología, la función muestra la fórmula, 
	seguida de la leyenda “es una tautología”.

	- Si la proposición lógica no es una tautología, la función muestra la fórmula, 
	seguida por la leyenda “no es una tautología” y muestra (al menos) una de las asignaciones
	de valores que produjeron false como resultado de la evaluación de la fórmula con esa asignación de valores.

	Por ejemplo,
		-(p ∨ ¬p) sí es una tautología.
		-(q ⇒ ¬q) no es una tautología, porque q = true la falsifica (la hace falsa).

*)

(* Determinar si una proposición es una tautología *)

exception NoEsUnaTautologia of (Identificador * bool) list

fun taut prop =
    let
    	(* variables de trabajo *)
    	val variables = vars prop
    	val n = length variables
    	val lista_combinaciones_booleanas = gen_bools n
    	(* generar evaluaciones de la proposición*)
    	fun recorrer []                  = true
		|   recorrer (fila :: mas_filas) = 
    		    let
		        	(* establecer una asociación entre variables y una combinación de valores booleanos (fila) *)
                    val asociacion = as_vals variables fila
                    (* esta asociación constituye un ambiente, o contexto, para evaluar la proposición prop *)
		        	val evaluacion_es_verdadera = evalProp asociacion prop
		        in
		        	if evaluacion_es_verdadera then
		        		recorrer mas_filas (* podría ser una tautología, continuar evaluando otras combinaciones *)
		        	else
		        	    raise NoEsUnaTautologia asociacion (* no es necesario continuar evaluando otras filas *)
		        end
    in
    	if recorrer lista_combinaciones_booleanas then
            imprimir prop ^ " SI es una tautologia"
        else
            "" (* este else nunca se activará *)
    end handle NoEsUnaTautologia la_asociacion =>
        let
            fun diagnosticar []         = "solo involucra constantes y la proposicion es falsa"
            |   diagnosticar asociacion = impr_as_vals asociacion
        in
            imprimir prop ^ " NO es una tautologia, porque " ^ diagnosticar la_asociacion
        end
;
