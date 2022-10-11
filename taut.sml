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
