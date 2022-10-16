(* Producir la tabla de verdad de una proposición con variables *)

(* Generar (como dato) la tabla de verdad de una proposición con variables *)
fun tabla_dato prop =
    let
        (* variables de trabajo *)
        val variables = vars prop
        val n = length variables
        val lista_combinaciones_booleanas = gen_bools n
        (* generar evaluaciones de la proposición*)
        fun recorrer []                  = []  (* toque final a la generación; ya fueron generadas las filas precedentes *)
        |   recorrer (fila :: mas_filas) = 
                let
                    (* establecer una asociación entre variables y una combinación de valores booleanos (fila) *)
                    val asociacion = as_vals variables fila
                    (* esta asociación constituye un ambiente, o contexto, para evaluar la proposición prop *)
                    val resultado_fila = evalProp asociacion prop
                in
                    (asociacion, resultado_fila) (* la asociación variables+booleanos apareada con el resultado de la evaluación *)
                    :: (* como cabeza de una lista que continúa *)
                    recorrer mas_filas (* continuar el trabajo *)
                end
    in
        recorrer lista_combinaciones_booleanas
    end
;


(* Imprimir la tabla de verdad de una proposición con variables *)
fun tabla prop =
    let
    	(* variables de trabajo *)
    	val variables = vars prop
    	val n = length variables
    	val lista_combinaciones_booleanas = gen_bools n
    	(* imprimir una fila de la tabla de verdad *)
    	fun imprimir_fila vars_bools es_verdadero =
    		print ( impr_as_vals vars_bools ^ " === " ^ (if es_verdadero then "true" else "false") ^ "\n" ) 
    	(* generar evaluaciones de la proposición*)
    	fun recorrer []                  = print "\n"  (* toque final a la impresión; previamente mostramos hileras con el resultado *)
		|   recorrer (fila :: mas_filas) = 
    		    let
		        	(* establecer una asociación entre variables y una combinación de valores booleanos (fila) *)
                    val asociacion = as_vals variables fila
                    (* esta asociación constituye un ambiente, o contexto, para evaluar la proposición prop *)
 	    		    val resultado_fila = evalProp asociacion prop
                in
            	    imprimir_fila  asociacion  resultado_fila (* efecto: imprimir fila y su evaluación *)
            	    ;
            	    recorrer mas_filas (* continuar el trabajo *)
            	end
    in
        recorrer lista_combinaciones_booleanas
    end
;


(* Mostrar la tabla de verdad de una proposición con variables, como una hilera *)

fun tabla_str prop =
    let
    	(* variables de trabajo *)
    	val variables = vars prop
    	val n = length variables
    	val lista_combinaciones_booleanas = gen_bools n
    	(* imprimir una fila de la tabla de verdad *)
    	fun mostrar_fila vars_bools es_verdadero =
    		impr_as_vals vars_bools ^ " === " ^ (if es_verdadero then "true" else "false") ^ "\n" 
    	(* generar evaluaciones de la proposición*)
    	fun recorrer []                  = ""  (* toque final a la generación; previamente generamos hileras como parte del resultado *)
		|   recorrer (fila :: mas_filas) = 
    		    let
		        	(* establecer una asociación entre variables y una combinación de valores booleanos (fila) *)
                    val asociacion = as_vals variables fila
                    (* esta asociación constituye un ambiente, o contexto, para evaluar la proposición prop *)
 	    		    val resultado_fila = evalProp asociacion prop
                in
            	    mostrar_fila  asociacion  resultado_fila (* mostrar fila y su evaluación *)
            	    ^ (* concatenar hileras *)
            	    recorrer mas_filas (* continuar el trabajo *)
            	end
    in
        recorrer lista_combinaciones_booleanas
    end
;
