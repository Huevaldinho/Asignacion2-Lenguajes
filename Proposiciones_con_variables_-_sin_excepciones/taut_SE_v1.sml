(* Determinar si una proposición es una tautología *)

(* exception NoEsUnaTautologia of (Identificador * bool) list *)

fun taut prop =
    let
        (* variables de trabajo *)
        val variables = vars prop
        val n = length variables
        val lista_combinaciones_booleanas = gen_bools n
        (* generar evaluaciones de la proposición *)
        fun recorrer []                  = Bien true (* revisó todas las opciones, sí es un tautología *)
        |   recorrer (fila :: mas_filas) = 
                let
                    (* establecer una asociación entre variables y una combinación de valores booleanos (fila) *)
                    val asociacion = as_vals variables fila
                    (* esta asociación constituye un ambiente, o contexto, para evaluar la proposición prop *)
                    val evaluacion_es_verdadera = evalProp asociacion prop
                in
                    case evaluacion_es_verdadera of
                        (Bien true )  => recorrer mas_filas (* hasta ahora todas son verdaderas, continuar evaluando el resto*)
                    |   (Bien false ) => NoEsUnaTautologia asignacion (* no es verdadera, dentro de asignación está la razón *)
                    |   otro          => otro (* más profundamente hay un error y pasamos esa información al invocador *)
                    (* case delimitado por el end del let circundante *)
                end
        fun diagnosticar []         = "solo involucra constantes y la proposicion es falsa"
        |   diagnosticar asociacion = impr_as_vals asociacion        
    in
    	(* Análisis de casos *)
        case recorrer lista_combinaciones_booleanas of
            (Bien true)   => imprimir prop ^ " SÍ es una tautología"
        |   (Bien false ) => imprimir prop ^ " esto NO debería ocurrir..." (* IMPOSIBLE: en fun recorrer no se genera Bien false *)
        |   (NoEsUnaTautologia la_asociacion) => imprimir prop ^ " NO es una tautologia, porque " ^ diagnosticar la_asociacion
        |   (NoEstaEnElDominio ident)      => "En " ^ imprimir prop ^ " la variable " ^ ident ^ "no aparece (esto NO debería ocurrir)"
                                               (* IMPOSIBLE: generamos las variables a partir de la proposición *)
    end
;
