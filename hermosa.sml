(*
Asignación 2 - Lenguajes de Programación - Ignacio Trejos Zelaya
Realizada por:
    Alonso Garita Granados, carné 2021030220
    Fernand aSanabria Solano María, carné 2021005572 
    Raquel Marcela Arguedas Sánchez, carné 2021032567
    Felipe de Jesús Obando Arrieta, carné 2021035489
*)


(*
Función encargada de escribir en el archivo
Autor: John Coleman
Fuente: https://stackoverflow.com/questions/33597175/how-to-write-to-a-file-in-sml
Ediciones: Alonso Garita Granados
*)
fun printToOutStream outstream str = let val os = outstream in
    TextIO.output(os,str);
    TextIO.closeOut os
end;



(*
Función hermosa
Creada por: Raquel Arguedas Sánchez
*)
fun hermosa (str) =         (*recibe como entrada, la salida de bonita*)
let 
    val counter = ref 0     (*contador para recorrer el string*)
    val strNueva = ref ""   (*string de salida*)
in
    while !counter < (size str) do
        (if ( (String.sub(str, !counter) = #"t") andalso (!counter + 3 < size str) andalso (substring (str, !counter, 4) = "true") ) then 
                ( strNueva := !strNueva ^ "\\texttt{true}"; counter := !counter+2 )     (*concatena el true*)

        else if ( (String.sub(str, !counter) = #"f") andalso (!counter + 4 < size str) andalso (substring (str, !counter, 5) = "false") ) then 
                ( strNueva := !strNueva ^ "\\texttt{false}"; counter := !counter+3 )     (*concatena el false*)

        else if (String.sub(str, !counter) = #"~") then 
                  strNueva := !strNueva ^ "\\neg"                                (*concatena la negación*)
                
        else if (String.sub(str, !counter) = #"&") then 
                  ( strNueva := !strNueva ^ "\\wedge"; counter := !counter+1 )     (*concatena la conjuncion*)

        else if (String.sub(str, !counter) = #"|") then 
                  ( strNueva := !strNueva ^ "\\vee"; counter := !counter+1 )     (*concatena la disyunción*)

        else if (String.sub(str, !counter) = #"=") then 
                  ( strNueva := !strNueva ^ "\\Rightarrow"; counter := !counter+1 )     (*concatena la implicación*)

        else if (String.sub(str, !counter) = #"<") then 
                 ( strNueva := !strNueva ^ "\\Leftrightarrow"; counter := !counter+2 )       (*concatena la equivalencia lógica*)
        
        else strNueva := !strNueva ^ Char.toString (String.sub(str, !counter)); (*concatena todo lo demás*)
        counter := !counter+1                                                   (*aumenta el contador, para ir al siguiente caracter*)
        ); printToOutStream (!strNueva) (*!strNueva*) end; 


