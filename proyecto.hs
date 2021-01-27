
module Main where

-------------------------------------------------------------------------------------------------------------
--Objetos 
--Ejemplos obtenidos de https://wiki.uqbar.org/wiki/articles/data--definiendo-nuestros-tipos-en-haskell.html
-------------------------------------------------------------------------------------------------------------
data Pool = 
  OnePool
    {poolTitle      :: String ,
    listQues        :: [Question]}
    deriving (Show,Eq)
    
data Question =
    OneQuestion
    {question       :: String,
    questResponse   :: [String],
    respEnter       :: [String],
    questType       :: Int}
    deriving (Show,Eq)
--------------------------------------------------------------------------------
            --Creando lista de datos tipo encuesta
--------------------------------------------------------------------------------
e1 :: Pool
e1 = OnePool "Saprissa"  [ 
    OneQuestion "Cree ud que Centeno es buen entrenador?" ["Califiquelo del 1 al 5"] [] 1,
    OneQuestion "Es ud aficionado?" ["a)si", "b)no"] [] 0,
    OneQuestion "Que tan aficionado es?" ["Califique del 1 al 5"] [] 1,
    OneQuestion "Saprissa es el mejor equipo de CR?" ["a)De acuerdo", "b)Muy deacuerdo", "c)Desacuerdo", "d)Muy desacuerdo"] [] 0 ]

e2 :: Pool
e2 = OnePool "Precidente" [
    OneQuestion "Como catalogaria el desempenno del gobierno en el manejo de la pandemia por COVID?" ["a)Malo", "b)Regular", "c)Bueno", "d)Muy bueno"] [] 0, 
    OneQuestion "Estaria de acuerdo en apoyar el cobro de más impuestos para promover el levantamiento de la economia?"  ["a)Si", "b)No"] [] 0] 

e3 :: Pool
e3 = OnePool "Ciudadania" [
    OneQuestion "Eficiencia en temas de informacion a la ciudadania" 
    ["responda con una escala de 1 a 5, donde 1 = Muy Malo, 2 = Malo, 3 = Regular, 4 = Bueno, 5 = Muy Bueno"] [] 1,
    OneQuestion "Control del gasto del aparato estatal" 
    ["responda con una escala de 1 a 5, donde 1 = Muy Malo, 2 = Malo, 3 = Regular, 4 = Bueno, 5 = Muy Bueno"] [] 1,
    OneQuestion "Grado de Éxito en el control de la criminalidad" 
    ["responda con una escala de 1 a 5, donde 1 = Muy Malo, 2 = Malo, 3 = Regular, 4 = Bueno, 5 = Muy Bueno"] [] 1
    ] 

-----------------------------------------------------
            --Lista donde guardo encuestas
-----------------------------------------------------
listForms :: [Pool]
listForms = [e1, e2, e3]

----------------------------______________________________FUNCIONES______________________________--------------------------

---------------------------------------------------------------
            --Función principal para correr el proyecto
---------------------------------------------------------------
main :: IO b
main = do mainMenuFunc listForms

-------------------------------------------------------
            --Funcion para pegar listas tipo encuesta
-------------------------------------------------------
pasteList :: Pool -> [Pool] -> [Pool]
pasteList l b = l : b

------------------------------------------------------
            --Funcion para pegar listas cuaquieras
------------------------------------------------------
pasteListR :: a -> [a] -> [a]
pasteListR l b = l : b

----------------------------------------------------------------
            --Función para sacar los que se repiten en una lista
----------------------------------------------------------------
withOutRepeate :: Eq a => [a] -> [a]
withOutRepeate listaNum | listaNum == [] = []
    | any (== head listaNum) (tail listaNum) = withOutRepeate (tail listaNum)
    | otherwise = (take 1 listaNum) ++ withOutRepeate (tail listaNum)

-------------------------------------------------------------
        --Función para sacar las respuestas repetidas
-------------------------------------------------------------
repeateList :: (Eq t, Num t, Monad m, Eq a) => [a] -> t -> [a] -> [[a]] -> m [[a]]
repeateList list index otherList newList = do

    if index == 0 then
        return newList
    else do
        let temp    = (filter (\ x -> (x == (head otherList))) list)
        let p       = [temp] ++ newList                                                             --------FILTER
        repeateList list (index - 1) (tail otherList) p

----------------------------------------------------------
            --Funcion menuPrincipal
----------------------------------------------------------
mainMenuFunc :: [Pool] -> IO b
mainMenuFunc lista = do  
    
        putStrLn "\n-----Menu principal----- \n [1]- Crear encuesta\n [2]- Llenar encuesta\n"
        x <- readLn
    
        if x == 1 then 
            do 
                        putStrLn "\n----Creando encuesta-----\n Ingrese el nombre de la encuesta:  "
                        registForm lista
        else do
            putStrLn    "\n Encuestas: "
            print       (map (poolTitle) lista)                                                         --MAP   
            print       "Ingrese el nombre de la encuesta a llenar: "
            e           <- getLine

            putStrLn    "\n [1] Llenar a mano \n [2] Llenar de manera atomática"
            p           <- getLine

            putStrLn    "Ingrese la cantidad de formularios a llenar: "
            a           <- getLine
            let b       = read a ::Int

            if p == "1" then 
                do
                        form <- foundList lista e
                        showQuestions form lista b []
            else 
                do 
                        form <- foundList lista e
                        responseAutomatic form lista b []

-------------------------------------------------------------------
            --Funcion para mostrar Pregunas de la encuesta manual
-------------------------------------------------------------------
showQuestions :: Pool -> [Pool] -> Int -> [Question] -> IO b
showQuestions param l quantityForms list_q  = do
        
        let headList    = (listQues param)
        let new         =  OnePool {
        poolTitle       = (poolTitle param),
        listQues        = list_q}

        putStrLn        "\n-----------------------------------NUEVA ENCUESTA ------------------------------------------------"

        if (length list_q) == 0 
            then do
                    printQuestion headList (length headList) l param quantityForms []
        else if quantityForms == 0
            then do   
                    putStrLn "\n ESTADISTICAS DE LA ENCUESTA MANUAL:"
                    print (poolTitle new)
                    putStrLn "\n"
                    printStadist list_q (length list_q) l
        else do
                    printQuestion (listQues new) (length headList) l param quantityForms []

----------------------------------------------------------------
            --Funcion de estadisticas de las respuestas
----------------------------------------------------------------
printStadist :: [Question] -> Int -> [Pool] -> IO b
printStadist listQ zise l = do

        let p    = (head listQ)
        let      listIn = (respEnter p)
        let      t = (length (respEnter p))

        if zise == 0 then 
            mainMenuFunc l
        else if (questType p) == 0 then
            do
                putStrLn            "\nRESULTADOS DE PREGUNTAS DE SELECCION UNICA"
                putStrLn            ("PREGUNTA: ")
                putStrLn            (question p)
                putStrLn            ("OBCIONES: " )
                print               (questResponse p)
                putStrLn            ("RESPUESTAS: ")
                print               (respEnter p)
                putStrLn            ("\nRESPUESTA MAS SELECCIONADA: ")

                big                 <- bigger listIn 1 "" t 0
                let listNoRepeat    = withOutRepeate listIn
                d                   <- repeateList listIn (length(listNoRepeat)) listNoRepeat [] 

                if (length d) == length(filter (\ x -> (length x == length(head d))) d) then            -----------FILTER
                    do
                        putStrLn     "Hubo una igualdad en las respuestas!"
                        print        d
                        putStrLn     ("TOTAL DE CADA TIPO: " )
                        print        (length(head d))
                else 
                    do 
                        print        ("SELECCIONADA: ")
                        print        (big !!1)
                        print        ("CANTIDAD: " )
                        print        (big !!0)
            
                putStrLn             "\nRESPUESTA MENOS SELECCIONADA Y TOTAL: "
                small                <- smaller listIn 10 "" t 0
                putStrLn             "OBCION"

                let b                = filter (\x -> ((head listIn) == x)) listIn ----------------FILTER
                let x                = filter (\x -> ((head listIn) == x)) listIn ----------------FILTER

                if  length(b) == length(listIn) || (length d) == length(filter (\ x -> (length x == length(head d))) d) ||
                    length(x) == length(listIn) then
                    do
                        putStrLn     "Ninguna!"
                        putStrLn     "CANTIDAD"
                        print        0
                        printStadist (tail listQ) (zise - 1) l
                else do
                    
                    print            (small !!1)
                    putStrLn         "CANTIDAD"
                    print            (small !!0)
                    printStadist     (tail listQ) (zise - 1) l

        else do
            putStrLn                 "\nRESULTADOS DE PREGUNTAS POR ESCALA"
            putStrLn                 "PREGUNTA: "
            print                    (question p)
            putStrLn                 "OBCION"
            print                    (questResponse p)
            putStrLn                 ("RESPUESTAS")
            print                    (respEnter p)
             
            putStrLn                 "\nCANTIDAD DE CALIFICACIONES DE 3 EN ADELANTE"
            print                    (length(filter (\ x -> ((x == "3") || (x == "4") || (x == "5")) ) listIn))-----FILTER
            putStrLn                    "CANTIDAD DE CALIFICACIONES MENORES A 3"
            print                    (length(filter (\ x -> ((x == "1") || (x == "2")) ) listIn))              -----FILTER
             
            printStadist             (tail listQ) (zise - 1) l

------------------------------------------------------------------------------------
            --Función para extraer la respuesta más selecionado de cada pregunta
------------------------------------------------------------------------------------
bigger :: Monad m => [String] -> Int -> String -> Int -> Int -> m [String]
bigger list tempInt tempStr size ind = do
    
        if size == 0 then  
            return      ([(show tempInt), tempStr])
        else
            do
            let i = list !!ind
            let index   = length (filter (\ x -> x == (i)) list)                                                  ------FILTER

            if (tempInt < index) then 
                do bigger (list) index (head list) (size - 1) (ind + 1)
            else 
                do bigger list tempInt tempStr (size - 1) (ind + 1)

-------------------------------------------------------------------------------------
             --Función para extraer la pregunta menos selecionada de cada pregunta
-------------------------------------------------------------------------------------
smaller :: Monad m => [String] -> Int -> String -> Int -> Int -> m [String]
smaller list tempInt tempStr size ind = do
   
        if size == 0 || (length list) < 2 then 
            return ([(show tempInt), tempStr]) 
        else do
            let i = list !!ind
            let index = length (filter (\ x -> x == (i)) list)                                                  ------FILTER

            if tempInt > index then
                smaller list index i (size - 1) (ind + 1)
            else 
                smaller list tempInt tempStr (size - 1) (ind + 1)

--------------------------------------------------------------------------------
                --Funcion para imprimir preguntas manuales
--------------------------------------------------------------------------------
printQuestion :: [Question] -> Int -> [Pool] -> Pool -> Int -> [Question] -> IO b
printQuestion param size l listOrig quantityF list_question= do

        let fstListQuest    = (head param)
        let fstQuest        = (question fstListQuest)
        let fstOpt          = (questResponse fstListQuest)
        let fstTipe         = (questType fstListQuest)
        let enter           = (respEnter fstListQuest)

        putStrLn            "\n"
        print               (fstQuest)
        print               (fstOpt)
        putStrLn            "Ingresa tu opcion:"
        response            <- getLine

        let p               = [OneQuestion fstQuest fstOpt (response : enter) fstTipe]
        let list_q          = p ++ list_question

        if size == 1 then   
            showQuestions listOrig l (quantityF - 1) list_q
        else
            printQuestion (tail param) (size -1) l listOrig quantityF list_q

--------------------------------------------------------------------------------
            --Función de respuestas automaticas
--------------------------------------------------------------------------------
responseAutomatic :: Pool -> [Pool] -> Int -> [Question] -> IO b
responseAutomatic param l quantityForms list_q  = do

        let headList = (listQues param)
        let new      = OnePool {
        poolTitle    = (poolTitle param),
        listQues     = list_q}

        if (length list_q) == 0 then 
            do
                printQuestionAutomatic headList (length headList) l param quantityForms []

        else if quantityForms == 0 then 
            do   
                putStrLn "\n ESTADISTICAS DE LA ENCUESTAS AUTOMATICAS:"
                print (poolTitle new)
                putStrLn "\n"
                printStadist list_q (length list_q) l
        else do
            printQuestionAutomatic (listQues new) (length headList) l param quantityForms []

--------------------------------------------------------------------------------
            --Funcion para imprimir preguntas automáticas
--------------------------------------------------------------------------------
printQuestionAutomatic :: [Question] -> Int -> [Pool] -> Pool -> Int -> [Question] -> IO b
printQuestionAutomatic param size l listOrig quantityF list_question= do

        let fstListQuest = (head param)
        let fstQuest     = (question fstListQuest)
        let fstOpt       = (questResponse fstListQuest)
        let fstTipe      = (questType fstListQuest)
        let enter        = (respEnter fstListQuest)

        response         <- genRandom list_question fstTipe fstOpt

        let p            = [OneQuestion fstQuest fstOpt (response : enter) fstTipe]
        let list_q       = p ++ list_question

        if size == 1 then
            responseAutomatic listOrig l (quantityF - 1) list_q
        else printQuestionAutomatic (tail param) (size -1) l listOrig quantityF list_q

--------------------------------------------------------------------------------
            --Función para generar respuestas random
--------------------------------------------------------------------------------
genRandom :: Monad m => [Question] -> Int -> [String] -> m String
genRandom responses tipe options = do

    let options1 = ["a","b","c","d","e","f"]
    let options2 = ["1","2","3","4","5"]

    if tipe == 0 then do
        if (length responses) > (length options) then
            genRandom (tail(tail responses)) tipe options
        else 
            (return (options1 !!(length responses)))

    else do 
        if(length responses) > 5 then
            genRandom (tail(tail responses)) tipe options
        else 
            (return (options2 !!(length responses)))

--------------------------------------------------------------------------------
            --Funcion para buscar lista a responder
--------------------------------------------------------------------------------
foundList :: Monad m => [Pool] -> String -> m Pool
foundList list param = do

        let index = head list
        
        if ((poolTitle index) == param) then
                (return index)
        else do
            foundList (tail list) param

--------------------------------------------------------------------------------
            --Funcion Registrando nueva encuesta
--------------------------------------------------------------------------------
registForm :: [Pool] -> IO b
registForm l  = do 

        nameForm     <- getLine
        putStrLn     "\nDigite la cantidad de preguntas que tendra la encuesta: "
        cant         <- readLn

        generateQuestion cant [] nameForm l     
                    
--------------------------------------------------------------------------------
            --Funcion para crear las preguntas
--------------------------------------------------------------------------------
generateQuestion :: (Read a, Eq a, Num a) => a -> [Question] -> String -> [Pool] -> IO b
generateQuestion index list_question n_form l = do
        
        if index == 0 then
            do  
                putStrLn       "------Encuesta------"
                putStrLn       n_form
                putStrLn       "Encuesta creada con exito!"
                let new        =  OnePool {
                poolTitle      = n_form,
                listQues       = list_question}
                let car        = pasteList new l

                mainMenuFunc car  
        else
            do
                putStrLn       "\nIngrese su pregunta: "
                nameQ          <- getLine
        
                putStrLn       "\nIngrese: \n[1]-Si la respuesta es por escala \n[0]-Si la respuesta es de selección"     
                typeResp       <- readLn           
        
                putStrLn       "\nIngrese la cantidad de respuestas que tendrá la pregunta"
                putStrLn       "(Si selecciono por escala ingrese 1): "
                cant           <- readLn

                response       <- generateResponse cant []
                let p          = [OneQuestion nameQ response [] typeResp]
                let list_q     = p ++ list_question

                generateQuestion ( index -1) list_q n_form l
                
--------------------------------------------------------------------------------
            --Funcion para crear las respuestas de las preguntas
--------------------------------------------------------------------------------
generateResponse :: (Eq t, Num t) => t -> [String] -> IO [String]
generateResponse index list_question = do
        if index == 0 then
            do
                return (list_question)
        else
            do
                putStrLn               "\nIngrese la respuesta, ejemplos:"
                putStrLn               "--> Si eligio Selección: a)Verdadero y en el siguiente input ingresa la siguiente opcion "
                putStrLn               "--> Si eligio Escala: ingrese lo que quiere evaluar del 1 al 5"
                nombre_escala          <- getLine
                let lista_con_pregunta = pasteListR nombre_escala list_question

                generateResponse (index - 1 ) lista_con_pregunta

