{--
Miquel Florensa Montilla
miquel.florensa@estudiantat.upc.edu
Grup 21 

--------------- QUERYING SMALL PROPERTY GRAPHS -------------------
--}

type Date = String

data Val = ValI Int | ValD Double | ValS String | ValB Bool | ValDa Date
    deriving (Eq)


-- Defineixo els Valors amb un Date i tots els possibles tipus de dades

type Vertex = String
type Edge = String
type Label = String
type Property = String
type VertexVal = (Vertex,Val)
type Prop = (Property,Val)

-- VERTICES
-- NameVertex, Label, List of properties, list of adjacent vertex
type Vertices = (Vertex,Label,[Prop],[(Vertex,Edge)])

-- EDGES
-- NameEdge, Label, List of properties, vertex 1, vertex 2
-- the direction of the edge is vertex 1 -> vertex 2
type Edges = (Edge,Label,[Prop],Vertex,Vertex)
type TypeProperty = (String,String)

-- Instancio cada Value amb el seu show
instance Show Val where
    show (ValI value) = show value
    show (ValD value) = show value
    show (ValS value) = show value
    show (ValB value) = show value
    show (ValDa value) = show value

-- El grapg té una llista de vertices una de edges i una de TypeProperty
data PG = PG [Vertices] [Edges] [TypeProperty]

-- La llista de TypeProperty contindrà totes les propietats i el seu corresponent tipus

create :: PG
create = PG [] [] []

-------------------------- EDGES ---------------------------------

-- Retorna True si existeix un vèrtex en una llista de vèrtex
existsVertex :: [Vertices] -> Vertex -> Bool
existsVertex [] v1 = False
existsVertex ((v,l,prop,a):xs) v1 
    | v == v1 = True
    | otherwise = existsVertex xs v1

-- Conjunt de funcions que permeten crear edges 
addVertices :: Vertices -> Vertex -> Vertex -> Edge -> Vertices
addVertices (v,l,prop,a) v1 v2 e
    | v == v1 = (v,l,prop,a ++ [(v2,e)])
    | otherwise = (v,l,prop,a)

createEdge :: PG -> [String] -> PG
createEdge pg (x:y:z:ys) = addEdge pg x y z 

addEdges :: PG -> [String] -> PG
addEdges pg [] = pg
addEdges pg (x:xs) = addEdges (createEdge pg (words x)) xs

addEdge :: PG -> Edge -> Vertex -> Vertex -> PG
addEdge (PG v e p) e1 v1 v2 
    | (existsVertex v v1 == False) && (existsVertex v v2) == False = PG v1' e' p
    | (existsVertex v v1) == False && (existsVertex v v2) == True = PG v2' e' p
    | (existsVertex v v1) == True && (existsVertex v v2) == False = PG v3' e' p
    | otherwise = PG v' e' p
    where 
        v1' = v ++ [(v1,[],[],[(v2,e1)])] ++ [(v2,[],[],[])]
        v2' = (map (\x -> addVertices x v2 [] []) v) ++ [(v1,[],[],[(v2,e1)])]
        v3' = (map (\x -> addVertices x v1 v2 e1) v) ++ [(v2,[],[],[])]
        e' = e ++ [(e1,[],[],v1,v2)]
        v' = map (\y -> addVertices y v2 [] []) (map (\x -> addVertices x v1 v2 e1) v)

------------------------------------------------------------------
-------------------------- VALUES --------------------------------

{-- Conjunt de funcions que permeten omplir la tercera llista del graf
    és a dir, aquella que conté els noms de les propietats i el seu tipus --}
createValues :: PG -> [String] -> PG
createValues pg (x:y:ys) = addValue pg x y

addValues :: PG -> [String] -> PG
addValues pg [] = pg
addValues pg (x:xs) = addValues (createValues pg (words x)) xs

addValue :: PG -> String -> String -> PG
addValue (PG v e p) prop typeProp = PG v e (p ++ [(prop,typeProp)])

------------------------------------------------------------------
------------------------ PROPERTIES ------------------------------

{-- Conjunt de funcions que permeten afegir propietats, modificar-les,
    treure'n informació etc.. --}
getVProps :: [Vertices] -> Vertex -> [Prop]
getVProps [] _ = []
getVProps ((v,l,prop,a):xs) v1
    | v == v1 = prop
    | otherwise = getVProps xs v1

getEProps :: [Edges] -> Edge -> [Prop]
getEProps [] _ = []
getEProps ((e,l,prop,v1,v2):xs) e1
    | e == e1 = prop
    | otherwise = getEProps xs e1


getVertexProp :: Vertices -> Property -> [(Vertex,Val)]
getVertexProp (v,l,[],a) _ = []
getVertexProp (v,l,((prop,value):xs),a) p
    | prop == p = [(v,value)]
    | otherwise = getVertexProp (v,l,xs,a) p

getEdgeProp :: Edges -> Property -> [(Edge,Val)]
getEdgeProp (e,l,[],v1,v2) _ = []
getEdgeProp (e,l,((prop,value):xs),v1,v2) p
    | prop == p = [(e,value)]
    | otherwise = getEdgeProp (e,l,xs,v1,v2) p

intersect :: [Prop] -> [Prop] -> [Prop]
intersect [] _ = []
intersect (x:xs) l 
    | elem x l = x : intersect xs l
    | otherwise = intersect xs l

existsProp :: [Prop] -> [Prop] -> Bool
existsProp xs ys 
    | x == [] = False
    | otherwise = True
    where
        x = intersect xs ys

existProperty :: Property -> [Prop] -> Bool
existProperty _ [] = False
existProperty p ((x,y):xs) 
    | p == x = True
    | otherwise = existProperty p xs

searchType :: PG -> String -> String
searchType (PG v e []) x = ""
searchType (PG v e ((x,y):xs)) z
    | x == z = y
    | otherwise = searchType (PG v e xs) z

typeConverter :: PG -> String -> String -> Val
typeConverter pg y x
    | ty == "Int" = ValI (read x ::Int)
    | ty == "Double" = ValD (read x ::Double)
    | ty == "String" = ValS x
    | ty == "Bool" = ValB (read x ::Bool)
    | ty == "Date" = ValDa x
    | otherwise = ValS "_|_"
    where 
        ty = searchType pg y

createProp :: PG -> [String] -> PG
createProp (PG v e p) (x:y:z:ys)
    | existsVertex v x = defVProp (PG v e p) x [(y,typeConverter (PG v e p) y z)] 
    | otherwise = defEProp (PG v e p) x [(y,typeConverter (PG v e p) y z)]

addProps :: PG -> [String] -> PG
addProps pg [] = pg
addProps pg (x:xs) = addProps (createProp pg (words x)) xs

{-- Si un vèrtex ja té una propietat no se li pot sobrescriure,
    en cas que s'intenti fer, retorno un erro amb el missatge,
    he pensat que així podia quedar bé, i ho he repetit en altres funcions.
--}
addVProp :: Vertices -> Vertex -> [Prop] -> Vertices
addVProp (v,l,prop,a) v1 x 
    | v == v1 && existsProp prop x = error "this vertex has already the property"
    | v == v1 && existsProp prop x == False = (v,l,(prop ++ x),a)
    | otherwise = (v,l,prop,a)

defVProp :: PG -> Vertex -> [Prop] -> PG
defVProp (PG v e p) v1 prop = PG v' e p where
    v' = map (\x -> addVProp x v1 prop) v

addEProp :: Edges -> Edge -> [Prop] -> Edges
addEProp (e,l,prop,v1,v2) e1 x 
    | e == e1 && existsProp prop x = error "this edge has already the property"
    | e == e1 && existsProp prop x == False = (e,l,(prop ++ x),v1,v2)
    | otherwise = (e,l,prop,v1,v2)

defEProp :: PG -> Edge -> [Prop] -> PG
defEProp (PG v e p) e1 prop = PG v e' p where
    e' = map (\x -> addEProp x e1 prop) e

------------------------------------------------------------------
-------------------------- LABELS --------------------------------

-- Funcions per al populate
createLabels :: PG -> [String] -> PG
createLabels (PG v e p) (x:y:ys)
    |  existsVertex v x == True = defVLabel (PG v e p) x y
    | otherwise = defELabel (PG v e p) x y

addLabels :: PG -> [String] -> PG
addLabels pg [] = pg
addLabels pg (x:xs) = addLabels (createLabels pg (words x)) xs

-- Vertex Labels
-- Funcions que permeten definir les etiquetes en vèrtex 
existsVLabel :: [Vertices] -> Vertex -> Bool
existsVLabel [] v1 = False
existsVLabel ((v,l,prop,a):xs) v1
    | v == v1 && l /= [] = True
    | otherwise = existsVLabel xs v1

addVLabel :: Vertices -> Vertex -> Label -> Vertices
addVLabel (v,l,prop,a) v1 l1
    | v == v1 = (v,l1,prop,a)
    | otherwise = (v,l,prop,a)
 
defVLabel :: PG -> Vertex -> Label -> PG 
defVLabel (PG v e p) v1 l 
    | existsVLabel v v1 = error "vertex has already a label"
    | existsVertex v v1 == False = PG (v ++ [(v1,l,[],[])]) e p
    | otherwise = PG v' e p
    where 
        v' = map (\x -> addVLabel x v1 l) v

-- Edges Labels
-- Funcions que permeten definir les etiquetes en arestes

existsEdge :: [Edges] -> Edge -> Bool
existsEdge [] e1 = False
existsEdge ((e,l,prop,v1,v2):xs) e1 
    | e == e1 = True
    | otherwise = existsEdge xs e1

existsELabel :: [Edges] -> Edge -> Bool
existsELabel [] e1 = False
existsELabel ((e,l,prop,v1,v2):xs) e1
    | e == e1 && l /= [] = True
    | otherwise = existsELabel xs e1 

addELabel :: Edges -> Edge -> Label -> Edges
addELabel (e,l,prop,v1,v2) e1 l1
    | e == e1 = (e,l1,prop,v1,v2)
    | otherwise = (e,l,prop,v1,v2)
 
defELabel :: PG -> Edge -> Label -> PG
defELabel (PG v e p) e1 l 
    | existsELabel e e1 = error "edge has already a label"
    | existsEdge e e1 == False = PG v (e ++ [(e1,l,[],[],[])]) p
    | otherwise = PG v e' p
    where 
        e' = map (\x -> addELabel x e1 l) e

------------------------------------------------------------------
------------------------ SHOW GRAPH ------------------------------

{-- 
Per poder printar el graf, el que faig és anar iterant sobre tots
els seus vèrtex i arestes i anar printant-los amb el seu corresponent
format. Converteixo tots els elments (incluides les propietats) a Strings
per tal de poder printar-ho tot junt. A més elimino cometes que es posen
en les strings.
--}

prop2String :: [Prop] -> String
prop2String [] = []
prop2String [(p,v)] = "("++p++","++show v++")"
prop2String ((p,v):xs) = "("++p++","++show v++"),"++prop2String xs


printEdge :: Edges -> IO()
printEdge (e,l,prop,v1,v2) = putStrLn $ x ++ y ++ z
    where 
    x = "("++v1++")"++" - "++e++"["++l++"]"++" -> "++"("++v2++"){"
    y = filter (not . (`elem` "\"\'")) (prop2String prop)
    z = "}"

printVertex :: Vertices -> IO()
printVertex (v1,l,prop,a) = putStrLn $ x ++ y ++ z
    where 
    x = v1++"["++l++"]"++"{" 
    y = filter (not . (`elem` "\"\'")) (prop2String prop)
    z = "}"

showGraph :: PG -> IO()
showGraph (PG [] [] p) = return ()
showGraph (PG [] (e:es) p) = printEdge e >> showGraph (PG [] es p)
showGraph (PG (v:vs) e p) = do printVertex v >> showGraph (PG vs e p)
------------------------------------------------------------------

------------------------- POPULATE -------------------------------

populate :: String -> String -> String -> String -> PG
populate rho lmd sgm prop = pg
    where 
    pg = addLabels props (lines lmd)
    props = addProps values (lines sgm) 
    values = addValues edg (lines prop)
    edg = addEdges (create) (lines rho)

------------------------------------------------------------------

-------------- QUERING AGAINST PROPERTY GRAPHS -------------------

-------------------------------- σ' ------------------------------

-- Retorna totes les propietats d'un vèrtex o aresta
sigma' :: PG -> String -> [Prop]
sigma' (PG [] [] _) _ = []
sigma' (PG v e _) s
    | existsVertex v s = getVProps v s
    | otherwise = getEProps e s

------------------------------------------------------------------
------------------------------ propV -----------------------------

-- Retorna els k primers vèrtex que contenen la propietat p
propV :: PG -> Int -> Property -> [(Vertex,Val)]
propV _ 0 _ = []
propV (PG [] _ _) _ _ = []
propV (PG ((v,l,prop,a):xs) e p) k proper
    | existProperty proper prop = (getVertexProp (v,l,prop,a) proper)++x
    | otherwise = propV (PG xs e p) k proper
    where
        x = propV (PG xs e p) (k-1) proper

------------------------------------------------------------------
------------------------------ propE -----------------------------

-- Retorna les k primeres arestes que contenen la propietat p
propE :: PG -> Int -> Property -> [(Edge,Val)]
propE _ 0 _ = []
propE (PG _ [] _) _ _ = []
propE (PG v ((e,l,prop,v1,v2):xs) p) k proper
    | existProperty proper prop = (getEdgeProp (e,l,prop,v1,v2) proper)++x
    | otherwise = propE (PG v xs p) k proper
    where
        x = propE (PG v xs p) (k-1) proper

------------------------------------------------------------------
------------------------------ KHOPS -----------------------------

{-- Itera sobre tots els veins del vèrtex recursivament i aquells en els que arriba
    en k passos i cumpleixen la funció f són afegits a la llista. --}
findHopsVertices::PG->Vertices->Int->Vertex->Property->(Val->Val->Bool)->Val->[Vertex]->[(Vertex,Label,Val)]
findHopsVertices pg (vPG,l,prop,[]) _ _ _ _ _ _ = []

findHopsVertices pg (vPG,l,prop,((v2,e1):as)) 0 v p f x vl
    | propNotExist prop p = []
    | f x y = [(vPG,l,y)] 
    | otherwise = []
    where
        y = getProp prop p 
        
findHopsVertices pg (vPG,l,prop,((v2,e1):as)) k v p f x vl 
    | existAVertex vl v2 = []
    | otherwise = y ++ z
    where
        z = findHopsVertices pg (vPG,l,prop,as) k v p f x vl
        y = findHopsVertices pg (findVertice pg v2) (k-1) v p f x (vl++[vPG])

{-- Retorna una llista amb vèrtex, etiqueta i Value de tots aquells vèrtex que 
    cumpleixin amb la funció f i que es s'arribi a ells amb k passos desde el 
    vèrtex v. --}
kHops :: PG -> Int -> Vertex -> Property -> (Val -> Val -> Bool) -> Val -> [(Vertex,Label,Val)]
kHops pg k v p f x = findHopsVertices pg (findVertice pg v) k v p f x []

-- Retorna True si no existeix cap propietat p en la llista de Propietat x Value
propNotExist :: [Prop] -> Property -> Bool
propNotExist [] _ = True
propNotExist ((x,y):xs) p 
    | x == p = False
    | otherwise = propNotExist xs p

-- Retorna el Value d'una propietat
getProp :: [Prop] -> Property -> Val 
getProp ((x,y):xs) p 
    | x == p = y
    | otherwise = getProp xs p

-- Donat un graf, retorna un Vertice (estructura del graf) amb el v = vèrtex
findVertice :: PG -> Vertex -> Vertices
findVertice (PG [] e p) _ = ("","",[],[])
findVertice (PG ((v,l,prop,a):xs) e p) v1
    | v1 == v = (v,l,prop,a)
    | otherwise = findVertice (PG xs e p) v1  

-- Retorna True si existeix un vertex en una llista de vèrtex 
existAVertex :: [Vertex] -> Vertex -> Bool
existAVertex [] _ = False
existAVertex (x:xs) v
    | x == v = True
    | otherwise = existAVertex xs v

------------------------------------------------------------------
---------------------------- REACHABLE ---------------------------

{-- Funció auxiliar que afegeix una llista dels vèrtex visitats.
    La funció va iterant sobre tots el vètrex adjacents a v1,
    i si en troba un amb el v2 i amb un camí amn la label retorna
    cert. --}
reachableAux :: PG -> Vertices -> Vertex -> Label -> [Vertex] -> Bool
reachableAux _ (v1,l1,prop,[]) _ _ _ = False
reachableAux pg (v1,l1,prop,((v2,e1):as)) v l vl
    | existAVertex vl v2 == False && v1 == v = True
    | existAVertex vl v2 == False && l == getELabel pg e1 = x || y
    | otherwise = y
    where
        x = reachableAux pg (findVertice pg v2) v l (vl ++ [v1])
        y = reachableAux pg (v1,l1,prop,as) v l vl

{-- Retorna Ture si existeix un camí de un vèrtex a un altre on totes les
    arestes tenen la etiqueta Label --}
reachable :: PG -> Vertex -> Vertex -> Label -> Bool
reachable pg v1 v2 l = reachableAux pg (findVertice pg v1) v2 l [] 

-- Retorna la etiqueta de una aresta
getELabel :: PG -> Edge -> Label
getELabel (PG _ [] _) _ = []
getELabel (PG v ((e,l,prop,v1,v2):es) p) e1
    | e1 == e = l
    | otherwise = getELabel (PG v es p) e1

------------------------------------------------------------------
------------------------------- MAIN -----------------------------

-- Funció que printa una llista de String Value
printStringVal :: [(String,Val)] -> IO()
printStringVal [] = return()
printStringVal (x:xs) = do 
    putStrLn $ show x 
    printStringVal xs

-- Funció que printa una llista de String String Value
printStringStringVal :: [(String,String,Val)] -> IO()
printStringStringVal [] = return()
printStringStringVal (x:xs) = do 
    putStrLn $ show x 
    printStringStringVal xs

-- Funció que pregunta els paràmetres de les funcions i imprimeix els resultats
doAction :: PG -> String -> IO()
doAction _ "0" = return ()

doAction pg "1" = do
    putStrLn "Escriu el vèrtex o la aresta de la qual vols saber les propietats:"
    vertexEdge <- getLine    
    let sigmaPrima = sigma' pg vertexEdge
    showGraph pg
    putStrLn " "
    putStrLn ("Les propietats del element "++vertexEdge++" son:")
    printStringVal sigmaPrima
    putStrLn " "
    loop pg

doAction pg "2" = do
    putStrLn "Escriu quants elements vols:"
    nat <- getLine
    let k = read nat :: Int
    putStrLn "Escriu la propietat:"
    prop <- getLine
    showGraph pg
    putStrLn " "
    putStrLn ("Els vertex que tenen la propietat "++prop++" son:")
    let proV = propV pg k prop
    printStringVal proV
    putStrLn " "
    loop pg

doAction pg "3" = do
    putStrLn "Escriu quants elements vols:"
    nat <- getLine
    let k = read nat :: Int
    putStrLn "Escriu la propietat:"
    prop <- getLine
    showGraph pg
    putStrLn " "
    putStrLn ("Els edges que tenen la propietat "++prop++" son:")
    let proE = propE pg k prop
    printStringVal proE
    putStrLn " "
    loop pg

doAction pg "4" = do
    putStrLn "Escriu quants salts vols:"
    nat <- getLine
    let k = read nat :: Int
    putStrLn "Escriu el vèrtex on vols arribar:"
    vertex <- getLine
    putStrLn "Escriu la propietat:"
    propietat <- getLine
    putStrLn "Suposem que la funció és d'igualtat (==)"
    putStrLn "ja que no es pot llegir una funció per consola"
    putStrLn "si es vol canviar d'ha de modificar el directament la linia 452 del codi"
    putStrLn " "
    putStrLn "Escriu el value que s'ha de complir:"
    value <- getLine
    let val = typeConverter pg propietat value
    showGraph pg
    putStrLn " "
    putStrLn ("Els vertex als que arriba amb "++nat++" salts son:")
    let khops = kHops pg k vertex propietat (==) val
    printStringStringVal khops
    putStrLn " "
    loop pg

doAction pg "5" = do
    putStrLn "Escriu desde quin vèrtex vols sortir:"
    v1 <- getLine
    putStrLn "Escriu el vèrtex on vols arribar:"
    v2 <- getLine
    putStrLn "Escriu la etiqueta qu han de tenir els camins:"
    label <- getLine
    showGraph pg
    putStrLn " "
    let reach = reachable pg v1 v2 label
    print reach 
    if (reach) then putStrLn ("SI es pot arribar a "++v2++" desde "++v1)
        else putStrLn ("NO es pot arribar a "++v2++" desde "++v1)
    putStrLn " "
    loop pg


doAction pg "6" = do
    putStrLn "Escriu el nom de l'aresta:"
    e <- getLine
    putStrLn "Escriu el nom del vèrtex del que surt l'aresta:"
    v1 <- getLine
    putStrLn "Escriu el nom del vèrtex on arriba l'aresta:"
    v2 <- getLine
    let addE = addEdge pg e v1 v2
    showGraph addE
    putStrLn " "
    loop addE

doAction pg "7" = do
    putStrLn "Escriu el nom del vèrtex:"
    v <- getLine
    putStrLn "Escriu el nom de la propietat:"
    prop <- getLine
    putStrLn "Escriu el valor de la propietat:"
    value <- getLine
    let defV = defVProp pg v [(prop,typeConverter pg prop value)]
    showGraph defV
    putStrLn " "
    loop defV

doAction pg "8" = do
    putStrLn "Escriu el nom de la aresta:"
    v <- getLine
    putStrLn "Escriu el nom de la propietat:"
    prop <- getLine
    putStrLn "Escriu el valor de la propietat:"
    value <- getLine
    let defE = defEProp pg v [(prop,typeConverter pg prop value)]
    showGraph defE
    putStrLn " "
    loop defE

-- defVlabel
doAction pg "9" = do
    putStrLn "Escriu el nom del vèrtex:"
    v <- getLine
    putStrLn "Escriu el nom de la etiqueta:"
    l <- getLine
    let defVl = defVLabel pg v l
    showGraph defVl
    putStrLn " "
    loop defVl

-- defElabel
doAction pg "10" = do
    putStrLn "Escriu el nom de la aresta:"
    v <- getLine
    putStrLn "Escriu el nom de la etiqueta:"
    l <- getLine
    let defEl = defELabel pg v l
    showGraph defEl
    putStrLn " "
    loop defEl

doAction pg _ = loop pg

-- loop principal del programa, on es poden executar totes les funcions del programa
loop :: PG -> IO()
loop pg = do
    putStrLn "Que vols fer?"
    putStrLn "1. σ'"
    putStrLn "2. propV"
    putStrLn "3. propE"
    putStrLn "4. kHops"
    putStrLn "5. reachable"
    putStrLn " "
    putStrLn "6. addEdge"
    putStrLn "7. defVProp"
    putStrLn "8. defEProp"
    putStrLn "9. defVlabel"
    putStrLn "10.defElabel"    
    putStrLn " "
    putStrLn "0. Sortir del programa"
    putStrLn " "
    action <- getLine
    doAction pg action
    return ()

-- main on es llegeixen els fitxers i es crida al script loop
main = do
    putStrLn "Quin és el nom del fitxer Rho?"
    nomRho <- getLine
    putStrLn "Quin és el nom del fitxer Lambda?"
    nomLambda <- getLine
    putStrLn "Quin és el nom del fitxer Sigma?"
    nomSigma <- getLine
    putStrLn "Quin és el nom del fitxer Prop?"
    nomProp <- getLine
    putStrLn " "
    rho <- readFile nomRho
    lmd <- readFile nomLambda
    sgm <- readFile nomSigma
    prop <- readFile nomProp
    let pg = populate rho lmd sgm prop  
    loop pg
    return ()
