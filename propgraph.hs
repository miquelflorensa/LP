type Date = String

data Val = ValI Int | ValD Double | ValS String | ValB Bool | ValDa Date
    deriving (Eq)

type Vertex = String
type Edge = String
type Label = String
type Property = String
type VertexVal = (Vertex,Val)
type Prop = (Property,Val)

-- NameVertex, Label, List of properties, list of adjacent vertex
type Vertices = (Vertex,Label,[Prop],[(Vertex,Edge)])

-- NameEdge, Label, List of properties, vertex 1, vertex 2
-- the direction of the edge is vertex 1 -> vertex 2
type Edges = (Edge,Label,[Prop],Vertex,Vertex)
type TypeProperty = (String,String)

instance Show Val where
    show (ValI value) = show value
    show (ValD value) = show value
    show (ValS value) = show value
    show (ValB value) = show value
    show (ValDa value) = show value


data PG = PG [Vertices] [Edges] [TypeProperty]
    deriving (Show)


create :: PG
create = PG [] [] []

-------------------------- EDGES ---------------------------------
existsVertex :: [Vertices] -> Vertex -> Bool
existsVertex [] v1 = False
existsVertex ((v,l,prop,a):xs) v1 
    | v == v1 = True
    | otherwise = existsVertex xs v1

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
        v2' = (map (\x -> addVertices x v2 "" "") v) ++ [(v1,[],[],[(v2,e1)])]
        v3' = (map (\x -> addVertices x v1 v2 e1) v) ++ [(v2,[],[],[])]
        e' = e ++ [(e1,[],[],v1,v2)]
        v' = map (\y -> addVertices y v2 [] []) (map (\x -> addVertices x v1 v2 e1) v)


------------------------------------------------------------------

-------------------------- VALUES --------------------------------

createValues :: PG -> [String] -> PG
createValues pg (x:y:ys) = addValue pg x y

addValues :: PG -> [String] -> PG
addValues pg [] = pg
addValues pg (x:xs) = addValues (createValues pg (words x)) xs

addValue :: PG -> String -> String -> PG
addValue (PG v e p) prop typeProp = PG v e (p ++ [(prop,typeProp)])
------------------------------------------------------------------

------------------------ PROPERTIES ------------------------------

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
intersect (x:xs) l | elem x l = x : intersect xs l
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
searchType (PG v e []) x = "String"
searchType (PG v e ((x,y):xs)) z
    | x == z = y
    | otherwise = searchType (PG v e xs) z

typeConverter :: PG -> String -> Val
typeConverter pg x
    | ty == "Int" = ValI (read x ::Int)
    | ty == "Double" = ValD (read x ::Double)
    | ty == "String" = ValS x
    | ty == "Bool" = ValB (read x ::Bool)
    | otherwise = ValDa x
    where 
        ty = searchType pg x

createProp :: PG -> [String] -> PG
createProp (PG v e p) (x:y:z:ys)
    | existsVertex v x == True = defVProp (PG v e p) x [(y,typeConverter (PG v e p) z)] 
    | otherwise = defEProp (PG v e p) x [(y,typeConverter (PG v e p) z)]

addProps :: PG -> [String] -> PG
addProps pg [] = pg
addProps pg (x:xs) = addProps (createProp pg (words x)) xs

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

createLabels :: PG -> [String] -> PG
createLabels (PG v e p) (x:y:ys)
    |  existsVertex v x == True = defVLabel (PG v e p) x y
    | otherwise = defELabel (PG v e p) x y

addLabels :: PG -> [String] -> PG
addLabels pg [] = pg
addLabels pg (x:xs) = addLabels (createLabels pg (words x)) xs

-- Vertex Labels 
existsVLabel :: [Vertices] -> Vertex -> Label -> Bool
existsVLabel [] v1 l1 = False
existsVLabel ((v,l,prop,a):xs) v1 l1
    | v == v1 && l == l1 = True
    | otherwise = existsVLabel xs v1 l1

addVLabel :: Vertices -> Vertex -> Label -> Vertices
addVLabel (v,l,prop,a) v1 l1
    | v == v1 = (v,l1,prop,a)
    | otherwise = (v,l,prop,a)
 
defVLabel :: PG -> Vertex -> Label -> PG 
defVLabel (PG v e p) v1 l 
    | existsVLabel v v1 l == True = error "vertex has already a label"
    | existsVertex v v1 == False = PG (v ++ [(v1,l,[],[])]) e p
    | otherwise = PG v' e p
    where 
        v' = map (\x -> addVLabel x v1 l) v

-- Edges Labels
existsEdge :: [Edges] -> Edge -> Bool
existsEdge [] e1 = False
existsEdge ((e,l,prop,v1,v2):xs) e1 
    | e == e1 = True
    | otherwise = existsEdge xs e1

existsELabel :: [Edges] -> Edge -> Label -> Bool
existsELabel [] e1 l1 = False
existsELabel ((e,l,prop,v1,v2):xs) e1 l1
    | e == e1 && l == l1 = True
    | otherwise = existsELabel xs e1 l1

addELabel :: Edges -> Edge -> Label -> Edges
addELabel (e,l,prop,v1,v2) e1 l1
    | e == e1 = (e,l1,prop,v1,v2)
    | otherwise = (e,l,prop,v1,v2)
 
defELabel :: PG -> Edge -> Label -> PG
defELabel (PG v e p) e1 l 
    | existsELabel e e1 l == True = error "edge has already a label"
    | existsEdge e e1 == False = PG v (e ++ [(e1,l,[],[],[])]) p
    | otherwise = PG v e' p
    where 
        e' = map (\x -> addELabel x e1 l) e
------------------------------------------------------------------

------------------------ SHOW GRAPH ------------------------------

prop2String :: [Prop] -> String
prop2String [] = []
prop2String [(p,v)] = "("++p++","++show v++")"
prop2String ((p,v):xs) = "("++p++","++show v++"),"++prop2String xs


printEdge :: Edges -> IO()
printEdge (e,l,prop,v1,v2) = print $ x ++ y ++ z
    where 
    x = "("++v1++")"++" - "++e++"["++l++"]"++" -> "++"("++v2++"){"
    y = filter (not . (`elem` "\"\'")) (prop2String prop)
    z = "}"

printVertex :: Vertices -> IO()
printVertex (v1,l,prop,a) = print $ x ++ y ++ z
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

------------------------------ 1 ---------------------------------
sigma' :: PG -> String -> [Prop]
sigma' (PG [] [] _) _ = []
sigma' (PG v e _) s
    | existsVertex v s = getVProps v s
    | otherwise = getEProps e s
------------------------------------------------------------------

------------------------------ 2 ---------------------------------
propV :: PG -> Int -> Property -> [(Vertex,Val)]
propV _ 0 _ = []
propV (PG [] _ _) _ _ = []
propV (PG ((v,l,prop,a):xs) e p) k proper
    | existProperty proper prop = (getVertexProp (v,l,prop,a) proper)++x
    | otherwise = propV (PG xs e p) k proper
    where
        x = propV (PG xs e p) (k-1) proper
------------------------------------------------------------------

------------------------------ 3 ---------------------------------
propE :: PG -> Int -> Property -> [(Edge,Val)]
propE _ 0 _ = []
propE (PG _ [] _) _ _ = []
propE (PG v ((e,l,prop,v1,v2):xs) p) k proper
    | existProperty proper prop = (getEdgeProp (e,l,prop,v1,v2) proper)++x
    | otherwise = propE (PG v xs p) k proper
    where
        x = propE (PG v xs p) (k-1) proper
------------------------------------------------------------------

------------------------------ 4 ---------------------------------
findHopsVertices::PG->Vertices->Int->Vertex->Property->(Val->Val->Bool)->Val->[Vertex]->[(Vertex,Label,Val)]

-- Base cases, we cannot reach any further so we return []
findHopsVertices pg (vPG,l,prop,[]) _ _ _ _ _ _ = []

findHopsVertices pg (vPG,l,prop,((v2,e1):as)) 0 v p f x vl
    | existAVertex vl v2 = []
    | vPG == v && f x y = [(vPG,l,y)] ++ z
    | otherwise = z
    where
        y = getProp prop p
        z = findHopsVertices pg (vPG,l,prop,as) 0 v p f x vl 

findHopsVertices pg (vPG,l,prop,((v2,e1):as)) k v p f x vl 
    | existAVertex vl v2 = []
    | otherwise = y ++ z
    where
        y = findHopsVertices pg (findVertice pg v2) (k-1) v p f x (vl++[vPG]) 
        z = findHopsVertices pg (vPG,l,prop,as) k v p f x vl 


kHops :: PG -> Int -> Vertex -> Property -> (Val -> Val -> Bool) -> Val -> [(Vertex,Label,Val)]
kHops pg k v p f x = findHopsVertices pg (findVertice pg v) k v p f x []

getProp :: [Prop] -> Property -> Val
getProp ((x,y):xs) p 
    | x == p = y
    | otherwise = getProp xs p

findVertice :: PG -> Vertex -> Vertices
findVertice (PG ((v,l,prop,a):xs) e p) v1
    | v1 == v = (v,l,prop,a)
    | otherwise = findVertice (PG xs e p) v1  

existAVertex :: [Vertex] -> Vertex -> Bool
existAVertex [] _ = False
existAVertex (x:xs) v
    | x == v = True
    | otherwise = existAVertex xs v
------------------------------------------------------------------

------------------------------ 5 ---------------------------------

reachableAux :: PG -> Vertices -> Vertex -> Label -> [Vertex] -> Bool
reachableAux _ (v1,l1,prop,[]) _ _ _ = False
reachableAux pg (v1,l1,prop,((v2,e1):as)) v l vl
    | existAVertex vl v2 == False && v1 == v = True
    | existAVertex vl v2 == False && l == getELabel pg e1 = x || y
    | otherwise = y
    where
        x = reachableAux pg (findVertice pg v2) v l (vl ++ [v1])
        y = reachableAux pg (v1,l1,prop,as) v l vl


reachable :: PG -> Vertex -> Vertex -> Label -> Bool
reachable pg v1 v2 l = reachableAux pg (findVertice pg v1) v2 l [] 


getELabel :: PG -> Edge -> Label
getELabel (PG _ [] _) _ = []
getELabel (PG v ((e,l,prop,v1,v2):es) p) e1
    | e1 == e = l
    | otherwise = getELabel (PG v es p) e1

------------------------------------------------------------------


main = do
    putStrLn "Quin és el nom del fitxer Rho?"
    nomRho <- getLine
    putStrLn "Quin és el nom del fitxer Lambda?"
    nomLambda <- getLine
    putStrLn "Quin és el nom del fitxer Sigma?"
    nomSigma <- getLine
    putStrLn "Quin és el nom del fitxer Prop?"
    nomProp <- getLine
    rho <- readFile nomRho
    lmd <- readFile nomLambda
    sgm <- readFile nomSigma
    prop <- readFile nomProp
    let pg = populate rho lmd sgm prop   
    putStrLn "Que vols fer?"
    putStrLn "0. Mostra el Graph"
    putStrLn "1. Funció sigma prima"
    putStrLn "2. Propietats Vertex"
    action <- getLine
    showGraph pg
    --let khops = kHops pg 3 "n3" "lastName" (Val == Val) "Blanco"
    let reach = reachable pg "n1" "n2" "has"
    print reach
    return ()
    
