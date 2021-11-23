type Date = String

data Val = ValI Int | ValD Double | ValS String | ValB Bool | ValDa Date
    deriving (Eq)

type Vertex = String
type Edge = String
type Label = String
type Prop = (String,Val)

-- NameVertex, Label, List of properties, list of adjacent vertex
type Vertices = (Vertex,String,[Prop],[(Vertex,Edge)])

-- NameEdge, Label, List of properties, vertex 1, vertex 2
-- the direction of the edge is vertex 1 -> vertex 2
type Edges = (Edge,String,[Prop],Vertex,Vertex)
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
    | v == v1 = (v,l,(prop ++ x),a)
    | otherwise = (v,l,prop,a)

defVProp :: PG -> Vertex -> [Prop] -> PG
defVProp (PG v e p) v1 prop = PG v' e p where
    v' = map (\x -> addVProp x v1 prop) v

addEProp :: Edges -> Edge -> [Prop] -> Edges
addEProp (e,l,prop,v1,v2) e1 x 
    | e == e1 = (e,l,(prop ++ x),v1,v2)
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

-------------- QUERING AGAINS PROPERTY GRAPHS --------------------


sigma' ::





main = do
    --putstrl Demana el nom dels fixers a introduir. A FER
    --nomRho <- getline
    rho <- readFile "rhoFile.pg"
    lmd <- readFile "lambdaFile.pg"
    sgm <- readFile "sigmaFile.pg"
    prop <- readFile "propFile.pg"
    let pg = populate rho lmd sgm prop
    showGraph pg
    return ()
    
