type Date = String

data Val = ValI Int | ValD Double | ValS String | ValB Bool | ValDa Date
    deriving (Eq)

type Vertex = String
type Edge = String
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
        v1' = v ++ [(v1,[],[],[(v2,e1)])] ++ [(v2,[],[],[],[])]
        v2' = (map (\x -> addVertices x v2 "" "") v) ++ [(v1,[],[],[(v2,e1)])]
        v3' = (map (\x -> addVertices x v1 v2 e1) v) ++ [(v2,[],[],[],[])]
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

------------------------ PROPERTIES -----------------------------

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
createProp pg (x:y:z:ys) = defVProp pg x [(y,typeConverter pg z)] 

addProps :: PG -> [String] -> PG
addProps pg [] = pg
addProps pg (x:xs) = addProps (createProp pg (words x)) xs

addProp :: Vertices -> Vertex -> [Prop] -> Vertices
addProp (v,l,prop,a) v1 x 
    | v == v1 = (v,l,(prop ++ x),a)
    | otherwise = (v,l,prop,a)

defVProp :: PG -> Vertex -> [Prop] -> PG
defVProp (PG v e p) v1 prop = PG v' e p 
    where
        v' = map (\x -> addProp x v1 prop) v



------------------------------------------------------------------
  





populate :: String -> String -> String -> String -> PG
populate rho lmd sgm prop = pg
    where 
    pg = addProps values (lines sgm) 
    values = addValues edg (lines prop)
    edg = addEdges (create) (lines rho)






main = do
    --putstrl Demana el nom dels fixers a introduir. A FER
    --nomRho <- getline
    rho <- readFile "rhoFile.pg"
    lmd <- readFile "lambdaFile.pg"
    sgm <- readFile "sigmaFile.pg"
    prop <- readFile "propFile.pg"
    let pg = populate rho lmd sgm prop
    print pg
    return ()
    


