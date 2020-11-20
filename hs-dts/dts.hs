--------------------
-- REPRESENTATION -- 
--------------------

type AttName = String                                   -- attribute name
type AttValue = String                                  -- attribute value (details)
type Values = [AttValue]                                -- values of an attribute or a data row    
type Attribute = (AttName, Values)                      -- attribute information                         
type Attributes = [Attribute]                           -- attribute list 
type MushData = (Attributes, [Values])                  -- data set of mushrooms (contains all data)
type AttSelector = MushData -> Attribute -> Attribute   -- attribute selector
type Partition = [(AttValue, MushData)]                 -- used to partition the data (useful for building the tree)
type AttList = [(AttName, [(AttValue, String)])]        -- attribute list with the large names

-- Decision Tree
data DecisionTree = Null | Leaf AttValue | Node AttName [(AttValue, DecisionTree)] 
   deriving (Eq)

-- Fractional attribute list useful for console interaction with the resulting tree and the dialogue interaction
attributeList :: AttList
attributeList = [ ("edibility", [("p","poisonous"), ("e","edible")]), 
                  ("spore-print-color", [("k","black"), ("n","brown"), ("b","buff"), ("h","chocolate"), 
                  ("r","green"), ("o","orange"), ("u","purple"), ("w","white"), ("y","yellow")]), 
                  ("odor", [("a","almond"), ("l","anise"), ("c","creosote"), ("y","fishy"), ("f","foul"), 
                  ("m","musty"), ("n","none"), ("p","pungent"),("s","spicy")]),
                  ("cap-color", [("n","brown"), ("b","buff"), ("c","cinnamon"), ("g","gray"), ("r","green"), 
                  ("p","pink"), ("u","purple"), ("e","red"), ("w","white"), ("y","yellow")]),
                  ("habitat", [("g","grasses"), ("l","leaves"), ("m","meadows"), ("p","paths"), 
                  ("u","urban"), ("w","waste"), ("d","woods")]),
                  ("gill-size", [("b","broad"), ("n","narrow")]) ]    

-- Attributes 
attributes :: Attributes
attributes = [edibility, capShape, capSurface, capColor, bruises, odor, gillAttachment,
         gillSpacing, gillSize, gillColor, stalkShape, stalkRoot, stalkSurfaceAboveRing,
         stalkSurfaceBelowRing, stalkColorAboveRing, stalkColorBelowRing, veilType, veilColor,
         ringNumber, rintType, sporePrintColor, population, habitat]

edibility :: Attribute
edibility = ("edibility", ["p","e"])

capShape :: Attribute
capShape = ("cap-shape", ["b","c","x","f","k","s"])

capSurface :: Attribute
capSurface = ("cap-surface", ["f","g","y","s"])

capColor :: Attribute
capColor = ("cap-color", ["n","b","c","g","r","p","u","e","w","y"])

bruises :: Attribute
bruises = ("bruises", ["t", "f"])

odor :: Attribute
odor = ("odor", ["a","l","c","y","f","m","n","p","s"])

gillAttachment :: Attribute
gillAttachment = ("gill-attachment", ["a","d","f","n"])

gillSpacing :: Attribute
gillSpacing = ("gill-spacing", ["c","w","d"])

gillSize :: Attribute
gillSize = ("gill-size", ["b","n"])

gillColor :: Attribute
gillColor = ("gill-color", ["k","n","b","h","g","r","o","p","u","e","w","y"])

stalkShape :: Attribute
stalkShape = ("stalk-shape", ["e","t"])

stalkRoot :: Attribute
stalkRoot = ("stalk-root", ["b","c","u","e","z","r","?"])

stalkSurfaceAboveRing :: Attribute
stalkSurfaceAboveRing = ("stalk-surface-above-ring", ["f","y","k","s"])

stalkSurfaceBelowRing :: Attribute
stalkSurfaceBelowRing = ("stalk-surface-below-ring", ["f","y","k","s"])

stalkColorAboveRing :: Attribute
stalkColorAboveRing = ("stalk-color-above-ring", ["n","b","c","g","o","p","e","w","y"])

stalkColorBelowRing :: Attribute
stalkColorBelowRing = ("stalk-color-below-ring", ["n","b","c","g","o","p","e","w","y"])

veilType :: Attribute
veilType = ("veil-type", ["p","u"])

veilColor :: Attribute
veilColor = ("veil-color", ["n","o","w","y"])

ringNumber :: Attribute
ringNumber = ("ring-number", ["n","o","t"])

rintType :: Attribute
rintType = ("ring-type", ["c","e","f","l","n","p","s","z"])

sporePrintColor :: Attribute
sporePrintColor = ("spore-print-color", ["k","n","b","h","r","o","u","w","y"])

population :: Attribute
population = ("population", ["a","c","n","s","v","y"])

habitat :: Attribute
habitat = ("habitat", ["g","l","m","p","u","w","d"])

-----------------
----- TOOLS -----
-----------------

-- Return true if every element of a list are the same, otherwise false
allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual xs = and $ map (== head xs) (tail xs)

-- Basic lookup function with error detection
getValue :: (Eq a, Show a) => a -> [(a, b)] -> b
getValue x table
    = fromMaybe (error ("get value error!, there is no value " ++ show x))
                (lookup x table)

-- Version of lookup function, returns the first element instead of the second
getValue' :: (Eq b, Show b) => b -> [(a, b)] -> a
getValue' x table
    = fromMaybe (error ("get value error!, there is no value " ++ show x))
                (lookup' x table)

-- The lookup version to get the first element instead of the second
lookup' :: (Eq b) => b -> [(a,b)] -> Maybe a
lookup' _key [] =  Nothing
lookup' key ((x,y):xys)
    | key == y = Just x
    | otherwise = lookup' key xys  

-- Remove an element of type a from a table of (a,b) pairs
remove :: Eq a => a -> [(a,b)] -> [(a,b)]
remove x table = filter ((x /=) . fst) table

-- Function to compute p*log_(2)p
plogp :: Double -> Double
plogp x
    | x <= 1e-100 = 0.0
    | otherwise = x * log2 x 
    where
        log2 p = log p / log 2

-- Given an integer i and a list, remove the i-th element from the list
deleteNth :: Int -> [a] -> [a]
deleteNth _ [] = []
deleteNth i (x:xs)
    | i == 0 = xs
    | otherwise = x : deleteNth (i-1) xs

-- Returns the index of the first element in the given list which is equal (by '==') to 
-- the query element, or 'Nothing' if there is no such element.
elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex x = findIndex (x==)

-- Takes a predicate and a list and returns the index of the first element in the list 
-- satisfying the predicate, or 'Nothing' if there is no such element.
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p = listToMaybe . findIndices p

-- Extends findIndex, returns the indices of all elements satisfying the predicate, 
-- in ascending order.
findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices p xs =  [ i | (x,i) <- zip xs [0..], p x ]

-- Returns Nothing on an empty list or Just a where a is the first element of the list.
listToMaybe :: [a] -> Maybe a
listToMaybe = foldr (const . Just) Nothing

-- This function takes a default value and 'Maybe' value.  
-- If the 'Maybe' is 'Nothing', it returns the default values;
-- otherwise, it returns the value contained in the 'Maybe'.
fromMaybe :: a -> Maybe a -> a
fromMaybe d x = case x of {Nothing -> d; Just v  -> v}

-- Breaks up a string at the specified separator and returns a list of strings
split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

-- Given a list of strings with commas, 
-- separates each string into sub string list formed by the words that were between commas 
sublist :: [String] -> [[String]]
sublist [] = []
sublist s = map (split (',')) s

-----------------
--- FUNCTIONS ---
-----------------

-- Gets the value of a given attribute in a given values list              
getAttValue :: AttName -> Attributes -> Values -> AttValue
getAttValue att attributes values = getValue att (zipWith f attributes values)
    where 
        f a b = (fst a, b)

-- Gets the position of a given attribute in a given attribute list
getAttPos :: AttName -> Attributes -> Int
getAttPos att attributes = 
    fromMaybe (error ("get value error!, there is no attribute " ++ show att))
    (elemIndex att (map fst attributes))

-- Remove the value of a attribute from a values list
removeAtt :: AttName -> Attributes -> Values -> Values
removeAtt att attributes values = deleteNth (getAttPos att attributes) values

-- Build a list that count the number of ocurrrences of each value of a given attribute in a values list
attValuesOccCount :: Attribute -> MushData -> [(AttValue, Int)]
attValuesOccCount (attName, (attValues)) (attributes, values)
    = [(x,n) | x <- attValues, n <- [length (filter (x ==) atts)]]
    where
        atts = map (getAttValue attName attributes) values

-- Partition a mushroom data using the specified attribute
partitionData :: MushData -> Attribute -> Partition
partitionData (atts, values) (attName, attVals)
    = [(attVal, (remove attName atts, [removeAtt attName atts val | val <- values, 
        getAttValue attName atts val == attVal])) | attVal <- attVals]

-- Build a decision tree from a given mush data respect an attribute using ID3 algorithm
buildDecisionTree :: MushData -> Attribute -> AttSelector -> DecisionTree 
buildDecisionTree (_,[]) _ _ = Null
buildDecisionTree mushData@(atts, values) classAtt@(classAttName, _) attSelector
    | allEqual (map (getAttValue classAttName atts) values) = Leaf (getAttValue classAttName atts (values !! 0))                
    | otherwise = Node attName [(x, t) | x <- attValues, t <- [buildDecisionTree (getValue x part) classAtt attSelector]]
    where
        att@(attName, attValues) = attSelector mushData classAtt        -- Select next attribute (node)
        part = partitionData mushData (att)

-- Calculate the entropy of a mushroom data set with respect an attribute
entropy :: MushData -> Attribute -> Double
entropy (_, []) _ = 0
entropy mushData att@(_, values) = sum (map (negate . plogp) (map (calculate mushData att) values))

-- Calculate the probability distribution
calculate :: MushData -> Attribute -> AttValue -> Double
calculate mushData@(_, values) att value
    = (fromIntegral (getValue value (attValuesOccCount att mushData))) / (fromIntegral (length values))

-- Computes the information gain of a mushroom data set respect an attribute
gain :: MushData -> Attribute -> Attribute -> Double
gain mushData att@(_, values) classAtt = entropy mushData classAtt - sum (map f values) 
    where 
        f val = (calculate mushData att val) * entropy (getValue val (partitionData mushData att)) classAtt

-- A function for selecting the next attribute to building the tree
nextAtt :: AttSelector
nextAtt mushData@(attributes, _) classAtt@(classAttName, _) = getValue (maximum (map fst gains)) gains
    where 
        gains = [ (gain mushData att classAtt, att) | att <- (remove classAttName attributes) ]


-- Function that traverses the decision tree from dialogues and gives you a prediction 
-- of the edability of the mushroom
classification :: DecisionTree -> IO ()
classification Null = putStrLn ("Prediction: null")
classification (Leaf attValue) = putStrLn ("Prediction: " ++ if (attValue == "e") then "edible" else "poisonous")
classification (Node attName childs) = do putStrLn $ "Which " ++ attName ++ "?"
                                          input <- getLine
                                          let value = find' attName input attributeList
                                          classification (getValue value childs)
     
                                      
find :: AttName -> AttValue -> AttList -> String
find attName attValue list = getValue attValue (getValue attName list)
                                       
find' :: AttName -> String -> AttList -> AttValue
find' attName value list = getValue' value (getValue attName list)


-- ++ unlines(map(showTree(tabs+1))childs)
--showTree :: Int -> DecisionTree -> String
--showTree tabs (Null) = indent tabs ("null")
--showTree tabs (Leaf attValue) = indent tabs (show attValue)
--showTree tabs (Node attName [(value, childs)]) = (indent tabs (show attName)) ++ "\n" ++ indent (tabs+1) (show value) ++ showTree (tabs+1) childs


instance Show DecisionTree where
    show dts = show' 0 dts
        where
            show' nvl Null = indent nvl "null"
            show' nvl (Leaf attValue) = indent nvl (show attValue)
            --show' nvl (Node attName childs@([(value,_)])) = indent nvl (show attName) ++ "\n" ++ indent (nvl+1) (show value) ++ concatMap (show' (nvl+1)) (map snd childs)
            --show' nvl (Node attName childs) = indent nvl (show attName) ++ "\n" ++ indent (nvl) (show "attValue") ++ "\n" ++ concatMap (show' (nvl+1)) (map snd childs)
            show' nvl (Node attName childs) = indent nvl (show attName) ++ "\n" ++ concatMap (show' (nvl+1)) (operate childs)
            indent n = (replicate n ' '++)
 

pairToList :: (a, a) -> [a]
pairToList (x,y) = [x,y]

tuplesToList :: [(a,a)] -> [[a]]
tuplesToList = map pairToList

flatten :: [[a]] -> [a]
flatten = foldl (++) []

operate :: [(a,a)] -> [a]
operate = flatten . tuplesToList

--instance Show DecisionTree where
  --  show Null = show("null")
  --  show (Leaf attValue) = show(attValue)
  --  show (Node attName childs) = show(attName) ++ showChilds "" childs
    --    where 
       --       showChilds indent childs = concatMap (show' indent)  (operate childs)
        --      show' indent Null = indent ++ show("null")
        --      show' indent (Leaf attValue) = indent ++ show (attValue)
         --     show' indent (Node attName childs) = 
           --       "\n" ++ indent ++ show(attName) ++ "\n" ++ showChilds ("   " ++ indent) childs 



--unlines(map(showTree(tabs+1))childs)
-- ++ mapM_ putStrLn $ (map (showTree (tabs + 1)) childs)


--showTree :: Int -> DecisionTree -> String
--showTree tabs (Hoja valor) = indent tabs (show valor)
--showTree tabs (Branch name x) = (indent tabs (show name)) ++ "\n" ++ -- unlines(map(showTree(tabs+1))x) -- Esta opcion usa show, que imprime los salts como \n
--mapM_ putStrLn (map (showTree (tabs + 1)) x) --Esta usa putStrLn, que imprime sin comillas ni nada mas bonito ::: Me podrian quitar puntos por tener la salida integrada en el programa en lugar de tenerla separada

-- Para IO -> escribe tantos espacios como le indique el primer parámetro 
--indent :: Int -> String -> String
--indent d = (replicate d ' '++)


-- MAIN 
main = do
    contents <- readFile "agaricus-lepiota.data"
    let linesOfFile = lines contents
    let table = sublist linesOfFile
    putStrLn $ "-------------------------------------"
    putStrLn $ "------ RESULTING DECISION TREE ------"
    putStrLn $ "-------------------------------------\n"
    let tree = buildDecisionTree (attributes,table) edibility nextAtt
    print $ tree
    --print $ showTree 0 tree
    putStrLn $ "\n**Look at the readme to see the resulting tree with more clarity**\n"
    putStrLn $ "-------------------------------------"
    putStrLn $ "------ PREDICTION INTERACTION ------"
    putStrLn $ "-------------------------------------\n"
    classification tree