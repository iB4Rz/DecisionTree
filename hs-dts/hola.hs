import Data.Maybe
import Data.List

-- REPRESENTATION
type AttName = String
type AttValue = String
type Attribute = (AttName, [AttValue])
type Header = [Attribute]
type Row = [AttValue]
type DataSet = (Header, [Row])

type Partition = [(AttValue, DataSet)]
type AttSelector = DataSet -> Attribute -> Attribute

-- Decision Tree
data DecisionTree = Null | Leaf AttValue | Node AttName [(AttValue, DecisionTree)] 
    deriving (Eq, Show)
    
-- Fishing Data
fishingData :: DataSet
fishingData = (header, table)

-- Header attributes
header :: Header
header = [edibility, capShape, capSurface, capColor, bruises, odor, gillAttachment,
         gillSpacing, gillSize, gillColor, stalkShape, stalkRoot, stalkSurfaceAboveRing,
         stalkSurfaceBelowRing, stalkColorAboveRing, stalkColorBelowRing, veilType, veilColor,
         ringNumber, rintType, sporePrintColor, population, habitat]

-- Table with attributes values
--table :: [Row]
table 
  = [["sunny",    "hot",  "high",   "calm",  "bad" ],
     ["sunny",    "hot",  "high",   "windy", "bad" ],
     ["overcast", "hot",  "high",   "calm",  "good"],
     ["rainy",    "mild", "high",   "calm",  "good"],
     ["rainy",    "cool", "normal", "calm",  "good"],
     ["rainy",    "cool", "normal", "windy", "bad" ],
     ["overcast", "cool", "normal", "windy", "good"],
     ["sunny",    "mild", "high",   "calm",  "bad" ],
     ["sunny",    "cool", "normal", "calm",  "good"],
     ["rainy",    "mild", "normal", "calm",  "good"],
     ["sunny",    "mild", "normal", "windy", "good"],
     ["overcast", "mild", "high",   "windy", "good"],
     ["overcast", "hot",  "normal", "calm",  "good"],
     ["rainy",    "mild", "high",   "windy", "bad" ]]

-- ATTRIBUTES
edibility :: Attribute
edibility = ("edibility", ["p","e"])

capShape :: Attribute
capShape = ("cap-shape", ["b","c","x","f","k","s"])

capSurface :: Attribute
capSurface = ("cap-surface", ["f","g","y","s"])

capColor :: Attribute
capColor = ("cap-color", ["n","b","c","g","r","p","u","e","w","y"])

bruises :: Attribute
bruises = ("bruises?", ["t", "f"])

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


xlogx :: Double -> Double
xlogx p
  | p <= 1e-100 = 0.0
  | otherwise   = p * log2 p 
  where
    log2 x = log x / log 2

lookUp :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp x table
  = fromMaybe (error ("lookUp error - no binding for " ++ show x ++ 
                      " in table: " ++ show table))
              (lookup x table)


-- If every item in a given list is the same
allSame :: Eq a => [a] -> Bool
allSame as
  | (a:as') <- as = and ( zipWith (==) (a : as') (as' ++ [a]) )
  | otherwise     = True

-- Remove a item of type a from a table of (a,b) pairs
remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove a table 
    = filter ( (a /=) . fst ) table

-- Look up the value of a given attribute in a given data row
lookUpAtt :: AttName -> Header -> Row -> AttValue
lookUpAtt att header row
    = lookUp att (zipWith f header row)
    where 
        f a b = (fst a, b)

-- Remove attribute
removeAtt :: AttName -> Header -> Row -> Row
removeAtt att header row 
    = delete (lookUpAtt att header row) row

-- Counts the number of ocurrences of each value of a given attribute in a given data set.
buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
buildFrequencyTable (attName, (attValues)) (header, rows)
    = [ (a, n) | a <- attValues, n <- [length (filter (a ==) atts)] ]
    where 
        atts = map (lookUpAtt attName header) rows

-- Evaluate a given tree using the attribute values in a given data row
evalTree :: DecisionTree -> Header -> Row -> AttValue
evalTree Null _ _
  = ""
evalTree (Leaf attVal) _ _
  = attVal 
evalTree (Node attName paths) header row
  = evalTree (lookUp (lookUpAtt attName header row) paths) header row

----------------------------
partitionData :: DataSet -> Attribute -> Partition
partitionData (h, rs) (a, attVals)
  = [ (attVal, (remove a h,  [ removeAtt a h r | r <- rs, lookUpAtt a h r == attVal ]  )) | attVal <- attVals ]

buildTree :: DataSet -> Attribute -> AttSelector -> DecisionTree 
buildTree (_,[]) _ _
  = Null
buildTree dataSet@(h,rs) classifierAtt@(classifierName, _) attSelector
  | allSame (map (lookUpAtt classifierName h) rs) = Leaf (lookUpAtt classifierName h (rs !! 0))
  | otherwise = Node attName [ (a, t) | a <- attValues, t <- [buildTree (lookUp a partition) classifierAtt attSelector] ]
  where 
    att@(attName, attValues) = attSelector dataSet classifierAtt
    partition = partitionData dataSet (att)

-----------------------------
--------------------
entropy :: DataSet -> Attribute -> Double
entropy (_, []) _ 
  = 0
entropy dataSet@(h, rs) att@(_, vals)
  = sum (map (negate . xlogx) (map (px dataSet att) vals))

px :: DataSet -> Attribute -> AttValue -> Double
px dataSet@(_, rs) att x 
  = (fromIntegral (lookUp x (buildFrequencyTable att dataSet))) / (fromIntegral (length rs))

gain :: DataSet -> Attribute -> Attribute -> Double
gain dataSet@(h, rs) att@(_, vals) classifierAtt
  = entropy dataSet classifierAtt - sum (map f vals) 
  where 
    f val = (px dataSet att val) * entropy (lookUp val (partitionData dataSet att)) classifierAtt

bestGainAtt :: AttSelector
bestGainAtt dataSet@(header, _) classifierAtt@(classifierName, _)
  = lookUp (maximum (map fst gains)) gains
  where 
    gains = [ (gain dataSet att classifierAtt, att) | att <- (remove classifierName header) ]


---------------

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

sublist :: [String] -> [[String]]
sublist [] = []
sublist s = map (split (',')) s


main = do
    contents <- readFile "agaricus-lepiota.data"
    let linesOfFile = lines contents
    let cont = sublist linesOfFile
    print $ buildTree (header,cont) edibility bestGainAtt
   





