instance Show (DecisionTree) where
    show Null = "null"
    show (Leaf attName) = indent tabs (show attName)
    show (Node attName childs) = showNode attName ++ showChilds "" childs
        where showNode attName = show attName
              --showChilds indent childs = concatMap (show' indent) (map show [0..] `zip` childs)
              showChilds indent childs = show' indent childs
              show' _ [(_,Null)] = "null"
              show' indent [(childIndex, (Node attName childs))] =
                  "\n" ++ indent ++ "<" ++ childIndex ++ ">" ++ showNode attName ++ 
                  showChilds ("    " ++ indent) childs



                  

instance Show (DecisionTree) where
    show Null = "null"
    show (Leaf attValue) = show attValue
    show (Node attName childs) = show attName ++ showChilds "" childs
        where showChilds indent childs = concatMap (show' indent) childs
              show' _ Null = "hull"
              show' indent [(attValue,(Node attName childs))] = 
                  "\n" ++ indent ++ "<" ++ attValue ++ ">" ++ show attName ++
                  showChilds ("    " ++ indent) childs

instance Show (DecisionTree) where
    show (Null) = "null"
    show (Leaf attValue) = show attValue
    show (Node attName [(value, childs)]) = showNode attName value ++ showChilds "" childs
        where showNode attName value = show attName ++ "\n" ++ " " ++ show value
              showChilds indent childs = show' indent childs
              show' indent (Null) = "\n" ++ indent ++ "null"
              show' indent (Leaf attValue) = "\n" ++ indent ++ show attValue
              show' indent (Node attName [(value, childs)]) = 
                  "\n" ++ indent ++ showNode attName value ++ showChilds indent childs



instance Show (DecisionTree) where
    -- show dts = show' 0 dts
    show Null = "null"
    show (Leaf attValue) = (show attValue)
    show (Node attName childs@[(value,_)]) = (show attName) ++ "\n" ++ " " ++ (show value) ++ show' 1 childs
        where
            show' nvl (Null) = indent nvl ("null")
            show' nvl (Leaf attValue) = indent nvl (show attValue)
            show' nvl (Node attName [(value,childs)]) = (indent nvl (show attName)) ++ "\n" ++ indent (nvl+1) (show value) ++ show' (nvl+1) childs
            indent d = (replicate d ' '++)
              

instance Show (DecisionTree) where
    show dts = show' 0 dts
        where
            show' nvl Null = indent nvl "null"
            show' nvl (Leaf attValue) = indent nvl (show attValue)
            show' nvl (Node attName childs@[(value,_)]) = indent nvl (show attName) ++ "\n" ++ indent (nvl+1) (show value) ++ showChilds
            showChilds nvl childs = concatMap (show' nvl) (map )
            
            show' (nvl+1) (getValue value childs) 
            indent d = (replicate d ' '++)





instance Show (DecisionTree) where
    show Null = "null"
    show (Leaf attValue) = show attValue
    show (Node attName [(value, childs)]) = showNode 0 attName value ++ showChilds 1 childs
        where 
            showNode nvl attName value = indent nvl (show attName) ++ "\n" ++ indent (nvl+1) (show value)
            showChilds nvl childs = concatMap (show' nvl) childs
            show' nvl Null = indent nvl "null"
            show' nvl (Leaf attValue) = indent nvl (show attValue)
            show' nvl (Node attName [(value, childs)]) = showNode nvl attName value ++ showChilds ("    " ++ nvl) childs   
            indent d = (replicate d ' '++)