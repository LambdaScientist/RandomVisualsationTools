import Data.List
import Prelude -- hiding (writeFile, readFile, putStrLn)
import System.IO.Unsafe
import System.FilePath (takeExtension) 
import System.Directory.Tree (AnchoredDirTree(..), DirTree(..),filterDir, readDirectoryWith, build, zipPaths, flattenDir)
import Control.Monad
import Text.Regex
import Text.PrettyPrint


type FileExtension = String 

data LanguageName = Haskell | VHDL
data LanguageDetails = Lang { lname :: LanguageName 
                            , extNames :: [FileExtension] 
                            , aproveValues :: [Regex] 
                            , nameRegex :: Regex 
                            , removeRegex :: [Regex]
                            }

listFiles :: FilePath -> LanguageDetails -> IO (DirTree (FilePath, FilePath))
listFiles fileStart (Lang ln exts _ _ _) = do aTree <- readDirectoryWith return fileStart
                                              let contents = zipPaths aTree
                                              let extOnly = filterDir extOrDir contents
                                              return extOnly
    where
        extOrDir :: DirTree a -> Bool
        extOrDir (Dir ('.':_) _) = False
        extOrDir (File n _) = or [takeExtension n == ext | ext <- exts]
        extOrDir _ = True

getMatchingLines :: LanguageDetails -> DirTree (String, String) -> [(FilePath, [String])]
getMatchingLines _ (Dir _ [] ) = []
getMatchingLines lang (Dir fName contents ) = concat $ map (getMatchingLines lang) contents
getMatchingLines (Lang ln exts regexTests _ _ ) (File fName (fp,content) ) = [(fp, unsafePerformIO filteredLines)]
    where 
        fileContents = lines <$> readFile fp 
        filteredLines = concat <$> sequence [(filter (unMaybe . matchRegex regexTest)) <$> fileContents | regexTest <- regexTests]
        unMaybe Nothing = False
        unMaybe (Just a) = True
getMatchingLines _ _ = error "Something went wrong"

cleanMatch :: FilePath -> LanguageDetails -> [(FilePath, [String])] -> [(String, [String])]
cleanMatch ignorePath (Lang ln _ _ regexName regexRemoveList) values = map fileName values
    where fileName (fp, linesMatch) = (drop (length ignorePath) fp, cleanLines)
              where cleanLines = [unMaybe lm $ matchRegexAll regexName (cleanStr lm) | lm <- linesMatch ]
                    unMaybe _ (Just (_, match, _, _) ) = match
                    unMaybe str _ = "" --str
                    cleanStr = foldr (.) id (map (\reg is -> subRegex reg is "") regexRemoveList )

haskell :: LanguageDetails
haskell = Lang Haskell [".hs"] --extenstion
                       [mkRegex "import.*"] --import line
                       (mkRegex " [A-z0-9\\.]+")--how to make a name
                       (map mkRegex [ ".*import" --stuff to remove in order of last --> first 
                                    , "qualified"
                                    , "--.*$"])

vhdl :: LanguageDetails
vhdl = Lang VHDL [".vhdl", ".vhd"] 
                 [mkRegex " *use .*[A-z0-9\\_]*\\.[A-z0-9\\_]*;"] 
                 (mkRegex "[A-z0-9\\.]+") 
                 (map mkRegex [ ".*use " -- .....use
                              , "\\.[A-z0-9\\_]*;" --function name 
                              , "--.*$"]) -- comments

prettyDeps :: [(String, [String])] -> Doc 
prettyDeps depRelation = vcat $ map prettyDep depRelation
    where
        prettyDep (fileName, deps) = text ("\n" ++ fileName) 
                                  $$ (nest 5 $ vcat $ map text (filter (/="") deps))

getFullFilePath :: String -> LanguageDetails -> DirTree (String, String) -> [FilePath]
getFullFilePath _ _ (Dir _ [] ) = []
getFullFilePath str lang (Dir fName contents ) = concat $ map (getFullFilePath str lang) contents
getFullFilePath str (Lang ln exts regexTests _ _ ) (File fName (fp,content) ) = [drop (length str) fp]
getFullFilePath _ _ _ = error "Something went wrong"

mkEdgeList :: [(String, [String])] -> [(String, String)]
mkEdgeList connections = concat [[(start, end) | end <- endList] | (start,endList) <- connections  ]

--- Examples ----------------

h_findFileTypes = listFiles "P:\\HaskellCode\\PandocReport" haskell
h_lineMatch = getMatchingLines haskell <$> h_findFileTypes
h_edgeList = cleanMatch "P:\\HaskellCode\\" haskell <$> h_lineMatch
h_printExample = join $ putStrLn.render.prettyDeps <$> h_edgeList



testPath = "Z:\\protected\\src\\SprintFW61\\"
findFileTypes = listFiles testPath vhdl
lineMatch = getMatchingLines vhdl <$> findFileTypes
vhdFilePaths = getFullFilePath testPath vhdl <$> findFileTypes
edgeList = cleanMatch "Z:\\protected\\src\\SprintFW61\\" vhdl <$> lineMatch

printIOList iolist = join $ putStrLn.render.prettyDeps <$> iolist


 --join $ putStrLn.render.hcat.map text <$> vhdFilePaths

mkDepDataBaseVHDL = do edges <- mkEdgeList <$> edgeList
                       writeFile "edgesRaw.txt" $ show edges 
                       allVhdlFiles <- vhdFilePaths
                       writeFile "allFoundVhdlRaw.txt" $ show allVhdlFiles 
                       
compareEdgesAndFound = do edges <- readGraph <$> readFile "edgesRaw.txt"
                          allVhdlFiles <- readStrList <$> readFile "allFoundVhdlRaw.txt"
                          let betterEdges = [(vhdl_FileToAltName x,vhdl_LibToFile y) | (x,y) <- edges, y /= ""]
                          writeEdges <- writeFile "commonNamedEdges.txt" $ show betterEdges
                          let nodesFound = findConnections2Node betterEdges fileName
                          let cleanVHDL = [ (vhdlFile, vhdl_FileToAltName vhdlFile)| vhdlFile <- allVhdlFiles ]
                        --   writeNodes <- writeFile "nodesTouched.txt" $ show betterEdges
                          let depTable = [ (trodoMatch $ elem vhdlAltName nodesFound)
                                        <> comma 
                                        <> text vhdlFile 
                                        | (vhdlFile, vhdlAltName) <- cleanVHDL   ]
                          writeDepTable <- writeFile "depTable.txt" $ (render.vcat) depTable
                          writeDepTable <- writeFile "nodesTouched.txt" $ (render.vcat) (map text nodesFound) -- depTable
                          return ()
    where
        readGraph :: String -> [(String,String)]
        readGraph = read
        readStrList :: String -> [String]
        readStrList = read
        trodoMatch True = text "1"
        trodoMatch False = text "0"
        comma = text ","

vhdl_LibToFile lib = "\\" ++ (subRegex dotRegex (subRegex libRegex lib "\\src") "\\") ++ ".vhd"
    where 
        libRegex = mkRegex "_lib"
        dotRegex = mkRegex "\\."

fileName = "\\dsc_fpga\\src\\dsc_fpga.vhd"

vhdl_FileToAltName :: FilePath -> String
vhdl_FileToAltName = dropWhile (/='\\') 


-- main = join $ writeFile "allVHDLFile.txt" <$> (render.vcat.map text <$> vhdFilePaths)

-- fileListing = lines <$> readFile "allVHDLFile.txt"

-- main = join $ writeFile "importsFound.txt" . render.prettyDeps <$> edgeList
main = join $ writeFile "importsFound.txt" 
            . render 
            . vcat 
            . tupleDoc
            . mkEdgeList <$> edgeList
    where tupleDoc edgeList' = [ text x <+> text "->" <+> text y | (x,y) <- edgeList', y /= ""] 

-- Graph code

type Explored = [Node]
type Queue = [Node]
type Graph = [Edge]
type Edge = (Node,Node)
type Node = String --Int
type TouchableNodes = [Node]

findConnections2Node :: Graph -> Node -> [Node]
findConnections2Node graph node = findAndListFriends graph [node]

findAndListFriends :: Graph -> [Node] -> [Node]
findAndListFriends graph nodes = findAndListFriends' graph ([], nodes)

findAndListFriends' :: Graph -> (TouchableNodes,Queue) -> [Node]
findAndListFriends' graph (done, []) = done
findAndListFriends' graph (done, queue)= findAndListFriends' graph $ (newdone, newqueue)
    where (newdone, newqueue) = findReachableNodes graph (done, queue)

findReachableNodes :: Graph -> (TouchableNodes, Queue) -> (TouchableNodes, Queue)
findReachableNodes graph (done, queue) = (friendsFound, allFriends \\ friendsFound)
    where
        allFriends = foldr sndNub [] $ friendAccumulator graph queue
        friendsFound = union done queue
        sndNub :: Edge -> [Node] -> [Node]
        sndNub (_,x) xs | x `elem` xs = xs
                        | otherwise = x:xs

friendAccumulator :: Graph -> [Node] -> Graph
friendAccumulator graph = friendAccumulatorList graph []

friendAccumulatorList :: Graph -> Graph -> [Node] -> Graph
friendAccumulatorList graph acc = concatMap (friendAccumulatorSingle graph acc)

friendAccumulatorSingle :: Graph -> Graph -> Node -> Graph
friendAccumulatorSingle [] acc _ = acc
friendAccumulatorSingle (edge:graph) acc me = friendAccumulatorSingle graph uniqueEdges me
    where uniqueEdges = if (fst edge == me) && (edge `notElem` acc) then (edge:acc) else acc
    -- where uniqueEdges = if trodoMatch (fst edge) me && (edge `notElem` acc) then (edge:acc) else acc

trodoMatch :: String -> String -> Bool
trodoMatch path importName = vhdl_LibToFile importName == vhdl_FileToAltName path

locatedDirectFriends :: Graph -> [Node] -> [Node]
locatedDirectFriends graph nodes = x ++ y
    where (x,y) = findReachableNodes graph ([],nodes)

-- Test functions

testNodes :: Graph
testNodes = [ ("1","2")
            , ("3","8")
            , ("2","8")
            , ("5","7")
            , ("6","5")
            , ("6","3")
            , ("6","5")
            , ("3","5")
            , ("4","1")
            , ("2","3")
            , ("8","3")
            , ("4","2")
            , ("2","5")
            ]

-- (done,queue) = findReachableNodes testNodes ([],[6])
-- (done2,queue2) = findReachableNodes testNodes (done,queue)
-- (done3,queue3) = findReachableNodes testNodes (done2,queue2)
-- (done4,queue4) = findReachableNodes testNodes (done3,queue3)
-- letsdothis = findConnections2Node testNodes 6
