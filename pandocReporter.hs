import Prelude hiding (writeFile, readFile, putStrLn)
import System.IO.Unsafe
import Text.Pandoc.UTF8
import System.FilePath (takeExtension)   
import Text.Pandoc.Pretty
import System.Directory.Tree (
    AnchoredDirTree(..), DirTree(..),
    filterDir, readDirectoryWith, build, zipPaths, flattenDir
    )

type FileExtension = String 
type HighlightType = String

listFiles :: FilePath -> FileExtension -> IO (DirTree (FilePath, [Char]))
listFiles fileStart ext = do 
    aTree <- readDirectoryWith return fileStart
    let contents = zipPaths aTree
    let extOnly = filterDir extOrDir contents
    return extOnly
  where
    extOrDir :: DirTree a -> Bool
    extOrDir (Dir ('.':_) _) = False
    extOrDir (File n _) = takeExtension n == ext
    extOrDir _ = True
mkReport :: FilePath -> FileExtension -> FilePath -> IO()
mkReport filePath ext saveAs = do files <- listFiles filePath ext
                                  let docResult = boldTitle <> mkPandoc filePath ext files
                                  writeFile saveAs $ render Nothing docResult

mkPandoc :: FilePath -> FileExtension -> DirTree (String, String) -> Doc
mkPandoc _ _ (Dir _ [] ) = text ""
mkPandoc parentPath ext (Dir fName contents ) = document
  where 
    title = text $ header $ "Directory: " ++ fName
    contentDocs :: [Doc]
    contentDocs = map (mkPandoc parentPath ext) contents
    filesOfExt :: [DirTree (String, String)]
    filesOfExt = filter extOnly contents
    fileNames = newline <> (hcat $ map name2Doc filesOfExt)
    fileList = newline
            $$ if filesOfExt == []
               then text "" 
               else text (italic $"Files of type '" ++ ext ++ "' in Directory:") <> fileNames
            $$ newline
    document = title
            $$ newline
            $$ fileList
            $$ newline
            $$ hcat contentDocs
    name2Doc :: DirTree a -> Doc
    name2Doc (File name _) = newline <> (text $ "* " ++ show name) <> newline
    name2Doc _ = text "ERROR"
    extOnly :: DirTree a -> Bool
    extOnly (Dir _ _) = False
    extOnly (File n _) = takeExtension n == ext
    extOnly _ = False
mkPandoc parentPath ext (File fName (fp,content) ) = unsafePerformIO $ document <$> fileCon
  where 
    fileCon = readFile fp 
    startCode = newline <> text ("~~~~ {#mycode " ++ codeType ext ++" .numberLines startFrom=\"1\"}") <> newline
    endCode = newline <> text "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" <> newline
    title = showLoc
    showLoc = text $ bold $ mono $ "\"" ++ (drop ignoreLen $ show fp)
    ignoreLen = 2 + length parentPath - length (keep parentPath)
    keep = takeWhile (\x -> x /= '\\' ).reverse.show
    document txt = title
                $$ newline
                $$ startCode
                $$ text txt
                $$ endCode
                $$ newline
                $$ newPage
                $$ newline
mkPandoc _ _ _ = text ""

codeType :: FileExtension -> HighlightType
codeType ext | ext == ".hs" = ".haskell"
             | ext == ".vhdl" || ext == ".vhd" = ".vhdl"
             | otherwise = ""

header,bold,italic,mono :: String -> String
header txt = "###" ++ txt ++ "###"
bold txt = "**" ++ txt ++ "**"
italic txt =  "*" ++ txt ++ "*"
mono txt = "`" ++ txt ++ "`"

newPage,newline,boldTitle :: Doc
newPage = text "\\newpage"
newline = text "\n"
boldTitle = text "% "



--- Examples----------------
testReportVHDL = mkReport "P:\\clashAdamExamples\\clash-utils\\src\\CLaSH" ".vhdl" "P:\\HaskellCode\\autoDocs\\reportVHDL.txt"

--- Real world 
cookbookReport = mkReport "P:\\HaskellCode\\ClashByExample" ".hs" "P:\\HaskellCode\\autoDocs\\cookingReport.txt"
sprint56_report = mkReport "Z:\\protected\\src\\SprintFW56\\RKV_DELIVERIES\\hsc_fpga" ".vhd" "P:\\HaskellCode\\autoDocs\\sprint56Report.txt"

