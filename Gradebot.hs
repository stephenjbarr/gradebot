{-# LANGUAGE OverloadedStrings #-}

module Gradebot where


import qualified Data.Map as M
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Maybe
import Data.Either.Unwrap
import Data.List

type Code = String

data CodeBlock = CodeBlock {
    cb_code    :: Code,
    cb_comment :: Maybe String
  } deriving (Show)

data GradeRecord = GradeRecord {
     gr_name :: String    
   , gr_Ambiguity :: Maybe String
   , gr_Code_1    :: Maybe String 
   , gr_Code_2    :: Maybe String 
   , gr_Code_3    :: Maybe String 
   , gr_Code_4    :: Maybe String 
   , gr_Code_5    :: Maybe String 
   , gr_Code_6    :: Maybe String 
   , gr_Code_7    :: Maybe String 
   , gr_CC_1      :: Maybe String 
   , gr_CC_2      :: Maybe String 
   , gr_CC_3      :: Maybe String 
   , gr_CC_4      :: Maybe String 
   , gr_CC_5      :: Maybe String 
   , gr_CC_6      :: Maybe String 
   , gr_CC_7      :: Maybe String 
   , gr_Misc_Comment :: Maybe String
     } deriving (Show)
  
data IndivCode = IndivCode {
      ic_code    :: Code
    , ic_title   :: String
    , ic_text    :: String  
    , ic_points  :: Double
    , ic_qnum    :: Int
    } deriving (Show)


type GMap = M.Map Code IndivCode

data Student = Student {
    studentName :: String
  , studentID   :: Int
  } deriving (Show)

data QFeedback = QFeedback {
    questionNum         :: Int
  , questionCodes       :: [CodeBlock]
  , questionMiscComment :: Maybe String
  , questionAmbiguity   :: Maybe String
  } deriving (Show)


data SFeedback = SFeedback {
  sf_student   :: Student,
  sf_qfeedback :: [QFeedback]
  } deriving (Show)


----------------------------------------
-- parsing gradeRecords
instance FromNamedRecord GradeRecord where
    parseNamedRecord r = GradeRecord <$> r .:  "Name"  <*> r .:  "Ambiguity"    <*> r .:  "Code_1"       <*> r .:  "Code_2"       <*> r .:  "Code_3"       <*> r .:  "Code_4"       <*> r .:  "Code_5"       <*> r .:  "Code_6"       <*> r .:  "Code_7"       <*> r .:  "CC_1"         <*> r .:  "CC_2"         <*> r .:  "CC_3"         <*> r .:  "CC_4"         <*> r .:  "CC_5"         <*> r .:  "CC_6"         <*> r .:  "CC_7"         <*> r .:  "Misc_Comment" 

parseFname :: BL.ByteString -> Either String [GradeRecord] 
parseFname csvData = 
    case decodeByName csvData of
        Left err     -> Left err
        Right (_, v) -> Right (V.toList v)


makeCodeTuples :: GradeRecord -> [CodeBlock]
makeCodeTuples gri = cb_list
  where
    cb_raw = [(gr_Code_1 gri, gr_CC_1 gri),
              (gr_Code_2 gri, gr_CC_2 gri), 
              (gr_Code_3 gri, gr_CC_3 gri), 
              (gr_Code_4 gri, gr_CC_4 gri), 
              (gr_Code_5 gri, gr_CC_5 gri), 
              (gr_Code_6 gri, gr_CC_6 gri), 
              (gr_Code_7 gri, gr_CC_7 gri)]

    has_code = filter (\a -> isJust $ fst a) cb_raw
    filtered = map (\a -> (fromJust $ fst a, snd a) ) has_code
    cb_list  = map (\(a,b) -> CodeBlock a b) filtered


makeQF :: GradeRecord -> Int -> QFeedback
makeQF gri qnum = qf
  where
    qf = QFeedback qnum (makeCodeTuples gri) (gr_Misc_Comment gri) (gr_Ambiguity gri)

makeStudent :: GradeRecord -> Student
makeStudent gri = Student (gr_name gri) 123456

processGR :: GradeRecord -> Int -> SFeedback
processGR gri qnum = SFeedback (makeStudent gri) [(makeQF gri qnum)]


addQF :: SFeedback -> QFeedback -> SFeedback
addQF sf qf = sf { sf_qfeedback = qf : sf_qfeedback sf  }



--------------------------------------------------------------------------------

stringifyCodeBlockHelper :: CodeBlock -> String
stringifyCodeBlockHelper cb = out
  where 
   out = "CODE: " ++ (cb_code cb) ++ " COMMENT: " ++ (fromMaybe "NONE" (cb_comment cb))

stringifyCodeBlock :: [CodeBlock] -> [String]
stringifyCodeBlock codes
  | n == 0 = []
  | otherwise = out
  where
    n = length codes
    out  = map stringifyCodeBlockHelper codes
    


codeLookupWithDefault :: [CodeBlock] -> GMap -> [IndivCode]
codeLookupWithDefault codes myGMap = indivs
  where
    codelist  = map (\cb -> cb_code cb) codes
    indivlist = map (\cb -> M.lookup cb myGMap) codelist
    indivs    = map (fromMaybe defaultIndivCode) indivlist


----------------------------------------
-- fix this to work for multiple questions
makeReport ::  SFeedback -> Int -> String
makeReport sf qn = rstr 
  where
    st = sf_student sf
    qf = sf_qfeedback sf !! 0
    codes = questionCodes qf 

    l1 = "STUDENT: " ++ studentName st
    l2 = "QUESTION: " ++ show qn
    l3 = "N CODES: " ++ show (length codes)
    l4 = unlines $ stringifyCodeBlock codes

    rstr = unlines [l1, l2, l3, l4]

----------------------------------------

showIC :: IndivCode -> String
showIC ic = str
  where
    l1 = "    CODE DESCR: "     ++  ic_code    ic          
    l2 = "      TITLE: "  ++  ic_title   ic
    l3 = "      TEXT: "   ++  ic_text    ic
    l4 = "      POINTS: " ++  show (ic_points  ic)
    l5 = "      QNUM: "   ++  show (ic_qnum    ic)
    str = unlines [l1, l2, l3, l4, l5]
    

showICfancy :: CodeBlock -> IndivCode -> String
showICfancy cb ic = str
  where
    l1 = "    CODE DESCR: "     ++  ic_code    ic          
    l2 = "      "  ++  ic_title   ic
    l3 = "      "   ++  ic_text    ic
    l4 = "      POINTS: " ++  show (ic_points  ic)
    cstr = fromMaybe "" (cb_comment cb)
    l5 = if cstr == "" then "" else ("      COMMENT: " ++ cstr)
--    l5 = "      COMMENT: " ++ 
    str = unlines [l1,l2, l3, l4, l5]


makeCodeReport :: SFeedback -> GMap -> String
makeCodeReport sf myGMap = rstr
  where
    st = sf_student sf
    qf = sf_qfeedback sf !! 0
    codes = questionCodes qf 
    iclist = codeLookupWithDefault codes myGMap
    rstr = unlines $ map showIC iclist




makeCodeReport_Q :: QFeedback -> GMap -> String
makeCodeReport_Q qf myGMap = rstr
  where
    codes           = questionCodes qf
    l1              = "Question: " ++ show (questionNum qf)
    iclist = codeLookupWithDefault codes myGMap
    rstr = unlines $ (++) [l1] (zipWith showICfancy codes iclist)



outerReport :: [GradeRecord] -> Int -> GMap -> String
outerReport grlist qnum myGMap = out
  where
    sflist  = map (\a -> processGR a qnum) grlist
    outlist = map (\a -> makeReport a qnum) sflist
    codereplist = map (\a -> makeCodeReport a myGMap) sflist
    seplist = replicate (length outlist) "--------------------------------------------------------------------------------" 
    out = unlines $ merge [outlist, codereplist, seplist]


make_full_SF :: [([GradeRecord],Int)] -> [SFeedback]
make_full_SF gr_list = sf_out
  where
    process gr_i qnum = map (\a -> processGR a qnum) gr_i
    sf_list           = map (\(g,i) -> process g i) gr_list
    sf_out            = mergeSFLists sf_list


-- studentReport takes the list of parsed GradeRecords, and the
    -- mapping per question
--  and the code mapping, and returns a report of the form:
--
--   STUDENT:
--    -Q1, Q2


studentReport :: SFeedback -> GMap -> String
studentReport sf myGMap = out
   where
    st = sf_student sf
    l0 = "--------------------------------------------------------------------------------"
    l1 = "STUDENT: " ++ studentName st
    qf_list = sf_qfeedback sf
    l2 = unlines $ map (\a -> indivQuestionReport a myGMap) qf_list
    l3 = groupQuestionReport qf_list myGMap
    out = unlines [l0, l1, l2, l3]

indivQuestionReport :: QFeedback -> GMap -> String
indivQuestionReport = makeCodeReport_Q


groupQuestionReport :: [QFeedback] -> GMap -> String
groupQuestionReport qf_list myGMap = rstr
  where
    
    allcodes     =  map (\q -> codeLookupWithDefault (questionCodes q) myGMap) qf_list
    sum_pts icl  =  sum $ map ic_points icl
    points_per_q =  map sum_pts allcodes

    l1 = "Total deductions: " ++ show (sum $ points_per_q)
    rstr = l1

----------------------------------------

instance FromNamedRecord IndivCode where
    parseNamedRecord r = IndivCode <$> r .: "Code" <*> r .: "Title" <*> r .: "Text" <*> r .: "Points" <*> r .: "Qnum"


parseCodeFile :: BL.ByteString -> Either String [IndivCode] 
parseCodeFile codeData = 
    case decodeByName codeData of
        Left err     -> Left err
        Right (_, v) -> Right (V.toList v)


makeGMap :: [IndivCode] -> GMap
makeGMap iclist = themap
  where
    mylist = map (\a -> ic_code a) iclist
    themap = M.fromList $ zip mylist iclist


merge = concat . transpose
defaultIndivCode = IndivCode "INVALID LOOKUP" "--" "--" (-99.0) 0


-- merge2parlist :: [SFeedback] -> [SFeedback] -> SFeedback
-- merge2parlist a b = ab
--   where
--     n = length a
--     ab = map (\i -> addQF (a !! i) (sf_qfeedback (b !! i))) [0..(n-1)]


-- this merges the SF, taking care to merge their lists
mergeSF :: SFeedback -> SFeedback -> SFeedback
mergeSF a b = a { sf_qfeedback = (sf_qfeedback a) ++ (sf_qfeedback b) }
  


-- assumes that this length > 1, and that all lists have the same length
mergeSFLists :: [[SFeedback]] -> [SFeedback]
mergeSFLists sfl = sf_final
  where
    sf_first          = head sfl
    n                 = length sf_first
    per_student_lists = map (\i -> (fmap (!! i) sfl)) [0..(n-1)]

    widemerge x       =  foldl mergeSF (head x) (tail x)
    sf_final          =  map widemerge per_student_lists
----------------------------------------

main :: IO ()
main = do
  csvData <- BL.readFile "graded-1-export.csv"
  let glist =  parseFname csvData
  let gr1   =  fromRight glist

  csvData <- BL.readFile "graded-2-export.csv"
  let glist2 =  parseFname csvData
  let gr2   =  fromRight glist2

  csvData <- BL.readFile "graded-3-export.csv"
  let glist3 =  parseFname csvData
  let gr3   =  fromRight glist3

  csvData <- BL.readFile "graded-4-export.csv"
  let glist4 =  parseFname csvData
  let gr4   =  fromRight glist4



  codeData    <- BL.readFile "full-error-codes-v2.csv"
  let codeObj =  parseCodeFile codeData
  let myGMap  = makeGMap (fromRight codeObj)

  let gr_list  = [(gr1,1), (gr2,2), (gr3,3), (gr4,4)]
  let x = make_full_SF gr_list

  let strs = map (\student -> studentReport student myGMap) x

  let fname = "final-report.txt"
  writeFile fname (unlines strs)
  
  print "Done"
  

-- parseCodes :: String -> GMap
-- parseCodes fname = gmap
--   where
  -- case glist of
  --   Left err  -> putStrLn err
  --   Right grs -> print $ outerReport grs 1 myGMap

  -- case glist2 of
  --   Left err  -> putStrLn err
  --   Right grs -> print $ outerReport grs 3 myGMap

  -- case glist3 of
  --   Left err  -> putStrLn err
  --   Right grs -> print $ outerReport grs 3 myGMap

  -- case glist4 of
  --   Left err  -> putStrLn err
  --   Right grs -> print $ outerReport grs 4 myGMap
