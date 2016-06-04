{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.BrainFuck(exec,BFConfig(..)) where

import              Control.Monad.Except
import              Control.Monad.State
import              Text.Printf(printf)
import Data.Word(Word8)
import Data.Char(ord,chr,isControl,isSpace,showLitChar)
import qualified    Data.List                   as L  
import qualified    Data.Vector.Unboxed         as U


type Byte = Word8

data Token = MovePointer {-# UNPACK #-} !Int
           | FlyPointer  {-# UNPACK #-} !Int 
           | AddValue    {-# UNPACK #-} !Int {-# UNPACK #-} !Byte 
           | SetValue    {-# UNPACK #-} !Int {-# UNPACK #-} !Byte 
           | SetValueIf  {-# UNPACK #-} !Int {-# UNPACK #-} !Byte {-# UNPACK #-} !Int 
           | CopyValue   {-# UNPACK #-} !Int {-# UNPACK #-} !Byte {-# UNPACK #-} !Int 
           | Output      {-# UNPACK #-} !Int 
           | Input       {-# UNPACK #-} !Int 
           | Loop        [Token]
           | Assert      {-# UNPACK #-} !Int -- Must 0 at some point
           | Empty
           deriving (Eq)

instance Show Token where
  show = go 2
    where space i = '\n' : replicate i ' '
          i2str i | i >= 0    = show i
                  | otherwise = ('(':show i) ++ ")"
          t2str i tokens = L.intercalate "," $ fmap (go i) tokens
          go l (Loop [])           = "[]"
          go l (Loop tokens)       = ('[':space (l+2)) ++ t2str (l+2) tokens ++ (space l) ++ "]"
          go l (MovePointer i)     = printf "%s %s"       "MovePointer" (i2str i)
          go l (FlyPointer  i)     = printf "%s %s"       "FlyPointer"  (i2str i)
          go l (AddValue    i p)   = printf "%s %s %s"    "AddValue"    (i2str i) (i2str p)
          go l (SetValue    i p)   = printf "%s %s %s"    "SetValue"    (i2str i) (i2str p)
          go l (SetValueIf  i p j) = printf "%s %s %s %s" "SetValueIf"  (i2str i) (i2str p) (i2str j)
          go l (CopyValue   i p j) = printf "%s %s %s %s" "CopyValue"   (i2str i) (i2str p) (i2str j)
          go l (Output      i)     = printf "%s %s"       "Output"      (i2str i)
          go l (Input       i)     = printf "%s %s"       "Input"       (i2str i)
          go l (Assert      i)     = printf "%s %s"       "Assert"      (i2str i)
          go l (Empty)            =                       "Empty"

data BrainFuckContext = BrainFuckContext
    { bfStr    :: String
    , bfPos    :: {-# UNPACK #-} !Int
    , bfLine   :: {-# UNPACK #-} !Int
    , bfConfig :: BFConfig
    } deriving (Eq,Show)

defaultContext :: BFConfig -> String -> BrainFuckContext
defaultContext config str = BrainFuckContext 
    { bfStr    = str
    , bfPos    = 0
    , bfLine   = 0
    , bfConfig = config
    }

isDebug :: BrainFuckContext -> Bool
isDebug = bfDebugs . bfConfig

newtype BFParser a = BFParser 
                   { runBF :: ExceptT String (State BrainFuckContext) a 
                   } deriving ( Monad
                              , Applicative
                              , Functor
                              , MonadError String
                              , MonadState BrainFuckContext
                              )

failLog :: String -> BrainFuckContext -> String
failLog str context  = if isDebug context then (go str context) else str
    where go str cxt = printf "Error %s at Position %d:%d" str (bfPos cxt) (bfPos cxt)


updateContext :: BrainFuckContext -> String -> String -> BrainFuckContext
updateContext cxt used unused = let (h,t) = span (=='\n') used in
  if L.null t then cxt{bfStr=unused,bfPos=L.length h + bfPos cxt}
            else cxt{bfStr=unused,bfPos=L.length t,bfLine=bfLine cxt+1}
                  

runParser :: BFParser a -> BrainFuckContext -> Either String (a, BrainFuckContext)
runParser p cxt = case (runState . runExceptT . runBF) p cxt of
  (Left  e,cxt') -> Left (failLog e cxt')
  (Right r,cxt') -> Right (r,cxt')


(<|>) :: BFParser a -> BFParser a -> BFParser a
p <|> q = do
  cxt <- get
  catchError p (\_ -> q)

none :: BFParser [a]
none = return []

some :: BFParser a -> BFParser [a]
some p = do {x <- p; xs <- many p; return (x:xs)}

many :: BFParser a -> BFParser [a]
many = optional . some

optional :: BFParser [a] -> BFParser [a]
optional = (<|> none)

satisfy :: (Char -> Bool) -> BFParser Char 
satisfy p = do
  context <- get
  case bfStr context of
    x:xs | p x -> put (updateContext context [x] xs) >> return x
    x:_        -> throwError ("Unexcepted " ++ [x])
    _          -> throwError  "Unexcepted eof"


choice :: [BFParser a] -> BFParser a
choice = L.foldl' (<|>) (throwError "Unexcepted Token")

parseOne :: BFParser Token
parseOne = choice [ ignore
                  , transform '>' (MovePointer   1   )
                  , transform '<' (MovePointer (-1)  )
                  , transform '+' (AddValue      0  1)
                  , transform '-' (AddValue      0 (-1))
                  , transform '.' (Output        0   )
                  , transform ',' (Input         0   )
                  , parseLoop
                  ] 
  where transform char token = satisfy (== char) >> return token
        ignore               = satisfy ignoreCondition >> return Empty
        ignoreCondition      = not . (`elem` "><+-,.[]")

parseLoop :: BFParser Token
parseLoop = do
  consume '['
  steps <- many parseOne
  consume ']'
  return $ Loop steps
  where consume char = satisfy (== char) >> return ()

eof :: BFParser ()
eof = do 
  s <- get
  case bfStr s of 
    [] -> return ()
    _  -> throwError "Unexpected EOF"


parse :: BFConfig -> String -> Either String ([Token], BrainFuckContext)
parse config = runParser go . defaultContext config . filter (`elem` "><+-,.[]")
      where go :: BFParser [Token]
            go = do {expr <- many parseOne ; eof ; return expr } 

optimize :: Token -> Token
optimize (Loop tokens) = Loop 
                       . op4MergeAddValue
                       . op3SwitchPointer
                       . op2Pointer
                       . op1Value
                       . op0 
                       $ tokens
optimize token         = token

op0 :: [Token] -> [Token]
op0 []     = []
op0 (a:as) = case optimizeLoop $ optimize a of
  [Empty] -> op0 as
  a'    -> a' ++ op0 as 

optimizeLoop :: Token -> [Token]
optimizeLoop (Loop []) = [Assert 0]
optimizeLoop (Loop [AddValue 0 n]) | n /= 0    = [SetValue 0 0]
                                   | otherwise = [Assert 0]
optimizeLoop (Loop [AddValue 0 (-1),SetValue i n]) | i == 0 && n == 0 = [SetValue 0 0]
                                                   | i == 0           = [Assert 0]
                                                   | i /= 0 = [SetValueIf i n 0,SetValue 0 0] 
optimizeLoop (Loop (a@(AddValue 0 (-1)):vs))   = case go vs of
  Nothing  -> [Loop (a:vs)]
  Just vs' -> vs' ++ [SetValue 0 0]
  where go (AddValue i m:vs'') = go vs'' >>= \vs''' -> return (CopyValue i m 0:vs''')
        go [] = Just []
        go _  = Nothing
optimizeLoop (Loop [MovePointer i]) | i /= 0    = [FlyPointer i]
                                    | otherwise = [Assert 0]
optimizeLoop a = [a] 


op1Value :: [Token] -> [Token]
op1Value (AddValue _ 0:vs) = op1Value vs
op1Value (AddValue i m:AddValue j n:vs) | i == j    = op1Value $ AddValue i (m+n): vs
op1Value (v:vs) = v:op1Value vs
op1Value []     = []

op2Pointer :: [Token] -> [Token]
op2Pointer (MovePointer 0:vs) = op2Pointer vs
op2Pointer (MovePointer n:MovePointer m:vs) = op2Pointer $ MovePointer (m+n) : vs
op2Pointer (v:vs) = v:op2Pointer vs
op2Pointer []     = []

op3SwitchPointer :: [Token] -> [Token]
op3SwitchPointer (MovePointer i:AddValue   j m   :vs) = AddValue   (i+j) m       : op3SwitchPointer (MovePointer i:vs)
op3SwitchPointer (MovePointer i:SetValue   j m   :vs) = SetValue   (i+j) m       : op3SwitchPointer (MovePointer i:vs)
op3SwitchPointer (MovePointer i:SetValueIf j m k :vs) = SetValueIf (i+j) m (i+k) : op3SwitchPointer (MovePointer i:vs)
op3SwitchPointer (MovePointer i:Output     j     :vs) = Output     (i+j)         : op3SwitchPointer (MovePointer i:vs)
op3SwitchPointer (MovePointer i:Input      j     :vs) = Input      (i+j)         : op3SwitchPointer (MovePointer i:vs)
op3SwitchPointer (MovePointer i:Assert     j     :vs) = Assert     (i+j)         : op3SwitchPointer (MovePointer i:vs)
op3SwitchPointer (MovePointer i:CopyValue  j m k :vs) | m == 0    = op3SwitchPointer (MovePointer i:vs)
                                                      | otherwise = CopyValue (i+j) m (i+k) : op3SwitchPointer (MovePointer i:vs)
op3SwitchPointer (MovePointer i:MovePointer j    :vs) | k == 0 = op3SwitchPointer vs
                                                      | otherwise  = op3SwitchPointer (MovePointer k:vs)
                                                      where k =  i + j
op3SwitchPointer (SetValue j m:AddValue   i n    :vs) | i == j    = op3SwitchPointer (SetValue   i (m + n):vs)
                                                      | otherwise = AddValue   i n:op3SwitchPointer (SetValue   j m:vs)
op3SwitchPointer (AddValue i n:SetValue   j m    :vs) | i == j    = op3SwitchPointer (SetValue   j m:vs)
op3SwitchPointer (SetValueIf j m k: AddValue i n :vs) | i /= j && i /= k   = AddValue   i n:op3SwitchPointer (SetValueIf j m k:vs)
op3SwitchPointer (CopyValue  j m k: AddValue i n :vs) | i /= j && i /= k   = AddValue   i n:op3SwitchPointer (CopyValue  j m k:vs)
op3SwitchPointer (Output j :        AddValue i n :vs) | i /= j    = AddValue   i n:op3SwitchPointer (Output j:vs)
op3SwitchPointer (Input  j :        AddValue i n :vs) | i /= j    = AddValue   i n:op3SwitchPointer (Input  j:vs)
op3SwitchPointer (Assert j :        AddValue i n :vs) | i /= j    = AddValue   i n:op3SwitchPointer (Assert j:vs)
op3SwitchPointer []                       = []
op3SwitchPointer (a:vs)                   = a:op3SwitchPointer vs


op4MergeAddValue :: [Token] -> [Token]
op4MergeAddValue vs = case splitPPlus vs of
  ([],[])     -> []
  ([],(v:vs)) -> v:op4MergeAddValue vs
  (ps,vs)     -> sortPPlus ps ++ op4MergeAddValue vs

sortPPlus :: [Token] -> [Token]
sortPPlus ps = go (sortPs ps)
  where sortPs = L.sortBy cp
        cp (AddValue i _) (AddValue j _) | i == j                       = EQ
                                         | i == 0 || ( i < j && j /= 0) = LT
                                         | otherwise                    = GT
        go [] = []
        go (AddValue i m:AddValue j n:vs) 
            | i == j    = go (AddValue i (m + n):vs)
            | otherwise = AddValue i m : go (AddValue j n:vs)
        go (p:ps') = p:go ps'

splitPPlus :: [Token] -> ([Token],[Token])
splitPPlus [] = ([],[])
splitPPlus (p@(AddValue _ _):vs) = let (ps,vs') = splitPPlus vs in (p:ps,vs')
splitPPlus vs = ([],vs)


bytecode :: BFConfig -> String -> Either String [Token]
bytecode config str = case parse config str of
  Left            e  -> Left e
  Right (tokens,cxt) -> let op = if (bfOptimize config) then id else optimize in
    case op $ Loop tokens of
      Loop tokens' -> Right tokens'

type Memory = (Int,U.Vector Byte)

runTokens :: BFConfig -> Memory -> [Token] -> IO Memory
runTokens config cxt (t:ts) = do
  when (bfDebugs config) (print cxt)
  cxt' <- runToken config cxt t
  runTokens config cxt' ts
runTokens _      cxt     [] = return cxt

runToken :: BFConfig -> Memory -> Token -> IO Memory
runToken c context    (Loop   tokens)     = let (i,vector) = context
                                                iv         = getInt vector i in case iv of
                                                     0     -> return context
                                                     _     -> do
                                                       con <- L.foldl' f (return (i,vector)) tokens 
                                                       when (bfDebugs c) (print con)
                                                       runToken c con (Loop tokens)
                                                     where f ct token = do {cxt' <- ct ; runToken c cxt' token}
runToken _ context    (Empty)             =             return context
runToken _ (i,vector) (Assert      j)     | jv == 0   = return (i, vector)
                                          | otherwise = error "Infinite Loop"
                                          where jv    = getInt vector (i + j)
runToken _ (i,vector) (MovePointer j)     =             return (i + j,vector)
runToken _ context    (FlyPointer  j)     | j == 0    = return context
                                          | otherwise = return (go j context)
                                          where go j (i,vector) | vi == 0     = (i,vector)
                                                                | otherwise   = go j (i + j,vector)
                                                                where vi = getInt vector i
runToken _ (i,vector) (AddValue    j m)   | m == 0    = return (i, vector)
                                          | otherwise = return (i, addTo vector (i + j) m)
runToken _ (i,vector) (SetValue    j m)   =             return (i, setTo vector (i + j) m)
runToken _ (i,vector) (SetValueIf  j m k) | v == 0    = return (i, vector)
                                          | otherwise = return (i, setTo vector (i + j) m)
                                          where v     = getInt vector (i + k)
runToken _ (i,vector) (CopyValue   j m k) | m == 0    = return (i, vector)
                                          | kv == 0   = return (i, vector)
                                          | otherwise = return (i, addTo vector (i + j) m')
                                          where kv    = getInt vector (i + k)
                                                m'    = m * kv
runToken _ (i,vector) (Output      j)     = pch jv   >> return (i,vector)
                                          where jv    = chr $ fromIntegral $ getInt vector (i + j)
                                                isp c = isControl c && not (isSpace c || c == '\ESC')
                                                pch c | isp c     = putStr (showLitChar c "")
                                                      | otherwise = putChar c
runToken _ (i,vector) (Input       j)     = getChar >>= return . \c -> (i,setTo vector (i + j) (fromIntegral $ ord c))


getInt :: U.Vector Byte -> Int -> Byte
getInt vector = (vector U.!)

addTo :: U.Vector Byte -> Int -> Byte -> U.Vector Byte 
addTo vector i m = setTo vector i m'
    where m' = m + (getInt vector i)

setTo :: U.Vector Byte -> Int -> Byte -> U.Vector Byte
setTo vector i m = U.update vector $ U.singleton (i, m)


data BFConfig = BFConfig 
  { bfVerbose  :: Bool
  , bfDebugs   :: Bool
  , bfShow     :: Bool
  , bfTime     :: Bool
  , bfOptimize :: Bool
  , bfDryrun   :: Bool
  , bfSize     :: !Int
  , bfExpress  :: String
  , bfFile     :: String
  } deriving (Eq,Show)


defaultConfig :: BFConfig
defaultConfig = BFConfig
  { bfVerbose  = False
  , bfDebugs   = False
  , bfShow     = False
  , bfTime     = False
  , bfOptimize = False
  , bfDryrun   = False
  , bfSize     = 512
  , bfExpress  = ""
  , bfFile     = "-"
  }

exec :: BFConfig -> String -> IO ()
exec config str = case bytecode config str of
  Left  err    -> putStrLn err
  Right tokens -> do
    when (bfShow config) (print tokens)
    if (bfDryrun config) then return ()
    else do 
      mem <- runTokens config (0,U.generate (bfSize config) (\_ -> 0)) tokens
      when (bfVerbose config) (print mem)

