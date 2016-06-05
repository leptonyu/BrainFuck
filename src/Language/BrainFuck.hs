{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}

module Language.BrainFuck(exec,parseExpr,parseExpr',run,BFConfig(..)) where

import              Control.Monad(unless)
import              Control.Monad.Except
import              Control.Monad.State
import              Data.Monoid((<>))
import              Data.Word(Word8)
import              Data.Char(ord,chr,isControl,isSpace,showLitChar)
import              Data.Attoparsec.ByteString as B
import qualified    Data.List             as L  
import qualified    Data.Vector.Unboxed   as U
import qualified    Data.ByteString.Char8 as C
import qualified    Data.ByteString.Internal as BS (c2w, w2c)
import qualified    Data.ByteString       as BB


type Byte = Word8

data Token = Move        {-# UNPACK #-} !Int
           | Scan        {-# UNPACK #-} !Int 
           | AddValue    {-# UNPACK #-} !Int {-# UNPACK #-} !Byte 
           | SetValue    {-# UNPACK #-} !Int {-# UNPACK #-} !Byte 
           | SetValueIf  {-# UNPACK #-} !Int {-# UNPACK #-} !Byte {-# UNPACK #-} !Int 
           | CopyValue   {-# UNPACK #-} !Int {-# UNPACK #-} !Byte {-# UNPACK #-} !Int 
           | Output      {-# UNPACK #-} !Int                      {-# UNPACK #-} !Int 
           | Input       {-# UNPACK #-} !Int 
           | Loop        [Token]
           | Assert      {-# UNPACK #-} !Int -- Must 0 at some point
           | Empty
           deriving (Eq,Show)

{-# INLINE i2str #-}
i2str :: (Show a,Num a,Ord a) => a -> C.ByteString
i2str i | i >= 0    = C.pack $ show i
        | otherwise = "(" <> C.pack ( show i) <> ")"

{-# INLINE i2space #-}
i2space :: Int -> C.ByteString
i2space i = "\n" <> C.replicate i ' '


toStr :: [Token] -> C.ByteString
toStr = go 2 . Loop
    where t2str i tokens           = C.intercalate "," $ fmap (go i) tokens
          go _ (Loop [])           = "[]"
          go l (Loop tokens)       = "[" <> i2space (l+2) <> t2str (l+2) tokens <> i2space l <> "]"
          go _ (Move        i)     = "Move "       <> i2str i
          go _ (Scan        i)     = "Scan "       <> i2str i
          go _ (AddValue    i p)   = "AddValue "   <> i2str i <> " " <> i2str p
          go _ (SetValue    i p)   = "SetValue "   <> i2str i <> " " <> i2str p
          go _ (SetValueIf  i p j) = "SetValueIf " <> i2str i <> " " <> i2str p <> " " <> i2str j
          go _ (CopyValue   i p j) = "CopyValue "  <> i2str i <> " " <> i2str p <> " " <> i2str j
          go _ (Output      i   j) = "Output "     <> i2str i                   <> " " <> i2str j
          go _ (Input       i)     = "Input "      <> i2str i
          go _ (Assert      i)     = "Assert "     <> i2str i
          go _ (Empty)             = "Empty"


{-# INLINE parseToken #-}
parseToken :: Parser Token
parseToken =  notWord8 93 >>= \case              -- ]
                 60 -> return $ Move (-1)        -- <
                 62 -> return $ Move   1         -- >
                 43 -> return $ AddValue 0   1   -- +
                 45 -> return $ AddValue 0 (-1)  -- -
                 44 -> return $ Input  0         -- ,
                 46 -> return $ Output 0 1       -- .
                 91 -> do                        -- [
                       token  <- parseToken
                       tokens <- B.manyTill' parseToken $ word8 93
                       return $ Loop $ token:tokens
                 _  -> return   Empty

parseExpr :: Parser [Token]
parseExpr = B.many' parseToken

parseExpr' :: Parser [Token]
parseExpr' = fmap optimize0 parseExpr

run :: Show a => Parser a -> C.ByteString -> IO ()
run p input = case parseOnly (p <* B.endOfInput) input of
        Left  e -> C.putStr   "Parse error at " >> print  e
        Right x -> print x

optimize :: Token -> Token
optimize (Loop tokens) = Loop . optimize0 $ tokens
optimize token         = token

optimize0 = op4MergeAddValue
          . op3SwitchAddValue
          . op3SwitchPointer
          . op2Output
          . op2Pointer
          . op1Value
          . op0

{-# INLINE op0 #-}
op0 :: [Token] -> [Token]
op0 (Empty:vs) =                              op0 vs
op0     (v:vs) = optimizeLoop (optimize v) ++ op0 vs
op0        []  =                              []

optimizeLoop :: Token -> [Token]
optimizeLoop (Loop [])                                                = [Assert   0  ]
optimizeLoop (Loop [AddValue 0 n])                 | n /= 0           = [SetValue 0 0]
                                                   | otherwise        = [Assert   0  ]
optimizeLoop (Loop [AddValue 0 (-1),SetValue i n]) | i == 0 && n == 0 = [SetValue 0 0]
                                                   | i == 0           = [Assert   0  ]
                                                   | i /= 0           = [SetValueIf i n 0,SetValue 0 0] 
optimizeLoop (Loop (a@(AddValue 0 (-1)):vs))                          = case go vs of
                                Nothing  ->        [Loop (a:vs)]
                                Just vs' -> vs' ++ [SetValue 0 0]
                                where go (AddValue i m:vs'')          = go vs'' >>= \vs''' -> return (CopyValue  i m 0:vs''')
                                      go (SetValue i m:vs'') | i /= 0 = go vs'' >>= \vs''' -> return (SetValueIf i m 0:vs''')
                                      go []                           = Just []
                                      go _                            = Nothing
optimizeLoop (Loop [Move i])                       | i /= 0           = [Scan i]
                                                   | otherwise        = [Assert  0   ]
optimizeLoop a                                                        = [a] 


{-# INLINE op1Value #-}
op1Value :: [Token] -> [Token]
op1Value (AddValue _ 0             :vs)          =   op1Value                     vs
op1Value (AddValue i m:AddValue j n:vs) | i == j =   op1Value $ AddValue i (m+n): vs
op1Value                         (v:vs)          = v:op1Value                     vs
op1Value []                                      =   []

{-# INLINE op2Pointer #-}
op2Pointer :: [Token] -> [Token]
op2Pointer (Move 0       :vs) =   op2Pointer                       vs
op2Pointer (Move n:Move m:vs) =   op2Pointer $ Move (m+n) : vs
op2Pointer             (v:vs) = v:op2Pointer                       vs
op2Pointer []                 =   []

{-# INLINE op2Output #-}
op2Output :: [Token] -> [Token]
op2Output (Output i n:Output j m:vs) | i == j =   op2Output $ Output i (m+n) : vs
op2Output (v:vs)                              = v:op2Output                    vs
op2Output []                                  =   []


{-# INLINE op3SwitchPointer #-}
op3SwitchPointer :: [Token] -> [Token]
op3SwitchPointer (Move i:AddValue   j m   :vs)             = AddValue   (i+j) m       : op3SwitchPointer (Move i:vs)
op3SwitchPointer (Move i:SetValue   j m   :vs)             = SetValue   (i+j) m       : op3SwitchPointer (Move i:vs)
op3SwitchPointer (Move i:SetValueIf j m k :vs)             = SetValueIf (i+j) m (i+k) : op3SwitchPointer (Move i:vs)
op3SwitchPointer (Move i:Output     j   k :vs)             = Output     (i+j)      k  : op3SwitchPointer (Move i:vs)
op3SwitchPointer (Move i:Input      j     :vs)             = Input      (i+j)         : op3SwitchPointer (Move i:vs)
op3SwitchPointer (Move i:Assert     j     :vs)             = Assert     (i+j)         : op3SwitchPointer (Move i:vs)
op3SwitchPointer (Move i:CopyValue  j m k :vs) | m == 0    =                            op3SwitchPointer (Move i:vs)
                                               | otherwise = CopyValue  (i+j) m (i+k) : op3SwitchPointer (Move i:vs)
op3SwitchPointer (Move i:Move j           :vs) | k == 0    =                            op3SwitchPointer         vs
                                               | otherwise =                            op3SwitchPointer (Move k:vs)
                                               where k     = i + j
op3SwitchPointer                        (a:vs)             = a                        : op3SwitchPointer         vs
op3SwitchPointer []                                               = []


{-# INLINE op3SwitchAddValue #-}
op3SwitchAddValue :: [Token] -> [Token]
op3SwitchAddValue (SetValue   j m  : AddValue i n :vs) | i == j           =                op3SwitchAddValue (SetValue   i (m + n) :vs)
                                                       | otherwise        = AddValue   i n:op3SwitchAddValue (SetValue   j  m      :vs)
op3SwitchAddValue (AddValue   i n  : SetValue j m :vs) | i == j           =                op3SwitchAddValue (SetValue   j  m      :vs)
op3SwitchAddValue (SetValueIf j m k: AddValue i n :vs) | i /= j && i /= k = AddValue   i n:op3SwitchAddValue (SetValueIf j  m   k  :vs)
op3SwitchAddValue (CopyValue  j m k: AddValue i n :vs) | i /= j && i /= k = AddValue   i n:op3SwitchAddValue (CopyValue  j  m   k  :vs)
op3SwitchAddValue (Output     j   k: AddValue i n :vs) | i /= j           = AddValue   i n:op3SwitchAddValue (Output     j      k  :vs)
op3SwitchAddValue (Input      j    : AddValue i n :vs) | i /= j           = AddValue   i n:op3SwitchAddValue (Input      j         :vs)
op3SwitchAddValue (Assert     j    : AddValue i n :vs) | i /= j           = AddValue   i n:op3SwitchAddValue (Assert     j         :vs)
op3SwitchAddValue                               (a:vs)                    =              a:op3SwitchAddValue                        vs
op3SwitchAddValue []                                                      = []

{-# INLINE op4MergeAddValue #-}
op4MergeAddValue :: [Token] -> [Token]
op4MergeAddValue vs = case splitPPlus vs of
  ([],[])     -> []
  ([],v:vs)   -> v             : op4MergeAddValue vs
  (ps,  vs)   -> sortPPlus ps ++ op4MergeAddValue vs

{-# INLINE sortPPlus #-}
sortPPlus :: [Token] -> [Token]
sortPPlus ps = go (sortPs ps)
  where sortPs = L.sortBy cp
        cp (AddValue i _) (AddValue j _)  | i == j                       = EQ
                                          | i == 0 || ( i < j && j /= 0) = LT
                                          | otherwise                    = GT
        go (AddValue i m:AddValue j n:vs) | i == j                       =                go (AddValue i (m + n):vs)
                                          | otherwise                    = AddValue i m : go (AddValue j      n :vs)
        go                         (p:vs)                                =            p : go                     vs
        go []                                                            = []

splitPPlus :: [Token] -> ([Token],[Token])
splitPPlus (p@(AddValue _ _):vs) = let (ps,vs') = splitPPlus vs in (p:ps,vs')
splitPPlus vs                    = ([],vs)


bytecode :: BFConfig -> C.ByteString -> Either String [Token]
bytecode config = parseOnly (p <* B.endOfInput)
        where p = if bfOptimize config then parseExpr else parseExpr'

type Memory = (Int,U.Vector Byte)

runTokens :: BFConfig -> Memory -> [Token] -> IO Memory
runTokens config cxt (t:ts) = do
  when (bfDebugs config) (print cxt)
  cxt' <- runToken config cxt t
  runTokens config cxt' ts
runTokens _      cxt     [] = return cxt


{-# INLINE runLoop #-}
runLoop :: BFConfig -> Memory -> [Token] -> IO Memory
runLoop c context tokens = case getInt (snd context) (fst context) of
                  0      -> return context
                  _      -> do 
                    context' <- L.foldl' (foldToken c) (return context) tokens
                    runLoop c context' tokens

{-# INLINE foldToken #-}
foldToken :: BFConfig -> IO Memory -> Token -> IO Memory
foldToken c context token = do {cxt' <- context ; runToken c cxt' token}

runToken :: BFConfig -> Memory -> Token -> IO Memory
runToken c context    (Loop   tokens)                 = runLoop c context tokens
runToken _ context    (Empty)                         = return context
runToken _ (i,vector) (Assert      j)     | jv == 0   = return (i, vector)
                                          | otherwise = error "Infinite Loop"
                                          where jv    = getInt vector (i + j)
runToken _ (i,vector) (Move        j)                 = return (i + j,vector)
runToken _ context    (Scan        j)     | j == 0    = return context
                                          | otherwise = return (go j context)
                                          where go j (i,vector) | vi == 0     = (i,vector)
                                                                | otherwise   = go j (i + j,vector)
                                                                where vi = getInt vector i
runToken _ (i,vector) (AddValue    j m)   | m == 0    = return (i, vector)
                                          | otherwise = return (i, addTo vector (i + j) m)
runToken _ (i,vector) (SetValue    j m)               = return (i, setTo vector (i + j) m)
runToken _ (i,vector) (SetValueIf  j m k) | v == 0    = return (i, vector)
                                          | otherwise = return (i, setTo vector (i + j) m)
                                          where v     = getInt vector (i + k)
runToken _ (i,vector) (CopyValue   j m k) | m == 0    = return (i, vector)
                                          | kv == 0   = return (i, vector)
                                          | otherwise = return (i, addTo vector (i + j) m')
                                          where kv    = getInt vector (i + k)
                                                m'    = m * kv
runToken _ (i,vector) (Output      j   k) = pch jv   >> return (i,vector)
                                          where jv    = getInt vector (i + j)
                                                pch c = C.putStr $ BB.replicate k jv
runToken _ (i,vector) (Input       j)     = fmap (\c -> (i,setTo vector (i + j) (BS.c2w c))) getChar


{-# INLINE getInt #-}
getInt :: U.Vector Byte -> Int -> Byte
getInt vector = (vector U.!)

{-# INLINE addTo #-}
addTo :: U.Vector Byte -> Int -> Byte -> U.Vector Byte 
addTo vector i m = setTo vector i m'
    where m'     = m + getInt vector i

{-# INLINE setTo #-}
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

exec :: BFConfig -> C.ByteString -> IO ()
exec config str  =  case bytecode config str of
  Left  err      -> print err
  Right tokens   -> do
    when (bfShow config) (C.putStrLn . toStr $ tokens)
    unless (bfDryrun config) $ do
      mem <- runTokens config (0,U.generate (bfSize config) (const 0)) tokens
      when (bfVerbose config) (print mem)

