{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}

module Language.BrainFuck(exec,parseExpr,parseExpr',run,BFConfig(..)) where

import           Control.Monad(unless)
import           Control.Monad.ST(runST)
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Monoid((<>))
import           Data.Word(Word8)
import           Data.Char(ord,chr,isControl,isSpace,showLitChar)
import           Data.Attoparsec.ByteString as B
import qualified Data.List             as L  
import qualified Data.Vector.Unboxed   as U
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import qualified Data.ByteString       as BB


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


parse' :: BFConfig -> C.ByteString -> Either String [Token]
parse' config = parseOnly (p <* B.endOfInput)
      where p = if bfOptimize config then parseExpr else parseExpr'

type Memory = (Int, U.Vector Byte, BFConfig)
type ByteCode = Memory -> IO Memory


{-# INLINE bcInput #-}
bcInput :: Int -> ByteCode
bcInput      j     (i,vector,config) = fmap (\c -> (i, setTo vector (i + j) (BS.c2w c),config)) getChar

{-# INLINE bcOutput #-}
bcOutput :: Int -> Int -> ByteCode
bcOutput     j   k (i,vector,config) = let jv = getInt vector (i + j) in do 
                                                   C.putStr $ BB.replicate k jv
                                                   return (i,vector,config)

{-# INLINE bcCopyValue #-}
bcCopyValue ::  Int -> Byte -> Int -> ByteCode
bcCopyValue  j m k (i,vector,config) | m == 0    = return (i,vector,config)
                                     | otherwise = return (i, copyTo vector (i + j) m (i + k),config)

{-# INLINE bcSetValueIf #-}
bcSetValueIf ::  Int -> Byte -> Int -> ByteCode
bcSetValueIf j m k (i,vector,config)             = return (i, setIf vector (i + j) m (i + k),config)

{-# INLINE bcSetValue #-}
bcSetValue :: Int -> Byte -> ByteCode
bcSetValue   j m   (i,vector,config)             = return (i, setTo vector (i + j) m,config)

{-# INLINE bcAddValue #-}
bcAddValue :: Int -> Byte -> ByteCode
bcAddValue   j m   (i,vector,config) | m == 0    = return (i, vector,config)
                                     | otherwise = return (i, addTo vector (i + j) m,config)

{-# INLINE bcScan #-}
bcScan :: Int -> ByteCode
bcScan       j     (i,vector,config) | j == 0    = return (i,vector,config)
                                     | otherwise = return (scanTo vector i j,vector,config)

{-# INLINE bcMove #-}
bcMove :: Int -> ByteCode
bcMove       j     (i,vector,config) =             return (i + j,vector,config)

{-# INLINE bcAssert #-}
bcAssert :: Int -> ByteCode
bcAssert     j     (i,vector,config) | m == 0    = return (i, vector,config)
                                     | otherwise = error "Infinite Loop"
                                     where m     = getInt vector (i + j)


{-# INLINE loopByteCode #-}
loopByteCode :: [ByteCode] -> ByteCode
loopByteCode bytecodes mem@(i,vector,_) = if   getInt vector i /= 0 
                                          then seqByteCode bytecodes mem >>= loopByteCode bytecodes 
                                          else return mem

{-# INLINE seqByteCode #-}
seqByteCode :: [ByteCode] -> ByteCode
seqByteCode bcs mem = L.foldl' (\m bc -> m >>= bc) (return mem) bcs

{-# INLINE generateByteCode #-}
generateByteCode :: [Token] -> [ByteCode]
generateByteCode (Loop ts         :tokens) = loopByteCode (generateByteCode ts) : generateByteCode tokens
generateByteCode (CopyValue  j m k:tokens) = bcCopyValue  j m k                 : generateByteCode tokens
generateByteCode (SetValueIf j m k:tokens) = bcSetValueIf j m k                 : generateByteCode tokens
generateByteCode (SetValue   j m  :tokens) = bcSetValue   j m                   : generateByteCode tokens
generateByteCode (AddValue   j m  :tokens) = bcAddValue   j m                   : generateByteCode tokens
generateByteCode (Scan       j    :tokens) = bcScan       j                     : generateByteCode tokens
generateByteCode (Move       j    :tokens) = bcMove       j                     : generateByteCode tokens
generateByteCode (Assert     j    :tokens) = bcAssert     j                     : generateByteCode tokens
generateByteCode (Empty           :tokens) =                                      generateByteCode tokens
generateByteCode (Input      j    :tokens) = bcInput      j                     : generateByteCode tokens
generateByteCode (Output     j   k:tokens) = bcOutput     j   k                 : generateByteCode tokens
generateByteCode [] = []


{-# INLINE toByteCode #-}
toByteCode :: BFConfig -> C.ByteString -> Either String ByteCode
toByteCode config input = case parse' config input of
           Left  err    -> Left  err
           Right tokens -> Right . seqByteCode . generateByteCode $ tokens

{-# INLINE getInt #-}
getInt :: U.Vector Byte -> Int -> Byte
getInt vector i= runST $ do U.unsafeThaw vector >>= \v -> MU.read v i 

{-# INLINE copyTo #-}
copyTo :: U.Vector Byte -> Int -> Byte -> Int -> U.Vector Byte 
copyTo vector i m j = runST $ do
    v  <- U.unsafeThaw vector
    mj <- MU.read v j
    let m' = mj * m
    when (m' /= 0) (MU.modify v (+m') i)
    U.unsafeFreeze v

{-# INLINE setIf #-}
setIf :: U.Vector Byte -> Int -> Byte -> Int -> U.Vector Byte 
setIf vector i m j = runST $ do
  v <- U.unsafeThaw vector
  vj <- MU.read v j
  when (vj /= 0) (MU.write v i m)
  U.unsafeFreeze v

{-# INLINE scanTo #-}
scanTo :: U.Vector Byte -> Int -> Int -> Int
scanTo vector i j = runST $ do
  U.unsafeThaw vector >>= loopScan i j
  where loopScan i j v = do
           vi <- MU.read v i
           if vi == 0 then return i
                      else loopScan (i+j) j v

{-# INLINE addTo #-}
addTo :: U.Vector Byte -> Int -> Byte -> U.Vector Byte 
addTo vector i m = runST $ do
    v  <- U.unsafeThaw vector
    MU.modify v (+m) i
    U.unsafeFreeze v

{-# INLINE setTo #-}
setTo :: U.Vector Byte -> Int -> Byte -> U.Vector Byte
setTo vector i m = runST $ do
    v  <- U.unsafeThaw vector
    MU.write v i m
    U.unsafeFreeze v


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
exec config str  =  case parse' config str of
  Left  err      -> print err
  Right tokens   -> do
    when (bfShow config) (C.putStrLn . toStr $ tokens)
    unless (bfDryrun config) $ do
      mem <- (seqByteCode . generateByteCode $ tokens) (0, U.generate (bfSize config) (const 0),config)
      when (bfVerbose config) (print mem)

