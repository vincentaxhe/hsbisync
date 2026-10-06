{-# LANGUAGE BangPatterns #-}

-- | 协议消息层：**一行一个规范 JSON**，全程 ByteString（UTF-8）。
--
--   为什么不用 `json`（Text.JSON）：它把整条消息 `T.unpack` 成 `String`，再逐字段
--   建 `JSValue`/`String`（每个 Char 一个堆单元）。实测 5 万条状态：88.8 B/条、
--   解码 ≈52 µs/条 —— 同一个 `[FileState]` 换掉这层就能快一个数量级。
--
--   这一层的目标就是"避免反复转换"：
--     * 字符串始终是 `ByteString`（UTF-8）；解码时是**输入的切片**（零拷贝）
--     * 数字是 `Integer`：编码走 `Builder.integerDec`（不是 `show`），
--       解码走手写数字扫描（不是 `reads`）
--     * 输出**规范**：调用方给的键顺序原样、无多余空白、只在必要时转义
--       （同一份数据 → 同一串字节，方便 diff 与日志比对）
--
--   形状约定（用户 2026-09-22 定）：
--     * 控制消息 = 一个 JSON **对象**（键值对，可读）
--     * 批量数据 = JSON **数组**，元素是数组（位置式，省掉每条重复的键名）
module Msg
    ( Value(..)
    , encode, encodeLine, decode, decodeStrict
    , int, str, bytes, arr, obj, cmd, raw
    , intBuilder, strBuilder
    , field, fieldInt, fieldStr, fieldBools, asInt, asStr, asArr, asBool
    ) where

import Data.Bits ((.&.), shiftR)
import Data.Char (chr)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T (pack, singleton, unpack)
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8With)
import qualified Data.Text.Encoding.Error as TEE (lenientDecode)

-- ============================================================================
-- 消息值
-- ============================================================================

data Value
    = JNull
    | JBool !Bool
    | JInt  !Integer
    | JStr  !BS.ByteString          -- ^ UTF-8 字节（不解码成 String）
    | JArr  [Value]
    | JObj  [(BS.ByteString, Value)]
    | JRaw  !B.Builder              -- ^ 已经编码好的片段（批量数据走流式路径，不建中间 Value）

-- | 调试用：显示为规范 JSON 文本
instance Show Value where
    show = B8.unpack . BL.toStrict . encode

-- ============================================================================
-- 构造（应用层就用这几个）
-- ============================================================================

int :: Integer -> Value
int = JInt

-- | 字符串字面量（UTF-8 编码一次）
str :: String -> Value
str = JStr . T.encodeUtf8 . T.pack

-- | 已经是 UTF-8 字节的字符串（文件名走这个，避免再转 String）
bytes :: BS.ByteString -> Value
bytes = JStr

arr :: [Value] -> Value
arr = JArr

-- | 对象；键顺序即输出顺序（规范输出的一部分）。
--
--   键按 **UTF-8** 编码。以前这里是 `B8.pack`（Char8，会把非 ASCII 截断），
--   现有调用方全是 @"cmd"@ 这种 ASCII 字面量所以看不出问题；但键还可能来自**数据**
--   （例如 state 响应的 @rmdir@ 用文件名做键），那必须编成真正的 UTF-8。
obj :: [(String, Value)] -> Value
obj = JObj . map (\(k, v) -> (T.encodeUtf8 (T.pack k), v))

-- | 请求：@{"cmd":<name>, 其它字段…}@
cmd :: String -> [(String, Value)] -> Value
cmd name kvs = obj (("cmd", str name) : kvs)

-- | 已经编码好的 JSON 片段（流式路径）：给批量数据用，
--   **不建中间 Value 树**，直接拼进输出的 Builder。
raw :: B.Builder -> Value
raw = JRaw

-- | 数字 / 字符串的底层编码原语（配合 'raw' 手写流式片段）
intBuilder :: Integer -> B.Builder
intBuilder = B.integerDec

strBuilder :: BS.ByteString -> B.Builder
strBuilder s = B.word8 34 <> encStr s <> B.word8 34

-- ============================================================================
-- 取值
-- ============================================================================

field :: String -> Value -> Maybe Value
field k (JObj kvs) = lookup (B8.pack k) kvs
field _ _          = Nothing

fieldInt :: String -> Value -> Maybe Integer
fieldInt k v = field k v >>= asInt

-- | 字符串字段（UTF-8 宽容解码；用于 cmd/kind/error/路径这类小字符串）
fieldStr :: String -> Value -> Maybe String
fieldStr k v = field k v >>= asStr >>= Just . T.unpack . T.decodeUtf8With TEE.lenientDecode

-- | 「路径 → 布尔」的**有序**列表字段：@[{"相对路径":true}, …]@
--   （例如 state 响应里的 @rmdir@）。
--
--   为什么不是一个大对象：**JSON 对象在语义上是无序的**，而这里的顺序有意义
--   （深 → 浅）。数组元素有序，所以用「单个键值对」组成的数组。
--   缺失 / 形状不对 → 空列表；不是「恰好一个键、值是布尔」的元素**跳过**，
--   不让一项毁掉整批。
fieldBools :: String -> Value -> [(String, Bool)]
fieldBools k v = case field k v of
    Just (JArr xs) -> [ (T.unpack (T.decodeUtf8With TEE.lenientDecode kb), ok)
                      | JObj [(kb, ov)] <- xs
                      , Just ok         <- [asBool ov] ]
    _              -> []

asInt :: Value -> Maybe Integer
asInt (JInt n) = Just n
asInt _        = Nothing

asStr :: Value -> Maybe BS.ByteString
asStr (JStr s) = Just s
asStr _        = Nothing

asBool :: Value -> Maybe Bool
asBool (JBool b) = Just b
asBool _         = Nothing

asArr :: Value -> Maybe [Value]
asArr (JArr xs) = Just xs
asArr _         = Nothing

-- ============================================================================
-- 编码（规范）
-- ============================================================================

encode :: Value -> BL.ByteString
encode = B.toLazyByteString . enc

-- | 协议用：编码 + 换行（一行一条消息）
encodeLine :: Value -> BL.ByteString
encodeLine v = encode v <> BL.singleton 10

enc :: Value -> B.Builder
enc (JRaw b)    = b
enc JNull       = B.byteString (B8.pack "null")
enc (JBool b)   = B.byteString (B8.pack (if b then "true" else "false"))
enc (JInt n)    = B.integerDec n
enc (JStr s)    = B.word8 34 <> encStr s <> B.word8 34
enc (JArr xs)   = B.word8 91 <> commaB (map enc xs) <> B.word8 93
enc (JObj kvs)  = B.word8 123 <> commaB [ enc (JStr k) <> B.word8 58 <> enc v | (k, v) <- kvs ] <> B.word8 125

commaB :: [B.Builder] -> B.Builder
commaB []       = mempty
commaB [x]      = x
commaB (x : xs) = x <> B.word8 44 <> commaB xs

-- | 字符串转义：单趟扫描，不需要转义时整段 `byteString`（零拷贝快路径）
encStr :: BS.ByteString -> B.Builder
encStr = go
  where
    go b = case BS.span (not . needsEsc) b of
        (plain, rest)
            | BS.null rest -> B.byteString plain
            | otherwise -> B.byteString plain <> escByte (BS.head rest) <> go (BS.tail rest)
    needsEsc c = c == 34 || c == 92 || c < 0x20

escByte :: Word8 -> B.Builder
escByte 34 = B.byteString (B8.pack "\\\"")        -- \"
escByte 92 = B.byteString (B8.pack "\\\\")        -- \\
escByte 10 = B.byteString (B8.pack "\\n")         -- \n
escByte 13 = B.byteString (B8.pack "\\r")         -- \r
escByte 9  = B.byteString (B8.pack "\\t")         -- \t
escByte c  = B.string8 ("\\u00" ++ hex2 c)     -- 其余控制字符

hex2 :: Word8 -> String
hex2 c = [hexDigit (c `shiftR` 4), hexDigit (c .&. 15)]
  where
    hexDigit n | n < 10    = chr (fromIntegral n + 48)
               | otherwise = chr (fromIntegral n + 87)   -- a-f

-- ============================================================================
-- 解码（严格扫描；字符串是输入切片，零拷贝）
-- ============================================================================

-- | 解析一条消息；多余的非空白内容会被拒绝（"一行一条"）
decode :: BL.ByteString -> Either String Value
decode = decodeStrict . BL.toStrict

decodeStrict :: BS.ByteString -> Either String Value
decodeStrict bs = do
    (v, st') <- pValue (St bs 0)
    st'' <- ws st'
    if stI st'' == BS.length bs
        then Right v
        else Left ("trailing junk at offset " ++ show (stI st''))

data St = St { stB :: !BS.ByteString, stI :: !Int }

peek :: St -> Maybe Word8
peek st
    | stI st < BS.length (stB st) = Just (BS.index (stB st) (stI st))
    | otherwise                   = Nothing

skip :: Int -> St -> Either String St
skip n st
    | stI st + n <= BS.length (stB st) = Right st { stI = stI st + n }
    | otherwise                        = Left "unexpected end of input"

expect :: Word8 -> St -> Either String St
expect c st = case peek st of
    Just c' | c' == c -> skip 1 st
    Just c'           -> Left ("expected " ++ show (chr (fromIntegral c)) ++ ", got " ++ show (chr (fromIntegral c')))
    Nothing           -> Left "unexpected end of input"

ws :: St -> Either String St
ws st = case peek st of
    Just c | c == 32 || c == 9 || c == 10 || c == 13 -> ws =<< skip 1 st
    _ -> Right st

pValue :: St -> Either String (Value, St)
pValue st = case peek st of
    Nothing -> Left "unexpected end of input"
    Just c
        | c == 91  -> pArray st                       -- [
        | c == 123 -> pObject st                      -- {
        | c == 34  -> pString st                      -- "
        | c == 116 -> literal (B8.pack "true")  (JBool True) st          -- true
        | c == 102 -> literal (B8.pack "false") (JBool False) st         -- false
        | c == 110 -> literal (B8.pack "null")  JNull st                 -- null
        | c == 45 || (c >= 48 && c <= 57) -> pInt st
        | otherwise -> Left ("unexpected byte " ++ show c)

literal :: BS.ByteString -> Value -> St -> Either String (Value, St)
literal word v st
    | BS.isPrefixOf word (BS.drop (stI st) (stB st)) = Right (v, st { stI = stI st + BS.length word })
    | otherwise = Left ("invalid literal at offset " ++ show (stI st))

pInt :: St -> Either String (Value, St)
pInt st = do
    (neg, st1) <- case peek st of
        Just 45 -> Right (True, st { stI = stI st + 1 })
        _       -> Right (False, st)
    let (n, st2) = digits st1
    -- 注意：0 是合法数字，"没吃到数字"要靠偏移量没前进判断（不能拿值等于 0 当标志）
    if stI st2 == stI st1
        then Left "invalid number"
        else Right (JInt (if neg then negate n else n), st2)

digits :: St -> (Integer, St)
digits st = go 0 (stI st)
  where
    len = BS.length (stB st)
    go !acc !i
        | i < len, c <- BS.index (stB st) i, c >= 48, c <= 57 = go (acc * 10 + fromIntegral (c - 48)) (i + 1)
        | otherwise = (acc, st { stI = i })

pString :: St -> Either String (Value, St)
pString st0 = do
    st1 <- expect 34 st0
    let bs = stB st1
        start = stI st1
        len = BS.length bs
        -- 快路径：整串没有转义 → 直接一段切片
        goFast !i
            | i >= len = Left "unterminated string"
            | otherwise = case BS.index bs i of
                34 -> Right (JStr (BS.take (i - start) (BS.drop start bs)), st1 { stI = i + 1 })
                92 -> slow i
                _  -> goFast (i + 1)
        -- 慢路径：把"普通片段"与"转义产物"按顺序拼起来（chunkStart 跟随扫描位置前移）
        slow !i = do
            (chunks, st') <- collect i start []
            Right (JStr (BS.concat (reverse chunks)), st')
        collect !i !chunkStart acc
            | i >= len = Left "unterminated string"
            | otherwise = case BS.index bs i of
                34 -> Right (BS.take (i - chunkStart) (BS.drop chunkStart bs) : acc, st1 { stI = i + 1 })
                92 -> do
                    (piece, st2) <- escape (st1 { stI = i + 1 })
                    collect (stI st2) (stI st2)
                            (piece : BS.take (i - chunkStart) (BS.drop chunkStart bs) : acc)
                _  -> collect (i + 1) chunkStart acc
    goFast start

-- | 解析一个转义序列，返回它对应的 UTF-8 字节
escape :: St -> Either String (BS.ByteString, St)
escape st = case peek st of
    Nothing -> Left "unexpected end of input in escape"
    Just c
        | c == 34 -> skip 1 st >>= \s -> Right (BS.singleton 34, s)
        | c == 92 -> skip 1 st >>= \s -> Right (BS.singleton 92, s)
        | c == 47 -> skip 1 st >>= \s -> Right (BS.singleton 47, s)
        | c == 110 -> skip 1 st >>= \s -> Right (BS.singleton 10, s)
        | c == 114 -> skip 1 st >>= \s -> Right (BS.singleton 13, s)
        | c == 116 -> skip 1 st >>= \s -> Right (BS.singleton 9, s)
        | c == 98 -> skip 1 st >>= \s -> Right (BS.singleton 8, s)
        | c == 102 -> skip 1 st >>= \s -> Right (BS.singleton 12, s)
        | c == 117 -> do
            st1 <- skip 1 st
            (cp, st2) <- hex4 st1
            if cp >= 0xD800 && cp <= 0xDFFF
                then Left "surrogate escape is not supported"
                else Right (T.encodeUtf8 (T.singleton (chr cp)), st2)
        | otherwise -> Left ("invalid escape \\" ++ [chr (fromIntegral c)])

hex4 :: St -> Either String (Int, St)
hex4 st = go (0 :: Int) (0 :: Int) st
  where
    go !acc !k s
        | k == 4 = Right (acc, s)
        | otherwise = case peek s of
            Just c | Just d <- hexVal c -> go (acc * 16 + d) (k + 1) =<< skip 1 s
            _ -> Left "invalid \\u escape"

hexVal :: Word8 -> Maybe Int
hexVal c
    | c >= 48 && c <= 57  = Just (fromIntegral c - 48)
    | c >= 97 && c <= 102 = Just (fromIntegral c - 87)
    | c >= 65 && c <= 70  = Just (fromIntegral c - 55)
    | otherwise           = Nothing

pArray :: St -> Either String (Value, St)
pArray st0 = do
    st1 <- ws =<< expect 91 st0
    case peek st1 of
        Just 93 -> Right (JArr [], st1 { stI = stI st1 + 1 })
        _ -> do
            (v0, st2) <- pValue st1
            (rest, st3) <- go st2 []
            Right (JArr (v0 : rest), st3)
  where
    go st acc = do
        st' <- ws st
        case peek st' of
            Just 44 -> do
                st'' <- ws =<< skip 1 st'
                (v, st''') <- pValue st''
                go st''' (v : acc)
            Just 93 -> Right (reverse acc, st' { stI = stI st' + 1 })
            _       -> Left "expected ',' or ']'"

pObject :: St -> Either String (Value, St)
pObject st0 = do
    st1 <- ws =<< expect 123 st0
    case peek st1 of
        Just 125 -> Right (JObj [], st1 { stI = stI st1 + 1 })
        _ -> do
            (k0, v0, st2) <- pair st1
            (rest, st3) <- go st2 []
            Right (JObj ((k0, v0) : rest), st3)
  where
    pair st = do
        (kv, st') <- pString st
        k <- case kv of
            JStr s -> Right s
            _      -> Left "object key must be a string"
        st'' <- ws =<< expect 58 st'
        st''' <- ws st''
        (v, st4) <- pValue st'''
        Right (k, v, st4)
    go st acc = do
        st' <- ws st
        case peek st' of
            Just 44 -> do
                st'' <- ws =<< skip 1 st'
                (k, v, st''') <- pair st''
                go st''' ((k, v) : acc)
            Just 125 -> Right (reverse acc, st' { stI = stI st' + 1 })
            _        -> Left "expected ',' or '}'"
