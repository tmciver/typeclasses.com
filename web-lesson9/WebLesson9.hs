{-# OPTIONS_GHC -Wall -fno-warn-unused-imports -fdefer-typed-holes #-}

{-# LANGUAGE TypeApplications #-}

module WebLesson9 where

-- attoparsec
import           Data.Attoparsec.ByteString.Char8 (Parser, (<?>))
import qualified Data.Attoparsec.ByteString.Char8 as P

-- base
import           Control.Applicative (many)
import           Control.Concurrent  (threadDelay)
import           Control.Monad       (unless)
import qualified Data.Char           as Char
import qualified Data.List           as List
import           Data.Word           (Word8)
import           Numeric             (showHex, showInt)

-- bytestring
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BSB
import qualified Data.ByteString.Char8      as ASCII
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LASCII

-- contravariant
import Data.Functor.Contravariant
    (Equivalence (..), contramap, defaultEquivalence)

-- network
import           Network.Socket                 (Socket)
import qualified Network.Socket.ByteString      as Socket
import qualified Network.Socket.ByteString.Lazy as LSocket

-- network-simple
import qualified Network.Simple.TCP as NS


--------------------------------------------------------------------------------
--  Lesson 1: Sockets
--------------------------------------------------------------------------------

{- In lesson 1, we introduced sockets and defined this foundation for all of the
servers we're going to write. The argument 'f' is how we specify what our server
will do each time a new client opens a connection. The connection is represented
by a *socket*, to which we can read and write byte strings. -}

server :: (Socket -> IO ()) -> IO a
server f =
    NS.serve NS.HostAny "8000" $ \(socket, _socketAddress) ->
        f socket


--------------------------------------------------------------------------------
--  Lesson 2: Say Hello to RFC 7230
--------------------------------------------------------------------------------

{- In lesson 2, we wrote a function called 'sayHello' which writes an HTTP
response to a socket. When we use this as the argument to the 'server' function
from lesson 1, we get a very basic working web server. -}

helloResponse_byteString :: BS.ByteString
helloResponse_byteString =
    asciiLines
        [ "HTTP/1.1 200 OK"
        , "Content-Type: text/plain; charset=us-ascii"
        , "Content-Length: 7"
        , ""
        , "Hello!\n"
        ]

asciiLines :: [String] -> BS.ByteString
asciiLines xs =
    ASCII.pack (List.intercalate "\r\n" xs)

sayHello :: Socket -> IO ()
sayHello socket =
    Socket.sendAll socket helloResponse_byteString


--------------------------------------------------------------------------------
--  Lesson 3: HTTP types
--------------------------------------------------------------------------------

{- In lesson 3, we defined types to represent HTTP requests and responses. These
types followed directly from reading the protocol specification, and so we have
included comments indicating what section of the document they come from. -}

-- RFC 7230, section 1.2: Syntax Notation

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
                                                       deriving (Eq, Show,
                                                         Bounded, Enum, Ord)

-- RFC 7230, section 2.6: Protocol Versioning

data HttpVersion = HttpVersion Digit Digit             deriving (Eq, Show)

-- RFC 7230, section 3: Message Format

data Request = Request RequestLine [HeaderField] (Maybe MessageBody)
                                                       deriving (Eq, Show)

data Response = Response StatusLine [HeaderField] (Maybe MessageBody)
                                                       deriving (Eq, Show)

-- RFC 7230, section 3.1.1: Request Line

data RequestLine = RequestLine Method RequestTarget HttpVersion
                                                       deriving (Eq, Show)

newtype Method = Method BS.ByteString                  deriving (Eq, Show)

-- RFC 7230, section 3.1.2: Status Line

data StatusLine = StatusLine HttpVersion StatusCode ReasonPhrase
                                                       deriving (Eq, Show)

data StatusCode = StatusCode Digit Digit Digit         deriving (Eq, Show)

newtype ReasonPhrase = ReasonPhrase BS.ByteString      deriving (Eq, Show)

-- RFC 7230, section 3.2: Header Fields

data HeaderField = HeaderField FieldName FieldValue    deriving (Eq, Show)

newtype FieldName = FieldName BS.ByteString            deriving (Eq, Show)

newtype FieldValue = FieldValue BS.ByteString          deriving (Eq, Show)

-- RFC 7230, section 3.3: Message Body

newtype MessageBody = MessageBody LBS.ByteString       deriving (Eq, Show)

-- RFC 7230, section 5.3: Request Target

newtype RequestTarget = RequestTarget BS.ByteString    deriving (Eq, Show)


--------------------------------------------------------------------------------
--  Lesson 4: Response encoding
--------------------------------------------------------------------------------

{- In lesson 4, we wrote a function for encoding our abstract representation of
a Response as a byte string suitable for transmission over a network according
to the HTTP protocol. -}

-- RFC 7230, section 1.2: Syntax Notation

encodeDigit :: Digit -> BSB.Builder
encodeDigit d = BSB.string7 [digitChar d]

digitChar :: Digit -> Char
digitChar d =
    case d of
        D0 -> '0'
        D1 -> '1'
        D2 -> '2'
        D3 -> '3'
        D4 -> '4'
        D5 -> '5'
        D6 -> '6'
        D7 -> '7'
        D8 -> '8'
        D9 -> '9'

-- RFC 7230, section 2.6: Protocol Versioning

encodeHttpVersion :: HttpVersion -> BSB.Builder
encodeHttpVersion (HttpVersion x y) =
    BSB.string7 "HTTP/" <> encodeDigit x <> BSB.string7 "." <> encodeDigit y

-- RFC 7230, section 3: Message Format

encodeResponse :: Response -> BSB.Builder
encodeResponse (Response statusLine headerFields bodyMaybe) =
    encodeStatusLine statusLine
    <> encodeHeaderFieldList headerFields
    <> BSB.string7 "\r\n"
    <> foldMap encodeMessageBody bodyMaybe

encodeHeaderFieldList :: [HeaderField] -> BSB.Builder
encodeHeaderFieldList headerFields =
    foldMap (\x -> encodeHeaderField x <> BSB.string7 "\r\n") headerFields

-- RFC 7230, section 3.1.2: Status Line

encodeStatusLine :: StatusLine -> BSB.Builder
encodeStatusLine (StatusLine httpVersion statusCode reasonPhrase) =
    encodeHttpVersion httpVersion <> BSB.string7 " "
    <> encodeStatusCode statusCode <> BSB.string7 " "
    <> encodeReasonPhrase reasonPhrase <> BSB.string7 "\r\n"

encodeStatusCode :: StatusCode -> BSB.Builder
encodeStatusCode (StatusCode x y z) =
    encodeDigit x <> encodeDigit y <> encodeDigit z

encodeReasonPhrase :: ReasonPhrase -> BSB.Builder
encodeReasonPhrase (ReasonPhrase x) = BSB.byteString x

-- RFC 7230, section 3.2: Header Fields

encodeHeaderField :: HeaderField -> BSB.Builder
encodeHeaderField (HeaderField (FieldName x) (FieldValue y)) =
    BSB.byteString x <> BSB.string7 ": " <> BSB.byteString y

-- RFC 7230, section 3.3: Message Body

encodeMessageBody :: MessageBody -> BSB.Builder
encodeMessageBody (MessageBody x) = BSB.lazyByteString x


--------------------------------------------------------------------------------
--  Lesson 5: Constructing and using Responses
--------------------------------------------------------------------------------

{- In lesson 5, we made a simple server that uses the Response type. Then we
started to practice constructing Response values: first by applying all of the
data constructors directly, and then by using some functions that we wrote to
make it a bit more convenient. -}

staticResponseServer :: Response -> IO ()
staticResponseServer response =
  server $ \socket ->
    LSocket.sendAll socket
      (BSB.toLazyByteString (encodeResponse response))

------------------------------------------

helloResponse_withMoreTypes :: Response
helloResponse_withMoreTypes =
    Response statusLine headerFields bodyMaybe

  where
    statusLine = StatusLine httpVersion statusCode reasonPhrase
    httpVersion = HttpVersion D1 D1
    statusCode = StatusCode D2 D0 D0
    reasonPhrase = ReasonPhrase (ASCII.pack "OK")

    headerFields = [h1, h2]
    h1 = HeaderField (FieldName (ASCII.pack "Content-Type"))
                     (FieldValue (ASCII.pack "text/plain; charset=us-ascii"))
    h2 = HeaderField (FieldName (ASCII.pack "Content-Length"))
                     (FieldValue (ASCII.pack "7"))

    bodyMaybe = Just (MessageBody (LASCII.pack "Hello!\n"))

------------------------------------------

http_1_1 :: HttpVersion
http_1_1 = HttpVersion D1 D1

status200 :: StatusCode
status200 = StatusCode D2 D0 D0

reasonOK :: ReasonPhrase
reasonOK = ReasonPhrase (ASCII.pack "OK")

contentLengthHeader :: Integral a => a -> HeaderField
contentLengthHeader contentLength =
    HeaderField
        (FieldName (ASCII.pack "Content-Length"))
        (FieldValue (ASCII.pack (showInt contentLength "")))

contentTypeHeader :: String -> HeaderField
contentTypeHeader contentType =
    HeaderField
        (FieldName (ASCII.pack "Content-Type"))
        (FieldValue (ASCII.pack contentType))

plainTextAsciiHeader :: HeaderField
plainTextAsciiHeader = contentTypeHeader "text/plain; charset=us-ascii"

asciiMessageBody :: String -> MessageBody
asciiMessageBody x =
    MessageBody (LASCII.pack x)

------------------------------------------

helloResponse_moreConveniently :: Response
helloResponse_moreConveniently =
    Response statusLine headerFields bodyMaybe
  where
    statusLine = StatusLine http_1_1 status200 reasonOK
    headerFields = [plainTextAsciiHeader, contentLengthHeader (7 :: Integer)]
    bodyMaybe = Just (asciiMessageBody "Hello!\n")


--------------------------------------------------------------------------------
--  Lesson 6: Content length
--------------------------------------------------------------------------------

{- In lesson 6, we wrote the 'alterHeader' function to manipulate lists of HTTP
headers. Since field names are case-insensitive, we defined a case-insensitive
Equivalence for them using 'contramap'. We then used 'alterHeader' to write a
function that adds an appropriate "Content-Length" header to a Response. -}

alterHeader :: (Maybe FieldValue -> Maybe FieldValue)
            -> FieldName
            -> [HeaderField] -> [HeaderField]
alterHeader f name headerFields =
    case headerFields of
        [] ->
            case (f Nothing) of
                Nothing -> []
                Just y  -> [HeaderField name y]

        (HeaderField x y : xys) | x ~= name ->
            case (f (Just y)) of
                Nothing -> xys
                Just y' -> HeaderField name y' : xys

        (xy : xys) ->
            xy : alterHeader f name xys

  where
    (~=) :: FieldName -> FieldName -> Bool
    a ~= b = getEquivalence fieldNameEquivalence a b

------------------------------------------

fieldNameEquivalence :: Equivalence FieldName
fieldNameEquivalence =
    contramap f (defaultEquivalence @String)
  where
    f :: FieldName -> String
    f (FieldName x) = Char.toLower <$> ASCII.unpack x

------------------------------------------

setContentLengthHeader :: Response -> Response
setContentLengthHeader (Response statusLine headerFields bodyMaybe) =
    Response statusLine headerFields' bodyMaybe
  where
    headerFields' = alterHeader f name headerFields
    name = FieldName (ASCII.pack "Content-Length")
    f _ =
        case bodyMaybe of
            Nothing -> Nothing
            Just (MessageBody lbs) ->
                Just (FieldValue (ASCII.pack (showInt (LBS.length lbs) "")))

------------------------------------------

helloResponse_base :: Response
helloResponse_base =
    Response
        (StatusLine http_1_1 status200 reasonOK)
        [plainTextAsciiHeader]
        (Just (asciiMessageBody "Hello!\n"))

helloResponse_withContentLength :: Response
helloResponse_withContentLength =
    setContentLengthHeader helloResponse_base

------------------------------------------

showResponseASCII :: Response -> String
showResponseASCII response =
    LASCII.unpack (BSB.toLazyByteString (encodeResponse response))

printResponseASCII :: Response -> IO ()
printResponseASCII response =
    putStrLn (showResponseASCII response)


--------------------------------------------------------------------------------
--  Lesson 7: Chunking the message body
--------------------------------------------------------------------------------

{- Lesson 7 introduced an alternative to the Content-Length header: A header
field that says "Transfer-Encoding: chunked" means we are going to segment the
message body in multiple smaller parts ("chunks"). We use 'alterHeader' again to
set this header field, and we write more encoding functions with BSB.Builder to
turn a regular body into a chunked body. -}

helloResponse_byteString_chunked :: BS.ByteString
helloResponse_byteString_chunked =
    asciiLines
        [ "HTTP/1.1 200 OK"
        , "Content-Type: text/plain; charset=us-ascii"
        , "Transfer-Encoding: chunked"
        , ""
        , "7"
        , "Hello!\n"
        , "1e"
        , "It is a lovely day today, no?\n"
        , "0\r\n\r\n"
        ]

sayHello_chunked :: Socket -> IO ()
sayHello_chunked socket =
    Socket.sendAll socket helloResponse_byteString_chunked

------------------------------------------

chunkResponse :: Response -> Response
chunkResponse (Response statusLine headerFields bodyMaybe) =
    case bodyMaybe of

        Nothing ->
            Response
                statusLine
                headerFields
                bodyMaybe

        Just messageBody ->
            Response
                statusLine
                (setChunkedHeader headerFields)
                (Just (chunkMessageBody messageBody))

setChunkedHeader :: [HeaderField] -> [HeaderField]
setChunkedHeader headerFields = alterHeader f name headerFields
  where
    name = FieldName (ASCII.pack "Transfer-Encoding")

    f oldValueMaybe =
        case oldValueMaybe of
            Nothing ->
                Just (FieldValue (ASCII.pack "chunked"))
            Just (FieldValue x) ->
                Just (FieldValue (x <> ASCII.pack ", chunked"))

chunkMessageBody :: MessageBody -> MessageBody
chunkMessageBody (MessageBody lbs) =
    MessageBody (BSB.toLazyByteString (encodeChunkedBody (LBS.toChunks lbs)))

encodeChunkedBody :: [BS.ByteString] -> BSB.Builder
encodeChunkedBody chunks =
    foldMap encodeChunk chunks -- all of the normal chunks
    <> encodeLastChunk         -- an additional chunk with size 0
    <> BSB.string7 "\r\n"      -- a final empty line

encodeLastChunk :: BSB.Builder
encodeLastChunk = BSB.string7 "0\r\n"

encodeChunk :: BS.ByteString -> BSB.Builder
encodeChunk bs =
    BSB.string7 (showHex (BS.length bs) "")
    <> BSB.string7 "\r\n"
    <> BSB.byteString bs
    <> BSB.string7 "\r\n"

------------------------------------------

helloResponse_chunked :: Response
helloResponse_chunked = chunkResponse helloResponse_base

helloResponse2_base :: Response
helloResponse2_base =
    Response
        (StatusLine http_1_1 status200 reasonOK)
        [ plainTextAsciiHeader ]
        (Just (MessageBody (LBS.fromChunks
            [ ASCII.pack "Hello!\n"
            , ASCII.pack "It is a lovely day today, no?\n"
            ])))


--------------------------------------------------------------------------------
--  Lesson 8: The never-ending response
--------------------------------------------------------------------------------

{- In lesson 8, we wrote an HTTP response body with infinite length, just to
show that we can. We tested it with 'staticResponseServer_slow' to make it
easier to watch it work. -}

staticResponseServer_slow :: Response -> IO ()
staticResponseServer_slow response =
  server $ \socket ->
    sendAll_slow socket
      (BSB.toLazyByteString (encodeResponse response))

sendAll_slow :: Socket -> LBS.ByteString -> IO ()
sendAll_slow socket lbs =
  do
    let
        (x, y) = LBS.splitAt 1 lbs

    LSocket.sendAll socket x

    if (LBS.null y)
        then
            return ()
        else
          do
            threadDelay 5000
            sendAll_slow socket y

infiniteResponse :: Response
infiniteResponse =
    chunkResponse (
        Response
            (StatusLine http_1_1 status200 reasonOK)
            [ plainTextAsciiHeader ]
            (Just infiniteMessageBody)
    )

infiniteMessageBody :: MessageBody
infiniteMessageBody =
    MessageBody (BSB.toLazyByteString (foldMap line [1 ..]))
  where
    line :: Integer -> BSB.Builder
    line i = BSB.string7 (showInt i "") <> BSB.char7 '\n'


--------------------------------------------------------------------------------
--  Lesson 9: Parsing requests
--------------------------------------------------------------------------------

-- RFC 7230, section 1.2: Syntax Notation

crlfParser :: Parser ()
crlfParser =
    do
        _ <- P.string (ASCII.pack "\r\n")
        return ()

    <?> "CRLF"

spParser :: Parser ()
spParser =
    do
        _ <- P.char ' '
        return ()

    <?> "SP"

isVchar :: Char -> Bool
isVchar c =
    c >= '!' && c <= '~'

digitParser :: Parser Digit
digitParser =
    do
        x <- P.satisfy P.isDigit

        case (charDigit x) of
            Just d  -> return d
            Nothing -> fail "Must be between 0 and 9"

    <?> "DIGIT"

charDigit :: Char -> Maybe Digit
charDigit x =
    case x of
        '0' -> Just D0
        '1' -> Just D1
        '2' -> Just D2
        '3' -> Just D3
        '4' -> Just D4
        '5' -> Just D5
        '6' -> Just D6
        '7' -> Just D7
        '8' -> Just D8
        '9' -> Just D9
        _   -> Nothing

-- RFC 7230, section 2.6: Protocol Versioning

httpVersionParser :: Parser HttpVersion
httpVersionParser =
    do
        _ <- httpNameParser
        _ <- P.char '/'
        x <- digitParser
        _ <- P.char '.'
        y <- digitParser

        return (HttpVersion x y)

    <?> "HTTP-version"

httpNameParser :: Parser ()
httpNameParser =
    do
        _ <- P.string (ASCII.pack "HTTP")
        return ()

    <?> "HTTP-name"

-- RFC 7230, section 3: Message Format

requestParser :: Parser Request
requestParser =
    do
        requestLine <- requestLineParser
        headerFields <- headerFieldListParser
        _ <- crlfParser
        bodyMaybe <- messageBodyParser headerFields

        return (Request requestLine headerFields bodyMaybe)

    <?> "HTTP-message"

headerFieldListParser :: Parser [HeaderField]
headerFieldListParser =
    many $
      do
        x <- headerFieldParser
        _ <- crlfParser
        return x

-- RFC 7230, section 3.1.1: Request Line

requestLineParser :: Parser RequestLine
requestLineParser =
    do
        method <- methodParser
        _ <- spParser
        requestTarget <- requestTargetParser
        _ <- spParser
        httpVersion <- httpVersionParser
        _ <- crlfParser

        return (RequestLine method requestTarget httpVersion)

    <?> "request-line"

methodParser :: Parser Method
methodParser =
    do
        x <- tokenParser
        return (Method x)

    <?> "method"

-- RFC 7230, section 3.2: Header Fields

headerFieldParser :: Parser HeaderField
headerFieldParser =
    do
        fieldName <- fieldNameParser
        _ <- P.char ':'
        _ <- owsParser
        fieldValue <- fieldValueParser
        _ <- owsParser

        return (HeaderField fieldName fieldValue)

    <?> "header-field"

fieldNameParser :: Parser FieldName
fieldNameParser =
    do
        x <- tokenParser
        return (FieldName x)

    <?> "field-name"

fieldValueParser :: Parser FieldValue
fieldValueParser =
    do
        (x, _) <- P.match $
            P.sepBy
                (P.takeWhile1 isVchar)
                (P.takeWhile1 (P.inClass " \t"))

        return (FieldValue x)

    <?> "field-value"

-- RFC 7230, section 3.2.3: Whitespace

owsParser :: Parser ()
owsParser =
    do
        _ <- P.takeWhile isWhitespace
        return ()

    <?> "OWS"

isWhitespace :: Char -> Bool
isWhitespace = P.inClass " \t"

-- RFC 7230, section 3.2.6: Field Value Components

tokenParser :: Parser BS.ByteString
tokenParser =
    P.takeWhile1 isTchar

    <?> "token"

isTchar :: Char -> Bool
isTchar c =
    P.isDigit c ||
    P.isAlpha_ascii c ||
    P.inClass "!#$%&'*+-.^_`|~" c

-- RFC 7230, section 5.3: Request Target

requestTargetParser :: Parser RequestTarget
requestTargetParser =
    do
        x <- P.takeWhile1 (/= ' ')
        return (RequestTarget x)

    <?> "request-target"

------------------------------------------

messageBodyParser :: [HeaderField] -> Parser (Maybe MessageBody)
messageBodyParser _headerFields =
    _next_lesson
