{-# OPTIONS_GHC -Wall -fno-warn-unused-imports -fdefer-typed-holes #-}

{-# LANGUAGE TypeApplications #-}

module WebLesson5 where

-- base
import qualified Data.Char as Char
import qualified Data.List as List
import           Data.Word (Word8)
import           Numeric   (showInt)

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

staticResponseServer :: Response -> IO ()
staticResponseServer response =
  server $ \socket ->
    LSocket.sendAll socket
      (BSB.toLazyByteString (encodeResponse response))

------------------------------------------

helloResponse_withMoreTypes :: Response
helloResponse_withMoreTypes =

    _exercise_1

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
asciiMessageBody x = MessageBody (LASCII.pack x)

------------------------------------------

helloResponse_moreConveniently :: Response
helloResponse_moreConveniently =

    _exercise_2
