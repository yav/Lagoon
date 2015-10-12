{-# LANGUAGE Trustworthy, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
module JSON
  (
    -- * JSON encodings

    -- ** Export
    Export(..)
  , jsonBytes
  , (.=)
  , object
  , jsNull
  , JS.Value
  , JS.Parser
  , ExportAsKey(..)
  , jsKey

    -- ** Import
  , fromBytesJS
  , Import(..)
  , withText
  , withObject
  , (.:)


  -- * Snap
  , sendJSON

    -- * Remote procedure calls
  , remote, Remote, I
  , snapStepI

    -- ** Accessing parameters
  , bsParam
  , jsParam
  , textParam
  , intParam
  , natParam
  , boolParam

    -- ** Errors
  , badInput
  , notFound
  ) where

import           Data.Text(Text)
import qualified Data.Aeson as JS
import qualified Data.Aeson.Types as JS
import qualified Data.Vector as Vector
import           Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Control.Monad(ap,liftM)
import           Snap.Core (Snap)
import qualified Snap.Core as Snap
import qualified Data.Text as Text
import           Data.Text.Read(decimal)
import           Data.Text.Encoding(decodeUtf8,encodeUtf8)
import qualified Data.HashMap.Strict as HashMap
import           Data.Scientific(toBoundedInteger)

class Export a where
  toJS :: a -> JS.Value

class ExportAsKey a where
  toKeyJS :: a -> Text

class Import a where
  fromJS :: JS.Value -> Maybe a


(.=) :: Export a => Text -> a -> JS.Pair
x .= y = x JS..= toJS y

object :: [ JS.Pair ] -> JS.Value
object = JS.object

jsNull :: JS.Value
jsNull = JS.Null

jsonBytes :: Export a => a -> LBS.ByteString
jsonBytes = JS.encode . toJS

fromBytesJS :: Import a => LBS.ByteString -> Maybe a
fromBytesJS bs = fromJS =<< JS.decode bs

jsKey :: ExportAsKey a => a -> JS.Value
jsKey = toJS . toKeyJS

withText :: (Text -> Maybe a) -> JS.Value -> Maybe a
withText k (JS.String t) = k t
withText _ _             = Nothing

withBool :: (Bool -> Maybe a) -> JS.Value -> Maybe a
withBool k (JS.Bool b) = k b
withBool _ _ = Nothing

withObject :: (JS.Object -> Maybe a) -> JS.Value -> Maybe a
withObject k (JS.Object o) = k o
withObject _ _ = Nothing

withNumber :: (Int -> Maybe a) -> JS.Value -> Maybe a
withNumber k (JS.Number x) = do n <- toBoundedInteger x
                                k n
withNumber _ _ = Nothing

withArray :: ([JS.Value] -> Maybe a) -> JS.Value -> Maybe a
withArray k (JS.Array x) = k (Vector.toList x)
withArray _ _            = Nothing



(.:) :: Import a => JS.Object -> Text -> Maybe a
o .: t = fromJS =<< HashMap.lookup t o

instance Export Bool where
  toJS n = JS.Bool n

instance Import Bool where
  fromJS = withBool return


-- | May loose precision
instance Export Int where
  toJS n = JS.Number (fromIntegral n)

instance Import Int where
  fromJS = withNumber return


instance Export Text where
  toJS xs = JS.String xs

instance ExportAsKey Text where
  toKeyJS = id

instance Import Text where
  fromJS = withText return


instance Export JS.Value where
  toJS x = x

instance Import JS.Value where
  fromJS = return


instance Export a => Export (Maybe a) where
  toJS x = case x of
             Nothing -> JS.Null
             Just a  -> toJS a

instance Import a => Import (Maybe a) where
  fromJS x = case x of
               JS.Null -> return Nothing
               v       -> do a <- fromJS v
                             return (Just a)

instance Export a => Export [a] where
  toJS xs = JS.Array (Vector.fromList (map toJS xs))

instance Import a => Import [a] where
  fromJS = withArray (mapM fromJS)

--------------------------------------------------------------------------------

data I a  = forall b. Import b => Receive (b -> I a)
          | Send JS.Value (I a)
          | Error Text
          | Return a


instance Functor I where
  fmap = liftM

instance Applicative I where
  (<*>) = ap
  pure  = Return

instance Monad I where
  fail x          = Error (Text.pack x)

  Return a  >>= k = k a
  Send x k  >>= f = Send x (k >>= f)
  Receive k >>= f = Receive (\a -> k a >>= f)



--------------------------------------------------------------------------------

data Cmd  = Cmd Text [JS.Value]

instance Export Cmd where
  toJS (Cmd txt vs) = toJS (toJS txt : vs)


class Remote a where
  mkCmd :: Text -> [JS.Value] -> a

instance MkRes a => Remote (I a) where
  mkCmd txt xs = Send (toJS (Cmd txt (reverse xs))) mkRes

instance (Export a, Remote b) => Remote (a -> b) where
  mkCmd txt xs = \x -> mkCmd txt (toJS x:xs)



class MkRes a where
  mkRes :: I a

instance {-# OVERLAPPING #-} MkRes () where
  mkRes = Return ()

instance Import a => MkRes a where
  mkRes = Receive Return



remote :: Remote a => Text -> a
remote x = mkCmd x []



--------------------------------------------------------------------------------

snapStepI :: I a -> Snap (I a)
snapStepI w =
  case w of
    Receive k  -> do a <- jsParam "result"
                     return (k a)

    Send c k   -> do sendJSON c
                     return k

    Error e    -> badInput (encodeUtf8 e)

    Return a   -> return (Return a)


sendJSON :: Export a => a -> Snap ()
sendJSON a =
  do Snap.modifyResponse (Snap.setHeader "content-type" "application/json")
     Snap.writeLBS (jsonBytes a)



bsParam :: ByteString -> Snap ByteString
bsParam p =
  do mb <- Snap.getParam p
     case mb of
       Just x  -> return x
       Nothing -> badInput ("Missing parameter: " `BS.append` p)

jsParam :: Import a => ByteString -> Snap a
jsParam p =
  do bs <- bsParam p
     case fromBytesJS (LBS.fromStrict bs) of
       Just a  -> return a
       Nothing -> badInput ("Malformed JSON encoded parameter: " `BS.append` p)

textParam :: ByteString -> Snap Text
textParam p = decodeUtf8 `fmap` bsParam p

intParam :: ByteString -> Snap Int
intParam p =
  do txt <- textParam p
     let (neg,numTxt) = case Text.uncons txt of
                          Just ('-',t) -> (negate, t)
                          _            -> (id, txt)
     case decimal numTxt of
       Right (a,t) | Text.null t -> return (neg a)
       _ -> badInput ("Malformed integer parameter: " `BS.append` p)

natParam :: ByteString -> Snap Int
natParam p =
  do txt <- textParam p
     case decimal txt of
       Right (a,t) | Text.null t -> return a
       _ -> badInput ("Malformed natural parameter: " `BS.append` p)

boolParam :: ByteString -> Snap Bool
boolParam p =
  do bs <- bsParam p
     case bs of
       "true"  -> return True
       "false" -> return False
       _       -> badInput ("Malformed boolean parameter: " `BS.append` p)




--------------------------------------------------------------------------------
-- Error reporting

badInput :: ByteString -> Snap a
badInput msg =
  Snap.finishWith (Snap.setResponseStatus 400 msg Snap.emptyResponse)

notFound :: Snap a
notFound = Snap.finishWith (Snap.setResponseStatus 404 "Not Found"
                                                      Snap.emptyResponse)




