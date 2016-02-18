{-|
Module      : Game.Werewolf.Response
Description : Response and message data structures.

Copyright   : (c) Henry J. Wylde, 2016
License     : BSD3
Maintainer  : public@hjwylde.com

A response is used as a return result of calling the @werewolf@ binary. Each response has a list of
associated messages.

@werewolf@ was designed to be ambivalent to the calling chat client. The response-message structure
reflects this by staying away from anything that could be construed as client-specific. This
includes features such as emoji support.
-}

{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveGeneric #-}

module Game.Werewolf.Response (
    -- * Response
    Response(..),

    -- ** Common responses
    success, failure,

    -- ** Exit functions
    exitWith,

    -- * Message
    Message(..),
    publicMessage, privateMessage, groupMessages,
) where

import Control.Monad.IO.Class

import Data.Aeson
#if !MIN_VERSION_aeson(0,10,0)
import Data.Aeson.Types
#endif
import           Data.Text               (Text)
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO       as T

import GHC.Generics

import qualified System.Exit as Exit

-- | When a user sends a command to the @werewolf@ binary, a response is always returned.
--
--   The chat client interface should then relay any @messages@ from the response. Whether or not
--   the command was valid (indicated by the @ok@ flag) is often irrelevant as the returned
--   @messages@ will include errors to the user.
data Response = Response
    { ok       :: Bool      -- ^ Boolean flag to indicate success.
    , messages :: [Message] -- ^ List of messages.
    } deriving (Eq, Generic, Show)

instance FromJSON Response

instance ToJSON Response where
    toJSON      = genericToJSON defaultOptions
#if MIN_VERSION_aeson(0,10,0)
    toEncoding  = genericToEncoding defaultOptions
#endif

-- | A successful, empty response.
success :: Response
success = Response True []

-- | An unsuccessful, empty response.
failure :: Response
failure = Response False []

-- | Exits fast with the given response. The response is encoded as JSON, printed to @stdout@ and
--   then the program is exited with @0@ (success).
--
--   The program always exits with success even if the response was a failure one. This is to
--   distinguish between bad calls to the binary and bad commands to the werewolf engine.
exitWith :: MonadIO m => Response -> m ()
exitWith response = liftIO $ T.putStrLn (T.decodeUtf8 $ encode response) >> Exit.exitSuccess

-- | A message may be either public or private, indicated by it's @to@ field.
--
--   Each message contains a single text field. This field is permitted to contain special
--   characters such as new lines and tabs.
data Message = Message
    { to      :: Maybe Text -- ^ The message recipient: 'Nothing' for a public message,
                            --   'Just' for a private message.
    , message :: Text       -- ^ The message text.
    } deriving (Eq, Generic, Show)

instance FromJSON Message

instance ToJSON Message where
    toJSON      = genericToJSON defaultOptions
#if MIN_VERSION_aeson(0,10,0)
    toEncoding  = genericToEncoding defaultOptions
#endif

-- | Creates a public message with the given text.
publicMessage :: Text -> Message
publicMessage = Message Nothing

-- | @privateMessage to message@
--
--   Creates a private message to @to@ with the given text.
privateMessage :: Text -> Text -> Message
privateMessage to = Message (Just to)

-- | @groupMessages tos message@
--
--   Creates multiple private messages (1 to each recipient) with the given text.
groupMessages :: [Text] -> Text -> [Message]
groupMessages tos message = map (`privateMessage` message) tos
