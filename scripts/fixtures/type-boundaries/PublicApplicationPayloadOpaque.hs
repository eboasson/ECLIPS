{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module PublicApplicationPayloadOpaque where

import Data.Coerce (coerce)
import Data.Text (Text)
import Eclips.Application.Typed qualified as App
import Eclips.Application.Typed.Advanced qualified as Advanced
import Eclips.Application.Types.Identity (PrivateNablaId)
import Eclips.Application.Types.Write (WriteResult)
import GHC.Generics (Generic (..))

#if defined(PAYLOAD_INTERNAL_IMPORT)
import Eclips.Application.Typed.Internal (Nabla)
#endif

data Message = Message {message :: Text}
  deriving stock (Eq, Show, Generic)

instance App.ValueType Message
instance App.ApplicationSort Message where
  sortPolicy = App.regularPolicy [App.key (App.field @"message")]

newtype Alternate = Alternate Message
newtype WrappedText = WrappedText Text

-- The payloads themselves are representationally equal. Coercion of an opaque
-- sort-bound handle must still fail because its payload role is nominal.
coerciblePayload :: Message -> Alternate
coerciblePayload = coerce

#if defined(PAYLOAD_NABLA_CONSTRUCTOR)
forged :: App.Nabla Message
forged = App.Nabla undefined undefined undefined
#elif defined(PAYLOAD_DELTA_CONSTRUCTOR)
forged :: App.Delta Message
forged = App.Delta undefined undefined undefined
#elif defined(PAYLOAD_QUERY_CONSTRUCTOR)
forged :: App.Query Message
forged = App.Query undefined undefined
#elif defined(PAYLOAD_SORT_CONSTRUCTOR)
forged :: App.Sort Message
forged = App.Sort undefined undefined
#elif defined(PAYLOAD_FIELD_CONSTRUCTOR)
forged :: App.Field Message Text
forged = App.Field undefined
#elif defined(PAYLOAD_RESERVATION_CONSTRUCTOR)
forged :: App.Reservation Message
forged = App.Reservation undefined undefined
#elif defined(PAYLOAD_OBJECT_CONSTRUCTOR)
forged :: App.Object Message
forged = App.Object undefined undefined
#elif defined(PAYLOAD_CALL_CONSTRUCTOR)
forged :: Advanced.Call Message
forged = Advanced.Call undefined undefined
#elif defined(PAYLOAD_EDGE_RESERVATION_CONSTRUCTOR)
forged :: App.EdgeReservation
forged = App.EdgeReservation undefined undefined undefined
#elif defined(PAYLOAD_SEQUENCER_CONSTRUCTOR)
forged :: App.Sequencer
forged = App.Sequencer undefined undefined
#elif defined(PAYLOAD_CREATION_CONSTRUCTOR)
forged :: App.Creation Message
forged = App.Creation undefined undefined undefined undefined undefined
#elif defined(PAYLOAD_SORT_RECORD)
forged :: App.Sort Message -> App.Sort Message
forged value = value {App.sortDescriptor = undefined}
#elif defined(PAYLOAD_NABLA_RECORD)
forged :: App.Nabla Message -> App.Nabla Message
forged value = value {App.nablaId = undefined}
#elif defined(PAYLOAD_NABLA_COERCION)
forged :: App.Nabla Message -> App.Nabla Alternate
forged = coerce
#elif defined(PAYLOAD_DELTA_COERCION)
forged :: App.Delta Message -> App.Delta Alternate
forged = coerce
#elif defined(PAYLOAD_QUERY_COERCION)
forged :: App.Query Message -> App.Query Alternate
forged = coerce
#elif defined(PAYLOAD_SORT_COERCION)
forged :: App.Sort Message -> App.Sort Alternate
forged = coerce
#elif defined(PAYLOAD_FIELD_SOURCE_COERCION)
forged :: App.Field Message Text -> App.Field Alternate Text
forged = coerce
#elif defined(PAYLOAD_FIELD_RESULT_COERCION)
forged :: App.Field Message Text -> App.Field Message WrappedText
forged = coerce
#elif defined(PAYLOAD_RESERVATION_COERCION)
forged :: App.Reservation Message -> App.Reservation Alternate
forged = coerce
#elif defined(PAYLOAD_OBJECT_COERCION)
forged :: App.Object Message -> App.Object Alternate
forged = coerce
#elif defined(PAYLOAD_CALL_COERCION)
forged :: Advanced.Call Message -> Advanced.Call Alternate
forged = coerce
#elif defined(PAYLOAD_CREATION_COERCION)
forged :: App.Creation Message -> App.Creation Alternate
forged = coerce
#elif defined(PAYLOAD_WRITE_MISMATCH)
forged :: App.Nabla Message -> IO (Either App.TypedError WriteResult)
forged writer = App.write writer True
#elif defined(PAYLOAD_QUERY_LITERAL_MISMATCH)
forged :: App.QueryPredicate Message
forged = App.queryEqual (App.field @"message" @Message) True
#elif defined(PAYLOAD_MISSING_FIELD)
forged :: App.Field Message Text
forged = App.field @"missing" @Message
#elif defined(PAYLOAD_REGULAR_RESERVATION)
forged :: App.Nabla Message -> IO (Either App.TypedError (App.Reservation Message))
forged = App.reserve
#elif defined(PAYLOAD_RAW_ID_BINDING)
forged :: App.Herald -> PrivateNablaId -> Either App.TypedError (App.Nabla Message)
forged = App.bindNabla @Message
#elif defined(PAYLOAD_CALL_RESULT_MISMATCH)
forged :: App.Query Message -> IO (Advanced.Call Bool)
forged predicate = Advanced.submit (Advanced.Read predicate)
#elif defined(PAYLOAD_NABLA_GENERIC)
forged :: App.Nabla Message
forged = to undefined
#elif defined(PAYLOAD_SORT_GENERIC)
forged :: App.Sort Message
forged = to undefined
#elif defined(PAYLOAD_INTERNAL_IMPORT)
forged :: Maybe (Nabla Message)
forged = Nothing
#else
-- Positive control shares the exact vocabulary and imports with every probe.
messageField :: App.Field Message Text
messageField = App.field @"message"

compiled :: Either App.SortError (App.Sort Message)
compiled = App.compileSort @Message

writer :: App.Herald -> Either App.TypedError (App.Nabla Message)
writer herald = App.bindNabla @Message herald "messages.writer"

publish :: App.Nabla Message -> Message -> IO (Either App.TypedError WriteResult)
publish = App.write

query :: App.Delta Message -> App.Query Message
query reader = App.query reader (App.queryEqual messageField "hello")

readMessages :: App.Delta Message -> IO (Either App.TypedError [Message])
readMessages reader = App.read (query reader)

beginRead :: App.Delta Message -> IO (Advanced.Call [Message])
beginRead reader = Advanced.submit (Advanced.Read (query reader))
#endif
