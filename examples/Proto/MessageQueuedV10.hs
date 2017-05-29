{- This file was auto-generated from message_queued_v1_0.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, MultiParamTypeClasses, FlexibleContexts,
  FlexibleInstances, PatternSynonyms, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Proto.MessageQueuedV10 where
import qualified Prelude
import qualified Data.Int
import qualified Data.Word
import qualified Data.ProtoLens
import qualified Data.ProtoLens.Message.Enum
import qualified Lens.Family2
import qualified Lens.Family2.Unchecked
import qualified Data.Default.Class
import qualified Data.Text
import qualified Data.Map
import qualified Data.ByteString
import qualified Lens.Labels

data MessageQueued = MessageQueued{_MessageQueued'messageId ::
                                   !Data.Text.Text,
                                   _MessageQueued'payload ::
                                   !(Data.Map.Map Data.Text.Text Data.Text.Text)}
                   deriving (Prelude.Show, Prelude.Eq)

instance (a ~ Data.Text.Text, b ~ Data.Text.Text,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "messageId" f MessageQueued MessageQueued a b
         where
        lensOf _
          = Lens.Family2.Unchecked.lens _MessageQueued'messageId
              (\ x__ y__ -> x__{_MessageQueued'messageId = y__})

instance (a ~ Data.Map.Map Data.Text.Text Data.Text.Text,
          b ~ Data.Map.Map Data.Text.Text Data.Text.Text,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "payload" f MessageQueued MessageQueued a b
         where
        lensOf _
          = Lens.Family2.Unchecked.lens _MessageQueued'payload
              (\ x__ y__ -> x__{_MessageQueued'payload = y__})

instance Data.Default.Class.Default MessageQueued where
        def
          = MessageQueued{_MessageQueued'messageId =
                            Data.ProtoLens.fieldDefault,
                          _MessageQueued'payload = Data.Map.empty}

instance Data.ProtoLens.Message MessageQueued where
        descriptor
          = let messageId__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "message_id"
                      (Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional messageId)
                      :: Data.ProtoLens.FieldDescriptor MessageQueued
                payload__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "payload"
                      (Data.ProtoLens.MessageField ::
                         Data.ProtoLens.FieldTypeDescriptor MessageQueued'PayloadEntry)
                      (Data.ProtoLens.MapField key value payload)
                      :: Data.ProtoLens.FieldDescriptor MessageQueued
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, messageId__field_descriptor),
                    (Data.ProtoLens.Tag 2, payload__field_descriptor)])
                (Data.Map.fromList
                   [("message_id", messageId__field_descriptor),
                    ("payload", payload__field_descriptor)])

data MessageQueued'PayloadEntry = MessageQueued'PayloadEntry{_MessageQueued'PayloadEntry'key
                                                             :: !Data.Text.Text,
                                                             _MessageQueued'PayloadEntry'value ::
                                                             !Data.Text.Text}
                                deriving (Prelude.Show, Prelude.Eq)

instance (a ~ Data.Text.Text, b ~ Data.Text.Text,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "key" f MessageQueued'PayloadEntry
           MessageQueued'PayloadEntry
           a
           b
         where
        lensOf _
          = Lens.Family2.Unchecked.lens _MessageQueued'PayloadEntry'key
              (\ x__ y__ -> x__{_MessageQueued'PayloadEntry'key = y__})

instance (a ~ Data.Text.Text, b ~ Data.Text.Text,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "value" f MessageQueued'PayloadEntry
           MessageQueued'PayloadEntry
           a
           b
         where
        lensOf _
          = Lens.Family2.Unchecked.lens _MessageQueued'PayloadEntry'value
              (\ x__ y__ -> x__{_MessageQueued'PayloadEntry'value = y__})

instance Data.Default.Class.Default MessageQueued'PayloadEntry
         where
        def
          = MessageQueued'PayloadEntry{_MessageQueued'PayloadEntry'key =
                                         Data.ProtoLens.fieldDefault,
                                       _MessageQueued'PayloadEntry'value =
                                         Data.ProtoLens.fieldDefault}

instance Data.ProtoLens.Message MessageQueued'PayloadEntry where
        descriptor
          = let key__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "key"
                      (Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional key)
                      :: Data.ProtoLens.FieldDescriptor MessageQueued'PayloadEntry
                value__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "value"
                      (Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional value)
                      :: Data.ProtoLens.FieldDescriptor MessageQueued'PayloadEntry
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, key__field_descriptor),
                    (Data.ProtoLens.Tag 2, value__field_descriptor)])
                (Data.Map.fromList
                   [("key", key__field_descriptor),
                    ("value", value__field_descriptor)])

key ::
    forall f s t a b . (Lens.Labels.HasLens "key" f s t a b) =>
      Lens.Family2.LensLike f s t a b
key
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "key")

messageId ::
          forall f s t a b . (Lens.Labels.HasLens "messageId" f s t a b) =>
            Lens.Family2.LensLike f s t a b
messageId
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "messageId")

payload ::
        forall f s t a b . (Lens.Labels.HasLens "payload" f s t a b) =>
          Lens.Family2.LensLike f s t a b
payload
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "payload")

value ::
      forall f s t a b . (Lens.Labels.HasLens "value" f s t a b) =>
        Lens.Family2.LensLike f s t a b
value
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "value")
