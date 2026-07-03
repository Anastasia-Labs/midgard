module Midgard.Node.DB.Schema.TxAdmissionStatus (
  TxAdmissionStatus (..),
) where

import Data.Char (toUpper)
import Data.Text qualified as Text
import Database.Persist.Class (PersistField (fromPersistValue, toPersistValue))
import Database.Persist.Sql (PersistFieldSql (sqlType), PersistValue (PersistText), SqlType (SqlOther))

-- For whatever reason, the Typescript side SQL declares this as a custom SQL type.
-- Thus, we cannot simply use derivePersistField as that merely uses a text type with a constraint.
data TxAdmissionStatus
  = Queued
  | Validating
  | Accepted
  | Rejected
  deriving stock (Eq, Read, Show)

instance PersistField TxAdmissionStatus where
  toPersistValue = PersistText . Text.toLower . Text.pack . show
  fromPersistValue (PersistText text) =
    case reads (mapHead toUpper $ Text.unpack text) of
      [(value, "")] -> Right value
      _ -> Left ("Invalid TxAdmissionStatus: " <> text)
  fromPersistValue other =
    Left ("Expected PersistText for TxAdmissionStatus, got: " <> Text.pack (show other))

instance PersistFieldSql TxAdmissionStatus where
  sqlType _ = SqlOther "tx_admission_status"

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x : xs) = f x : xs
