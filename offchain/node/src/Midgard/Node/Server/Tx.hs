module Midgard.Node.Server.Tx (
  txServer,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import Midgard.Node.Server.Api (TxAPI)
import Midgard.Node.Server.Monad (ServerM)
import Midgard.Node.Server.Tx.Types (
  DepositBuildRequest,
  DepositBuildResponse (..),
  SubmitAcceptedResponse (..),
  SubmitTxRequest,
 )
import Servant (ServerT, type (:<|>) (..))

txServer :: ServerT TxAPI ServerM
txServer =
  depositBuildPlaceholderHandler
    :<|> submitPlaceholderHandler

depositBuildPlaceholderHandler :: DepositBuildRequest -> ServerM DepositBuildResponse
depositBuildPlaceholderHandler _request =
  -- TODO(deposit): build the unsigned Cardano deposit transaction by calling
  -- the Haskell Midgard contracts instead of returning an empty placeholder.
  pure DepositBuildResponse {unsignedTxCbor = ""}

submitPlaceholderHandler :: SubmitTxRequest -> ServerM SubmitAcceptedResponse
submitPlaceholderHandler _request = do
  now <- liftIO getCurrentTime
  -- TODO(submit): validate, persist, and enqueue the submitted native tx.
  pure
    SubmitAcceptedResponse
      { txId = ""
      , status = "queued"
      , firstSeenAt = now
      , lastSeenAt = now
      , duplicate = False
      }
