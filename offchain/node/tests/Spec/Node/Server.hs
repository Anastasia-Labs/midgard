{-# LANGUAGE OverloadedStrings #-}

module Spec.Node.Server (tests) where

import Data.Aeson qualified as Aeson
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Midgard.Node.Codec.Scripts (migardSupportedScriptLanguages)
import Midgard.Node.Config (
  ApiConfig (..),
  ContractsConfig (..),
  FeatureFlags (..),
  MidgardConfig (..),
  MidgardNodeConfig (..),
  ProtocolConfig (..),
 )
import Midgard.Node.Env (NodeEnv (..))
import Midgard.Node.Server (mkApplication)
import Midgard.Node.Server.Api (HealthAPI)
import Midgard.Node.Server.Health.Types (HealthResponse (..), HealthResponseOk (..), ReadinessResponse)
import Midgard.Node.Server.JSON.Types (NaturalJSON (..))
import Midgard.Node.Server.Protocol.Types (
  Network (..),
  ProtocolFeeParameters (..),
  ProtocolInfoApiVersion (..),
  ProtocolInfoResponse (..),
  ProtocolSubmissionLimits (..),
  ProtocolValidation (..),
 )
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status, status501)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant.API (Get, JSON, Post, QueryParam, ReqBody, type (:<|>) (..), type (:>))
import Servant.Client (
  BaseUrl (..),
  ClientError (FailureResponse),
  ClientM,
  Scheme (Http),
  client,
  mkClientEnv,
  responseStatusCode,
  runClientM,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "node-server"
    [ testCase "GET /healthz returns service health" $
        withTestClient $ \run -> do
          result <- run healthClient
          case result of
            Right response -> response.status @?= HealthResponseOk
            Left err -> assertFailure ("Expected successful health response but got client error: " <> show err)
    , testCase "GET /protocol-info returns OpenAPI-shaped protocol metadata" $
        withTestClient $ \run -> do
          assertClientOk
            ProtocolInfoResponse
              { apiVersion = ProtocolInfoApiV_1
              , network = Preview
              , midgardNativeTxVersion = 1
              , currentSlot = NaturalJSON 0
              , supportedScriptLanguages = migardSupportedScriptLanguages
              , protocolFeeParameters =
                  ProtocolFeeParameters
                    { minFeeA = NaturalJSON 44
                    , minFeeB = NaturalJSON 155381
                    }
              , submissionLimits =
                  ProtocolSubmissionLimits
                    { maxSubmitTxCborBytes = 32768
                    }
              , validation =
                  ProtocolValidation
                    { strictnessProfile = "phase1_midgard"
                    , localValidationIsAuthoritative = False
                    }
              }
            =<< run protocolInfoClient
    , testCase "DB-backed query endpoints are parked behind placeholders" $
        withTestClient $ \run -> do
          assertClientStatus status501 =<< run (getTxClient (Just "00"))
          assertClientStatus status501 =<< run (getTxStatusClient (Just "00"))
          assertClientStatus status501 =<< run (getDepositStatusClient (Just "00") Nothing)
          assertClientStatus status501 =<< run (_getTxsClient (Just "addr_test1placeholder"))
          assertClientStatus status501 =<< run (getUtxoClient (Just "00"))
          assertClientStatus status501 =<< run (getUtxosClient (Just "addr_test1placeholder"))
          assertClientStatus status501 =<< run (postUtxosByOutRefsClient (Just "") ([] :: [Text]))
          assertClientStatus status501 =<< run (getBlockClient (Just "00"))
    ]

withTestClient :: ((forall a. ClientM a -> IO (Either ClientError a)) -> IO result) -> IO result
withTestClient action =
  -- Running through Warp plus servant-client exercises the generated HTTP
  -- client and the real Servant routing/encoding boundary together.
  testWithApplication (pure (mkApplication testEnv)) $ \port -> do
    manager <- newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager (BaseUrl Http "127.0.0.1" port "")
    action (`runClientM` clientEnv)

testEnv :: NodeEnv
testEnv =
  NodeEnv
    { config =
        MidgardNodeConfig
          { api = ApiConfig {port = 0}
          , database = Nothing
          , logLevel = "debug"
          , midgard = MidgardConfig {network = "Preview", adminApiKey = Nothing}
          , protocol =
              ProtocolConfig
                { minFeeA = "44"
                , minFeeB = "155381"
                , maxSubmitTxCborBytes = 32768
                , validationStrictnessProfile = "phase1_midgard"
                }
          , contracts = ContractsConfig {midgardEnvFile = Nothing, deploymentInfoFile = Nothing}
          , features = FeatureFlags {enableMutatingEndpoints = False}
          }
    , migrationDirectory = "/tmp/midgard-node-test-migrations"
    }

healthClient :: ClientM HealthResponse
_readyClient :: ClientM ReadinessResponse
healthClient
  :<|> _readyClient =
    client (Proxy @HealthAPI)

protocolInfoClient :: ClientM ProtocolInfoResponse
protocolInfoClient =
  client (Proxy @ProtocolInfoAPI)

type ProtocolInfoAPI = "protocol-info" :> Get '[JSON] ProtocolInfoResponse

getTxClient :: Maybe Text -> ClientM Aeson.Value
getTxStatusClient :: Maybe Text -> ClientM Aeson.Value
getDepositStatusClient :: Maybe Text -> Maybe Text -> ClientM Aeson.Value
_getTxsClient :: Maybe Text -> ClientM Aeson.Value
getUtxoClient :: Maybe Text -> ClientM Aeson.Value
getUtxosClient :: Maybe Text -> ClientM Aeson.Value
postUtxosByOutRefsClient :: Maybe Text -> [Text] -> ClientM Aeson.Value
getBlockClient :: Maybe Text -> ClientM Aeson.Value
getTxClient
  :<|> getTxStatusClient
  :<|> getDepositStatusClient
  :<|> _getTxsClient
  :<|> getUtxoClient
  :<|> getUtxosClient
  :<|> postUtxosByOutRefsClient
  :<|> getBlockClient =
    client (Proxy @ParkedQueryAPI)

type ParkedQueryAPI =
  "tx" :> QueryParam "tx_hash" Text :> Get '[JSON] Aeson.Value
    :<|> "tx-status" :> QueryParam "tx_hash" Text :> Get '[JSON] Aeson.Value
    :<|> "deposit-status" :> QueryParam "eventId" Text :> QueryParam "cardanoTxHash" Text :> Get '[JSON] Aeson.Value
    :<|> "txs" :> QueryParam "address" Text :> Get '[JSON] Aeson.Value
    :<|> "utxo" :> QueryParam "txOutRef" Text :> Get '[JSON] Aeson.Value
    :<|> "utxos" :> QueryParam "address" Text :> Get '[JSON] Aeson.Value
    :<|> "utxos" :> QueryParam "by-outrefs" Text :> ReqBody '[JSON] [Text] :> Post '[JSON] Aeson.Value
    :<|> "block" :> QueryParam "header_hash" Text :> Get '[JSON] Aeson.Value

assertClientOk :: (Eq a, Show a) => a -> Either ClientError a -> IO ()
assertClientOk expected result =
  case result of
    Right actual -> actual @?= expected
    Left err -> assertFailure ("Expected successful response but got client error: " <> show err)

assertClientStatus :: Status -> Either ClientError a -> IO ()
assertClientStatus expectedStatus result =
  case result of
    Left (FailureResponse _ response) ->
      responseStatusCode response @?= expectedStatus
    Left other ->
      assertFailure ("Expected HTTP " <> show expectedStatus <> " but got client error: " <> show other)
    Right _ ->
      assertFailure ("Expected HTTP " <> show expectedStatus <> " but request succeeded")
