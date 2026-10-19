{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.WitnessValidator
Description : Behavioural tests for the Plutarch port of
              @validators/user-events/witness.ak@.

The witness script is the staking credential paired one-to-one with a user
event. Two properties carry the design, and the tests are grouped by them.

The first is the pairing itself: the credential's registration must accompany
the event's mint and its unregistration must accompany the burn, so the two have
the same lifetime. The mint quantity is looked up under the /nonce/ the script
was parameterised with, which is what stops one event's witness standing in for
another's.

The second is the "prove not registered" pair. Registering a credential is only
possible while it is unregistered, so a transaction that registers and
immediately unregisters it proves prior absence and leaves the ledger as it
found it. Both halves are witnessed by this script; the tests below establish
what each half actually demands, including where the two halves differ.
-}
module Testing.WitnessValidator (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, singleton)
import PlutusLedgerApi.V3 (
  Credential (ScriptCredential),
  PubKeyHash (..),
  ScriptContext,
  ScriptHash (..),
  TxCert (..),
 )
import PlutusTx.Builtins (dataToBuiltinData, fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.Witness (witnessPublishValidator)
import Testing.Eval (pfails, psucceeds)
import Testing.ScriptContextBuilder (
  buildScriptContext,
  currencySymbolFromHex,
  withCertificate,
  withCertifyingScript,
  withMint,
 )

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Witness Validator Tests"
    [ testGroup
        "publish / MintOrBurn"
        [ testCase "accepts a registration alongside the event mint" $
            psucceeds $ run (mintOrBurn eventPolicy) regCert [regCert] (eventMint 1)
        , testCase "accepts an unregistration alongside the event burn" $
            psucceeds $ run (mintOrBurn eventPolicy) unregCert [unregCert] (eventMint (-1))
        , -- Neither half may happen on its own: that is what ties the
          -- credential's lifetime to the event's.
          testCase "rejects a registration with no mint" $
            pfails $ run (mintOrBurn eventPolicy) regCert [regCert] mempty
        , testCase "rejects an unregistration with no burn" $
            pfails $ run (mintOrBurn eventPolicy) unregCert [unregCert] mempty
        , -- The cross pair. Registering while burning would leave a credential
          -- registered with no event behind it; unregistering while minting
          -- would leave an event with no credential.
          testCase "rejects a registration alongside the burn" $
            pfails $ run (mintOrBurn eventPolicy) regCert [regCert] (eventMint (-1))
        , testCase "rejects an unregistration alongside the mint" $
            pfails $ run (mintOrBurn eventPolicy) unregCert [unregCert] (eventMint 1)
        , -- One registration cannot cover two events.
          testCase "rejects a registration alongside a mint of two" $
            pfails $ run (mintOrBurn eventPolicy) regCert [regCert] (eventMint 2)
        , -- The `_ -> False` arm of the original.
          testCase "rejects a certificate that neither registers nor unregisters" $
            pfails $ run (mintOrBurn eventPolicy) poolRetireCert [poolRetireCert] (eventMint 1)
        , -- The nonce is a script parameter, so the quantity consulted is this
          -- event's and no other's.
          testCase "rejects a mint under a different asset name" $
            pfails $
              run
                (mintOrBurn eventPolicy)
                regCert
                [regCert]
                (singleton eventPolicy otherNonce 1)
        , testCase "rejects a mint under a different policy" $
            pfails $
              run
                (mintOrBurn eventPolicy)
                regCert
                [regCert]
                (singleton otherPolicy nonce 1)
        , -- The redeemer names the policy whose mint is consulted, and nothing
          -- here constrains that choice: the binding lives on the event side,
          -- in `validate_witness_redeemer`, which requires the witness's
          -- redeemer to be `MintOrBurn` naming the event policy itself.
          testCase "rejects a redeemer naming a policy that did not mint" $
            pfails $ run (mintOrBurn otherPolicy) regCert [regCert] (eventMint 1)
        ]
    , testGroup
        "publish / RegisterToProveNotRegistered"
        [ testCase "accepts a register/unregister pair at the named index" $
            psucceeds $ run (registerToProve 0) regCert [regCert, unregCert] mempty
        , testCase "accepts a pair further along the list" $
            psucceeds $
              run (registerToProve 1) regCert [poolRetireCert, regCert, unregCert] mempty
        , -- The index must name the certificate this script is witnessing,
          -- otherwise the pair proved absent could be someone else's.
          testCase "rejects an index naming a different certificate" $
            pfails $
              run (registerToProve 0) regCert [otherRegCert, regCert, unregCert] mempty
        , -- A registration with nothing after it is not a no-op: it leaves the
          -- credential registered.
          testCase "rejects a registration with no certificate after it" $
            pfails $ run (registerToProve 0) regCert [regCert] mempty
        , testCase "rejects a following certificate that is not an unregistration" $
            pfails $ run (registerToProve 0) regCert [regCert, poolRetireCert] mempty
        , testCase "rejects an unregistration of a different credential" $
            pfails $ run (registerToProve 0) regCert [regCert, otherUnregCert] mempty
        , -- Adjacency is load-bearing: an intervening certificate could observe
          -- the credential as registered, so the pair would no longer be inert.
          testCase "rejects a certificate between the pair" $
            pfails $
              run (registerToProve 0) regCert [regCert, poolRetireCert, unregCert] mempty
        , testCase "rejects an index past the end of the list" $
            pfails $ run (registerToProve 5) regCert [regCert, unregCert] mempty
        , -- Aiken's `list.drop` returns the list untouched for a non-positive
          -- count rather than failing, so a negative index names the head. It
          -- is harmless here only because the head still has to be the
          -- certificate being witnessed.
          testCase "a negative index names the head of the list" $
            psucceeds $ run (registerToProve (-1)) regCert [regCert, unregCert] mempty
        ]
    , testGroup
        "publish / UnregisterToProveNotRegistered"
        [ testCase "accepts an unregistration naming its registration" $
            psucceeds $ run (unregisterToProve 0) unregCert [regCert, unregCert] mempty
        , -- This half witnesses the unregistration; a registration arriving
          -- here means the redeemer and the purpose disagree.
          testCase "rejects a witnessed certificate that is a registration" $
            pfails $ run (unregisterToProve 0) regCert [regCert, unregCert] mempty
        , testCase "rejects an index naming a certificate that is not a registration" $
            pfails $ run (unregisterToProve 0) unregCert [unregCert, regCert] mempty
        , testCase "rejects a registration of a different credential" $
            pfails $ run (unregisterToProve 0) unregCert [otherRegCert, unregCert] mempty
        , testCase "rejects an index past the end of the list" $
            pfails $ run (unregisterToProve 5) unregCert [regCert, unregCert] mempty
        , -- The asymmetry between the two halves, pinned. The registration half
          -- above demands the unregistration immediately follow it; this half
          -- demands only that a matching registration exist /somewhere/, in any
          -- position. See the note in the port's README.
          testCase "accepts a registration that is not adjacent to the unregistration" $
            psucceeds $
              run (unregisterToProve 0) unregCert [regCert, poolRetireCert, unregCert] mempty
        ]
    ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

{- | Runs the validator against a transaction publishing @witnessed@ under
@redeemer@, with @certs@ as the transaction's certificate list.

The certifying purpose's own index is set to where @witnessed@ actually sits, so
the fixture stays realistic; the validator never reads it, matching the
original's explicit note that the index goes unchecked.
-}
run :: forall s. PD.Data -> TxCert -> [TxCert] -> Value -> Term s PUnit
run redeemer witnessed certs minted =
  witnessPublishValidator # pdata (pconstant nonce) # pconstant ctx
  where
    ctx :: ScriptContext
    ctx =
      buildScriptContext $
        withCertifyingScript (dataToBuiltinData redeemer) (indexOf witnessed certs) witnessed
          <> foldMap withCertificate certs
          <> mintOf minted

    -- `withMint` reads the policy off the value, so an empty mint has to be
    -- left out altogether rather than added as `mempty`.
    mintOf v
      | v == mempty = mempty
      | otherwise = withMint v (dataToBuiltinData (PD.I 0))

    indexOf c cs = case [i | (i, c') <- zip [0 ..] cs, c' == c] of
      (i : _) -> i
      [] -> 0

eventMint :: Integer -> Value
eventMint = singleton eventPolicy nonce

mintOrBurn :: CurrencySymbol -> PD.Data
mintOrBurn cs = PD.Constr 0 [PD.B (fromBuiltin (unCurrencySymbol cs))]

registerToProve :: Integer -> PD.Data
registerToProve i = PD.Constr 1 [PD.I i]

unregisterToProve :: Integer -> PD.Data
unregisterToProve i = PD.Constr 2 [PD.I i]

--------------------------------------------------------------------------------
-- Identities
--------------------------------------------------------------------------------

regCert, unregCert, otherRegCert, otherUnregCert, poolRetireCert :: TxCert
regCert = TxCertRegStaking witnessCred Nothing
unregCert = TxCertUnRegStaking witnessCred Nothing
otherRegCert = TxCertRegStaking otherCred Nothing
otherUnregCert = TxCertUnRegStaking otherCred Nothing
poolRetireCert = TxCertPoolRetire (PubKeyHash (toBuiltin (BS.replicate 28 0x99))) 1

witnessCred, otherCred :: Credential
witnessCred = ScriptCredential (ScriptHash (toBuiltin (BS.replicate 28 0x11)))
otherCred = ScriptCredential (ScriptHash (toBuiltin (BS.replicate 28 0x22)))

eventPolicy, otherPolicy :: CurrencySymbol
eventPolicy = currencySymbolFromHex (concat (replicate 28 "33"))
otherPolicy = currencySymbolFromHex (concat (replicate 28 "44"))

-- | The event's nonce, which doubles as its NFT's asset name.
nonce, otherNonce :: TokenName
nonce = TokenName (toBuiltin (BS.replicate 32 0x55))
otherNonce = TokenName (toBuiltin (BS.replicate 32 0x66))
