{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FieldPreimageCertificateValidator
Description : Behavioural tests for the Plutarch port of
              @validators/field-preimage-certificate.ak@ — the permissionless
              chunk-certificate validator of @docs/spec/midgard-tx.md@ §8.4–§8.7.

The content rules — the tx-id binding, the split shape, the digest vector, the
reconstruction hash — are proved at their own seam in "Testing.NativeTxCarriage"
and are not re-proved here. What this module covers is everything that is /only/
true of the transaction: which output the certificate lands in, which token
exists afterwards, and who may make it stop existing.

One honest builder per handler, and one refusal per guard whose body is the
honest fixture with a single field mutated, so exactly one conjunct flips. The
fixture is the smallest preimage that is legally tier 3 — one byte over @K@,
splitting into two chunks — which is the cheapest transaction that can carry a
certificate at all, and the split is asserted rather than assumed.

__The healing test is the one that is not a guard.__ §8.7 says a failed or
malicious publication is healable by anyone, and that claim is only real if an
unrelated party — different key, different UTxOs, different min-Ada authority, no
relationship to the original publisher — can certify the same content and get a
token with the same name. That is asserted here as a positive, against carriage
republished from the preimage bytes alone.

Everything the fixture commits is computed here from the format: the §8.4 split,
the per-chunk digests and §8.6's asset-name derivation are written out below
rather than taken from the port, so a table that agreed with itself and with
nothing else would not pass.
-}
module Testing.FieldPreimageCertificateValidator (tests) where

import Data.ByteString qualified as BS
import Data.Word (Word8)
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (
  CurrencySymbol (..),
  TokenName (..),
  Value,
  adaSymbol,
  adaToken,
  getValue,
  singleton,
 )
import PlutusLedgerApi.V3 (
  Address (..),
  Credential (..),
  Datum (..),
  OutputDatum (..),
  PubKeyHash (..),
  Redeemer (..),
  ScriptContext (..),
  ScriptHash (..),
  ScriptInfo (..),
  StakingCredential (..),
  TxId (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
 )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.Builtins (dataToBuiltinData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FieldPreimageCertificate (
  fieldPreimageCertificateMintValidator,
  fieldPreimageCertificateSpendValidator,
 )
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture (
  blake2b256,
  cborInt,
  compactWith,
  defBytes32,
  tx1,
  txIdOf,
  witnessSetCborFrom,
 )
import Testing.ScriptContextBuilder (buildScriptContext)

tests :: TestTree
tests =
  testGroup
    "Field Preimage Certificate Validator Tests"
    [ testGroup "the fixture" fixtureTests
    , testGroup "certification" certificationTests
    , testGroup "healing" healingTests
    , testGroup "retirement" retirementTests
    , testGroup "purpose" purposeTests
    , testGroup "Aiken maximum-corner parity" maximumCornerTests
    ]

--------------------------------------------------------------------------------
-- The fixture
--------------------------------------------------------------------------------

{- | The fixture is the corner it is quoted as: one byte over @K@ is the smallest
preimage that is legally tier 3, and it is what makes the two-chunk carriage
below the cheapest certification the ladder admits.
-}
fixtureTests :: [TestTree]
fixtureTests =
  [ testCase "the preimage is the smallest legally tier-3 size" $
      BS.length preimage @?= chunkK + 1
  , testCase "it splits into a full chunk and a one-byte tail" $
      map BS.length (splitRef preimage) @?= [chunkK, 1]
  , testCase "the certificate carries one digest per chunk" $
      length chunkDigests @?= 2
  , testCase "the content address is a 32-byte digest" $
      BS.length assetName @?= 32
  ]

--------------------------------------------------------------------------------
-- Exact maximum-corner and cost fixtures
--------------------------------------------------------------------------------

maximumCornerTests :: [TestTree]
maximumCornerTests =
  [ testCase "certificate_corner_fixture_sits_at_the_three_chunk_corner" $ do
      BS.length cornerPreimage @?= 32_763
      BS.length cornerPreimage + 5 @?= 32_768
      map BS.length cornerChunks @?= [chunkK, chunkK, 963]
      length (chunkDigestsFor cornerPreimage) @?= 3
      length (aikenChunkInputs cornerPreimage) @?= 3
  , testCase "certificate_mint_fixture_only_smallest_tier3" $ do
      let txInfo = scriptContextTxInfo aikenSmallCertificationTx
      length (txInfoReferenceInputs txInfo) @?= 2
      length (txInfoOutputs txInfo) @?= 1
  , testCase "certificate_mint_cost_at_the_smallest_tier3" $
      psucceeds $ runMint aikenSmallCertificationTx
  , testCase "certificate_mint_fixture_only_at_the_corner" $ do
      let txInfo = scriptContextTxInfo cornerCertificationTx
      length (txInfoReferenceInputs txInfo) @?= 3
      length (txInfoOutputs txInfo) @?= 1
  , testCase "certificate_mint_cost_at_the_corner" $
      psucceeds $ runMint cornerCertificationTx
  , testCase "certificate_spend_fixture_only" $ do
      let txInfo = scriptContextTxInfo aikenHonestBurnTx
      length (txInfoInputs txInfo) @?= 1
      txInfoSignatories txInfo @?= [PubKeyHash (toBuiltin publisherHash)]
  , testCase "certificate_spend_cost" $
      psucceeds $ runSpend aikenHonestBurnTx
  , testCase "certificate_mint_rejects_a_tampered_corner_chunk" $
      pfails $ runMint tamperedCornerCertificationTx
  ]

--------------------------------------------------------------------------------
-- Certification (§8.6)
--------------------------------------------------------------------------------

certificationTests :: [TestTree]
certificationTests =
  [ -- The honest publication: two raw chunks at the publisher's key address, one
    -- certificate output at the script's own address carrying exactly one token
    -- of the content-addressed name.
    testCase "certifies an honest publication" $
      psucceeds $ runMint honestCertificationTx
  , -- §8.5 puts the certificate at script custody. A certificate at a key
    -- address would be spendable without the burn the spend handler enforces,
    -- so the token could outlive any check the policy made.
    testCase "rejects a certificate output at a foreign address" $
      pfails $
        runMint $
          certificationTx
            (certifyRedeemer [0, 1] 0)
            [honestCertificateOutput {txOutAddress = keyAddress publisherHash}]
            (mintOf assetName 1)
            (chunkInputs publisherHash carriageUtxoId)
  , -- §8.5 pins the whole address, not just its payment credential. Nothing
    -- reads the stake credential, which is exactly why leaving it open would be
    -- free money: the minter could point the deposit's staking rights at their
    -- own reward account on a certification anyone else paid for.
    testCase "rejects a certificate output with a stake credential" $
      pfails $
        runMint $
          certificationTx
            (certifyRedeemer [0, 1] 0)
            [ honestCertificateOutput
                { txOutAddress =
                    Address
                      (ScriptCredential (ScriptHash (toBuiltin certificateScriptHash)))
                      (Just (StakingHash (PubKeyCredential (PubKeyHash (toBuiltin healerHash)))))
                }
            ]
            (mintOf assetName 1)
            (chunkInputs publisherHash carriageUtxoId)
  , -- A reference script on the certificate output is not something any consumer
    -- reads, and refusing it keeps the output's shape exactly what §8.6
    -- describes.
    testCase "rejects a certificate output carrying a reference script" $
      pfails $
        runMint $
          certificationTx
            (certifyRedeemer [0, 1] 0)
            [ honestCertificateOutput
                { txOutReferenceScript = Just (ScriptHash (toBuiltin certificateScriptHash))
                }
            ]
            (mintOf assetName 1)
            (chunkInputs publisherHash carriageUtxoId)
  , -- The manifest is read out of a *reference input* by every consuming step,
    -- so a datum hash would leave the certificate unreadable — the token would
    -- exist and authenticate nothing.
    testCase "rejects a certificate output without an inline datum" $
      pfails $
        runMint $
          certificationTx
            (certifyRedeemer [0, 1] 0)
            [honestCertificateOutput {txOutDatum = NoOutputDatum}]
            (mintOf assetName 1)
            (chunkInputs publisherHash carriageUtxoId)
  , -- §8.6's name is derived from `(tx_id, field_index)` so a consuming step can
    -- require the exact token for the field it is disputing. A token minted
    -- under any other name is a certificate nobody can ask for by name.
    testCase "rejects an asset name that is not the content address" $
      pfails $
        runMint $
          certificationTx
            (certifyRedeemer [0, 1] 0)
            [certificateOutput (certificateFor publisherHash) (certificateValue foreignName 1)]
            (mintOf foreignName 1)
            (chunkInputs publisherHash carriageUtxoId)
  , -- Quantity 1 (§8.6). A second unit of the same name is a certificate that
    -- can be split away from the output whose datum was proved.
    testCase "rejects a quantity other than one" $
      pfails $
        runMint $
          certificationTx
            (certifyRedeemer [0, 1] 0)
            [certificateOutput (certificateFor publisherHash) (certificateValue assetName 2)]
            (mintOf assetName 2)
            (chunkInputs publisherHash carriageUtxoId)
  , -- One certification proves one certificate. A second asset name of this
    -- policy riding along on the same mint would be unexamined by construction,
    -- and could land at the script address with any datum its minter liked.
    testCase "rejects a second token of the policy in the mint" $
      pfails $
        runMint $
          certificationTx
            (certifyRedeemer [0, 1] 0)
            [honestCertificateOutput]
            (mintOf assetName 1 <> mintOf "\xde\xad\xbe\xef" 1)
            (chunkInputs publisherHash carriageUtxoId)
  , -- The certificate output carries the proved token and nothing else of this
    -- policy. A quantity check on the one name says nothing about the names it
    -- was not asked about, so a second name arriving from an input could ride
    -- into the very output whose datum was proved.
    testCase "rejects a second asset name in the certificate output" $
      pfails $
        runMint $
          certificationTx
            (certifyRedeemer [0, 1] 0)
            [ certificateOutput
                (certificateFor publisherHash)
                ( certificateValue assetName 1
                    <> singleton certificatePolicy (TokenName (toBuiltin ("\xde\xad\xbe\xef" :: BS.ByteString))) 1
                )
            ]
            (mintOf assetName 1)
            (chunkInputs publisherHash carriageUtxoId)
  , -- The §8.3 ladder is at most three chunks. A longer index vector cannot
    -- describe a legal preimage, so it is refused before any reference input is
    -- resolved.
    testCase "rejects more chunk indices than the ladder allows" $
      pfails $
        runMint $
          certificationTx
            (certifyRedeemer [0, 1, 0, 1] 0)
            [honestCertificateOutput]
            (mintOf assetName 1)
            (chunkInputs publisherHash carriageUtxoId)
  , -- Carriage that is not the committed preimage. The chunk UTxOs are perfectly
    -- well-formed; their bytes are simply not the ones the transaction id
    -- committed, which is the only thing §8.5 ever trusts about raw carriage.
    testCase "rejects chunks that are not the committed preimage" $
      pfails $
        runMint $
          certificationTx
            (certifyRedeemer [0, 1] 0)
            [honestCertificateOutput]
            (mintOf assetName 1)
            [ chunkInput publisherHash carriageUtxoId 0 (BS.replicate chunkK 0xff)
            , chunkInput publisherHash carriageUtxoId 1 (last (splitRef preimage))
            ]
  , -- `output_index` is positional, and Aiken's `list.at` is total — it answers
    -- `None` rather than aborting — so a negative index would be a silent miss
    -- if the guard were not there.
    testCase "rejects a negative output index" $
      pfails $ runMint (withRedeemer (certifyRedeemer [0, 1] (-1)) honestCertificationTx)
  , -- And past the end of the output list, which is the other side of the same
    -- guard: nothing was proved about any output, so nothing may be minted.
    testCase "rejects an output index past the end" $
      pfails $ runMint (withRedeemer (certifyRedeemer [0, 1] 1) honestCertificationTx)
  , -- The chunk indices are positional too, and `raw_chunk_bytes` keeps its own
    -- guards rather than trusting the caller's. Negative first.
    testCase "rejects a negative chunk index" $
      pfails $ runMint (withRedeemer (certifyRedeemer [-1, 1] 0) honestCertificationTx)
  , -- And past the end of the reference-input list.
    testCase "rejects a chunk index past the end" $
      pfails $ runMint (withRedeemer (certifyRedeemer [0, 7] 0) honestCertificationTx)
  , -- §8.5 carriage is a nothing-but-bytes *inline* datum. A referenced UTxO
    -- carrying a datum hash — or none at all — has no bytes to certify, so the
    -- read fails closed instead of certifying an empty chunk.
    testCase "rejects a chunk without an inline datum" $
      pfails $
        runMint $
          certificationTx
            (certifyRedeemer [0, 1] 0)
            [honestCertificateOutput]
            (mintOf assetName 1)
            [ head (chunkInputs publisherHash carriageUtxoId)
            , (chunkInputs publisherHash carriageUtxoId !! 1)
                { txInInfoResolved =
                    (txInInfoResolved (chunkInputs publisherHash carriageUtxoId !! 1))
                      {txOutDatum = NoOutputDatum}
                }
            ]
  ]

--------------------------------------------------------------------------------
-- §8.7 healing
--------------------------------------------------------------------------------

{- | The permissionless claim, as a transaction.

An unrelated party republishes the same content from the preimage bytes alone —
their own key address, their own UTxOs, their own min-Ada reclaim authority — and
certifies. The policy asks nothing about who they are, and the token they get
carries the same content-addressed name the yanked one had, so every consuming
step that named the old certificate finds the new one.

The decoy output ahead of the certificate is what keeps the positional
@output_index@ doing real work rather than picking the only output there is.
-}
healingTests :: [TestTree]
healingTests =
  [ testCase "the healed certificate is the same content under a different owner" $
      assertBool "digests match and owners differ" $
        certificateField 4 (certificateFor healerHash)
          == certificateField 4 (certificateFor publisherHash)
          && certificateField 0 (certificateFor healerHash)
            /= certificateField 0 (certificateFor publisherHash)
  , testCase "heals a yanked publication from an unrelated party" $
      psucceeds $
        runMint $
          certificationTx
            (certifyRedeemer [0, 1] 1)
            [ TxOut (keyAddress healerHash) (lovelace 2_000_000) NoOutputDatum Nothing
            , certificateOutput (certificateFor healerHash) (certificateValue assetName 1)
            ]
            (mintOf assetName 1)
            (chunkInputs healerHash healedCarriageUtxoId)
  ]

--------------------------------------------------------------------------------
-- Retirement (§8.7)
--------------------------------------------------------------------------------

retirementTests :: [TestTree]
retirementTests =
  [ -- Cleanup is owner-discretionary (§8.7): the owner signs, the token is
    -- burnt, the min-Ada comes back.
    testCase "reclaims min ada when the owner burns the certificate" $
      psucceeds $ runSpend honestBurnTx
  , -- The owner set at mint is the reclaim authority, so an unsigned burn is a
    -- stranger taking the deposit.
    testCase "rejects a burn without the owner signature" $
      pfails $
        runSpend $
          burnTx (Just (certificateFor publisherHash)) [] [PubKeyHash (toBuiltin healerHash)]
  , -- The guard that makes forgery impossible: a certificate token that survives
    -- its own spend could be re-attached to an output whose datum the minting
    -- policy never saw.
    testCase "rejects a surviving certificate token" $
      pfails $
        runSpend $
          burnTx
            (Just (certificateFor publisherHash))
            [certificateOutput (certificateFor publisherHash) (certificateValue assetName 1)]
            [PubKeyHash (toBuiltin publisherHash)]
  , -- A UTxO at this address with no inline datum is not a certificate: nothing
    -- names the asset that has to stop existing or the owner who may authorise
    -- it, and unlocking it would unlock it for anyone.
    testCase "rejects a missing datum" $
      pfails $ runSpend (burnTx Nothing [] [PubKeyHash (toBuiltin publisherHash)])
  , -- The spent UTxO is resolved from the ledger's own `own_ref`, so a
    -- transaction whose inputs do not contain it has nothing for the handler to
    -- read the script hash out of.
    testCase "rejects an own ref that is not an input" $
      pfails $
        runSpend
          honestBurnTx
            { scriptContextTxInfo =
                (scriptContextTxInfo honestBurnTx)
                  { txInfoInputs =
                      [ TxInInfo
                          (TxOutRef (TxId (toBuiltin healedCarriageUtxoId)) 0)
                          honestCertificateOutput
                      ]
                  }
            }
  , -- The burn branch of the policy: it exists so a burn can happen at all, and
    -- it must never be a way around the content proof.
    testCase "the retire redeemer permits a burn" $
      psucceeds $
        runMint $ certificationTx retireRedeemer [] (mintOf assetName (-1)) []
  , -- `Retire` with a positive quantity would be an uncertified certificate.
    testCase "the retire redeemer rejects a mint" $
      pfails $
        runMint $
          certificationTx retireRedeemer [honestCertificateOutput] (mintOf assetName 1) []
  ]

--------------------------------------------------------------------------------
-- Purpose
--------------------------------------------------------------------------------

{- | Aiken's @else(_) { fail }@, applied per handler.

Mint and spend are two terms here because Plutarch has no handler syntax, so each
has to refuse the other's purpose itself — otherwise a spend could be satisfied
by whatever the mint branch happened to check, and the other way round.
-}
purposeTests :: [TestTree]
purposeTests =
  [ testCase "the mint handler refuses a spending purpose" $
      pfails $
        fieldPreimageCertificateMintValidator
          # pconstant honestCertificationTx {scriptContextScriptInfo = SpendingScript certificateRef Nothing}
  , testCase "the spend handler refuses a minting purpose" $
      pfails $
        fieldPreimageCertificateSpendValidator
          # pconstant honestBurnTx {scriptContextScriptInfo = MintingScript certificatePolicy}
  ]

--------------------------------------------------------------------------------
-- Driving the handlers
--------------------------------------------------------------------------------

runMint :: forall (s :: S). ScriptContext -> Term s PUnit
runMint ctx = fieldPreimageCertificateMintValidator # pconstant ctx

runSpend :: forall (s :: S). ScriptContext -> Term s PUnit
runSpend ctx = fieldPreimageCertificateSpendValidator # pconstant ctx

--------------------------------------------------------------------------------
-- The certification transaction
--------------------------------------------------------------------------------

{- | A minting context whose outputs and reference inputs are exactly these, in
exactly this order.

Built through 'buildScriptContext' for the fields nothing reads and then
overridden, because both lists are addressed by position and the builder keeps
inputs sorted by out-ref.
-}
certificationTx :: PD.Data -> [TxOut] -> Value -> [TxInInfo] -> ScriptContext
certificationTx redeemer outputs minted references =
  case buildScriptContext mempty of
    ScriptContext txInfo _ _ ->
      ScriptContext
        txInfo
          { txInfoOutputs = outputs
          , txInfoReferenceInputs = references
          , txInfoMint = UnsafeMintValue (getValue minted)
          }
        (Redeemer (dataToBuiltinData redeemer))
        (MintingScript certificatePolicy)

honestCertificationTx :: ScriptContext
honestCertificationTx =
  certificationTx
    (certifyRedeemer [0, 1] 0)
    [honestCertificateOutput]
    (mintOf assetName 1)
    (chunkInputs publisherHash carriageUtxoId)

-- | The same transaction under a different redeemer.
withRedeemer :: PD.Data -> ScriptContext -> ScriptContext
withRedeemer redeemer (ScriptContext txInfo _ scriptInfo) =
  ScriptContext txInfo (Redeemer (dataToBuiltinData redeemer)) scriptInfo

certifyRedeemer :: [Integer] -> Integer -> PD.Data
certifyRedeemer chunkRefInputIndices outputIndex =
  PD.Constr
    0
    [ PD.B compactCbor
    , PD.B witnessSetCbor
    , PD.List (map PD.I chunkRefInputIndices)
    , PD.I outputIndex
    ]

retireRedeemer :: PD.Data
retireRedeemer = PD.Constr 1 []

--------------------------------------------------------------------------------
-- The retirement transaction
--------------------------------------------------------------------------------

{- | A spending context over the certificate UTxO.

The datum is the one the ledger hands the handler, so @Nothing@ is the
datumless UTxO the last retirement test drives.
-}
burnTx :: Maybe PD.Data -> [TxOut] -> [PubKeyHash] -> ScriptContext
burnTx datum outputs signatories =
  case buildScriptContext mempty of
    ScriptContext txInfo _ _ ->
      ScriptContext
        txInfo
          { txInfoInputs = [TxInInfo certificateRef honestCertificateOutput]
          , txInfoOutputs = outputs
          , txInfoSignatories = signatories
          , txInfoMint = UnsafeMintValue (getValue (mintOf assetName (-1)))
          }
        (Redeemer (dataToBuiltinData (PD.I 0)))
        (SpendingScript certificateRef (fmap (Datum . dataToBuiltinData) datum))

honestBurnTx :: ScriptContext
honestBurnTx =
  burnTx (Just (certificateFor publisherHash)) [] [PubKeyHash (toBuiltin publisherHash)]

--------------------------------------------------------------------------------
-- Parties, addresses and out-refs
--------------------------------------------------------------------------------

-- | The certificate script hash: both the policy id and the address's credential.
certificateScriptHash :: BS.ByteString
certificateScriptHash = BS.replicate 28 0x22

certificatePolicy :: CurrencySymbol
certificatePolicy = CurrencySymbol (toBuiltin certificateScriptHash)

-- | The original publisher, and the min-Ada reclaim authority they set.
publisherHash :: BS.ByteString
publisherHash = BS.replicate 28 0x66

{- | An unrelated party. Never signs anything the publisher signed, holds no
certificate, and has no relationship to the original carriage — which is the
point of the healing test.
-}
healerHash :: BS.ByteString
healerHash = BS.replicate 28 0x77

carriageUtxoId, healedCarriageUtxoId, certificateUtxoId :: BS.ByteString
carriageUtxoId = BS.replicate 32 0x44
healedCarriageUtxoId = BS.replicate 32 0x55
certificateUtxoId = BS.replicate 32 0x33

certificateRef :: TxOutRef
certificateRef = TxOutRef (TxId (toBuiltin certificateUtxoId)) 0

keyAddress :: BS.ByteString -> Address
keyAddress hash = Address (PubKeyCredential (PubKeyHash (toBuiltin hash))) Nothing

--------------------------------------------------------------------------------
-- Carriage and the certificate output
--------------------------------------------------------------------------------

{- | §8.5: raw carriage is a nothing-but-bytes inline datum at the publisher's own
key address. Any address, any key — the bytes are what get checked.
-}
chunkInput :: BS.ByteString -> BS.ByteString -> Integer -> BS.ByteString -> TxInInfo
chunkInput publisher utxoId index chunk =
  TxInInfo
    (TxOutRef (TxId (toBuiltin utxoId)) index)
    ( TxOut
        (keyAddress publisher)
        (lovelace 2_000_000)
        (OutputDatum (Datum (dataToBuiltinData (PD.B chunk))))
        Nothing
    )

chunkInputs :: BS.ByteString -> BS.ByteString -> [TxInInfo]
chunkInputs publisher utxoId =
  zipWith (chunkInput publisher utxoId) [0 ..] (splitRef preimage)

certificateOutput :: PD.Data -> Value -> TxOut
certificateOutput datum value =
  TxOut
    (Address (ScriptCredential (ScriptHash (toBuiltin certificateScriptHash))) Nothing)
    value
    (OutputDatum (Datum (dataToBuiltinData datum)))
    Nothing

honestCertificateOutput :: TxOut
honestCertificateOutput =
  certificateOutput (certificateFor publisherHash) (certificateValue assetName 1)

certificateValue :: BS.ByteString -> Integer -> Value
certificateValue name quantity = lovelace 2_000_000 <> mintOf name quantity

-- | A value of this policy alone — the shape a mint field takes, Ada excluded.
mintOf :: BS.ByteString -> Integer -> Value
mintOf name quantity = singleton certificatePolicy (TokenName (toBuiltin name)) quantity

lovelace :: Integer -> Value
lovelace = singleton adaSymbol adaToken

--------------------------------------------------------------------------------
-- The committed transaction, and the certificate over it
--------------------------------------------------------------------------------

{- | §8.4: chunk @j@ is bytes @[j·K, (j+1)·K)@, ragged last chunk, minimum chunks.

Written from the rule rather than taken from the port, because the whole value of
the rule is that two implementations that never met agree about it.
-}
splitRef :: BS.ByteString -> [BS.ByteString]
splitRef bytes
  | BS.length bytes <= chunkK = [bytes]
  | otherwise = BS.take chunkK bytes : splitRef (BS.drop chunkK bytes)

-- | @native_tx_field_access_v1.chunk_bytes_k@.
chunkK :: Int
chunkK = 15900

{- | One byte over @K@: the smallest preimage that is legally tier 3, so the
fixture is the cheapest transaction that can carry a certificate at all.
-}
preimage :: BS.ByteString
preimage = BS.pack [fromIntegral (i `mod` 251) | i <- [0 .. chunkK]]

{- | The disputed field. Field 8 is the redeemer-witness slot, which the fixture
commits through the witness set — so the preimage is bound without rebuilding the
committed body.
-}
disputedField :: Integer
disputedField = 8

witnessSetCbor :: BS.ByteString
witnessSetCbor =
  witnessSetCborFrom (BS.replicate 32 0x01, BS.replicate 32 0x02, blake2b256 preimage)

compactCbor :: BS.ByteString
compactCbor = compactWith tx1 (blake2b256 witnessSetCbor)

disputedTxId :: BS.ByteString
disputedTxId = txIdOf tx1

{- | §8.6's deterministic name: @blake2b_256(field_index ‖ tx_id)@.

Derived from the certificate's own @(tx_id, field_index)@ and from nothing the
publisher chooses, which is what makes a republished certificate answer to the
name the yanked one had — the healing test is that claim as a transaction.
-}
assetName :: BS.ByteString
assetName = blake2b256 (BS.singleton (fromIntegral disputedField) <> disputedTxId)

-- | The name of a certificate for the next field along — a real name, wrong token.
foreignName :: BS.ByteString
foreignName = blake2b256 (BS.singleton (fromIntegral (disputedField - 1)) <> disputedTxId)

chunkDigests :: [BS.ByteString]
chunkDigests = map blake2b256 (splitRef preimage)

-- | The §8.8 wire record, as the ledger encodes it.
certificateFor :: BS.ByteString -> PD.Data
certificateFor owner =
  PD.Constr
    0
    [ PD.B owner
    , PD.B disputedTxId
    , PD.I disputedField
    , PD.I (fromIntegral (BS.length preimage))
    , PD.List (map PD.B chunkDigests)
    ]

-- | The maximum-profile fixtures below reproduce the Aiken suite byte for byte.
aikenSmallPreimage, cornerPreimage :: BS.ByteString
aikenSmallPreimage = repeatingSeed (chunkK + 1) [0xa5, 0xc3, 0x0f]
cornerPreimage = repeatingSeed 32_763 [0xb7, 0xe2, 0x14]

repeatingSeed :: Int -> [Word8] -> BS.ByteString
repeatingSeed length' seed = BS.pack (take length' (cycle seed))

zeroHash32 :: BS.ByteString
zeroHash32 = BS.replicate 32 0

aikenWitnessSetCbor :: BS.ByteString
aikenWitnessSetCbor = witnessSetCborFrom (zeroHash32, zeroHash32, zeroHash32)

aikenBodyCbor :: BS.ByteString -> BS.ByteString
aikenBodyCbor fieldPreimage =
  BS.concat
    [ "\x8c"
    , defBytes32 (blake2b256 fieldPreimage)
    , defBytes32 zeroHash32
    , defBytes32 zeroHash32
    , cborInt 0
    , cborInt 0
    , cborInt 0
    , defBytes32 zeroHash32
    , defBytes32 zeroHash32
    , defBytes32 zeroHash32
    , defBytes32 zeroHash32
    , defBytes32 zeroHash32
    , cborInt 255
    ]

aikenCompactCbor :: BS.ByteString -> BS.ByteString
aikenCompactCbor fieldPreimage =
  BS.concat
    [ "\x84"
    , cborInt 1
    , aikenBodyCbor fieldPreimage
    , defBytes32 (blake2b256 aikenWitnessSetCbor)
    , cborInt 0
    ]

aikenTxId :: BS.ByteString -> BS.ByteString
aikenTxId fieldPreimage =
  blake2b256 ("MidgardNativeTxBodyV1" <> cborInt 1 <> aikenBodyCbor fieldPreimage)

aikenDisputedField :: Integer
aikenDisputedField = 0

aikenAssetName :: BS.ByteString -> BS.ByteString
aikenAssetName fieldPreimage =
  blake2b256 (BS.singleton (fromIntegral aikenDisputedField) <> aikenTxId fieldPreimage)

chunkDigestsFor :: BS.ByteString -> [BS.ByteString]
chunkDigestsFor = map blake2b256 . splitRef

aikenCertificateFor :: BS.ByteString -> BS.ByteString -> PD.Data
aikenCertificateFor owner fieldPreimage =
  PD.Constr
    0
    [ PD.B owner
    , PD.B (aikenTxId fieldPreimage)
    , PD.I aikenDisputedField
    , PD.I (fromIntegral (BS.length fieldPreimage))
    , PD.List (map PD.B (chunkDigestsFor fieldPreimage))
    ]

aikenCertifyRedeemer :: BS.ByteString -> [Integer] -> PD.Data
aikenCertifyRedeemer fieldPreimage chunkIndices =
  PD.Constr
    0
    [ PD.B (aikenCompactCbor fieldPreimage)
    , PD.B aikenWitnessSetCbor
    , PD.List (map PD.I chunkIndices)
    , PD.I 0
    ]

aikenChunkInputs :: BS.ByteString -> [TxInInfo]
aikenChunkInputs fieldPreimage =
  zipWith (chunkInput publisherHash carriageUtxoId) [0 ..] (splitRef fieldPreimage)

aikenCertificateOutput :: BS.ByteString -> TxOut
aikenCertificateOutput fieldPreimage =
  certificateOutput
    (aikenCertificateFor publisherHash fieldPreimage)
    (certificateValue (aikenAssetName fieldPreimage) 1)

aikenCertificationTxWith :: BS.ByteString -> [TxInInfo] -> ScriptContext
aikenCertificationTxWith fieldPreimage references =
  certificationTx
    (aikenCertifyRedeemer fieldPreimage [0 .. fromIntegral (length references) - 1])
    [aikenCertificateOutput fieldPreimage]
    (mintOf (aikenAssetName fieldPreimage) 1)
    references

aikenSmallCertificationTx, cornerCertificationTx, tamperedCornerCertificationTx :: ScriptContext
aikenSmallCertificationTx =
  aikenCertificationTxWith aikenSmallPreimage (aikenChunkInputs aikenSmallPreimage)
cornerCertificationTx =
  aikenCertificationTxWith cornerPreimage (aikenChunkInputs cornerPreimage)
tamperedCornerCertificationTx =
  aikenCertificationTxWith cornerPreimage $ case cornerChunks of
    [first, second, final] ->
      zipWith
        (chunkInput publisherHash carriageUtxoId)
        [0 ..]
        [first, second, BS.init final <> "\xff"]
    _ -> error "corner fixture must have exactly three chunks"

cornerChunks :: [BS.ByteString]
cornerChunks = splitRef cornerPreimage

aikenHonestBurnTx :: ScriptContext
aikenHonestBurnTx =
  case buildScriptContext mempty of
    ScriptContext txInfo _ _ ->
      ScriptContext
        txInfo
          { txInfoInputs =
              [TxInInfo certificateRef (aikenCertificateOutput aikenSmallPreimage)]
          , txInfoOutputs = []
          , txInfoSignatories = [PubKeyHash (toBuiltin publisherHash)]
          , txInfoMint =
              UnsafeMintValue
                (getValue (mintOf (aikenAssetName aikenSmallPreimage) (-1)))
          }
        (Redeemer (dataToBuiltinData (PD.I 0)))
        ( SpendingScript
            certificateRef
            ( Just
                ( Datum
                    ( dataToBuiltinData
                        (aikenCertificateFor publisherHash aikenSmallPreimage)
                    )
                )
            )
        )

-- | One field of an encoded certificate, by position.
certificateField :: Int -> PD.Data -> PD.Data
certificateField index (PD.Constr _ fields) = fields !! index
certificateField _ _ = error "not a certificate"
