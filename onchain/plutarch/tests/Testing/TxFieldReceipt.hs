{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}

{- |
Module      : Testing.TxFieldReceipt
Description : Behavioural tests for the ported half of
              @lib/midgard/user-events/tx-field-receipt-v1.ak@.

@BurnReceipts@ walks two lists in lockstep: the receipt policy's entries in
@tx.mint@, and the input indices the redeemer supplies. Everything these tests
probe is a way for those two to come apart — a burnt name with no receipt behind
it, a receipt counted twice, an input that is not the one the burnt token came
from — or a way for a receipt to be destroyed while the order it belongs to
survives.

The mint's token order is what the index list has to follow, so the two-receipt
fixtures read that order back out of the value they build rather than assuming
it. That is not incidental: a redeemer whose indices are in any other order is
rejected, and one of the tests says so.
-}
module Testing.TxFieldReceipt (tests) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (
  CurrencySymbol (..),
  TokenName (..),
  Value,
  getValue,
  singleton,
 )
import PlutusLedgerApi.V3 (
  Credential (..),
  Datum (..),
  OutputDatum (..),
  Redeemer (..),
  ScriptContext (..),
  ScriptHash (..),
  ScriptInfo (..),
  ToData,
  TxId (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
  toBuiltinData,
 )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (builtinDataToData, dataToBuiltinData, fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.LedgerApi.V3 (PMintValue, PTxInInfo)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.BoundedCollection qualified as BoundedCollection
import Midgard.BoundedItem qualified as BoundedItem
import Midgard.FraudProofs.NativeTx.Compact qualified as NativeCompact
import Midgard.FraudProofs.NativeTx.Components qualified as NativeComponents
import Midgard.FraudProofs.NativeTx.Transaction qualified as NativeTransaction
import Midgard.FraudProofs.NativeTx.Types qualified as NativeTypes
import Midgard.LedgerState (PNativeTxProofSourceV1 (..))
import Midgard.NativeTxFieldAccess qualified as FieldAccess
import Midgard.UserEvents.TxFieldReceipt (PMintRedeemer (..), pvalidateBurnReceipts)
import Midgard.ValidationMerkle qualified as ValidationMerkle
import Midgard.Validators.TxFieldReceipt (txFieldReceiptMintValidator)
import Testing.Eval (passertEval, pfails, psucceeds)
import Testing.ScriptContextBuilder (buildScriptContext, currencySymbolFromHex, mkAdaValue, withMintingScript)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Tx Field Receipt Burn Tests"
    [ aikenPublicationParityTests
    , testGroup
        "validateBurnReceipts"
        [ testCase "accepts one receipt burnt against its own input" $
            holds $ run defaultBurn
        , testCase "accepts two receipts burnt in the mint's token order" $
            holds $ run twoReceipts
        , -- The branch must do something. Burning nothing under this policy
          -- would let it be attached to any transaction at all.
          testCase "rejects a burn of no tokens" $
            pfails $ run defaultBurn {bReceipts = [], bIndices = Just []}
        , -- One receipt UTxO cannot answer for two burnt names.
          testCase "rejects repeated input indices" $
            pfails $ run twoReceipts {bIndices = Just [0, 0]}
        , -- The lists are consumed together, so a mismatch in either direction
          -- is a rejection.
          testCase "rejects fewer indices than burnt tokens" $
            pfails $ run twoReceipts {bIndices = Just [0]}
        , testCase "rejects more indices than burnt tokens" $
            pfails $ run defaultBurn {bIndices = Just [0, 1]}
        , -- The redeemer pairs positionally with the mint's token order.
          testCase "rejects indices given in the wrong order" $
            pfails $ run twoReceipts {bIndices = Just [1, 0]}
        , -- `== -1` exactly: a receipt is a one-off NFT.
          testCase "rejects a mint where the receipt quantity is positive" $
            pfails $ run defaultBurn {bBurnQty = 1}
        , testCase "rejects a mint where the receipt quantity is minus two" $
            pfails $ run defaultBurn {bBurnQty = -2}
        , -- The input has to be a receipt UTxO.
          testCase "rejects an input at a different script" $
            pfails $ run defaultBurn {bInputScript = Just otherScriptHash}
        , testCase "rejects an input with no inline datum" $
            pfails $ run defaultBurn {bInlineDatum = False}
        , testCase "rejects an input carrying a reference script" $
            pfails $ run defaultBurn {bReferenceScript = True}
        , testCase "rejects a receipt datum naming a different receipt policy" $
            pfails $ run defaultBurn {bDatumPolicy = Just otherPolicy}
        , -- The burnt name must be the one this receipt's coordinates hash to,
          -- which is what stops a receipt being retired under another chunk's
          -- name.
          testCase "rejects a burnt name that is not the datum's own" $
            pfails $ run defaultBurn {bBurnName = Just (receiptName defaultCoords {cChunk = 9})}
        , -- ...and the input must actually hold the token that was burnt.
          testCase "rejects an input holding a different receipt token" $
            pfails $ run defaultBurn {bHeldName = Just (receiptName defaultCoords {cChunk = 9})}
        , testCase "rejects an input holding two of the receipt token" $
            pfails $ run defaultBurn {bHeldQty = 2}
        , testCase "rejects an input holding no token of this policy" $
            pfails $ run defaultBurn {bHeldQty = 0}
        , -- The tie back to the order: a receipt may only go when the order it
          -- acknowledges goes.
          testCase "rejects a burn that leaves the order NFT alone" $
            pfails $ run defaultBurn {bBurnOrder = False}
        , testCase "rejects a burn that mints the order NFT instead" $
            pfails $ run defaultBurn {bOrderQty = 1}
        ]
    , testGroup "MintRedeemer wire format" redeemerWireTests
    , testGroup "the mint validator" validatorTests
    ]

--------------------------------------------------------------------------------
-- Aiken publication-gate parity
--------------------------------------------------------------------------------

aikenPublicationParityTests :: TestTree
aikenPublicationParityTests =
  testGroup
    "tx-field-receipt-v1 Aiken parity"
    [ testCase "the_counted_opening_is_internally_well_formed" $
        holds $ P.do
          BoundedCollection.PItemProofV1 _ collectionFieldIndex _ collectionItemIndex collectionItemLength itemCommitment _ _ <-
            pmatch receiptCollectionProofT
          BoundedItem.PChunkProofV1 _ chunkFieldIndex chunkItemIndex chunkTotalLength _ _ _ _ <-
            pmatch receiptChunkProofT
          BoundedItem.pverifyChunk # pfromData itemCommitment # receiptChunkProofT
            #&& (pfromData collectionItemLength #== pfromData chunkTotalLength)
            #&& (pfromData collectionFieldIndex #== pfromData chunkFieldIndex)
            #&& (pfromData collectionItemIndex #== pfromData chunkItemIndex)
    , testCase "counted_item_proof_cannot_open_a_flat_field_commitment" $
        holds $
          pnot
            #$ BoundedCollection.pverifyBoundedCollectionItem
            # (pblake2b_256 # receiptAddressWitnessPreimage)
            # receiptCollectionProofT
    , testCase "publish_field_gate_is_unsatisfiable" $
        holds $
          pnot
            #$ NativeTransaction.pverifyMidgardTransactionFieldChunkV1
            # receiptSourceTransactionId
            # receiptSourceCommitment
            # receiptSourceCompactCbor
            # receiptSourceWitnessSetCbor
            # receiptSourceLengthsCbor
            # receiptCollectionProofT
            # receiptChunkProofT
    , testCase "the_same_field_still_opens_through_the_door" $
        holds $ P.do
          PPair verified witnessSet <-
            pmatch
              ( NativeCompact.pverifyNativeTxProofSourceV1
                  # receiptSourceTransactionId
                  # receiptSourceCompactCbor
                  # receiptSourceWitnessSetCbor
                  # receiptSourceLengthsCbor
              )
          view <-
            plet $
              FieldAccess.pauthenticatedFieldView
                # verified
                # witnessSet
                # 7
                # pcon (FieldAccess.PInline (pdata receiptAddressWitnessPreimage))
                # pnil
                # pdata (pconstant receiptCertificatePolicy)
          (FieldAccess.pfieldItemCount # view #== 1)
            #&& (FieldAccess.pfieldItemAt # view # 0 #== receiptAddressWitnessItem)
    ]

receiptAddressWitnessItem :: forall s. Term s PByteString
receiptAddressWitnessItem =
  NativeComponents.pencodeMidgardAddressWitness
    # pcon
      ( NativeTypes.PMidgardAddressWitness
          (pdata (pblake2b_256 # pconstant "\x01"))
          (pdata ((pblake2b_256 # pconstant "\x02") <> (pblake2b_256 # pconstant "\x03")))
      )

receiptAddressWitnessPreimage :: forall s. Term s PByteString
receiptAddressWitnessPreimage =
  FieldAccess.pencodeFieldPreimage # (pcons # receiptAddressWitnessItem # pnil)

receiptItemFrontier :: forall s. Term s (PBuiltinList (PAsData ValidationMerkle.PFrontierPeak))
receiptItemFrontier =
  singletonFrontier $
    BoundedItem.phashChunk # 7 # 0 # 0 # receiptAddressWitnessItem

receiptItemCommitment :: forall s. Term s PByteString
receiptItemCommitment =
  BoundedItem.pcommitment
    # 7
    # 0
    # (plengthBS # receiptAddressWitnessItem)
    # receiptItemFrontier

receiptCollectionFrontier :: forall s. Term s (PBuiltinList (PAsData ValidationMerkle.PFrontierPeak))
receiptCollectionFrontier =
  singletonFrontier $
    BoundedCollection.phashBoundedCollectionItem
      # 7
      # 0
      # (plengthBS # receiptAddressWitnessItem)
      # receiptItemCommitment

receiptCollectionProofT :: forall s. Term s BoundedCollection.PItemProofV1
receiptCollectionProofT =
  pcon $
    BoundedCollection.PItemProofV1
      (pdata BoundedCollection.pboundedCollectionVersion)
      (pdata 7)
      (pdata 1)
      (pdata 0)
      (pdata (plengthBS # receiptAddressWitnessItem))
      (pdata receiptItemCommitment)
      (pdata receiptCollectionFrontier)
      (pdata pnil)

receiptChunkProofT :: forall s. Term s BoundedItem.PChunkProofV1
receiptChunkProofT =
  pcon $
    BoundedItem.PChunkProofV1
      (pdata BoundedItem.pversion)
      (pdata 7)
      (pdata 0)
      (pdata (plengthBS # receiptAddressWitnessItem))
      (pdata 0)
      (pdata receiptAddressWitnessItem)
      (pdata receiptItemFrontier)
      (pdata pnil)

singletonFrontier :: forall s. Term s PByteString -> Term s (PBuiltinList (PAsData ValidationMerkle.PFrontierPeak))
singletonFrontier leaf =
  pmatch (ValidationMerkle.pbuildFrontier # (pcons # pdata leaf # pnil)) $
    \(ValidationMerkle.PBuiltFrontier _ peaks) -> peaks

receiptSourceWitnessSet :: forall s. Term s NativeTypes.PNativeTxWitnessSetCompact
receiptSourceWitnessSet =
  pcon $
    NativeTypes.PNativeTxWitnessSetCompact
      (pdata (pblake2b_256 # receiptAddressWitnessPreimage))
      (pdata receiptZeroHash)
      (pdata receiptZeroHash)

receiptSourceBody :: forall s. Term s NativeTypes.PNativeTxBodyCompact
receiptSourceBody =
  pcon $
    NativeTypes.PNativeTxBodyCompact
      receiptZeroHash
      receiptZeroHash
      receiptZeroHash
      0
      (-1)
      (-1)
      receiptZeroHash
      receiptZeroHash
      receiptZeroHash
      receiptZeroHash
      receiptZeroHash
      255

receiptSourceCompact :: forall s. Term s NativeTypes.PNativeTxCompact
receiptSourceCompact =
  pcon $
    NativeTypes.PNativeTxCompact
      receiptSourceBody
      (pblake2b_256 # receiptSourceWitnessSetCbor)
      0

receiptSourceLengths :: forall s. Term s NativeTypes.PNativeTxFieldPreimageLengthsV1
receiptSourceLengths =
  pcon $
    NativeTypes.PNativeTxFieldPreimageLengthsV1
      1
      1
      1
      1
      1
      1
      (plengthBS # receiptAddressWitnessPreimage)
      1
      1

receiptSourceWitnessSetCbor :: forall s. Term s PByteString
receiptSourceWitnessSetCbor =
  NativeCompact.pencodeNativeTxWitnessSetCompact # receiptSourceWitnessSet

receiptSourceCompactCbor :: forall s. Term s PByteString
receiptSourceCompactCbor =
  NativeCompact.pencodeNativeTxCompactV1 # receiptSourceCompact

receiptSourceLengthsCbor :: forall s. Term s PByteString
receiptSourceLengthsCbor =
  NativeCompact.pencodeNativeTxFieldPreimageLengthsV1 # receiptSourceLengths

receiptSourceTransactionId :: forall s. Term s PByteString
receiptSourceTransactionId =
  NativeCompact.pnativeTxIdForVersion
    # 1
    # (NativeCompact.pencodeNativeTxBodyCompact # receiptSourceBody)

receiptSourceCommitment :: forall s. Term s PByteString
receiptSourceCommitment =
  NativeCompact.pnativeTxProofCommitmentV1
    # receiptSourceCompactCbor
    # receiptSourceWitnessSetCbor
    # receiptSourceLengthsCbor

receiptZeroHash :: forall s. Term s PByteString
receiptZeroHash = pconstant (BS.replicate 32 0)

receiptCertificatePolicy :: CurrencySymbol
receiptCertificatePolicy = currencySymbolFromHex (concat (replicate 28 "22"))

--------------------------------------------------------------------------------
-- The redeemer
--------------------------------------------------------------------------------

{- | The policy's redeemer is an interface an SDK builds against, so its
constructor tags and field order are written out from the Aiken declaration
rather than read back from the port. @PublishField@ is declared even though it
cannot succeed: a type with one arm missing would shift @BurnReceipts@ to tag 0
and every burn built off-chain would decode as something else.
-}
redeemerWireTests :: [TestTree]
redeemerWireTests =
  [ testCase "BurnReceipts is Constr 1 wrapping the index list" $
      holds $ pencodes (burnReceiptsT [0, 1]) (PD.Constr 1 [PD.List [PD.I 0, PD.I 1]])
  , testCase "PublishField is Constr 0 with five fields in declaration order" $
      holds $ pencodes publishFieldT publishFieldData
  , -- The nested proof source is a `Constr 0` record, not three bare fields
    -- spliced into the redeemer. Stated as a refusal so the case cannot pass by
    -- agreeing with itself.
    testCase "the proof source is not spliced flat into the redeemer" $
      holds $
        pnot
          #$ pencodes
            publishFieldT
            ( PD.Constr
                0
                [ PD.I 0
                , PD.I (-1)
                , PD.I 0
                , PD.B (BS.replicate 32 0x5a)
                , PD.B "\x84"
                , PD.B "\x83"
                , PD.B "\x89"
                ]
            )
  ]

publishFieldData :: PD.Data
publishFieldData =
  PD.Constr
    0
    [ PD.I 0
    , PD.I (-1)
    , PD.I 0
    , PD.B (BS.replicate 32 0x5a)
    , PD.Constr 0 [PD.B "\x84", PD.B "\x83", PD.B "\x89"]
    ]

{- | Built with 'pcon', so the encoding under test is the one the type derives —
coercing the expected @Data@ into the type and back would agree with itself
whatever the type said.
-}
publishFieldT :: forall s. Term s PMintRedeemer
publishFieldT =
  pcon
    ( PPublishField
        { ppublishField'fieldReferenceInputIndex = pdata 0
        , ppublishField'predecessorReceiptReferenceInputIndex = pdata (-1)
        , ppublishField'receiptOutputIndex = pdata 0
        , ppublishField'transactionId = pdata (pconstant (BS.replicate 32 0x5a))
        , ppublishField'source =
            pdata
              ( pcon
                  ( PNativeTxProofSourceV1
                      { pnativeSource'compactCbor = pdata (pconstant "\x84")
                      , pnativeSource'witnessSetCompactCbor = pdata (pconstant "\x83")
                      , pnativeSource'fieldPreimageLengthsCbor = pdata (pconstant "\x89")
                      }
                  )
              )
        }
    )

burnReceiptsT :: forall s. [Integer] -> Term s PMintRedeemer
burnReceiptsT indices =
  pcon (PBurnReceipts {pburnReceipts'receiptInputIndices = pdata (pconstant indices)})

pencodes :: forall a s. (PIsData a) => Term s a -> PD.Data -> Term s PBool
pencodes value expected = pforgetData (pdata value) #== pconstant expected

--------------------------------------------------------------------------------
-- The validator
--------------------------------------------------------------------------------

{- | The validator's own surface: which purpose reaches the dispatch, which arm
of the redeemer it dispatches to, and that the reachable arm is the branch the
group above already covers guard by guard.

@PublishField@ runs the full ported branch. The compact bytes in this small
dispatch fixture are deliberately malformed, so it fails before publication;
the positive counted-source and receipt-chain predicates are covered in
"Testing.NativeTxTransaction" and "Testing.TxOrderFields".
-}
validatorTests :: [TestTree]
validatorTests =
  [ testCase "a BurnReceipts mint reaching the branch succeeds" $
      psucceeds $ pvalidator (mintContext defaultBurn (burnReceiptsData (burnIndices defaultBurn)))
  , testCase "two receipts burnt in the mint's token order succeed" $
      psucceeds $ pvalidator (mintContext twoReceipts (burnReceiptsData (burnIndices twoReceipts)))
  , testCase "a BurnReceipts mint whose indices are crossed fails" $
      pfails $ pvalidator (mintContext twoReceipts (burnReceiptsData [1, 0]))
  , testCase "a BurnReceipts mint that leaves the order NFT alone fails" $
      pfails $
        pvalidator
          ( mintContext
              defaultBurn {bBurnOrder = False}
              (burnReceiptsData (burnIndices defaultBurn))
          )
  , -- This fixture carries placeholder compact bytes and must fail closed.
    testCase "a PublishField mint fails" $
      pfails $ pvalidator (mintContext defaultBurn publishFieldData)
  , -- `else(_) { fail }`.
    testCase "a spending purpose fails" $
      pfails $
        pvalidator (asSpending (mintContext defaultBurn (burnReceiptsData (burnIndices defaultBurn))))
  , testCase "a rewarding purpose fails" $
      pfails $
        pvalidator (asRewarding (mintContext defaultBurn (burnReceiptsData (burnIndices defaultBurn))))
  ]

burnReceiptsData :: [Integer] -> PD.Data
burnReceiptsData indices = PD.Constr 1 [PD.List (map PD.I indices)]

pvalidator :: forall s. ScriptContext -> Term s PUnit
pvalidator ctx =
  txFieldReceiptMintValidator
    # pdata (pconstant preimageScriptHash)
    # pdata (pconstant receiptScriptHash)
    # pconstant ctx

{- | A minting context under the receipt policy, with this burn's inputs and
mint in place. Built through 'buildScriptContext' and then overridden, because
the inputs are addressed by position.
-}
mintContext :: Burn -> PD.Data -> ScriptContext
mintContext b redeemer =
  case buildScriptContext (withMintingScript (burnMint b) (dataToBuiltinData redeemer)) of
    ScriptContext txInfo _ _ ->
      ScriptContext
        txInfo
          { txInfoInputs = burnInputs b
          , txInfoMint = UnsafeMintValue (getValue (burnMint b))
          }
        (Redeemer (dataToBuiltinData redeemer))
        (MintingScript receiptPolicy)

asSpending :: ScriptContext -> ScriptContext
asSpending (ScriptContext txInfo r _) =
  ScriptContext txInfo r (SpendingScript (TxOutRef (TxId (toBuiltin (BS.replicate 32 0x01))) 0) Nothing)

asRewarding :: ScriptContext -> ScriptContext
asRewarding (ScriptContext txInfo r _) =
  ScriptContext txInfo r (RewardingScript (ScriptCredential receiptScriptHash))

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

holds :: (forall s. Term s PBool) -> Assertion
holds = passertEval

{- | A burn of some set of receipts.

Each knob names one way a transaction can fail to account for what it destroys.
-}
data Burn = Burn
  { bReceipts :: [Coords]
  -- ^ The receipts whose tokens are burnt, one input each.
  , bIndices :: Maybe [Integer]
  -- ^ Redeemer indices; defaults to the mint's token order.
  , bBurnQty :: Integer
  , bOrderQty :: Integer
  , bBurnOrder :: Bool
  , bBurnName :: Maybe TokenName
  -- ^ Overrides the name burnt for the first receipt.
  , bHeldName :: Maybe TokenName
  -- ^ Overrides the name the first receipt's input holds.
  , bHeldQty :: Integer
  , bInputScript :: Maybe ScriptHash
  , bInlineDatum :: Bool
  , bReferenceScript :: Bool
  , bDatumPolicy :: Maybe CurrencySymbol
  }

defaultBurn :: Burn
defaultBurn =
  Burn
    { bReceipts = [defaultCoords]
    , bIndices = Nothing
    , bBurnQty = -1
    , bOrderQty = -1
    , bBurnOrder = True
    , bBurnName = Nothing
    , bHeldName = Nothing
    , bHeldQty = 1
    , bInputScript = Nothing
    , bInlineDatum = True
    , bReferenceScript = False
    , bDatumPolicy = Nothing
    }

twoReceipts :: Burn
twoReceipts = defaultBurn {bReceipts = [defaultCoords, defaultCoords {cChunk = 2}]}

{- | Builds the transaction and runs the branch.

The inputs are laid out in the mint's own token order, so the default index list
is @[0 .. n-1]@ and a test that wants a different pairing says so explicitly.
-}
run :: forall s. Burn -> Term s PBool
run b =
  pvalidateBurnReceipts
    # pdata (pconstant receiptScriptHash)
    # pdata (pconstant receiptPolicy)
    # inputsT (burnInputs b)
    # mintT b
    # indicesT (burnIndices b)

{- | The mint field as the on-chain code sees it.

Not @pconstant@: @PMintValue@'s 'PLiftable' instance goes through
@DeriveDataPLiftable (PAsData PMintValue)@, so @pconstant@ hands back a term
that is still a @Data@ @Map@ while its type claims to be the unwrapped
association list. Every read of it then fails on a @case@ over @Data@. Unwrapping
with @pasMap@ here is what @pfromData@ does for the real transaction field.
-}
mintT :: forall s. Burn -> Term s PMintValue
mintT b = punsafeCoerce (pasMap # pconstant @PData (toPD (burnMint b)))

-- | The name burnt for each receipt, with the first one overridable.
burnNames :: Burn -> [TokenName]
burnNames b = case (bBurnName b, bReceipts b) of
  (Just n, _ : rest) -> n : map receiptName rest
  _ -> map receiptName (bReceipts b)

burnMint :: Burn -> Value
burnMint b =
  mconcat
    [ mconcat [singleton receiptPolicy n (bBurnQty b) | n <- burnNames b]
    , if bBurnOrder b then singleton orderPolicy orderNonce (bOrderQty b) else mempty
    ]

-- | The order the on-chain map presents the burnt tokens in.
mintOrder :: Burn -> [TokenName]
mintOrder b = maybe [] Map.keys (Map.lookup receiptPolicy (getValue (burnMint b)))

burnInputs :: Burn -> [TxInInfo]
burnInputs b = [receiptInput b c | n <- mintOrder b, c <- byBurnName b n]

byBurnName :: Burn -> TokenName -> [Coords]
byBurnName b n = [c | (n', c) <- zip (burnNames b) (bReceipts b), n' == n]

burnIndices :: Burn -> [Integer]
burnIndices b = maybe [0 .. fromIntegral (length (burnInputs b)) - 1] id (bIndices b)

receiptInput :: Burn -> Coords -> TxInInfo
receiptInput b c =
  TxInInfo
    (TxOutRef (TxId (toBuiltin (BS.replicate 32 0x88))) (cChunk c))
    ( TxOut
        (scriptHashAddress (maybe receiptScriptHash id (bInputScript b)))
        ( mkAdaValue 2_000_000
            <> singleton receiptPolicy (maybe (receiptName c) id (bHeldName b)) heldQty
        )
        ( if bInlineDatum b
            then OutputDatum (Datum (dataToBuiltinData (receiptDatum b c)))
            else NoOutputDatum
        )
        (if bReferenceScript b then Just otherScriptHash else Nothing)
    )
  where
    -- Only the first receipt's held quantity is overridable; the rest stay
    -- well-formed so a negative isolates one cause.
    heldQty
      | [c] == take 1 (bReceipts b) = bHeldQty b
      | otherwise = 1

receiptDatum :: Burn -> Coords -> PD.Data
receiptDatum b c =
  PD.Constr
    0
    [ PD.B (unCS (maybe receiptPolicy id (bDatumPolicy b)))
    , PD.B (unCS (cPolicy c))
    , toPD (cOutRef c)
    , PD.B (cCommitment c)
    , itemProof c
    , PD.I (cChunk c)
    , toPD (cOutRef c)
    , PD.Constr 1 []
    , PD.I 128
    ]

itemProof :: Coords -> PD.Data
itemProof c =
  PD.Constr
    0
    [ PD.I 1
    , PD.I (cField c)
    , PD.I 16
    , PD.I (cItem c)
    , PD.I 64
    , PD.B (BS.replicate 32 0xcc)
    , PD.List []
    , PD.List []
    ]

--------------------------------------------------------------------------------
-- Coordinates
--------------------------------------------------------------------------------

data Coords = Coords
  { cPolicy :: CurrencySymbol
  , cOutRef :: TxOutRef
  , cCommitment :: BS.ByteString
  , cField :: Integer
  , cItem :: Integer
  , cChunk :: Integer
  }
  deriving stock (Eq)

defaultCoords :: Coords
defaultCoords =
  Coords
    { cPolicy = orderPolicy
    , cOutRef = TxOutRef (TxId (toBuiltin (BS.replicate 32 0x77))) 0
    , cCommitment = BS.replicate 32 0xaa
    , cField = 3
    , cItem = 2
    , cChunk = 1
    }

-- | The receipt name, recomputed here from the spec rather than from the port.
receiptName :: Coords -> TokenName
receiptName c =
  TokenName . toBuiltin . blake2b256 $
    BS.concat
      [ "MidgardTxFieldReceiptV1"
      , unCS (cPolicy c)
      , txIdBytes (txOutRefId (cOutRef c))
      , bigEndian 8 (txOutRefIdx (cOutRef c))
      , cCommitment c
      , bigEndian 1 (cField c)
      , bigEndian 8 (cItem c)
      , bigEndian 8 (cChunk c)
      ]
  where
    txIdBytes (TxId x) = fromBuiltin x

-- | The order's event NFT name — @user_events.out_ref_to_nonce@.
orderNonce :: TokenName
orderNonce =
  TokenName . toBuiltin . blake2b256 . fromBuiltin . Builtins.serialiseData $
    toBuiltinData (cOutRef defaultCoords)

--------------------------------------------------------------------------------
-- Identities and plumbing
--------------------------------------------------------------------------------

orderPolicy, receiptPolicy, otherPolicy :: CurrencySymbol
orderPolicy = currencySymbolFromHex (concat (replicate 28 "33"))
receiptPolicy = currencySymbolFromHex (concat (replicate 28 "44"))
otherPolicy = currencySymbolFromHex (concat (replicate 28 "55"))

receiptScriptHash, otherScriptHash, preimageScriptHash :: ScriptHash
receiptScriptHash = ScriptHash (toBuiltin (BS.replicate 28 0x66))
otherScriptHash = ScriptHash (toBuiltin (BS.replicate 28 0x99))

-- | The validator's other parameter. Nothing reachable reads it.
preimageScriptHash = ScriptHash (toBuiltin (BS.replicate 28 0x67))

unCS :: CurrencySymbol -> BS.ByteString
unCS = fromBuiltin . unCurrencySymbol

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

bigEndian :: Int -> Integer -> BS.ByteString
bigEndian width n =
  BS.pack [fromIntegral ((n `shiftR` (8 * i)) .&. 0xff) | i <- [width - 1, width - 2 .. 0]]

inputsT :: forall s. [TxInInfo] -> Term s (PBuiltinList (PAsData PTxInInfo))
inputsT xs = punsafeCoerce (pasList # pconstant @PData (PD.List (map toPD xs)))

indicesT :: forall s. [Integer] -> Term s (PBuiltinList (PAsData PInteger))
indicesT xs = punsafeCoerce (pasList # pconstant @PData (PD.List (map PD.I xs)))

toPD :: ToData a => a -> PD.Data
toPD = builtinDataToData . toBuiltinData
