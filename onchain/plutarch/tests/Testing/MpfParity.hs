{-# LANGUAGE OverloadedStrings #-}

module Testing.MpfParity (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import MerkleTree.Validators.Membership (nonMembershipStakeValidator)
import Midgard.MpfProof qualified as Mpf
import Midgard.MpfProof.Types (PProof)
import Midgard.MpfProofFold
import Midgard.TransitionTrace (pverifyRootNonMembershipRaw)
import Midgard.ValidationMerkle
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V3 qualified as Ledger
import Test.Tasty hiding (after)
import Test.Tasty.HUnit
import Testing.Eval (passertEval, pfails, psucceeds)
import Testing.ScriptContextBuilder (buildScriptContext, withRewardingScript)

-- Captured host roots from the target Aiken mpf-prefix-fixture.ak.
data Vector = Vector {before :: BS.ByteString, after :: BS.ByteString, key :: BS.ByteString, proof :: [PD.Data]}

hex :: BS.ByteString -> BS.ByteString
hex = Base16.decodeLenient

forkVector, leafVector :: Vector
forkVector =
  Vector
    (hex "a3cb6c7cc11c8ab91629b7fa17089818c9751c5a9a0f256b0449acc17a6fb5b7")
    (hex "aae9218b732ed0dd511191290953ea78b87bf1d3a9e53fa0d2753fe678c9ba9b")
    (hex "abababababababababababababababababababababababababababab001f")
    [ PD.Constr 0 [PD.I 0, PD.B (hex "bd3871c02105e5ec24751ba8fb1a5e6d285cdcc8399993a0ca82b26f5ae179d65fed1c2681e29bbb4baed7e7d2a0f618637b48308f351ef786980f3dcdf001205318b031fecedfbfb987a7db1e2fcbb1c1d1c82dbf31e099558ddab0e96e3d38c73adabe2c5d1f9741c04a9dc2e856e29ecb6b96346905f7fb7b266b7865c857")]
    , PD.Constr 0 [PD.I 0, PD.B (hex "42a0d1b9e50273451d2a93f195a000843d6a0c114672d0b5fadbf8fce300fb90b6be92c2f492c9bce1b8ecac7b530487537b2f448a80be834ad7a21df127a5468fe7d543b2434538a0c6d78fae96ed7f767ed36752ce7c1f54242d57eeaaf059cec5360efb71d228292cdf2b52f551f199b32bda593c4a4745a9b94aa17f8c9a")]
    , PD.Constr 1 [PD.I 1, PD.Constr 0 [PD.I 8, PD.B "", PD.B (hex "2f425ececd6ad91ad57238470324c4623ff0bcd1b51f6393ce0105874e651ccb")]]
    ]
leafVector =
  Vector
    (hex "88e28232999483e47f2779227d77d46358aad506223f4de36808d08fc57f11c4")
    (hex "3e090fbdb93ef5cbeb10616c4f7238e36149d89d9974620230c304e36f7a517b")
    (hex "abcd")
    [ PD.Constr 2 [PD.I 1, PD.B (hex "9706e52f00c679e548b5155af5026f5af4130d7a15c990a791fff8d652c464f5"), PD.B (BS.replicate 32 0x11)]
    , PD.Constr 0 [PD.I 0, PD.B (BS.replicate 128 0x22)]
    ]

droppedRoot :: BS.ByteString
droppedRoot = hex "c8aa9e6b96398745d8c5ec972fc6a0f3baaf7cbe154b9fcd5ae642d8ffa82f36"

proofTerm :: Vector -> Term s PProof
proofTerm v = pfromData $ punsafeCoerce $ pconstant @PData (PD.List (proof v))

justRoot :: Term s (PMaybe PByteString) -> BS.ByteString -> Term s PBool
justRoot result expected = pmatch result $ \case
  PNothing -> pconstant @PBool False
  PJust root -> root #== pconstant expected

rewarding :: Vector -> BS.ByteString -> Term s PUnit
rewarding v root =
  nonMembershipStakeValidator
    # pconstant
      ( buildScriptContext $
          withRewardingScript
            (Ledger.dataToBuiltinData (PD.List [PD.B root, PD.B (key v), PD.List (proof v)]))
            (Ledger.ScriptCredential (Ledger.ScriptHash (Ledger.toBuiltin (BS.replicate 28 1))))
            0
      )

tests :: TestTree
tests =
  testGroup
    "MPF target parity"
    ( [ testGroup
          name
          [ testCase "membership and deletion match host roots" $
              passertEval $
                (Mpf.phasV1 # pconstant (before v) # pconstant (key v) # phexByteStr "01" # proofTerm v)
                  #&& justRoot (Mpf.pdeleteRoot # pconstant (before v) # pconstant (key v) # phexByteStr "01" # proofTerm v) (after v)
          , testCase "paired deletion preserves the exact compressed successor" $
              passertEval $
                justRoot (Mpf.pdeleteRootPairedFold # pconstant (before v) # pconstant (key v) # phexByteStr "01" # proofTerm v) (after v)
          , testCase "paired deletion refuses substituted predecessor" $
              passertEval $
                pmatch (Mpf.pdeleteRootPairedFold # pconstant (after v) # pconstant (key v) # phexByteStr "01" # proofTerm v) $ \case
                  PNothing -> pconstant @PBool True
                  PJust _ -> pconstant @PBool False
          , testCase "paired insertion reverses the canonical deletion" $
              passertEval $
                justRoot (Mpf.pinsertRootPairedFold # pconstant (after v) # pconstant (key v) # phexByteStr "01" # proofTerm v) (before v)
          , testCase "paired insertion refuses an existing member" $
              passertEval $
                pmatch (Mpf.pinsertRootPairedFold # pconstant (before v) # pconstant (key v) # phexByteStr "01" # proofTerm v) $ \case
                  PNothing -> pconstant @PBool True
                  PJust _ -> pconstant @PBool False
          , testCase "exclusion and insertion reverse deletion" $
              passertEval $
                (Mpf.pdoesNotHave # pconstant (after v) # pconstant (key v) # proofTerm v)
                  #&& justRoot (Mpf.pinsertRoot # pconstant (after v) # pconstant (key v) # phexByteStr "01" # proofTerm v) (before v)
          , testCase "transition trace authenticates canonical exclusion" $
              passertEval $
                pverifyRootNonMembershipRaw (pconstant (after v)) 2 (pconstant (key v)) (proofTerm v)
          , testCase "rewarding validator accepts canonical exclusion" $ psucceeds $ rewarding v (after v)
          , testCase "rewarding validator rejects the pre-deletion root" $ pfails $ rewarding v (before v)
          ]
      | (name, v) <- [("compressed terminal fork", forkVector), ("skipped non-terminal leaf", leafVector)]
      ]
        <> [ testCase "dropped prefix is rejected by every exclusion consumer" $ do
               passertEval $ pnot # (Mpf.pdoesNotHave # pconstant droppedRoot # pconstant (key forkVector) # proofTerm forkVector)
               passertEval $ pmatch (Mpf.pinsertRoot # pconstant droppedRoot # pconstant (key forkVector) # phexByteStr "01" # proofTerm forkVector) $ \case
                 PNothing -> pconstant @PBool True
                 PJust _ -> pconstant @PBool False
               passertEval $ pnot # pverifyRootNonMembershipRaw (pconstant droppedRoot) 2 (pconstant (key forkVector)) (proofTerm forkVector)
               pfails $ rewarding forkVector droppedRoot
           , testCase "resumable frames preserve the terminal fork prefix" $ passertEval $ frameFold False
           , testCase "resumable fold rejects a substituted sibling" $ passertEval $ frameFold True
           ]
    )

expectJust :: Term s (PMaybe a) -> Term s a
expectJust x = pmatch x $ \case
  PNothing -> perror
  PJust y -> y

frameFold :: Bool -> Term s PBool
frameFold tamper =
  let frame i cursor next =
        pfromData $
          punsafeCoerce $
            pconstant @PData
              (PD.Constr 0 [PD.I 1, PD.I i, PD.I cursor, PD.I next, proof forkVector !! fromIntegral i])
      first = frame 0 0 1
      second = frame 1 1 2
      third = frame 2 2 4
      firstHash = pproofFrameLeafHashV1 # first
      secondHash = pproofFrameLeafHashV1 # second
      hashes = pcons # pdata firstHash # (pcons # pdata secondHash # (pcons # pdata (pproofFrameLeafHashV1 # third) # pnil))
      keyTerm = pconstant (key forkVector)
   in pmatch (pbuildFrontier # hashes) $ \(PBuiltFrontier _ peaks) ->
        plet (pcon $ PProofDescriptorV1 (pdata 1) (pdata 3) (pdata 4) (pdata peaks)) $ \descriptor ->
          plet (expectJust (pinitialFoldControlV1 # keyTerm # phexByteStr "01" # descriptor)) $ \initial ->
            plet (expectJust (pfoldProofFrameV1 # keyTerm # descriptor # initial # third # pnil)) $ \afterThird ->
              plet
                ( pfoldProofFrameV1
                    # keyTerm
                    # descriptor
                    # afterThird
                    # second
                    # (pcons # pdata (if tamper then pconstant droppedRoot else firstHash) # pnil)
                )
                $ \secondResult ->
                  if tamper
                    then pmatch secondResult $ \case
                      PNothing -> pconstant @PBool True
                      PJust _ -> pconstant @PBool False
                    else plet (expectJust secondResult) $ \afterSecond ->
                      plet (expectJust (pfoldProofFrameV1 # keyTerm # descriptor # afterSecond # first # (pcons # pdata secondHash # pnil))) $ \terminal ->
                        pmatch terminal $ \t ->
                          (pfoldIsCompleteV1 # terminal)
                            #&& pfromData (pfoldControl'includingRoot t)
                            #== pconstant (before forkVector)
                            #&& pfromData (pfoldControl'excludingRoot t)
                            #== pconstant (after forkVector)
