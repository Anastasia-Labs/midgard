{-# LANGUAGE OverloadedStrings #-}

module Testing.LedgerOutputCarriers (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Midgard.FraudProofs.NativeTx.Codec (pencodeDefiniteBytes)
import Midgard.ResolveInputsControl qualified as Resolve
import Midgard.ScriptSourcesRawFrame qualified as Sources
import Plutarch.Prelude
import PlutusCore.Data qualified as D
import Test.Tasty
import Test.Tasty.HUnit
import Testing.Eval (passertEvalNoTraceWithoutHoistChecks, pfailsNoTraceWithoutHoistChecks)

tests :: TestTree
tests =
    testGroup
        "Ledger output carrier splices"
        [ testCase "resolve pending splice equals re-encoding across CBOR header sizes" $
            passertEvalNoTraceWithoutHoistChecks $
                plet pending $ \old -> plet (Resolve.pencodePending # old) $ \oldCbor ->
                    plet (control oldCbor) $ \current -> pmatch current $ \c -> pmatch old $ \pd ->
                        plet (pconstant $ BS.replicate 256 0xaa) $ \next ->
                            Resolve.psplicePendingSuccessorV1
                                # (Resolve.pencodeControlRaw # current)
                                # oldCbor
                                # Resolve.ppending'outputProofCbor pd
                                # next
                                #== Resolve.pencodeControlRaw
                                # pcon c{Resolve.pcontrol'pendingCbor = Resolve.pencodePending # pcon pd{Resolve.ppending'outputProofCbor = next}}
        , testCase "resolve control decodes and canonically re-encodes" $
            passertEvalNoTraceWithoutHoistChecks $
                plet (Resolve.pencodeControlRaw # control (Resolve.pencodePending # pending)) $
                    \cbor -> Resolve.pencodeControlRaw # (Resolve.pcontrolRawFromWitness # cbor) #== cbor
        , testCase "pending record uses the target five-item encoding" $
            passertEvalNoTraceWithoutHoistChecks $
                Resolve.pencodePending # pending #== pconstant (Base16.decodeLenient "85014101410241034100")
        , testCase "resolve splice refuses a replaced old item" $
            pfailsNoTraceWithoutHoistChecks $
                Resolve.psplicePendingSuccessorV1 # (Resolve.pencodeControlRaw # control (Resolve.pencodePending # pending)) # (Resolve.pencodePending # pending) # pconstant "wrong" # pconstant "next"
        , testCase "resolve splice refuses a replaced pending field" $
            pfailsNoTraceWithoutHoistChecks $
                Resolve.psplicePendingSuccessorV1 # (Resolve.pencodeControlRaw # control (pconstant "wrong")) # (Resolve.pencodePending # pending) # pconstant "\x00" # pconstant "next"
        , testCase "no-pending parser refuses a pending record" $
            pfailsNoTraceWithoutHoistChecks $
                Resolve.pcontrolNoPendingFromWitness # (Resolve.pencodeControlRaw # control (Resolve.pencodePending # pending))
        , testCase "empty pending marker roundtrips" $
            passertEvalNoTraceWithoutHoistChecks $
                plet (Resolve.pencodeControlRaw # control (pconstant "\x00")) $
                    \cbor -> Resolve.pencodeControlRaw # (Resolve.pcontrolNoPendingFromWitness # cbor) #== cbor
        , testCase "script-source splice preserves repeated payload elsewhere" $
            passertEvalNoTraceWithoutHoistChecks $
                Sources.pspliceV1 # pconstant "ababab" # 2 # pconstant "ab" # pconstant "xyz" #== pconstant "abxyzab"
        , testCase "script-source splice rejects mismatched old bytes" $
            pfailsNoTraceWithoutHoistChecks $
                Sources.pspliceV1 # pconstant "abcdef" # 2 # pconstant "xx" # pconstant "y"
        , testCase "script-source splice rejects negative offsets" $
            pfailsNoTraceWithoutHoistChecks $
                Sources.pspliceV1 # pconstant "abcdef" # (-1) # pconstant "ab" # pconstant "y"
        , testCase "script-source splice rejects a suffix beyond the input" $
            pfailsNoTraceWithoutHoistChecks $
                Sources.pspliceV1 # pconstant "abcdef" # 5 # pconstant "fg" # pconstant "y"
        , testCase "append, replace and drop extension preserve the predecessor" $
            passertEvalNoTraceWithoutHoistChecks $
                plet (Sources.pappendExtensionV1 # sourceCbor # pconstant "old") $ \extended ->
                    plet (Sources.preplaceExtensionV1 # extended # pconstant "old" # pconstant (BS.replicate 24 1)) $ \next ->
                        Sources.pdropExtensionV1 # next # pconstant (BS.replicate 24 1) #== sourceCbor
        , testCase "extension replacement requires the 31-item header" $
            pfailsNoTraceWithoutHoistChecks $
                Sources.preplaceExtensionV1 # (sourceCbor <> (pencodeDefiniteBytes # pconstant "old")) # pconstant "old" # pconstant "new"
        , testCase "extension replacement rejects a substituted suffix" $
            pfailsNoTraceWithoutHoistChecks $
                Sources.preplaceExtensionV1 # (Sources.pappendExtensionV1 # sourceCbor # pconstant "old") # pconstant "bad" # pconstant "new"
        , testCase "nested canonical item offsets count header boundaries" $
            passertEvalNoTraceWithoutHoistChecks $
                Sources.pitemOffsetV1 # rawFrame # 3 #== 32
        , testCase "canonical offset refuses foreign Data constructors" $
            pfailsNoTraceWithoutHoistChecks $
                Sources.pitemOffsetV1 # (pcon $ Sources.PFrame (pconstant [D.Constr 0 []]) 1 (pconstant "") 0 (pconstant "") (pconstant "") (pconstant "") (pconstant "")) # 1
        , testCase "offset index cannot exceed the frame" $
            pfailsNoTraceWithoutHoistChecks $
                Sources.pitemOffsetV1 # rawFrame # 4
        ]

pending :: forall s. Term s Resolve.PPending
pending = pcon $ Resolve.PPending 1 (pconstant "\x01") (pconstant "\x02") (pconstant "\x03") (pconstant "\x00")
control :: forall s. Term s PByteString -> Term s Resolve.PControl
control pendingCbor = pcon $ Resolve.PControl (pconstant "compact") (pconstant "witness") (pconstant "lengths") (pconstant "context") 2 hash hash 1 hash pendingCbor hash
  where
    hash = pconstant $ BS.replicate 32 0
sourceCbor :: forall s. Term s PByteString
sourceCbor = pconstant $ BS.pack [0x98, 0x1e] <> BS.replicate 30 0
rawFrame :: forall s. Term s Sources.PFrame
rawFrame = pcon $ Sources.PFrame (pconstant [D.I 24, D.B $ BS.replicate 24 1, D.List [D.I 0, D.B ""]]) 3 (pconstant "") 0 (pconstant "") (pconstant "") (pconstant "") (pconstant "")
