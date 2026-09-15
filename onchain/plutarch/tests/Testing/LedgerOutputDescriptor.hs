{-# LANGUAGE OverloadedStrings #-}

module Testing.LedgerOutputDescriptor (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.Core.Utils ((#/=))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.LedgerOutput (pdecodeCanonicalOutput)
import Midgard.LedgerOutputCommitment (
    pdecodeLedgerOutputCommitment,
    pencodeLedgerOutputCommitment,
 )
import Midgard.LedgerOutputDescriptor (pledgerValueV1)
import Testing.Eval (passertEvalNoTrace)

tests :: TestTree
tests =
    testGroup
        "Midgard.LedgerOutputDescriptor"
        [ exact "builds_no_reference_output_exactly" 0 noReferenceOutput noReferenceDescriptor
        , exact "builds_small_datum_output_exactly" 0 smallDatumOutput smallDatumDescriptor
        , exact "builds_small_reference_output_exactly" 0 smallReferenceOutput smallReferenceDescriptor
        , exact "builds_native_reference_script_output_exactly" 65_535 nativeReferenceOutput nativeReferenceDescriptor
        , exact "builds_rich_multi_chunk_output_exactly" 7 richMultiChunkOutput richMultiChunkDescriptor
        , testCase "every_pinned_descriptor_round_trips_through_the_decoder" $
            passertEvalNoTrace $
                pand
                    [ pencodeLedgerOutputCommitment
                        # (pdecodeLedgerOutputCommitment # pconstant descriptor)
                        #== pconstant descriptor
                    | descriptor <- descriptors
                    ]
        , testCase "the_output_index_is_bound_into_the_value" $
            passertEvalNoTrace $
                pmatch (pledgerValueV1 # 1 # pconstant noReferenceOutput) $ \case
                    PNothing -> pconstant True
                    PJust value -> value #/= pconstant noReferenceDescriptor
        , testCase "an_out_of_domain_output_index_has_no_value" $
            passertEvalNoTrace $
                pisNothing (pledgerValueV1 # 65_536 # pconstant noReferenceOutput)
                    #&& pisNothing (pledgerValueV1 # (-1) # pconstant noReferenceOutput)
        , testCase "non_canonical_output_bytes_have_no_value" $
            passertEvalNoTrace $
                pisNothing
                    ( pledgerValueV1
                        # 0
                        # bytes "b80200581d7811111111111111111111111111111111111111111111111111111111018200a0"
                    )
        , testCase "an_unmaterialisable_canonical_datum_has_no_value" $
            passertEvalNoTrace $
                let output = bytes "a300581d7811111111111111111111111111111111111111111111111111111111018200a00246d8668218ff80"
                 in pisJust (pdecodeCanonicalOutput # output)
                        #&& pisNothing (pledgerValueV1 # 0 # output)
        ]

exact :: String -> Integer -> BS.ByteString -> BS.ByteString -> TestTree
exact name outputIndex output descriptor =
    testCase name $
        passertEvalNoTrace $
            pmatch (pledgerValueV1 # pconstant outputIndex # pconstant output) $ \case
                PNothing -> pconstant False
                PJust value -> value #== pconstant descriptor

pand :: forall s. [Term s PBool] -> Term s PBool
pand = foldr (\condition rest -> condition #&& rest) (pconstant True)

pisNothing :: forall s a. Term s (PMaybe a) -> Term s PBool
pisNothing value = pmatch value $ \case
    PNothing -> pconstant True
    PJust _ -> pconstant False

pisJust :: forall s a. Term s (PMaybe a) -> Term s PBool
pisJust value = pmatch value $ \case
    PNothing -> pconstant False
    PJust _ -> pconstant True

descriptors :: [BS.ByteString]
descriptors =
    [ noReferenceDescriptor
    , smallDatumDescriptor
    , smallReferenceDescriptor
    , nativeReferenceDescriptor
    , richMultiChunkDescriptor
    ]

noReferenceOutput, noReferenceDescriptor :: BS.ByteString
noReferenceOutput = decode "a200581d7811111111111111111111111111111111111111111111111111111111018200a0"
noReferenceDescriptor = decode "90010018255820855089c279a2084237bfc980ad11c3cb72bd80055b3d89fdf9105faff6b9d3ec581d781111111111111111111111111111111111111111111111111111111100005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd012040004083582068221e2117402083ee82606dd1c3296b6b06d69d71bd1417555a0f1848ab7f731834183c83582023656d4d955fd1968ba6d1d923f25fda39fe3bbc3c9a8e474b16c9340ab32a081834183c8358209525e1ea4350de9f831fc817b64355d7c3e26427effb7f4ca9bd29541d5eda390304"

smallDatumOutput, smallDatumDescriptor :: BS.ByteString
smallDatumOutput = decode "a300581d7811111111111111111111111111111111111111111111111111111111018200a00243d87980"
smallDatumDescriptor = decode "900100182a5820831569d49cdf1d62af52a7ba84294373584f80bafa4b90aafb97dd9070ddbe41581d781111111111111111111111111111111111111111111111111111111100005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd01204000408358200a2e786d1965b870e307b60f251e29f6653560d7dbf72fa5ece0c5c44bbfd03c18381840835820ad9e3605aefc6d2070c186504f4e16ec0afef0f6c1342f7f423fa47f569e93421838184083582029f8b517a22889be5795b8b91050155dbc935ce541d077fd7124de5af8dd664c0708"

smallReferenceOutput, smallReferenceDescriptor :: BS.ByteString
smallReferenceOutput = decode "a300581d7811111111111111111111111111111111111111111111111111111111018200a0038203436b6b6b"
smallReferenceDescriptor = decode "900100182c582022477a156a6db4a27cd5c7af0cda47026474f4a97dbd31f3baabeb17b5064b78581d781111111111111111111111111111111111111111111111111111111100005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd0103581c556f006134510d3f7f607d251bd0e49aae300988b3fcfc0756c568e2065820e03a1cdbe9503904744a34d1163c1b22985c89586a997faa8c152a440043c34283582038d29cb45b5ac7901e51036d0fac974140394ebbd34ca5f70320e724380187521853185c835820bc293befa4e0e81021c3b002707d83cc1350338be43fc26303aa9acd600a55791853185c8358209525e1ea4350de9f831fc817b64355d7c3e26427effb7f4ca9bd29541d5eda390304"

nativeReferenceOutput, nativeReferenceDescriptor :: BS.ByteString
nativeReferenceOutput = decode "a300581d7811111111111111111111111111111111111111111111111111111111018200a003820058208200581c33333333333333333333333333333333333333333333333333333333"
nativeReferenceDescriptor = decode "900119ffff184a58209cc398e4f08855f03f791f3e43d1d00fa55b196c2aa8432c8558e3c8df3dc9cf581d781111111111111111111111111111111111111111111111111111111100005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd0100581cc78b7b4b696fffb06ba43034b2ddb692c43a88ea824ddfdf455b93721824582057e1c7765325cfa3e8676ca5c28b3477b878a1637cc3348d1027245a30a414978358204ccd01eb52febe88afa3b3be5af0a74c8936116264b85db97508b5cb88605d0c1853185c835820a06f11972ff408d3ca430a373f274af65377a84ed13bb21bd75008505ea71ec11853185c8358209525e1ea4350de9f831fc817b64355d7c3e26427effb7f4ca9bd29541d5eda390304"

richMultiChunkOutput, richMultiChunkDescriptor :: BS.ByteString
richMultiChunkOutput =
    decode "a400583900111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111101821a007a1200a2581c55555555555555555555555555555555555555555555555555555555a24001420102182a581c66666666666666666666666666666666666666666666666666666666a15820abababababababababababababababababababababababababababababababab1b000000044b82fa09025908115f"
        <> BS.concat (replicate 31 (decode "5840" <> BS.replicate 64 0xab))
        <> decode "50"
        <> BS.replicate 16 0xab
        <> decode "ff0382035864"
        <> BS.replicate 100 0x6b
richMultiChunkDescriptor = decode "90010719093358201d44e4026471138e8ee55364b7f5edfb548bbcb418ef6158fb2234dc604aebf758390011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111a007a120003582008059de11e25e694627ded37fc07251446b431d0b0f01ad7988b50fae7b37cd2187703581c22c0e1c50c8393c705226fd7578a859b9049b24e0c9eb6c1a5f7b2bb18685820fa4940294dd31f0806fb59218d53fe245a6717649bf8785e8eba6e7fddf755fc8358205f721c66e18ad314fbda580939ed42add1cd7476d78918ff074a579a441407341909041908f78358205f721c66e18ad314fbda580939ed42add1cd7476d78918ff074a579a441407341909041908f7835820b0396a03f113d1587164a43e74652ff1f1054dd2e54d8c6ab7628031346a65a21908151907d8"

bytes :: forall s. BS.ByteString -> Term s PByteString
bytes = pconstant . decode

decode :: BS.ByteString -> BS.ByteString
decode = Base16.decodeLenient
