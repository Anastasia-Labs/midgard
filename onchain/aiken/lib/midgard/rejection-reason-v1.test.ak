//// Frozen vectors for the rejection-code preimage.
////
//// The rejection code of a reason is
//// `blake2b_256("MidgardValidationRejectCodeV1" || cbor.serialise(reason))`.
//// Every one of the 48 arms is pinned here twice: its Plutus Data CBOR (the
//// constructor tag is 121 + i for i <= 6 and 1280 + (i - 7) above, so a
//// reordered or inserted arm moves a tag) and its domain-separated hash. Each
//// arm carries distinct coordinates, so a dropped or swapped field also moves
//// the bytes. The vectors were computed off-chain, independently of this
//// codebase, and the same vectors pin the TypeScript encoder.

use aiken/cbor
use midgard/rejection_reason_v1.{
  AddressWitnessSignatureInvalid, DuplicateInput, EmptyInputs,
  ExecutionNativeScriptDepthLimit, ExecutionNativeScriptFalse,
  ExecutionNativeScriptMalformed, ExecutionNativeScriptNodeLimit,
  FeeBelowMinimum, FieldItemWidthIllegal, FieldPreimageLengthMismatch,
  InputAssetAccumulationLimit, InputNotFound, InputSpentOutputNonCanonical,
  MintAssetAccumulationLimit, MintDeclaredAssetLimit, MintItemMalformed,
  NetworkIdMismatch, ObserverOrderInvalid, ObserversForbiddenOnUntaggedNetwork,
  OutputAssetAccumulationLimit, OutputBelowMinAda, OutputNonCanonical,
  OutputReferenceScriptDepthLimit, OutputReferenceScriptMalformed,
  OutputReferenceScriptNodeLimit, PlutusExecutionFailed,
  ProtectedOutputSignerMissing, ReceivePurposePlutusV3Forbidden,
  RedeemerMalformed, RedeemerMissing, RejectionReasonV1, RequiredSignerUnsigned,
  ResolvedReferenceScriptDepthLimit, ResolvedReferenceScriptMalformed,
  ResolvedReferenceScriptNodeLimit, ScriptIntegrityHashMismatch,
  ScriptIntegrityHashMissing, ScriptSourceMissing, SpendInputSignerMissing,
  UnusedRedeemer, UnusedScriptWitness, ValidityIntervalExcludesBlockSlot,
  ValidityIntervalMalformed, ValueNotPreserved, WitnessNativeScriptDepthLimit,
  WitnessNativeScriptFalse, WitnessNativeScriptMalformed,
  WitnessNativeScriptNodeLimit, WitnessScriptHeaderMalformed,
  reject_duplicate_input,
}
use midgard/validation_trace_v1.{hash_rejection_code}

fn pinned(reason: RejectionReasonV1, cbor_hex: ByteArray, hash: ByteArray) {
  and {
    cbor.serialise(reason) == cbor_hex,
    hash_rejection_code(reason) == hash,
  }
}

test reason_vectors_arms_0_to_11() {
  and {
    pinned(
      FieldPreimageLengthMismatch { field_index: 0 },
      #"d8799f00ff",
      #"bc594a430a54f68413833f9c54bb976d86523fe5d3c4ee8bcc1e170b810a926c",
    ),
    pinned(
      FieldItemWidthIllegal { field_index: 1, item_index: 2 },
      #"d87a9f0102ff",
      #"f1788a1be1dc8659c3fb3a6eb11da8e1349157cbaec1aea818e6ae477e4fcf48",
    ),
    pinned(
      EmptyInputs,
      #"d87b80",
      #"9c71cc54543951c53a94a959911cab6c8c2586f22a9f87c81479eae5b6d8a5a1",
    ),
    pinned(
      DuplicateInput {
        first_field_index: 3,
        first_item_index: 4,
        second_field_index: 5,
        second_item_index: 6,
      },
      #"d87c9f03040506ff",
      #"44cf51a9c10b394c3f61d1ec5cc49b29582b5c9942f31bd052da25876b68b1f1",
    ),
    pinned(
      ValidityIntervalMalformed,
      #"d87d80",
      #"301c0832c101922e51a6233a42ccebb09450154f40dc30d5a350d32fecc5f9b2",
    ),
    pinned(
      NetworkIdMismatch,
      #"d87e80",
      #"748ffda500eba98ba9ea9cc8d84358fbb888fdb54c71bba46b2987616e8210ab",
    ),
    pinned(
      FeeBelowMinimum,
      #"d87f80",
      #"1c58e368280379bf2a43565907240d90e71114cc8fc3422c0f9b1fdc58bd7bef",
    ),
    pinned(
      AddressWitnessSignatureInvalid { witness_index: 7 },
      #"d905009f07ff",
      #"8efca108e0ab132150ba9228f84436e0db12ddd2bccd7f684b36c99a3422c439",
    ),
    pinned(
      RequiredSignerUnsigned { signer_index: 8 },
      #"d905019f08ff",
      #"69083b93d2fa1bbfc8fc061ecb097301fb670bfd022cfa9e818e607091bbf6e1",
    ),
    pinned(
      WitnessScriptHeaderMalformed { script_index: 9 },
      #"d905029f09ff",
      #"820f18f0c555155ea0b92903d618228ee5d5c2897e7066426acef1d3e0b710dd",
    ),
    pinned(
      WitnessNativeScriptMalformed { script_index: 10 },
      #"d905039f0aff",
      #"55c364c32f54655213377ad500b93b55a8e718eeff3521a102d86d5de9de1757",
    ),
    pinned(
      WitnessNativeScriptNodeLimit { script_index: 11 },
      #"d905049f0bff",
      #"39d4cdbff1ddffe9382922fd369acea3ac1df521f322adfcb900d4189217b26b",
    ),
  }
}

test reason_vectors_arms_12_to_23() {
  and {
    pinned(
      WitnessNativeScriptDepthLimit { script_index: 12 },
      #"d905059f0cff",
      #"a17f7e11eb02ad3488c799d37bf62a7bafd77bed49e329835999d70fc626c744",
    ),
    pinned(
      WitnessNativeScriptFalse { script_index: 13 },
      #"d905069f0dff",
      #"d4274cca7a0e9e0b812d7c72312d817ca55029d4ef6bef35eef0840f66a874b7",
    ),
    pinned(
      ScriptIntegrityHashMissing,
      #"d9050780",
      #"f344e698a16976323bd7a8b3e5623d2d576447cca002adb97621bbed58a70170",
    ),
    pinned(
      ObserversForbiddenOnUntaggedNetwork,
      #"d9050880",
      #"87405a3291b4ccc221964a2aeaec8db3786df37be06d316ab6d476fda4da932f",
    ),
    pinned(
      ObserverOrderInvalid { observer_index: 16 },
      #"d905099f10ff",
      #"4d6f0c4a802256327906b09fe80765248b5be645a3bfc4830c8123240267ad9a",
    ),
    pinned(
      ValidityIntervalExcludesBlockSlot,
      #"d9050a80",
      #"977035a610d3f71f60c1521bb1c75c6b64e30e96eeccc23115fd037f7f341087",
    ),
    pinned(
      InputNotFound { source_kind: 18, input_index: 19 },
      #"d9050b9f1213ff",
      #"360b7276c87e7f2c7cff4243f33a3ca9cfdfba2500bc5dde2fca43ebbf2cbb74",
    ),
    pinned(
      InputSpentOutputNonCanonical { source_kind: 19, input_index: 20 },
      #"d9050c9f1314ff",
      #"8c11e5498240aa0baaacf9515618c7525ea60f25a9692bab7a6b7bb0cfff5cd7",
    ),
    pinned(
      ResolvedReferenceScriptMalformed { source_kind: 20, input_index: 21 },
      #"d9050d9f1415ff",
      #"787ba5ea9260dc7e8c71ea92a61841cb1b2f16bdf5b8247f4a4178acbb5216f9",
    ),
    pinned(
      ResolvedReferenceScriptNodeLimit { source_kind: 21, input_index: 22 },
      #"d9050e9f1516ff",
      #"aea165f9c787f481b7f4a91ec3be48458efd15964be3671387201789bee773c7",
    ),
    pinned(
      ResolvedReferenceScriptDepthLimit { source_kind: 22, input_index: 23 },
      #"d9050f9f1617ff",
      #"7eab051742f629fd163465b00ca523b0a553ec911379fc561ff4157b64a400a1",
    ),
    pinned(
      SpendInputSignerMissing { input_index: 23 },
      #"d905109f17ff",
      #"a0ed7b09f5311cc7cf4f5d279636d39bbe54bd953b39d0610dc013ba7d7399d2",
    ),
  }
}

test reason_vectors_arms_24_to_35() {
  and {
    pinned(
      RedeemerMalformed { redeemer_index: 24 },
      #"d905119f1818ff",
      #"c6d2615d85198275e528325d3b3bdb9996f64276014747f7ef0fac8f873c584f",
    ),
    pinned(
      OutputNonCanonical { output_index: 25 },
      #"d905129f1819ff",
      #"a20f0ce92b04fde70e1cea53571dde27cf8c825aab93a85128afd55721a55bac",
    ),
    pinned(
      OutputReferenceScriptMalformed { output_index: 26 },
      #"d905139f181aff",
      #"bd75607c79c80dc28ff656f3417d7ef7b14a9dbc44a553c7286ca59fa6b28b12",
    ),
    pinned(
      OutputReferenceScriptNodeLimit { output_index: 27 },
      #"d905149f181bff",
      #"c24376f2b2ed4bda97d8f06789c315d3ba8ff569059a30de31cb9ae9bd86c30a",
    ),
    pinned(
      OutputReferenceScriptDepthLimit { output_index: 28 },
      #"d905159f181cff",
      #"5ee0d0d1a34340095582d43fc1ab8c0ca277c5d9ddf43bf53a8a8d39652515ce",
    ),
    pinned(
      ProtectedOutputSignerMissing { output_index: 29 },
      #"d905169f181dff",
      #"3beea3ec18ccab5a62424a69ffb15c70a0b8a99cc14c2597557aba1c2b1fa1f0",
    ),
    pinned(
      MintDeclaredAssetLimit { policy_index: 30 },
      #"d905179f181eff",
      #"24d971db4fba7fde03874327763beaf3d8d26b4ca667b17937a3e9dfe82a482e",
    ),
    pinned(
      ScriptSourceMissing { purpose_kind: 31, purpose_index: 32 },
      #"d905189f181f1820ff",
      #"c30e56fb0dbaf1e7428e934ff1a6301cc556d14d23e98fc99e86f4710e712933",
    ),
    pinned(
      RedeemerMissing { purpose_kind: 32, purpose_index: 33 },
      #"d905199f18201821ff",
      #"3d52ad10c0bf7af648753abce156495f263c8050738b355da60f0ffd883a9ef5",
    ),
    pinned(
      UnusedScriptWitness { script_index: 33 },
      #"d9051a9f1821ff",
      #"129728be3cc3f3df13fe503cdb434f44283a2d4bbdb567687baa5c9d0f016527",
    ),
    pinned(
      UnusedRedeemer { redeemer_index: 34 },
      #"d9051b9f1822ff",
      #"4d85c48f6e3d82dabb9d56cd1f939d3c6d64d4b310a0d78ac2d569103af819d4",
    ),
    pinned(
      ExecutionNativeScriptMalformed { purpose_kind: 35, purpose_index: 36 },
      #"d9051c9f18231824ff",
      #"f3609cd0586c95ec46a0b9a933e11b2e91b30ff8e49b443ccb8de2ea32cf8328",
    ),
  }
}

test reason_vectors_arms_36_to_47() {
  and {
    pinned(
      ExecutionNativeScriptNodeLimit { purpose_kind: 36, purpose_index: 37 },
      #"d9051d9f18241825ff",
      #"f694991059500f501a3daedc22696d1bde1f7107f3a772d265313813637f0743",
    ),
    pinned(
      ExecutionNativeScriptDepthLimit { purpose_kind: 37, purpose_index: 38 },
      #"d9051e9f18251826ff",
      #"56208a49a59345290853c7db1475a102903552fb43b81c3f71efddf04f02600b",
    ),
    pinned(
      ExecutionNativeScriptFalse { purpose_kind: 38, purpose_index: 39 },
      #"d9051f9f18261827ff",
      #"89a7ebfca4f3da3b4c7c95c3d0d5fb17b38a04484865abdb366f08f2a5e41e5a",
    ),
    pinned(
      ScriptIntegrityHashMismatch,
      #"d9052080",
      #"800b4eb07dcdb39a9617f27e213bc60f0612e16138ebfadc876485ebd33720c8",
    ),
    pinned(
      ReceivePurposePlutusV3Forbidden { purpose_kind: 40, purpose_index: 41 },
      #"d905219f18281829ff",
      #"3112b34d078800cefa88946fb87a615b6a31f57ee8891b4c37b610fa861ecc7e",
    ),
    pinned(
      PlutusExecutionFailed { purpose_kind: 41, purpose_index: 42 },
      #"d905229f1829182aff",
      #"248e3bf957b996ce34e7e5ce5b0cb11012d9ab04e897fb67f15d0537dbe96369",
    ),
    pinned(
      InputAssetAccumulationLimit { input_index: 42, asset_index: 43 },
      #"d905239f182a182bff",
      #"19657af2b499b060b75b8a871b0c6ebf104e742c72ee550fce36cf94b6d27979",
    ),
    pinned(
      OutputAssetAccumulationLimit { output_index: 43, asset_index: 44 },
      #"d905249f182b182cff",
      #"ca0eb3741e01475d9012369039f63ec479f4897d91a584eff3c7cc79af906316",
    ),
    pinned(
      MintAssetAccumulationLimit { mint_index: 44 },
      #"d905259f182cff",
      #"8e6a9aa075f8b4a8bf207e737995a084133854792ff4abe22b64cd7093594ac3",
    ),
    pinned(
      OutputBelowMinAda { output_index: 45 },
      #"d905269f182dff",
      #"47efd554e1a3c3b8d0612f5580100384669633d9c23afd7d195eba9cb7218573",
    ),
    pinned(
      ValueNotPreserved,
      #"d9052780",
      #"84e558e4cb39753af892fe35f097d3a3d528fe341a76662b628d891b8b5675b7",
    ),
    pinned(
      MintItemMalformed { policy_index: 47 },
      #"d905289f182fff",
      #"ea475757f825e41acf1cd6d3de55252c715db6cbcbac0a5e8286baa38bc6f191",
    ),
  }
}

/// The interim sentinel coordinate (-1), a two-byte coordinate, and the
/// P1 mint-item arm at its smallest coordinate.
test reason_vectors_sentinel_and_width() {
  and {
    pinned(
      DuplicateInput {
        first_field_index: -1,
        first_item_index: -1,
        second_field_index: -1,
        second_item_index: -1,
      },
      #"d87c9f20202020ff",
      #"96e7f93cbb70c6db8dfc2a93ace1adbb2ec5dec7600b422aef738c86d50937ca",
    ),
    pinned(
      InputNotFound { source_kind: 0, input_index: -1 },
      #"d9050b9f0020ff",
      #"92540c0ddc1150a85e4f87ce8be9affe086dd2c235dc5ddf3da712ae43bda343",
    ),
    pinned(
      PlutusExecutionFailed { purpose_kind: 3, purpose_index: 300 },
      #"d905229f0319012cff",
      #"ce9e5d97c13b80dd5090bc19165e7796680e43663281cb2bd0508a088737146a",
    ),
    pinned(
      MintItemMalformed { policy_index: 0 },
      #"d905289f00ff",
      #"f280ed3127a64fb8cb31fecb28c993f21ec05b890003c993d96570e723c255ba",
    ),
  }
}

/// Two reasons of one arm differing only in a coordinate commit to different
/// codes: the code names the exact site, not only the arm.
test reason_code_separates_coordinates() {
  and {
    hash_rejection_code(OutputBelowMinAda { output_index: 0 }) != hash_rejection_code(
      OutputBelowMinAda { output_index: 1 },
    ),
    hash_rejection_code(
      PlutusExecutionFailed { purpose_kind: 0, purpose_index: 1 },
    ) != hash_rejection_code(
      PlutusExecutionFailed { purpose_kind: 1, purpose_index: 0 },
    ),
  }
}

/// The duplicate-pair constructor maps its four arguments positionally onto
/// `DuplicateInput`: first (earlier-scanned) pair, then second pair.
test reject_duplicate_input_maps_the_pair_positionally() {
  let reason = reject_duplicate_input(0, 3, 1, 0)
  and {
    pinned(
      reason,
      #"d87c9f00030100ff",
      #"be6b664e5a0ebfa2ebf8da29c959e7d84d0c6631980897bc8efa9801dec9b2c6",
    ),
    reason == DuplicateInput {
      first_field_index: 0,
      first_item_index: 3,
      second_field_index: 1,
      second_item_index: 0,
    },
    hash_rejection_code(reason) != hash_rejection_code(
      reject_duplicate_input(1, 0, 0, 3),
    ),
  }
}
