import {
  DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type DeploymentManifestV1FraudProofCatalogueIdentity,
  verifyDeploymentManifestV1FraudProofCatalogueIdentity,
} from "../../midgard-core/src/deployment-manifest-identity-v1.js";

type ContractIdentity = Readonly<{ scriptHash: string }>;

const CONTRACT_BY_CATEGORY = Object.freeze({
  doubleSpend: "fraudProofDoubleSpend",
  nonExistentInput: "fraudProofNonExistentInput",
  nonExistentInputNoIndex: "fraudProofNonExistentInputNoIndex",
  invalidRange: "fraudProofInvalidRange",
  transitionTrace: "fraudProofTransitionTrace",
  zeroInput: "fraudProofZeroInput",
  validationTraceDispute: "validationTraceDispute",
  daHashPreimage: "fraudProofDaHashPreimage",
  noReferenceInput: "fraudProofNoReferenceInput",
  referenceInputNoIdx: "fraudProofReferenceInputNoIdx",
  invalidSignature: "fraudProofInvalidSignature",
  fabricatedDeposit: "fraudProofFabricatedDeposit",
  fabricatedWithdrawal: "fraudProofFabricatedWithdrawal",
  nativeScriptDecoding: "fraudProofNativeScriptDecoding",
  missingSignature: "fraudProofMissingSignature",
  missingNativeScriptTx: "fraudProofMissingNativeScriptTx",
  withdrawnReferenceInput: "fraudProofWithdrawnReferenceInput",
  canonicalDecodability: "fraudProofCanonicalDecodability",
  committedFieldShape: "fraudProofCommittedFieldShape",
  minFee: "fraudProofMinFee",
  withdrawalMistag: "fraudProofWithdrawalMistag",
  doubleWithdraw: "fraudProofDoubleWithdraw",
  crossBlockDuplicateEvent: "fraudProofCrossBlockDuplicateEvent",
  l2TxMistag: "fraudProofL2TxMistag",
  withdrawnInput: "fraudProofWithdrawnInput",
  valueNotPreserved: "fraudProofValueNotPreserved",
  inputSetUniqueness: "fraudProofInputSetUniqueness",
  mintAuthorization: "fraudProofMintAuthorization",
} as const);

/**
 * Re-derived from the deployment producer over the positional synthetic
 * contract set used by watcher authority tests. Category registration and
 * appended reference-script roles both move this root, so the fixture is kept
 * as a canonical identity pin rather than synthesized from the assertion.
 */
const POSITIONAL_SCRIPT_CATALOGUE =
  verifyDeploymentManifestV1FraudProofCatalogueIdentity({
    root: "55bbe2184287045c7230e50b27a1fcf2b38c91215168855642b805ffefbf13e9",
    categories: {
      doubleSpend: {
        categoryId: "00000000",
        scriptHash: "9c294dd20e9bc9eda66208e9a7069b5f5dbcbabcc302f77af24a1ebb",
        membershipProofCbor:
          "9fd8799f005f5840faa55c7537c19720d7cd177cd67b1a3971ae86d3cb112cf7ee6e3dce6b9bbcb924d2f74fd0d45e25850ae3cad207fc0dab4752247beeed5f9409beec5cc0dbde5840494e30245b452661d214274c080d99e65dd33dda6cc99d8af5157131b214e703d6d056c456b33714ff3d5b39ecb793ce5c5a280eb7b93231c45cc07dbce5f680ffffd8799f005f5840afe1f10b692f01f3028f67e5f5717564d9c1bc32142369fb0e2193d52da7e49d1e29fc5e676054b7b8cb5c7a5416b333cf628bfa8cc93c063f58e13f08ebf84158400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      nonExistentInput: {
        categoryId: "00000001",
        scriptHash: "394ec87928dca91a6f0ae9b7395e12c9dadb6e03c1c3999826f590b3",
        membershipProofCbor:
          "9fd8799f005f5840dac82bb4198f5f83ce26ec265b24ccf77107513639df40056ae2a555a4815483d2a33dd802a805d04f3220db0e8697b3878ed2a407035e4634dd720f55b90dc6584008e3930d2a17e7b285036a36f35b584aff6bbf0ee16eef8523b2218ede4d95b80000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ee008edbcebc51812e09a76640db05559631cf9730f2fddd093fc72d072d323b5820ae7bd279d277b52b78d6f35c7b55cd6ce09a213209684b0ba29757976351dbebffff",
      },
      nonExistentInputNoIndex: {
        categoryId: "00000002",
        scriptHash: "3c7cf1b727a2e1b8e42c991aea3dcf4576c45165a1ec87cc646c3be4",
        membershipProofCbor:
          "9fd8799f005f5840dac82bb4198f5f83ce26ec265b24ccf77107513639df40056ae2a555a48154835957d0a5af9d0777db62376430f63258c9d5c0b48cf50f92366749c0a8df80b4584012f1fb926209ae7ce685c439653c4db169c7c02b1ab70efca31bf7c5b7b5f18563e65669629b60cde3ff15e76a477fecad04ffc6edc76cfef83ff27c6a8759a6ffffd8799f005f5840b22df1a126b5ba4e33c16fd6157507610e55ffce20dae7ac44cae168a463612a93df2210e6f062f58f2e7a9d04a12bc2c206e9f62fa48ab855b3178874e94b8b58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97619fac65a85ad636d767dabcf821dbb0d02c4b97b34b71f99702f1de1270142b70ffffff",
      },
      invalidRange: {
        categoryId: "00000003",
        scriptHash: "9dfd836b784ba4a6d2ae0d834e11290f5b552e651a5f1d32339a5d2d",
        membershipProofCbor:
          "9fd8799f005f5840dac82bb4198f5f83ce26ec265b24ccf77107513639df40056ae2a555a4815483d2a33dd802a805d04f3220db0e8697b3878ed2a407035e4634dd720f55b90dc6584008e3930d2a17e7b285036a36f35b584aff6bbf0ee16eef8523b2218ede4d95b80000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ef9660efe206d50189739680bd866387c07a061a7b5de18f85b6af241f1686385820be9292844c478407ac234010335515c74c73e41872c9d72b5b88342cb9a4931affff",
      },
      transitionTrace: {
        categoryId: "00000004",
        scriptHash: "fa33d919b9072221cab6322dbba0839f89cb70241c01ec7939fb374c",
        membershipProofCbor:
          "9fd8799f005f5840dac82bb4198f5f83ce26ec265b24ccf77107513639df40056ae2a555a48154835957d0a5af9d0777db62376430f63258c9d5c0b48cf50f92366749c0a8df80b4584012f1fb926209ae7ce685c439653c4db169c7c02b1ab70efca31bf7c5b7b5f18563e65669629b60cde3ff15e76a477fecad04ffc6edc76cfef83ff27c6a8759a6ffffd8799f005f5840b22df1a126b5ba4e33c16fd6157507610e55ffce20dae7ac44cae168a463612a95397cf37b1d803e408a51396be079cf3a742d94a2775948ca37063f66dfff5858400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      zeroInput: {
        categoryId: "00000005",
        scriptHash: "1486de3debbd969c6922c5badd51a94dc959038066a0e2de48b65815",
        membershipProofCbor:
          "9fd8799f005f5840faa55c7537c19720d7cd177cd67b1a3971ae86d3cb112cf7ee6e3dce6b9bbcb9a90d94b33b686d48e6ed6eaeb09f32761a1101b3428bf6a4c7ca731c0a4a12ff58400a10a6c19f45f6aefad7bb62b764dcb00f7270941266fc96fba227cd43b32d5466ab89cd867f6fd7224c7483b3d8a9945da2ae232c23874a9948e03b1463cdb6ffffd87b9f0058204d539335cc05bbb119d570610044977c7c13a943bb5c13611d7162e1b2269fa35820a68e8fc2724b44e5f942af7d81150df5ca9f6a33e3df60cd93984da6afff2216ffff",
      },
      validationTraceDispute: {
        categoryId: "00000006",
        scriptHash: "7a7b8dfce83085bb6f120f13f01d05ac9cba8672a0fd619692b41912",
        membershipProofCbor:
          "9fd8799f005f5840dac82bb4198f5f83ce26ec265b24ccf77107513639df40056ae2a555a48154835957d0a5af9d0777db62376430f63258c9d5c0b48cf50f92366749c0a8df80b45840963d98951876fea34491e7fb49822d779d1c403abfcd683e644b4abde058437a894aa0d493ff82ebdfe65c6fab89f7d6a265528facacc2030d0a7ac424686884ffffd8799f005f58403a229d063a50f4ee03544e6beaab8e6ac084863e4ac053fdae4a1b14b886c06b85c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b1584047172c00c7ff011402f0b25a480da3cdbf5ba3fcdcd1649aa7f05b5d9d6c2ca80000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      daHashPreimage: {
        categoryId: "00000007",
        scriptHash: "69bdfdab36b50823d1d4007b02be37134b2ab1b865066e8d1d3e6e04",
        membershipProofCbor:
          "9fd8799f005f5840faa55c7537c19720d7cd177cd67b1a3971ae86d3cb112cf7ee6e3dce6b9bbcb924d2f74fd0d45e25850ae3cad207fc0dab4752247beeed5f9409beec5cc0dbde5840494e30245b452661d214274c080d99e65dd33dda6cc99d8af5157131b214e703d6d056c456b33714ff3d5b39ecb793ce5c5a280eb7b93231c45cc07dbce5f680ffffd8799f005f58404d29d706ed22daa11651978adfc174e6b5a7764d4cda1c047e71e418555f4d3b85c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b158400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610b31b4703f69f13d5a843b79ed448f05e0180ec6bb93ed88ff4899b995d33863ffffff",
      },
      noReferenceInput: {
        categoryId: "00000008",
        scriptHash: "4825f9cbd0477cb8aae650a39a0d9264d0c6b89a07023e0dbcc849f6",
        membershipProofCbor:
          "9fd8799f005f5840dac82bb4198f5f83ce26ec265b24ccf77107513639df40056ae2a555a48154835957d0a5af9d0777db62376430f63258c9d5c0b48cf50f92366749c0a8df80b45840963d98951876fea34491e7fb49822d779d1c403abfcd683e644b4abde058437a894aa0d493ff82ebdfe65c6fab89f7d6a265528facacc2030d0a7ac424686884ffffd8799f005f58403a229d063a50f4ee03544e6beaab8e6ac084863e4ac053fdae4a1b14b886c06b85c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b15840444d87163c64e5e219f4976d4acc1fcfbb2c9dbeee4f26f43f8228f85e6263120000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      referenceInputNoIdx: {
        categoryId: "00000009",
        scriptHash: "5bfeeac158e74889619ce0f963b8e05c9cb30ac721ce9673099891cb",
        membershipProofCbor:
          "9fd8799f005f5840faa55c7537c19720d7cd177cd67b1a3971ae86d3cb112cf7ee6e3dce6b9bbcb9a90d94b33b686d48e6ed6eaeb09f32761a1101b3428bf6a4c7ca731c0a4a12ff58400a10a6c19f45f6aefad7bb62b764dcb00f7270941266fc96fba227cd43b32d549ad6d9863aff26baf3f6c9eddf8c3824b771ab5bf14cd1f6d47964f1d3d28049ffffd87b9f00582052149618b234c08150fd26ae73bdef9bd7843240f0a25a50d9798ca6ebe52fa95820932e5fb134f5dadd27ca470fba3cbd3dda580cd5ba57c02657531f4dc02d04aaffff",
      },
      invalidSignature: {
        categoryId: "0000000a",
        scriptHash: "216bf7588748dd26962b470586c46e9c069a43167b0ce93cfffe26ea",
        membershipProofCbor:
          "9fd8799f005f5840dac82bb4198f5f83ce26ec265b24ccf77107513639df40056ae2a555a4815483d2a33dd802a805d04f3220db0e8697b3878ed2a407035e4634dd720f55b90dc658406b99558eead19428d58b4b0008d34acb70ec564adf2a1c6f2f5000bd4e598d2fbffcd2b4168eddb129c8f80397a2b991903c591d6d018c282b6aeeba661b57f3ffffd87b9f005820d058b2a00a48c1053abeb11bf3b5669ec7ad5d17824b518fe8c5bb99fe9024cc582013ce3b1f2e2b949ba0a2b1c2d6873734752e63911a5ab85f9c2e7bda523790c5ffff",
      },
      fabricatedDeposit: {
        categoryId: "0000000b",
        scriptHash: "cfcace059b5f3fbfe8744ebe8f377ca82b2fba5512ce41fea29bbf0e",
        membershipProofCbor:
          "9fd8799f005f5840faa55c7537c19720d7cd177cd67b1a3971ae86d3cb112cf7ee6e3dce6b9bbcb924d2f74fd0d45e25850ae3cad207fc0dab4752247beeed5f9409beec5cc0dbde5840494e30245b452661d214274c080d99e65dd33dda6cc99d8af5157131b214e703d6d056c456b33714ff3d5b39ecb793ce5c5a280eb7b93231c45cc07dbce5f680ffffd8799f005f58404d29d706ed22daa11651978adfc174e6b5a7764d4cda1c047e71e418555f4d3b85c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b158400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97617d23621920ea5cfb990405b40d0977c320b19110ec2770f99cb5b43e00808eebffffff",
      },
      fabricatedWithdrawal: {
        categoryId: "0000000c",
        scriptHash: "080f99d32ae3e08dba347b59a66041fdc3861cbec61a13d2ac42fb1c",
        membershipProofCbor:
          "9fd8799f005f5840dac82bb4198f5f83ce26ec265b24ccf77107513639df40056ae2a555a4815483d2a33dd802a805d04f3220db0e8697b3878ed2a407035e4634dd720f55b90dc658406b99558eead19428d58b4b0008d34acb70ec564adf2a1c6f2f5000bd4e598d2fbffcd2b4168eddb129c8f80397a2b991903c591d6d018c282b6aeeba661b57f3ffffd87b9f005820d3d63f5628388eee4a0dde3296830a843cc7ebda255b0abfb4e058d1e058e7f4582075f3d8a83f8ea240c9752055be77fe3c802010419d583e660ec246f7bfa8c44bffff",
      },
      nativeScriptDecoding: {
        categoryId: "0000000d",
        scriptHash: "c89af4528795a17a3c80f567be7f0879be62d6747e8d145c7b57d511",
        membershipProofCbor:
          "9fd8799f005f5840dac82bb4198f5f83ce26ec265b24ccf77107513639df40056ae2a555a48154835957d0a5af9d0777db62376430f63258c9d5c0b48cf50f92366749c0a8df80b4584012f1fb926209ae7ce685c439653c4db169c7c02b1ab70efca31bf7c5b7b5f18563e65669629b60cde3ff15e76a477fecad04ffc6edc76cfef83ff27c6a8759a6ffffd8799f005f5840b22df1a126b5ba4e33c16fd6157507610e55ffce20dae7ac44cae168a463612a93df2210e6f062f58f2e7a9d04a12bc2c206e9f62fa48ab855b3178874e94b8b58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f9761c4653bd0b2257cc4fea8d68f164e61e8f08d10788870191abdb96546cfbdde8effffff",
      },
      missingSignature: {
        categoryId: "0000000e",
        scriptHash: "5e550aad8931a16a53890849973dde5da78d9b28ab2af6f2e07753f7",
        membershipProofCbor:
          "9fd8799f005f5840dac82bb4198f5f83ce26ec265b24ccf77107513639df40056ae2a555a4815483d2a33dd802a805d04f3220db0e8697b3878ed2a407035e4634dd720f55b90dc658406b99558eead19428d58b4b0008d34acb70ec564adf2a1c6f2f5000bd4e598d2ff14a7250f1fd81c5ff790d279620d4da71d72c008bb17ffa6f014b20c3b7ab28ffffff",
      },
      missingNativeScriptTx: {
        categoryId: "0000000f",
        scriptHash: "7093ec86e57fa8cfc27f6871ff67b97bf2cecce01fbf226c84ecb9d7",
        membershipProofCbor:
          "9fd8799f005f5840faa55c7537c19720d7cd177cd67b1a3971ae86d3cb112cf7ee6e3dce6b9bbcb9a90d94b33b686d48e6ed6eaeb09f32761a1101b3428bf6a4c7ca731c0a4a12ff584051ee76f29652b3b0659b44a90bca0d786298bb3dc91d7532d56e087c1788d6b60000000000000000000000000000000000000000000000000000000000000000ffffd87b9f00582074d95aa7096d5f8293cda4d8589964ea8d8ec37a0479478544820c4a52c912cd5820d14448942801b518ebc4208d8362d0e9ef0981eeddc10a0ed54c12bdaeeaa543ffff",
      },
      withdrawnReferenceInput: {
        categoryId: "00000010",
        scriptHash: "9c4883efd3e78a065c7584f7257deb6fc7bac71a083def812eff68f4",
        membershipProofCbor:
          "9fd8799f005f5840dac82bb4198f5f83ce26ec265b24ccf77107513639df40056ae2a555a48154835957d0a5af9d0777db62376430f63258c9d5c0b48cf50f92366749c0a8df80b4584012f1fb926209ae7ce685c439653c4db169c7c02b1ab70efca31bf7c5b7b5f185479666d147c46a9ac92b99c195ad994a6583688da34aec3a681fd7aa65922f62ffffff",
      },
      canonicalDecodability: {
        categoryId: "00000011",
        scriptHash: "1ff0e9ecb714aa264dc65d1456ac29906ee881c4f918f24dc4620f3c",
        membershipProofCbor:
          "9fd8799f005f5840faa55c7537c19720d7cd177cd67b1a3971ae86d3cb112cf7ee6e3dce6b9bbcb924d2f74fd0d45e25850ae3cad207fc0dab4752247beeed5f9409beec5cc0dbde5840494e30245b452661d214274c080d99e65dd33dda6cc99d8af5157131b214e7033b0db03998f9b23e94eeeeb5bd93621a142f17ea7391a1cb21c0b344f341a2aeffffd87b9f005820342462f44a6302b0049152bc292ac60f4d7617a5fd7f226943e58ef7223fccd6582008ad68fd6dd3f5be9afb5d9b8dd801752a46301fe8a623292bcd849244248d82ffff",
      },
      committedFieldShape: {
        categoryId: "00000012",
        scriptHash: "f199c3f32f378d1711a0104a7ed16f1e50cef6738fcdd0697b3b26c3",
        membershipProofCbor:
          "9fd8799f005f5840dac82bb4198f5f83ce26ec265b24ccf77107513639df40056ae2a555a48154835957d0a5af9d0777db62376430f63258c9d5c0b48cf50f92366749c0a8df80b45840963d98951876fea34491e7fb49822d779d1c403abfcd683e644b4abde058437a894aa0d493ff82ebdfe65c6fab89f7d6a265528facacc2030d0a7ac424686884ffffd8799f005f5840fa20f7904d69edf460fb8ee4f7b4acf0b22b93b3890b0124c369ff3098e6f09385c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b158400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      minFee: {
        categoryId: "00000013",
        scriptHash: "99645534e751635640dd74e3184cf45e1a6c324e2779324e9fe3bd07",
        membershipProofCbor:
          "9fd8799f005f5840faa55c7537c19720d7cd177cd67b1a3971ae86d3cb112cf7ee6e3dce6b9bbcb924d2f74fd0d45e25850ae3cad207fc0dab4752247beeed5f9409beec5cc0dbde5840f529b32b366c051f08839230451e3d6ce154a9f2b6bc63b0807c625bd9ee643a0000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820129e0add80680412132355f87a5229a5f4aa9415a232277c56fbd3d54077bfe15820fc583860d5d6971cac39edc4ec8d04626180420570126f5343ac7be258db489affff",
      },
      withdrawalMistag: {
        categoryId: "00000014",
        scriptHash: "36bda9f359ea4107470a8c745c7e8ac2145958f063bc0c5a65bdb66f",
        membershipProofCbor:
          "9fd8799f005f5840faa55c7537c19720d7cd177cd67b1a3971ae86d3cb112cf7ee6e3dce6b9bbcb9a90d94b33b686d48e6ed6eaeb09f32761a1101b3428bf6a4c7ca731c0a4a12ff584051ee76f29652b3b0659b44a90bca0d786298bb3dc91d7532d56e087c1788d6b60000000000000000000000000000000000000000000000000000000000000000ffffd87b9f0058207e023fd5b23a747196d48a15c4318e292acf4fd1a25e848552f9af7c56d5d0245820a61c1106b48c6280db508977452ed887393d505f32738d5fd16e27e0d85704a9ffff",
      },
      doubleWithdraw: {
        categoryId: "00000015",
        scriptHash: "446cbd4f5f95a7c551b22345d4ef6181e478042c78e009ee214a92ce",
        membershipProofCbor:
          "9fd8799f005f5840faa55c7537c19720d7cd177cd67b1a3971ae86d3cb112cf7ee6e3dce6b9bbcb9a90d94b33b686d48e6ed6eaeb09f32761a1101b3428bf6a4c7ca731c0a4a12ff58400a10a6c19f45f6aefad7bb62b764dcb00f7270941266fc96fba227cd43b32d549ad6d9863aff26baf3f6c9eddf8c3824b771ab5bf14cd1f6d47964f1d3d28049ffffd87b9f0058205fce224aa235294c791e822adfc03c7944cf388696e988e1d9cffd72780a841658206d917a4659837fee0d7f05e0c7d149a71f66b00cc61fa518960f438e7a80be84ffff",
      },
      crossBlockDuplicateEvent: {
        categoryId: "00000016",
        scriptHash: "58bb6f8170bdb893f4c458270cf6790d6f2022bcc18c47cad1076038",
        membershipProofCbor:
          "9fd8799f005f5840faa55c7537c19720d7cd177cd67b1a3971ae86d3cb112cf7ee6e3dce6b9bbcb924d2f74fd0d45e25850ae3cad207fc0dab4752247beeed5f9409beec5cc0dbde5840f529b32b366c051f08839230451e3d6ce154a9f2b6bc63b0807c625bd9ee643a0000000000000000000000000000000000000000000000000000000000000000ffffd87b9f00582018381c1e06288c341974ff129a8c50fec85b71b3c73b87e9a26b0d6d8da7b18a58200b209b19c78aba8bdf864441a2a5fed4c5f5c4f49aecb782895ada4560ff203dffff",
      },
      l2TxMistag: {
        categoryId: "00000017",
        scriptHash: "8eee9dc8b4e8edb75c1923801202b5685de486efd444204efed11b38",
        membershipProofCbor:
          "9fd8799f005f5840faa55c7537c19720d7cd177cd67b1a3971ae86d3cb112cf7ee6e3dce6b9bbcb924d2f74fd0d45e25850ae3cad207fc0dab4752247beeed5f9409beec5cc0dbde5840494e30245b452661d214274c080d99e65dd33dda6cc99d8af5157131b214e7033b0db03998f9b23e94eeeeb5bd93621a142f17ea7391a1cb21c0b344f341a2aeffffd87b9f0058203a219ce19d5583088930b21e35dcc030242e8a7873e5650164adafaef7ea17ea58201de8d5af1a9a21d60f7438dd7cdd3b2a184340de404920cfa0f08a0f9cecfc9dffff",
      },
      withdrawnInput: {
        categoryId: "00000018",
        scriptHash: "69cc4fa206a2ffa1b0f9fa1e6e418d9dc944fa30804c416e8b5508b4",
        membershipProofCbor:
          "9fd8799f005f5840faa55c7537c19720d7cd177cd67b1a3971ae86d3cb112cf7ee6e3dce6b9bbcb9a90d94b33b686d48e6ed6eaeb09f32761a1101b3428bf6a4c7ca731c0a4a12ff58400a10a6c19f45f6aefad7bb62b764dcb00f7270941266fc96fba227cd43b32d5466ab89cd867f6fd7224c7483b3d8a9945da2ae232c23874a9948e03b1463cdb6ffffd87b9f00582043b91e923070f319b8f50b95e78954278f5896c42868698eaf2d10c226d28a5a5820b2b437710c1902cd213fb2dbfa15b8d8c9a23832a0fe07c17085ea14c4e850fcffff",
      },
      valueNotPreserved: {
        categoryId: "00000019",
        scriptHash: "91b8648d74804427af14a596ab1ad50976918ba405ada21305d8996c",
        membershipProofCbor:
          "9fd8799f005f5840dac82bb4198f5f83ce26ec265b24ccf77107513639df40056ae2a555a48154835957d0a5af9d0777db62376430f63258c9d5c0b48cf50f92366749c0a8df80b45840963d98951876fea34491e7fb49822d779d1c403abfcd683e644b4abde058437a848c14b44511d525377bcae699094dc10245d39e393454689b6d4145f4feac7dffffd87b9f005820a2641c1ac0d1e6c78e8717612ef6b33e39a8c7227fd0a064b34b384919ff8e525820286da4ff32f9a677461832649469fbaf00afcacee4228151cc690d7e2af7445dffff",
      },
      inputSetUniqueness: {
        categoryId: "0000001a",
        scriptHash: "6369283bc236f55541ccdbe52f38dd70c24db692af6e61c3f03ec03b",
        membershipProofCbor:
          "9fd8799f005f5840faa55c7537c19720d7cd177cd67b1a3971ae86d3cb112cf7ee6e3dce6b9bbcb924d2f74fd0d45e25850ae3cad207fc0dab4752247beeed5f9409beec5cc0dbde5840494e30245b452661d214274c080d99e65dd33dda6cc99d8af5157131b214e703d6d056c456b33714ff3d5b39ecb793ce5c5a280eb7b93231c45cc07dbce5f680ffffd8799f005f5840afe1f10b692f01f3028f67e5f5717564d9c1bc32142369fb0e2193d52da7e49d6c4f1d4e95bf3933b2ff5800e119a94fec77dee0d23a5560d21baa7d067fce5458400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      mintAuthorization: {
        categoryId: "0000001b",
        scriptHash: "f3aab62acd0ebf912eee03f162c8ca5a959a36dd1cfa01cab3bb9767",
        membershipProofCbor:
          "9fd8799f005f5840dac82bb4198f5f83ce26ec265b24ccf77107513639df40056ae2a555a48154835957d0a5af9d0777db62376430f63258c9d5c0b48cf50f92366749c0a8df80b45840963d98951876fea34491e7fb49822d779d1c403abfcd683e644b4abde058437a848c14b44511d525377bcae699094dc10245d39e393454689b6d4145f4feac7dffffd87b9f005820a7c2b9bf5be11a84ddc71691490874e526afef78e0cd66ce1db27cab621a7efa5820cbbfc1d8494e8ccdcab4c300d3ea377ff5bd18894296df0520185143759b47a1ffff",
      },
    },
  });

const matchesDeployedScripts = (
  catalogue: DeploymentManifestV1FraudProofCatalogueIdentity,
  contracts: Readonly<Record<string, ContractIdentity>>,
): boolean =>
  DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.every(
    (category) =>
      catalogue.categories[category].scriptHash ===
      contracts[CONTRACT_BY_CATEGORY[category]]?.scriptHash,
  );

export const canonicalFraudProofCatalogueFixture = (
  contracts: Readonly<Record<string, ContractIdentity>>,
): DeploymentManifestV1FraudProofCatalogueIdentity => {
  if (!matchesDeployedScripts(POSITIONAL_SCRIPT_CATALOGUE, contracts)) {
    throw new Error(
      "No canonical watcher fraud-proof catalogue fixture matches the deployed scripts",
    );
  }
  return structuredClone(POSITIONAL_SCRIPT_CATALOGUE);
};
