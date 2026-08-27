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
} as const);

/**
 * Re-derived from the deployment producer over the positional synthetic
 * contract set used by watcher authority tests. Category registration and
 * appended reference-script roles both move this root, so the fixture is kept
 * as a canonical identity pin rather than synthesized from the assertion.
 */
const POSITIONAL_SCRIPT_CATALOGUE =
  verifyDeploymentManifestV1FraudProofCatalogueIdentity({
    root: "e90488767da512a65637528ffe3ac9205883980044e0eeafe42d42520f7406b9",
    categories: {
      doubleSpend: {
        categoryId: "00000000",
        scriptHash: "9c294dd20e9bc9eda66208e9a7069b5f5dbcbabcc302f77af24a1ebb",
        membershipProofCbor:
          "9fd8799f005f5840322e9b797c837152e7ee509f78f0623d22bb23d6836c947ce6a8e2709a8033b7a910d858f5aafa98346adab4bf0a1ec4d8f8686ba006a706b1abcd4a60a1a1fd58406a95a74dcdd295f9edf72b0e8b61a93127cd6def80ccd975e28e804f70d5a71425fa75a3b104dcbc7db0af00f3921ad77ba7019af0f4d2339fc5ad8b2a37fc5cffffd8799f005f5840afe1f10b692f01f3028f67e5f5717564d9c1bc32142369fb0e2193d52da7e49d85c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b158400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      nonExistentInput: {
        categoryId: "00000001",
        scriptHash: "394ec87928dca91a6f0ae9b7395e12c9dadb6e03c1c3999826f590b3",
        membershipProofCbor:
          "9fd8799f005f584039ad34d2df4ad8a8a86493b9258b75d5b4ab4f622db261197307add5bf0da2611cf8af1aa80f7df18c9247bb4f6deca4db743737ed4bfca95fa8d426b6ac539c58408cce1f92de1f47259965cce651db34d19ffa283660053388e2eeafe3ab5140790000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ee008edbcebc51812e09a76640db05559631cf9730f2fddd093fc72d072d323b5820ae7bd279d277b52b78d6f35c7b55cd6ce09a213209684b0ba29757976351dbebffff",
      },
      nonExistentInputNoIndex: {
        categoryId: "00000002",
        scriptHash: "3c7cf1b727a2e1b8e42c991aea3dcf4576c45165a1ec87cc646c3be4",
        membershipProofCbor:
          "9fd8799f005f584039ad34d2df4ad8a8a86493b9258b75d5b4ab4f622db261197307add5bf0da26148b81996ce4f4f5539c1c724c6afe62d79849efae1af1eed24ad7f0a378e5e1e5840ed7ae3ddb177e3cf7e68e77ca70c0fc495ecda9035a4e8c4b511d39734f9f85e5d9527d30b567f40ec178bd94d47ae86bd474c7b0abfe862546184daaaeafc61ffffd8799f005f5840b22df1a126b5ba4e33c16fd6157507610e55ffce20dae7ac44cae168a463612a93df2210e6f062f58f2e7a9d04a12bc2c206e9f62fa48ab855b3178874e94b8b58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97619fac65a85ad636d767dabcf821dbb0d02c4b97b34b71f99702f1de1270142b70ffffff",
      },
      invalidRange: {
        categoryId: "00000003",
        scriptHash: "9dfd836b784ba4a6d2ae0d834e11290f5b552e651a5f1d32339a5d2d",
        membershipProofCbor:
          "9fd8799f005f584039ad34d2df4ad8a8a86493b9258b75d5b4ab4f622db261197307add5bf0da2611cf8af1aa80f7df18c9247bb4f6deca4db743737ed4bfca95fa8d426b6ac539c58408cce1f92de1f47259965cce651db34d19ffa283660053388e2eeafe3ab5140790000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ef9660efe206d50189739680bd866387c07a061a7b5de18f85b6af241f1686385820be9292844c478407ac234010335515c74c73e41872c9d72b5b88342cb9a4931affff",
      },
      transitionTrace: {
        categoryId: "00000004",
        scriptHash: "fa33d919b9072221cab6322dbba0839f89cb70241c01ec7939fb374c",
        membershipProofCbor:
          "9fd8799f005f584039ad34d2df4ad8a8a86493b9258b75d5b4ab4f622db261197307add5bf0da26148b81996ce4f4f5539c1c724c6afe62d79849efae1af1eed24ad7f0a378e5e1e5840ed7ae3ddb177e3cf7e68e77ca70c0fc495ecda9035a4e8c4b511d39734f9f85e5d9527d30b567f40ec178bd94d47ae86bd474c7b0abfe862546184daaaeafc61ffffd8799f005f5840b22df1a126b5ba4e33c16fd6157507610e55ffce20dae7ac44cae168a463612a95397cf37b1d803e408a51396be079cf3a742d94a2775948ca37063f66dfff5858400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      zeroInput: {
        categoryId: "00000005",
        scriptHash: "1486de3debbd969c6922c5badd51a94dc959038066a0e2de48b65815",
        membershipProofCbor:
          "9fd8799f005f5840322e9b797c837152e7ee509f78f0623d22bb23d6836c947ce6a8e2709a8033b72ecf74033192835350169cf3eff86f80207c52ff850a12745582159c5c8d803e584099bd446e70ef0f9d5de12e78aa4f83a3b74c26af7a31fc7585c18fbdb8fc24048eeaf204b5f64f6e39830ee898be3e97e976be8b9273023754ec2560f8d6699cffffd87b9f0058204d539335cc05bbb119d570610044977c7c13a943bb5c13611d7162e1b2269fa3582008ad68fd6dd3f5be9afb5d9b8dd801752a46301fe8a623292bcd849244248d82ffff",
      },
      validationTraceDispute: {
        categoryId: "00000006",
        scriptHash: "7a7b8dfce83085bb6f120f13f01d05ac9cba8672a0fd619692b41912",
        membershipProofCbor:
          "9fd8799f005f584039ad34d2df4ad8a8a86493b9258b75d5b4ab4f622db261197307add5bf0da26148b81996ce4f4f5539c1c724c6afe62d79849efae1af1eed24ad7f0a378e5e1e58403c115359267a107fba54d93b91a85604304e2e0a5e94a5facd7471c114b732ea0000000000000000000000000000000000000000000000000000000000000000ffffd8799f005f58404a9a12a8fa3b87575aaca8ac3d34d0df3841f52e9ad8496f41953ace16a6bb5585c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b1584047172c00c7ff011402f0b25a480da3cdbf5ba3fcdcd1649aa7f05b5d9d6c2ca80000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      daHashPreimage: {
        categoryId: "00000007",
        scriptHash: "69bdfdab36b50823d1d4007b02be37134b2ab1b865066e8d1d3e6e04",
        membershipProofCbor:
          "9fd8799f005f5840322e9b797c837152e7ee509f78f0623d22bb23d6836c947ce6a8e2709a8033b7a910d858f5aafa98346adab4bf0a1ec4d8f8686ba006a706b1abcd4a60a1a1fd58406a95a74dcdd295f9edf72b0e8b61a93127cd6def80ccd975e28e804f70d5a71425fa75a3b104dcbc7db0af00f3921ad77ba7019af0f4d2339fc5ad8b2a37fc5cffffd8799f005f58409d0607b2143fd6107e2773eefd116ab877d6cfd5ee80c9d06769a82fa21765db85c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b158400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610b31b4703f69f13d5a843b79ed448f05e0180ec6bb93ed88ff4899b995d33863ffffff",
      },
      noReferenceInput: {
        categoryId: "00000008",
        scriptHash: "4825f9cbd0477cb8aae650a39a0d9264d0c6b89a07023e0dbcc849f6",
        membershipProofCbor:
          "9fd8799f005f584039ad34d2df4ad8a8a86493b9258b75d5b4ab4f622db261197307add5bf0da26148b81996ce4f4f5539c1c724c6afe62d79849efae1af1eed24ad7f0a378e5e1e58403c115359267a107fba54d93b91a85604304e2e0a5e94a5facd7471c114b732ea0000000000000000000000000000000000000000000000000000000000000000ffffd8799f005f58404a9a12a8fa3b87575aaca8ac3d34d0df3841f52e9ad8496f41953ace16a6bb5585c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b15840444d87163c64e5e219f4976d4acc1fcfbb2c9dbeee4f26f43f8228f85e6263120000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      referenceInputNoIdx: {
        categoryId: "00000009",
        scriptHash: "5bfeeac158e74889619ce0f963b8e05c9cb30ac721ce9673099891cb",
        membershipProofCbor:
          "9fd8799f005f5840322e9b797c837152e7ee509f78f0623d22bb23d6836c947ce6a8e2709a8033b72ecf74033192835350169cf3eff86f80207c52ff850a12745582159c5c8d803e584099bd446e70ef0f9d5de12e78aa4f83a3b74c26af7a31fc7585c18fbdb8fc2404b745b52e163f3d418e8b56684925b060fc90bb4c2f8b5fd58a984b124caf98afffffd87b9f00582052149618b234c08150fd26ae73bdef9bd7843240f0a25a50d9798ca6ebe52fa958207de3b4e27535f5952831df183316a00f7f7465c3ff5f8d3c7383c9903d288d96ffff",
      },
      invalidSignature: {
        categoryId: "0000000a",
        scriptHash: "216bf7588748dd26962b470586c46e9c069a43167b0ce93cfffe26ea",
        membershipProofCbor:
          "9fd8799f005f584039ad34d2df4ad8a8a86493b9258b75d5b4ab4f622db261197307add5bf0da2611cf8af1aa80f7df18c9247bb4f6deca4db743737ed4bfca95fa8d426b6ac539c58406b99558eead19428d58b4b0008d34acb70ec564adf2a1c6f2f5000bd4e598d2f10bd8cde621becdfa223a60abf85b3c7bed49de869c915b0fd61b232e3f64334ffffd87b9f005820d058b2a00a48c1053abeb11bf3b5669ec7ad5d17824b518fe8c5bb99fe9024cc582013ce3b1f2e2b949ba0a2b1c2d6873734752e63911a5ab85f9c2e7bda523790c5ffff",
      },
      fabricatedDeposit: {
        categoryId: "0000000b",
        scriptHash: "cfcace059b5f3fbfe8744ebe8f377ca82b2fba5512ce41fea29bbf0e",
        membershipProofCbor:
          "9fd8799f005f5840322e9b797c837152e7ee509f78f0623d22bb23d6836c947ce6a8e2709a8033b7a910d858f5aafa98346adab4bf0a1ec4d8f8686ba006a706b1abcd4a60a1a1fd58406a95a74dcdd295f9edf72b0e8b61a93127cd6def80ccd975e28e804f70d5a71425fa75a3b104dcbc7db0af00f3921ad77ba7019af0f4d2339fc5ad8b2a37fc5cffffd8799f005f58409d0607b2143fd6107e2773eefd116ab877d6cfd5ee80c9d06769a82fa21765db85c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b158400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97617d23621920ea5cfb990405b40d0977c320b19110ec2770f99cb5b43e00808eebffffff",
      },
      fabricatedWithdrawal: {
        categoryId: "0000000c",
        scriptHash: "080f99d32ae3e08dba347b59a66041fdc3861cbec61a13d2ac42fb1c",
        membershipProofCbor:
          "9fd8799f005f584039ad34d2df4ad8a8a86493b9258b75d5b4ab4f622db261197307add5bf0da2611cf8af1aa80f7df18c9247bb4f6deca4db743737ed4bfca95fa8d426b6ac539c58406b99558eead19428d58b4b0008d34acb70ec564adf2a1c6f2f5000bd4e598d2f10bd8cde621becdfa223a60abf85b3c7bed49de869c915b0fd61b232e3f64334ffffd87b9f005820d3d63f5628388eee4a0dde3296830a843cc7ebda255b0abfb4e058d1e058e7f4582075f3d8a83f8ea240c9752055be77fe3c802010419d583e660ec246f7bfa8c44bffff",
      },
      nativeScriptDecoding: {
        categoryId: "0000000d",
        scriptHash: "c89af4528795a17a3c80f567be7f0879be62d6747e8d145c7b57d511",
        membershipProofCbor:
          "9fd8799f005f584039ad34d2df4ad8a8a86493b9258b75d5b4ab4f622db261197307add5bf0da26148b81996ce4f4f5539c1c724c6afe62d79849efae1af1eed24ad7f0a378e5e1e5840ed7ae3ddb177e3cf7e68e77ca70c0fc495ecda9035a4e8c4b511d39734f9f85e5d9527d30b567f40ec178bd94d47ae86bd474c7b0abfe862546184daaaeafc61ffffd8799f005f5840b22df1a126b5ba4e33c16fd6157507610e55ffce20dae7ac44cae168a463612a93df2210e6f062f58f2e7a9d04a12bc2c206e9f62fa48ab855b3178874e94b8b58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f9761c4653bd0b2257cc4fea8d68f164e61e8f08d10788870191abdb96546cfbdde8effffff",
      },
      missingSignature: {
        categoryId: "0000000e",
        scriptHash: "c1a43ce22d3608f89f68a0790f042bb0fda651fc29fab030698776ef",
        membershipProofCbor:
          "9fd8799f005f584039ad34d2df4ad8a8a86493b9258b75d5b4ab4f622db261197307add5bf0da2611cf8af1aa80f7df18c9247bb4f6deca4db743737ed4bfca95fa8d426b6ac539c58406b99558eead19428d58b4b0008d34acb70ec564adf2a1c6f2f5000bd4e598d2ff14a7250f1fd81c5ff790d279620d4da71d72c008bb17ffa6f014b20c3b7ab28ffffff",
      },
      missingNativeScriptTx: {
        categoryId: "0000000f",
        scriptHash: "089d5f3a30ab98beaf3ca7364c2f8da88bc772d0b63da6685c2c4420",
        membershipProofCbor:
          "9fd8799f005f5840322e9b797c837152e7ee509f78f0623d22bb23d6836c947ce6a8e2709a8033b72ecf74033192835350169cf3eff86f80207c52ff850a12745582159c5c8d803e584004d1936827765bf22959d72b5b30be0429da2839962d2bcc1ad632561c2dac6a0000000000000000000000000000000000000000000000000000000000000000ffffd87b9f00582074d95aa7096d5f8293cda4d8589964ea8d8ec37a0479478544820c4a52c912cd58200b209b19c78aba8bdf864441a2a5fed4c5f5c4f49aecb782895ada4560ff203dffff",
      },
      withdrawnReferenceInput: {
        categoryId: "00000010",
        scriptHash: "89b95f084f4f0e4da9558cf3812b5434ac687962d8d5d37b8f763f6b",
        membershipProofCbor:
          "9fd8799f005f584039ad34d2df4ad8a8a86493b9258b75d5b4ab4f622db261197307add5bf0da26148b81996ce4f4f5539c1c724c6afe62d79849efae1af1eed24ad7f0a378e5e1e5840ed7ae3ddb177e3cf7e68e77ca70c0fc495ecda9035a4e8c4b511d39734f9f85e479666d147c46a9ac92b99c195ad994a6583688da34aec3a681fd7aa65922f62ffffff",
      },
      canonicalDecodability: {
        categoryId: "00000011",
        scriptHash: "a55d8e5213cb3e85c3d5a261219dae26006b5a1b1826fd1069f4d673",
        membershipProofCbor:
          "9fd8799f005f5840322e9b797c837152e7ee509f78f0623d22bb23d6836c947ce6a8e2709a8033b7a910d858f5aafa98346adab4bf0a1ec4d8f8686ba006a706b1abcd4a60a1a1fd58406a95a74dcdd295f9edf72b0e8b61a93127cd6def80ccd975e28e804f70d5a714ff42e9ae5c5dc468dcce528e4194d1dfe3eae9b77a1dc70e189a7dc894b7945bffffd87b9f005820342462f44a6302b0049152bc292ac60f4d7617a5fd7f226943e58ef7223fccd65820fc583860d5d6971cac39edc4ec8d04626180420570126f5343ac7be258db489affff",
      },
      committedFieldShape: {
        categoryId: "00000012",
        scriptHash: "1ff0e9ecb714aa264dc65d1456ac29906ee881c4f918f24dc4620f3c",
        membershipProofCbor:
          "9fd8799f005f584039ad34d2df4ad8a8a86493b9258b75d5b4ab4f622db261197307add5bf0da26148b81996ce4f4f5539c1c724c6afe62d79849efae1af1eed24ad7f0a378e5e1e58403c115359267a107fba54d93b91a85604304e2e0a5e94a5facd7471c114b732ea0000000000000000000000000000000000000000000000000000000000000000ffffd8799f005f5840fa20f7904d69edf460fb8ee4f7b4acf0b22b93b3890b0124c369ff3098e6f09385c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b158400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      minFee: {
        categoryId: "00000013",
        scriptHash: "f199c3f32f378d1711a0104a7ed16f1e50cef6738fcdd0697b3b26c3",
        membershipProofCbor:
          "9fd8799f005f5840322e9b797c837152e7ee509f78f0623d22bb23d6836c947ce6a8e2709a8033b7a910d858f5aafa98346adab4bf0a1ec4d8f8686ba006a706b1abcd4a60a1a1fd58406ef9183cedb148637f24c6a5a4b5c6963adde2deee0034e8ffd019b82d7232160000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820129e0add80680412132355f87a5229a5f4aa9415a232277c56fbd3d54077bfe15820932e5fb134f5dadd27ca470fba3cbd3dda580cd5ba57c02657531f4dc02d04aaffff",
      },
      withdrawalMistag: {
        categoryId: "00000014",
        scriptHash: "99645534e751635640dd74e3184cf45e1a6c324e2779324e9fe3bd07",
        membershipProofCbor:
          "9fd8799f005f5840322e9b797c837152e7ee509f78f0623d22bb23d6836c947ce6a8e2709a8033b72ecf74033192835350169cf3eff86f80207c52ff850a12745582159c5c8d803e584004d1936827765bf22959d72b5b30be0429da2839962d2bcc1ad632561c2dac6a0000000000000000000000000000000000000000000000000000000000000000ffffd87b9f0058207e023fd5b23a747196d48a15c4318e292acf4fd1a25e848552f9af7c56d5d024582090133d4902767d4c429ff40b87bc14cd52114e5eeacbf19764da0ab1d712eb5bffff",
      },
      doubleWithdraw: {
        categoryId: "00000015",
        scriptHash: "d0acd1e15c06f4253751a50b85c8ea82a90316a1dcbda8bf9013efe1",
        membershipProofCbor:
          "9fd8799f005f5840322e9b797c837152e7ee509f78f0623d22bb23d6836c947ce6a8e2709a8033b72ecf74033192835350169cf3eff86f80207c52ff850a12745582159c5c8d803e584099bd446e70ef0f9d5de12e78aa4f83a3b74c26af7a31fc7585c18fbdb8fc2404b745b52e163f3d418e8b56684925b060fc90bb4c2f8b5fd58a984b124caf98afffffd87b9f0058205fce224aa235294c791e822adfc03c7944cf388696e988e1d9cffd72780a841658206d917a4659837fee0d7f05e0c7d149a71f66b00cc61fa518960f438e7a80be84ffff",
      },
      crossBlockDuplicateEvent: {
        categoryId: "00000016",
        scriptHash: "446cbd4f5f95a7c551b22345d4ef6181e478042c78e009ee214a92ce",
        membershipProofCbor:
          "9fd8799f005f5840322e9b797c837152e7ee509f78f0623d22bb23d6836c947ce6a8e2709a8033b7a910d858f5aafa98346adab4bf0a1ec4d8f8686ba006a706b1abcd4a60a1a1fd58406ef9183cedb148637f24c6a5a4b5c6963adde2deee0034e8ffd019b82d7232160000000000000000000000000000000000000000000000000000000000000000ffffd87b9f00582018381c1e06288c341974ff129a8c50fec85b71b3c73b87e9a26b0d6d8da7b18a5820361d4ce05c4c8764a4bce50f9151c2a470023a69b7969b61c3331eefed564f10ffff",
      },
      l2TxMistag: {
        categoryId: "00000017",
        scriptHash: "58bb6f8170bdb893f4c458270cf6790d6f2022bcc18c47cad1076038",
        membershipProofCbor:
          "9fd8799f005f5840322e9b797c837152e7ee509f78f0623d22bb23d6836c947ce6a8e2709a8033b7a910d858f5aafa98346adab4bf0a1ec4d8f8686ba006a706b1abcd4a60a1a1fd58406a95a74dcdd295f9edf72b0e8b61a93127cd6def80ccd975e28e804f70d5a714ff42e9ae5c5dc468dcce528e4194d1dfe3eae9b77a1dc70e189a7dc894b7945bffffd87b9f0058203a219ce19d5583088930b21e35dcc030242e8a7873e5650164adafaef7ea17ea5820903ee49ab71e63a6f0632d858c97e783bb144f53a73dd754369d8b725826aeabffff",
      },
      withdrawnInput: {
        categoryId: "00000018",
        scriptHash: "8eee9dc8b4e8edb75c1923801202b5685de486efd444204efed11b38",
        membershipProofCbor:
          "9fd8799f005f5840322e9b797c837152e7ee509f78f0623d22bb23d6836c947ce6a8e2709a8033b72ecf74033192835350169cf3eff86f80207c52ff850a12745582159c5c8d803e584099bd446e70ef0f9d5de12e78aa4f83a3b74c26af7a31fc7585c18fbdb8fc24048eeaf204b5f64f6e39830ee898be3e97e976be8b9273023754ec2560f8d6699cffffd87b9f00582043b91e923070f319b8f50b95e78954278f5896c42868698eaf2d10c226d28a5a5820b2b437710c1902cd213fb2dbfa15b8d8c9a23832a0fe07c17085ea14c4e850fcffff",
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
