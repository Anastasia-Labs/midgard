import { describe, expect, it } from "vitest";

import {
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
  MIDGARD_DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION,
} from "../src/consensus-profile-v1.js";
import {
  assertDeploymentMarkerV1Matches,
  computeDeploymentManifestV1Id,
  computeDeploymentManifestV1JsonDigest,
  DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES,
  type DeploymentManifestV1FraudProofCatalogueIdentity,
  makeDeploymentMarkerV1,
  MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
  normalizeDeploymentManifestV1JsonValue,
  parseDeploymentMarkerV1,
  verifyDeploymentManifestV1FraudProofCatalogueIdentity,
  verifyDeploymentManifestV1Identity,
} from "../src/deployment-manifest-identity-v1.js";

const catalogueFixture =
  (): DeploymentManifestV1FraudProofCatalogueIdentity => ({
    root: "5f33f1cf4ecf9d003bc5e5a1f4100de4416e8e007f41b4684d70ae9a50142f4e",
    categories: {
      doubleSpend: {
        categoryId: "00000000",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840b8b62b6d81ee12cc0d0d07c27f8a97ed22585d6c48655aea606e0f7d7e14c4abc29b7adb9f06f12c15d04e8f79b920e55b06ad0e91b382005d97183ddfc47adf5840d48dfb2bc5c238b3e52ee0ad4684195ed900877af057737cb582351ce73f3a8a12b83fa14aacbe69b71f5a017fd56b855f11a781deb8475aa6e1b82fccd25058ffffd8799f005f5840621466f9f4db73ef635eb18ad7394465fb3d3c7ae2f37bf5fcf6e99a337455c485c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b158400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      nonExistentInput: {
        categoryId: "00000001",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58404b86ea58d81b7b7745d27b48846e4b5c9ec6ccf0abc91c264eb031b64a0d43d931906a7e7caf0941e3313c2be48d635a9c786a83bcebaa806d4d5dd9145a0c5058405d89f3d289a539ea30e07610f030b7545840e912757f618fdbafb3341d8e949a0000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ee008edbcebc51812e09a76640db05559631cf9730f2fddd093fc72d072d323b5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      nonExistentInputNoIndex: {
        categoryId: "00000002",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58404b86ea58d81b7b7745d27b48846e4b5c9ec6ccf0abc91c264eb031b64a0d43d956c0ea3769be933527dff21ccb49ac4d62547d5fd4396cf92635ba4295d33a725840bd4f2c683ff398c8ac23ffbcd8a2c30171ff5704690af2d201031de5907a9de03f69f77baf91bb11de6b8d53cd12bca25dbe306ad63fd743821a00417c01c6c1ffffd8799f005f5840b22df1a126b5ba4e33c16fd6157507610e55ffce20dae7ac44cae168a463612ad7632ab5fe3136f94fb50c0fbf9e8e7528bc392d554c8555bd31a47e5672ef5b58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f976197067033f977fabfb6894eb9e603c830e9683f092b99442e93e804c1d72d52dbffffff",
      },
      invalidRange: {
        categoryId: "00000003",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58404b86ea58d81b7b7745d27b48846e4b5c9ec6ccf0abc91c264eb031b64a0d43d931906a7e7caf0941e3313c2be48d635a9c786a83bcebaa806d4d5dd9145a0c5058405d89f3d289a539ea30e07610f030b7545840e912757f618fdbafb3341d8e949a0000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ef9660efe206d50189739680bd866387c07a061a7b5de18f85b6af241f1686385820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      transitionTrace: {
        categoryId: "00000004",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58404b86ea58d81b7b7745d27b48846e4b5c9ec6ccf0abc91c264eb031b64a0d43d956c0ea3769be933527dff21ccb49ac4d62547d5fd4396cf92635ba4295d33a725840bd4f2c683ff398c8ac23ffbcd8a2c30171ff5704690af2d201031de5907a9de03f69f77baf91bb11de6b8d53cd12bca25dbe306ad63fd743821a00417c01c6c1ffffd8799f005f5840b22df1a126b5ba4e33c16fd6157507610e55ffce20dae7ac44cae168a463612a260ef720add4722f2a5cc54e0acaf1cea960ab43783115c6fb7180111b1d719b58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      zeroInput: {
        categoryId: "00000005",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840b8b62b6d81ee12cc0d0d07c27f8a97ed22585d6c48655aea606e0f7d7e14c4ab1cbb98fc0b5daafb6a1fd1170ff38b25b627258f3a273deafd34f2e0660cc2ff584069c456f64fe1e805c345177b642a4ddc92d65b7a19a64d7bb4f83dd90d620b0d10cc762108314aeeb064009daf71465b9df57c5c885542140bd7f3a4e2197b95ffffd87b9f0058204d539335cc05bbb119d570610044977c7c13a943bb5c13611d7162e1b2269fa35820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      validationTraceDispute: {
        categoryId: "00000006",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58404b86ea58d81b7b7745d27b48846e4b5c9ec6ccf0abc91c264eb031b64a0d43d956c0ea3769be933527dff21ccb49ac4d62547d5fd4396cf92635ba4295d33a72584032b47cdeda8ef2d0fd3c0dc65613d5362b17c661c3ce6d1633e464a4fb224dad0000000000000000000000000000000000000000000000000000000000000000ffffd8799f005f5840f12388751ffd0c903190c80827eabfe9576d0ccbb0e677137ad72aa12545d35c85c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b15840938dd0f2117579675b68f4a18ff7e6058853b87f01e1e7dab9233859dff7b31e0000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      daHashPreimage: {
        categoryId: "00000007",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840b8b62b6d81ee12cc0d0d07c27f8a97ed22585d6c48655aea606e0f7d7e14c4abc29b7adb9f06f12c15d04e8f79b920e55b06ad0e91b382005d97183ddfc47adf5840d48dfb2bc5c238b3e52ee0ad4684195ed900877af057737cb582351ce73f3a8a12b83fa14aacbe69b71f5a017fd56b855f11a781deb8475aa6e1b82fccd25058ffffd8799f005f5840a538de3de20cfe37ddacd29429460d40a489465c0f2fc39c06c8a3a8632ed0dc85c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b158400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f9761bbc5942f66d5b8cce5e82c6dbdc27086a04620af66635cda2025ec06104825a2ffffff",
      },
      noReferenceInput: {
        categoryId: "00000008",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58404b86ea58d81b7b7745d27b48846e4b5c9ec6ccf0abc91c264eb031b64a0d43d956c0ea3769be933527dff21ccb49ac4d62547d5fd4396cf92635ba4295d33a72584032b47cdeda8ef2d0fd3c0dc65613d5362b17c661c3ce6d1633e464a4fb224dad0000000000000000000000000000000000000000000000000000000000000000ffffd8799f005f5840f12388751ffd0c903190c80827eabfe9576d0ccbb0e677137ad72aa12545d35c85c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b15840fa90ee34d5e3822d9a42d28583f2bc4daaf8012997fde61f6c9f506544dac5300000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      referenceInputNoIdx: {
        categoryId: "00000009",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840b8b62b6d81ee12cc0d0d07c27f8a97ed22585d6c48655aea606e0f7d7e14c4ab1cbb98fc0b5daafb6a1fd1170ff38b25b627258f3a273deafd34f2e0660cc2ff584069c456f64fe1e805c345177b642a4ddc92d65b7a19a64d7bb4f83dd90d620b0d78d20f03ff6f69f9eb6e71ebde520e3d842282190d0157fb3fe92a94c49dcd95ffffd87b9f00582052149618b234c08150fd26ae73bdef9bd7843240f0a25a50d9798ca6ebe52fa95820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      invalidSignature: {
        categoryId: "0000000a",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58404b86ea58d81b7b7745d27b48846e4b5c9ec6ccf0abc91c264eb031b64a0d43d931906a7e7caf0941e3313c2be48d635a9c786a83bcebaa806d4d5dd9145a0c505840cb4cbd809e8abb415df606bb52334d9bd8d5eb6ed33c8256a6b5f186fd80bf35ce3c6646c60e38be1b5c830306d68fa6ab2466640147876e00e1dd1f95a34826ffffd87b9f005820d058b2a00a48c1053abeb11bf3b5669ec7ad5d17824b518fe8c5bb99fe9024cc5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      fabricatedDeposit: {
        categoryId: "0000000b",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840b8b62b6d81ee12cc0d0d07c27f8a97ed22585d6c48655aea606e0f7d7e14c4abc29b7adb9f06f12c15d04e8f79b920e55b06ad0e91b382005d97183ddfc47adf5840d48dfb2bc5c238b3e52ee0ad4684195ed900877af057737cb582351ce73f3a8a12b83fa14aacbe69b71f5a017fd56b855f11a781deb8475aa6e1b82fccd25058ffffd8799f005f5840a538de3de20cfe37ddacd29429460d40a489465c0f2fc39c06c8a3a8632ed0dc85c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b158400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f9761d22b5a5eed4bd2f239fbe7b0428573d8279544fa7edb28e001c0e0dba16bd8b8ffffff",
      },
      fabricatedWithdrawal: {
        categoryId: "0000000c",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58404b86ea58d81b7b7745d27b48846e4b5c9ec6ccf0abc91c264eb031b64a0d43d931906a7e7caf0941e3313c2be48d635a9c786a83bcebaa806d4d5dd9145a0c505840cb4cbd809e8abb415df606bb52334d9bd8d5eb6ed33c8256a6b5f186fd80bf35ce3c6646c60e38be1b5c830306d68fa6ab2466640147876e00e1dd1f95a34826ffffd87b9f005820d3d63f5628388eee4a0dde3296830a843cc7ebda255b0abfb4e058d1e058e7f45820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      nativeScriptDecoding: {
        categoryId: "0000000d",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58404b86ea58d81b7b7745d27b48846e4b5c9ec6ccf0abc91c264eb031b64a0d43d956c0ea3769be933527dff21ccb49ac4d62547d5fd4396cf92635ba4295d33a725840bd4f2c683ff398c8ac23ffbcd8a2c30171ff5704690af2d201031de5907a9de03f69f77baf91bb11de6b8d53cd12bca25dbe306ad63fd743821a00417c01c6c1ffffd8799f005f5840b22df1a126b5ba4e33c16fd6157507610e55ffce20dae7ac44cae168a463612ad7632ab5fe3136f94fb50c0fbf9e8e7528bc392d554c8555bd31a47e5672ef5b58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f976153a07bbb70969553627c426b3ab026bba116a4531b6a8fbcccb272a7cb617d18ffffff",
      },
      missingSignature: {
        categoryId: "0000000e",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58404b86ea58d81b7b7745d27b48846e4b5c9ec6ccf0abc91c264eb031b64a0d43d931906a7e7caf0941e3313c2be48d635a9c786a83bcebaa806d4d5dd9145a0c505840cb4cbd809e8abb415df606bb52334d9bd8d5eb6ed33c8256a6b5f186fd80bf35fad3041d40a1254b79f55858cc6060c4844f95b49d496c0a4f1e2332f5242449ffffff",
      },
      missingNativeScriptTx: {
        categoryId: "0000000f",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840b8b62b6d81ee12cc0d0d07c27f8a97ed22585d6c48655aea606e0f7d7e14c4ab1cbb98fc0b5daafb6a1fd1170ff38b25b627258f3a273deafd34f2e0660cc2ff58406e6b6b94ce4b4c31e8aaf697daa3d8cd2f39f920af6f1dbf3dba7860a7f9f1180000000000000000000000000000000000000000000000000000000000000000ffffd87b9f00582074d95aa7096d5f8293cda4d8589964ea8d8ec37a0479478544820c4a52c912cd5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      withdrawnReferenceInput: {
        categoryId: "00000010",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58404b86ea58d81b7b7745d27b48846e4b5c9ec6ccf0abc91c264eb031b64a0d43d956c0ea3769be933527dff21ccb49ac4d62547d5fd4396cf92635ba4295d33a725840bd4f2c683ff398c8ac23ffbcd8a2c30171ff5704690af2d201031de5907a9de02159cd402817e6b55cbd2f4bb577ce5d86ab531432e0821675da48297494f136ffffff",
      },
      canonicalDecodability: {
        categoryId: "00000011",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840b8b62b6d81ee12cc0d0d07c27f8a97ed22585d6c48655aea606e0f7d7e14c4abc29b7adb9f06f12c15d04e8f79b920e55b06ad0e91b382005d97183ddfc47adf5840d48dfb2bc5c238b3e52ee0ad4684195ed900877af057737cb582351ce73f3a8a32725558858d477e011b9b171993ce24bd1d6cea5bb651e5e0dc9a96fdfd4a1dffffd87b9f005820342462f44a6302b0049152bc292ac60f4d7617a5fd7f226943e58ef7223fccd65820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      committedFieldShape: {
        categoryId: "00000012",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58404b86ea58d81b7b7745d27b48846e4b5c9ec6ccf0abc91c264eb031b64a0d43d956c0ea3769be933527dff21ccb49ac4d62547d5fd4396cf92635ba4295d33a72584032b47cdeda8ef2d0fd3c0dc65613d5362b17c661c3ce6d1633e464a4fb224dad0000000000000000000000000000000000000000000000000000000000000000ffffd8799f005f5840be811f19d4275c24ddb7d72113319b495a24e7c6872c7c98603ee2e58766985985c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b158400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      minFee: {
        categoryId: "00000013",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840b8b62b6d81ee12cc0d0d07c27f8a97ed22585d6c48655aea606e0f7d7e14c4abc29b7adb9f06f12c15d04e8f79b920e55b06ad0e91b382005d97183ddfc47adf58400b0ccad64d9a426382469e4709b8c038b5a2f936e77ca10de69fcf348c6412cd0000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820129e0add80680412132355f87a5229a5f4aa9415a232277c56fbd3d54077bfe15820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      withdrawalMistag: {
        categoryId: "00000014",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840b8b62b6d81ee12cc0d0d07c27f8a97ed22585d6c48655aea606e0f7d7e14c4ab1cbb98fc0b5daafb6a1fd1170ff38b25b627258f3a273deafd34f2e0660cc2ff58406e6b6b94ce4b4c31e8aaf697daa3d8cd2f39f920af6f1dbf3dba7860a7f9f1180000000000000000000000000000000000000000000000000000000000000000ffffd87b9f0058207e023fd5b23a747196d48a15c4318e292acf4fd1a25e848552f9af7c56d5d0245820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      doubleWithdraw: {
        categoryId: "00000015",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840b8b62b6d81ee12cc0d0d07c27f8a97ed22585d6c48655aea606e0f7d7e14c4ab1cbb98fc0b5daafb6a1fd1170ff38b25b627258f3a273deafd34f2e0660cc2ff584069c456f64fe1e805c345177b642a4ddc92d65b7a19a64d7bb4f83dd90d620b0d78d20f03ff6f69f9eb6e71ebde520e3d842282190d0157fb3fe92a94c49dcd95ffffd87b9f0058205fce224aa235294c791e822adfc03c7944cf388696e988e1d9cffd72780a84165820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      crossBlockDuplicateEvent: {
        categoryId: "00000016",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840b8b62b6d81ee12cc0d0d07c27f8a97ed22585d6c48655aea606e0f7d7e14c4abc29b7adb9f06f12c15d04e8f79b920e55b06ad0e91b382005d97183ddfc47adf58400b0ccad64d9a426382469e4709b8c038b5a2f936e77ca10de69fcf348c6412cd0000000000000000000000000000000000000000000000000000000000000000ffffd87b9f00582018381c1e06288c341974ff129a8c50fec85b71b3c73b87e9a26b0d6d8da7b18a5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      l2TxMistag: {
        categoryId: "00000017",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840b8b62b6d81ee12cc0d0d07c27f8a97ed22585d6c48655aea606e0f7d7e14c4abc29b7adb9f06f12c15d04e8f79b920e55b06ad0e91b382005d97183ddfc47adf5840d48dfb2bc5c238b3e52ee0ad4684195ed900877af057737cb582351ce73f3a8a32725558858d477e011b9b171993ce24bd1d6cea5bb651e5e0dc9a96fdfd4a1dffffd87b9f0058203a219ce19d5583088930b21e35dcc030242e8a7873e5650164adafaef7ea17ea5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      withdrawnInput: {
        categoryId: "00000018",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840b8b62b6d81ee12cc0d0d07c27f8a97ed22585d6c48655aea606e0f7d7e14c4ab1cbb98fc0b5daafb6a1fd1170ff38b25b627258f3a273deafd34f2e0660cc2ff584069c456f64fe1e805c345177b642a4ddc92d65b7a19a64d7bb4f83dd90d620b0d10cc762108314aeeb064009daf71465b9df57c5c885542140bd7f3a4e2197b95ffffd87b9f00582043b91e923070f319b8f50b95e78954278f5896c42868698eaf2d10c226d28a5a5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
    },
  });

const identityInput = () => ({
  schemaVersion: MIDGARD_DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION,
  consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
  consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
  network: "Preprod",
  cardanoProtocolParameters: {},
  genesis: {},
  createdAt: "2026-07-24T00:00:00.000Z",
  updatedAt: "2026-07-24T00:00:00.000Z",
  referenceScriptDeployAddress: "addr_test1reference",
  hubOracleOneShot: {},
  referenceScriptAuthPolicy: {},
  contracts: {},
  referenceScripts: {},
  da: {},
  proofEvidence: {},
  steps: {},
  validationDispute: {},
});

describe("DeploymentManifestV1 shared identity", () => {
  it("includes every registered fraud-proof validator in the canonical registry", () => {
    expect(DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES).toContain(
      "fraudProofZeroInput",
    );
    // #547 appended the Q18/Q31/Q15 first-step validators. The registry is
    // append-only, so each must be present and the catalogue order must name
    // exactly the same set of categories in the same positions.
    expect(DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES).toContain(
      "fraudProofNoReferenceInput",
    );
    expect(DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES).toContain(
      "fraudProofReferenceInputNoIdx",
    );
    expect(DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES).toContain(
      "fraudProofInvalidSignature",
    );
    expect(
      DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.slice(-14),
    ).toEqual([
      "fabricatedDeposit",
      "fabricatedWithdrawal",
      "nativeScriptDecoding",
      "missingSignature",
      "missingNativeScriptTx",
      "withdrawnReferenceInput",
      "canonicalDecodability",
      "committedFieldShape",
      "minFee",
      "withdrawalMistag",
      "doubleWithdraw",
      "crossBlockDuplicateEvent",
      "l2TxMistag",
      "withdrawnInput",
    ]);
    expect(DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES).toHaveLength(107);
    expect(
      Object.keys(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
    ).toHaveLength(90);
    expect(
      Object.keys(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES),
    ).toHaveLength(91);
    expect(
      DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
        "V1 fraud-proof withdrawn-input step-03"
      ],
    ).toBe("fraudProofWithdrawnInputStep03");
    expect(
      DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
        "V1 fraud-proof transition-trace final-7"
      ],
    ).toBe("fraudProofTransitionTraceDuplicate");

    const appendedLinearFamilies = [
      ["FabricatedDeposit", "fabricated-deposit", 4],
      ["FabricatedWithdrawal", "fabricated-withdrawal", 4],
      ["NativeScriptDecoding", "native-script-decoding", 4],
      ["MissingSignature", "missing-signature", 4],
      ["MissingNativeScriptTx", "missing-native-script-tx", 6],
      ["WithdrawnReferenceInput", "withdrawn-reference-input", 3],
      ["CanonicalDecodability", "canonical-decodability", 2],
      ["CommittedFieldShape", "committed-field-shape", 2],
      ["MinFee", "min-fee", 2],
      ["WithdrawalMistag", "withdrawal-mistag", 5],
      ["DoubleWithdraw", "double-withdraw", 2],
      ["CrossBlockDuplicateEvent", "cross-block-duplicate-event", 2],
      ["L2TxMistag", "l2-tx-mistag", 2],
      ["WithdrawnInput", "withdrawn-input", 3],
    ] as const;
    for (const [contractStem, roleStem, stepCount] of appendedLinearFamilies) {
      for (let step = 1; step <= stepCount; step += 1) {
        const stepSuffix =
          step === 1 ? "" : `Step${step.toString().padStart(2, "0")}`;
        const contractName = `fraudProof${contractStem}${stepSuffix}`;
        const role = `V1 fraud-proof ${roleStem} step-${step.toString().padStart(2, "0")}`;
        expect(DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES).toContain(contractName);
        expect(
          DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
            role as keyof typeof DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
          ],
        ).toBe(contractName);
      }
    }

    const transitionFinalContracts = [
      "fraudProofTransitionTraceControl",
      "fraudProofTransitionTraceSource",
      "fraudProofTransitionTraceWithdrawal",
      "fraudProofTransitionTraceForced",
      "fraudProofTransitionTraceAcceptedTransaction",
      "fraudProofTransitionTraceDeposit",
      "fraudProofTransitionTraceL1Event",
      "fraudProofTransitionTraceDuplicate",
    ] as const;
    transitionFinalContracts.forEach((contractName, index) => {
      expect(DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES).toContain(contractName);
      expect(
        DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
          `V1 fraud-proof transition-trace final-${index.toString()}` as keyof typeof DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
        ],
      ).toBe(contractName);
    });
  });

  it("authenticates the exact 25-entry fraud-proof catalogue root and proofs", () => {
    const catalogue = catalogueFixture();
    expect(
      verifyDeploymentManifestV1FraudProofCatalogueIdentity(catalogue),
    ).toEqual(catalogue);
  });

  it("rejects catalogue root, position, value, proof, and category-set tampering", () => {
    const catalogue = catalogueFixture();

    expect(() =>
      verifyDeploymentManifestV1FraudProofCatalogueIdentity({
        ...catalogue,
        root: "ff".repeat(32),
      }),
    ).toThrow(/catalogue root mismatch/u);

    expect(() =>
      verifyDeploymentManifestV1FraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          nonExistentInputNoIndex: {
            ...catalogue.categories.nonExistentInputNoIndex,
            categoryId: "00000003",
          },
        },
      }),
    ).toThrow(/nonExistentInputNoIndex\.categoryId must be 00000002/u);

    expect(() =>
      verifyDeploymentManifestV1FraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          zeroInput: {
            ...catalogue.categories.zeroInput,
            scriptHash: "aa".repeat(28),
          },
        },
      }),
    ).toThrow(/catalogue root mismatch/u);

    expect(() =>
      verifyDeploymentManifestV1FraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          invalidRange: {
            ...catalogue.categories.invalidRange,
            membershipProofCbor:
              catalogue.categories.doubleSpend.membershipProofCbor,
          },
        },
      }),
    ).toThrow(/invalidRange\.membershipProofCbor does not prove membership/u);

    const { validationTraceDispute: _missing, ...missingCategory } =
      catalogue.categories;
    expect(() =>
      verifyDeploymentManifestV1FraudProofCatalogueIdentity({
        ...catalogue,
        categories:
          missingCategory as DeploymentManifestV1FraudProofCatalogueIdentity["categories"],
      }),
    ).toThrow(/validationTraceDispute is required/u);

    expect(() =>
      verifyDeploymentManifestV1FraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          historicalCategory: catalogue.categories.doubleSpend,
        } as DeploymentManifestV1FraudProofCatalogueIdentity["categories"],
      }),
    ).toThrow(/historicalCategory is unexpected/u);
  });

  it("rejects malformed categories at the exported catalogue boundary", () => {
    const catalogue = catalogueFixture();
    const { membershipProofCbor: _proof, ...missingProof } =
      catalogue.categories.doubleSpend;
    expect(() =>
      verifyDeploymentManifestV1FraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          doubleSpend:
            missingProof as DeploymentManifestV1FraudProofCatalogueIdentity["categories"]["doubleSpend"],
        },
      }),
    ).toThrow(/doubleSpend\.membershipProofCbor is required/u);

    expect(() =>
      verifyDeploymentManifestV1FraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          doubleSpend: {
            ...catalogue.categories.doubleSpend,
            scriptHash: "AA".repeat(28),
          },
        },
      }),
    ).toThrow(/doubleSpend\.scriptHash must be lowercase canonical hex/u);

    expect(() =>
      verifyDeploymentManifestV1FraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          doubleSpend: {
            ...catalogue.categories.doubleSpend,
            membershipProofCbor: "f",
          },
        },
      }),
    ).toThrow(
      /doubleSpend\.membershipProofCbor must be lowercase canonical hex/u,
    );
  });

  it("owns canonical JSON normalization and digest vectors", () => {
    const normalized = normalizeDeploymentManifestV1JsonValue({
      z: [1, 2n],
      a: { y: true, x: null },
    });
    expect(normalized).toEqual({
      z: [1, "2"],
      a: { y: true, x: null },
    });
    expect(computeDeploymentManifestV1JsonDigest(normalized)).toBe(
      "ccff47a9e0ebd42629b30db95fa7988b032093e903958b916820987a100d7cb4",
    );
    expect(
      computeDeploymentManifestV1JsonDigest({
        a: { x: null, y: true },
        z: [1, "2"],
      }),
    ).toBe("ccff47a9e0ebd42629b30db95fa7988b032093e903958b916820987a100d7cb4");
    expect(
      computeDeploymentManifestV1JsonDigest({
        a: { x: null, y: false },
        z: [1, "2"],
      }),
    ).not.toBe(
      "ccff47a9e0ebd42629b30db95fa7988b032093e903958b916820987a100d7cb4",
    );
  });

  it("rejects values outside the canonical JSON boundary", () => {
    expect(() =>
      normalizeDeploymentManifestV1JsonValue({ missing: undefined }),
    ).toThrow(/value\.missing must not be undefined/u);
    expect(() =>
      normalizeDeploymentManifestV1JsonValue({ invalid: Number.NaN }),
    ).toThrow(/must contain only finite numbers/u);
    expect(() => computeDeploymentManifestV1JsonDigest({ raw: 2n })).toThrow(
      /must contain only JSON-safe values/u,
    );
  });

  it("recomputes the exact full-manifest identity", () => {
    const identity = identityInput();
    const manifest = {
      ...identity,
      manifestId: computeDeploymentManifestV1Id(identity),
    };
    // Rebound 2026-08-23: the identity input embeds
    // MIDGARD_CONSENSUS_PROFILE_V1, whose committed constants changed in
    // 2c7fd3bb (E_MIN_ADA at the ValueAndMint descriptor step, #618/#627);
    // the old pin predated that commit. Previously rebound 2026-08-01 for
    // 4a4bc660 on the same basis.
    expect(manifest.manifestId).toBe(
      "a8d1a02bdd143f17d44569232837c539fe53b9bd0c39cc332b0091fcd20d7218",
    );
    expect(verifyDeploymentManifestV1Identity(manifest)).toEqual(manifest);
  });

  it("owns the sole exact DeploymentMarkerV1 boundary", () => {
    const manifestId = computeDeploymentManifestV1Id(identityInput());
    const marker = makeDeploymentMarkerV1(manifestId);
    expect(marker).toEqual({
      schemaVersion: MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
      manifestId,
    });
    expect(parseDeploymentMarkerV1(marker)).toEqual(marker);
    expect(assertDeploymentMarkerV1Matches(marker, marker, "Postgres")).toEqual(
      marker,
    );
    expect(() =>
      parseDeploymentMarkerV1({ ...marker, historicalVersion: 9 }),
    ).toThrow(/exactly schemaVersion and manifestId/u);
    expect(() =>
      parseDeploymentMarkerV1({ manifestId: marker.manifestId }),
    ).toThrow(/exactly schemaVersion and manifestId/u);
    expect(() =>
      assertDeploymentMarkerV1Matches(
        marker,
        makeDeploymentMarkerV1("ff".repeat(32)),
        "DA store",
      ),
    ).toThrow(
      `DA store deployment marker mismatch: expected ${marker.manifestId}, found ${"ff".repeat(32)}`,
    );
  });

  it("rejects tampering, missing fields, and extra fields", () => {
    const identity = identityInput();
    const manifest = {
      ...identity,
      manifestId: computeDeploymentManifestV1Id(identity),
    };
    expect(() =>
      verifyDeploymentManifestV1Identity({
        ...manifest,
        network: "Preview",
      }),
    ).toThrow(/id mismatch/u);

    const { da: _da, ...missingDa } = manifest;
    expect(() => verifyDeploymentManifestV1Identity(missingDa)).toThrow(
      /value\.da is required/u,
    );
    expect(() =>
      verifyDeploymentManifestV1Identity({
        ...manifest,
        historicalVersion: 9,
      }),
    ).toThrow(/value\.historicalVersion is unexpected/u);
  });
});
