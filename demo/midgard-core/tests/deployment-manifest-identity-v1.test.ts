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
  DEPLOYMENT_MANIFEST_V1_ECONOMICS_BY_PROFILE,
  DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  DEPLOYMENT_MANIFEST_V1_L1_FINALITY,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES,
  type DeploymentManifestV1FraudProofCatalogueIdentity,
  makeDeploymentMarkerV1,
  MIDGARD_DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES_V1,
  MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
  normalizeDeploymentManifestV1JsonValue,
  parseDeploymentManifestV1AvailabilityChallenge,
  parseDeploymentManifestV1Economics,
  parseDeploymentMarkerV1,
  verifyDeploymentManifestV1FraudProofCatalogueIdentity,
  verifyDeploymentManifestV1Identity,
} from "../src/deployment-manifest-identity-v1.js";

const catalogueFixture =
  (): DeploymentManifestV1FraudProofCatalogueIdentity => ({
    root: "b8c6dfc9e40500d5188cc4fe3d395509c2df64796bdbfa0da790058cf3106b56",
    categories: {
      doubleSpend: {
        categoryId: "00000000",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840bfdd6ff35766ef062b99f4cd44a34307ad4ca7997d8d754cf00e3e4182c6071dc29b7adb9f06f12c15d04e8f79b920e55b06ad0e91b382005d97183ddfc47adf584027d8279ad324364b8f20bac1bfa97e0f4695d3259301724e6c3d34a8b1eedec612b83fa14aacbe69b71f5a017fd56b855f11a781deb8475aa6e1b82fccd25058ffffd8799f005f5840886f3c7b971346a082dbb9a3d5044db43d40550ff83dc7aa2eeb95a5f640dcbf165ee5f2aacded6fb614c55df2e832c39e476f7c954d237d759d77cf5579f5e058400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      nonExistentInput: {
        categoryId: "00000001",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58409b3e184a432dbbc2edfaceea4b4b5d3063eef22e797e10c239da8972d2630d220d3e67d2d393d5e7613760b3174906df4eb27bdf793a1bf0e024d05ac9e6168b58400310ec16d580927c8ab13b4775bb7b5e17952a52ad7278480cdca78c034302aa0000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ee008edbcebc51812e09a76640db05559631cf9730f2fddd093fc72d072d323b5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      nonExistentInputNoIndex: {
        categoryId: "00000002",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58409b3e184a432dbbc2edfaceea4b4b5d3063eef22e797e10c239da8972d2630d2210d40b2227b432b911c064dda8d186644289e5cb1092a848d126d7797a716c10584067dce71d69f0bf2ac8d2cb3f8a1a27b6b509ab91afa07b37375e67e1e9f6b51553ddc92fe9c348973734430734811e15ffe695284ead11620df3c76127ef2c0effffd8799f005f5840b22df1a126b5ba4e33c16fd6157507610e55ffce20dae7ac44cae168a463612ad7632ab5fe3136f94fb50c0fbf9e8e7528bc392d554c8555bd31a47e5672ef5b58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f976197067033f977fabfb6894eb9e603c830e9683f092b99442e93e804c1d72d52dbffffff",
      },
      invalidRange: {
        categoryId: "00000003",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58409b3e184a432dbbc2edfaceea4b4b5d3063eef22e797e10c239da8972d2630d220d3e67d2d393d5e7613760b3174906df4eb27bdf793a1bf0e024d05ac9e6168b58400310ec16d580927c8ab13b4775bb7b5e17952a52ad7278480cdca78c034302aa0000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ef9660efe206d50189739680bd866387c07a061a7b5de18f85b6af241f1686385820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      transitionTrace: {
        categoryId: "00000004",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58409b3e184a432dbbc2edfaceea4b4b5d3063eef22e797e10c239da8972d2630d2210d40b2227b432b911c064dda8d186644289e5cb1092a848d126d7797a716c10584067dce71d69f0bf2ac8d2cb3f8a1a27b6b509ab91afa07b37375e67e1e9f6b51553ddc92fe9c348973734430734811e15ffe695284ead11620df3c76127ef2c0effffd8799f005f5840b22df1a126b5ba4e33c16fd6157507610e55ffce20dae7ac44cae168a463612a260ef720add4722f2a5cc54e0acaf1cea960ab43783115c6fb7180111b1d719b58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      zeroInput: {
        categoryId: "00000005",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840bfdd6ff35766ef062b99f4cd44a34307ad4ca7997d8d754cf00e3e4182c6071d2a1a84cdc262f1e68f4da7d638265939b872e048acceb5e1dfc75defbaf85bc6584069c456f64fe1e805c345177b642a4ddc92d65b7a19a64d7bb4f83dd90d620b0d10cc762108314aeeb064009daf71465b9df57c5c885542140bd7f3a4e2197b95ffffd87b9f0058204d539335cc05bbb119d570610044977c7c13a943bb5c13611d7162e1b2269fa35820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      validationTraceDispute: {
        categoryId: "00000006",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58409b3e184a432dbbc2edfaceea4b4b5d3063eef22e797e10c239da8972d2630d2210d40b2227b432b911c064dda8d186644289e5cb1092a848d126d7797a716c10584069fa5613f35d64ad2435ce772d0f1d743568897c5847438644eb9c09f8a3f2bb0f3b5540aeb2097e759f6e7dd858267c79bccd97c9dedf27567fe936d05234bfffffd8799f005f5840f12388751ffd0c903190c80827eabfe9576d0ccbb0e677137ad72aa12545d35c85c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b15840938dd0f2117579675b68f4a18ff7e6058853b87f01e1e7dab9233859dff7b31e0000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      daHashPreimage: {
        categoryId: "00000007",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840bfdd6ff35766ef062b99f4cd44a34307ad4ca7997d8d754cf00e3e4182c6071dc29b7adb9f06f12c15d04e8f79b920e55b06ad0e91b382005d97183ddfc47adf584027d8279ad324364b8f20bac1bfa97e0f4695d3259301724e6c3d34a8b1eedec612b83fa14aacbe69b71f5a017fd56b855f11a781deb8475aa6e1b82fccd25058ffffd8799f005f58405daa4de0a54fcd081dca26f10d85bf3d6f3d546d224a5703c708ea28c524f2658e9674d257e404b9e164a8c12bb46e6cc8fab91b5e3b5e8be47d93c5b4e4548758400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f9761bbc5942f66d5b8cce5e82c6dbdc27086a04620af66635cda2025ec06104825a2ffffff",
      },
      noReferenceInput: {
        categoryId: "00000008",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58409b3e184a432dbbc2edfaceea4b4b5d3063eef22e797e10c239da8972d2630d2210d40b2227b432b911c064dda8d186644289e5cb1092a848d126d7797a716c10584069fa5613f35d64ad2435ce772d0f1d743568897c5847438644eb9c09f8a3f2bb0f3b5540aeb2097e759f6e7dd858267c79bccd97c9dedf27567fe936d05234bfffffd8799f005f5840f12388751ffd0c903190c80827eabfe9576d0ccbb0e677137ad72aa12545d35c85c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b15840fa90ee34d5e3822d9a42d28583f2bc4daaf8012997fde61f6c9f506544dac5300000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      referenceInputNoIdx: {
        categoryId: "00000009",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840bfdd6ff35766ef062b99f4cd44a34307ad4ca7997d8d754cf00e3e4182c6071d2a1a84cdc262f1e68f4da7d638265939b872e048acceb5e1dfc75defbaf85bc6584069c456f64fe1e805c345177b642a4ddc92d65b7a19a64d7bb4f83dd90d620b0d78d20f03ff6f69f9eb6e71ebde520e3d842282190d0157fb3fe92a94c49dcd95ffffd87b9f00582052149618b234c08150fd26ae73bdef9bd7843240f0a25a50d9798ca6ebe52fa95820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      invalidSignature: {
        categoryId: "0000000a",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58409b3e184a432dbbc2edfaceea4b4b5d3063eef22e797e10c239da8972d2630d220d3e67d2d393d5e7613760b3174906df4eb27bdf793a1bf0e024d05ac9e6168b5840cb4cbd809e8abb415df606bb52334d9bd8d5eb6ed33c8256a6b5f186fd80bf35df7b9a6d54302a544dfb91910ccee639d0a6decba948869552ed359b7b5f5609ffffd87b9f005820d058b2a00a48c1053abeb11bf3b5669ec7ad5d17824b518fe8c5bb99fe9024cc5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      fabricatedDeposit: {
        categoryId: "0000000b",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840bfdd6ff35766ef062b99f4cd44a34307ad4ca7997d8d754cf00e3e4182c6071dc29b7adb9f06f12c15d04e8f79b920e55b06ad0e91b382005d97183ddfc47adf584027d8279ad324364b8f20bac1bfa97e0f4695d3259301724e6c3d34a8b1eedec612b83fa14aacbe69b71f5a017fd56b855f11a781deb8475aa6e1b82fccd25058ffffd8799f005f58405daa4de0a54fcd081dca26f10d85bf3d6f3d546d224a5703c708ea28c524f2658e9674d257e404b9e164a8c12bb46e6cc8fab91b5e3b5e8be47d93c5b4e4548758400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f9761d22b5a5eed4bd2f239fbe7b0428573d8279544fa7edb28e001c0e0dba16bd8b8ffffff",
      },
      fabricatedWithdrawal: {
        categoryId: "0000000c",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58409b3e184a432dbbc2edfaceea4b4b5d3063eef22e797e10c239da8972d2630d220d3e67d2d393d5e7613760b3174906df4eb27bdf793a1bf0e024d05ac9e6168b5840cb4cbd809e8abb415df606bb52334d9bd8d5eb6ed33c8256a6b5f186fd80bf35df7b9a6d54302a544dfb91910ccee639d0a6decba948869552ed359b7b5f5609ffffd87b9f005820d3d63f5628388eee4a0dde3296830a843cc7ebda255b0abfb4e058d1e058e7f45820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      nativeScriptDecoding: {
        categoryId: "0000000d",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58409b3e184a432dbbc2edfaceea4b4b5d3063eef22e797e10c239da8972d2630d2210d40b2227b432b911c064dda8d186644289e5cb1092a848d126d7797a716c10584067dce71d69f0bf2ac8d2cb3f8a1a27b6b509ab91afa07b37375e67e1e9f6b51553ddc92fe9c348973734430734811e15ffe695284ead11620df3c76127ef2c0effffd8799f005f5840b22df1a126b5ba4e33c16fd6157507610e55ffce20dae7ac44cae168a463612ad7632ab5fe3136f94fb50c0fbf9e8e7528bc392d554c8555bd31a47e5672ef5b58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f976153a07bbb70969553627c426b3ab026bba116a4531b6a8fbcccb272a7cb617d18ffffff",
      },
      missingSignature: {
        categoryId: "0000000e",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58409b3e184a432dbbc2edfaceea4b4b5d3063eef22e797e10c239da8972d2630d220d3e67d2d393d5e7613760b3174906df4eb27bdf793a1bf0e024d05ac9e6168b5840cb4cbd809e8abb415df606bb52334d9bd8d5eb6ed33c8256a6b5f186fd80bf35fad3041d40a1254b79f55858cc6060c4844f95b49d496c0a4f1e2332f5242449ffffd87b9f005820cad18a972b883780111f240ceeccb0f18be7b90155c9e38de938b1e71c0086885820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      missingNativeScriptTx: {
        categoryId: "0000000f",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840bfdd6ff35766ef062b99f4cd44a34307ad4ca7997d8d754cf00e3e4182c6071d2a1a84cdc262f1e68f4da7d638265939b872e048acceb5e1dfc75defbaf85bc658406e6b6b94ce4b4c31e8aaf697daa3d8cd2f39f920af6f1dbf3dba7860a7f9f1180000000000000000000000000000000000000000000000000000000000000000ffffd87b9f00582074d95aa7096d5f8293cda4d8589964ea8d8ec37a0479478544820c4a52c912cd5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      withdrawnReferenceInput: {
        categoryId: "00000010",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58409b3e184a432dbbc2edfaceea4b4b5d3063eef22e797e10c239da8972d2630d2210d40b2227b432b911c064dda8d186644289e5cb1092a848d126d7797a716c10584067dce71d69f0bf2ac8d2cb3f8a1a27b6b509ab91afa07b37375e67e1e9f6b5152159cd402817e6b55cbd2f4bb577ce5d86ab531432e0821675da48297494f136ffffd87b9f0058209107ce69de61b7e6474dc51573f81c8d13b64aa52c2cb753286309a2b2e4a1b05820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      canonicalDecodability: {
        categoryId: "00000011",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840bfdd6ff35766ef062b99f4cd44a34307ad4ca7997d8d754cf00e3e4182c6071dc29b7adb9f06f12c15d04e8f79b920e55b06ad0e91b382005d97183ddfc47adf584027d8279ad324364b8f20bac1bfa97e0f4695d3259301724e6c3d34a8b1eedec68081e78a683306bdad42279984abf18142c55c2179676daff2f3895e646ec892ffffd87b9f005820342462f44a6302b0049152bc292ac60f4d7617a5fd7f226943e58ef7223fccd65820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      committedFieldShape: {
        categoryId: "00000012",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58409b3e184a432dbbc2edfaceea4b4b5d3063eef22e797e10c239da8972d2630d2210d40b2227b432b911c064dda8d186644289e5cb1092a848d126d7797a716c10584069fa5613f35d64ad2435ce772d0f1d743568897c5847438644eb9c09f8a3f2bb0f3b5540aeb2097e759f6e7dd858267c79bccd97c9dedf27567fe936d05234bfffffd8799f005f5840be811f19d4275c24ddb7d72113319b495a24e7c6872c7c98603ee2e58766985985c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b158400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      minFee: {
        categoryId: "00000013",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840bfdd6ff35766ef062b99f4cd44a34307ad4ca7997d8d754cf00e3e4182c6071dc29b7adb9f06f12c15d04e8f79b920e55b06ad0e91b382005d97183ddfc47adf5840a509a26de9a7274294372131f81d0ebfabb82f63fa5f095834490ed88444131a0000000000000000000000000000000000000000000000000000000000000000ffffd8799f005f58409e33909d5dd93673870a900647022655080272f206a9343a16ad39adadbcd30f85c09af929492a871e4fae32d9d5c36e352471cd659bcdb61de08f1722acc3b158400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      withdrawalMistag: {
        categoryId: "00000014",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840bfdd6ff35766ef062b99f4cd44a34307ad4ca7997d8d754cf00e3e4182c6071d2a1a84cdc262f1e68f4da7d638265939b872e048acceb5e1dfc75defbaf85bc658406e6b6b94ce4b4c31e8aaf697daa3d8cd2f39f920af6f1dbf3dba7860a7f9f1180000000000000000000000000000000000000000000000000000000000000000ffffd87b9f0058207e023fd5b23a747196d48a15c4318e292acf4fd1a25e848552f9af7c56d5d0245820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      doubleWithdraw: {
        categoryId: "00000015",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840bfdd6ff35766ef062b99f4cd44a34307ad4ca7997d8d754cf00e3e4182c6071d2a1a84cdc262f1e68f4da7d638265939b872e048acceb5e1dfc75defbaf85bc6584069c456f64fe1e805c345177b642a4ddc92d65b7a19a64d7bb4f83dd90d620b0d78d20f03ff6f69f9eb6e71ebde520e3d842282190d0157fb3fe92a94c49dcd95ffffd87b9f0058205fce224aa235294c791e822adfc03c7944cf388696e988e1d9cffd72780a84165820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      crossBlockDuplicateEvent: {
        categoryId: "00000016",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840bfdd6ff35766ef062b99f4cd44a34307ad4ca7997d8d754cf00e3e4182c6071dc29b7adb9f06f12c15d04e8f79b920e55b06ad0e91b382005d97183ddfc47adf5840a509a26de9a7274294372131f81d0ebfabb82f63fa5f095834490ed88444131a0000000000000000000000000000000000000000000000000000000000000000ffffd8799f005f5840e08eb8183ae4fb69faf88ca97c6b603d6410f919fe8f195840f38463cd9e0ce8385570c7c3e61967eb7ee28964422811e69023f839db04c6c7641171856342d758400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      l2TxMistag: {
        categoryId: "00000017",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840bfdd6ff35766ef062b99f4cd44a34307ad4ca7997d8d754cf00e3e4182c6071dc29b7adb9f06f12c15d04e8f79b920e55b06ad0e91b382005d97183ddfc47adf584027d8279ad324364b8f20bac1bfa97e0f4695d3259301724e6c3d34a8b1eedec68081e78a683306bdad42279984abf18142c55c2179676daff2f3895e646ec892ffffd87b9f0058203a219ce19d5583088930b21e35dcc030242e8a7873e5650164adafaef7ea17ea5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      withdrawnInput: {
        categoryId: "00000018",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840bfdd6ff35766ef062b99f4cd44a34307ad4ca7997d8d754cf00e3e4182c6071d2a1a84cdc262f1e68f4da7d638265939b872e048acceb5e1dfc75defbaf85bc6584069c456f64fe1e805c345177b642a4ddc92d65b7a19a64d7bb4f83dd90d620b0d10cc762108314aeeb064009daf71465b9df57c5c885542140bd7f3a4e2197b95ffffd87b9f00582043b91e923070f319b8f50b95e78954278f5896c42868698eaf2d10c226d28a5a5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      valueNotPreserved: {
        categoryId: "00000019",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58409b3e184a432dbbc2edfaceea4b4b5d3063eef22e797e10c239da8972d2630d2210d40b2227b432b911c064dda8d186644289e5cb1092a848d126d7797a716c10584069fa5613f35d64ad2435ce772d0f1d743568897c5847438644eb9c09f8a3f2bbc72fcddf74185276e9e4cd1787614140c99722eb84857ca1814b8d8c2526837affffd87b9f005820a2641c1ac0d1e6c78e8717612ef6b33e39a8c7227fd0a064b34b384919ff8e525820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      inputSetUniqueness: {
        categoryId: "0000001a",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840bfdd6ff35766ef062b99f4cd44a34307ad4ca7997d8d754cf00e3e4182c6071dc29b7adb9f06f12c15d04e8f79b920e55b06ad0e91b382005d97183ddfc47adf584027d8279ad324364b8f20bac1bfa97e0f4695d3259301724e6c3d34a8b1eedec612b83fa14aacbe69b71f5a017fd56b855f11a781deb8475aa6e1b82fccd25058ffffd8799f005f5840886f3c7b971346a082dbb9a3d5044db43d40550ff83dc7aa2eeb95a5f640dcbf7777d107cc4348873566e8de7f65bbc79a393d2876104db9ad47b509584d90f558400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      mintAuthorization: {
        categoryId: "0000001b",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58409b3e184a432dbbc2edfaceea4b4b5d3063eef22e797e10c239da8972d2630d2210d40b2227b432b911c064dda8d186644289e5cb1092a848d126d7797a716c10584069fa5613f35d64ad2435ce772d0f1d743568897c5847438644eb9c09f8a3f2bbc72fcddf74185276e9e4cd1787614140c99722eb84857ca1814b8d8c2526837affffd87b9f005820a7c2b9bf5be11a84ddc71691490874e526afef78e0cd66ce1db27cab621a7efa5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      networkId: {
        categoryId: "0000001c",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58409b3e184a432dbbc2edfaceea4b4b5d3063eef22e797e10c239da8972d2630d220d3e67d2d393d5e7613760b3174906df4eb27bdf793a1bf0e024d05ac9e6168b5840cb4cbd809e8abb415df606bb52334d9bd8d5eb6ed33c8256a6b5f186fd80bf35fad3041d40a1254b79f55858cc6060c4844f95b49d496c0a4f1e2332f5242449ffffd87b9f005820c0ee924910e653405f9f6d0e449b048ccb7ccfe11394ef1a2a124cb2960d5e5a5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      missingNativeScriptUtxo: {
        categoryId: "0000001d",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840bfdd6ff35766ef062b99f4cd44a34307ad4ca7997d8d754cf00e3e4182c6071dc29b7adb9f06f12c15d04e8f79b920e55b06ad0e91b382005d97183ddfc47adf5840a509a26de9a7274294372131f81d0ebfabb82f63fa5f095834490ed88444131a0000000000000000000000000000000000000000000000000000000000000000ffffd8799f005f5840e08eb8183ae4fb69faf88ca97c6b603d6410f919fe8f195840f38463cd9e0ce8c899b1eac81b0f914b9ca8c19375f483b37215b5674a7de548b90d990ba212b058400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      nativeScriptInvalid: {
        categoryId: "0000001e",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f5840bfdd6ff35766ef062b99f4cd44a34307ad4ca7997d8d754cf00e3e4182c6071dc29b7adb9f06f12c15d04e8f79b920e55b06ad0e91b382005d97183ddfc47adf584027d8279ad324364b8f20bac1bfa97e0f4695d3259301724e6c3d34a8b1eedec612b83fa14aacbe69b71f5a017fd56b855f11a781deb8475aa6e1b82fccd25058ffffd8799f005f58405daa4de0a54fcd081dca26f10d85bf3d6f3d546d224a5703c708ea28c524f2652066c324c687a584322111c3e463c1472fafba6b963341352b63c103fdeb752b58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      minAda: {
        categoryId: "0000001f",
        scriptHash: "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0",
        membershipProofCbor:
          "9fd8799f005f58409b3e184a432dbbc2edfaceea4b4b5d3063eef22e797e10c239da8972d2630d2210d40b2227b432b911c064dda8d186644289e5cb1092a848d126d7797a716c10584067dce71d69f0bf2ac8d2cb3f8a1a27b6b509ab91afa07b37375e67e1e9f6b5152159cd402817e6b55cbd2f4bb577ce5d86ab531432e0821675da48297494f136ffffd87b9f005820943a2ff140d3096c7f7a5ce68b9d1f70878def760f9957486bee76547117cc705820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
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
  l1Finality: DEPLOYMENT_MANIFEST_V1_L1_FINALITY,
  economics:
    DEPLOYMENT_MANIFEST_V1_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
  availabilityChallenge: {
    responseClasses: {
      smallPayloadMaxBytes: 65_536,
      smallResponseWindowMs: 3_600_000,
      fullPayloadMaxBytes: 67_108_864,
      fullResponseWindowMs: 172_800_000,
    },
    responseGeometry: {
      chunkByteLength: 14_020,
      trancheByteLength: 4_194_304,
      maxTrancheCount: 16,
    },
    daBondLovelace: 10_000_000_000,
    challengerBondLovelace: 10_000_000_000,
    maxOpenFeeLovelace: 500_000,
    maxPublicationFeeLovelace: 500_000,
    maxSettlementFeeLovelace: 500_000,
    maxCloseFeeLovelace: 1_000_000,
    maxTimeoutFeeLovelace: 1_200_000,
    bondOwnerCredential: "77".repeat(28),
  },
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
      DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.slice(-21),
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
      "valueNotPreserved",
      "inputSetUniqueness",
      "mintAuthorization",
      "networkId",
      "missingNativeScriptUtxo",
      "nativeScriptInvalid",
      "minAda",
    ]);
    expect(DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES).toHaveLength(163);
    expect(
      Object.keys(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
    ).toHaveLength(156);
    expect(
      Object.keys(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES),
    ).toHaveLength(157);
    expect(
      DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
        "claim-registry spending"
      ],
    ).toBe("claimRegistrySpend");
    expect(
      DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES[
        "claim-registry spending"
      ],
    ).toBe("ClaimRegistrySpend");
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
      ["MissingSignature", "missing-signature", 4],
      ["MissingNativeScriptTx", "missing-native-script-tx", 8],
      ["WithdrawnReferenceInput", "withdrawn-reference-input", 3],
      ["CanonicalDecodability", "canonical-decodability", 2],
      ["CommittedFieldShape", "committed-field-shape", 2],
      ["MinFee", "min-fee", 2],
      ["WithdrawalMistag", "withdrawal-mistag", 5],
      ["DoubleWithdraw", "double-withdraw", 2],
      ["CrossBlockDuplicateEvent", "cross-block-duplicate-event", 2],
      ["L2TxMistag", "l2-tx-mistag", 2],
      ["WithdrawnInput", "withdrawn-input", 3],
      ["ValueNotPreserved", "value-not-preserved", 4],
      ["InputSetUniqueness", "input-set-uniqueness", 2],
      ["MintAuthorization", "mint-authorization", 5],
      ["MissingNativeScriptUtxo", "missing-native-script-utxo", 5],
      ["NativeScriptInvalid", "native-script-invalid", 3],
      ["MinAda", "min-ada", 2],
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

    const nativeScriptDecodingContracts = [
      [
        "V1 fraud-proof native-script-decoding step-01",
        "fraudProofNativeScriptDecoding",
      ],
      [
        "V1 fraud-proof native-script-decoding step-02",
        "fraudProofNativeScriptDecodingStep02",
      ],
      [
        "V1 fraud-proof native-script-decoding step-03 open-subject",
        "fraudProofNativeScriptDecodingStep03OpenSubject",
      ],
      [
        "V1 fraud-proof native-script-decoding step-03 bind-descriptor",
        "fraudProofNativeScriptDecodingStep03BindDescriptor",
      ],
      [
        "V1 fraud-proof native-script-decoding step-03 advance-or-close",
        "fraudProofNativeScriptDecodingStep03AdvanceOrClose",
      ],
      [
        "V1 fraud-proof native-script-decoding step-04",
        "fraudProofNativeScriptDecodingStep04",
      ],
    ] as const;
    for (const [role, contractName] of nativeScriptDecodingContracts) {
      expect(DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES).toContain(contractName);
      expect(
        DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[role],
      ).toBe(contractName);
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

  it("authenticates the exact 32-entry fraud-proof catalogue root and proofs", () => {
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
    // Rebound 2026-08-30: Q58's exact response classes and release-selected
    // geometry/bond/all lifecycle fee ceilings became authenticated deployment
    // identity after the exact 14,020-byte signed-transaction measurement.
    // The same rebound adds F04's exact 5 ADA prover collateral floor as an
    // authenticated release-economics term. Rebound 2026-08-29: exact release economics became an authenticated
    // root field, so manifest identity distinguishes public launch from the
    // bounded acceptance profile without consulting `network`. The preceding
    // rebound made the 30/2160 rollback policy release-bound.
    // Previously rebound 2026-08-23: the identity input embeds
    // MIDGARD_CONSENSUS_PROFILE_V1, whose committed constants changed in
    // 2c7fd3bb (E_MIN_ADA at the ValueAndMint descriptor step, #618/#627);
    // the old pin predated that commit. Previously rebound 2026-08-01 for
    // 4a4bc660 on the same basis.
    expect(manifest.manifestId).toBe(
      "263ccec93bf8b49fc0b2e76b7cd84ab9955b928596383ce94e0b1cf31681baf3",
    );
    expect(verifyDeploymentManifestV1Identity(manifest)).toEqual(manifest);
  });

  it("accepts only exact release-bound economics profiles", () => {
    const bounded =
      DEPLOYMENT_MANIFEST_V1_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"];
    const publicPreprod =
      DEPLOYMENT_MANIFEST_V1_ECONOMICS_BY_PROFILE["public-preprod-launch-v1"];
    expect(parseDeploymentManifestV1Economics(bounded)).toEqual(bounded);
    expect(parseDeploymentManifestV1Economics(publicPreprod)).toEqual(
      publicPreprod,
    );
    expect(() =>
      parseDeploymentManifestV1Economics({
        ...bounded,
        slashingPenaltyLovelace: bounded.slashingPenaltyLovelace + 1,
      }),
    ).toThrow(/slashingPenaltyLovelace must equal/u);
    expect(() =>
      parseDeploymentManifestV1Economics({
        ...bounded,
        profile: "public-preprod-launch-v1",
      }),
    ).toThrow(/requiredBondLovelace must equal/u);
    expect(() =>
      parseDeploymentManifestV1Economics({ ...bounded, extra: true }),
    ).toThrow(/must contain exactly/u);
    const { proverCollateralFloorLovelace: _omitted, ...legacy } = bounded;
    expect(() => parseDeploymentManifestV1Economics(legacy)).toThrow(
      /must contain exactly/u,
    );
    expect(() =>
      parseDeploymentManifestV1Economics({
        ...bounded,
        proverCollateralFloorLovelace:
          bounded.proverCollateralFloorLovelace + 1,
      }),
    ).toThrow(/proverCollateralFloorLovelace must equal/u);
  });

  it("keeps activated Q58 chunk geometry separate from the 4,095-byte proof-field limit", () => {
    const availability = identityInput().availabilityChallenge;
    expect(
      parseDeploymentManifestV1AvailabilityChallenge({
        ...availability,
        responseGeometry: {
          ...availability.responseGeometry,
          chunkByteLength: 8_192,
        },
      }).responseGeometry.chunkByteLength,
    ).toBe(8_192);
    expect(() =>
      parseDeploymentManifestV1AvailabilityChallenge({
        ...availability,
        responseGeometry: {
          ...availability.responseGeometry,
          chunkByteLength:
            MIDGARD_DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES_V1 + 1,
        },
      }),
    ).toThrow(/safety\/coverage bounds/u);
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
