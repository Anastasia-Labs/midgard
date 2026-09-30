import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";

import {
  canonicalAbsolutePath,
  canonicalUtcTimestamp,
  exactKeysRecord,
  nonNegativeSafeInteger,
  positiveSafeInteger,
  sha256Digest,
} from "../../artifact-schema.js";
import { type ArchitectureGCommitCandidateInput } from "./mpf-commit-candidate-artifacts.architecture-gcommit-candidate-input.js";
import {
  decodePhase1FormalBindingIdentity,
  decodeRuntimeIdentity,
} from "./mpf-commit-candidate-artifacts.decode-phase1-formal-binding-identity.js";

export const decodeArchitectureGCommitCandidateInput = (
  value: unknown,
): ArchitectureGCommitCandidateInput => {
  const input = exactKeysRecord(
    value,
    "Architecture G commit-candidate input",
    [
      "schemaVersion",
      "phase1FormalBinding",
      "runtimeIdentity",
      "levelPath",
      "binaryPath",
      "binarySha256",
      "sidecarPath",
      "expectedTransactionCount",
      "corpusSha256",
      "corpusSliceSha256",
      "fundingMapSha256",
      "fixtureCreationPath",
      "fixtureCreationSha256",
      "fixtureInitialUtxoCount",
      "baseUtxoPayloadAggregate",
      "forcedValidationSlotConfigArtifact",
      "workerInput",
    ],
  );
  const phase1FormalBinding = decodePhase1FormalBindingIdentity(
    input.phase1FormalBinding,
  );
  decodeRuntimeIdentity(input.runtimeIdentity);
  const aggregate = exactKeysRecord(
    input.baseUtxoPayloadAggregate,
    "Architecture G candidate base UTxO aggregate",
    ["entryCount", "encodedTupleBytes"],
  );
  const slotConfigArtifact = exactKeysRecord(
    input.forcedValidationSlotConfigArtifact,
    "Architecture G candidate slot-config artifact binding",
    ["path", "sha256", "document"],
  );
  const slotConfigArtifactPath = canonicalAbsolutePath(
    slotConfigArtifact.path,
    "candidateInput.forcedValidationSlotConfigArtifact.path",
  );
  const slotConfigArtifactSha256 = sha256Digest(
    slotConfigArtifact.sha256,
    "candidateInput.forcedValidationSlotConfigArtifact.sha256",
  );
  const slotConfigArtifactBytes = readFileSync(slotConfigArtifactPath);
  if (
    createHash("sha256").update(slotConfigArtifactBytes).digest("hex") !==
    slotConfigArtifactSha256
  ) {
    throw new Error("Node slot-config evidence SHA-256 mismatch");
  }
  const slotConfigDocument = exactKeysRecord(
    slotConfigArtifact.document,
    "Architecture G candidate slot-config artifact document",
    ["schemaVersion", "capturedAtIso", "network", "source", "slotConfig"],
  );
  if (
    slotConfigDocument.schemaVersion !== "midgard-node-slot-config-evidence-v1"
  ) {
    throw new Error("Unsupported node slot-config evidence schema");
  }
  canonicalUtcTimestamp(
    slotConfigDocument.capturedAtIso,
    "candidateInput.forcedValidationSlotConfigArtifact.capturedAtIso",
  );
  const slotConfigSource =
    slotConfigDocument.network === "Custom"
      ? exactKeysRecord(
          slotConfigDocument.source,
          "Custom slot-config source",
          ["kind", "configurationSha256"],
        )
      : exactKeysRecord(
          slotConfigDocument.source,
          "Static slot-config source",
          ["kind", "lucidVersion"],
        );
  if (slotConfigDocument.network === "Custom") {
    if (slotConfigSource.kind !== "local_ogmios_genesis") {
      throw new Error("Custom slot-config source is invalid");
    }
    sha256Digest(
      slotConfigSource.configurationSha256,
      "candidateInput.forcedValidationSlotConfigArtifact.source.configurationSha256",
    );
  } else if (
    !["Mainnet", "Preview", "Preprod"].includes(
      slotConfigDocument.network as string,
    ) ||
    slotConfigSource.kind !== "lucid_network_table" ||
    slotConfigSource.lucidVersion !== "0.6.0"
  ) {
    throw new Error("Static slot-config source is invalid");
  }
  const artifactSlotConfig = exactKeysRecord(
    slotConfigDocument.slotConfig,
    "Architecture G candidate artifact slot configuration",
    ["zeroTime", "zeroSlot", "slotLength"],
  );
  if (
    JSON.stringify(
      JSON.parse(slotConfigArtifactBytes.toString("utf8")) as unknown,
    ) !== JSON.stringify(slotConfigArtifact.document)
  ) {
    throw new Error(
      "Architecture G candidate slot-config document does not match its bound artifact",
    );
  }
  const staticSlotConfigs: Readonly<
    Record<string, Readonly<Record<string, number>>>
  > = {
    Mainnet: {
      zeroTime: 1_596_059_091_000,
      zeroSlot: 4_492_800,
      slotLength: 1_000,
    },
    Preview: {
      zeroTime: 1_666_656_000_000,
      zeroSlot: 0,
      slotLength: 1_000,
    },
    Preprod: {
      zeroTime: 1_655_769_600_000,
      zeroSlot: 86_400,
      slotLength: 1_000,
    },
  };
  if (
    slotConfigDocument.network !== "Custom" &&
    JSON.stringify(artifactSlotConfig) !==
      JSON.stringify(staticSlotConfigs[slotConfigDocument.network as string])
  ) {
    throw new Error(
      "Static slot configuration does not match the pinned Lucid network table",
    );
  }
  const workerInput = exactKeysRecord(
    input.workerInput,
    "Architecture G candidate worker input",
    ["data"],
  );
  const data = exactKeysRecord(
    workerInput.data,
    "Architecture G candidate worker data",
    [
      "availableConfirmedBlock",
      "availableLocalFinalizationBlock",
      "currentBlockStartTimeMs",
      "forcedValidationSlotConfig",
      "localFinalizationPending",
      "ledgerStoreLeaseOwner",
      "mempoolTxsCountSoFar",
      "sizeOfProcessedTxsSoFar",
      "baseSnapshotId",
      "stateQueueHasUnmergedTail",
      "speculativeBuild",
    ],
  );
  const speculativeBuild = exactKeysRecord(
    data.speculativeBuild,
    "Architecture G candidate speculative build",
    [
      "base",
      "watermarks",
      "excludedMempoolTxIds",
      "excludedDepositEventIds",
      "excludedForcedTransactionEventIds",
      "excludedWithdrawalEventIds",
    ],
  );
  const base = exactKeysRecord(
    speculativeBuild.base,
    "Architecture G candidate speculative base",
    ["headerHash", "utxosRoot", "blockEndTimeMs", "submittedTxHash"],
  );
  const watermarks = exactKeysRecord(
    speculativeBuild.watermarks,
    "Architecture G candidate barrier watermarks",
    ["depositMs", "withdrawalMs", "txOrderMs", "refreshedAtMs"],
  );
  const forcedValidationSlotConfig = exactKeysRecord(
    data.forcedValidationSlotConfig,
    "Architecture G candidate forced-validation slot configuration",
    ["zeroTime", "zeroSlot", "slotLength"],
  );
  if (
    input.schemaVersion !== "midgard-architecture-g-commit-candidate-input-v1"
  ) {
    throw new Error("Unsupported Architecture G commit-candidate input");
  }
  for (const [pathValue, label] of [
    [input.levelPath, "candidateInput.levelPath"],
    [input.binaryPath, "candidateInput.binaryPath"],
    [input.sidecarPath, "candidateInput.sidecarPath"],
    [input.fixtureCreationPath, "candidateInput.fixtureCreationPath"],
  ] as const) {
    canonicalAbsolutePath(pathValue, label);
  }
  for (const [hashValue, label] of [
    [input.binarySha256, "candidateInput.binarySha256"],
    [input.corpusSha256, "candidateInput.corpusSha256"],
    [input.corpusSliceSha256, "candidateInput.corpusSliceSha256"],
    [input.fundingMapSha256, "candidateInput.fundingMapSha256"],
    [input.fixtureCreationSha256, "candidateInput.fixtureCreationSha256"],
  ] as const) {
    sha256Digest(hashValue, label);
  }
  positiveSafeInteger(
    input.expectedTransactionCount,
    "candidateInput.expectedTransactionCount",
  );
  const fixtureInitialUtxoCount = positiveSafeInteger(
    input.fixtureInitialUtxoCount,
    "candidateInput.fixtureInitialUtxoCount",
  );
  if (
    input.corpusSha256 !== phase1FormalBinding.corpus.corpusSha256 ||
    aggregate.entryCount !== fixtureInitialUtxoCount ||
    data.availableConfirmedBlock !== "" ||
    data.availableLocalFinalizationBlock !== "" ||
    data.localFinalizationPending !== false ||
    !/^commit:[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/u.test(
      data.ledgerStoreLeaseOwner as string,
    ) ||
    data.mempoolTxsCountSoFar !== 0 ||
    data.sizeOfProcessedTxsSoFar !== 0 ||
    data.stateQueueHasUnmergedTail !== true
  ) {
    throw new Error("Architecture G candidate input identity is invalid");
  }
  positiveSafeInteger(
    aggregate.encodedTupleBytes,
    "candidateInput.baseUtxoPayloadAggregate.encodedTupleBytes",
  );
  const currentBlockStartTimeMs = positiveSafeInteger(
    data.currentBlockStartTimeMs,
    "candidateInput.currentBlockStartTimeMs",
  );
  const slotZeroTime = nonNegativeSafeInteger(
    forcedValidationSlotConfig.zeroTime,
    "candidateInput.forcedValidationSlotConfig.zeroTime",
  );
  const slotZeroSlot = nonNegativeSafeInteger(
    forcedValidationSlotConfig.zeroSlot,
    "candidateInput.forcedValidationSlotConfig.zeroSlot",
  );
  const slotLength = positiveSafeInteger(
    forcedValidationSlotConfig.slotLength,
    "candidateInput.forcedValidationSlotConfig.slotLength",
  );
  if (
    JSON.stringify(forcedValidationSlotConfig) !==
    JSON.stringify(artifactSlotConfig)
  ) {
    throw new Error(
      "Architecture G candidate worker slot configuration does not match its bound artifact",
    );
  }
  const currentBlockSlot =
    Math.floor((currentBlockStartTimeMs - slotZeroTime) / slotLength) +
    slotZeroSlot;
  if (!Number.isSafeInteger(currentBlockSlot) || currentBlockSlot < 0) {
    throw new Error(
      "Architecture G candidate block time is outside its forced-validation slot configuration",
    );
  }
  const submittedTxHash = sha256Digest(
    base.submittedTxHash,
    "candidateInput.speculativeBuild.base.submittedTxHash",
  );
  sha256Digest(
    base.utxosRoot,
    "candidateInput.speculativeBuild.base.utxosRoot",
  );
  if (
    typeof base.headerHash !== "string" ||
    !/^[0-9a-f]{56}$/u.test(base.headerHash) ||
    base.headerHash !== submittedTxHash.slice(0, 56) ||
    base.blockEndTimeMs !== currentBlockStartTimeMs ||
    data.baseSnapshotId !== `architecture-g-candidate:${submittedTxHash}`
  ) {
    throw new Error("Architecture G candidate speculative base is invalid");
  }
  const watermarkValues = [
    positiveSafeInteger(watermarks.depositMs, "watermarks.depositMs"),
    positiveSafeInteger(watermarks.withdrawalMs, "watermarks.withdrawalMs"),
    positiveSafeInteger(watermarks.txOrderMs, "watermarks.txOrderMs"),
  ];
  const refreshedAtMs = positiveSafeInteger(
    watermarks.refreshedAtMs,
    "watermarks.refreshedAtMs",
  );
  if (
    Math.max(...watermarkValues) > refreshedAtMs ||
    currentBlockStartTimeMs >= Math.min(...watermarkValues)
  ) {
    throw new Error("Architecture G candidate barrier watermarks are invalid");
  }
  for (const field of [
    "excludedMempoolTxIds",
    "excludedDepositEventIds",
    "excludedForcedTransactionEventIds",
    "excludedWithdrawalEventIds",
  ] as const) {
    if (
      !Array.isArray(speculativeBuild[field]) ||
      speculativeBuild[field].length !== 0
    ) {
      throw new Error(
        `Architecture G candidate ${field} must be an exact empty array`,
      );
    }
  }
  return input as ArchitectureGCommitCandidateInput;
};
