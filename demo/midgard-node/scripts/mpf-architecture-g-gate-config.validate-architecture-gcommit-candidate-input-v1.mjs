import {
  validateArchitectureGPhase1FormalBindingIdentity,
  validateArchitectureGRuntimeIdentity,
} from "./mpf-architecture-g-gate-config.capture-architecture-gphase1-formal-binding-identity.mjs";
import {
  ARCHITECTURE_G_FORMAL_GATE_CONFIG,
  isCanonicalAbsolutePath,
  isHash,
  isNonNegativeSafeInteger,
  isPositiveSafeInteger,
  requireExactObjectKeys,
} from "./mpf-architecture-g-gate-config.validate-architecture-gfixture-creation-evidence.mjs";
import { readNodeSlotConfigEvidenceV1 } from "./node-slot-config-evidence.mjs";

const positiveSafeInteger = (value, label) => {
  if (typeof value !== "string" || !/^[1-9]\d*$/u.test(value)) {
    throw new Error(`${label} must be a positive base-10 integer`);
  }
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed <= 0) {
    throw new Error(`${label} must be a positive safe integer`);
  }
  return parsed;
};

export const resolveArchitectureGGateConfig = ({
  mode,
  profile,
  runs,
  transactions,
}) => {
  if (mode !== "50k" && mode !== "growth") {
    throw new Error("Use --mode=50k or --mode=growth");
  }
  if (profile !== "formal" && profile !== "smoke") {
    throw new Error("Use --profile=formal or --profile=smoke");
  }
  const required = ARCHITECTURE_G_FORMAL_GATE_CONFIG[mode];
  const resolvedRuns = positiveSafeInteger(
    runs ?? required.runs.toString(),
    "--runs",
  );
  const resolvedTransactions = positiveSafeInteger(
    transactions ?? required.transactions.toString(),
    "--transactions",
  );
  if (
    profile === "formal" &&
    (resolvedRuns !== required.runs ||
      resolvedTransactions !== required.transactions)
  ) {
    throw new Error(
      `Formal ${mode} gate requires --runs=${required.runs.toString()} and --transactions=${required.transactions.toString()}; use --profile=smoke for reduced diagnostics`,
    );
  }
  return {
    mode,
    profile,
    formal: profile === "formal",
    runs: resolvedRuns,
    transactions: resolvedTransactions,
    required,
  };
};

export const validateArchitectureGCommitCandidateInputV1 = (input) => {
  requireExactObjectKeys(
    input,
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
      "baseUtxosRoot",
      "baseUtxoPayloadAggregate",
      "forcedValidationSlotConfigArtifact",
      "workerInput",
    ],
    "Architecture G commit-candidate input",
  );
  validateArchitectureGPhase1FormalBindingIdentity(input.phase1FormalBinding);
  validateArchitectureGRuntimeIdentity({
    identity: input.runtimeIdentity,
    expectedVersion: input.runtimeIdentity?.version,
    expectedExecutableSha256: input.runtimeIdentity?.executableSha256,
  });
  requireExactObjectKeys(
    input.baseUtxoPayloadAggregate,
    ["entryCount", "encodedTupleBytes"],
    "Architecture G candidate base UTxO aggregate",
  );
  const slotConfigArtifact = requireExactObjectKeys(
    input.forcedValidationSlotConfigArtifact,
    ["path", "sha256", "document"],
    "Architecture G candidate slot-config artifact binding",
  );
  const verifiedSlotConfigDocument = readNodeSlotConfigEvidenceV1({
    path: slotConfigArtifact.path,
    expectedSha256: slotConfigArtifact.sha256,
  });
  requireExactObjectKeys(
    input.workerInput,
    ["data"],
    "Architecture G candidate worker input",
  );
  const data = requireExactObjectKeys(
    input.workerInput.data,
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
    ],
    "Architecture G candidate worker data",
  );
  const forcedValidationSlotConfig = requireExactObjectKeys(
    data.forcedValidationSlotConfig,
    ["zeroTime", "zeroSlot", "slotLength"],
    "Architecture G candidate forced-validation slot configuration",
  );
  const currentBlockSlot =
    Math.floor(
      (data.currentBlockStartTimeMs - forcedValidationSlotConfig.zeroTime) /
        forcedValidationSlotConfig.slotLength,
    ) + forcedValidationSlotConfig.zeroSlot;
  if (
    input.schemaVersion !==
      "midgard-architecture-g-commit-candidate-input-v1" ||
    ![
      input.binarySha256,
      input.corpusSha256,
      input.corpusSliceSha256,
      input.fundingMapSha256,
      input.fixtureCreationSha256,
      input.baseUtxosRoot,
    ].every(isHash) ||
    ![
      input.levelPath,
      input.binaryPath,
      input.sidecarPath,
      input.fixtureCreationPath,
    ].every(isCanonicalAbsolutePath) ||
    !isPositiveSafeInteger(input.expectedTransactionCount) ||
    !isPositiveSafeInteger(input.fixtureInitialUtxoCount) ||
    input.baseUtxoPayloadAggregate.entryCount !==
      input.fixtureInitialUtxoCount ||
    !isPositiveSafeInteger(input.baseUtxoPayloadAggregate.encodedTupleBytes) ||
    input.corpusSha256 !== input.phase1FormalBinding.corpus.corpusSha256 ||
    JSON.stringify(slotConfigArtifact.document) !==
      JSON.stringify(verifiedSlotConfigDocument) ||
    JSON.stringify(forcedValidationSlotConfig) !==
      JSON.stringify(verifiedSlotConfigDocument.slotConfig) ||
    data.availableConfirmedBlock !== "" ||
    data.availableLocalFinalizationBlock !== "" ||
    !isPositiveSafeInteger(data.currentBlockStartTimeMs) ||
    !isNonNegativeSafeInteger(forcedValidationSlotConfig.zeroTime) ||
    !isNonNegativeSafeInteger(forcedValidationSlotConfig.zeroSlot) ||
    !isPositiveSafeInteger(forcedValidationSlotConfig.slotLength) ||
    !isNonNegativeSafeInteger(currentBlockSlot) ||
    data.localFinalizationPending !== false ||
    !/^commit:[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/u.test(
      data.ledgerStoreLeaseOwner,
    ) ||
    data.mempoolTxsCountSoFar !== 0 ||
    data.sizeOfProcessedTxsSoFar !== 0 ||
    data.stateQueueHasUnmergedTail !== true ||
    typeof data.baseSnapshotId !== "string" ||
    !/^architecture-g-candidate:[0-9a-f]{64}$/u.test(data.baseSnapshotId)
  ) {
    throw new Error("Architecture G commit-candidate input is invalid");
  }
  return input;
};
