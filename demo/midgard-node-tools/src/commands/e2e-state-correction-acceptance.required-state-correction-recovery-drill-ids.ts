export const E2E_STATE_CORRECTION_ACCEPTANCE_SCHEMA_VERSION =
  "midgard-e2e-state-correction-acceptance-v1" as const;

export const REQUIRED_STATE_CORRECTION_RECOVERY_DRILL_IDS = [
  "crash-before-detect",
  "crash-after-detect",
  "crash-before-persist-evidence",
  "crash-after-persist-evidence",
  "crash-before-proof-init",
  "crash-after-proof-init",
  "crash-before-submit",
  "crash-after-submit",
  "crash-before-proof-token-confirm",
  "crash-after-proof-token-confirm",
  "crash-before-removal-slashing-confirm",
  "crash-after-removal-slashing-confirm",
  "crash-before-terminal-verification",
  "crash-after-terminal-verification",
  "l1-rollback-before-finality",
  "l1-rollback-after-finality-within-k",
  "configured-source-inconsistency",
  "external-provider-disagreement",
  "missing-da",
  "withholding",
  "stale-manifest",
  "recorded-live-chain-rewind",
] as const;

export const REQUIRED_STATE_CORRECTION_GATE_LABELS = [
  "state_correction_acceptance",
  "state_correction_exact_economics",
  "withdrawal_reserve_payout",
  "forced_classification_directions",
  "watcher_crash_rollback_matrix",
  "state_correction_final_reconciliation",
] as const;

type DeploymentBinding = {
  readonly manifestId: string;
  readonly blueprintSha256: string;
  readonly catalogueRoot: string;
  readonly parametersSha256: string;
};

type ChainPoint = {
  readonly slot: string;
  readonly blockHash: string;
};

export type StateCorrectionFamilyDrill = {
  readonly familyId: string;
  readonly violationId: string;
  readonly headerHash: string;
  readonly routeId: string;
  readonly detectionSource: "public-l1-da";
  readonly watcherDriven: true;
  readonly initTxHash: string;
  readonly proofStepTxHashes: readonly string[];
  readonly proofTokenTxHash: string;
  readonly removalTxHash: string;
  readonly correctionTxHash: string;
  readonly permanentProofTokenRetained: true;
  readonly stateQueueNodeRemoved: true;
  readonly correctedQueueObserved: true;
  readonly expectedSlashLovelace: string;
  readonly observedSlashLovelace: string;
  readonly expectedProverRewardLovelace: string;
  readonly observedProverRewardLovelace: string;
  readonly chainPoint: ChainPoint;
  readonly finalStateRoot: string;
};

export type WithdrawalReservePayout = {
  readonly withdrawalOrderTxHash: string;
  readonly reserveTxHash: string;
  readonly payoutInitTxHash: string;
  readonly payoutAddTxHashes: readonly string[];
  readonly payoutConcludeTxHash: string;
  readonly expectedDestination: string;
  readonly observedDestination: string;
  readonly expectedPayoutValueSha256: string;
  readonly observedPayoutValueSha256: string;
  readonly expectedReserveValueSha256: string;
  readonly observedReserveValueSha256: string;
  readonly reserveAccountingExact: true;
  readonly finalStatus: "paid";
  readonly chainPoint: ChainPoint;
};

export type ForcedClassificationDrill = {
  readonly direction: "valid-marked-invalid" | "invalid-marked-valid";
  readonly operatorClassification: "valid" | "invalid";
  readonly canonicalClassification: "valid" | "invalid";
  readonly finalClassification: "valid" | "invalid";
  readonly detectionSource: "public-l1-da";
  readonly watcherDriven: true;
  readonly routeId: string;
  readonly evidenceTxHash: string;
  readonly correctionTxHash: string;
  readonly corrected: true;
  readonly chainPoint: ChainPoint;
};

export type RecoveryDrill = {
  readonly id: string;
  readonly status: "recovered";
  readonly failClosed: true;
  readonly duplicateSubmissions: 0;
  readonly lostEvidence: 0;
  readonly falseVerifiedStates: 0;
  readonly unrecoverableWorkflows: 0;
  readonly manualRepair: false;
  readonly watcherReadyAfterRecovery: true;
  readonly evidenceSha256: string;
};

export type E2EStateCorrectionAcceptance = {
  readonly schemaVersion: typeof E2E_STATE_CORRECTION_ACCEPTANCE_SCHEMA_VERSION;
  readonly runId: string;
  readonly network: "Preprod";
  readonly deployment: DeploymentBinding;
  readonly families: readonly StateCorrectionFamilyDrill[];
  readonly withdrawalReservePayout: WithdrawalReservePayout;
  readonly forcedClassifications: readonly ForcedClassificationDrill[];
  readonly recoveryDrills: readonly RecoveryDrill[];
  readonly finalState: {
    readonly stateQueueDepth: 0;
    readonly unfinishedMutationJobs: 0;
    readonly pendingFinalizations: 0;
    readonly watcherReady: true;
    readonly watcherVerificationResumed: true;
    readonly exactEconomicReconciliation: true;
    readonly finalStateSha256: string;
  };
};

const SHA256_PATTERN = /^[0-9a-f]{64}$/u;

const LOVELACE_PATTERN = /^(?:0|[1-9][0-9]*)$/u;

export const record = (
  value: unknown,
  field: string,
): Readonly<Record<string, unknown>> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${field} must be an object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

export const exactKeys = (
  value: Readonly<Record<string, unknown>>,
  keys: readonly string[],
  field: string,
): void => {
  const actual = Object.keys(value).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(
      `${field} must contain exactly: ${expected.join(", ")}; found: ${actual.join(", ")}`,
    );
  }
};

export const string = (value: unknown, field: string): string => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value !== value.trim()
  ) {
    throw new Error(`${field} must be a non-empty canonical string`);
  }
  return value;
};

export const literal = <T extends string | number | boolean>(
  value: unknown,
  expected: T,
  field: string,
): T => {
  if (value !== expected) {
    throw new Error(`${field} must be ${JSON.stringify(expected)}`);
  }
  return expected;
};

export const sha256 = (value: unknown, field: string): string => {
  const parsed = string(value, field);
  if (!SHA256_PATTERN.test(parsed)) {
    throw new Error(`${field} must be lowercase SHA-256 hex`);
  }
  return parsed;
};

export const positiveLovelace = (value: unknown, field: string): string => {
  const parsed = string(value, field);
  if (!LOVELACE_PATTERN.test(parsed) || BigInt(parsed) <= 0n) {
    throw new Error(`${field} must be a positive canonical lovelace string`);
  }
  return parsed;
};

export const stringArray = (
  value: unknown,
  field: string,
  parse: (entry: unknown, entryField: string) => string = string,
): readonly string[] => {
  if (!Array.isArray(value) || value.length === 0) {
    throw new Error(`${field} must be a non-empty array`);
  }
  const parsed = value.map((entry, index) =>
    parse(entry, `${field}[${index.toString()}]`),
  );
  if (new Set(parsed).size !== parsed.length) {
    throw new Error(`${field} must not contain duplicates`);
  }
  return parsed;
};

export const parseChainPoint = (value: unknown, field: string): ChainPoint => {
  const candidate = record(value, field);
  exactKeys(candidate, ["slot", "blockHash"], field);
  const slot = string(candidate.slot, `${field}.slot`);
  if (!LOVELACE_PATTERN.test(slot)) {
    throw new Error(`${field}.slot must be a canonical non-negative integer`);
  }
  return {
    slot,
    blockHash: sha256(candidate.blockHash, `${field}.blockHash`),
  };
};

export const parseDeployment = (value: unknown): DeploymentBinding => {
  const candidate = record(value, "state-correction deployment");
  exactKeys(
    candidate,
    ["manifestId", "blueprintSha256", "catalogueRoot", "parametersSha256"],
    "state-correction deployment",
  );
  return {
    manifestId: sha256(candidate.manifestId, "deployment.manifestId"),
    blueprintSha256: sha256(
      candidate.blueprintSha256,
      "deployment.blueprintSha256",
    ),
    catalogueRoot: sha256(candidate.catalogueRoot, "deployment.catalogueRoot"),
    parametersSha256: sha256(
      candidate.parametersSha256,
      "deployment.parametersSha256",
    ),
  };
};

export const FAMILY_KEYS = [
  "familyId",
  "violationId",
  "headerHash",
  "routeId",
  "detectionSource",
  "watcherDriven",
  "initTxHash",
  "proofStepTxHashes",
  "proofTokenTxHash",
  "removalTxHash",
  "correctionTxHash",
  "permanentProofTokenRetained",
  "stateQueueNodeRemoved",
  "correctedQueueObserved",
  "expectedSlashLovelace",
  "observedSlashLovelace",
  "expectedProverRewardLovelace",
  "observedProverRewardLovelace",
  "chainPoint",
  "finalStateRoot",
] as const;
