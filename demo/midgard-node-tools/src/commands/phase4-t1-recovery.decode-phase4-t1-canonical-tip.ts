import { exactObjectKeys } from "midgard-node/exact-object-keys";

export const PHASE4_T1_ACCEPTANCE_TOKEN =
  "phase4-t1-local-canonical-advance-v1";

export const PHASE4_T1_PROBE_SCHEMA = "midgard-phase4-t1-probe-v1";

export const PHASE4_T1_ADVANCE_SCHEMA =
  "midgard-phase4-t1-canonical-advance-v1";

export const PHASE4_T1_RECOVERY_SCHEMA =
  "midgard-phase4-t1-recovery-attestation-v1";

export const L2_HEADER_HASH = /^[a-f0-9]{56}$/u;

export const CARDANO_HASH = /^[a-f0-9]{64}$/u;

export const SHA256 = CARDANO_HASH;

export const SAFE_ATTEMPT_ID = /^[a-zA-Z0-9][a-zA-Z0-9_.-]{0,127}$/u;

export const CARDANO_OUT_REF = /^[a-f0-9]{64}#(?:0|[1-9][0-9]*)$/u;

export const CANONICAL_NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const requireL2HeaderHash = (value: string, label: string): string => {
  if (!L2_HEADER_HASH.test(value)) {
    throw new Error(
      `${label} must be a 28-byte L2 header hash (56 lowercase hex)`,
    );
  }
  return value;
};

export const requireCardanoHash = (value: string, label: string): string => {
  if (!CARDANO_HASH.test(value)) {
    throw new Error(
      `${label} must be a 32-byte Cardano hash (64 lowercase hex)`,
    );
  }
  return value;
};

export const requireL2TransactionId = (
  value: string,
  label: string,
): string => {
  if (!CARDANO_HASH.test(value)) {
    throw new Error(
      `${label} must be a 32-byte L2 transaction id (64 lowercase hex)`,
    );
  }
  return value;
};

export const requirePhase4T1CandidateLine = ({
  attemptLog,
  baseHeaderHash,
  label,
}: {
  readonly attemptLog: string;
  readonly baseHeaderHash: string;
  readonly label: string;
}): string => {
  const base = requireL2HeaderHash(
    baseHeaderHash,
    "candidate base header hash",
  );
  const pattern = new RegExp(
    `pipeline_trace phase=candidate_ready[^\\n]*base_header_hash=${base}(?:\\s|$)`,
    "u",
  );
  const line = attemptLog
    .split("\n")
    .filter((candidate) => pattern.test(candidate))
    .at(-1);
  if (line === undefined) {
    throw new Error(`T1 attempt has no ${label} evidence`);
  }
  return line;
};

export const assertPhase4T1ReplacementAttemptOrdering = ({
  attemptLog,
  recoveredTipHeaderHash,
}: {
  readonly attemptLog: string;
  readonly recoveredTipHeaderHash: string;
}): { readonly replacementCandidateLine: string } => {
  const recoveryMarkerIndex = attemptLog.indexOf(
    "recovered canonical chain tip",
  );
  if (recoveryMarkerIndex < 0) {
    throw new Error("T1 restart did not execute stale-pending recovery");
  }
  const replacementCandidateLine = requirePhase4T1CandidateLine({
    attemptLog: attemptLog.slice(recoveryMarkerIndex),
    baseHeaderHash: recoveredTipHeaderHash,
    label: `replacement candidate N' built on recovered F ${recoveredTipHeaderHash}`,
  });
  const replacementLineIndex = attemptLog.indexOf(
    replacementCandidateLine,
    recoveryMarkerIndex,
  );
  const submissionIndex = attemptLog.indexOf(
    "pipeline_trace phase=candidate_submitted",
    replacementLineIndex,
  );
  if (replacementLineIndex < recoveryMarkerIndex || submissionIndex < 0) {
    throw new Error(
      "T1 per-attempt log does not order stale recovery before F-based build and submission",
    );
  }
  return { replacementCandidateLine };
};

const requireSha256 = (value: string, label: string): string => {
  if (!SHA256.test(value)) {
    throw new Error(`${label} must be a SHA-256 digest (64 lowercase hex)`);
  }
  return value;
};

export type Phase4T1Gate = {
  readonly snapshotIdentitySha256: string;
  readonly attemptId: string;
};

export const assertPhase4T1Gate = ({
  env,
  snapshotIdentitySha256,
  attemptId,
}: {
  readonly env: Readonly<NodeJS.ProcessEnv>;
  readonly snapshotIdentitySha256: string;
  readonly attemptId: string;
}): Phase4T1Gate => {
  if (env.MIDGARD_PHASE4_PROCESS_ACCEPTANCE !== "pipelined-commit-live-v1") {
    throw new Error("Phase 4 T1 command requires the process-acceptance token");
  }
  if (env.MIDGARD_PHASE4_PROCESS_TARGET !== "local-devnet") {
    throw new Error(
      "Phase 4 T1 command refuses every target except local-devnet",
    );
  }
  if (env.MIDGARD_PHASE4_T1_ACCEPTANCE_TOKEN !== PHASE4_T1_ACCEPTANCE_TOKEN) {
    throw new Error("Phase 4 T1 command requires its dedicated mutation token");
  }
  const expectedIdentity = requireSha256(
    snapshotIdentitySha256,
    "snapshotIdentitySha256",
  );
  if (env.MIDGARD_PHASE4_T1_SNAPSHOT_IDENTITY_SHA256 !== expectedIdentity) {
    throw new Error(
      "Phase 4 T1 command snapshot identity does not match its gated environment",
    );
  }
  if (!SAFE_ATTEMPT_ID.test(attemptId)) {
    throw new Error("Phase 4 T1 attempt id is missing or unsafe");
  }
  if (env.MIDGARD_PHASE4_T1_ATTEMPT_ID !== attemptId) {
    throw new Error(
      "Phase 4 T1 command attempt id does not match its gated environment",
    );
  }
  return { snapshotIdentitySha256: expectedIdentity, attemptId };
};

export type Phase4T1CanonicalTip = {
  readonly headerHash: string;
  readonly outRef: string;
  readonly datumKind: "confirmed" | "header";
  readonly prevHeaderHash: string;
  readonly prevUtxosRoot: string | null;
  readonly utxosRoot: string;
  readonly transactionsRoot: string | null;
  readonly depositsRoot: string | null;
  readonly withdrawalsRoot: string | null;
  readonly forcedTransactionsRoot: string | null;
  readonly transitionTraceRoot: string | null;
  readonly eventToStepRoot: string | null;
  readonly withdrawalCount: string | null;
  readonly forcedTransactionCount: string | null;
  readonly l2TransactionCount: string | null;
  readonly depositCount: string | null;
  readonly totalEventCount: string | null;
  readonly transitionStepCount: string | null;
  readonly startTimeMs: number;
  readonly endTimeMs: number;
};

export type Phase4T1ProbeEvidence = {
  readonly schemaVersion: typeof PHASE4_T1_PROBE_SCHEMA;
  readonly snapshotIdentitySha256: string;
  readonly attemptId: string;
  readonly canonicalHeaderHashes: readonly string[];
  readonly canonicalTip: Phase4T1CanonicalTip;
};

export const decodePhase4T1CanonicalTip = (
  value: unknown,
): Phase4T1CanonicalTip => {
  if (
    !exactObjectKeys(value, [
      "headerHash",
      "outRef",
      "datumKind",
      "prevHeaderHash",
      "prevUtxosRoot",
      "utxosRoot",
      "transactionsRoot",
      "depositsRoot",
      "withdrawalsRoot",
      "forcedTransactionsRoot",
      "transitionTraceRoot",
      "eventToStepRoot",
      "withdrawalCount",
      "forcedTransactionCount",
      "l2TransactionCount",
      "depositCount",
      "totalEventCount",
      "transitionStepCount",
      "startTimeMs",
      "endTimeMs",
    ])
  ) {
    throw new Error(
      "Phase 4 T1 canonical tip fields do not match the exact V1 schema",
    );
  }
  if (
    typeof value.headerHash !== "string" ||
    !L2_HEADER_HASH.test(value.headerHash) ||
    typeof value.outRef !== "string" ||
    !CARDANO_OUT_REF.test(value.outRef) ||
    (value.datumKind !== "confirmed" && value.datumKind !== "header") ||
    typeof value.prevHeaderHash !== "string" ||
    !L2_HEADER_HASH.test(value.prevHeaderHash) ||
    typeof value.utxosRoot !== "string" ||
    !CARDANO_HASH.test(value.utxosRoot) ||
    !Number.isSafeInteger(value.startTimeMs) ||
    (value.startTimeMs as number) < 0 ||
    !Number.isSafeInteger(value.endTimeMs) ||
    (value.endTimeMs as number) <= (value.startTimeMs as number)
  ) {
    throw new Error("Phase 4 T1 canonical tip contains a noncanonical value");
  }
  const nullableRoots = [
    "prevUtxosRoot",
    "transactionsRoot",
    "depositsRoot",
    "withdrawalsRoot",
    "forcedTransactionsRoot",
    "transitionTraceRoot",
    "eventToStepRoot",
  ] as const;
  const nullableCounts = [
    "withdrawalCount",
    "forcedTransactionCount",
    "l2TransactionCount",
    "depositCount",
    "totalEventCount",
    "transitionStepCount",
  ] as const;
  for (const key of nullableRoots) {
    const root = value[key];
    if (
      root !== null &&
      (typeof root !== "string" || !CARDANO_HASH.test(root))
    ) {
      throw new Error(`Phase 4 T1 canonical tip ${key} is noncanonical`);
    }
  }
  for (const key of nullableCounts) {
    const count = value[key];
    if (
      count !== null &&
      (typeof count !== "string" || !CANONICAL_NATURAL.test(count))
    ) {
      throw new Error(`Phase 4 T1 canonical tip ${key} is noncanonical`);
    }
  }
  if (
    value.datumKind === "confirmed" &&
    [...nullableRoots, ...nullableCounts].some((key) => value[key] !== null)
  ) {
    throw new Error("Phase 4 T1 confirmed tip contains header-only V1 fields");
  }
  if (
    value.datumKind === "header" &&
    [...nullableRoots, ...nullableCounts].some((key) => value[key] === null)
  ) {
    throw new Error("Phase 4 T1 header tip is missing required V1 fields");
  }
  return value as Phase4T1CanonicalTip;
};
