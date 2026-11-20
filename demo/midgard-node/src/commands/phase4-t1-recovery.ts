import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { writeTextFileAtomicNoReplace } from "@/files/atomic-write.js";
import { Lucid, MidgardContracts, NodeConfig } from "@/services/index.js";
import { commitExplicitBlockHeaderProgram } from "@/workers/commit-block-header.js";
import {
  fetchLatestCommittedBlockLocal,
  getConfirmedStateFromStateQueueDatumLocal,
  getHeaderFromStateQueueDatumLocal,
  hashBlockHeaderLocal,
  localizeSdkEffect,
  stateQueueBaseHeaderHash,
  stateQueueOutRef,
} from "@/workers/commit-block-header/state-queue.js";

export const PHASE4_T1_ACCEPTANCE_TOKEN =
  "phase4-t1-local-canonical-advance-v1";
export const PHASE4_T1_PROBE_SCHEMA = "midgard-phase4-t1-probe-v1";
export const PHASE4_T1_ADVANCE_SCHEMA =
  "midgard-phase4-t1-canonical-advance-v1";
export const PHASE4_T1_RECOVERY_SCHEMA =
  "midgard-phase4-t1-recovery-attestation-v1";

const L2_HEADER_HASH = /^[a-f0-9]{56}$/u;
const CARDANO_HASH = /^[a-f0-9]{64}$/u;
const SHA256 = CARDANO_HASH;
const SAFE_ATTEMPT_ID = /^[a-zA-Z0-9][a-zA-Z0-9_.-]{0,127}$/u;

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

const safeTimeMs = (value: bigint, label: string): number => {
  const result = Number(value);
  if (!Number.isSafeInteger(result) || result < 0) {
    throw new Error(
      `${label} is not a nonnegative safe POSIX millisecond value`,
    );
  }
  return result;
};

const fetchPhase4T1CanonicalState = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const fetchConfig: SDK.StateQueueFetchConfig = {
    stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
    stateQueuePolicyId: contracts.stateQueue.policyId,
  };
  const sorted = yield* localizeSdkEffect<
    readonly SDK.StateQueueUTxO[],
    SDK.StateQueueError | SDK.LucidError
  >(SDK.fetchSortedStateQueueUTxOsProgram(lucid.api, fetchConfig));
  const latest = yield* fetchLatestCommittedBlockLocal(lucid.api, fetchConfig);
  const latestHeaderHash = yield* stateQueueBaseHeaderHash(latest);
  const canonicalHeaderHashes: string[] = [];
  for (const block of sorted) {
    if (block.datum.key === "Empty") {
      const { data } = yield* getConfirmedStateFromStateQueueDatumLocal(
        block.datum,
      );
      canonicalHeaderHashes.push(
        requireL2HeaderHash(data.headerHash, "confirmed header hash"),
      );
      continue;
    }
    const header = yield* getHeaderFromStateQueueDatumLocal(block.datum);
    canonicalHeaderHashes.push(
      requireL2HeaderHash(
        yield* hashBlockHeaderLocal(header),
        "canonical header hash",
      ),
    );
  }
  const uniqueCanonicalHeaderHashes = [...new Set(canonicalHeaderHashes)];
  if (!uniqueCanonicalHeaderHashes.includes(latestHeaderHash)) {
    throw new Error(
      "Latest state_queue tail is absent from the canonical hash set",
    );
  }

  let canonicalTip: Phase4T1CanonicalTip;
  if (latest.datum.key === "Empty") {
    const { data } = yield* getConfirmedStateFromStateQueueDatumLocal(
      latest.datum,
    );
    canonicalTip = {
      headerHash: requireL2HeaderHash(latestHeaderHash, "canonical tip hash"),
      outRef: stateQueueOutRef(latest),
      datumKind: "confirmed",
      prevHeaderHash: requireL2HeaderHash(
        data.prevHeaderHash,
        "canonical confirmed previous header hash",
      ),
      prevUtxosRoot: null,
      utxosRoot: data.utxoRoot,
      transactionsRoot: null,
      depositsRoot: null,
      withdrawalsRoot: null,
      forcedTransactionsRoot: null,
      transitionTraceRoot: null,
      eventToStepRoot: null,
      withdrawalCount: null,
      forcedTransactionCount: null,
      l2TransactionCount: null,
      depositCount: null,
      totalEventCount: null,
      transitionStepCount: null,
      startTimeMs: safeTimeMs(data.startTime, "confirmed start time"),
      endTimeMs: safeTimeMs(data.endTime, "confirmed end time"),
    };
  } else {
    const header = yield* getHeaderFromStateQueueDatumLocal(latest.datum);
    canonicalTip = {
      headerHash: requireL2HeaderHash(latestHeaderHash, "canonical tip hash"),
      outRef: stateQueueOutRef(latest),
      datumKind: "header",
      prevHeaderHash: requireL2HeaderHash(
        header.prevHeaderHash,
        "canonical previous header hash",
      ),
      prevUtxosRoot: header.prevUtxosRoot,
      utxosRoot: header.utxosRoot,
      transactionsRoot: header.transactionsRoot,
      depositsRoot: header.depositsRoot,
      withdrawalsRoot: header.withdrawalsRoot,
      forcedTransactionsRoot: header.forcedTransactionsRoot,
      transitionTraceRoot: header.transitionTraceRoot,
      eventToStepRoot: header.eventToStepRoot,
      withdrawalCount: header.withdrawalCount.toString(),
      forcedTransactionCount: header.forcedTransactionCount.toString(),
      l2TransactionCount: header.l2TransactionCount.toString(),
      depositCount: header.depositCount.toString(),
      totalEventCount: header.totalEventCount.toString(),
      transitionStepCount: header.transitionStepCount.toString(),
      startTimeMs: safeTimeMs(header.startTime, "header start time"),
      endTimeMs: safeTimeMs(header.endTime, "header end time"),
    };
  }
  return { canonicalHeaderHashes: uniqueCanonicalHeaderHashes, canonicalTip };
});

export type Phase4T1ProbeOptions = Phase4T1Gate & {
  readonly expectedTipHeaderHash?: string;
  readonly expectedPresentHeaderHash?: string;
  readonly expectedAbsentHeaderHash?: string;
};

export const phase4T1ProbeProgram = (
  options: Phase4T1ProbeOptions,
): Effect.Effect<
  Phase4T1ProbeEvidence,
  unknown,
  Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    assertPhase4T1Gate({ ...options, env: process.env });
    const state = yield* fetchPhase4T1CanonicalState;
    const expectedTip =
      options.expectedTipHeaderHash === undefined
        ? undefined
        : requireL2HeaderHash(
            options.expectedTipHeaderHash,
            "expected tip header hash",
          );
    const expectedPresent =
      options.expectedPresentHeaderHash === undefined
        ? undefined
        : requireL2HeaderHash(
            options.expectedPresentHeaderHash,
            "expected present header hash",
          );
    const expectedAbsent =
      options.expectedAbsentHeaderHash === undefined
        ? undefined
        : requireL2HeaderHash(
            options.expectedAbsentHeaderHash,
            "expected absent header hash",
          );
    if (
      expectedTip !== undefined &&
      state.canonicalTip.headerHash !== expectedTip
    ) {
      throw new Error(
        `Canonical L2 tip mismatch: expected=${expectedTip},actual=${state.canonicalTip.headerHash}`,
      );
    }
    if (
      expectedPresent !== undefined &&
      !state.canonicalHeaderHashes.includes(expectedPresent)
    ) {
      throw new Error(
        `Required canonical L2 header is absent: ${expectedPresent}`,
      );
    }
    if (
      expectedAbsent !== undefined &&
      state.canonicalHeaderHashes.includes(expectedAbsent)
    ) {
      throw new Error(
        `Forbidden canonical L2 header is still present: ${expectedAbsent}`,
      );
    }
    return {
      schemaVersion: PHASE4_T1_PROBE_SCHEMA,
      snapshotIdentitySha256: options.snapshotIdentitySha256,
      attemptId: options.attemptId,
      ...state,
    };
  });

export type Phase4T1NoopAdvanceAssertion = {
  readonly baseHeaderHash: string;
  readonly recoveredTipHeaderHash: string;
  readonly abandonedHeaderHash: string;
  readonly baseEndTimeMs: number;
  readonly recoveredEndTimeMs: number;
  readonly minimumRecoveredEndTimeMs: number;
  readonly rootsPreserved: true;
  readonly transitionIsEmpty: true;
};

export const assertPhase4T1NoopAdvance = ({
  before,
  after,
  expectedBaseHeaderHash,
  abandonedHeaderHash,
  minimumEndTimeMs,
}: {
  readonly before: Phase4T1ProbeEvidence;
  readonly after: Phase4T1ProbeEvidence;
  readonly expectedBaseHeaderHash: string;
  readonly abandonedHeaderHash: string;
  readonly minimumEndTimeMs: number;
}): Phase4T1NoopAdvanceAssertion => {
  const expectedBase = requireL2HeaderHash(
    expectedBaseHeaderHash,
    "expected base header hash",
  );
  const abandoned = requireL2HeaderHash(
    abandonedHeaderHash,
    "abandoned header hash",
  );
  if (before.canonicalTip.headerHash !== expectedBase) {
    throw new Error(
      "T1 no-op advance did not start from the expected canonical base",
    );
  }
  if (before.canonicalHeaderHashes.includes(abandoned)) {
    throw new Error(
      "Abandoned header N was canonical before the no-op advance",
    );
  }
  const recovered = after.canonicalTip;
  if (recovered.datumKind !== "header") {
    throw new Error(
      "T1 no-op advance did not produce a state_queue header node",
    );
  }
  if (
    recovered.headerHash === abandoned ||
    recovered.headerHash === expectedBase
  ) {
    throw new Error(
      "T1 no-op advance did not produce a distinct replacement tip F",
    );
  }
  if (recovered.prevHeaderHash !== expectedBase) {
    throw new Error(
      "Replacement tip F does not link to the expected canonical base",
    );
  }
  if (
    recovered.prevUtxosRoot !== before.canonicalTip.utxosRoot ||
    recovered.utxosRoot !== before.canonicalTip.utxosRoot
  ) {
    throw new Error("Replacement tip F changed the canonical UTxO root");
  }
  if (recovered.startTimeMs !== before.canonicalTip.endTimeMs) {
    throw new Error(
      "Replacement tip F start time does not equal its base end time",
    );
  }
  if (
    !Number.isSafeInteger(minimumEndTimeMs) ||
    recovered.endTimeMs < minimumEndTimeMs ||
    recovered.endTimeMs <= recovered.startTimeMs
  ) {
    throw new Error(
      "Replacement tip F does not advance beyond N's end-time bound",
    );
  }
  for (const [label, value] of [
    ["transactionsRoot", recovered.transactionsRoot],
    ["depositsRoot", recovered.depositsRoot],
    ["withdrawalsRoot", recovered.withdrawalsRoot],
    ["forcedTransactionsRoot", recovered.forcedTransactionsRoot],
    ["transitionTraceRoot", recovered.transitionTraceRoot],
    ["eventToStepRoot", recovered.eventToStepRoot],
  ] as const) {
    if (value !== SDK.EMPTY_MERKLE_TREE_ROOT) {
      throw new Error(
        `Replacement tip F ${label} is not the empty authenticated root`,
      );
    }
  }
  for (const [label, value] of [
    ["withdrawalCount", recovered.withdrawalCount],
    ["forcedTransactionCount", recovered.forcedTransactionCount],
    ["l2TransactionCount", recovered.l2TransactionCount],
    ["depositCount", recovered.depositCount],
    ["totalEventCount", recovered.totalEventCount],
    ["transitionStepCount", recovered.transitionStepCount],
  ] as const) {
    if (value !== "0") {
      throw new Error(`Replacement tip F ${label} is not zero`);
    }
  }
  if (after.canonicalHeaderHashes.includes(abandoned)) {
    throw new Error("Abandoned header N reappeared after the no-op advance");
  }
  return {
    baseHeaderHash: expectedBase,
    recoveredTipHeaderHash: recovered.headerHash,
    abandonedHeaderHash: abandoned,
    baseEndTimeMs: before.canonicalTip.endTimeMs,
    recoveredEndTimeMs: recovered.endTimeMs,
    minimumRecoveredEndTimeMs: minimumEndTimeMs,
    rootsPreserved: true,
    transitionIsEmpty: true,
  };
};

export type Phase4T1AdvanceOptions = Phase4T1Gate & {
  readonly expectedBaseHeaderHash: string;
  readonly abandonedHeaderHash: string;
  readonly minimumEndTimeMs: number;
};

export type Phase4T1AdvanceEvidence = {
  readonly schemaVersion: typeof PHASE4_T1_ADVANCE_SCHEMA;
  readonly snapshotIdentitySha256: string;
  readonly attemptId: string;
  readonly abandonedHeaderHash: string;
  readonly before: Phase4T1ProbeEvidence;
  readonly submittedTxHash: string;
  readonly recoveredTipHeaderHash: string;
  readonly blockOutRef: string;
  readonly txSize: number;
  readonly blockEndTimeMs: number;
  readonly after: Phase4T1ProbeEvidence;
  readonly invariants: Phase4T1NoopAdvanceAssertion;
};

export const phase4T1AdvanceProgram = (
  options: Phase4T1AdvanceOptions,
): Effect.Effect<
  Phase4T1AdvanceEvidence,
  unknown,
  Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    assertPhase4T1Gate({ ...options, env: process.env });
    const expectedBaseHeaderHash = requireL2HeaderHash(
      options.expectedBaseHeaderHash,
      "expected base header hash",
    );
    const abandonedHeaderHash = requireL2HeaderHash(
      options.abandonedHeaderHash,
      "abandoned header hash",
    );
    if (
      !Number.isSafeInteger(options.minimumEndTimeMs) ||
      options.minimumEndTimeMs <= 0
    ) {
      throw new Error("minimumEndTimeMs must be a positive safe integer");
    }
    const before = yield* phase4T1ProbeProgram({
      ...options,
      expectedTipHeaderHash: expectedBaseHeaderHash,
      expectedAbsentHeaderHash: abandonedHeaderHash,
    });
    const submitted = yield* commitExplicitBlockHeaderProgram({
      utxosRoot: before.canonicalTip.utxosRoot,
      transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      endTimeMs: options.minimumEndTimeMs,
      awaitConfirmation: true,
    });
    requireCardanoHash(submitted.submittedTxHash, "canonical advance tx hash");
    requireL2HeaderHash(submitted.headerHash, "recovered L2 tip hash");
    if (
      submitted.blockOutRef === null ||
      !/^[a-f0-9]{64}#[0-9]+$/u.test(submitted.blockOutRef)
    ) {
      throw new Error(
        "Canonical advance did not resolve a Cardano block outref",
      );
    }
    const after = yield* phase4T1ProbeProgram({
      ...options,
      expectedTipHeaderHash: submitted.headerHash,
      expectedAbsentHeaderHash: abandonedHeaderHash,
    });
    if (after.canonicalTip.endTimeMs !== submitted.blockEndTimeMs) {
      throw new Error(
        "Canonical advance output and provider-visible F end time differ",
      );
    }
    const invariants = assertPhase4T1NoopAdvance({
      before,
      after,
      expectedBaseHeaderHash,
      abandonedHeaderHash,
      minimumEndTimeMs: options.minimumEndTimeMs,
    });
    return {
      schemaVersion: PHASE4_T1_ADVANCE_SCHEMA,
      snapshotIdentitySha256: options.snapshotIdentitySha256,
      attemptId: options.attemptId,
      abandonedHeaderHash,
      before,
      submittedTxHash: submitted.submittedTxHash,
      recoveredTipHeaderHash: submitted.headerHash,
      blockOutRef: submitted.blockOutRef,
      txSize: submitted.txSize,
      blockEndTimeMs: submitted.blockEndTimeMs,
      after,
      invariants,
    };
  });

export type Phase4T1RecoveryAttestation = {
  readonly schemaVersion: typeof PHASE4_T1_RECOVERY_SCHEMA;
  readonly scenarioLabel: string;
  readonly attemptId: string;
  readonly composeProject: string;
  readonly networkMagic: number;
  readonly snapshotSetSha256: string;
  readonly snapshotIdentitySha256: string;
  readonly abandonedHeaderHash: string;
  readonly abandonedSubmittedTxHash: string;
  readonly baseHeaderHash: string;
  readonly recoveredTipHeaderHash: string;
  readonly canonicalAdvanceTxHash: string;
  readonly journalSha256Before: string;
  readonly journalSha256After: string;
  readonly cardanoTip: { readonly slot: number; readonly hash: string };
  readonly kupoCheckpoint: number;
};

export const parseAndValidatePhase4T1RecoveryAttestation = ({
  output,
  expected,
}: {
  readonly output: string;
  readonly expected: {
    readonly scenarioLabel: string;
    readonly attemptId: string;
    readonly composeProject: string;
    readonly networkMagic: number;
    readonly snapshotIdentitySha256: string;
    readonly abandonedHeaderHash: string;
    readonly abandonedSubmittedTxHash: string;
    readonly baseHeaderHash: string;
  };
}): Phase4T1RecoveryAttestation => {
  let value: unknown;
  try {
    value = JSON.parse(output.trim());
  } catch (cause) {
    throw new Error(
      `T1 recovery command must emit exactly one JSON object: ${String(cause)}`,
    );
  }
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error("T1 recovery attestation must be a JSON object");
  }
  const attestationKeys = Object.keys(value).sort((left, right) =>
    left.localeCompare(right),
  );
  const requiredKeys = [
    "schemaVersion",
    "scenarioLabel",
    "attemptId",
    "composeProject",
    "networkMagic",
    "snapshotSetSha256",
    "snapshotIdentitySha256",
    "abandonedHeaderHash",
    "abandonedSubmittedTxHash",
    "baseHeaderHash",
    "recoveredTipHeaderHash",
    "canonicalAdvanceTxHash",
    "journalSha256Before",
    "journalSha256After",
    "cardanoTip",
    "kupoCheckpoint",
  ].sort((left, right) => left.localeCompare(right));
  if (JSON.stringify(attestationKeys) !== JSON.stringify(requiredKeys)) {
    throw new Error(
      "T1 recovery attestation fields do not match the exact schema",
    );
  }
  const attestation = value as Phase4T1RecoveryAttestation;
  const exact = {
    schemaVersion: PHASE4_T1_RECOVERY_SCHEMA,
    scenarioLabel: expected.scenarioLabel,
    attemptId: expected.attemptId,
    composeProject: expected.composeProject,
    networkMagic: expected.networkMagic,
    snapshotIdentitySha256: expected.snapshotIdentitySha256,
    abandonedHeaderHash: requireL2HeaderHash(
      expected.abandonedHeaderHash,
      "expected abandoned header hash",
    ),
    abandonedSubmittedTxHash: requireCardanoHash(
      expected.abandonedSubmittedTxHash,
      "expected abandoned submitted tx hash",
    ),
    baseHeaderHash: requireL2HeaderHash(
      expected.baseHeaderHash,
      "expected base header hash",
    ),
  } as const;
  for (const [key, expectedValue] of Object.entries(exact)) {
    if (
      attestation[key as keyof Phase4T1RecoveryAttestation] !== expectedValue
    ) {
      throw new Error(
        `T1 recovery attestation ${key} mismatch: expected=${String(expectedValue)},actual=${String(attestation[key as keyof Phase4T1RecoveryAttestation])}`,
      );
    }
  }
  requireSha256(attestation.snapshotSetSha256, "snapshot set digest");
  requireSha256(attestation.journalSha256Before, "pre-recovery journal digest");
  requireSha256(attestation.journalSha256After, "post-recovery journal digest");
  if (attestation.journalSha256Before !== attestation.journalSha256After) {
    throw new Error(
      "T1 recovery attestation journal digests are not byte-identical",
    );
  }
  requireL2HeaderHash(
    attestation.recoveredTipHeaderHash,
    "recovered L2 tip hash",
  );
  requireCardanoHash(
    attestation.canonicalAdvanceTxHash,
    "canonical advance tx hash",
  );
  if (
    !Number.isSafeInteger(attestation.cardanoTip?.slot) ||
    attestation.cardanoTip.slot < 0
  ) {
    throw new Error("T1 recovery attestation has an invalid Cardano tip slot");
  }
  requireCardanoHash(attestation.cardanoTip.hash, "Cardano tip hash");
  if (
    typeof attestation.cardanoTip !== "object" ||
    attestation.cardanoTip === null ||
    JSON.stringify(Object.keys(attestation.cardanoTip).sort()) !==
      JSON.stringify(["hash", "slot"])
  ) {
    throw new Error(
      "T1 recovery Cardano tip fields do not match the exact schema",
    );
  }
  if (
    !Number.isSafeInteger(attestation.kupoCheckpoint) ||
    attestation.kupoCheckpoint !== attestation.cardanoTip.slot
  ) {
    throw new Error(
      "T1 recovery attestation does not bind synchronized Cardano and Kupo providers",
    );
  }
  if (
    attestation.recoveredTipHeaderHash === attestation.abandonedHeaderHash ||
    attestation.recoveredTipHeaderHash === attestation.baseHeaderHash
  ) {
    throw new Error(
      "T1 recovery attestation did not produce a distinct canonical F tip",
    );
  }
  return attestation;
};

export const writePhase4T1Evidence = async (
  path: string,
  evidence: Phase4T1ProbeEvidence | Phase4T1AdvanceEvidence,
): Promise<void> => {
  if (!path.startsWith("/")) {
    throw new Error("Phase 4 T1 evidence path must be absolute");
  }
  await writeTextFileAtomicNoReplace(
    path,
    `${JSON.stringify(evidence, null, 2)}\n`,
    { mode: 0o600 },
  );
};
