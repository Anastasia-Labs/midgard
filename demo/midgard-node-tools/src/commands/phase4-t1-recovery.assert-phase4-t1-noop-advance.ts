import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { positiveSafeInteger } from "midgard-node/artifact-schema";
import { exactObjectKeys } from "midgard-node/exact-object-keys";
import {
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "midgard-node/services/index";
import { commitExplicitBlockHeaderProgram } from "midgard-node/workers/commit-block-header";

import {
  assertPhase4T1Gate,
  CARDANO_HASH,
  CARDANO_OUT_REF,
  L2_HEADER_HASH,
  PHASE4_T1_ADVANCE_SCHEMA,
  PHASE4_T1_RECOVERY_SCHEMA,
  type Phase4T1Gate,
  type Phase4T1ProbeEvidence,
  requireCardanoHash,
  requireL2HeaderHash,
  SAFE_ATTEMPT_ID,
  SHA256,
} from "./phase4-t1-recovery.decode-phase4-t1-canonical-tip.js";
import {
  decodePhase4T1NoopAdvanceAssertion,
  decodePhase4T1ProbeEvidence,
  type Phase4T1NoopAdvanceAssertion,
  phase4T1ProbeProgram,
} from "./phase4-t1-recovery.fetch-phase4-t1-canonical-state.js";

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
  return decodePhase4T1NoopAdvanceAssertion({
    baseHeaderHash: expectedBase,
    recoveredTipHeaderHash: recovered.headerHash,
    abandonedHeaderHash: abandoned,
    baseEndTimeMs: before.canonicalTip.endTimeMs,
    recoveredEndTimeMs: recovered.endTimeMs,
    minimumRecoveredEndTimeMs: minimumEndTimeMs,
    rootsPreserved: true,
    transitionIsEmpty: true,
  });
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

export const decodePhase4T1AdvanceEvidence = (
  value: unknown,
): Phase4T1AdvanceEvidence => {
  if (
    !exactObjectKeys(value, [
      "schemaVersion",
      "snapshotIdentitySha256",
      "attemptId",
      "abandonedHeaderHash",
      "before",
      "submittedTxHash",
      "recoveredTipHeaderHash",
      "blockOutRef",
      "txSize",
      "blockEndTimeMs",
      "after",
      "invariants",
    ])
  ) {
    throw new Error(
      "Phase 4 T1 canonical advance fields do not match the exact V1 schema",
    );
  }
  if (
    value.schemaVersion !== PHASE4_T1_ADVANCE_SCHEMA ||
    typeof value.snapshotIdentitySha256 !== "string" ||
    !SHA256.test(value.snapshotIdentitySha256) ||
    typeof value.attemptId !== "string" ||
    !SAFE_ATTEMPT_ID.test(value.attemptId) ||
    typeof value.abandonedHeaderHash !== "string" ||
    !L2_HEADER_HASH.test(value.abandonedHeaderHash) ||
    typeof value.submittedTxHash !== "string" ||
    !CARDANO_HASH.test(value.submittedTxHash) ||
    typeof value.recoveredTipHeaderHash !== "string" ||
    !L2_HEADER_HASH.test(value.recoveredTipHeaderHash) ||
    typeof value.blockOutRef !== "string" ||
    !CARDANO_OUT_REF.test(value.blockOutRef) ||
    !Number.isSafeInteger(value.txSize) ||
    (value.txSize as number) <= 0 ||
    !Number.isSafeInteger(value.blockEndTimeMs) ||
    (value.blockEndTimeMs as number) <= 0
  ) {
    throw new Error(
      "Phase 4 T1 canonical advance contains a noncanonical V1 value",
    );
  }
  const before = decodePhase4T1ProbeEvidence(value.before);
  const after = decodePhase4T1ProbeEvidence(value.after);
  const invariants = decodePhase4T1NoopAdvanceAssertion(value.invariants);
  if (
    before.snapshotIdentitySha256 !== value.snapshotIdentitySha256 ||
    after.snapshotIdentitySha256 !== value.snapshotIdentitySha256 ||
    before.attemptId !== value.attemptId ||
    after.attemptId !== value.attemptId ||
    value.recoveredTipHeaderHash !== after.canonicalTip.headerHash ||
    value.blockEndTimeMs !== after.canonicalTip.endTimeMs ||
    invariants.baseHeaderHash !== before.canonicalTip.headerHash ||
    invariants.recoveredTipHeaderHash !== value.recoveredTipHeaderHash ||
    invariants.abandonedHeaderHash !== value.abandonedHeaderHash
  ) {
    throw new Error(
      "Phase 4 T1 canonical advance is not bound to its nested evidence",
    );
  }
  return value as Phase4T1AdvanceEvidence;
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
    positiveSafeInteger(options.minimumEndTimeMs, "minimumEndTimeMs");
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
    return decodePhase4T1AdvanceEvidence({
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
    });
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
