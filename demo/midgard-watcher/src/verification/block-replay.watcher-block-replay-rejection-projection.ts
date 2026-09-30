import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import type { MidgardValidationPhaseName } from "@al-ft/midgard-core/validation-trace";
import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk";
import type { RejectCode, RejectedTx } from "@al-ft/midgard-validation/types";
import { RejectCodes } from "@al-ft/midgard-validation/types";

import { type WatcherForcedOperatorVerdict } from "../indexers/user-event-indexer.js";
import {
  STAGE_ORDER,
  WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_DEPENDENCY_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_REFERENCE_DETAIL_PREFIXES,
  WATCHER_BLOCK_REPLAY_SCHEMA_VERSION,
  WATCHER_BLOCK_REPLAY_STAGE_BY_CONSENSUS_PHASE,
  WATCHER_BLOCK_REPLAY_VERIFIED_CONTRACT,
  type WatcherBlockReplayStage,
} from "./block-replay.watcher-block-replay-reason-codes.js";
import {
  digestResult,
  fail,
  orderReasonCodes,
  type WatcherBlockReplayReasonCode,
  type WatcherBlockReplayRejection,
  type WatcherBlockReplayResult,
  type WatcherBlockReplayStageMismatch,
} from "./block-replay.watcher-block-replay-result.js";
import {
  WATCHER_RULE_BUNDLE_REJECTION_SELECTION,
  WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY,
} from "./rule-bundle.js";

export type WatcherBlockReplayContext = Readonly<{
  headerHash: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
  reconstructionDigest: string;
  phaseAResultDigest: string;
  ruleBundleCommitment: string;
}>;

export const NULL_CONTEXT = {
  headerHash: null,
  payloadEnvelopeSha256: null,
  payloadSha256: null,
  reconstructionDigest: null,
  phaseAResultDigest: null,
  ruleBundleCommitment: null,
  authorityManifestDigest: null,
  sourceManifestDigest: null,
  effectManifestDigest: null,
} as const;

const EMPTY_CORE = {
  priorStateRoot: null,
  expectedPriorStateRoot: null,
  postStateRoot: null,
  expectedPostStateRoot: null,
  acceptedCount: 0,
  acceptedTxIds: Object.freeze([]),
  intermediateRoots: Object.freeze([]),
  transactionRoots: Object.freeze([]),
  eventRoots: Object.freeze([]),
  forcedValidationFacts: Object.freeze([]),
  stageMismatches: Object.freeze([]),
  rejections: Object.freeze([]),
  selectedRejection: null,
} as const;

export const errorResult = (
  reasonCodes: Iterable<WatcherBlockReplayReasonCode>,
  context: WatcherBlockReplayContext | null,
  transactionCount: number,
): WatcherBlockReplayResult =>
  digestResult({
    schemaVersion: WATCHER_BLOCK_REPLAY_SCHEMA_VERSION,
    action: "error",
    reasonCodes: orderReasonCodes(reasonCodes),
    verifiedRequires: WATCHER_BLOCK_REPLAY_VERIFIED_CONTRACT,
    rejectionSelection: WATCHER_RULE_BUNDLE_REJECTION_SELECTION,
    consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
    ...(context ?? NULL_CONTEXT),
    authorityManifestDigest: null,
    sourceManifestDigest: null,
    effectManifestDigest: null,
    ...EMPTY_CORE,
    transactionCount,
  });

// ---------------------------------------------------------------------------
// Canonical verdict projection
// ---------------------------------------------------------------------------

export const HEX_32 = /^[0-9a-f]{64}$/u;

const ZERO_ROOT = "00".repeat(32);

/**
 * The canonical validation-machine trie reports the empty tree as 32 zero
 * bytes, while the canonical PHAS helper reports it as
 * `EMPTY_MERKLE_TREE_ROOT`. Both are the canonical spelling of "no entries" in
 * their own module; this maps the former onto the latter so the two canonical
 * root sources are comparable at all. It is a spelling normalisation, not a
 * root computation.
 */
export const normalizeRootHex = (root: string): string =>
  root === ZERO_ROOT ? EMPTY_MERKLE_TREE_ROOT : root;

/**
 * Exact canonical rejection-to-forced-verdict partition used by forced-order
 * replay below.
 *
 * The class boundaries this partition has always published are unchanged;
 * #640 re-spells each one as the `RejectionReasonV1` constructor tag the
 * forced leaf now carries (`ForcedInclusionTxV1.verdict`). The one code the
 * node classifier phase-splits — `E_NATIVE_SCRIPT_INVALID` becomes
 * `WitnessNativeScriptFalse` when Phase A rejects and
 * `ExecutionNativeScriptFalse` when Phase B does — is split identically
 * here, so an exact-arm comparison against the authenticated leaf verdict
 * never flags an honest operator. Both arms bridge to the same frozen
 * rejection code, so nothing on-chain distinguishes them; the phase only
 * picks which tag the leaf carries.
 */
export const watcherBlockReplayForcedValidityForRejectCode = (
  code: RejectCode,
  phase: "phaseA" | "phaseB",
): WatcherForcedOperatorVerdict => {
  if (code === RejectCodes.InputNotFound) {
    return "InputNotFound";
  }
  if (
    code === RejectCodes.InvalidSignature ||
    code === RejectCodes.MissingRequiredWitness
  ) {
    return "AddressWitnessSignatureInvalid";
  }
  if (code === RejectCodes.NativeScriptInvalid) {
    return phase === "phaseA"
      ? "WitnessNativeScriptFalse"
      : "ExecutionNativeScriptFalse";
  }
  if (
    code === RejectCodes.PlutusScriptInvalid ||
    code === RejectCodes.PlutusEvaluationUnavailable
  ) {
    return "PlutusExecutionFailed";
  }
  if (code === RejectCodes.MinFee) {
    return "FeeBelowMinimum";
  }
  return "ValueNotPreserved";
};

/**
 * Attributes one canonical rejection to a replay stage.
 *
 * Nothing about the verdict is reinterpreted; this only chooses which stage
 * name the record carries. The dependency codes win first because the canonical
 * pipeline gives them the default `resolveInputs` phase even though they are
 * block-graph properties; the reference-input detail prefixes win next for the
 * same reason; everything else follows the canonical phase.
 */
export const watcherBlockReplayStageForRejection = (input: {
  readonly code: RejectCode;
  readonly consensusPhase: MidgardValidationPhaseName;
  readonly detail: string | null;
}): WatcherBlockReplayStage | null => {
  if (
    (
      WATCHER_BLOCK_REPLAY_DEPENDENCY_REJECT_CODES as readonly string[]
    ).includes(input.code)
  ) {
    return "dependencies";
  }
  const detail = input.detail;
  if (
    detail !== null &&
    WATCHER_BLOCK_REPLAY_REFERENCE_DETAIL_PREFIXES.some((prefix) =>
      detail.startsWith(prefix),
    )
  ) {
    return "references";
  }
  const stage: WatcherBlockReplayStage | undefined = (
    WATCHER_BLOCK_REPLAY_STAGE_BY_CONSENSUS_PHASE as Partial<
      Record<string, WatcherBlockReplayStage>
    >
  )[input.consensusPhase];
  return stage ?? null;
};

/**
 * Projects one canonical `RejectedTx` into the watcher's record shape.
 *
 * The code, phase, and detail are copied verbatim; the only additions are the
 * block position, the W23 phase priority, and the replay stage. Every failure
 * path throws, and the caller turns a throw into an error result, so an
 * unrecognisable canonical rejection can never become an acceptance.
 */
export const watcherBlockReplayRejectionProjection = (input: {
  readonly rejected: RejectedTx;
  readonly indexByTxId: ReadonlyMap<string, number>;
}): WatcherBlockReplayRejection => {
  const { rejected, indexByTxId } = input;
  const txId = rejected.txId.toString("hex");
  const index = indexByTxId.get(txId);
  if (index === undefined) {
    fail("rejection_tx_id_mismatch", `$.rejections[${txId}].txId`);
  }
  const code: string = rejected.code;
  if (
    !WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES.includes(code as RejectCode)
  ) {
    fail("unknown_reject_code", `$.rejections[${txId}].code`);
  }
  const consensusPhasePriority =
    WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY.indexOf(
      rejected.consensusPhase as MidgardValidationPhaseName,
    );
  if (consensusPhasePriority < 0) {
    fail("missing_rejection_stage", `$.rejections[${txId}].consensusPhase`);
  }
  const consensusPhase = rejected.consensusPhase as MidgardValidationPhaseName;
  const stage = watcherBlockReplayStageForRejection({
    code: rejected.code,
    consensusPhase,
    detail: rejected.detail,
  });
  if (stage === null) {
    fail("missing_rejection_stage", `$.rejections[${txId}].stage`);
  }
  return Object.freeze({
    index: index as number,
    txId,
    code: rejected.code,
    consensusPhase,
    consensusPhasePriority,
    stage: stage as WatcherBlockReplayStage,
    detail: rejected.detail,
  });
};

/**
 * `first_rejection_by_phase_then_program_counter_v1` at block scope: the lowest
 * canonical validation phase wins, and canonical block order breaks ties.
 * `rejections` is always sorted by ascending `index`, so a strict `<` already
 * implements the tie-break - an explicit clause would be unreachable.
 */
export const selectRejection = (
  rejections: readonly WatcherBlockReplayRejection[],
): WatcherBlockReplayRejection | null => {
  let selected: WatcherBlockReplayRejection | null = null;
  for (const rejection of rejections) {
    if (
      selected === null ||
      rejection.consensusPhasePriority < selected.consensusPhasePriority
    ) {
      selected = rejection;
    }
  }
  return selected;
};

export const orderStageMismatches = (
  mismatches: readonly WatcherBlockReplayStageMismatch[],
): readonly WatcherBlockReplayStageMismatch[] =>
  Object.freeze(
    [...mismatches].sort((left, right) => {
      const stageDelta =
        (STAGE_ORDER.get(left.stage) ?? 0) -
        (STAGE_ORDER.get(right.stage) ?? 0);
      if (stageDelta !== 0) {
        return stageDelta;
      }
      return left.field < right.field ? -1 : left.field > right.field ? 1 : 0;
    }),
  );

// ---------------------------------------------------------------------------
// Prior state
// ---------------------------------------------------------------------------

/** A prior-state ledger entry as the W21 store holds it: hex out-ref and output. */
export type WatcherBlockReplayPriorUtxo = Readonly<{
  outRef: string;
  outputCbor: string;
}>;

export const HEX_BYTES = /^(?:[0-9a-f]{2})+$/u;
