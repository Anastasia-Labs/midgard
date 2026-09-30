import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import type { MidgardValidationPhaseName } from "@al-ft/midgard-core/validation-trace";
import type { EventKey } from "@al-ft/midgard-sdk";
import { type CanonicalTransitionEffect } from "@al-ft/midgard-validation";
import type { RejectCode } from "@al-ft/midgard-validation/types";

import {
  type WatcherForcedOperatorVerdict,
  type WatcherLocalUserEventAuthority,
} from "../indexers/user-event-indexer.js";
import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import {
  WATCHER_BLOCK_REPLAY_DOWNSTREAM_PREREQUISITE_SCHEMA_VERSION,
  WATCHER_BLOCK_REPLAY_REASON_CODES,
  WATCHER_BLOCK_REPLAY_SCHEMA_VERSION,
  WATCHER_BLOCK_REPLAY_VERIFIED_CONTRACT,
  type WatcherBlockReplayStage,
} from "./block-replay.watcher-block-replay-reason-codes.js";
import { WATCHER_RULE_BUNDLE_REJECTION_SELECTION } from "./rule-bundle.js";

export type WatcherBlockReplayReasonCode =
  (typeof WATCHER_BLOCK_REPLAY_REASON_CODES)[number];

export class WatcherBlockReplayError extends Error {
  readonly code: WatcherBlockReplayReasonCode;
  readonly path: string;

  constructor(code: WatcherBlockReplayReasonCode, path: string) {
    super(`${code}: ${path}`);
    this.name = "WatcherBlockReplayError";
    this.code = code;
    this.path = path;
  }
}

export const fail = (
  code: WatcherBlockReplayReasonCode,
  path: string,
): never => {
  throw new WatcherBlockReplayError(code, path);
};

export const reasonCodeOf = (error: unknown): WatcherBlockReplayReasonCode =>
  error instanceof WatcherBlockReplayError
    ? error.code
    : "canonical_replay_threw";

export const orderReasonCodes = (
  codes: Iterable<WatcherBlockReplayReasonCode>,
): readonly WatcherBlockReplayReasonCode[] => {
  const present = new Set<string>(codes);
  return Object.freeze(
    WATCHER_BLOCK_REPLAY_REASON_CODES.filter((code) => present.has(code)),
  );
};

// ---------------------------------------------------------------------------
// Result shape
// ---------------------------------------------------------------------------

export type WatcherBlockReplayRejection = Readonly<{
  /** Position in the canonical block transaction order. */
  index: number;
  /** 32-byte canonical transaction id, lowercase hex. */
  txId: string;
  /** Exact canonical `RejectCode`, copied unchanged. */
  code: RejectCode;
  /** Exact canonical `consensusPhase`, copied unchanged. */
  consensusPhase: MidgardValidationPhaseName;
  /** Index of `consensusPhase` in the W23 validation phase priority. */
  consensusPhasePriority: number;
  /** Replay stage this rejection is attributed to. */
  stage: WatcherBlockReplayStage;
  /** Exact canonical `detail`, copied unchanged. */
  detail: string | null;
}>;

/** One canonical ledger mutation and the roots it moved between. */
export type WatcherBlockReplayIntermediateRoot = Readonly<{
  /** Position in the replay's total mutation order, from zero. */
  sequence: number;
  /** Index of the accepted transaction, or null for a non-L2 event. */
  txIndex: number | null;
  txId: string | null;
  /** Authenticated transition step, null for candidate-only replay. */
  stepIndex: number | null;
  phase: WatcherBlockReplayCommittedStep["phase"] | null;
  operation: "delete" | "insert";
  outRef: string;
  preRoot: string;
  postRoot: string;
}>;

/** The canonical state boundary around one replayed transaction.
 * Rejected committed transactions retain an exact zero-mutation boundary. */
export type WatcherBlockReplayTransactionRoot = Readonly<{
  /** Position in the canonical block transaction order. */
  txIndex: number;
  txId: string;
  preRoot: string;
  postRoot: string;
  mutationCount: number;
  /**
   * The operator's committed transition-trace step for this transaction, or
   * null when the replay was run without a committed trace (candidate-level
   * evaluation).
   */
  committedStepIndex: number | null;
  committedPreRoot: string | null;
  committedPostRoot: string | null;
}>;

export type WatcherBlockReplayForcedValidationFact = Readonly<{
  eventKeyFingerprint: string;
  stepIndex: number;
  authenticatedOperatorValidity: WatcherForcedOperatorVerdict;
  canonicalOperatorValidity: WatcherForcedOperatorVerdict;
  phaseAStatus: "accepted" | "rejected";
  phaseARejectCode: RejectCode | null;
  phaseBStatus: "not_run" | "accepted" | "rejected";
  phaseBRejectCode: RejectCode | null;
  canonicalEffectDigest: string;
  canonicalEffectMutationCount: number;
}>;

export type WatcherBlockReplayStageMismatch = Readonly<{
  stage: WatcherBlockReplayStage;
  reasonCode: WatcherBlockReplayReasonCode;
  /** Stable JSON-path-like locator for the diverging value. */
  field: string;
  expected: string;
  actual: string;
}>;

export type WatcherBlockReplayAction = "accept" | "reject" | "error";

export type WatcherBlockReplayDownstreamPrerequisite = Readonly<{
  schemaVersion: typeof WATCHER_BLOCK_REPLAY_DOWNSTREAM_PREREQUISITE_SCHEMA_VERSION;
  requiredVerifier: "W26";
  inputDigest: string;
  w29Eligibility: "requires_w26_accept";
}>;

export type WatcherBlockReplayResult = Readonly<{
  schemaVersion: typeof WATCHER_BLOCK_REPLAY_SCHEMA_VERSION;
  action: WatcherBlockReplayAction;
  reasonCodes: readonly WatcherBlockReplayReasonCode[];
  /** The W29 contract, carried with the record. */
  verifiedRequires: typeof WATCHER_BLOCK_REPLAY_VERIFIED_CONTRACT;
  downstreamPrerequisite: WatcherBlockReplayDownstreamPrerequisite;
  /** W23 rejection-selection rule that produced `selectedRejection`. */
  rejectionSelection: typeof WATCHER_RULE_BUNDLE_REJECTION_SELECTION;
  consensusProfileId: typeof MIDGARD_CONSENSUS_PROFILE_ID;
  headerHash: string | null;
  payloadEnvelopeSha256: string | null;
  payloadSha256: string | null;
  reconstructionDigest: string | null;
  phaseAResultDigest: string | null;
  ruleBundleCommitment: string | null;
  authorityManifestDigest: string | null;
  sourceManifestDigest: string | null;
  effectManifestDigest: string | null;
  /** PHAS root of the supplied prior state, recomputed canonically. */
  priorStateRoot: string | null;
  /** The L1-committed `prevUtxosRoot`, or null with no block context. */
  expectedPriorStateRoot: string | null;
  /** Root after the last canonical ledger mutation of the replay. */
  postStateRoot: string | null;
  /** The L1-committed `utxosRoot`, or null with no block context. */
  expectedPostStateRoot: string | null;
  transactionCount: number;
  acceptedCount: number;
  /** Accepted transaction ids in canonical accepted order. */
  acceptedTxIds: readonly string[];
  intermediateRoots: readonly WatcherBlockReplayIntermediateRoot[];
  transactionRoots: readonly WatcherBlockReplayTransactionRoot[];
  /** Exact canonical root boundary around every authenticated non-L2 event. */
  eventRoots: readonly WatcherBlockReplayEventRoot[];
  /** Canonical validity/effect evidence for forced steps, in step order. */
  forcedValidationFacts: readonly WatcherBlockReplayForcedValidationFact[];
  /** Ordered by replay stage, then by `field`. */
  stageMismatches: readonly WatcherBlockReplayStageMismatch[];
  /** Rejections in canonical block order (ascending `index`). */
  rejections: readonly WatcherBlockReplayRejection[];
  /** The W23-priority first fault: lowest phase priority, then `index`. */
  selectedRejection: WatcherBlockReplayRejection | null;
  resultDigest: string;
}>;

export const admittedFullBlockReplayResults = new WeakSet<object>();

/**
 * Production replay artifacts may consume only a result minted by the full
 * W21/W22/W23/W24-bound entry point below. A digest-correct structural clone
 * is durable evidence, not live replay authority.
 */
export const assertWatcherFullBlockReplayResult = (
  result: WatcherBlockReplayResult,
): void => {
  if (!admittedFullBlockReplayResults.has(result)) {
    throw new Error("watcher full block-replay result is not admitted");
  }
};

export const admitFullBlockReplayResult = (
  result: WatcherBlockReplayResult,
): WatcherBlockReplayResult => {
  admittedFullBlockReplayResults.add(result);
  return result;
};

export const watcherBlockReplayDownstreamInputDigest = (
  result: Pick<
    WatcherBlockReplayResult,
    | "headerHash"
    | "payloadEnvelopeSha256"
    | "reconstructionDigest"
    | "phaseAResultDigest"
    | "ruleBundleCommitment"
    | "authorityManifestDigest"
    | "sourceManifestDigest"
    | "effectManifestDigest"
    | "forcedValidationFacts"
    | "priorStateRoot"
    | "postStateRoot"
  >,
): string =>
  watcherSha256CanonicalJson({
    headerHash: result.headerHash,
    payloadEnvelopeSha256: result.payloadEnvelopeSha256,
    reconstructionDigest: result.reconstructionDigest,
    phaseAResultDigest: result.phaseAResultDigest,
    ruleBundleCommitment: result.ruleBundleCommitment,
    authorityManifestDigest: result.authorityManifestDigest,
    sourceManifestDigest: result.sourceManifestDigest,
    effectManifestDigest: result.effectManifestDigest,
    forcedValidationFacts: result.forcedValidationFacts,
    priorStateRoot: result.priorStateRoot,
    postStateRoot: result.postStateRoot,
  });

export const digestResult = (
  result: Omit<
    WatcherBlockReplayResult,
    "resultDigest" | "downstreamPrerequisite"
  >,
): WatcherBlockReplayResult =>
  Object.freeze(
    (() => {
      const inputDigest = watcherBlockReplayDownstreamInputDigest(result);
      const downstreamPrerequisite = Object.freeze({
        schemaVersion:
          WATCHER_BLOCK_REPLAY_DOWNSTREAM_PREREQUISITE_SCHEMA_VERSION,
        requiredVerifier: "W26" as const,
        inputDigest,
        w29Eligibility: "requires_w26_accept" as const,
      });
      const withPrerequisite = { ...result, downstreamPrerequisite };
      return {
        ...withPrerequisite,
        resultDigest: watcherSha256CanonicalJson(withPrerequisite),
      };
    })(),
  );

/** The committed transition-trace step material the events stage binds. */
export type WatcherBlockReplayCommittedStep = Readonly<{
  stepIndex: number;
  phase: "Withdrawal" | "ForcedTransaction" | "L2Transaction" | "Deposit";
  /** L2 transaction id when `phase` is `L2Transaction`, else null. */
  txId: string | null;
  /** Canonical identity of the event committed at this step. */
  eventKeyFingerprint: string;
  preRoot: string;
  postRoot: string;
  /** `step_index` the `event_to_step` entry for this event points at. */
  eventToStepIndex: number | null;
  /** `phase` the `event_to_step` entry for this event carries. */
  eventToStepPhase: string | null;
}>;

/**
 * An event effect requires originating authority from private local
 * user-event publication, plus the exact operator claim authenticated by the
 * block DA roots. Settlement accounting is never a prerequisite for
 * challenge-period replay.
 */
export type WatcherBlockReplayEventAuthority = Readonly<
  {
    eventKey: EventKey;
    localUserEvent: WatcherLocalUserEventAuthority;
  } & (
    | {
        phase: "Withdrawal" | "Deposit";
        transitionEffect: CanonicalTransitionEffect;
        canonicalNativeTxCbor?: never;
        programMaterialSidecarCbor?: never;
      }
    | {
        phase: "ForcedTransaction";
        /** Exact bytes compact/commitment-bound to the originating user-event order. */
        canonicalNativeTxCbor: Uint8Array;
        /** Canonical Phase-A program material for the forced native transaction. */
        programMaterialSidecarCbor?: Uint8Array | null;
        /** W25 derives the effect against its current canonical ledger. */
        transitionEffect?: never;
      }
  )
>;

export type WatcherBlockReplayEventRoot = Readonly<{
  stepIndex: number;
  phase: WatcherBlockReplayEventAuthority["phase"];
  eventKeyFingerprint: string;
  preRoot: string;
  postRoot: string;
  mutationCount: number;
}>;
