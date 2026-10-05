/**
 * Phase B / block replay (GOAL_SPEC 10.3 W25).
 *
 * W25 answers one question for a single state-queue block: starting from the
 * prior ledger state the L1 header commits to, does replaying the block's
 * public payload reproduce *exactly* the intermediate roots and the post-state
 * root the operator committed? The answer must be produced by canonical code,
 * must be deterministic, and must never be "yes" by omission.
 *
 * This module is a THIN ADAPTER and runs under the same recorded CG3 waiver as
 * W24. Its binding conditions are reproduced here because they are the only
 * reason the module is allowed to exist:
 *
 * 1. CANONICAL SEMANTICS ONLY. Every accept/reject verdict comes from
 *    `runPhaseBValidationWithPatch` (`@al-ft/midgard-validation/phase-b`).
 *    Every intermediate ledger root comes from
 *    `buildValidationMachineLedgerMutationSteps`
 *    (`@al-ft/midgard-validation`, the canonical validation-machine ledger
 *    mutator that the deployed resolver chain consumes). Every set-level root
 *    comes from `keyValuePhasRootWithCount` (`@al-ft/midgard-fault-proofs`),
 *    and every ledger descriptor from
 *    `buildCanonicalMidgardLedgerEntryOutputMaterial`. There is not one
 *    watcher-authored validation predicate in this file: no spend check, no
 *    value equation, no script rule, no interval comparison, no dependency
 *    algorithm. The watcher owns (a) the derivation of the canonical inputs
 *    from bytes the canonical reconstruction already authenticated, (b) the
 *    binding of those bytes to W21/W22/W23/W24, and (c) a deterministic,
 *    digest-bound projection of the canonical verdicts and roots.
 *
 *    `runPhaseBValidationWithPatch` returns an `Effect`, and `effect` is not a
 *    `midgard-watcher` dependency. It is run through `makeReturn(...).unsafeRun`
 *    from `@al-ft/midgard-sdk` - a declared watcher dependency whose only job
 *    here is to supply the canonical runtime. No watcher-side interpreter, no
 *    dynamic module resolution, and no change to any package manifest.
 *
 * 2. PUBLISHED REJECTION VOCABULARY. The canonical 50-member `RejectCodes`
 *    vocabulary is partitioned below into the 13 codes the canonical Phase B
 *    pipeline can emit (`WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES`), the
 *    27 codes W24's Phase A verifier owns
 *    (`WATCHER_BLOCK_REPLAY_PHASE_A_OWNED_REJECT_CODES`), and the 10 codes
 *    neither lane claims (`WATCHER_BLOCK_REPLAY_UNCLAIMED_REJECT_CODES`),
 *    each of the latter carrying a one-line justification. The three groups are
 *    a partition of the 50, so the W24 + W25 union is provably total: every
 *    canonical code is claimed by exactly one lane or explicitly and
 *    individually disclaimed by both.
 *
 *    The partition is derived from the canonical source, not invented: the
 *    reachable set is exactly the union of the codes reachable from a
 *    `reject(...)` call site in phase-b.ts. It is enforced as a fail-closed
 *    guard, never as a filter: a canonical code outside the vocabulary is an
 *    error result, and a canonical code inside the vocabulary but outside the
 *    declared reachable set is still reported as a rejection, with a reason
 *    code recording that the published table needs updating.
 *
 * WHAT `verified` REQUIRES. W29 may map this block to `verified` only on
 * `action: "accept"`, and `WATCHER_BLOCK_REPLAY_VERIFIED_CONTRACT` is
 * carried inside every result so the contract travels with the record. A
 * non-L2 step is applied from its locally published user event and DA
 * claim-bound effect in the exact W22-authenticated transition order, and
 * every resulting root is checked against the committed trace. W25 proves
 * root-exact replay only; it does not classify an event as due, omitted,
 * fabricated or duplicated. The W26 classification verifier that the
 * carried contract names has been removed (see
 * docs/midgard/decisions/watcher-external-provider-evaluator-removal.md); the
 * contract text is unchanged because it is part of the durable result format.
 *
 * NON-CIRCULAR INPUT BINDING. The transaction bytes are never taken from a
 * caller-supplied list: they come from
 * `canonicalBlockEvidenceFromVerifiedPayload` (the Q03 evidence core), which
 * re-admits the L1-authenticated header observation, re-derives every root, and
 * authenticates each `transaction_preimages` entry before this module sees it.
 * The W22 record and the W24 record supplied by the caller are both re-checked
 * against that recomputation (digest, header hash, payload envelope sha256) and
 * must themselves be acceptances. The prior state is not trusted either: its
 * canonical PHAS root must equal the L1-committed `prevUtxosRoot` *before* any
 * transaction is replayed.
 *
 * FAIL-CLOSED. Every binding failure, decode failure, canonical throw, or
 * bookkeeping inconsistency produces `action: "error"` with a deterministic
 * reason code. Like W22 and W24, the result is frozen, versioned, and
 * digest-bound with `watcherSha256CanonicalJson`, so two runs over the same
 * bytes produce the same `resultDigest`.
 *
 * An *unrun* binding is fail-closed too, which is a separate statement from
 * the one above (#517). Acceptance compares the recomputed roots against the
 * operator's committed values in exactly two places - the committed
 * transition trace and the header's `utxosRoot` - and both are conditional on
 * the caller supplying that committed material. `finalizeResult` therefore
 * gates `accept` on a receipt from each binding rather than on the absence of
 * reason codes, and reports `committed_trace_binding_unrun` /
 * `post_state_binding_unrun` for whichever did not run. The candidate-level
 * entry point (`evaluateWatcherBlockReplayCandidates`), which has no
 * committed trace by construction, consequently can never return `accept`: it
 * is an evaluation surface, not an acceptance authority.
 */

import "node:crypto";
import "@al-ft/midgard-core/codec";
import "@al-ft/midgard-core/codec/cbor";
import "@al-ft/midgard-core/consensus-profile";
import "@al-ft/midgard-core/plutus-data-cbor";
import "@al-ft/midgard-core/validation-trace";
import "@al-ft/midgard-fault-proofs";
import "@al-ft/midgard-sdk";
import "@al-ft/midgard-validation";
import "@al-ft/midgard-validation/ledger";
import "@al-ft/midgard-validation/phase-a";
import "@al-ft/midgard-validation/phase-b";
import "@al-ft/midgard-validation/types";
import "@lucid-evolution/lucid";
import "../indexers/user-event-indexer.js";
import "../storage/durable-store.js";
import "./event-claims.js";
import "./header-root-reconstruction.js";
import "./history-original-assets.js";
import "./phase-a-verifier.js";
import "./rule-bundle.js";
import "./block-replay.watcher-block-replay-reason-codes.js";
import "./block-replay.watcher-block-replay-result.js";
import "./block-replay.watcher-block-replay-rejection-projection.js";
import "./block-replay.watcher-block-replay-prior-state.js";
import "./block-replay.validate-event-authority.js";
import "./block-replay.replay-candidates.js";
import "./block-replay.bind-committed-steps.js";
import "./block-replay.replay-forced-transition-effect.js";
import "./block-replay.replay-committed-block.js";
import "./block-replay.watcher-block-replay-committed-steps.js";
import "./block-replay.evaluate-watcher-block-replay.js";
import "./block-replay.make-watcher-block-replay-reconstructed-state.js";
export { evaluateWatcherBlockReplayCandidates } from "./block-replay.bind-committed-steps.js";
export {
  evaluateWatcherBlockReplay,
  WatcherBlockReplayRecordError,
  type WatcherBlockReplayRecordErrorCode,
} from "./block-replay.evaluate-watcher-block-replay.js";
export {
  makeWatcherBlockReplayReconstructedState,
  WATCHER_BLOCK_REPLAY_CANONICAL_PHASES,
  WATCHER_BLOCK_REPLAY_CLAIMED_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_PROTOCOL_MINUS_UNCLAIMED,
} from "./block-replay.make-watcher-block-replay-reconstructed-state.js";
export { type EvaluateWatcherBlockReplayCandidatesInput } from "./block-replay.validate-event-authority.js";
export {
  type EvaluateWatcherBlockReplayInput,
  snapshotWatcherBlockReplayEventAuthorities,
  watcherBlockReplayCommittedSteps,
} from "./block-replay.watcher-block-replay-committed-steps.js";
export {
  makeWatcherPhaseBConfig,
  readWatcherBlockReplayEventAuthorityRecords,
  WATCHER_BLOCK_REPLAY_BUCKET_CONCURRENCY,
  type WatcherBlockReplayEffectRecord,
  watcherBlockReplayEventAuthorityManifest,
  type WatcherBlockReplayEventAuthorityRecord,
  type WatcherBlockReplayEventOriginRecord,
  watcherBlockReplayPriorState,
} from "./block-replay.watcher-block-replay-prior-state.js";
export {
  WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_DEPENDENCY_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_DOMINATED_REJECT_CODE_JUSTIFICATIONS,
  WATCHER_BLOCK_REPLAY_DOMINATED_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_DOWNSTREAM_PREREQUISITE_SCHEMA_VERSION,
  WATCHER_BLOCK_REPLAY_EVIDENCED_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_PHASE_A_OWNED_JUSTIFICATION,
  WATCHER_BLOCK_REPLAY_PHASE_A_OWNED_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_REASON_CODES,
  WATCHER_BLOCK_REPLAY_REFERENCE_DETAIL_PREFIXES,
  WATCHER_BLOCK_REPLAY_SCHEMA_VERSION,
  WATCHER_BLOCK_REPLAY_STAGE_BY_CONSENSUS_PHASE,
  WATCHER_BLOCK_REPLAY_STAGES,
  WATCHER_BLOCK_REPLAY_UNCLAIMED_REJECT_CODE_JUSTIFICATIONS,
  WATCHER_BLOCK_REPLAY_UNCLAIMED_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_VERIFIED_CONTRACT,
  type WatcherBlockReplayStage,
} from "./block-replay.watcher-block-replay-reason-codes.js";
export {
  type WatcherBlockReplayContext,
  type WatcherBlockReplayPriorUtxo,
  watcherBlockReplayRejectionProjection,
  watcherBlockReplayStageForRejection,
} from "./block-replay.watcher-block-replay-rejection-projection.js";
export {
  assertWatcherFullBlockReplayResult,
  type WatcherBlockReplayAction,
  type WatcherBlockReplayCommittedStep,
  watcherBlockReplayDownstreamInputDigest,
  type WatcherBlockReplayDownstreamPrerequisite,
  WatcherBlockReplayError,
  type WatcherBlockReplayEventAuthority,
  type WatcherBlockReplayEventRoot,
  type WatcherBlockReplayForcedValidationFact,
  type WatcherBlockReplayIntermediateRoot,
  type WatcherBlockReplayReasonCode,
  type WatcherBlockReplayRejection,
  type WatcherBlockReplayResult,
  type WatcherBlockReplayStageMismatch,
  type WatcherBlockReplayTransactionRoot,
} from "./block-replay.watcher-block-replay-result.js";
