/**
 * Phase A verifier (GOAL_SPEC 10.3 W24).
 *
 * This module is a THIN ADAPTER. It runs under the recorded CG3 waiver whose
 * binding conditions are reproduced here because they are the only reason the
 * module is allowed to exist at all:
 *
 * 1. CANONICAL SEMANTICS ONLY. Every accept/reject decision is produced by
 *    `validatePhaseASingle` from `@al-ft/midgard-validation/phase-a`. There is
 *    not one watcher-authored validation predicate in this file: no size
 *    check, no count check, no signature check, no fee check, no field
 *    inspection that could change a verdict. Search this file for a comparison
 *    against a protocol limit and you will not find one. What the watcher owns
 *    is (a) the derivation of the canonical `QueuedTx` inputs from bytes the
 *    canonical reconstruction already authenticated, (b) the binding of those
 *    bytes to W21/W22/W23, and (c) a deterministic, digest-bound projection of
 *    the canonical verdicts. If a check is not reachable through the canonical
 *    entry point it is a recorded residual, never a local reimplementation.
 *
 *    `runPhaseAValidation` (phase-a.ts:556) is the batch form of exactly the
 *    same function - `Effect.forEach(validatePhaseASingle)` plus an
 *    accepted/rejected partition. It is deliberately NOT used here: it returns
 *    an `Effect`, and `effect` is not a `midgard-watcher` dependency. Adding
 *    one is outside this lane's path lease, and the per-transaction entry
 *    point is the same canonical function with the same arguments, so nothing
 *    about the semantics differs.
 *
 * 2. PUBLISHED REJECTION VOCABULARY. The canonical 50-member `RejectCodes`
 *    vocabulary (`@al-ft/midgard-validation/types`) is partitioned below into
 *    `WATCHER_PHASE_A_REACHABLE_REJECT_CODES` (32) and
 *    `WATCHER_PHASE_A_EXCLUDED_REJECT_CODES` (18), each excluded code
 *    carrying a one-line justification in
 *    `WATCHER_PHASE_A_EXCLUDED_REJECT_CODE_JUSTIFICATIONS`. The partition is
 *    derived from the canonical source, not invented: the reachable set is
 *    exactly the union of the codes `validatePhaseASingle` can return directly
 *    (13, phase-a.ts:412-545) and the codes `consensusProfileRejectCode`
 *    (phase-a.ts:73-116) maps the 19 `MidgardConsensusV1ViolationCode` values
 *    onto. The partition is enforced as a fail-closed guard, never as a filter:
 *    a canonical code outside the vocabulary is an error result, and a
 *    canonical code inside the vocabulary but outside the declared reachable
 *    set is still reported as a rejection, with a reason code recording that
 *    the published table needs updating. No code path can turn a canonical
 *    rejection into an acceptance.
 *
 * NON-CIRCULAR INPUT BINDING. The transaction bytes are never taken from a
 * caller-supplied list. They come from `canonicalBlockEvidenceFromVerifiedPayload`
 * (the Q03 evidence core), which re-admits the L1-authenticated header
 * observation, re-derives every root, and authenticates each
 * `transaction_preimages` entry against its `transactions` source commitment
 * before this module sees it. The W22 record supplied by the caller is
 * re-checked against that recomputation (digest, header hash, payload
 * envelope sha256) and must itself be an `accept`; the W23 rule bundle must
 * carry the exact compiled V1 consensus profile; and the Phase A configuration
 * (`expectedNetworkId`, `minFeeA`, `minFeeB`) is read from the L1-committed
 * header, never from the DA payload's own claim.
 *
 * FAIL-CLOSED. Every binding failure, decode failure, canonical throw, or
 * bookkeeping inconsistency produces `action: "error"` with a deterministic
 * reason code. `action: "accept"` is reachable only when the canonical
 * validator accepted every transaction in the block.
 *
 * Like the finality engine and W22, the result is frozen, versioned, and
 * digest-bound with `watcherSha256CanonicalJson`, so two runs over the same
 * bytes produce the same `resultDigest`.
 */

import "@al-ft/midgard-core/cek-proof";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-core/codec/native";
import "@al-ft/midgard-core/consensus-profile";
import "@al-ft/midgard-core/script-proof";
import "@al-ft/midgard-fault-proofs";
import "@al-ft/midgard-validation/phase-a";
import "@al-ft/midgard-validation/types";
import "../storage/durable-store.js";
import "./header-root-reconstruction.js";
import "./rule-bundle.js";
import "./phase-a-verifier.watcher-phase-a-excluded-reject-code-justifications.js";
import "./phase-a-verifier.project-program-material-sidecar.js";
import "./phase-a-verifier.evaluate-watcher-phase-aqueued-txs.js";
export {
  evaluateWatcherPhaseABlock,
  type EvaluateWatcherPhaseABlockInput,
  evaluateWatcherPhaseAQueuedTxs,
  type WatcherPhaseABlockContext,
  watcherPhaseAQueuedTxs,
} from "./phase-a-verifier.evaluate-watcher-phase-aqueued-txs.js";
export {
  makeWatcherPhaseAConfig,
  WATCHER_PHASE_A_CONCURRENCY,
  WATCHER_PHASE_A_CREATED_AT,
  type WatcherPhaseABlockTransaction,
  watcherPhaseARejectionProjection,
  type WatcherPhaseAVerificationResult,
} from "./phase-a-verifier.project-program-material-sidecar.js";
export {
  WATCHER_PHASE_A_CANONICAL_REJECT_CODES,
  WATCHER_PHASE_A_CONSENSUS_REJECT_CODES,
  WATCHER_PHASE_A_DIRECT_REJECT_CODES,
  WATCHER_PHASE_A_DOMINATED_REJECT_CODE_JUSTIFICATIONS,
  WATCHER_PHASE_A_DOMINATED_REJECT_CODES,
  WATCHER_PHASE_A_EVIDENCED_REJECT_CODES,
  WATCHER_PHASE_A_EXCLUDED_REJECT_CODE_JUSTIFICATIONS,
  WATCHER_PHASE_A_EXCLUDED_REJECT_CODES,
  WATCHER_PHASE_A_REACHABLE_REJECT_CODES,
  WATCHER_PHASE_A_VERIFIER_REASON_CODES,
  WATCHER_PHASE_A_VERIFIER_SCHEMA_VERSION,
  type WatcherPhaseARejection,
  WatcherPhaseAVerifierError,
  type WatcherPhaseAVerifierReasonCode,
} from "./phase-a-verifier.watcher-phase-a-excluded-reject-code-justifications.js";
