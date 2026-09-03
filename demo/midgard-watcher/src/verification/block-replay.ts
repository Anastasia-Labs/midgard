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
 * non-L2 step is applied from its complete W15/W16 authority-derived effect in
 * the exact W22-authenticated transition order, and every resulting root is
 * checked against the committed trace. W25 proves root-exact replay only;
 * W26 remains responsible for due/omitted/fabricated/duplicate event
 * classification before the decision engine can treat the block as ready.
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

import { createHash } from "node:crypto";

import {
  computeMidgardNativeTxId,
  computeMidgardNativeTxProofCommitment,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core/codec";
import {
  encodeCborArrayRaw,
  encodeCborBytes,
} from "@al-ft/midgard-core/codec/cbor";
import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile-v1";
import type { MidgardValidationPhaseName } from "@al-ft/midgard-core/validation-trace";
import { MidgardValidationPhase } from "@al-ft/midgard-core/validation-trace";
import {
  canonicalBlockEvidenceFromVerifiedPayload,
  keyValuePhasRootWithCount,
} from "@al-ft/midgard-fault-proofs";
import type {
  AuthenticatedStateQueueHeaderObservation,
  EventKey,
  EventToStepValue,
  EvidenceProvenance,
  Header,
  TransitionStep,
} from "@al-ft/midgard-sdk";
import {
  DepositEvent,
  EMPTY_MERKLE_TREE_ROOT,
  makeReturn,
  OutputReference,
  TxOrderEvent,
  WithdrawalEvent,
} from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  buildCanonicalTransitionEffect,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  canonicalCommittedWithdrawalTransitionEffect,
  type CanonicalTransitionEffect,
  canonicalTransitionEffectFromStatePatch,
  deriveCanonicalDepositTransitionEffect,
  type ValidationMachineLedgerEntry,
  type ValidationMachineLedgerOp,
} from "@al-ft/midgard-validation";
import { LedgerColumns } from "@al-ft/midgard-validation/ledger";
import { validatePhaseASingle } from "@al-ft/midgard-validation/phase-a";
import { runPhaseBValidationWithPatch } from "@al-ft/midgard-validation/phase-b";
import type {
  PhaseAConfig,
  PhaseAValidatedTx,
  PhaseBConfig,
  QueuedTx,
  RejectCode,
  RejectedTx,
} from "@al-ft/midgard-validation/types";
import { RejectCodes } from "@al-ft/midgard-validation/types";
import { CML, Data as LucidData } from "@lucid-evolution/lucid";

import {
  parseWatcherSettlementIndexerResult,
  type WatcherSettlementIndexerResult,
  type WatcherSettlementResultVerificationContext,
} from "../indexers/settlement-indexer.js";
import {
  parseWatcherUserEventIndexerResult,
  WATCHER_FORCED_TX_VALID,
  type WatcherForcedOperatorVerdict,
  type WatcherForcedTerminalClassification,
  type WatcherIndexedUserEvent,
  type WatcherTerminalUserEvent,
  type WatcherUserEventIndexerResult,
} from "../indexers/user-event-indexer.js";
import type { WatcherL1TransportAttestationContext } from "../l1/l1-adapter.js";
import {
  makeWatcherDurablePayload,
  type WatcherReconstructedState,
  watcherSha256CanonicalJson,
} from "../storage/durable-store.js";
import type { WatcherHeaderRootReconstructionResult } from "./header-root-reconstruction.js";
import { WATCHER_HEADER_ROOT_RECONSTRUCTION_SCHEMA_VERSION } from "./header-root-reconstruction.js";
import {
  makeWatcherPhaseAConfig,
  WATCHER_PHASE_A_VERIFIER_SCHEMA_VERSION,
  watcherPhaseAQueuedTxs,
  type WatcherPhaseAVerificationResult,
} from "./phase-a-verifier.js";
import {
  WATCHER_RULE_BUNDLE_REJECTION_SELECTION,
  WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY,
  type WatcherRuleBundle,
} from "./rule-bundle-v1.js";

export const WATCHER_BLOCK_REPLAY_SCHEMA_VERSION =
  "midgard-watcher-block-replay-v1" as const;

export const WATCHER_BLOCK_REPLAY_DOWNSTREAM_PREREQUISITE_SCHEMA_VERSION =
  "midgard-watcher-block-replay-downstream-prerequisite-v1" as const;

/**
 * The W29 contract, carried inside every result so a decision cannot be made
 * from the action alone without the qualification that produced it.
 */
export const WATCHER_BLOCK_REPLAY_VERIFIED_CONTRACT =
  'complete_replay_only_v1: action "accept" proves root-exact W25 replay but can never by itself imply W29 "verified". W29 additionally requires an accepting W26 result over downstreamPrerequisite.inputDigest; downstreamPrerequisite.w29Eligibility therefore remains "requires_w26_accept" for every W25 action.' as const;

// ---------------------------------------------------------------------------
// Replay stages
// ---------------------------------------------------------------------------

/**
 * The eight replay stages, in the order the replay performs them. Every
 * mismatch and every canonical rejection is attributed to exactly one of them,
 * so a diverging block always names the stage that diverged.
 */
export const WATCHER_BLOCK_REPLAY_STAGES = [
  "prior_state",
  "dependencies",
  "spends",
  "references",
  "scripts",
  "value",
  "events",
  "post_state",
] as const;

export type WatcherBlockReplayStage =
  (typeof WATCHER_BLOCK_REPLAY_STAGES)[number];

const STAGE_ORDER: ReadonlyMap<WatcherBlockReplayStage, number> = new Map(
  WATCHER_BLOCK_REPLAY_STAGES.map((stage, index) => [stage, index]),
);

/**
 * Canonical validation phase -> replay stage, for the eight phases the
 * canonical Phase B pipeline can attach to a rejection.
 *
 * The seven Phase A phases (`canonicalDecode` .. `phaseAScriptPreconditions`)
 * are deliberately absent. A transaction only reaches Phase B after Phase A
 * accepted it, so a Phase A phase arriving on a Phase B rejection is a
 * canonical-layer surprise, and an unmapped phase fails closed with
 * `missing_rejection_stage` rather than being silently bucketed.
 */
export const WATCHER_BLOCK_REPLAY_STAGE_BY_CONSENSUS_PHASE = Object.freeze({
  resolveInputs: "spends",
  scriptSources: "scripts",
  nativeScripts: "scripts",
  scriptIntegrity: "scripts",
  cek: "scripts",
  valueAndMint: "value",
  ledgerDelta: "post_state",
  terminal: "post_state",
} as const) satisfies Partial<
  Record<MidgardValidationPhaseName, WatcherBlockReplayStage>
>;

/**
 * The two canonical detail prefixes phase-b.ts uses when the failing input is a
 * *reference* input rather than a spend input (`resolveReferenceInputs`,
 * phase-b.ts:191-207). Both rejections carry `consensusPhase: "resolveInputs"`,
 * so the phase alone cannot separate the spends stage from the references
 * stage; the canonical detail text can, and the suite pins both prefixes
 * against the canonical function so a producer-side rewording is caught.
 */
export const WATCHER_BLOCK_REPLAY_REFERENCE_DETAIL_PREFIXES = Object.freeze([
  "reference input not found: ",
  "failed to decode reference input output ",
] as const);

/**
 * The two codes the canonical pipeline emits from the block-wide dependency
 * graph rather than from any single transaction's own content
 * (`findCycleNodes` and `cascadeRejectDescendants`, phase-b.ts:856-889 and
 * :1196-1221). Both carry the default `consensusPhase: "resolveInputs"`, so
 * they are attributed by code, not by phase.
 */
export const WATCHER_BLOCK_REPLAY_DEPENDENCY_REJECT_CODES = Object.freeze([
  RejectCodes.DependencyCycle,
  RejectCodes.DependsOnRejectedTx,
] as const);

// ---------------------------------------------------------------------------
// Published rejection vocabulary (CG3 waiver condition b)
// ---------------------------------------------------------------------------

/** The full canonical vocabulary, in `RejectCodes` declaration order. */
export const WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES = Object.freeze(
  Object.values(RejectCodes),
);

/**
 * The 13 canonical codes the Phase B pipeline can emit, in `RejectCodes`
 * declaration order.
 *
 * Provenance, one entry per `reject(...)`/`fail(...)` call site in phase-b.ts:
 * `E_INVALID_OUTPUT` (:204, :1030, :1146), `E_INVALID_FIELD_TYPE` (:340, :485,
 * :570, :611), `E_MISSING_REQUIRED_WITNESS` (:370, :380, :457, :965, :1011,
 * :1139), `E_NATIVE_SCRIPT_INVALID` (:590), `E_INPUT_NOT_FOUND` (:193, :397,
 * :998, :1128), `E_DOUBLE_SPEND` (:993, :1124), `E_DEPENDENCY_CYCLE` (:1255),
 * `E_DEPENDS_ON_REJECTED_TX` (:1215, :1396), `E_VALIDITY_INTERVAL_MISMATCH`
 * (:915, :925, :1097, :1106), `E_MIN_ADA` (the output-descriptor scan),
 * `E_VALUE_NOT_PRESERVED` (:1061, :1160),
 * `E_PLUTUS_SCRIPT_INVALID` (:629, :717, :729), `E_CEK_PROGRAM_MATERIAL`
 * (:247, :551).
 */
export const WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES = Object.freeze([
  RejectCodes.InvalidOutput,
  RejectCodes.InvalidFieldType,
  RejectCodes.MissingRequiredWitness,
  RejectCodes.NativeScriptInvalid,
  RejectCodes.InputNotFound,
  RejectCodes.DoubleSpend,
  RejectCodes.DependencyCycle,
  RejectCodes.DependsOnRejectedTx,
  RejectCodes.ValidityIntervalMismatch,
  RejectCodes.MinAda,
  RejectCodes.ValueNotPreserved,
  RejectCodes.PlutusScriptInvalid,
  RejectCodes.CekProgramMaterial,
] as const);

const REACHABLE_SET: ReadonlySet<string> = new Set<string>(
  WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES,
);

/**
 * The 11 reachable codes the W25 suite pins with a deterministic
 * rejection-evidence case produced by the canonical Phase B entry point, in
 * `RejectCodes` declaration order.
 *
 * The suite asserts this list equals the set of codes its evidence corpus
 * actually produced, so it cannot drift into an unbacked claim.
 */
export const WATCHER_BLOCK_REPLAY_EVIDENCED_REJECT_CODES = Object.freeze([
  RejectCodes.InvalidFieldType,
  RejectCodes.MissingRequiredWitness,
  RejectCodes.NativeScriptInvalid,
  RejectCodes.InputNotFound,
  RejectCodes.DoubleSpend,
  RejectCodes.DependencyCycle,
  RejectCodes.DependsOnRejectedTx,
  RejectCodes.ValidityIntervalMismatch,
  RejectCodes.MinAda,
  RejectCodes.ValueNotPreserved,
  RejectCodes.PlutusScriptInvalid,
] as const);

const EVIDENCED_SET: ReadonlySet<string> = new Set<string>(
  WATCHER_BLOCK_REPLAY_EVIDENCED_REJECT_CODES,
);

/**
 * Reachable in the canonical Phase B control flow but not producible from an
 * authenticated canonical block: an earlier canonical rule always fires first.
 */
export const WATCHER_BLOCK_REPLAY_DOMINATED_REJECT_CODES = Object.freeze(
  WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES.filter(
    (code) => !EVIDENCED_SET.has(code),
  ),
);

/** One line per dominated code, naming the canonical rule that fires first. */
export const WATCHER_BLOCK_REPLAY_DOMINATED_REJECT_CODE_JUSTIFICATIONS =
  Object.freeze({
    [RejectCodes.InvalidOutput]:
      "dominated: prior-state entries must produce an exact canonical V1 ledger descriptor (buildCanonicalMidgardLedgerEntryOutputMaterialV1) before the replay starts, so an output byte string that decodeMidgardTxOutput would reject never reaches Phase B - it is reported as malformed_prior_state instead.",
    [RejectCodes.CekProgramMaterial]:
      "dominated: the sidecar handed to Phase B is the one canonical Phase A already accepted (E_CEK_PROGRAM_MATERIAL, phase-a.ts:456/487), and the watcher projects it with the canonical reachability computation itself, so the Phase B re-verification cannot be the first to fail. Reference-script-carried CEK envelopes are the one residual path and have no deterministic block fixture in this lane.",
  } as const);

/**
 * The 27 canonical codes W24's Phase A verifier owns.
 *
 * Group justification, which is the same for every member and is why they are
 * published as a group rather than with 27 near-identical lines: a transaction
 * only reaches the Phase B pipeline after `validatePhaseASingle` accepted it,
 * and phase-b.ts has no `reject(...)` call site for any of these codes. W24
 * publishes each one's reachability and per-code evidence.
 */
export const WATCHER_BLOCK_REPLAY_PHASE_A_OWNED_REJECT_CODES = Object.freeze([
  RejectCodes.CborDeserialization,
  RejectCodes.TxHashMismatch,
  RejectCodes.EmptyInputs,
  RejectCodes.DuplicateInputInTx,
  RejectCodes.InvalidValidityIntervalFormat,
  RejectCodes.MinFee,
  RejectCodes.InvalidSignature,
  RejectCodes.IsValidFalseForbidden,
  RejectCodes.AuxDataForbidden,
  RejectCodes.NetworkIdMismatch,
  RejectCodes.TxVersion,
  RejectCodes.TxSize,
  RejectCodes.ValueSize,
  RejectCodes.InputCount,
  RejectCodes.ReferenceInputCount,
  RejectCodes.OutputCount,
  RejectCodes.AddressWitnessCount,
  RejectCodes.RequiredSignerCount,
  RejectCodes.ScriptExecutionCount,
  RejectCodes.ObserverCount,
  RejectCodes.FieldPreimageSize,
  RejectCodes.LedgerOutputSize,
  RejectCodes.ScriptProgramSize,
  RejectCodes.ScriptProgramEncoding,
  RejectCodes.NativeScriptDepth,
  RejectCodes.NativeScriptNodeCount,
  RejectCodes.AssetCount,
] as const);

export const WATCHER_BLOCK_REPLAY_PHASE_A_OWNED_JUSTIFICATION =
  "owned by W24: a transaction only reaches the canonical Phase B pipeline after validatePhaseASingle accepted it, and phase-b.ts carries no reject(...) call site for this code. W24 publishes its reachability and per-code evidence." as const;

/**
 * The 10 canonical codes neither W24 nor W25 claims, each with the reason no
 * canonical call site in either pipeline can emit it. Together with the two
 * reachable sets and the Phase-A-owned set these exhaust the 50-member
 * vocabulary, which is what makes the W24 + W25 union total.
 */
export const WATCHER_BLOCK_REPLAY_UNCLAIMED_REJECT_CODE_JUSTIFICATIONS =
  Object.freeze({
    [RejectCodes.UnsupportedFieldNonEmpty]:
      "no call site in either pipeline: V1 canonical decoding rejects unsupported fields structurally, so the code is never constructed.",
    [RejectCodes.PlutusEvaluationUnavailable]:
      "no call site in phase-b.ts: the pipeline always has an evaluator - config.evaluateProofScript / config.evaluateScript when supplied, otherwise the in-process evaluateScriptWithHarmonic and executeMidgardCekStructuralProgramV1 defaults this lane uses - and every evaluation failure path, including a thrown CEK error, is mapped to E_PLUTUS_SCRIPT_INVALID (phase-b.ts:695-721). The code is therefore not environment-dependent for the watcher: there is no watcher configuration under which the canonical pipeline can report evaluation as unavailable, because the watcher never injects a remote or optional evaluator. An evaluator-injecting embedder (the operator's worker pool) is the only context that could surface it, and that is not this lane.",
    [RejectCodes.CertificatesForbidden]:
      "no call site in either pipeline: V1 canonical transactions have no certificate field to carry, so the prohibition is structural.",
    [RejectCodes.NonZeroWithdrawal]:
      "no call site in either pipeline: V1 canonical transactions have no withdrawal field, so the prohibition is structural.",
    [RejectCodes.DatumSize]:
      "no call site in either pipeline: datum bounds are enforced by the output/ledger-output preimage bounds that surface as E_LEDGER_OUTPUT_SIZE in W24.",
    [RejectCodes.ScriptProgramAggregateSize]:
      "no call site in either pipeline: the aggregate program bound is checked by the CEK bundle verifier and surfaces as E_CEK_PROGRAM_MATERIAL.",
    [RejectCodes.RedeemerSize]:
      "no call site in either pipeline: the redeemer preimage bound surfaces as E_FIELD_PREIMAGE_SIZE in W24.",
    [RejectCodes.MintForbidden]:
      "no call site in either pipeline: minting is an enabled V1 feature, so no prohibition fires.",
    [RejectCodes.ReferenceInputForbidden]:
      "no call site in either pipeline: reference inputs are an enabled V1 feature, so no prohibition fires.",
    [RejectCodes.ScriptFeatureForbidden]:
      "no call site in either pipeline: the V1 feature set enables the script features, so no prohibition fires.",
  } as const);

/** The 10 unclaimed codes, in `RejectCodes` declaration order. */
export const WATCHER_BLOCK_REPLAY_UNCLAIMED_REJECT_CODES = Object.freeze(
  WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES.filter(
    (code) => code in WATCHER_BLOCK_REPLAY_UNCLAIMED_REJECT_CODE_JUSTIFICATIONS,
  ),
);

// ---------------------------------------------------------------------------
// Reason codes and errors
// ---------------------------------------------------------------------------

/** Every reason this evaluation can report, in a fixed total order. */
export const WATCHER_BLOCK_REPLAY_REASON_CODES = [
  "reconstruction_unsupported_schema",
  "reconstruction_digest_mismatch",
  "reconstruction_not_accepted",
  "reconstruction_header_mismatch",
  "payload_bytes_mismatch",
  "phase_a_unsupported_schema",
  "phase_a_digest_mismatch",
  "phase_a_not_accepted",
  "phase_a_context_mismatch",
  "phase_a_candidate_mismatch",
  "rule_bundle_profile_mismatch",
  "header_protocol_version_mismatch",
  "canonical_reconstruction_failed",
  "malformed_prior_state",
  "prior_state_root_mismatch",
  "canonical_validation_threw",
  "canonical_replay_threw",
  "unknown_reject_code",
  "undeclared_reachable_code",
  "missing_rejection_stage",
  "rejection_tx_id_mismatch",
  "phase_b_rejection",
  "user_event_authority_invalid",
  "user_event_authority_not_indexed",
  "user_event_authority_identity_mismatch",
  "settlement_authority_invalid",
  "settlement_authority_identity_mismatch",
  "transition_effect_digest_mismatch",
  "transition_effect_semantics_mismatch",
  "missing_event_authority",
  "duplicate_event_authority",
  "event_authority_identity_mismatch",
  "committed_trace_binding_unrun",
  "transition_trace_mismatch",
  "intermediate_root_mismatch",
  "post_state_binding_unrun",
  "post_state_root_mismatch",
] as const;

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

const fail = (code: WatcherBlockReplayReasonCode, path: string): never => {
  throw new WatcherBlockReplayError(code, path);
};

const reasonCodeOf = (error: unknown): WatcherBlockReplayReasonCode =>
  error instanceof WatcherBlockReplayError
    ? error.code
    : "canonical_replay_threw";

const orderReasonCodes = (
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

/** The state boundary around one accepted transaction. */
export type WatcherBlockReplayTransactionRoot = Readonly<{
  /** Position in the canonical accepted order. */
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

const admittedFullBlockReplayResults = new WeakSet<object>();

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

const admitFullBlockReplayResult = (
  result: WatcherBlockReplayResult,
): WatcherBlockReplayResult => {
  admittedFullBlockReplayResults.add(result);
  return result;
};

const digestResult = (
  result: Omit<
    WatcherBlockReplayResult,
    "resultDigest" | "downstreamPrerequisite"
  >,
): WatcherBlockReplayResult =>
  Object.freeze(
    (() => {
      const inputDigest = watcherSha256CanonicalJson({
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

export type WatcherBlockReplayContext = Readonly<{
  headerHash: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
  reconstructionDigest: string;
  phaseAResultDigest: string;
  ruleBundleCommitment: string;
}>;

const NULL_CONTEXT = {
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

const errorResult = (
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

const HEX_32 = /^[0-9a-f]{64}$/u;
const ZERO_ROOT = "00".repeat(32);

/**
 * The canonical validation-machine trie reports the empty tree as 32 zero
 * bytes, while the canonical PHAS helper reports it as
 * `EMPTY_MERKLE_TREE_ROOT`. Both are the canonical spelling of "no entries" in
 * their own module; this maps the former onto the latter so the two canonical
 * root sources are comparable at all. It is a spelling normalisation, not a
 * root computation.
 */
const normalizeRootHex = (root: string): string =>
  root === ZERO_ROOT ? EMPTY_MERKLE_TREE_ROOT : root;

/**
 * Exact canonical rejection-to-forced-verdict partition consumed by W26.
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
const selectRejection = (
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

const orderStageMismatches = (
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

const HEX_BYTES = /^(?:[0-9a-f]{2})+$/u;

/**
 * Turns the supplied prior-state entries into canonical ledger entries and
 * recomputes their PHAS root with the canonical helper.
 *
 * Every value is canonicalised by `buildCanonicalMidgardLedgerEntryOutputMaterial`,
 * the exact descriptor builder the canonical reconstruction uses for
 * `block_body.utxos` (reconstruct.ts:816-826), so the root computed here is
 * comparable to the header's committed roots by construction rather than by
 * agreement.
 */
export const watcherBlockReplayPriorState = async (
  entries: readonly WatcherBlockReplayPriorUtxo[],
): Promise<{
  readonly ledgerEntries: readonly ValidationMachineLedgerEntry[];
  readonly root: string;
}> => {
  const ledgerEntries: ValidationMachineLedgerEntry[] = [];
  const descriptors: { readonly key: Buffer; readonly value: Buffer }[] = [];
  const seen = new Set<string>();
  for (const [index, entry] of entries.entries()) {
    const path = `$.priorState[${index.toString()}]`;
    if (!HEX_BYTES.test(entry.outRef) || !HEX_BYTES.test(entry.outputCbor)) {
      fail("malformed_prior_state", path);
    }
    if (seen.has(entry.outRef)) {
      fail("malformed_prior_state", `${path}.outRef`);
    }
    seen.add(entry.outRef);
    const outRef = Buffer.from(entry.outRef, "hex");
    const output = Buffer.from(entry.outputCbor, "hex");
    let descriptorCbor: Buffer;
    try {
      descriptorCbor = buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef,
        outputCbor: output,
      }).descriptorCbor;
    } catch {
      return fail("malformed_prior_state", `${path}.outputCbor`);
    }
    ledgerEntries.push({ outRef, output });
    descriptors.push({ key: outRef, value: descriptorCbor });
  }
  const phas = await keyValuePhasRootWithCount(descriptors);
  return {
    ledgerEntries: Object.freeze(
      [...ledgerEntries].sort((left, right) =>
        Buffer.compare(left.outRef, right.outRef),
      ),
    ),
    root: normalizeRootHex(phas.root),
  };
};

// ---------------------------------------------------------------------------
// Canonical replay core
// ---------------------------------------------------------------------------

/**
 * Phase B is deterministic in the candidates, the prior state, and the
 * header-committed block slot. Concurrency is pinned to 1 because a verifier
 * has no throughput requirement and the value must not influence a digest, and
 * the script budget is enforced because the canonical default for a validating
 * node is enforcement.
 */
export const WATCHER_BLOCK_REPLAY_BUCKET_CONCURRENCY = 1 as const;

/**
 * Builds the canonical `PhaseBConfig` from the L1-committed header. Nothing
 * here is a policy choice by the watcher: the slot is a header field and the
 * other two values are pinned constants that cannot change a verdict's
 * direction.
 */
export const makeWatcherPhaseBConfig = (header: Header): PhaseBConfig =>
  Object.freeze({
    nowCardanoSlotNo: header.blockSlot,
    bucketConcurrency: WATCHER_BLOCK_REPLAY_BUCKET_CONCURRENCY,
    enforceScriptBudget: true,
  });

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

export type WatcherBlockReplayUserEventVerificationContext = Readonly<{
  policy: unknown;
  previousState: unknown;
  observation: unknown;
  publicContext: unknown;
  transportAttestations: readonly WatcherL1TransportAttestationContext[];
}>;

export type WatcherBlockReplayUserEventAuthority = Readonly<{
  result: unknown;
  context: WatcherBlockReplayUserEventVerificationContext;
}>;

export type WatcherBlockReplaySettlementAuthority = Readonly<{
  result: unknown;
  context: WatcherSettlementResultVerificationContext;
  /** Selects the exact accepted observation whose transition is authoritative. */
  observationDigest: string;
}>;

/**
 * An event effect is accepted only alongside a parser-recomputed W15 record;
 * withdrawals additionally require the exact parser-recomputed W16 terminal
 * transition that commits whether the L2 out-ref was consumed or refunded.
 */
export type WatcherBlockReplayEventAuthority = Readonly<{
  eventKey: EventKey;
  phase: Exclude<WatcherBlockReplayCommittedStep["phase"], "L2Transaction">;
  userEvent: WatcherBlockReplayUserEventAuthority;
  settlement?: WatcherBlockReplaySettlementAuthority | null;
  transitionEffect: CanonicalTransitionEffect;
  /** Required only for a forced order; exact bytes are compact/commitment-bound to W15. */
  canonicalNativeTxCbor?: Uint8Array | null;
  /** Canonical Phase-A program material for the forced native transaction. */
  programMaterialSidecarCbor?: Uint8Array | null;
}>;

export type WatcherBlockReplayEventRoot = Readonly<{
  stepIndex: number;
  phase: WatcherBlockReplayEventAuthority["phase"];
  eventKeyFingerprint: string;
  preRoot: string;
  postRoot: string;
  mutationCount: number;
}>;

const eventKeyTxId = (eventKey: EventKey): string | null =>
  "L2TransactionEventKey" in eventKey
    ? eventKey.L2TransactionEventKey.tx_id
    : null;

const phaseForEventKey = (
  eventKey: EventKey,
): WatcherBlockReplayCommittedStep["phase"] =>
  "DepositEventKey" in eventKey
    ? "Deposit"
    : "WithdrawalEventKey" in eventKey
      ? "Withdrawal"
      : "ForcedTransactionEventKey" in eventKey
        ? "ForcedTransaction"
        : "L2Transaction";

const eventKeyFingerprint = (eventKey: EventKey): string => {
  if ("L2TransactionEventKey" in eventKey) {
    return `L2Transaction:${eventKey.L2TransactionEventKey.tx_id}`;
  }
  if ("DepositEventKey" in eventKey) {
    const id = eventKey.DepositEventKey.deposit_id;
    return `Deposit:${id.transactionId}:${id.outputIndex.toString()}`;
  }
  if ("WithdrawalEventKey" in eventKey) {
    const id = eventKey.WithdrawalEventKey.withdrawal_id;
    return `Withdrawal:${id.transactionId}:${id.outputIndex.toString()}`;
  }
  const id = eventKey.ForcedTransactionEventKey.tx_order_id;
  return `ForcedTransaction:${id.transactionId}:${id.outputIndex.toString()}`;
};

type ValidatedEventAuthority = Readonly<{
  phase: WatcherBlockReplayEventAuthority["phase"];
  eventKeyFingerprint: string;
  effect: CanonicalTransitionEffect;
  canonicalNativeTxCbor: Buffer | null;
  programMaterialSidecarCbor: Buffer | null;
  terminalClassification: WatcherForcedTerminalClassification | null;
  userEvent: WatcherIndexedUserEvent | WatcherTerminalUserEvent;
  authorityManifest: Readonly<Record<string, unknown>>;
  effectManifest: Readonly<Record<string, unknown>>;
}>;

const sha256Hex = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");

const userEventKindForPhase = (
  phase: WatcherBlockReplayEventAuthority["phase"],
): WatcherIndexedUserEvent["kind"] =>
  phase === "Deposit"
    ? "deposit"
    : phase === "Withdrawal"
      ? "withdrawal"
      : "forced_order";

const eventIdForKey = (
  eventKey: EventKey,
): Readonly<{ transactionId: string; outputIndex: bigint }> =>
  "DepositEventKey" in eventKey
    ? eventKey.DepositEventKey.deposit_id
    : "WithdrawalEventKey" in eventKey
      ? eventKey.WithdrawalEventKey.withdrawal_id
      : "ForcedTransactionEventKey" in eventKey
        ? eventKey.ForcedTransactionEventKey.tx_order_id
        : fail("user_event_authority_identity_mismatch", "$.eventKey");

const eventIdForKeyCborHex = (eventKey: EventKey): string =>
  LucidData.to(eventIdForKey(eventKey) as never, OutputReference as never);

/**
 * The ledger trie key / transition-effect out-ref: the §5.3 field-0/1 item form
 * `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, a fixed 38 bytes, matching on-chain
 * `ledger_outref_key` / `encode_midgard_tx_input`. CML's minimal-index
 * `TransactionInput` CBOR would not compare equal to the effect out-refs
 * `canonicalOutRefCbor` admits.
 */
const ledgerOutRefCborHex = (value: {
  readonly transactionId: string;
  readonly outputIndex: bigint;
}): string =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(value.transactionId, "hex"),
    outputIndex: Number(value.outputIndex),
  }).toString("hex");

const l1AssetsFromOutputCbor = (
  outputCborHex: string,
): Readonly<Record<string, bigint>> => {
  const output = CML.TransactionOutput.from_cbor_hex(outputCborHex);
  const value = output.amount();
  const assets: Record<string, bigint> = { lovelace: value.coin() };
  const multiasset = value.multi_asset();
  if (multiasset !== undefined) {
    const policies = multiasset.keys();
    for (let policyIndex = 0; policyIndex < policies.len(); policyIndex += 1) {
      const policy = policies.get(policyIndex);
      const policyAssets = multiasset.get_assets(policy);
      if (policyAssets === undefined) {
        continue;
      }
      const names = policyAssets.keys();
      for (let nameIndex = 0; nameIndex < names.len(); nameIndex += 1) {
        const name = names.get(nameIndex);
        const quantity = policyAssets.get(name);
        if (quantity !== undefined) {
          assets[`${policy.to_hex()}${name.to_hex()}`] = quantity;
        }
      }
    }
  }
  return Object.freeze(assets);
};

const decodeUserEventIdCborHex = (
  event: WatcherIndexedUserEvent,
): string | null => {
  try {
    const schema =
      event.kind === "deposit"
        ? DepositEvent
        : event.kind === "withdrawal"
          ? WithdrawalEvent
          : TxOrderEvent;
    const decoded = LucidData.from(event.eventCborHex, schema as never) as {
      readonly id: unknown;
    };
    return LucidData.to(decoded.id as never, OutputReference as never);
  } catch {
    return null;
  }
};

const allUserEvents = (
  result: WatcherUserEventIndexerResult,
): readonly (WatcherIndexedUserEvent | WatcherTerminalUserEvent)[] =>
  result.state === null
    ? []
    : [
        ...result.state.snapshot.activeEvents,
        ...result.state.snapshot.terminalEvents,
      ];

const authoritativeHistoryDigests = (
  result: WatcherUserEventIndexerResult,
  event: WatcherIndexedUserEvent,
): readonly string[] =>
  result.state === null
    ? []
    : Object.freeze(
        result.state.history
          .filter(({ observation }) =>
            [
              ...observation.snapshot.activeEvents,
              ...observation.snapshot.terminalEvents,
            ].some(
              (candidate) =>
                candidate.eventId === event.eventId &&
                candidate.eventContentDigest === event.eventContentDigest &&
                candidate.datumDigest === event.datumDigest &&
                candidate.outputDigest === event.outputDigest &&
                candidate.originPointDigest === event.originPointDigest,
            ),
          )
          .map(({ entryDigest }) => entryDigest),
      );

const parseSettlementAuthority = (
  authority: WatcherBlockReplaySettlementAuthority,
): Readonly<{
  result: WatcherSettlementIndexerResult;
  observation: NonNullable<
    WatcherSettlementIndexerResult["state"]
  >["activeHistory"][number]["observation"];
}> => {
  const parsed = parseWatcherSettlementIndexerResult(
    authority.result,
    authority.context,
  );
  if (
    parsed === null ||
    parsed.action !== "accept" ||
    parsed.protocolDecision !== "indexed" ||
    parsed.state === null
  ) {
    return fail("settlement_authority_invalid", "$.settlement.result");
  }
  const observation = parsed.state.activeHistory.find(
    (entry) =>
      entry.observation.observationDigest === authority.observationDigest,
  )?.observation;
  if (observation === undefined) {
    return fail(
      "settlement_authority_identity_mismatch",
      "$.settlement.observationDigest",
    );
  }
  return Object.freeze({ result: parsed, observation });
};

const validateSettlementEventBinding = (input: {
  readonly phase: WatcherBlockReplayEventAuthority["phase"];
  readonly event: WatcherIndexedUserEvent;
  readonly authority: WatcherBlockReplaySettlementAuthority;
}): Readonly<Record<string, unknown>> => {
  const { result, observation } = parseSettlementAuthority(input.authority);
  const transition = observation.transition;
  const subjects = result.state!.snapshot.subjects;
  const hasExactConsumedEvent =
    transition.consumedOutRefs.length === 1 &&
    transition.consumedOutRefs[0] === input.event.outRef;
  const isAbsorbedDeposit =
    input.phase === "Deposit" &&
    transition.kind === "absorb_to_reserve" &&
    transition.subjectId === input.event.assetNameHex &&
    transition.relatedSubjectId === null &&
    hasExactConsumedEvent &&
    transition.producedOutRefs.length === 1 &&
    subjects.some(
      (subject) =>
        subject.subjectKind === "reserve" &&
        subject.relatedSubjectId === input.event.assetNameHex &&
        subject.resourceOutRef === transition.producedOutRefs[0],
    );
  const isInitializedPayout =
    input.phase === "Withdrawal" &&
    transition.kind === "initialize_payout" &&
    transition.subjectId !== null &&
    transition.relatedSubjectId === input.event.assetNameHex &&
    hasExactConsumedEvent &&
    subjects.some(
      (subject) =>
        subject.subjectKind === "payout" &&
        subject.subjectId === transition.subjectId &&
        subject.relatedSubjectId === input.event.assetNameHex &&
        subject.resourceOutRef !== null &&
        transition.producedOutRefs.includes(subject.resourceOutRef),
    ) &&
    subjects.some(
      (subject) =>
        subject.subjectKind === "withdrawal" &&
        subject.subjectId === input.event.assetNameHex &&
        subject.status === "resolved" &&
        subject.resourceOutRef === null,
    );
  const isRefundedWithdrawal =
    input.phase === "Withdrawal" &&
    transition.kind === "refund_withdrawal" &&
    transition.subjectId === input.event.assetNameHex &&
    transition.relatedSubjectId === null &&
    hasExactConsumedEvent &&
    transition.producedOutRefs.length === 0 &&
    subjects.some(
      (subject) =>
        subject.subjectKind === "withdrawal" &&
        subject.subjectId === input.event.assetNameHex &&
        subject.status === "refunded" &&
        subject.resourceOutRef === null,
    );
  if (!isAbsorbedDeposit && !isInitializedPayout && !isRefundedWithdrawal) {
    return fail(
      "settlement_authority_identity_mismatch",
      "$.settlement.transition",
    );
  }
  return Object.freeze({
    resultDigest: result.resultDigest,
    stateDigest: result.state!.stateDigest,
    observationDigest: observation.observationDigest,
    transitionKind: transition.kind,
    subjectId: transition.subjectId,
    relatedSubjectId: transition.relatedSubjectId,
    consumedOutRefs: transition.consumedOutRefs,
    producedOutRefs: transition.producedOutRefs,
    transactionHash: observation.transactionHash,
    pointDigest: observation.pointDigest,
    chainPointId: observation.chainPointId,
    blockHash: observation.blockHash,
    slot: observation.slot,
    blockNo: observation.blockNo,
    snapshotDigest: observation.snapshot.snapshotDigest,
  });
};

const canonicalEffectFromAuthority = (
  authority: WatcherBlockReplayEventAuthority,
): CanonicalTransitionEffect => {
  const rebuilt = buildCanonicalTransitionEffect(
    authority.transitionEffect.operations,
  );
  if (
    rebuilt.schemaVersion !== authority.transitionEffect.schemaVersion ||
    rebuilt.digest !== authority.transitionEffect.digest ||
    !rebuilt.canonicalCbor.equals(authority.transitionEffect.canonicalCbor)
  ) {
    return fail(
      "transition_effect_digest_mismatch",
      "$.transitionEffect.digest",
    );
  }
  return rebuilt;
};

const validateEventAuthority = (
  authority: WatcherBlockReplayEventAuthority,
): ValidatedEventAuthority => {
  const parsed = parseWatcherUserEventIndexerResult(
    authority.userEvent.result,
    authority.userEvent.context,
  );
  if (parsed === null) {
    return fail("user_event_authority_invalid", "$.userEvent.result");
  }
  if (
    parsed.action !== "accept" ||
    parsed.protocolDecision !== "indexed" ||
    parsed.state === null ||
    parsed.state.snapshot.quarantined
  ) {
    return fail(
      "user_event_authority_not_indexed",
      "$.userEvent.result.action",
    );
  }
  const eventId = eventIdForKeyCborHex(authority.eventKey);
  const eventOutRef = eventIdForKey(authority.eventKey);
  const expectedKind = userEventKindForPhase(authority.phase);
  const matches = allUserEvents(parsed).filter(
    (event) =>
      event.kind === expectedKind &&
      event.eventId === eventId &&
      decodeUserEventIdCborHex(event) === eventId,
  );
  if (matches.length !== 1) {
    return fail(
      "user_event_authority_identity_mismatch",
      "$.userEvent.result.state.snapshot",
    );
  }
  const event = matches[0]!;
  if (
    event.nonceOutRef !==
      `${eventOutRef.transactionId}#${eventOutRef.outputIndex.toString()}` ||
    sha256Hex(Buffer.from(event.eventCborHex, "hex")) !==
      event.eventContentDigest ||
    sha256Hex(Buffer.from(event.datumCborHex, "hex")) !== event.datumDigest ||
    sha256Hex(Buffer.from(event.outputCborHex, "hex")) !== event.outputDigest
  ) {
    return fail(
      "user_event_authority_identity_mismatch",
      "$.userEvent.result.state.snapshot.digest",
    );
  }
  if (
    authority.phase === "ForcedTransaction" &&
    (!("terminalStatus" in event) ||
      event.terminalStatus !== "processed" ||
      event.terminalClassification === undefined)
  ) {
    return fail(
      "user_event_authority_identity_mismatch",
      "$.userEvent.result.state.snapshot.terminalStatus",
    );
  }
  const terminalEvent = "terminalStatus" in event ? event : null;
  const terminalClassification =
    authority.phase === "ForcedTransaction" && terminalEvent !== null
      ? (terminalEvent.terminalClassification ?? null)
      : null;
  if (
    authority.phase === "ForcedTransaction" &&
    (terminalClassification === null ||
      terminalClassification.terminalTransactionHash !==
        terminalEvent?.terminalTransactionHash ||
      terminalClassification.terminalPointDigest !==
        terminalEvent?.terminalPointDigest)
  ) {
    return fail(
      "user_event_authority_identity_mismatch",
      "$.userEvent.result.state.snapshot.terminalClassification",
    );
  }
  const historyEntryDigests = authoritativeHistoryDigests(parsed, event);
  if (historyEntryDigests.length === 0) {
    return fail(
      "user_event_authority_identity_mismatch",
      "$.userEvent.result.state.history",
    );
  }
  const effect = canonicalEffectFromAuthority(authority);
  let canonicalNativeTxCbor: Buffer | null = null;
  let programMaterialSidecarCbor: Buffer | null = null;
  let settlementManifest: Readonly<Record<string, unknown>> | null = null;
  if (authority.phase === "Withdrawal") {
    if (authority.settlement === undefined || authority.settlement === null) {
      return fail("settlement_authority_invalid", "$.settlement");
    }
    settlementManifest = validateSettlementEventBinding({
      phase: authority.phase,
      event,
      authority: authority.settlement,
    });
    const isCommittedValid =
      settlementManifest.transitionKind === "initialize_payout";
    const decoded = LucidData.from(
      event.eventCborHex,
      WithdrawalEvent as never,
    ) as {
      readonly info: {
        readonly body: {
          readonly l2_outref: {
            readonly transactionId: string;
            readonly outputIndex: bigint;
          };
        };
      };
    };
    const expectedOutRef = ledgerOutRefCborHex(decoded.info.body.l2_outref);
    const derivedEffect = canonicalCommittedWithdrawalTransitionEffect({
      committedValid: isCommittedValid,
      outRefCbor: Buffer.from(expectedOutRef, "hex"),
    });
    if (
      effect.digest !== derivedEffect.digest ||
      !effect.canonicalCbor.equals(derivedEffect.canonicalCbor)
    ) {
      return fail(
        "transition_effect_semantics_mismatch",
        "$.transitionEffect.operations",
      );
    }
  } else if (authority.phase === "Deposit") {
    if (authority.settlement === undefined || authority.settlement === null) {
      return fail("settlement_authority_invalid", "$.settlement");
    }
    settlementManifest = validateSettlementEventBinding({
      phase: authority.phase,
      event,
      authority: authority.settlement,
    });
    const decoded = LucidData.from(
      event.eventCborHex,
      DepositEvent as never,
    ) as {
      readonly id: {
        readonly transactionId: string;
        readonly outputIndex: bigint;
      };
      readonly info: {
        readonly l2_network_id: bigint;
        readonly l2_address: Parameters<
          typeof deriveCanonicalDepositTransitionEffect
        >[0]["l2Address"];
        readonly l2_datum: unknown | null;
      };
    };
    const derivedEffect = deriveCanonicalDepositTransitionEffect({
      configuredNetwork: parsed.state.network,
      eventId: decoded.id,
      l2NetworkId: decoded.info.l2_network_id,
      l2Address: decoded.info.l2_address,
      l2DatumCbor:
        decoded.info.l2_datum === null
          ? null
          : Buffer.from(LucidData.to(decoded.info.l2_datum as never), "hex"),
      l1Assets: l1AssetsFromOutputCbor(event.outputCborHex),
      depositPolicyId: event.policyId,
      depositAssetNameHex: event.assetNameHex,
    });
    if (
      effect.operations.length !== 1 ||
      effect.operations[0]!.type !== "insert" ||
      effect.operations[0]!.outRefCbor.toString("hex") !==
        ledgerOutRefCborHex(decoded.id) ||
      effect.digest !== derivedEffect.digest ||
      !effect.canonicalCbor.equals(derivedEffect.canonicalCbor)
    ) {
      return fail(
        "transition_effect_semantics_mismatch",
        "$.transitionEffect.operations",
      );
    }
  } else {
    if (
      authority.canonicalNativeTxCbor === undefined ||
      authority.canonicalNativeTxCbor === null
    ) {
      return fail(
        "transition_effect_semantics_mismatch",
        "$.canonicalNativeTxCbor",
      );
    }
    canonicalNativeTxCbor = Buffer.from(authority.canonicalNativeTxCbor);
    programMaterialSidecarCbor =
      authority.programMaterialSidecarCbor === undefined ||
      authority.programMaterialSidecarCbor === null
        ? null
        : Buffer.from(authority.programMaterialSidecarCbor);
    try {
      const decodedEvent = LucidData.from(
        event.eventCborHex,
        TxOrderEvent as never,
      ) as {
        readonly tx: {
          readonly tx_id: string;
          readonly transaction_commitment: string;
          readonly source: {
            readonly compact_cbor: string;
            readonly witness_set_compact_cbor: string;
            readonly field_preimage_lengths_cbor: string;
          };
        };
      };
      const nativeTx = decodeMidgardNativeTxFullFromCanonicalCbor(
        canonicalNativeTxCbor,
      );
      const source = deriveMidgardNativeTxProofSourceFromCanonicalCbor(
        canonicalNativeTxCbor,
      );
      if (
        computeMidgardNativeTxId(nativeTx).toString("hex") !==
          decodedEvent.tx.tx_id ||
        computeMidgardNativeTxProofCommitment(source).toString("hex") !==
          decodedEvent.tx.transaction_commitment ||
        source.compactCbor.toString("hex") !==
          decodedEvent.tx.source.compact_cbor ||
        source.witnessSetCompactCbor.toString("hex") !==
          decodedEvent.tx.source.witness_set_compact_cbor ||
        source.fieldPreimageLengthsCbor.toString("hex") !==
          decodedEvent.tx.source.field_preimage_lengths_cbor
      ) {
        return fail(
          "transition_effect_semantics_mismatch",
          "$.canonicalNativeTxCbor.binding",
        );
      }
    } catch (error) {
      if (error instanceof WatcherBlockReplayError) {
        throw error;
      }
      return fail(
        "transition_effect_semantics_mismatch",
        "$.canonicalNativeTxCbor",
      );
    }
  }
  const authorityManifest = Object.freeze({
    phase: authority.phase,
    eventKeyFingerprint: eventKeyFingerprint(authority.eventKey),
    userEventResultDigest: parsed.resultDigest,
    userEventStateDigest: parsed.state.stateDigest,
    userEventSnapshotDigest: parsed.state.snapshot.snapshotDigest,
    historyEntryDigests,
    eventId: event.eventId,
    eventOutRef: event.outRef,
    transactionHash: event.transactionHash,
    eventContentDigest: event.eventContentDigest,
    datumDigest: event.datumDigest,
    outputDigest: event.outputDigest,
    originPointDigest: event.originPointDigest,
    originChainPointId: event.originChainPointId,
    originBlockHash: event.originBlockHash,
    originSlot: event.originSlot,
    originBlockNo: event.originBlockNo,
    finalityStatus: event.finalityStatus,
    ...("terminalStatus" in event
      ? {
          terminalStatus: event.terminalStatus,
          terminalTransactionHash: event.terminalTransactionHash,
          terminalPointDigest: event.terminalPointDigest,
          terminalBlockHash: event.terminalBlockHash,
          terminalSlot: event.terminalSlot,
          terminalBlockNo: event.terminalBlockNo,
          terminalFinalityStatus: event.terminalFinalityStatus,
          ...(event.terminalClassification === undefined
            ? {}
            : { terminalClassification: event.terminalClassification }),
        }
      : {}),
    ...(settlementManifest === null ? {} : { settlementManifest }),
  });
  const effectManifest = Object.freeze({
    phase: authority.phase,
    eventKeyFingerprint: eventKeyFingerprint(authority.eventKey),
    effectDigest: effect.digest,
    effectCborSha256: sha256Hex(effect.canonicalCbor),
    operations: effect.operations.map((operation) => ({
      type: operation.type,
      outRefCbor: operation.outRefCbor.toString("hex"),
      ...(operation.type === "insert"
        ? { outputCborSha256: sha256Hex(operation.outputCbor) }
        : {}),
    })),
  });
  return Object.freeze({
    phase: authority.phase,
    eventKeyFingerprint: eventKeyFingerprint(authority.eventKey),
    effect,
    canonicalNativeTxCbor,
    programMaterialSidecarCbor,
    terminalClassification,
    userEvent: event,
    authorityManifest,
    effectManifest,
  });
};

export type EvaluateWatcherBlockReplayCandidatesInput = {
  /** Canonical Phase A candidates, in canonical block order. */
  readonly candidates: readonly PhaseAValidatedTx[];
  /** Prior-state ledger entries, from the W21 records. */
  readonly priorState: readonly WatcherBlockReplayPriorUtxo[];
  /** The L1-committed `prevUtxosRoot` the prior state must reproduce. */
  readonly expectedPriorStateRoot: string;
  /** The L1-committed `utxosRoot`, or null to leave the post state unbound. */
  readonly expectedPostStateRoot?: string | null;
  readonly config: PhaseBConfig;
  readonly context?: WatcherBlockReplayContext;
};

type ReplayCore = {
  readonly reasonCodes: Set<WatcherBlockReplayReasonCode>;
  readonly stageMismatches: WatcherBlockReplayStageMismatch[];
  readonly rejections: WatcherBlockReplayRejection[];
  readonly acceptedTxIds: string[];
  readonly intermediateRoots: WatcherBlockReplayIntermediateRoot[];
  readonly transactionRoots: WatcherBlockReplayTransactionRoot[];
  readonly eventRoots: WatcherBlockReplayEventRoot[];
  readonly forcedValidationFacts: WatcherBlockReplayForcedValidationFact[];
  readonly priorStateRoot: string;
  readonly postStateRoot: string;
  readonly authorityManifestDigest: string | null;
  readonly sourceManifestDigest: string | null;
  readonly effectManifestDigest: string | null;
};

const buildLedgerOperations = (
  accepted: readonly PhaseAValidatedTx[],
): readonly {
  readonly txId: string;
  readonly operations: readonly ValidationMachineLedgerOp[];
}[] =>
  accepted.map((candidate) => ({
    txId: candidate.ledgerTx.txId.toString("hex"),
    operations: [
      ...candidate.graph.spentOutRefHexes.map(
        (outRefHex) =>
          ({
            type: "delete",
            key: Buffer.from(outRefHex, "hex"),
          }) satisfies ValidationMachineLedgerOp,
      ),
      ...candidate.graph.produced.map((produced) =>
        buildValidationMachineLedgerInsertOp({
          key: produced[LedgerColumns.OUTREF],
          outputCbor: produced[LedgerColumns.OUTPUT],
        }),
      ),
    ],
  }));

/**
 * The W25 core: runs the canonical Phase B pipeline over already-derived
 * candidates and recomputes every intermediate ledger root with the canonical
 * validation-machine mutator.
 *
 * This is the only place a verdict is produced, and it produces none of its
 * own. Everything after the canonical calls is bookkeeping.
 */
export const evaluateWatcherBlockReplayCandidates = async (
  input: EvaluateWatcherBlockReplayCandidatesInput,
): Promise<WatcherBlockReplayResult> => {
  const context = input.context ?? null;
  const transactionCount = input.candidates.length;
  try {
    const core = await replayCandidates(input);
    return finalizeResult({
      core,
      context,
      transactionCount,
      expectedPriorStateRoot: input.expectedPriorStateRoot,
      expectedPostStateRoot: input.expectedPostStateRoot ?? null,
      committedSteps: null,
    });
  } catch (error) {
    return errorResult([reasonCodeOf(error)], context, transactionCount);
  }
};

const replayCandidates = async (
  input: EvaluateWatcherBlockReplayCandidatesInput,
): Promise<ReplayCore> => {
  const reasonCodes = new Set<WatcherBlockReplayReasonCode>();
  const stageMismatches: WatcherBlockReplayStageMismatch[] = [];

  const prior = await watcherBlockReplayPriorState(input.priorState);
  if (prior.root !== input.expectedPriorStateRoot) {
    // Fail before the replay: a prior state that is not the committed one
    // cannot produce meaningful intermediate roots, and continuing would
    // manufacture an attributed fault against the operator out of the
    // watcher's own bad input.
    reasonCodes.add("prior_state_root_mismatch");
    stageMismatches.push({
      stage: "prior_state",
      reasonCode: "prior_state_root_mismatch",
      field: "$.header.prevUtxosRoot",
      expected: input.expectedPriorStateRoot,
      actual: prior.root,
    });
    return {
      reasonCodes,
      stageMismatches,
      rejections: [],
      acceptedTxIds: [],
      intermediateRoots: [],
      transactionRoots: [],
      eventRoots: [],
      forcedValidationFacts: [],
      priorStateRoot: prior.root,
      postStateRoot: prior.root,
      authorityManifestDigest: null,
      sourceManifestDigest: null,
      effectManifestDigest: null,
    };
  }

  const indexByTxId = new Map<string, number>();
  for (const [index, candidate] of input.candidates.entries()) {
    indexByTxId.set(candidate.ledgerTx.txId.toString("hex"), index);
  }

  const preState = new Map<string, Buffer>(
    prior.ledgerEntries.map((entry) => [
      entry.outRef.toString("hex"),
      entry.output,
    ]),
  );

  let phaseB;
  try {
    phaseB = await makeReturn(
      runPhaseBValidationWithPatch(input.candidates, preState, input.config),
    ).unsafeRun();
  } catch {
    return fail("canonical_validation_threw", "$.phaseB");
  }

  const rejections = phaseB.rejected
    .map((rejected) =>
      watcherBlockReplayRejectionProjection({ rejected, indexByTxId }),
    )
    .sort((left, right) => left.index - right.index);
  for (const rejection of rejections) {
    if (!REACHABLE_SET.has(rejection.code)) {
      reasonCodes.add("undeclared_reachable_code");
    }
    reasonCodes.add("phase_b_rejection");
  }

  const perTx = buildLedgerOperations(phaseB.accepted);
  let mutationSteps;
  try {
    mutationSteps = await buildValidationMachineLedgerMutationSteps({
      initialEntries: prior.ledgerEntries,
      operations: perTx.flatMap((entry) => entry.operations),
    });
  } catch {
    return fail("canonical_replay_threw", "$.intermediateRoots");
  }

  const intermediateRoots: WatcherBlockReplayIntermediateRoot[] = [];
  const transactionRoots: WatcherBlockReplayTransactionRoot[] = [];
  let cursor = 0;
  let postStateRoot = prior.root;
  for (const [txIndex, entry] of perTx.entries()) {
    const first = cursor;
    for (
      let remaining = entry.operations.length;
      remaining > 0;
      remaining -= 1
    ) {
      const step = mutationSteps[cursor];
      const operation = step.operation;
      intermediateRoots.push(
        Object.freeze({
          sequence: cursor,
          txIndex,
          txId: entry.txId,
          stepIndex: null,
          phase: null,
          operation: operation.type,
          outRef: operation.key.toString("hex"),
          preRoot: normalizeRootHex(step.preRoot.toString("hex")),
          postRoot: normalizeRootHex(step.postRoot.toString("hex")),
        }),
      );
      cursor += 1;
    }
    const preRoot =
      first === cursor
        ? postStateRoot
        : normalizeRootHex(mutationSteps[first].preRoot.toString("hex"));
    postStateRoot =
      first === cursor
        ? postStateRoot
        : normalizeRootHex(mutationSteps[cursor - 1].postRoot.toString("hex"));
    transactionRoots.push(
      Object.freeze({
        txIndex,
        txId: entry.txId,
        preRoot,
        postRoot: postStateRoot,
        mutationCount: cursor - first,
        committedStepIndex: null,
        committedPreRoot: null,
        committedPostRoot: null,
      }),
    );
  }

  return {
    reasonCodes,
    stageMismatches,
    rejections,
    acceptedTxIds: phaseB.accepted.map((candidate) =>
      candidate.ledgerTx.txId.toString("hex"),
    ),
    intermediateRoots,
    transactionRoots,
    eventRoots: [],
    forcedValidationFacts: [],
    priorStateRoot: prior.root,
    postStateRoot,
    authorityManifestDigest: null,
    sourceManifestDigest: null,
    effectManifestDigest: null,
  };
};

type CommittedReplayGroup = Readonly<{
  step: WatcherBlockReplayCommittedStep;
  txIndex: number | null;
  txId: string | null;
  phase: WatcherBlockReplayCommittedStep["phase"];
  eventKeyFingerprint: string;
  operations: readonly ValidationMachineLedgerOp[];
}>;

const eventLedgerOperations = (
  authority: ValidatedEventAuthority,
): readonly ValidationMachineLedgerOp[] =>
  authority.effect.operations.map((operation) =>
    operation.type === "delete"
      ? {
          type: "delete",
          key: Buffer.from(operation.outRefCbor),
        }
      : buildValidationMachineLedgerInsertOp({
          key: operation.outRefCbor,
          outputCbor: operation.outputCbor,
        }),
  );

const applyRawEventOperations = (
  state: Map<string, Buffer>,
  authority: ValidatedEventAuthority,
): void => {
  for (const operation of authority.effect.operations) {
    if (operation.type === "delete") {
      state.delete(operation.outRefCbor.toString("hex"));
    } else {
      state.set(
        operation.outRefCbor.toString("hex"),
        Buffer.from(operation.outputCbor),
      );
    }
  }
};

const applyAcceptedCandidate = (
  state: Map<string, Buffer>,
  candidate: PhaseAValidatedTx,
): void => {
  for (const outRef of candidate.graph.spentOutRefHexes) {
    state.delete(outRef);
  }
  for (const produced of candidate.graph.produced) {
    state.set(
      produced[LedgerColumns.OUTREF].toString("hex"),
      Buffer.from(produced[LedgerColumns.OUTPUT]),
    );
  }
};

const bindForcedTransitionEffect = async (input: {
  readonly authority: ValidatedEventAuthority;
  readonly state: ReadonlyMap<string, Buffer>;
  readonly phaseAConfig: PhaseAConfig;
  readonly phaseBConfig: PhaseBConfig;
  readonly step: WatcherBlockReplayCommittedStep;
}): Promise<WatcherBlockReplayForcedValidationFact | null> => {
  if (input.authority.phase !== "ForcedTransaction") {
    return null;
  }
  // #517, same class as the acceptance gate above: this used to be a second
  // disjunct of the early `return null`, so a forced step whose canonical
  // native transaction was missing skipped the canonical Phase A/B rerun and
  // the terminal-validity comparison entirely, and the caller applied the
  // authority's effect as if the terminality binding had passed.
  // `validateEventAuthority` already refuses such an authority, so this is
  // unreachable today - which is exactly why it must fail closed rather than
  // silently skip if that invariant is ever relaxed.
  if (input.authority.canonicalNativeTxCbor === null) {
    return fail(
      "transition_effect_semantics_mismatch",
      "$.canonicalNativeTxCbor",
    );
  }
  if (input.authority.terminalClassification === null) {
    return fail(
      "user_event_authority_identity_mismatch",
      "$.userEvent.result.state.snapshot.terminalClassification",
    );
  }
  const nativeTx = decodeMidgardNativeTxFullFromCanonicalCbor(
    input.authority.canonicalNativeTxCbor,
  );
  const queued: QueuedTx = {
    txId: Buffer.from(computeMidgardNativeTxId(nativeTx)),
    txCbor: Buffer.from(input.authority.canonicalNativeTxCbor),
    programMaterialSidecarCbor: input.authority.programMaterialSidecarCbor,
    arrivalSeq: 0n,
    createdAt: new Date(0),
  };
  const phaseA = validatePhaseASingle(queued, input.phaseAConfig);
  let derived = buildCanonicalTransitionEffect([]);
  let phaseAStatus: WatcherBlockReplayForcedValidationFact["phaseAStatus"] =
    "accepted";
  let phaseARejectCode: RejectCode | null = null;
  let phaseBStatus: WatcherBlockReplayForcedValidationFact["phaseBStatus"] =
    "not_run";
  let phaseBRejectCode: RejectCode | null = null;
  let canonicalOperatorValidity: WatcherForcedOperatorVerdict;
  if ("code" in phaseA) {
    phaseAStatus = "rejected";
    phaseARejectCode = phaseA.code;
    canonicalOperatorValidity = watcherBlockReplayForcedValidityForRejectCode(
      phaseA.code,
      "phaseA",
    );
  } else {
    const phaseB = await makeReturn(
      runPhaseBValidationWithPatch(
        [phaseA],
        new Map(
          [...input.state.entries()].map(([outRef, output]) => [
            outRef,
            Buffer.from(output),
          ]),
        ),
        input.phaseBConfig,
      ),
    ).unsafeRun();
    if (phaseB.rejected.length > 0) {
      if (phaseB.rejected.length !== 1 || phaseB.accepted.length !== 0) {
        return fail("canonical_validation_threw", "$.phaseB.forced");
      }
      phaseBStatus = "rejected";
      phaseBRejectCode = phaseB.rejected[0]!.code;
      canonicalOperatorValidity = watcherBlockReplayForcedValidityForRejectCode(
        phaseBRejectCode,
        "phaseB",
      );
    } else {
      if (phaseB.accepted.length !== 1) {
        return fail("canonical_validation_threw", "$.phaseB.forced");
      }
      phaseBStatus = "accepted";
      canonicalOperatorValidity = WATCHER_FORCED_TX_VALID;
      derived = canonicalTransitionEffectFromStatePatch(phaseB.statePatch);
    }
  }
  if (
    input.authority.terminalClassification.operatorValidity !==
    canonicalOperatorValidity
  ) {
    return fail(
      "transition_effect_semantics_mismatch",
      "$.transitionEffect.forced.operatorValidity",
    );
  }
  if (
    derived.digest !== input.authority.effect.digest ||
    !derived.canonicalCbor.equals(input.authority.effect.canonicalCbor)
  ) {
    return fail(
      "transition_effect_semantics_mismatch",
      "$.transitionEffect.forced",
    );
  }
  return Object.freeze({
    eventKeyFingerprint: input.authority.eventKeyFingerprint,
    stepIndex: input.step.stepIndex,
    authenticatedOperatorValidity:
      input.authority.terminalClassification.operatorValidity,
    canonicalOperatorValidity,
    phaseAStatus,
    phaseARejectCode,
    phaseBStatus,
    phaseBRejectCode,
    canonicalEffectDigest: derived.digest,
    canonicalEffectMutationCount: derived.operations.length,
  });
};

/**
 * Replays the authenticated transition sequence exactly. Non-L2 deltas are
 * complete W15/W16 witnesses bound by the committed trace roots; contiguous
 * L2 runs are evaluated by canonical Phase B against the state produced by all
 * preceding events, so event/L2 interleavings cannot observe a stale ledger.
 */
const replayCommittedBlock = async (input: {
  readonly candidates: readonly PhaseAValidatedTx[];
  readonly priorState: readonly WatcherBlockReplayPriorUtxo[];
  readonly expectedPriorStateRoot: string;
  readonly config: PhaseBConfig;
  readonly phaseAConfig: PhaseAConfig;
  readonly committedSteps: readonly WatcherBlockReplayCommittedStep[];
  readonly eventAuthorities: readonly WatcherBlockReplayEventAuthority[];
}): Promise<ReplayCore> => {
  const prior = await watcherBlockReplayPriorState(input.priorState);
  const reasonCodes = new Set<WatcherBlockReplayReasonCode>();
  const stageMismatches: WatcherBlockReplayStageMismatch[] = [];
  if (prior.root !== input.expectedPriorStateRoot) {
    reasonCodes.add("prior_state_root_mismatch");
    stageMismatches.push({
      stage: "prior_state",
      reasonCode: "prior_state_root_mismatch",
      field: "$.header.prevUtxosRoot",
      expected: input.expectedPriorStateRoot,
      actual: prior.root,
    });
    return {
      reasonCodes,
      stageMismatches,
      rejections: [],
      acceptedTxIds: [],
      intermediateRoots: [],
      transactionRoots: [],
      eventRoots: [],
      forcedValidationFacts: [],
      priorStateRoot: prior.root,
      postStateRoot: prior.root,
      authorityManifestDigest: null,
      sourceManifestDigest: null,
      effectManifestDigest: null,
    };
  }

  const steps = [...input.committedSteps].sort(
    (left, right) => left.stepIndex - right.stepIndex,
  );
  const candidateByTxId = new Map(
    input.candidates.map((candidate) => [
      candidate.ledgerTx.txId.toString("hex"),
      candidate,
    ]),
  );
  const indexByTxId = new Map(
    input.candidates.map((candidate, index) => [
      candidate.ledgerTx.txId.toString("hex"),
      index,
    ]),
  );
  const authorityByFingerprint = new Map<string, ValidatedEventAuthority>();
  for (const [index, authority] of input.eventAuthorities.entries()) {
    const fingerprint = eventKeyFingerprint(authority.eventKey);
    if (authorityByFingerprint.has(fingerprint)) {
      return fail(
        "duplicate_event_authority",
        `$.eventAuthorities[${index.toString()}].eventKey`,
      );
    }
    authorityByFingerprint.set(fingerprint, validateEventAuthority(authority));
  }

  const state = new Map(
    prior.ledgerEntries.map((entry) => [
      entry.outRef.toString("hex"),
      Buffer.from(entry.output),
    ]),
  );
  const groups: CommittedReplayGroup[] = [];
  const rejections: WatcherBlockReplayRejection[] = [];
  const acceptedTxIds: string[] = [];
  const seenCandidateIds = new Set<string>();
  const seenEventFingerprints = new Set<string>();
  const forcedValidationFacts: WatcherBlockReplayForcedValidationFact[] = [];

  let pendingL2: WatcherBlockReplayCommittedStep[] = [];
  const flushL2 = async (): Promise<void> => {
    if (pendingL2.length === 0) {
      return;
    }
    const segmentSteps = pendingL2;
    pendingL2 = [];
    const segmentCandidates: PhaseAValidatedTx[] = [];
    for (const step of segmentSteps) {
      const txId = step.txId;
      const candidate = txId === null ? undefined : candidateByTxId.get(txId);
      if (
        candidate === undefined ||
        txId === null ||
        seenCandidateIds.has(txId)
      ) {
        reasonCodes.add("transition_trace_mismatch");
        stageMismatches.push({
          stage: "events",
          reasonCode: "transition_trace_mismatch",
          field: `$.transitionTrace[${step.stepIndex.toString()}].event_key`,
          expected: "one previously-unseen Phase-A-accepted transaction",
          actual: txId ?? "",
        });
        continue;
      }
      seenCandidateIds.add(txId);
      segmentCandidates.push(candidate);
    }
    let phaseB;
    try {
      phaseB = await makeReturn(
        runPhaseBValidationWithPatch(segmentCandidates, state, input.config),
      ).unsafeRun();
    } catch {
      return fail("canonical_validation_threw", "$.phaseB");
    }
    const projected = phaseB.rejected
      .map((rejected) =>
        watcherBlockReplayRejectionProjection({ rejected, indexByTxId }),
      )
      .sort((left, right) => left.index - right.index);
    rejections.push(...projected);
    for (const rejection of projected) {
      if (!REACHABLE_SET.has(rejection.code)) {
        reasonCodes.add("undeclared_reachable_code");
      }
      reasonCodes.add("phase_b_rejection");
    }
    for (const candidate of phaseB.accepted) {
      const txId = candidate.ledgerTx.txId.toString("hex");
      const step = segmentSteps.find((entry) => entry.txId === txId);
      if (step === undefined) {
        return fail("canonical_replay_threw", `$.accepted[${txId}]`);
      }
      const operations = buildLedgerOperations([candidate])[0]!.operations;
      groups.push({
        step,
        txIndex: indexByTxId.get(txId) ?? null,
        txId,
        phase: "L2Transaction",
        eventKeyFingerprint: step.eventKeyFingerprint,
        operations,
      });
      acceptedTxIds.push(txId);
      applyAcceptedCandidate(state, candidate);
    }
  };

  for (const step of steps) {
    if (step.phase === "L2Transaction") {
      pendingL2.push(step);
      continue;
    }
    await flushL2();
    const authority = authorityByFingerprint.get(step.eventKeyFingerprint);
    if (authority === undefined) {
      return fail(
        "missing_event_authority",
        `$.transitionTrace[${step.stepIndex.toString()}].event_key`,
      );
    }
    if (authority.phase !== step.phase) {
      return fail(
        "event_authority_identity_mismatch",
        `$.eventAuthorities[${step.eventKeyFingerprint}].phase`,
      );
    }
    const forcedValidationFact = await bindForcedTransitionEffect({
      authority,
      state,
      phaseAConfig: input.phaseAConfig,
      phaseBConfig: input.config,
      step,
    });
    if (forcedValidationFact !== null) {
      if (
        forcedValidationFacts.some(
          (fact) =>
            fact.eventKeyFingerprint ===
              forcedValidationFact.eventKeyFingerprint ||
            fact.stepIndex === forcedValidationFact.stepIndex,
        )
      ) {
        return fail(
          "duplicate_event_authority",
          `$.forcedValidationFacts[${step.stepIndex.toString()}]`,
        );
      }
      forcedValidationFacts.push(forcedValidationFact);
    }
    seenEventFingerprints.add(step.eventKeyFingerprint);
    groups.push({
      step,
      txIndex: null,
      txId: null,
      phase: step.phase,
      eventKeyFingerprint: step.eventKeyFingerprint,
      operations: eventLedgerOperations(authority),
    });
    applyRawEventOperations(state, authority);
  }
  await flushL2();

  for (const txId of candidateByTxId.keys()) {
    if (!seenCandidateIds.has(txId)) {
      reasonCodes.add("transition_trace_mismatch");
      stageMismatches.push({
        stage: "events",
        reasonCode: "transition_trace_mismatch",
        field: "$.transitionTrace.l2Transactions",
        expected: txId,
        actual: "omitted",
      });
    }
  }
  for (const fingerprint of authorityByFingerprint.keys()) {
    if (!seenEventFingerprints.has(fingerprint)) {
      return fail(
        "event_authority_identity_mismatch",
        `$.eventAuthorities[${fingerprint}]`,
      );
    }
  }

  let mutationSteps;
  try {
    mutationSteps = await buildValidationMachineLedgerMutationSteps({
      initialEntries: prior.ledgerEntries,
      operations: groups.flatMap((group) => group.operations),
    });
  } catch {
    return fail("canonical_replay_threw", "$.intermediateRoots");
  }

  const intermediateRoots: WatcherBlockReplayIntermediateRoot[] = [];
  const transactionRoots: WatcherBlockReplayTransactionRoot[] = [];
  const eventRoots: WatcherBlockReplayEventRoot[] = [];
  let cursor = 0;
  let postStateRoot = prior.root;
  for (const group of groups) {
    const first = cursor;
    for (
      let remaining = group.operations.length;
      remaining > 0;
      remaining -= 1
    ) {
      const mutationStep = mutationSteps[cursor]!;
      intermediateRoots.push(
        Object.freeze({
          sequence: cursor,
          txIndex: group.txIndex,
          txId: group.txId,
          stepIndex: group.step.stepIndex,
          phase: group.phase,
          operation: mutationStep.operation.type,
          outRef: mutationStep.operation.key.toString("hex"),
          preRoot: normalizeRootHex(mutationStep.preRoot.toString("hex")),
          postRoot: normalizeRootHex(mutationStep.postRoot.toString("hex")),
        }),
      );
      cursor += 1;
    }
    const preRoot =
      first === cursor
        ? postStateRoot
        : normalizeRootHex(mutationSteps[first]!.preRoot.toString("hex"));
    postStateRoot =
      first === cursor
        ? postStateRoot
        : normalizeRootHex(mutationSteps[cursor - 1]!.postRoot.toString("hex"));
    if (group.phase === "L2Transaction") {
      transactionRoots.push(
        Object.freeze({
          txIndex: group.txIndex!,
          txId: group.txId!,
          preRoot,
          postRoot: postStateRoot,
          mutationCount: cursor - first,
          committedStepIndex: group.step.stepIndex,
          committedPreRoot: group.step.preRoot,
          committedPostRoot: group.step.postRoot,
        }),
      );
    } else {
      eventRoots.push(
        Object.freeze({
          stepIndex: group.step.stepIndex,
          phase: group.phase,
          eventKeyFingerprint: group.eventKeyFingerprint,
          preRoot,
          postRoot: postStateRoot,
          mutationCount: cursor - first,
        }) as WatcherBlockReplayEventRoot,
      );
    }
  }

  const orderedAuthorities = steps.flatMap((step) => {
    if (step.phase === "L2Transaction") {
      return [];
    }
    const authority = authorityByFingerprint.get(step.eventKeyFingerprint);
    return authority === undefined ? [] : [authority];
  });
  return {
    reasonCodes,
    stageMismatches,
    rejections: rejections.sort((left, right) => left.index - right.index),
    acceptedTxIds,
    intermediateRoots,
    transactionRoots,
    eventRoots,
    forcedValidationFacts,
    priorStateRoot: prior.root,
    postStateRoot,
    authorityManifestDigest: watcherSha256CanonicalJson(
      orderedAuthorities.map((authority) => authority.authorityManifest),
    ),
    sourceManifestDigest: watcherSha256CanonicalJson(
      steps.map((step) => ({
        stepIndex: step.stepIndex,
        phase: step.phase,
        eventKeyFingerprint: step.eventKeyFingerprint,
        preRoot: step.preRoot,
        postRoot: step.postRoot,
        eventToStepIndex: step.eventToStepIndex,
        eventToStepPhase: step.eventToStepPhase,
      })),
    ),
    effectManifestDigest: watcherSha256CanonicalJson(
      orderedAuthorities.map((authority) => authority.effectManifest),
    ),
  };
};

// ---------------------------------------------------------------------------
// Events stage: the committed transition trace
// ---------------------------------------------------------------------------

/**
 * Binds the operator's committed transition trace to the replay.
 *
 * The trace is the operator's own claim about the order the block's events were
 * applied in and about every intermediate root along the way. This is where the
 * watcher's independently recomputed roots meet that claim: the L2 steps must
 * be exactly the accepted transactions in the same order, the chain of
 * committed pre/post roots must run unbroken from the committed
 * `prevUtxosRoot`, and each committed `post_utxos_root` must equal the root the
 * watcher recomputed for that transaction.
 *
 * A non-L2 step is bounded by the same chain, using the complete W15/W16
 * authority-derived effect that the replay core passed through the canonical ledger
 * mutator. This binder does not classify whether the event was due or
 * legitimate; that semantic partition remains W26's responsibility.
 */
const bindCommittedSteps = (input: {
  readonly core: ReplayCore;
  readonly committedSteps: readonly WatcherBlockReplayCommittedStep[];
  readonly expectedPriorStateRoot: string;
}): {
  readonly transactionRoots: readonly WatcherBlockReplayTransactionRoot[];
  readonly stageMismatches: readonly WatcherBlockReplayStageMismatch[];
  readonly reasonCodes: readonly WatcherBlockReplayReasonCode[];
} => {
  const { core, committedSteps } = input;
  const stageMismatches: WatcherBlockReplayStageMismatch[] = [];
  const reasonCodes: WatcherBlockReplayReasonCode[] = [];
  const steps = [...committedSteps].sort(
    (left, right) => left.stepIndex - right.stepIndex,
  );
  const seenFingerprints = new Set<string>();
  for (const [index, step] of steps.entries()) {
    if (
      step.stepIndex !== index ||
      seenFingerprints.has(step.eventKeyFingerprint)
    ) {
      reasonCodes.push("transition_trace_mismatch");
      stageMismatches.push({
        stage: "events",
        reasonCode: "transition_trace_mismatch",
        field: `$.transitionTrace[${index.toString()}].step_index`,
        expected: index.toString(),
        actual: step.stepIndex.toString(),
      });
    }
    seenFingerprints.add(step.eventKeyFingerprint);
    if (
      step.eventToStepIndex !== step.stepIndex ||
      step.eventToStepPhase !== step.phase
    ) {
      reasonCodes.push("transition_trace_mismatch");
      stageMismatches.push({
        stage: "events",
        reasonCode: "transition_trace_mismatch",
        field: `$.transitionTrace[${step.stepIndex.toString()}].event_to_step`,
        expected: `${step.stepIndex.toString()}:${step.phase}`,
        actual: `${(step.eventToStepIndex ?? -1).toString()}:${step.eventToStepPhase ?? ""}`,
      });
    }
  }
  const transactionByStep = new Map(
    core.transactionRoots.map((root) => [root.committedStepIndex, root]),
  );
  const eventByStep = new Map(
    core.eventRoots.map((root) => [root.stepIndex, root]),
  );
  let expectedPreRoot = input.expectedPriorStateRoot;
  for (const step of steps) {
    const field = `$.transitionTrace[${step.stepIndex.toString()}]`;
    const replayed =
      step.phase === "L2Transaction"
        ? transactionByStep.get(step.stepIndex)
        : eventByStep.get(step.stepIndex);
    if (replayed === undefined) {
      reasonCodes.push("transition_trace_mismatch");
      stageMismatches.push({
        stage: "events",
        reasonCode: "transition_trace_mismatch",
        field: `${field}.replayedBoundary`,
        expected: `${step.phase}:${step.eventKeyFingerprint}`,
        actual: "missing",
      });
      continue;
    }
    if (
      step.preRoot !== expectedPreRoot ||
      replayed.preRoot !== expectedPreRoot
    ) {
      reasonCodes.push("transition_trace_mismatch");
      stageMismatches.push({
        stage: "events",
        reasonCode: "transition_trace_mismatch",
        field: `${field}.pre_utxos_root`,
        expected: expectedPreRoot,
        actual: `${step.preRoot}:${replayed.preRoot}`,
      });
    }
    if (step.postRoot !== replayed.postRoot) {
      reasonCodes.push("intermediate_root_mismatch");
      stageMismatches.push({
        stage: "post_state",
        reasonCode: "intermediate_root_mismatch",
        field: `${field}.post_utxos_root`,
        expected: replayed.postRoot,
        actual: step.postRoot,
      });
    }
    expectedPreRoot = step.postRoot;
  }

  return {
    transactionRoots: core.transactionRoots,
    stageMismatches,
    reasonCodes,
  };
};

// ---------------------------------------------------------------------------
// Result assembly
// ---------------------------------------------------------------------------

const finalizeResult = (input: {
  readonly core: ReplayCore;
  readonly context: WatcherBlockReplayContext | null;
  readonly transactionCount: number;
  readonly expectedPriorStateRoot: string;
  readonly expectedPostStateRoot: string | null;
  readonly committedSteps: readonly WatcherBlockReplayCommittedStep[] | null;
}): WatcherBlockReplayResult => {
  const { core } = input;
  const reasonCodes = new Set(core.reasonCodes);
  const stageMismatches = [...core.stageMismatches];
  let transactionRoots: readonly WatcherBlockReplayTransactionRoot[] =
    core.transactionRoots;

  const priorStateFailed = reasonCodes.has("prior_state_root_mismatch");

  // #517. The two bindings below are the only places a recomputed root is
  // compared against something the operator committed, and both are
  // conditional. Deriving `accept` from an empty `reasonCodes` alone was
  // therefore fail-open: a caller that supplied neither a committed transition
  // trace nor an expected post-state root skipped both comparisons, left
  // `reasonCodes` empty, and was stamped `accept` without one committed value
  // ever being checked. These receipts are set only from inside the branch that
  // actually performs each binding, and `accept` is gated on both, so an unrun
  // binding cannot be an acceptance regardless of what the reason-code set
  // happens to contain.
  let committedTraceBound = false;
  let postStateRootBound = false;

  if (!priorStateFailed && input.committedSteps !== null) {
    const bound = bindCommittedSteps({
      core,
      committedSteps: input.committedSteps,
      expectedPriorStateRoot: input.expectedPriorStateRoot,
    });
    transactionRoots = bound.transactionRoots;
    stageMismatches.push(...bound.stageMismatches);
    for (const code of bound.reasonCodes) {
      reasonCodes.add(code);
    }
    committedTraceBound = true;
  }

  if (!priorStateFailed && input.expectedPostStateRoot !== null) {
    postStateRootBound = true;
    if (core.postStateRoot !== input.expectedPostStateRoot) {
      reasonCodes.add("post_state_root_mismatch");
      stageMismatches.push({
        stage: "post_state",
        reasonCode: "post_state_root_mismatch",
        field: "$.header.utxosRoot",
        expected: input.expectedPostStateRoot,
        actual: core.postStateRoot,
      });
    }
  }

  if (!committedTraceBound) {
    reasonCodes.add("committed_trace_binding_unrun");
  }
  if (!postStateRootBound) {
    reasonCodes.add("post_state_binding_unrun");
  }

  // `accept` means root-exact replay only. W26 remains mandatory downstream
  // before classification/decision readiness; W25 does not adjudicate whether
  // an authenticated event was due, omitted, fabricated, or duplicated.
  const action: WatcherBlockReplayAction =
    committedTraceBound && postStateRootBound && reasonCodes.size === 0
      ? "accept"
      : "reject";

  return digestResult({
    schemaVersion: WATCHER_BLOCK_REPLAY_SCHEMA_VERSION,
    action,
    reasonCodes: orderReasonCodes(reasonCodes),
    verifiedRequires: WATCHER_BLOCK_REPLAY_VERIFIED_CONTRACT,
    rejectionSelection: WATCHER_RULE_BUNDLE_REJECTION_SELECTION,
    consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
    ...(input.context ?? NULL_CONTEXT),
    authorityManifestDigest: core.authorityManifestDigest,
    sourceManifestDigest: core.sourceManifestDigest,
    effectManifestDigest: core.effectManifestDigest,
    priorStateRoot: core.priorStateRoot,
    expectedPriorStateRoot: input.expectedPriorStateRoot,
    postStateRoot: core.postStateRoot,
    expectedPostStateRoot: input.expectedPostStateRoot,
    transactionCount: input.transactionCount,
    acceptedCount: core.acceptedTxIds.length,
    acceptedTxIds: Object.freeze([...core.acceptedTxIds]),
    intermediateRoots: Object.freeze([...core.intermediateRoots]),
    transactionRoots: Object.freeze([...transactionRoots]),
    eventRoots: Object.freeze([...core.eventRoots]),
    forcedValidationFacts: Object.freeze([...core.forcedValidationFacts]),
    stageMismatches: orderStageMismatches(stageMismatches),
    rejections: Object.freeze([...core.rejections]),
    selectedRejection: selectRejection(core.rejections),
  });
};

// ---------------------------------------------------------------------------
// Block evaluation
// ---------------------------------------------------------------------------

export type EvaluateWatcherBlockReplayInput = {
  /** L1-authenticated header observation, as W22 and W24 consumed it. */
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  /** The accepted W22 record for this block. */
  readonly reconstruction: WatcherHeaderRootReconstructionResult;
  /** The accepted W24 record for this block. */
  readonly phaseA: WatcherPhaseAVerificationResult;
  /** Exact public `DaPayloadEnvelopeV1` bytes, from the W21 store. */
  readonly payloadEnvelopeCbor: Uint8Array;
  /** Provenance of those bytes; must be public/permissionless DA. */
  readonly daProvenance: EvidenceProvenance;
  /** Prior-state ledger entries, from the W21 records for the parent block. */
  readonly priorState: readonly WatcherBlockReplayPriorUtxo[];
  /** The W23 rule bundle, with its commitment. */
  readonly ruleBundle: WatcherRuleBundle;
  readonly ruleBundleCommitment: string;
  /** Parser-recomputed W15/W16 authorities and shared canonical effects. */
  readonly eventAuthorities?: readonly WatcherBlockReplayEventAuthority[];
  readonly minimumConfirmationDepth?: number;
};

/** Re-checks the caller's W22 record against a fresh canonical recomputation. */
const bindReconstruction = (input: {
  readonly reconstruction: WatcherHeaderRootReconstructionResult;
  readonly headerHash: string;
  readonly payloadEnvelopeSha256: string;
}): void => {
  const { reconstruction } = input;
  if (
    reconstruction.schemaVersion !==
    WATCHER_HEADER_ROOT_RECONSTRUCTION_SCHEMA_VERSION
  ) {
    fail("reconstruction_unsupported_schema", "$.reconstruction.schemaVersion");
  }
  const { resultDigest, ...withoutDigest } = reconstruction;
  if (watcherSha256CanonicalJson(withoutDigest) !== resultDigest) {
    fail("reconstruction_digest_mismatch", "$.reconstruction.resultDigest");
  }
  if (
    reconstruction.action !== "accept" ||
    reconstruction.reconstructedRoots === null ||
    reconstruction.reconstructedCounts === null ||
    reconstruction.reasonCodes.length !== 0
  ) {
    fail("reconstruction_not_accepted", "$.reconstruction.action");
  }
  if (reconstruction.headerHash !== input.headerHash) {
    fail("reconstruction_header_mismatch", "$.reconstruction.headerHash");
  }
  if (reconstruction.payloadEnvelopeSha256 !== input.payloadEnvelopeSha256) {
    fail("payload_bytes_mismatch", "$.reconstruction.payloadEnvelopeSha256");
  }
};

/**
 * Re-checks the caller's W24 record the same way, and additionally requires it
 * to describe this exact block, this exact reconstruction, and this exact rule
 * bundle. A Phase A record that rejected anything stops the replay: replaying a
 * block whose transactions Phase A already refused would attribute Phase B
 * faults to a block that never had a valid candidate set.
 */
const bindPhaseAV1 = (input: {
  readonly phaseA: WatcherPhaseAVerificationResult;
  readonly headerHash: string;
  readonly payloadEnvelopeSha256: string;
  readonly reconstructionDigest: string;
  readonly ruleBundleCommitment: string;
}): void => {
  const { phaseA } = input;
  if (phaseA.schemaVersion !== WATCHER_PHASE_A_VERIFIER_SCHEMA_VERSION) {
    fail("phase_a_unsupported_schema", "$.phaseA.schemaVersion");
  }
  const { resultDigest, ...withoutDigest } = phaseA;
  if (watcherSha256CanonicalJson(withoutDigest) !== resultDigest) {
    fail("phase_a_digest_mismatch", "$.phaseA.resultDigest");
  }
  if (
    phaseA.action !== "accept" ||
    phaseA.rejections.length !== 0 ||
    phaseA.reasonCodes.length !== 0
  ) {
    fail("phase_a_not_accepted", "$.phaseA.action");
  }
  if (
    phaseA.headerHash !== input.headerHash ||
    phaseA.payloadEnvelopeSha256 !== input.payloadEnvelopeSha256 ||
    phaseA.reconstructionDigest !== input.reconstructionDigest ||
    phaseA.ruleBundleCommitment !== input.ruleBundleCommitment
  ) {
    fail("phase_a_context_mismatch", "$.phaseA.headerHash");
  }
};

/**
 * Projects the canonical reconstruction's transition trace and event-to-step
 * map into the committed-step shape the events stage binds. Every field is
 * copied from the canonical decode; nothing is re-derived.
 */
export const watcherBlockReplayCommittedSteps = (input: {
  readonly transitionTrace: readonly {
    readonly key: bigint;
    readonly value: TransitionStep;
  }[];
  readonly eventToStep: readonly {
    readonly key: EventKey;
    readonly value: EventToStepValue;
  }[];
}): readonly WatcherBlockReplayCommittedStep[] => {
  const eventToStepByFingerprint = new Map<string, EventToStepValue>();
  for (const [index, entry] of input.eventToStep.entries()) {
    const fingerprint = eventKeyFingerprint(entry.key);
    if (eventToStepByFingerprint.has(fingerprint)) {
      fail(
        "transition_trace_mismatch",
        `$.eventToStep[${index.toString()}].eventKey`,
      );
    }
    eventToStepByFingerprint.set(fingerprint, entry.value);
  }
  const orderedTrace = [...input.transitionTrace].sort((left, right) =>
    left.key < right.key ? -1 : left.key > right.key ? 1 : 0,
  );
  if (eventToStepByFingerprint.size !== orderedTrace.length) {
    fail("transition_trace_mismatch", "$.eventToStep.length");
  }
  const seenTraceFingerprints = new Set<string>();
  return Object.freeze(
    orderedTrace.map((entry, index) => {
      const fingerprint = eventKeyFingerprint(entry.value.event_key);
      const mapped =
        eventToStepByFingerprint.get(fingerprint) ??
        fail(
          "transition_trace_mismatch",
          `$.transitionTrace[${index.toString()}].eventToStep`,
        );
      if (
        entry.key !== BigInt(index) ||
        entry.value.step_index !== BigInt(index) ||
        entry.value.phase !== phaseForEventKey(entry.value.event_key) ||
        seenTraceFingerprints.has(fingerprint) ||
        mapped.step_index !== entry.value.step_index ||
        mapped.phase !== entry.value.phase
      ) {
        fail(
          "transition_trace_mismatch",
          `$.transitionTrace[${index.toString()}]`,
        );
      }
      seenTraceFingerprints.add(fingerprint);
      return Object.freeze({
        stepIndex: Number(entry.value.step_index),
        phase: entry.value.phase,
        txId: eventKeyTxId(entry.value.event_key),
        eventKeyFingerprint: fingerprint,
        preRoot: entry.value.pre_utxos_root,
        postRoot: entry.value.post_utxos_root,
        eventToStepIndex: Number(mapped.step_index),
        eventToStepPhase: mapped.phase,
      });
    }),
  );
};

/**
 * The W25 entry point: an accepted W22 reconstruction, an accepted W24 Phase A
 * record, the exact W21 block bytes, the W21 prior-state material, and the W23
 * rule bundle produce a frozen, digest-bound record of the canonical Phase B
 * replay of the block - prior state, dependencies, spends, references, scripts,
 * value, events, every intermediate root, and the exact post state.
 */
export const evaluateWatcherBlockReplay = async (
  input: EvaluateWatcherBlockReplayInput,
): Promise<WatcherBlockReplayResult> => {
  let evidence;
  try {
    evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: input.observation,
      payloadEnvelopeCbor: input.payloadEnvelopeCbor,
      daProvenance: input.daProvenance,
      ...(input.minimumConfirmationDepth === undefined
        ? {}
        : { minimumConfirmationDepth: input.minimumConfirmationDepth }),
    });
  } catch {
    return admitFullBlockReplayResult(
      errorResult(["canonical_reconstruction_failed"], null, 0),
    );
  }

  const context: WatcherBlockReplayContext = Object.freeze({
    headerHash: evidence.headerHash,
    payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
    payloadSha256: evidence.payloadSha256,
    reconstructionDigest: input.reconstruction.resultDigest,
    phaseAResultDigest: input.phaseA.resultDigest,
    ruleBundleCommitment: input.ruleBundleCommitment,
  });
  const transactionCount = evidence.reconstruction.transactions.length;

  let candidates: readonly PhaseAValidatedTx[];
  let committedSteps: readonly WatcherBlockReplayCommittedStep[];
  let phaseAConfig: PhaseAConfig;
  let config: PhaseBConfig;
  try {
    bindReconstruction({
      reconstruction: input.reconstruction,
      headerHash: evidence.headerHash,
      payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
    });
    bindPhaseAV1({
      phaseA: input.phaseA,
      headerHash: evidence.headerHash,
      payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
      reconstructionDigest: input.reconstruction.resultDigest,
      ruleBundleCommitment: input.ruleBundleCommitment,
    });
    // The Phase A configuration binding (rule bundle profile, header protocol
    // version) is W24's, reused unchanged rather than restated.
    phaseAConfig = makeWatcherPhaseAConfig({
      header: evidence.header,
      ruleBundle: input.ruleBundle,
    });
    const queuedTxs: readonly QueuedTx[] = watcherPhaseAQueuedTxs({
      transactions: evidence.reconstruction.transactions.map((entry) =>
        Object.freeze({ txId: entry.txId, txCbor: entry.fullTransactionCbor }),
      ),
      programMaterial: evidence.reconstruction.payload.block_body
        .cek_program_material as readonly (readonly [string, string])[],
    });
    candidates = deriveCandidates(queuedTxs, phaseAConfig, input.phaseA);
    committedSteps = watcherBlockReplayCommittedSteps({
      transitionTrace: evidence.reconstruction.transitionTrace,
      eventToStep: evidence.reconstruction.eventToStep,
    });
    config = makeWatcherPhaseBConfig(evidence.header);
  } catch (error) {
    return admitFullBlockReplayResult(
      errorResult([reasonCodeOf(error)], context, transactionCount),
    );
  }

  try {
    const core = await replayCommittedBlock({
      candidates,
      priorState: input.priorState,
      expectedPriorStateRoot: evidence.header.prevUtxosRoot,
      config,
      phaseAConfig,
      committedSteps,
      eventAuthorities: input.eventAuthorities ?? [],
    });
    return admitFullBlockReplayResult(
      finalizeResult({
        core,
        context,
        transactionCount,
        expectedPriorStateRoot: evidence.header.prevUtxosRoot,
        expectedPostStateRoot: evidence.header.utxosRoot,
        committedSteps,
      }),
    );
  } catch (error) {
    return admitFullBlockReplayResult(
      errorResult([reasonCodeOf(error)], context, transactionCount),
    );
  }
};

/**
 * Re-derives the canonical Phase A candidates the replay needs.
 *
 * W24's record carries verdicts, not the `PhaseAValidatedTx` values Phase B
 * consumes, so the candidates are produced by the same canonical function W24
 * used (`validatePhaseASingle`) over the same canonical inputs (W24's own
 * `watcherPhaseAQueuedTxs` derivation and `makeWatcherPhaseAConfig`
 * configuration). The result is then required to agree with W24's accepted list
 * exactly, so the two lanes cannot silently disagree about what the block
 * contains.
 */
const deriveCandidates = (
  queuedTxs: readonly QueuedTx[],
  config: Parameters<typeof validatePhaseASingle>[1],
  phaseA: WatcherPhaseAVerificationResult,
): readonly PhaseAValidatedTx[] => {
  const candidates: PhaseAValidatedTx[] = [];
  for (const [index, queuedTx] of queuedTxs.entries()) {
    let outcome;
    try {
      outcome = validatePhaseASingle(queuedTx, config);
    } catch {
      return fail("canonical_validation_threw", `$.candidates[${index}]`);
    }
    if (!("ledgerTx" in outcome)) {
      return fail("phase_a_candidate_mismatch", `$.candidates[${index}]`);
    }
    candidates.push(outcome);
  }
  const derivedTxIds = candidates.map((candidate) =>
    candidate.ledgerTx.txId.toString("hex"),
  );
  if (
    derivedTxIds.length !== phaseA.acceptedTxIds.length ||
    derivedTxIds.some((txId, index) => txId !== phaseA.acceptedTxIds[index])
  ) {
    fail("phase_a_candidate_mismatch", "$.phaseA.acceptedTxIds");
  }
  return Object.freeze(candidates);
};

// ---------------------------------------------------------------------------
// Durable record
// ---------------------------------------------------------------------------

/**
 * Canonical CBOR commitment persisted as the replayed state:
 * `[ header_hash, prior_state_root, post_state_root, [ per-transaction post
 * roots in canonical accepted order ] ]`. Unlike W22's record, which commits
 * the roots the header *claims*, this commits the roots the replay
 * *recomputed*, which is what makes the two records worth holding separately.
 */
const replayedStateBytes = (result: WatcherBlockReplayResult): Buffer =>
  encodeCborArrayRaw([
    encodeCborBytes(Buffer.from(result.headerHash as string, "hex")),
    encodeCborBytes(Buffer.from(result.priorStateRoot as string, "hex")),
    encodeCborBytes(Buffer.from(result.postStateRoot as string, "hex")),
    encodeCborArrayRaw(
      result.transactionRoots.map((entry) =>
        encodeCborBytes(Buffer.from(entry.postRoot, "hex")),
      ),
    ),
  ]);

export type WatcherBlockReplayRecordErrorCode =
  | "invalid_input_ids"
  | "result_not_accepted"
  | "unsupported_schema";

export class WatcherBlockReplayRecordError extends Error {
  readonly code: WatcherBlockReplayRecordErrorCode;
  readonly path: string;

  constructor(code: WatcherBlockReplayRecordErrorCode, path: string) {
    super(`${code}: ${path}`);
    this.name = "WatcherBlockReplayRecordError";
    this.code = code;
    this.path = path;
  }
}

const failRecord = (
  code: WatcherBlockReplayRecordErrorCode,
  path: string,
): never => {
  throw new WatcherBlockReplayRecordError(code, path);
};

/**
 * Builds the W03-reserved `WatcherReconstructedState` record for a completed
 * replay.
 *
 * This is the sibling of `makeWatcherHeaderRootReconstructedStateV1`, not a
 * duplicate of it: W22's record binds the header's claimed roots, this one
 * binds the post state the replay actually produced. Both are needed, because
 * the whole point of W25 is that the two can disagree.
 */
export const makeWatcherBlockReplayReconstructedState = (input: {
  readonly result: WatcherBlockReplayResult;
  readonly chainPointId: string;
  readonly inputIds: readonly string[];
}): WatcherReconstructedState => {
  const result = input.result;
  if (result.schemaVersion !== WATCHER_BLOCK_REPLAY_SCHEMA_VERSION) {
    failRecord("unsupported_schema", "$.result.schemaVersion");
  }
  if (
    result.action !== "accept" ||
    result.headerHash === null ||
    result.priorStateRoot === null ||
    result.postStateRoot === null ||
    !HEX_32.test(result.postStateRoot)
  ) {
    failRecord("result_not_accepted", "$.result.action");
  }
  if (typeof input.chainPointId !== "string" || input.chainPointId === "") {
    failRecord("invalid_input_ids", "$.chainPointId");
  }
  if (input.inputIds.length === 0) {
    failRecord("invalid_input_ids", "$.inputIds");
  }
  const seen = new Set<string>();
  for (const [index, inputId] of input.inputIds.entries()) {
    if (typeof inputId !== "string" || inputId === "" || seen.has(inputId)) {
      failRecord("invalid_input_ids", `$.inputIds[${index.toString()}]`);
    }
    seen.add(inputId);
  }
  return Object.freeze({
    blockHash: result.headerHash as string,
    chainPointId: input.chainPointId,
    priorStateRoot: result.priorStateRoot as string,
    postStateRoot: result.postStateRoot as string,
    inputIds: Object.freeze([...input.inputIds]),
    state: makeWatcherDurablePayload(
      replayedStateBytes(result).toString("hex"),
    ),
  });
};

/**
 * The canonical phase enumeration, re-exported as a frozen list so a suite can
 * assert the stage map covers exactly the Phase-B-reachable phases and no
 * others. Declared here rather than imported at the use site so the assertion
 * and the map cannot drift apart.
 */
export const WATCHER_BLOCK_REPLAY_CANONICAL_PHASES = Object.freeze(
  Object.keys(MidgardValidationPhase) as readonly MidgardValidationPhaseName[],
);

/** Codes this lane claims, published as the CG3 waiver requires. */
export const WATCHER_BLOCK_REPLAY_CLAIMED_REJECT_CODES =
  WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES;

/**
 * The union of the codes W24 and W25 claim plus the codes both disclaim must be
 * the whole canonical vocabulary. Exposed as data so the suite can assert
 * totality rather than restate the arithmetic.
 */
export const WATCHER_BLOCK_REPLAY_PROTOCOL_MINUS_UNCLAIMED = Object.freeze(
  WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES.filter(
    (code) =>
      !(code in WATCHER_BLOCK_REPLAY_UNCLAIMED_REJECT_CODE_JUSTIFICATIONS),
  ),
);
