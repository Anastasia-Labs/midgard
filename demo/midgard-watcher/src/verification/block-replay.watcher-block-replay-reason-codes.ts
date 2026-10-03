import type { MidgardValidationPhaseName } from "@al-ft/midgard-core/validation-trace";
import { RejectCodes } from "@al-ft/midgard-validation/types";

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

export const STAGE_ORDER: ReadonlyMap<WatcherBlockReplayStage, number> =
  new Map(WATCHER_BLOCK_REPLAY_STAGES.map((stage, index) => [stage, index]));

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
 * The 14 canonical codes the Phase B pipeline can emit, in `RejectCodes`
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
 * `E_PLUTUS_SCRIPT_INVALID` (:629, :717, :729), `E_ASSET_COUNT` (the
 * ValueAndMint walk, phase-b.check-value-and-mint.ts),
 * `E_CEK_PROGRAM_MATERIAL` (:247, :551).
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
  RejectCodes.AssetCount,
  RejectCodes.CekProgramMaterial,
] as const);

export const REACHABLE_SET: ReadonlySet<string> = new Set<string>(
  WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES,
);

/**
 * The 12 reachable codes the W25 suite pins with a deterministic
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
  RejectCodes.AssetCount,
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
 * The 26 canonical codes W24's Phase A verifier owns.
 *
 * Group justification, which is the same for every member and is why they are
 * published as a group rather than with 26 near-identical lines: a transaction
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
      "no call site in phase-b.ts: the pipeline always has an evaluator - config.evaluateProofScript when supplied, otherwise the in-process structural CEK executor this lane uses - and every evaluation failure path, including a thrown CEK error, is mapped to E_PLUTUS_SCRIPT_INVALID. The code is therefore not environment-dependent for the watcher: there is no watcher configuration under which the canonical pipeline can report evaluation as unavailable, because the watcher never injects a remote or optional evaluator.",
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
