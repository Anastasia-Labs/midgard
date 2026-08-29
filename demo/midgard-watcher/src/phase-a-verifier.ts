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
 *    `WATCHER_PHASE_A_REACHABLE_REJECT_CODES_V1` (32) and
 *    `WATCHER_PHASE_A_EXCLUDED_REJECT_CODES_V1` (18), each excluded code
 *    carrying a one-line justification in
 *    `WATCHER_PHASE_A_EXCLUDED_REJECT_CODE_JUSTIFICATIONS_V1`. The partition is
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
 * caller-supplied list. They come from `canonicalBlockEvidenceFromVerifiedPayloadV1`
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
 * digest-bound with `watcherSha256CanonicalJsonV1`, so two runs over the same
 * bytes produce the same `resultDigest`.
 */

import { canonicalBlockEvidenceFromVerifiedPayloadV1 } from "@al-ft/midgard-fault-proofs";
import type {
  AuthenticatedStateQueueHeaderObservationV1,
  EvidenceProvenanceV1,
  HeaderV1,
} from "@al-ft/midgard-sdk";
import { validatePhaseASingle } from "@al-ft/midgard-validation/phase-a";
import type {
  PhaseAConfig,
  QueuedTx,
  RejectCode,
  RejectedTx,
} from "@al-ft/midgard-validation/types";
import { RejectCodes } from "@al-ft/midgard-validation/types";

import {
  decodeMidgardCekProgramMaterialDaEntryV1,
  encodeMidgardCekProgramMaterialSidecarV1,
  type MidgardCekProgramMaterialEntryV1,
  verifyMidgardCekProgramMaterialBundleV1,
} from "../../midgard-core/src/cek-proof.js";
import { decodeMidgardNativeTxFullV1FromCanonicalCbor } from "../../midgard-core/src/codec/native.js";
import {
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
  MIDGARD_CONSENSUS_PROFILE_V1_ID,
  MIDGARD_PROTOCOL_V1_VERSION,
} from "../../midgard-core/src/consensus-profile-v1.js";
import { collectMidgardV1AttachedProgramEnvelopes } from "../../midgard-core/src/script-proof.js";
import type { MidgardValidationPhaseName } from "../../midgard-core/src/validation-trace.js";
import { watcherSha256CanonicalJsonV1 } from "./durable-store.js";
import type { WatcherHeaderRootReconstructionResultV1 } from "./header-root-reconstruction.js";
import { WATCHER_HEADER_ROOT_RECONSTRUCTION_V1_SCHEMA_VERSION } from "./header-root-reconstruction.js";
import {
  WATCHER_RULE_BUNDLE_V1_REJECTION_SELECTION,
  WATCHER_RULE_BUNDLE_V1_VALIDATION_PHASE_PRIORITY,
  type WatcherRuleBundleV1,
} from "./rule-bundle-v1.js";

export const WATCHER_PHASE_A_VERIFIER_V1_SCHEMA_VERSION =
  "midgard-watcher-phase-a-verifier-v1" as const;

// ---------------------------------------------------------------------------
// Published rejection vocabulary (CG3 waiver condition b)
// ---------------------------------------------------------------------------

/**
 * The codes `validatePhaseASingle` returns directly, in canonical
 * `RejectCodes` declaration order (types.ts:19-69).
 *
 * Provenance, one entry per `reject(...)` call site in phase-a.ts:
 * `E_CBOR_DESERIALIZATION`/`E_INVALID_OUTPUT`/`E_INVALID_FIELD_TYPE` (:424-427,
 * :377, :420), `E_TX_HASH_MISMATCH` (:435), `E_EMPTY_INPUTS` (:201),
 * `E_DUPLICATE_INPUT_IN_TX` (:217, :237, :249), `E_MIN_FEE` (:513),
 * `E_MISSING_REQUIRED_WITNESS` (:331), `E_INVALID_SIGNATURE` (:313),
 * `E_NATIVE_SCRIPT_INVALID` (:358), `E_INVALID_VALIDITY_INTERVAL_FORMAT`
 * (:268, :277), `E_NETWORK_ID_MISMATCH` (:502), `E_TX_VERSION` (:447 for a
 * non-V1 consensus profile), `E_CEK_PROGRAM_MATERIAL` (:456, :487).
 */
export const WATCHER_PHASE_A_DIRECT_REJECT_CODES_V1 = Object.freeze([
  RejectCodes.CborDeserialization,
  RejectCodes.TxHashMismatch,
  RejectCodes.EmptyInputs,
  RejectCodes.DuplicateInputInTx,
  RejectCodes.InvalidOutput,
  RejectCodes.InvalidFieldType,
  RejectCodes.InvalidValidityIntervalFormat,
  RejectCodes.MinFee,
  RejectCodes.MissingRequiredWitness,
  RejectCodes.InvalidSignature,
  RejectCodes.NativeScriptInvalid,
  RejectCodes.NetworkIdMismatch,
  RejectCodes.TxVersion,
  RejectCodes.CekProgramMaterial,
] as const);

/**
 * The codes `consensusProfileRejectCode` (phase-a.ts:73-116) maps the 19
 * `MidgardConsensusV1ViolationCode` values onto, in canonical `RejectCodes`
 * declaration order. `E_TX_VERSION` is also a direct code, so the two lists
 * overlap by exactly one member.
 */
export const WATCHER_PHASE_A_CONSENSUS_REJECT_CODES_V1 = Object.freeze([
  RejectCodes.IsValidFalseForbidden,
  RejectCodes.AuxDataForbidden,
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

/** The full canonical vocabulary, in `RejectCodes` declaration order. */
export const WATCHER_PHASE_A_CANONICAL_REJECT_CODES_V1 = Object.freeze(
  Object.values(RejectCodes),
);

const REACHABLE_SET: ReadonlySet<string> = new Set<string>([
  ...WATCHER_PHASE_A_DIRECT_REJECT_CODES_V1,
  ...WATCHER_PHASE_A_CONSENSUS_REJECT_CODES_V1,
]);

/**
 * The 32 canonical codes Phase A can produce, in `RejectCodes` declaration
 * order so the published table and every emitted list are stable.
 */
export const WATCHER_PHASE_A_REACHABLE_REJECT_CODES_V1 = Object.freeze(
  WATCHER_PHASE_A_CANONICAL_REJECT_CODES_V1.filter((code) =>
    REACHABLE_SET.has(code),
  ),
);

/** The 18 canonical codes Phase A cannot produce. */
export const WATCHER_PHASE_A_EXCLUDED_REJECT_CODES_V1 = Object.freeze(
  WATCHER_PHASE_A_CANONICAL_REJECT_CODES_V1.filter(
    (code) => !REACHABLE_SET.has(code),
  ),
);

/**
 * The 21 reachable codes the W24 suite pins with a deterministic
 * rejection-evidence case, in `RejectCodes` declaration order.
 *
 * The suite asserts this list equals the set of codes its evidence corpus
 * actually produced, so it cannot drift into an unbacked claim.
 */
export const WATCHER_PHASE_A_EVIDENCED_REJECT_CODES_V1 = Object.freeze([
  RejectCodes.CborDeserialization,
  RejectCodes.TxHashMismatch,
  RejectCodes.EmptyInputs,
  RejectCodes.DuplicateInputInTx,
  RejectCodes.InvalidOutput,
  RejectCodes.InvalidFieldType,
  RejectCodes.InvalidValidityIntervalFormat,
  RejectCodes.MinFee,
  RejectCodes.MissingRequiredWitness,
  RejectCodes.InvalidSignature,
  RejectCodes.NativeScriptInvalid,
  RejectCodes.IsValidFalseForbidden,
  RejectCodes.AuxDataForbidden,
  RejectCodes.NetworkIdMismatch,
  RejectCodes.TxVersion,
  RejectCodes.TxSize,
  RejectCodes.ValueSize,
  RejectCodes.FieldPreimageSize,
  RejectCodes.LedgerOutputSize,
  RejectCodes.ScriptProgramEncoding,
  RejectCodes.CekProgramMaterial,
] as const);

const EVIDENCED_SET: ReadonlySet<string> = new Set<string>(
  WATCHER_PHASE_A_EVIDENCED_REJECT_CODES_V1,
);

/**
 * Reachable in the canonical control flow but not producible by any canonical
 * V1 transaction: an earlier canonical rule always fires first, so no input
 * exists that reaches the check.
 */
export const WATCHER_PHASE_A_DOMINATED_REJECT_CODES_V1 = Object.freeze(
  WATCHER_PHASE_A_REACHABLE_REJECT_CODES_V1.filter(
    (code) => !EVIDENCED_SET.has(code),
  ),
);

/**
 * One line per dominated code, naming the canonical rule that always fires
 * first. Each claim is pinned by an "adjacent boundary" case in the W24 suite
 * that shows the dominating code, not the dominated one.
 */
export const WATCHER_PHASE_A_DOMINATED_REJECT_CODE_JUSTIFICATIONS_V1 =
  Object.freeze({
    [RejectCodes.InputCount]:
      "dominated: 16,385 spend inputs need far more than the 32,768-byte spend-inputs preimage bound, and short-enough items fail canonical out-ref decoding first (E_INVALID_FIELD_TYPE).",
    [RejectCodes.ReferenceInputCount]:
      "dominated: same bound as spend inputs; the reference-input preimage or canonical out-ref decoding fires first.",
    [RejectCodes.OutputCount]:
      "dominated: 16,385 outputs exceed the 32,768-byte outputs preimage bound, and empty items fail canonical output decoding first (E_INVALID_OUTPUT).",
    [RejectCodes.AddressWitnessCount]:
      "dominated: 16,385 vkey witnesses exceed the 32,768-byte address-witnesses preimage bound (E_FIELD_PREIMAGE_SIZE).",
    [RejectCodes.RequiredSignerCount]:
      "dominated: required signers must be 28 bytes each, so 16,385 of them exceed the 32,768-byte preimage bound (E_FIELD_PREIMAGE_SIZE / E_INVALID_FIELD_TYPE).",
    [RejectCodes.ScriptExecutionCount]:
      "dominated: 16,385 redeemers exceed the 32,768-byte redeemers preimage bound, and degenerate items fail canonical redeemer decoding first.",
    [RejectCodes.ObserverCount]:
      "dominated: observers are 28-byte credentials, so 16,385 of them exceed the 32,768-byte preimage bound (E_FIELD_PREIMAGE_SIZE / E_INVALID_FIELD_TYPE).",
    [RejectCodes.AssetCount]:
      "dominated: 16,385 distinct assets cannot fit the 32,768-byte outputs and mint preimage bounds (E_FIELD_PREIMAGE_SIZE), and a single large value hits E_VALUE_SIZE at 5,000 bytes first.",
    [RejectCodes.ScriptProgramSize]:
      "dominated: consensus-validation-v1.ts has no E_SCRIPT_PROGRAM_SIZE call site; oversized programs surface as E_SCRIPT_PROGRAM_ENCODING from the bounded envelope decoder.",
    [RejectCodes.NativeScriptDepth]:
      "dominated: the canonical native-script encoder refuses nesting past the V1 maximum, so no canonical transaction can carry an over-deep script.",
    [RejectCodes.NativeScriptNodeCount]:
      "dominated: 16,385 native-script nodes exceed the 32,768-byte script-witnesses preimage bound (E_FIELD_PREIMAGE_SIZE).",
  } as const);

/**
 * One line per excluded code, as CG3 waiver condition (b) requires.
 *
 * "Phase B" here means W25's dependency-aware replay, which is the only place
 * the corresponding predicate exists. "No Phase A call site" means the code is
 * declared in the shared vocabulary but no `reject(...)` in phase-a.ts and no
 * `consensusProfileRejectCode` case can emit it, so a watcher-side check would
 * be a new, looser rule rather than a reuse.
 */
export const WATCHER_PHASE_A_EXCLUDED_REJECT_CODE_JUSTIFICATIONS_V1 =
  Object.freeze({
    [RejectCodes.UnsupportedFieldNonEmpty]:
      "no Phase A call site: V1 canonical decoding rejects unsupported fields structurally, so the code is never constructed.",
    [RejectCodes.InputNotFound]:
      "Phase B (W25): resolving a spend input requires the prior ledger state Phase A does not have.",
    [RejectCodes.DoubleSpend]:
      "Phase B (W25): cross-transaction spend conflicts are only visible once the block's dependency graph is built.",
    [RejectCodes.DependencyCycle]:
      "Phase B (W25): cycles are a property of the block-wide dependency graph, not of a single transaction.",
    [RejectCodes.DependsOnRejectedTx]:
      "Phase B (W25): depends on which sibling transactions were already rejected.",
    [RejectCodes.ValidityIntervalMismatch]:
      "Phase B (W25): comparing the interval against the block's time bounds needs the header time context Phase A does not apply.",
    [RejectCodes.MinAda]:
      "Phase B (W25): the minimum-Ada predicate runs over resolved output descriptors in value accounting after Phase A has committed the output bytes.",
    [RejectCodes.ValueNotPreserved]:
      "Phase B (W25): value preservation needs resolved input values.",
    [RejectCodes.PlutusScriptInvalid]:
      "Phase B (W25): script execution runs after inputs, references, and script sources are resolved.",
    [RejectCodes.PlutusEvaluationUnavailable]:
      "Phase B (W25): only the evaluating phase can report that evaluation was unavailable.",
    [RejectCodes.CertificatesForbidden]:
      "no Phase A call site: V1 canonical transactions have no certificate field to carry, so the prohibition is structural.",
    [RejectCodes.NonZeroWithdrawal]:
      "no Phase A call site: V1 canonical transactions have no withdrawal field, so the prohibition is structural.",
    [RejectCodes.DatumSize]:
      "no Phase A call site: datum bounds are enforced by the output/ledger-output preimage bounds that surface as E_LEDGER_OUTPUT_SIZE.",
    [RejectCodes.ScriptProgramAggregateSize]:
      "no Phase A call site: the aggregate program bound is checked by the CEK bundle verifier and surfaces as E_CEK_PROGRAM_MATERIAL.",
    [RejectCodes.RedeemerSize]:
      "no Phase A call site: the redeemer preimage bound surfaces as E_FIELD_PREIMAGE_SIZE.",
    [RejectCodes.MintForbidden]:
      "no Phase A call site: minting is an enabled V1 feature, so no prohibition fires.",
    [RejectCodes.ReferenceInputForbidden]:
      "no Phase A call site: reference inputs are an enabled V1 feature, so no prohibition fires.",
    [RejectCodes.ScriptFeatureForbidden]:
      "no Phase A call site: the V1 feature set enables the script features, so no prohibition fires.",
  } as const);

// ---------------------------------------------------------------------------
// Reason codes and errors
// ---------------------------------------------------------------------------

/** Every reason this evaluation can report, in a fixed total order. */
export const WATCHER_PHASE_A_VERIFIER_REASON_CODES_V1 = [
  "reconstruction_not_accepted",
  "reconstruction_unsupported_schema",
  "reconstruction_digest_mismatch",
  "reconstruction_header_mismatch",
  "reconstruction_root_mismatch",
  "payload_bytes_mismatch",
  "rule_bundle_profile_mismatch",
  "header_protocol_version_mismatch",
  "canonical_reconstruction_failed",
  "malformed_program_material",
  "canonical_validation_threw",
  "unknown_reject_code",
  "undeclared_reachable_code",
  "missing_rejection_stage",
  "rejection_tx_id_mismatch",
  "phase_a_rejection",
] as const;

export type WatcherPhaseAVerifierReasonCodeV1 =
  (typeof WATCHER_PHASE_A_VERIFIER_REASON_CODES_V1)[number];

export class WatcherPhaseAVerifierError extends Error {
  readonly code: WatcherPhaseAVerifierReasonCodeV1;
  readonly path: string;

  constructor(code: WatcherPhaseAVerifierReasonCodeV1, path: string) {
    super(`${code}: ${path}`);
    this.name = "WatcherPhaseAVerifierError";
    this.code = code;
    this.path = path;
  }
}

const fail = (code: WatcherPhaseAVerifierReasonCodeV1, path: string): never => {
  throw new WatcherPhaseAVerifierError(code, path);
};

// ---------------------------------------------------------------------------
// Result shape
// ---------------------------------------------------------------------------

export type WatcherPhaseARejectionV1 = Readonly<{
  /** Position in the canonical block transaction order. */
  index: number;
  /** 32-byte canonical transaction id, lowercase hex. */
  txId: string;
  /** Exact canonical `RejectCode`, copied unchanged. */
  code: RejectCode;
  /** Exact canonical `consensusPhase`, copied unchanged. */
  stage: MidgardValidationPhaseName;
  /** Index of `stage` in the W23 rule bundle's validation phase priority. */
  stagePriority: number;
  /** Exact canonical `detail`, copied unchanged. */
  detail: string | null;
}>;

export type WatcherPhaseAVerificationResultV1 = Readonly<{
  schemaVersion: typeof WATCHER_PHASE_A_VERIFIER_V1_SCHEMA_VERSION;
  action: "accept" | "reject" | "error";
  reasonCodes: readonly WatcherPhaseAVerifierReasonCodeV1[];
  /** W23 rejection-selection rule that produced `selectedRejection`. */
  rejectionSelection: typeof WATCHER_RULE_BUNDLE_V1_REJECTION_SELECTION;
  consensusProfileId: typeof MIDGARD_CONSENSUS_PROFILE_V1_ID;
  /** Null for a queued-transaction evaluation with no block context. */
  headerHash: string | null;
  payloadEnvelopeSha256: string | null;
  payloadSha256: string | null;
  reconstructionDigest: string | null;
  ruleBundleCommitment: string | null;
  transactionCount: number;
  acceptedCount: number;
  /** Accepted transaction ids in canonical block order. */
  acceptedTxIds: readonly string[];
  /** Rejections in canonical block order (ascending `index`). */
  rejections: readonly WatcherPhaseARejectionV1[];
  /** The W23-priority first fault: lowest `stagePriority`, then `index`. */
  selectedRejection: WatcherPhaseARejectionV1 | null;
  resultDigest: string;
}>;

const digestResult = (
  result: Omit<WatcherPhaseAVerificationResultV1, "resultDigest">,
): WatcherPhaseAVerificationResultV1 =>
  Object.freeze({
    ...result,
    resultDigest: watcherSha256CanonicalJsonV1(result),
  });

const orderReasonCodes = (
  codes: Iterable<WatcherPhaseAVerifierReasonCodeV1>,
): readonly WatcherPhaseAVerifierReasonCodeV1[] => {
  const present = new Set<string>(codes);
  return Object.freeze(
    WATCHER_PHASE_A_VERIFIER_REASON_CODES_V1.filter((code) =>
      present.has(code),
    ),
  );
};

// ---------------------------------------------------------------------------
// Canonical verdict projection
// ---------------------------------------------------------------------------

const HEX_32 = /^[0-9a-f]{64}$/u;

/**
 * Projects one canonical `RejectedTx` into the watcher's record shape.
 *
 * Nothing is reinterpreted: the code, stage, and detail are copied verbatim.
 * The only additions are the block position and the W23 stage priority. Every
 * failure path throws, and the caller turns a throw into an error result, so
 * an unrecognisable canonical rejection can never become an acceptance.
 */
export const watcherPhaseARejectionProjectionV1 = (input: {
  readonly rejected: RejectedTx;
  readonly index: number;
  readonly expectedTxId: string;
}): WatcherPhaseARejectionV1 => {
  const { rejected, index, expectedTxId } = input;
  const txId = rejected.txId.toString("hex");
  if (txId !== expectedTxId) {
    fail("rejection_tx_id_mismatch", `$.rejections[${index.toString()}].txId`);
  }
  const code: string = rejected.code;
  if (!WATCHER_PHASE_A_CANONICAL_REJECT_CODES_V1.includes(code as RejectCode)) {
    fail("unknown_reject_code", `$.rejections[${index.toString()}].code`);
  }
  // One guard, not two: the W23 priority list is the canonical phase
  // enumeration in order, so "absent from the priority list" already covers a
  // missing stage and an unknown stage name. A second `in MidgardValidationPhase`
  // check would be unreachable, and unreachable guards are indistinguishable
  // from no guard at all under mutation.
  const stagePriority =
    WATCHER_RULE_BUNDLE_V1_VALIDATION_PHASE_PRIORITY.indexOf(
      rejected.consensusPhase as MidgardValidationPhaseName,
    );
  if (stagePriority < 0) {
    fail("missing_rejection_stage", `$.rejections[${index.toString()}].stage`);
  }
  const stage = rejected.consensusPhase as MidgardValidationPhaseName;
  return Object.freeze({
    index,
    txId,
    code: rejected.code,
    stage,
    stagePriority,
    detail: rejected.detail,
  });
};

/**
 * `first_rejection_by_phase_then_program_counter_v1` at block scope: the
 * lowest canonical validation phase wins, and canonical block order breaks
 * ties.
 *
 * `rejections` is always built in ascending `index` order, so a strict `<` on
 * the phase priority already implements "earliest block position wins on a
 * tie" - the first rejection at the winning phase is reached first and is
 * never displaced. An explicit tie-break clause would be unreachable code.
 */
const selectRejection = (
  rejections: readonly WatcherPhaseARejectionV1[],
): WatcherPhaseARejectionV1 | null => {
  let selected: WatcherPhaseARejectionV1 | null = null;
  for (const rejection of rejections) {
    if (selected === null || rejection.stagePriority < selected.stagePriority) {
      selected = rejection;
    }
  }
  return selected;
};

// ---------------------------------------------------------------------------
// Phase A configuration from the L1-committed header and the W23 rule bundle
// ---------------------------------------------------------------------------

/**
 * Phase A is deterministic in the transaction bytes, the consensus profile,
 * and three header-committed parameters. Concurrency is pinned to 1 because a
 * verifier has no throughput requirement and the value must not influence a
 * digest.
 */
export const WATCHER_PHASE_A_CONCURRENCY_V1 = 1 as const;

/**
 * Arrival metadata is submission bookkeeping, not a validation input:
 * `validatePhaseASingle` copies `arrivalSeq`/`createdAt` into the accepted
 * candidate and never reads them. The watcher cannot recover the operator's
 * values from public DA, so it pins them: `arrivalSeq` is the canonical block
 * position and `createdAt` is the Unix epoch. Neither appears in the result.
 */
export const WATCHER_PHASE_A_CREATED_AT_V1 = new Date(0);

/**
 * Builds the canonical `PhaseAConfig` from the L1-committed header and the
 * W23 rule bundle. Nothing here is a policy choice by the watcher: the three
 * numeric parameters are header fields, and the profile is the exact compiled
 * V1 tuple the rule bundle commits to.
 */
export const makeWatcherPhaseAConfigV1 = (input: {
  readonly header: HeaderV1;
  readonly ruleBundle: WatcherRuleBundleV1;
}): PhaseAConfig => {
  const { header, ruleBundle } = input;
  if (
    ruleBundle.consensusProfileId !== MIDGARD_CONSENSUS_PROFILE_V1_ID ||
    ruleBundle.consensusProfileDigest !== MIDGARD_CONSENSUS_PROFILE_V1_DIGEST ||
    ruleBundle.protocolVersion !== MIDGARD_PROTOCOL_V1_VERSION
  ) {
    fail("rule_bundle_profile_mismatch", "$.ruleBundle.consensusProfileId");
  }
  if (
    ruleBundle.validation.rejectionSelection !==
      WATCHER_RULE_BUNDLE_V1_REJECTION_SELECTION ||
    ruleBundle.validation.phasePriority.length !==
      WATCHER_RULE_BUNDLE_V1_VALIDATION_PHASE_PRIORITY.length ||
    ruleBundle.validation.phasePriority.some(
      (phase, index) =>
        phase !== WATCHER_RULE_BUNDLE_V1_VALIDATION_PHASE_PRIORITY[index],
    )
  ) {
    fail("rule_bundle_profile_mismatch", "$.ruleBundle.validation");
  }
  if (header.protocolVersion !== BigInt(ruleBundle.protocolVersion)) {
    fail("header_protocol_version_mismatch", "$.header.protocolVersion");
  }
  return Object.freeze({
    expectedNetworkId: header.expectedNetworkId,
    minFeeA: header.minFeeA,
    minFeeB: header.minFeeB,
    concurrency: WATCHER_PHASE_A_CONCURRENCY_V1,
    strictnessProfile: MIDGARD_CONSENSUS_PROFILE_V1_ID,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
  });
};

// ---------------------------------------------------------------------------
// Program-material projection
// ---------------------------------------------------------------------------

const EMPTY_SIDECAR_V1 = encodeMidgardCekProgramMaterialSidecarV1([]);

/**
 * Reconstructs the per-transaction CEK program-material sidecar from the
 * block-wide, content-addressed `cek_program_material` set.
 *
 * The operator's per-transaction sidecars are NOT recoverable from public DA:
 * the block merges and deduplicates them by content root
 * (`mergeMidgardCekProgramMaterialSidecarsV1`, cek-proof.ts:3856). Handing the
 * merged superset to Phase A would be wrong in the unsafe direction's mirror
 * image - it would reject honest transactions, because
 * `verifyMidgardCekProgramMaterialBundleV1` requires every supplied node to be
 * reachable from the transaction's own program envelopes.
 *
 * So the watcher projects the superset down to the reachable subset, and it
 * does so with the canonical reachability computation itself: the envelopes
 * come from `collectMidgardV1AttachedProgramEnvelopes` and the reachable roots
 * come from `verifyMidgardCekProgramMaterialBundleV1(..., allowUnreachable)`.
 * There is no watcher-authored traversal. If that canonical computation throws
 * for any reason, the complete block-wide set is handed to Phase A unchanged,
 * so the canonical validator - not this function - renders the verdict.
 */
const projectProgramMaterialSidecarV1 = (
  txCbor: Buffer,
  blockMaterial: readonly MidgardCekProgramMaterialEntryV1[],
): Buffer => {
  if (blockMaterial.length === 0) {
    return EMPTY_SIDECAR_V1;
  }
  try {
    const canonicalTx = decodeMidgardNativeTxFullV1FromCanonicalCbor(txCbor);
    const envelopes = collectMidgardV1AttachedProgramEnvelopes(canonicalTx);
    const verifications = verifyMidgardCekProgramMaterialBundleV1(
      envelopes,
      blockMaterial,
      { allowUnreachable: true },
    );
    const reached = new Set<string>();
    for (const verification of verifications) {
      for (const root of verification.reachableRoots) {
        reached.add(root);
      }
    }
    return encodeMidgardCekProgramMaterialSidecarV1(
      blockMaterial.filter((entry) =>
        reached.has(Buffer.from(entry.root).toString("hex")),
      ),
    );
  } catch {
    return encodeMidgardCekProgramMaterialSidecarV1(blockMaterial);
  }
};

/** Decodes the block-wide DA program-material entries, or fails closed. */
const decodeBlockProgramMaterialV1 = (
  entries: readonly (readonly [string, string])[],
): readonly MidgardCekProgramMaterialEntryV1[] => {
  try {
    return Object.freeze(
      entries.map(([rootHex, valueHex]) =>
        decodeMidgardCekProgramMaterialDaEntryV1(
          Buffer.from(rootHex, "hex"),
          Buffer.from(valueHex, "hex"),
        ),
      ),
    );
  } catch {
    return fail(
      "malformed_program_material",
      "$.payload.block_body.cek_program_material",
    );
  }
};

// ---------------------------------------------------------------------------
// Queued-transaction derivation
// ---------------------------------------------------------------------------

export type WatcherPhaseABlockTransactionV1 = Readonly<{
  /** 32-byte canonical transaction id, lowercase hex. */
  txId: string;
  /** Exact canonical transaction CBOR from `transaction_preimages`. */
  txCbor: Buffer;
}>;

/**
 * Derives the canonical `QueuedTx` list from authenticated block transactions.
 *
 * The `txId` is not trusted as a claim: the canonical reconstruction already
 * required it to equal `computeMidgardNativeTxIdV1` of the preimage
 * (reconstruct.ts:445-490), and `validatePhaseASingle` re-checks it a second
 * time (E_TX_HASH_MISMATCH, phase-a.ts:431). This function only reshapes.
 */
export const watcherPhaseAQueuedTxsV1 = (input: {
  readonly transactions: readonly WatcherPhaseABlockTransactionV1[];
  readonly programMaterial: readonly (readonly [string, string])[];
}): readonly QueuedTx[] => {
  const blockMaterial = decodeBlockProgramMaterialV1(input.programMaterial);
  return Object.freeze(
    input.transactions.map((transaction, index) => {
      if (!HEX_32.test(transaction.txId)) {
        fail(
          "canonical_reconstruction_failed",
          `$.transactions[${index}].txId`,
        );
      }
      return Object.freeze({
        txId: Buffer.from(transaction.txId, "hex"),
        txCbor: transaction.txCbor,
        programMaterialSidecarCbor: projectProgramMaterialSidecarV1(
          transaction.txCbor,
          blockMaterial,
        ),
        arrivalSeq: BigInt(index),
        createdAt: WATCHER_PHASE_A_CREATED_AT_V1,
      });
    }),
  );
};

// ---------------------------------------------------------------------------
// Core evaluation
// ---------------------------------------------------------------------------

export type WatcherPhaseABlockContextV1 = Readonly<{
  headerHash: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
  reconstructionDigest: string;
  ruleBundleCommitment: string;
}>;

const NULL_CONTEXT = {
  headerHash: null,
  payloadEnvelopeSha256: null,
  payloadSha256: null,
  reconstructionDigest: null,
  ruleBundleCommitment: null,
} as const;

const errorResult = (
  reasonCodes: Iterable<WatcherPhaseAVerifierReasonCodeV1>,
  context: WatcherPhaseABlockContextV1 | null,
  transactionCount: number,
): WatcherPhaseAVerificationResultV1 =>
  digestResult({
    schemaVersion: WATCHER_PHASE_A_VERIFIER_V1_SCHEMA_VERSION,
    action: "error",
    reasonCodes: orderReasonCodes(reasonCodes),
    rejectionSelection: WATCHER_RULE_BUNDLE_V1_REJECTION_SELECTION,
    consensusProfileId: MIDGARD_CONSENSUS_PROFILE_V1_ID,
    ...(context ?? NULL_CONTEXT),
    transactionCount,
    acceptedCount: 0,
    acceptedTxIds: Object.freeze([]),
    rejections: Object.freeze([]),
    selectedRejection: null,
  });

const reasonCodeOf = (error: unknown): WatcherPhaseAVerifierReasonCodeV1 =>
  error instanceof WatcherPhaseAVerifierError
    ? error.code
    : "canonical_validation_threw";

/**
 * Runs the canonical Phase A validator over already-derived queued
 * transactions and projects the verdicts.
 *
 * This is the only place a verdict is produced, and it produces none of its
 * own: `validatePhaseASingle` decides, and everything after the call is
 * bookkeeping. A throw out of the canonical layer, an unrecognisable
 * rejection, or an accepted/rejected count that does not add up all yield
 * `action: "error"`.
 */
export const evaluateWatcherPhaseAQueuedTxsV1 = (input: {
  readonly queuedTxs: readonly QueuedTx[];
  readonly config: PhaseAConfig;
  readonly context?: WatcherPhaseABlockContextV1;
}): WatcherPhaseAVerificationResultV1 => {
  const { queuedTxs, config } = input;
  const context = input.context ?? null;
  const transactionCount = queuedTxs.length;
  const reasonCodes = new Set<WatcherPhaseAVerifierReasonCodeV1>();
  const acceptedTxIds: string[] = [];
  const rejections: WatcherPhaseARejectionV1[] = [];

  for (const [index, queuedTx] of queuedTxs.entries()) {
    const expectedTxId = queuedTx.txId.toString("hex");
    let outcome;
    try {
      outcome = validatePhaseASingle(queuedTx, config);
    } catch (error) {
      return errorResult(
        [reasonCodeOf(error), "canonical_validation_threw"],
        context,
        transactionCount,
      );
    }
    if ("ledgerTx" in outcome) {
      acceptedTxIds.push(expectedTxId);
      continue;
    }
    let projected: WatcherPhaseARejectionV1;
    try {
      projected = watcherPhaseARejectionProjectionV1({
        rejected: outcome,
        index,
        expectedTxId,
      });
    } catch (error) {
      return errorResult([reasonCodeOf(error)], context, transactionCount);
    }
    if (!REACHABLE_SET.has(projected.code)) {
      reasonCodes.add("undeclared_reachable_code");
    }
    reasonCodes.add("phase_a_rejection");
    rejections.push(projected);
  }

  // No accepted/rejected reconciliation guard here: the loop above puts every
  // queued transaction into exactly one bucket or returns early, so the counts
  // cannot disagree. A guard that no input can trip is not fail-closed
  // behaviour, it is unreachable code.
  return digestResult({
    schemaVersion: WATCHER_PHASE_A_VERIFIER_V1_SCHEMA_VERSION,
    action: rejections.length === 0 ? "accept" : "reject",
    reasonCodes: orderReasonCodes(reasonCodes),
    rejectionSelection: WATCHER_RULE_BUNDLE_V1_REJECTION_SELECTION,
    consensusProfileId: MIDGARD_CONSENSUS_PROFILE_V1_ID,
    ...(context ?? NULL_CONTEXT),
    transactionCount,
    acceptedCount: acceptedTxIds.length,
    acceptedTxIds: Object.freeze([...acceptedTxIds]),
    rejections: Object.freeze([...rejections]),
    selectedRejection: selectRejection(rejections),
  });
};

// ---------------------------------------------------------------------------
// Block evaluation
// ---------------------------------------------------------------------------

export type EvaluateWatcherPhaseABlockInputV1 = {
  /** L1-authenticated header observation, as W22 consumed it. */
  readonly observation: AuthenticatedStateQueueHeaderObservationV1;
  /** The accepted W22 record for this block. */
  readonly reconstruction: WatcherHeaderRootReconstructionResultV1;
  /** Exact public `DaPayloadEnvelopeV1` bytes, from the W21 store. */
  readonly payloadEnvelopeCbor: Uint8Array;
  /** Provenance of those bytes; must be public/permissionless DA. */
  readonly daProvenance: EvidenceProvenanceV1;
  /** The W23 rule bundle, with its commitment. */
  readonly ruleBundle: WatcherRuleBundleV1;
  readonly ruleBundleCommitment: string;
  readonly minimumConfirmationDepth?: number;
};

/**
 * Re-checks the caller's W22 record against a fresh canonical recomputation.
 *
 * The W22 record is a digest-bound summary, so it can be replayed but not
 * trusted on its own: this recomputes its `resultDigest` from its own fields
 * and requires the accepted root/count set and header identity to equal what
 * the canonical evidence core just produced from the supplied bytes. A record
 * describing a different block, a different payload, or a rejected
 * reconstruction cannot reach the validator.
 */
const bindReconstructionV1 = (input: {
  readonly reconstruction: WatcherHeaderRootReconstructionResultV1;
  readonly headerHash: string;
  readonly payloadEnvelopeSha256: string;
}): void => {
  const { reconstruction } = input;
  if (
    reconstruction.schemaVersion !==
    WATCHER_HEADER_ROOT_RECONSTRUCTION_V1_SCHEMA_VERSION
  ) {
    fail("reconstruction_unsupported_schema", "$.reconstruction.schemaVersion");
  }
  const { resultDigest, ...withoutDigest } = reconstruction;
  if (watcherSha256CanonicalJsonV1(withoutDigest) !== resultDigest) {
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
 * The W24 entry point: an accepted W22 reconstruction plus the exact W21 block
 * bytes plus the W23 rule bundle produce a frozen, digest-bound record of the
 * canonical Phase A verdict for every transaction in the block.
 */
export const evaluateWatcherPhaseABlockV1 = async (
  input: EvaluateWatcherPhaseABlockInputV1,
): Promise<WatcherPhaseAVerificationResultV1> => {
  let evidence;
  try {
    evidence = await canonicalBlockEvidenceFromVerifiedPayloadV1({
      observation: input.observation,
      payloadEnvelopeCbor: input.payloadEnvelopeCbor,
      daProvenance: input.daProvenance,
      ...(input.minimumConfirmationDepth === undefined
        ? {}
        : { minimumConfirmationDepth: input.minimumConfirmationDepth }),
    });
  } catch {
    return errorResult(["canonical_reconstruction_failed"], null, 0);
  }

  const context: WatcherPhaseABlockContextV1 = Object.freeze({
    headerHash: evidence.headerHash,
    payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
    payloadSha256: evidence.payloadSha256,
    reconstructionDigest: input.reconstruction.resultDigest,
    ruleBundleCommitment: input.ruleBundleCommitment,
  });

  let queuedTxs: readonly QueuedTx[];
  let config: PhaseAConfig;
  try {
    bindReconstructionV1({
      reconstruction: input.reconstruction,
      headerHash: evidence.headerHash,
      payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
    });
    config = makeWatcherPhaseAConfigV1({
      header: evidence.header,
      ruleBundle: input.ruleBundle,
    });
    queuedTxs = watcherPhaseAQueuedTxsV1({
      transactions: evidence.reconstruction.transactions.map((entry) =>
        Object.freeze({
          txId: entry.txId,
          txCbor: entry.fullTransactionCbor,
        }),
      ),
      programMaterial: evidence.reconstruction.payload.block_body
        .cek_program_material as readonly (readonly [string, string])[],
    });
  } catch (error) {
    return errorResult(
      [reasonCodeOf(error)],
      context,
      evidence.reconstruction.transactions.length,
    );
  }

  return evaluateWatcherPhaseAQueuedTxsV1({ queuedTxs, config, context });
};
