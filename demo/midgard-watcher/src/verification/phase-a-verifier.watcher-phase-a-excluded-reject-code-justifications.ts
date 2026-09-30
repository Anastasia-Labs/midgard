import type { MidgardValidationPhaseName } from "@al-ft/midgard-core/validation-trace";
import type { RejectCode } from "@al-ft/midgard-validation/types";
import { RejectCodes } from "@al-ft/midgard-validation/types";

export const WATCHER_PHASE_A_VERIFIER_SCHEMA_VERSION =
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
export const WATCHER_PHASE_A_DIRECT_REJECT_CODES = Object.freeze([
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
export const WATCHER_PHASE_A_CONSENSUS_REJECT_CODES = Object.freeze([
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
export const WATCHER_PHASE_A_CANONICAL_REJECT_CODES = Object.freeze(
  Object.values(RejectCodes),
);

export const REACHABLE_SET: ReadonlySet<string> = new Set<string>([
  ...WATCHER_PHASE_A_DIRECT_REJECT_CODES,
  ...WATCHER_PHASE_A_CONSENSUS_REJECT_CODES,
]);

/**
 * The 32 canonical codes Phase A can produce, in `RejectCodes` declaration
 * order so the published table and every emitted list are stable.
 */
export const WATCHER_PHASE_A_REACHABLE_REJECT_CODES = Object.freeze(
  WATCHER_PHASE_A_CANONICAL_REJECT_CODES.filter((code) =>
    REACHABLE_SET.has(code),
  ),
);

/** The 18 canonical codes Phase A cannot produce. */
export const WATCHER_PHASE_A_EXCLUDED_REJECT_CODES = Object.freeze(
  WATCHER_PHASE_A_CANONICAL_REJECT_CODES.filter(
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
export const WATCHER_PHASE_A_EVIDENCED_REJECT_CODES = Object.freeze([
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
  WATCHER_PHASE_A_EVIDENCED_REJECT_CODES,
);

/**
 * Reachable in the canonical control flow but not producible by any canonical
 * V1 transaction: an earlier canonical rule always fires first, so no input
 * exists that reaches the check.
 */
export const WATCHER_PHASE_A_DOMINATED_REJECT_CODES = Object.freeze(
  WATCHER_PHASE_A_REACHABLE_REJECT_CODES.filter(
    (code) => !EVIDENCED_SET.has(code),
  ),
);

/**
 * One line per dominated code, naming the canonical rule that always fires
 * first. Each claim is pinned by an "adjacent boundary" case in the W24 suite
 * that shows the dominating code, not the dominated one.
 */
export const WATCHER_PHASE_A_DOMINATED_REJECT_CODE_JUSTIFICATIONS =
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
      "dominated: consensus-validation.ts has no E_SCRIPT_PROGRAM_SIZE call site; oversized programs surface as E_SCRIPT_PROGRAM_ENCODING from the bounded envelope decoder.",
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
export const WATCHER_PHASE_A_EXCLUDED_REJECT_CODE_JUSTIFICATIONS =
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
export const WATCHER_PHASE_A_VERIFIER_REASON_CODES = [
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

export type WatcherPhaseAVerifierReasonCode =
  (typeof WATCHER_PHASE_A_VERIFIER_REASON_CODES)[number];

export class WatcherPhaseAVerifierError extends Error {
  readonly code: WatcherPhaseAVerifierReasonCode;
  readonly path: string;

  constructor(code: WatcherPhaseAVerifierReasonCode, path: string) {
    super(`${code}: ${path}`);
    this.name = "WatcherPhaseAVerifierError";
    this.code = code;
    this.path = path;
  }
}

export const fail = (
  code: WatcherPhaseAVerifierReasonCode,
  path: string,
): never => {
  throw new WatcherPhaseAVerifierError(code, path);
};

// ---------------------------------------------------------------------------
// Result shape
// ---------------------------------------------------------------------------

export type WatcherPhaseARejection = Readonly<{
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
