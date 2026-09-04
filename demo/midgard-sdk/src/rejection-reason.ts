import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

/**
 * Twin of `midgard/rejection_reason_v1.RejectionReason` (#640).
 *
 * Normative inventory: `docs/fault-proofs/rejection-reason-catalogue-v1.md`
 * (§5 arm table and coordinate conventions, §6 type). The constructor order
 * below is **wire-normative** — it fixes the Plutus Data constructor indices
 * of the forced leaf (`ForcedInclusionTxV1.verdict`), so arms must never be
 * reordered, inserted, or removed except through a leaf-schema format
 * revision (`RejectionReasonV2`; catalogue design note 1).
 *
 * The constructor tag IS the rejection reason; payloads carry **only**
 * subject coordinates (field/item/witness/output/redeemer/execution
 * ordinals) — never expected values, hashes, or recomputable arguments
 * (catalogue design note 4).
 *
 * A reserved family, not populated in V1: `GuardrailExceeded` — kept
 * unpopulated per the 2026-08-24 owner ruling that the L1 forced-order door
 * (`docs/spec/midgard-tx.md` §8.11) excludes guardrail-violating preimages
 * (catalogue §4.3 resolution, design note 6).
 */
export const RejectionReasonSchema = Data.Enum([
  // ── CanonicalDecode ────────────────────────────────────────────────
  Data.Object({
    FieldPreimageLengthMismatch: Data.Object({ field_index: Data.Integer() }),
  }),
  Data.Object({
    FieldItemWidthIllegal: Data.Object({
      field_index: Data.Integer(),
      item_index: Data.Integer(),
    }),
  }),
  // ── InputSets ──────────────────────────────────────────────────────
  Data.Literal("EmptyInputs"),
  Data.Object({
    DuplicateInput: Data.Object({
      first_field_index: Data.Integer(),
      first_item_index: Data.Integer(),
      second_field_index: Data.Integer(),
      second_item_index: Data.Integer(),
    }),
  }),
  Data.Literal("ValidityIntervalMalformed"),
  // ── StaticLedgerRules ──────────────────────────────────────────────
  Data.Literal("NetworkIdMismatch"),
  Data.Literal("FeeBelowMinimum"),
  // ── Signatures ─────────────────────────────────────────────────────
  Data.Object({
    AddressWitnessSignatureInvalid: Data.Object({
      witness_index: Data.Integer(),
    }),
  }),
  Data.Object({
    RequiredSignerUnsigned: Data.Object({ signer_index: Data.Integer() }),
  }),
  // ── PhaseANativeScripts (witness scripts, field 6) ─────────────────
  Data.Object({
    WitnessScriptHeaderMalformed: Data.Object({ script_index: Data.Integer() }),
  }),
  Data.Object({
    WitnessNativeScriptMalformed: Data.Object({ script_index: Data.Integer() }),
  }),
  Data.Object({
    WitnessNativeScriptNodeLimit: Data.Object({ script_index: Data.Integer() }),
  }),
  Data.Object({
    WitnessNativeScriptDepthLimit: Data.Object({
      script_index: Data.Integer(),
    }),
  }),
  Data.Object({
    WitnessNativeScriptFalse: Data.Object({ script_index: Data.Integer() }),
  }),
  // ── PhaseAScriptPreconditions ──────────────────────────────────────
  Data.Literal("ScriptIntegrityHashMissing"),
  Data.Literal("ObserversForbiddenOnUntaggedNetwork"),
  Data.Object({
    ObserverOrderInvalid: Data.Object({ observer_index: Data.Integer() }),
  }),
  // ── ResolveInputs ──────────────────────────────────────────────────
  Data.Literal("ValidityIntervalExcludesBlockSlot"),
  Data.Object({
    InputNotFound: Data.Object({
      source_kind: Data.Integer(),
      input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    InputSpentOutputNonCanonical: Data.Object({
      source_kind: Data.Integer(),
      input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    ResolvedReferenceScriptMalformed: Data.Object({
      source_kind: Data.Integer(),
      input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    ResolvedReferenceScriptNodeLimit: Data.Object({
      source_kind: Data.Integer(),
      input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    ResolvedReferenceScriptDepthLimit: Data.Object({
      source_kind: Data.Integer(),
      input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    SpendInputSignerMissing: Data.Object({ input_index: Data.Integer() }),
  }),
  // ── ScriptSources ──────────────────────────────────────────────────
  Data.Object({
    RedeemerMalformed: Data.Object({ redeemer_index: Data.Integer() }),
  }),
  Data.Object({
    OutputNonCanonical: Data.Object({ output_index: Data.Integer() }),
  }),
  Data.Object({
    OutputReferenceScriptMalformed: Data.Object({
      output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    OutputReferenceScriptNodeLimit: Data.Object({
      output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    OutputReferenceScriptDepthLimit: Data.Object({
      output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    ProtectedOutputSignerMissing: Data.Object({
      output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    MintDeclaredAssetLimit: Data.Object({ policy_index: Data.Integer() }),
  }),
  Data.Object({
    ScriptSourceMissing: Data.Object({
      purpose_kind: Data.Integer(),
      purpose_index: Data.Integer(),
    }),
  }),
  Data.Object({
    RedeemerMissing: Data.Object({
      purpose_kind: Data.Integer(),
      purpose_index: Data.Integer(),
    }),
  }),
  Data.Object({
    UnusedScriptWitness: Data.Object({ script_index: Data.Integer() }),
  }),
  Data.Object({
    UnusedRedeemer: Data.Object({ redeemer_index: Data.Integer() }),
  }),
  // ── NativeScripts / Phase-B executions ─────────────────────────────
  Data.Object({
    ExecutionNativeScriptMalformed: Data.Object({
      execution_index: Data.Integer(),
    }),
  }),
  Data.Object({
    ExecutionNativeScriptNodeLimit: Data.Object({
      execution_index: Data.Integer(),
    }),
  }),
  Data.Object({
    ExecutionNativeScriptDepthLimit: Data.Object({
      execution_index: Data.Integer(),
    }),
  }),
  Data.Object({
    ExecutionNativeScriptFalse: Data.Object({
      execution_index: Data.Integer(),
    }),
  }),
  // ── ScriptIntegrity ────────────────────────────────────────────────
  Data.Literal("ScriptIntegrityHashMismatch"),
  // ── Cek ────────────────────────────────────────────────────────────
  Data.Object({
    ReceivePurposePlutusV3Forbidden: Data.Object({
      execution_index: Data.Integer(),
    }),
  }),
  Data.Object({
    PlutusExecutionFailed: Data.Object({ execution_index: Data.Integer() }),
  }),
  // ── ValueAndMint ───────────────────────────────────────────────────
  Data.Object({
    InputAssetAccumulationLimit: Data.Object({
      input_index: Data.Integer(),
      asset_index: Data.Integer(),
    }),
  }),
  Data.Object({
    OutputAssetAccumulationLimit: Data.Object({
      output_index: Data.Integer(),
      asset_index: Data.Integer(),
    }),
  }),
  Data.Object({
    MintAssetAccumulationLimit: Data.Object({ mint_index: Data.Integer() }),
  }),
  Data.Object({
    OutputBelowMinAda: Data.Object({ output_index: Data.Integer() }),
  }),
  Data.Literal("ValueNotPreserved"),
]);
export type RejectionReason = Data.Static<typeof RejectionReasonSchema>;
export const RejectionReason = asDataType<RejectionReason>(
  RejectionReasonSchema,
);

/**
 * Twin of `midgard/rejection_reason_v1.OperatorVerdict` (#640): the
 * operator's recorded verdict on a forced-inclusion transaction, as the
 * forced leaf (`ForcedInclusionTxV1.verdict`) carries it. The sum's collapse
 * deliberately makes "valid with a reason" and "invalid without one"
 * unrepresentable.
 */
export const OperatorVerdictSchema = Data.Enum([
  Data.Literal("ForcedTxValid"),
  Data.Object({
    ForcedTxInvalid: Data.Object({ reason: RejectionReasonSchema }),
  }),
]);
export type OperatorVerdict = Data.Static<typeof OperatorVerdictSchema>;
export const OperatorVerdict = asDataType<OperatorVerdict>(
  OperatorVerdictSchema,
);

/**
 * The 19 `E_*` rejection-code byte strings (hex of the ASCII label), twins of
 * the canonical `pub const reject_*` values in
 * `midgard/rejection_reason_v1`. The frozen descriptor bridge
 * (`hash_rejection_code`) is pinned on these bytes.
 */
export const RejectionCodes = {
  E_FIELD_PREIMAGE_SIZE: "455f4649454c445f505245494d4147455f53495a45",
  E_ASSET_COUNT: "455f41535345545f434f554e54",
  E_INVALID_FIELD_TYPE: "455f494e56414c49445f4649454c445f54595045",
  E_NATIVE_SCRIPT_DEPTH: "455f4e41544956455f5343524950545f4445505448",
  E_NATIVE_SCRIPT_NODE_COUNT:
    "455f4e41544956455f5343524950545f4e4f44455f434f554e54",
  E_MIN_ADA: "455f4d494e5f414441",
  E_EMPTY_INPUTS: "455f454d5054595f494e50555453",
  E_DUPLICATE_INPUT_IN_TX: "455f4455504c49434154455f494e5055545f494e5f5458",
  E_NETWORK_ID_MISMATCH: "455f4e4554574f524b5f49445f4d49534d41544348",
  E_MIN_FEE: "455f4d494e5f464545",
  E_INVALID_VALIDITY_INTERVAL_FORMAT:
    "455f494e56414c49445f56414c49444954595f494e54455256414c5f464f524d4154",
  E_MISSING_REQUIRED_WITNESS:
    "455f4d495353494e475f52455155495245445f5749544e455353",
  E_INVALID_SIGNATURE: "455f494e56414c49445f5349474e4154555245",
  E_NATIVE_SCRIPT_INVALID: "455f4e41544956455f5343524950545f494e56414c4944",
  E_PLUTUS_SCRIPT_INVALID: "455f504c555455535f5343524950545f494e56414c4944",
  E_VALIDITY_INTERVAL_MISMATCH:
    "455f56414c49444954595f494e54455256414c5f4d49534d41544348",
  E_INPUT_NOT_FOUND: "455f494e5055545f4e4f545f464f554e44",
  E_INVALID_OUTPUT: "455f494e56414c49445f4f5554505554",
  E_VALUE_NOT_PRESERVED: "455f56414c55455f4e4f545f505245534552564544",
} as const;
export type RejectionCodeLabel = keyof typeof RejectionCodes;

/** The constructor tag of a {@link RejectionReason} value. */
export const rejectionReasonArmOf = (reason: RejectionReason): string => {
  if (typeof reason === "string") return reason;
  const [arm] = Object.keys(reason);
  if (arm === undefined) {
    throw new Error("rejectionReasonArmOf: empty RejectionReasonV1 object");
  }
  return arm;
};

const REJECTION_CODE_BY_ARM: Record<string, RejectionCodeLabel> = {
  FieldPreimageLengthMismatch: "E_FIELD_PREIMAGE_SIZE",
  FieldItemWidthIllegal: "E_INVALID_FIELD_TYPE",
  EmptyInputs: "E_EMPTY_INPUTS",
  DuplicateInput: "E_DUPLICATE_INPUT_IN_TX",
  ValidityIntervalMalformed: "E_INVALID_VALIDITY_INTERVAL_FORMAT",
  NetworkIdMismatch: "E_NETWORK_ID_MISMATCH",
  FeeBelowMinimum: "E_MIN_FEE",
  AddressWitnessSignatureInvalid: "E_INVALID_SIGNATURE",
  RequiredSignerUnsigned: "E_MISSING_REQUIRED_WITNESS",
  WitnessScriptHeaderMalformed: "E_INVALID_FIELD_TYPE",
  WitnessNativeScriptMalformed: "E_INVALID_FIELD_TYPE",
  WitnessNativeScriptNodeLimit: "E_NATIVE_SCRIPT_NODE_COUNT",
  WitnessNativeScriptDepthLimit: "E_NATIVE_SCRIPT_DEPTH",
  WitnessNativeScriptFalse: "E_NATIVE_SCRIPT_INVALID",
  ScriptIntegrityHashMissing: "E_INVALID_FIELD_TYPE",
  ObserversForbiddenOnUntaggedNetwork: "E_INVALID_FIELD_TYPE",
  ObserverOrderInvalid: "E_INVALID_FIELD_TYPE",
  ValidityIntervalExcludesBlockSlot: "E_VALIDITY_INTERVAL_MISMATCH",
  InputNotFound: "E_INPUT_NOT_FOUND",
  InputSpentOutputNonCanonical: "E_INVALID_OUTPUT",
  ResolvedReferenceScriptMalformed: "E_INVALID_FIELD_TYPE",
  ResolvedReferenceScriptNodeLimit: "E_NATIVE_SCRIPT_NODE_COUNT",
  ResolvedReferenceScriptDepthLimit: "E_NATIVE_SCRIPT_DEPTH",
  SpendInputSignerMissing: "E_MISSING_REQUIRED_WITNESS",
  RedeemerMalformed: "E_INVALID_FIELD_TYPE",
  OutputNonCanonical: "E_INVALID_OUTPUT",
  OutputReferenceScriptMalformed: "E_INVALID_FIELD_TYPE",
  OutputReferenceScriptNodeLimit: "E_NATIVE_SCRIPT_NODE_COUNT",
  OutputReferenceScriptDepthLimit: "E_NATIVE_SCRIPT_DEPTH",
  ProtectedOutputSignerMissing: "E_MISSING_REQUIRED_WITNESS",
  MintDeclaredAssetLimit: "E_ASSET_COUNT",
  ScriptSourceMissing: "E_MISSING_REQUIRED_WITNESS",
  RedeemerMissing: "E_MISSING_REQUIRED_WITNESS",
  UnusedScriptWitness: "E_INVALID_FIELD_TYPE",
  UnusedRedeemer: "E_INVALID_FIELD_TYPE",
  ExecutionNativeScriptMalformed: "E_INVALID_FIELD_TYPE",
  ExecutionNativeScriptNodeLimit: "E_NATIVE_SCRIPT_NODE_COUNT",
  ExecutionNativeScriptDepthLimit: "E_NATIVE_SCRIPT_DEPTH",
  ExecutionNativeScriptFalse: "E_NATIVE_SCRIPT_INVALID",
  ScriptIntegrityHashMismatch: "E_INVALID_FIELD_TYPE",
  ReceivePurposePlutusV3Forbidden: "E_PLUTUS_SCRIPT_INVALID",
  PlutusExecutionFailed: "E_PLUTUS_SCRIPT_INVALID",
  InputAssetAccumulationLimit: "E_ASSET_COUNT",
  OutputAssetAccumulationLimit: "E_ASSET_COUNT",
  MintAssetAccumulationLimit: "E_ASSET_COUNT",
  OutputBelowMinAda: "E_MIN_ADA",
  ValueNotPreserved: "E_VALUE_NOT_PRESERVED",
};

/**
 * Twin of `midgard/rejection_reason_v1.rejection_code_of`: the total,
 * non-injective 47 → 19 bridge back to the frozen descriptor codes
 * (catalogue §5.1). Returns the `E_*` code bytes as lowercase hex.
 */
export const rejectionCodeOf = (reason: RejectionReason): string => {
  const label = REJECTION_CODE_BY_ARM[rejectionReasonArmOf(reason)];
  if (label === undefined) {
    throw new Error(
      `rejectionCodeOf: unknown RejectionReasonV1 arm ${rejectionReasonArmOf(reason)}`,
    );
  }
  return RejectionCodes[label];
};
