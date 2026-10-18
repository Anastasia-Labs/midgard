import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  MIDGARD_REJECTION_REASON_V1_ARMS,
  type MidgardRejectionReasonV1,
} from "@al-ft/midgard-core/validation-trace";
import { Data } from "@lucid-evolution/lucid";

/**
 * Twin of `midgard/rejection_reason_v1.RejectionReasonV1`: 49 arms (0–48).
 * Its Plutus Data bytes equal the structural codec in
 * `@al-ft/midgard-core/validation-trace` (`encodeMidgardRejectionReasonV1Cbor`),
 * which the descriptor's rejection-code hash is computed over.
 *
 * Normative inventory: `docs/fault-proofs/rejection-reason-catalogue-v1.md`
 * (§5 arm table and coordinate conventions, §6 type). The constructor order
 * below is **wire-normative** — it fixes the Plutus Data constructor indices
 * of the forced leaf (`ForcedInclusionTxV1.verdict`), so arms must never be
 * reordered, inserted, or removed except through a leaf-schema format
 * revision (`RejectionReasonV2`; catalogue design note 1).
 *
 * The constructor tag IS the rejection reason; payloads carry **only**
 * subject coordinates (field/item/witness/output/redeemer/purpose
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
      purpose_kind: Data.Integer(),
      purpose_index: Data.Integer(),
    }),
  }),
  Data.Object({
    ExecutionNativeScriptNodeLimit: Data.Object({
      purpose_kind: Data.Integer(),
      purpose_index: Data.Integer(),
    }),
  }),
  Data.Object({
    ExecutionNativeScriptDepthLimit: Data.Object({
      purpose_kind: Data.Integer(),
      purpose_index: Data.Integer(),
    }),
  }),
  Data.Object({
    ExecutionNativeScriptFalse: Data.Object({
      purpose_kind: Data.Integer(),
      purpose_index: Data.Integer(),
    }),
  }),
  // ── ScriptIntegrity ────────────────────────────────────────────────
  Data.Literal("ScriptIntegrityHashMismatch"),
  // ── Cek ────────────────────────────────────────────────────────────
  Data.Object({
    ReceivePurposePlutusV3Forbidden: Data.Object({
      purpose_kind: Data.Integer(),
      purpose_index: Data.Integer(),
    }),
  }),
  Data.Object({
    PlutusExecutionFailed: Data.Object({
      purpose_kind: Data.Integer(),
      purpose_index: Data.Integer(),
    }),
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
  // ── ScriptSources mint items ───────────────────────────────────────
  Data.Object({
    MintItemMalformed: Data.Object({ policy_index: Data.Integer() }),
  }),
  // ── ScriptDecode ───────────────────────────────────────────────────
  Data.Object({
    PlutusScriptUndecodable: Data.Object({
      item_kind: Data.Integer(),
      item_index: Data.Integer(),
      class: Data.Integer(),
    }),
  }),
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

/** The constructor tag of a {@link RejectionReason} value. */
export const rejectionReasonArmOf = (reason: RejectionReason): string => {
  if (typeof reason === "string") return reason;
  const [arm] = Object.keys(reason);
  if (arm === undefined) {
    throw new Error("rejectionReasonArmOf: empty RejectionReasonV1 object");
  }
  return arm;
};

/** True when `name` is the constructor tag of a `RejectionReasonV1` arm. */
export const isRejectionReasonArm = (name: string): boolean =>
  MIDGARD_REJECTION_REASON_V1_ARMS.some((arm) => arm.name === name);

/**
 * The structural form of a schema value, for the core rejection-code hash.
 * The two conversions below only type-check while the schema and the core
 * arm table name the same arms with the same coordinates.
 */
export const rejectionReasonV1Structural = (
  reason: RejectionReason,
): MidgardRejectionReasonV1 => reason;

/** The schema form of a structural reason; see {@link rejectionReasonV1Structural}. */
export const rejectionReasonFromStructural = (
  reason: MidgardRejectionReasonV1,
): RejectionReason => reason;
