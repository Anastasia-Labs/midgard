/**
 * TypeScript wire twins of the `value-not-preserved` on-chain types
 * (`onchain/aiken/lib/midgard/fraud-proofs/value-not-preserved/step-0{1..4}.ak`).
 *
 * **Constructor order is frozen consensus wire format.** `Data.Enum` pins the
 * Constr index positionally, so every array order below IS the on-chain tag
 * order: `AdaAsset` 0 / `TokenAsset` 1, `ClaimedAssetInflated` 0 /
 * `ClaimedAssetDeflated` 1, and on the step-02 redeemer `FoldInput` 0 /
 * `FinishInputs` 1. Reordering any of them silently re-tags every redeemer
 * built through this module.
 *
 * The family predates catalogue registration, so these schemas live here in
 * the family's own module rather than in `@al-ft/midgard-sdk` — the SDK file
 * is a parent-owned surface that lands at registration. Everything generic
 * (the `Cancel`/`Continue` step-redeemer shell, the step datum shell, the
 * shared inclusion and field-opening shapes) is taken from the SDK, not
 * restated.
 */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldCarriageSchema,
  FieldOpeningSchema,
  H32Schema,
  NativeTxInclusionArgsSchema,
  ProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

// ---------------------------------------------------------------------------
// The single-asset claim (lib step-01)
// ---------------------------------------------------------------------------

/** `ClaimedAsset`: `AdaAsset` is Constr 0, `TokenAsset` Constr 1. */
export const ClaimedAssetSchema = Data.Enum([
  Data.Literal("AdaAsset"),
  Data.Object({
    TokenAsset: Data.Object({
      policy_id: Data.Bytes({ minLength: 28, maxLength: 28 }),
      asset_name: Data.Bytes({ maxLength: 32 }),
    }),
  }),
]);
export type ClaimedAsset = Data.Static<typeof ClaimedAssetSchema>;
export const ClaimedAsset = asDataType<ClaimedAsset>(ClaimedAssetSchema);

/**
 * `ClaimedImbalanceDirection`: `ClaimedAssetInflated` (Constr 0) convicts
 * on `final_delta < 0`, `ClaimedAssetDeflated` (Constr 1) on
 * `final_delta > 0`.
 */
export const ClaimedImbalanceDirectionSchema = Data.Enum([
  Data.Literal("ClaimedAssetInflated"),
  Data.Literal("ClaimedAssetDeflated"),
]);
export type ClaimedImbalanceDirection = Data.Static<
  typeof ClaimedImbalanceDirectionSchema
>;
export const ClaimedImbalanceDirection = asDataType<ClaimedImbalanceDirection>(
  ClaimedImbalanceDirectionSchema,
);

// ---------------------------------------------------------------------------
// Step 01 — bind and freeze the claim
// ---------------------------------------------------------------------------

export const ValueNotPreservedStep01ArgsSchema = Data.Object({
  tx_inclusion: NativeTxInclusionArgsSchema,
  claimed_asset: ClaimedAssetSchema,
  claimed_direction: ClaimedImbalanceDirectionSchema,
});
export type ValueNotPreservedStep01Args = Data.Static<
  typeof ValueNotPreservedStep01ArgsSchema
>;
export const ValueNotPreservedStep01Args =
  asDataType<ValueNotPreservedStep01Args>(ValueNotPreservedStep01ArgsSchema);

export const ValueNotPreservedStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(ValueNotPreservedStep01ArgsSchema);
export type ValueNotPreservedStep01SpendRedeemer = Data.Static<
  typeof ValueNotPreservedStep01SpendRedeemerSchema
>;
export const ValueNotPreservedStep01SpendRedeemer =
  asDataType<ValueNotPreservedStep01SpendRedeemer>(
    ValueNotPreservedStep01SpendRedeemerSchema,
  );

// ---------------------------------------------------------------------------
// Step 02 — the spent-input fold
// ---------------------------------------------------------------------------

export const ValueNotPreservedStep02StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  claimed_asset: ClaimedAssetSchema,
  claimed_direction: ClaimedImbalanceDirectionSchema,
  committed_fee: Data.Integer(),
  prev_utxos_root: H32Schema,
  input_cursor: Data.Integer(),
  claimed_delta: Data.Integer(),
});
export type ValueNotPreservedStep02State = Data.Static<
  typeof ValueNotPreservedStep02StateSchema
>;
export const ValueNotPreservedStep02State =
  asDataType<ValueNotPreservedStep02State>(ValueNotPreservedStep02StateSchema);

export const ValueNotPreservedStep02DatumSchema = faultProofStepDatumSchema(
  ValueNotPreservedStep02StateSchema,
);
export type ValueNotPreservedStep02Datum = Data.Static<
  typeof ValueNotPreservedStep02DatumSchema
>;
export const ValueNotPreservedStep02Datum =
  asDataType<ValueNotPreservedStep02Datum>(ValueNotPreservedStep02DatumSchema);

/** One authenticated asset leaf of a spent input's descriptor, in index order. */
export const AssetLeafOpeningSchema = Data.Object({
  policy_id: Data.Bytes({ minLength: 28, maxLength: 28 }),
  asset_name: Data.Bytes({ maxLength: 32 }),
  quantity: Data.Integer(),
  siblings: Data.Array(H32Schema),
});
export type AssetLeafOpening = Data.Static<typeof AssetLeafOpeningSchema>;
export const AssetLeafOpening = asDataType<AssetLeafOpening>(
  AssetLeafOpeningSchema,
);

/** Wire twin of the aiken `validation_merkle_v1.FrontierPeak`. */
export const FrontierPeakSchema = Data.Object({
  height: Data.Integer(),
  hash: H32Schema,
});
export type FrontierPeak = Data.Static<typeof FrontierPeakSchema>;
export const FrontierPeak = asDataType<FrontierPeak>(FrontierPeakSchema);

/** The pre-state value witness for one spend input. */
export const SpentInputValueWitnessSchema = Data.Object({
  descriptor_cbor: Data.Bytes(),
  ledger_membership_proof: ProofSchema,
  asset_peaks: Data.Array(FrontierPeakSchema),
  asset_openings: Data.Array(AssetLeafOpeningSchema),
});
export type SpentInputValueWitness = Data.Static<
  typeof SpentInputValueWitnessSchema
>;
export const SpentInputValueWitness = asDataType<SpentInputValueWitness>(
  SpentInputValueWitnessSchema,
);

export const ValueNotPreservedStep02FoldArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  spend_inputs_opening: FieldOpeningSchema,
  value_witness: SpentInputValueWitnessSchema,
});
export type ValueNotPreservedStep02FoldArgs = Data.Static<
  typeof ValueNotPreservedStep02FoldArgsSchema
>;
export const ValueNotPreservedStep02FoldArgs =
  asDataType<ValueNotPreservedStep02FoldArgs>(
    ValueNotPreservedStep02FoldArgsSchema,
  );

/** Step-02 redeemer arms: `FoldInput` is Constr 0, `FinishInputs` Constr 1. */
export const ValueNotPreservedStep02ArgsSchema = Data.Enum([
  Data.Object({
    FoldInput: Data.Tuple([ValueNotPreservedStep02FoldArgsSchema]),
  }),
  Data.Object({
    FinishInputs: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      spend_inputs_opening: FieldOpeningSchema,
    }),
  }),
]);
export type ValueNotPreservedStep02Args = Data.Static<
  typeof ValueNotPreservedStep02ArgsSchema
>;
export const ValueNotPreservedStep02Args =
  asDataType<ValueNotPreservedStep02Args>(ValueNotPreservedStep02ArgsSchema);

export const ValueNotPreservedStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(ValueNotPreservedStep02ArgsSchema);
export type ValueNotPreservedStep02SpendRedeemer = Data.Static<
  typeof ValueNotPreservedStep02SpendRedeemerSchema
>;
export const ValueNotPreservedStep02SpendRedeemer =
  asDataType<ValueNotPreservedStep02SpendRedeemer>(
    ValueNotPreservedStep02SpendRedeemerSchema,
  );

// ---------------------------------------------------------------------------
// Step 03 — outputs, mint and fee
// ---------------------------------------------------------------------------

export const ValueNotPreservedStep03StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  claimed_asset: ClaimedAssetSchema,
  claimed_direction: ClaimedImbalanceDirectionSchema,
  committed_fee: Data.Integer(),
  claimed_delta: Data.Integer(),
});
export type ValueNotPreservedStep03State = Data.Static<
  typeof ValueNotPreservedStep03StateSchema
>;
export const ValueNotPreservedStep03State =
  asDataType<ValueNotPreservedStep03State>(ValueNotPreservedStep03StateSchema);

export const ValueNotPreservedStep03DatumSchema = faultProofStepDatumSchema(
  ValueNotPreservedStep03StateSchema,
);
export type ValueNotPreservedStep03Datum = Data.Static<
  typeof ValueNotPreservedStep03DatumSchema
>;
export const ValueNotPreservedStep03Datum =
  asDataType<ValueNotPreservedStep03Datum>(ValueNotPreservedStep03DatumSchema);

export const ValueNotPreservedStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  native_tx_compact_cbor: Data.Bytes(),
  outputs_carriage: FieldCarriageSchema,
  /** `Some` exactly for a token claim; an ADA claim must carry `None`. */
  mint_carriage: Data.Nullable(FieldCarriageSchema),
});
export type ValueNotPreservedStep03Args = Data.Static<
  typeof ValueNotPreservedStep03ArgsSchema
>;
export const ValueNotPreservedStep03Args =
  asDataType<ValueNotPreservedStep03Args>(ValueNotPreservedStep03ArgsSchema);

export const ValueNotPreservedStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(ValueNotPreservedStep03ArgsSchema);
export type ValueNotPreservedStep03SpendRedeemer = Data.Static<
  typeof ValueNotPreservedStep03SpendRedeemerSchema
>;
export const ValueNotPreservedStep03SpendRedeemer =
  asDataType<ValueNotPreservedStep03SpendRedeemer>(
    ValueNotPreservedStep03SpendRedeemerSchema,
  );

// ---------------------------------------------------------------------------
// Step 04 — finalize
// ---------------------------------------------------------------------------

export const ValueNotPreservedStep04StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  claimed_asset: ClaimedAssetSchema,
  claimed_direction: ClaimedImbalanceDirectionSchema,
  final_delta: Data.Integer(),
});
export type ValueNotPreservedStep04State = Data.Static<
  typeof ValueNotPreservedStep04StateSchema
>;
export const ValueNotPreservedStep04State =
  asDataType<ValueNotPreservedStep04State>(ValueNotPreservedStep04StateSchema);

export const ValueNotPreservedStep04DatumSchema = faultProofStepDatumSchema(
  ValueNotPreservedStep04StateSchema,
);
export type ValueNotPreservedStep04Datum = Data.Static<
  typeof ValueNotPreservedStep04DatumSchema
>;
export const ValueNotPreservedStep04Datum =
  asDataType<ValueNotPreservedStep04Datum>(ValueNotPreservedStep04DatumSchema);

export const ValueNotPreservedStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type ValueNotPreservedStep04Args = Data.Static<
  typeof ValueNotPreservedStep04ArgsSchema
>;
export const ValueNotPreservedStep04Args =
  asDataType<ValueNotPreservedStep04Args>(ValueNotPreservedStep04ArgsSchema);

export const ValueNotPreservedStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(ValueNotPreservedStep04ArgsSchema);
export type ValueNotPreservedStep04SpendRedeemer = Data.Static<
  typeof ValueNotPreservedStep04SpendRedeemerSchema
>;
export const ValueNotPreservedStep04SpendRedeemer =
  asDataType<ValueNotPreservedStep04SpendRedeemer>(
    ValueNotPreservedStep04SpendRedeemerSchema,
  );

// ---------------------------------------------------------------------------
// Local mirrors of the on-chain predicates
// ---------------------------------------------------------------------------

/** Twin of `claimed_asset_is_well_formed_v1`, applied before paying for step-01. */
export const claimedAssetIsWellFormed = (claim: ClaimedAsset): boolean => {
  if (claim === "AdaAsset") return true;
  const { policy_id, asset_name } = claim.TokenAsset;
  return policy_id.length === 56 && asset_name.length <= 64;
};

/**
 * Twin of `value_not_preserved_fault_is_established_v1` — the step-04
 * conviction condition. A zero delta satisfies neither direction, so a
 * balanced fold is never finalizable.
 */
export const valueNotPreservedFaultIsEstablished = ({
  claimedDirection,
  finalDelta,
}: {
  readonly claimedDirection: ClaimedImbalanceDirection;
  readonly finalDelta: bigint;
}): boolean =>
  claimedDirection === "ClaimedAssetInflated"
    ? finalDelta < 0n
    : finalDelta > 0n;
