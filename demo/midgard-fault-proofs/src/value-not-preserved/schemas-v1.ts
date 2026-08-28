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
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  FieldCarriageV1Schema,
  FieldOpeningV1Schema,
  H32Schema,
  NativeTxInclusionArgsSchema,
  ProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

/**
 * Reserved pre-registration category id for the family. It appears only in
 * emulator wiring (an `extraCategories` sidecar); never in the canonical
 * catalogue order and never through the deployment manifest, whose parser
 * silently drops non-canonical keys.
 */
export const VALUE_NOT_PRESERVED_RESERVED_CATEGORY_ID_V1 = "00000019";

// ---------------------------------------------------------------------------
// The single-asset claim (lib step-01)
// ---------------------------------------------------------------------------

/** `ClaimedAssetV1`: `AdaAsset` is Constr 0, `TokenAsset` Constr 1. */
export const ClaimedAssetV1Schema = Data.Enum([
  Data.Literal("AdaAsset"),
  Data.Object({
    TokenAsset: Data.Object({
      policy_id: Data.Bytes({ minLength: 28, maxLength: 28 }),
      asset_name: Data.Bytes({ maxLength: 32 }),
    }),
  }),
]);
export type ClaimedAssetV1 = Data.Static<typeof ClaimedAssetV1Schema>;
export const ClaimedAssetV1 = ClaimedAssetV1Schema as unknown as ClaimedAssetV1;

/**
 * `ClaimedImbalanceDirectionV1`: `ClaimedAssetInflated` (Constr 0) convicts
 * on `final_delta < 0`, `ClaimedAssetDeflated` (Constr 1) on
 * `final_delta > 0`.
 */
export const ClaimedImbalanceDirectionV1Schema = Data.Enum([
  Data.Literal("ClaimedAssetInflated"),
  Data.Literal("ClaimedAssetDeflated"),
]);
export type ClaimedImbalanceDirectionV1 = Data.Static<
  typeof ClaimedImbalanceDirectionV1Schema
>;
export const ClaimedImbalanceDirectionV1 =
  ClaimedImbalanceDirectionV1Schema as unknown as ClaimedImbalanceDirectionV1;

// ---------------------------------------------------------------------------
// Step 01 — bind and freeze the claim
// ---------------------------------------------------------------------------

export const ValueNotPreservedStep01ArgsSchema = Data.Object({
  tx_inclusion: NativeTxInclusionArgsSchema,
  claimed_asset: ClaimedAssetV1Schema,
  claimed_direction: ClaimedImbalanceDirectionV1Schema,
});
export type ValueNotPreservedStep01Args = Data.Static<
  typeof ValueNotPreservedStep01ArgsSchema
>;
export const ValueNotPreservedStep01Args =
  ValueNotPreservedStep01ArgsSchema as unknown as ValueNotPreservedStep01Args;

export const ValueNotPreservedStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(ValueNotPreservedStep01ArgsSchema);
export type ValueNotPreservedStep01SpendRedeemer = Data.Static<
  typeof ValueNotPreservedStep01SpendRedeemerSchema
>;
export const ValueNotPreservedStep01SpendRedeemer =
  ValueNotPreservedStep01SpendRedeemerSchema as unknown as ValueNotPreservedStep01SpendRedeemer;

// ---------------------------------------------------------------------------
// Step 02 — the spent-input fold
// ---------------------------------------------------------------------------

export const ValueNotPreservedStep02StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  claimed_asset: ClaimedAssetV1Schema,
  claimed_direction: ClaimedImbalanceDirectionV1Schema,
  committed_fee: Data.Integer(),
  prev_utxos_root: H32Schema,
  input_cursor: Data.Integer(),
  claimed_delta: Data.Integer(),
});
export type ValueNotPreservedStep02State = Data.Static<
  typeof ValueNotPreservedStep02StateSchema
>;
export const ValueNotPreservedStep02State =
  ValueNotPreservedStep02StateSchema as unknown as ValueNotPreservedStep02State;

export const ValueNotPreservedStep02DatumSchema = faultProofStepDatumSchema(
  ValueNotPreservedStep02StateSchema,
);
export type ValueNotPreservedStep02Datum = Data.Static<
  typeof ValueNotPreservedStep02DatumSchema
>;
export const ValueNotPreservedStep02Datum =
  ValueNotPreservedStep02DatumSchema as unknown as ValueNotPreservedStep02Datum;

/** One authenticated asset leaf of a spent input's descriptor, in index order. */
export const AssetLeafOpeningV1Schema = Data.Object({
  policy_id: Data.Bytes({ minLength: 28, maxLength: 28 }),
  asset_name: Data.Bytes({ maxLength: 32 }),
  quantity: Data.Integer(),
  siblings: Data.Array(H32Schema),
});
export type AssetLeafOpeningV1 = Data.Static<typeof AssetLeafOpeningV1Schema>;
export const AssetLeafOpeningV1 =
  AssetLeafOpeningV1Schema as unknown as AssetLeafOpeningV1;

/** Wire twin of the aiken `validation_merkle_v1.FrontierPeak`. */
export const FrontierPeakV1Schema = Data.Object({
  height: Data.Integer(),
  hash: H32Schema,
});
export type FrontierPeakV1 = Data.Static<typeof FrontierPeakV1Schema>;
export const FrontierPeakV1 = FrontierPeakV1Schema as unknown as FrontierPeakV1;

/** The pre-state value witness for one spend input. */
export const SpentInputValueWitnessV1Schema = Data.Object({
  descriptor_cbor: Data.Bytes(),
  ledger_membership_proof: ProofSchema,
  asset_peaks: Data.Array(FrontierPeakV1Schema),
  asset_openings: Data.Array(AssetLeafOpeningV1Schema),
});
export type SpentInputValueWitnessV1 = Data.Static<
  typeof SpentInputValueWitnessV1Schema
>;
export const SpentInputValueWitnessV1 =
  SpentInputValueWitnessV1Schema as unknown as SpentInputValueWitnessV1;

export const ValueNotPreservedStep02FoldArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  spend_inputs_opening: FieldOpeningV1Schema,
  value_witness: SpentInputValueWitnessV1Schema,
});
export type ValueNotPreservedStep02FoldArgs = Data.Static<
  typeof ValueNotPreservedStep02FoldArgsSchema
>;
export const ValueNotPreservedStep02FoldArgs =
  ValueNotPreservedStep02FoldArgsSchema as unknown as ValueNotPreservedStep02FoldArgs;

/** Step-02 redeemer arms: `FoldInput` is Constr 0, `FinishInputs` Constr 1. */
export const ValueNotPreservedStep02ArgsSchema = Data.Enum([
  Data.Object({
    FoldInput: Data.Tuple([ValueNotPreservedStep02FoldArgsSchema]),
  }),
  Data.Object({
    FinishInputs: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      spend_inputs_opening: FieldOpeningV1Schema,
    }),
  }),
]);
export type ValueNotPreservedStep02Args = Data.Static<
  typeof ValueNotPreservedStep02ArgsSchema
>;
export const ValueNotPreservedStep02Args =
  ValueNotPreservedStep02ArgsSchema as unknown as ValueNotPreservedStep02Args;

export const ValueNotPreservedStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(ValueNotPreservedStep02ArgsSchema);
export type ValueNotPreservedStep02SpendRedeemer = Data.Static<
  typeof ValueNotPreservedStep02SpendRedeemerSchema
>;
export const ValueNotPreservedStep02SpendRedeemer =
  ValueNotPreservedStep02SpendRedeemerSchema as unknown as ValueNotPreservedStep02SpendRedeemer;

// ---------------------------------------------------------------------------
// Step 03 — outputs, mint and fee
// ---------------------------------------------------------------------------

export const ValueNotPreservedStep03StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  claimed_asset: ClaimedAssetV1Schema,
  claimed_direction: ClaimedImbalanceDirectionV1Schema,
  committed_fee: Data.Integer(),
  claimed_delta: Data.Integer(),
});
export type ValueNotPreservedStep03State = Data.Static<
  typeof ValueNotPreservedStep03StateSchema
>;
export const ValueNotPreservedStep03State =
  ValueNotPreservedStep03StateSchema as unknown as ValueNotPreservedStep03State;

export const ValueNotPreservedStep03DatumSchema = faultProofStepDatumSchema(
  ValueNotPreservedStep03StateSchema,
);
export type ValueNotPreservedStep03Datum = Data.Static<
  typeof ValueNotPreservedStep03DatumSchema
>;
export const ValueNotPreservedStep03Datum =
  ValueNotPreservedStep03DatumSchema as unknown as ValueNotPreservedStep03Datum;

export const ValueNotPreservedStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  native_tx_compact_cbor: Data.Bytes(),
  outputs_carriage: FieldCarriageV1Schema,
  /** `Some` exactly for a token claim; an ADA claim must carry `None`. */
  mint_carriage: Data.Nullable(FieldCarriageV1Schema),
});
export type ValueNotPreservedStep03Args = Data.Static<
  typeof ValueNotPreservedStep03ArgsSchema
>;
export const ValueNotPreservedStep03Args =
  ValueNotPreservedStep03ArgsSchema as unknown as ValueNotPreservedStep03Args;

export const ValueNotPreservedStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(ValueNotPreservedStep03ArgsSchema);
export type ValueNotPreservedStep03SpendRedeemer = Data.Static<
  typeof ValueNotPreservedStep03SpendRedeemerSchema
>;
export const ValueNotPreservedStep03SpendRedeemer =
  ValueNotPreservedStep03SpendRedeemerSchema as unknown as ValueNotPreservedStep03SpendRedeemer;

// ---------------------------------------------------------------------------
// Step 04 — finalize
// ---------------------------------------------------------------------------

export const ValueNotPreservedStep04StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  claimed_asset: ClaimedAssetV1Schema,
  claimed_direction: ClaimedImbalanceDirectionV1Schema,
  final_delta: Data.Integer(),
});
export type ValueNotPreservedStep04State = Data.Static<
  typeof ValueNotPreservedStep04StateSchema
>;
export const ValueNotPreservedStep04State =
  ValueNotPreservedStep04StateSchema as unknown as ValueNotPreservedStep04State;

export const ValueNotPreservedStep04DatumSchema = faultProofStepDatumSchema(
  ValueNotPreservedStep04StateSchema,
);
export type ValueNotPreservedStep04Datum = Data.Static<
  typeof ValueNotPreservedStep04DatumSchema
>;
export const ValueNotPreservedStep04Datum =
  ValueNotPreservedStep04DatumSchema as unknown as ValueNotPreservedStep04Datum;

export const ValueNotPreservedStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type ValueNotPreservedStep04Args = Data.Static<
  typeof ValueNotPreservedStep04ArgsSchema
>;
export const ValueNotPreservedStep04Args =
  ValueNotPreservedStep04ArgsSchema as unknown as ValueNotPreservedStep04Args;

export const ValueNotPreservedStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(ValueNotPreservedStep04ArgsSchema);
export type ValueNotPreservedStep04SpendRedeemer = Data.Static<
  typeof ValueNotPreservedStep04SpendRedeemerSchema
>;
export const ValueNotPreservedStep04SpendRedeemer =
  ValueNotPreservedStep04SpendRedeemerSchema as unknown as ValueNotPreservedStep04SpendRedeemer;

// ---------------------------------------------------------------------------
// Local mirrors of the on-chain predicates
// ---------------------------------------------------------------------------

/** Twin of `claimed_asset_is_well_formed_v1`, applied before paying for step-01. */
export const claimedAssetIsWellFormedV1 = (claim: ClaimedAssetV1): boolean => {
  if (claim === "AdaAsset") return true;
  const { policy_id, asset_name } = claim.TokenAsset;
  return policy_id.length === 56 && asset_name.length <= 64;
};

/**
 * Twin of `value_not_preserved_fault_is_established_v1` — the step-04
 * conviction condition. A zero delta satisfies neither direction, so a
 * balanced fold is never finalizable.
 */
export const valueNotPreservedFaultIsEstablishedV1 = ({
  claimedDirection,
  finalDelta,
}: {
  readonly claimedDirection: ClaimedImbalanceDirectionV1;
  readonly finalDelta: bigint;
}): boolean =>
  claimedDirection === "ClaimedAssetInflated"
    ? finalDelta < 0n
    : finalDelta > 0n;
