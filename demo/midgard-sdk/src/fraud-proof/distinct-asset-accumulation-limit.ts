/** Direct `distinctAssetAccumulationLimit` wire
 * (`distinct-asset-accumulation-limit/{fold,workers,terminal}.ak`) and the
 * fold's twin. Constructor and field order is wire format. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { ProofSchema } from "../common.js";
import {
  DirectFieldCursorSchema,
  DirectScriptBoundSchema,
} from "./direct-script-derivation.js";
import { FieldOpeningSchema } from "./field-opening.js";
import type { NetworkOrderContext } from "./first-fault.js";
import {
  leafAdmitsLaterFault,
  type ValueFaultPosition,
  valuePositionOfReason,
} from "./first-fault-later-phases.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";
import {
  terminalVerdictContradiction,
  type VerdictSubject,
} from "./proof-thread-substrate.js";

/** The producer refuses an asset entry once this many precede it. */
export const DISTINCT_ASSET_ACCUMULATION_LIMIT_MAX_ENTRIES = 16_384n;

export const DistinctAssetOutputResumeSchema = Data.Object({
  offset: Data.Integer(),
  policies_remaining: Data.Integer(),
  assets_remaining: Data.Integer(),
  asset_index: Data.Integer(),
});

export const DistinctAssetWalkSchema = Data.Object({
  cursor: DirectFieldCursorSchema,
  complete: Data.Boolean(),
  entry_count: Data.Integer(),
  resume: Data.Nullable(DistinctAssetOutputResumeSchema),
  mint_index: Data.Integer(),
});
export type DistinctAssetWalk = Data.Static<typeof DistinctAssetWalkSchema>;
export const DistinctAssetWalk = asDataType<DistinctAssetWalk>(
  DistinctAssetWalkSchema,
);

export const DistinctAssetPendingInputsSchema = Data.Object({
  resume_script_hash: Data.Bytes(),
  state: DistinctAssetWalkSchema,
  first_ordinal: Data.Integer(),
  pending: Data.Array(Data.Bytes()),
});
export type DistinctAssetPendingInputs = Data.Static<
  typeof DistinctAssetPendingInputsSchema
>;

export const DistinctAssetRankSchema = Data.Object({
  bound: DirectScriptBoundSchema,
  input_ordinal: Data.Integer(),
  input_item: Data.Bytes(),
  asset_index: Data.Integer(),
});
export type DistinctAssetRank = Data.Static<typeof DistinctAssetRankSchema>;

/** `first_fault_v1.ValueFaultPositionV1`. */
export const DistinctAssetCrossingSchema = Data.Enum([
  Data.Object({
    InputAssetFault: Data.Object({
      input_index: Data.Integer(),
      asset_index: Data.Integer(),
    }),
  }),
  Data.Object({
    OutputAssetFault: Data.Object({
      output_index: Data.Integer(),
      asset_index: Data.Integer(),
    }),
  }),
  Data.Object({
    MintAssetFault: Data.Object({ mint_index: Data.Integer() }),
  }),
]);
export type DistinctAssetCrossing = Data.Static<
  typeof DistinctAssetCrossingSchema
>;

export const DistinctAssetTerminalSchema = Data.Object({
  bound: DirectScriptBoundSchema,
  crossing: Data.Nullable(DistinctAssetCrossingSchema),
});
export type DistinctAssetTerminal = Data.Static<
  typeof DistinctAssetTerminalSchema
>;

export const DistinctAssetInputWitnessSchema = Data.Object({
  descriptor_cbor: Data.Bytes(),
  proof: ProofSchema,
});
export type DistinctAssetInputWitness = Data.Static<
  typeof DistinctAssetInputWitnessSchema
>;

const indices = { input_index: Data.Integer(), output_index: Data.Integer() };

export const DistinctAssetWalkDatumSchema = faultProofStepDatumSchema(
  DistinctAssetWalkSchema,
);
export type DistinctAssetWalkDatum = Data.Static<
  typeof DistinctAssetWalkDatumSchema
>;
export const DistinctAssetWalkDatum = asDataType<DistinctAssetWalkDatum>(
  DistinctAssetWalkDatumSchema,
);

export const DistinctAssetPendingDatumSchema = faultProofStepDatumSchema(
  DistinctAssetPendingInputsSchema,
);
export type DistinctAssetPendingDatum = Data.Static<
  typeof DistinctAssetPendingDatumSchema
>;
export const DistinctAssetPendingDatum = asDataType<DistinctAssetPendingDatum>(
  DistinctAssetPendingDatumSchema,
);

export const DistinctAssetRankDatumSchema = faultProofStepDatumSchema(
  DistinctAssetRankSchema,
);
export type DistinctAssetRankDatum = Data.Static<
  typeof DistinctAssetRankDatumSchema
>;
export const DistinctAssetRankDatum = asDataType<DistinctAssetRankDatum>(
  DistinctAssetRankDatumSchema,
);

export const DistinctAssetTerminalDatumSchema = faultProofStepDatumSchema(
  DistinctAssetTerminalSchema,
);
export type DistinctAssetTerminalDatum = Data.Static<
  typeof DistinctAssetTerminalDatumSchema
>;
export const DistinctAssetTerminalDatum =
  asDataType<DistinctAssetTerminalDatum>(DistinctAssetTerminalDatumSchema);

export const DistinctAssetEntryRedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({ ...indices, opening: FieldOpeningSchema }),
);
export type DistinctAssetEntryRedeemer = Data.Static<
  typeof DistinctAssetEntryRedeemerSchema
>;
export const DistinctAssetEntryRedeemer =
  asDataType<DistinctAssetEntryRedeemer>(DistinctAssetEntryRedeemerSchema);

/** The spend take (an item count), the output walk (a work budget) and the
 * mint walk (a policy count). */
export const DistinctAssetFieldRedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    ...indices,
    opening: FieldOpeningSchema,
    checkpoint_bytes: Data.Bytes(),
    budget: Data.Integer(),
  }),
);
export type DistinctAssetFieldRedeemer = Data.Static<
  typeof DistinctAssetFieldRedeemerSchema
>;
export const DistinctAssetFieldRedeemer =
  asDataType<DistinctAssetFieldRedeemer>(DistinctAssetFieldRedeemerSchema);

export const DistinctAssetResolveRedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    ...indices,
    witnesses: Data.Array(DistinctAssetInputWitnessSchema),
  }),
);
export type DistinctAssetResolveRedeemer = Data.Static<
  typeof DistinctAssetResolveRedeemerSchema
>;
export const DistinctAssetResolveRedeemer =
  asDataType<DistinctAssetResolveRedeemer>(DistinctAssetResolveRedeemerSchema);

export const DistinctAssetRankRedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({
    ...indices,
    opening: FieldOpeningSchema,
    rank: Data.Integer(),
  }),
);
export type DistinctAssetRankRedeemer = Data.Static<
  typeof DistinctAssetRankRedeemerSchema
>;
export const DistinctAssetRankRedeemer = asDataType<DistinctAssetRankRedeemer>(
  DistinctAssetRankRedeemerSchema,
);

export const DistinctAssetTerminalRedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({ ...indices, fraud_proof_mint_redeemer_index: Data.Integer() }),
);
export type DistinctAssetTerminalRedeemer = Data.Static<
  typeof DistinctAssetTerminalRedeemerSchema
>;
export const DistinctAssetTerminalRedeemer =
  asDataType<DistinctAssetTerminalRedeemer>(
    DistinctAssetTerminalRedeemerSchema,
  );

/** The entry units the producer folds, in its order: each spent input's
 * asset entries at its schedule position, then each output's per-policy
 * asset-map sizes, then each mint policy's declared asset count. */
export type DistinctAssetUnits = Readonly<{
  inputs: readonly Readonly<{ scheduleIndex: bigint; entries: bigint }>[];
  outputs: readonly (readonly bigint[])[];
  mint: readonly bigint[];
}>;

/** Twin of the family's fold: the first entry the producer refuses, if any.
 * A unit crosses when the running count exceeds the bound, at the entry whose
 * index within the unit is the bound minus the count before it. */
export const distinctAssetCrossing = (
  units: DistinctAssetUnits,
): ValueFaultPosition | null => {
  const max = DISTINCT_ASSET_ACCUMULATION_LIMIT_MAX_ENTRIES;
  let count = 0n;
  const admit = (entries: bigint): bigint | null => {
    if (entries < 0n) throw new Error("negative asset entry count");
    if (count + entries > max) return max - count;
    count += entries;
    return null;
  };
  for (const input of units.inputs) {
    const at = admit(input.entries);
    if (at !== null)
      return {
        kind: "input",
        inputIndex: input.scheduleIndex,
        assetIndex: at,
      };
  }
  for (const [outputIndex, policies] of units.outputs.entries()) {
    let assetIndex = 0n;
    for (const entries of policies) {
      const at = admit(entries);
      if (at !== null)
        return {
          kind: "output",
          outputIndex: BigInt(outputIndex),
          assetIndex: assetIndex + at,
        };
      assetIndex += entries;
    }
  }
  let mintIndex = 0n;
  for (const declared of units.mint) {
    const at = admit(declared);
    if (at !== null) return { kind: "mint", mintIndex: mintIndex + at };
    mintIndex += declared;
  }
  return null;
};

const samePosition = (
  left: ValueFaultPosition,
  right: ValueFaultPosition,
): boolean => {
  switch (left.kind) {
    case "input":
      return (
        right.kind === "input" &&
        left.inputIndex === right.inputIndex &&
        left.assetIndex === right.assetIndex
      );
    case "output":
      return (
        right.kind === "output" &&
        left.outputIndex === right.outputIndex &&
        left.assetIndex === right.assetIndex
      );
    case "mint":
      return right.kind === "mint" && left.mintIndex === right.mintIndex;
  }
};

/** Twin of `terminal.convicts_v1` once the crossing is derived: an acceptance
 * convicts on a crossing; a rejection recording an accumulation-limit reason
 * convicts unless it names the derived crossing; a rejection for another
 * reason convicts when a crossing holds and the reason ranks after it. */
export const distinctAssetAccumulationLimitConvicts = (
  subject: VerdictSubject,
  context: NetworkOrderContext,
  crossing: ValueFaultPosition | null,
): boolean => {
  const reason = subject.rejection_reason;
  if (reason === null)
    return terminalVerdictContradiction(subject, crossing !== null);
  const recorded = valuePositionOfReason(reason);
  return terminalVerdictContradiction(
    subject,
    recorded !== null
      ? crossing !== null && samePosition(recorded, crossing)
      : crossing === null ||
          leafAdmitsLaterFault(subject, context, {
            phase: "value",
            position: crossing,
          }),
  );
};

/** The wire form of a derived crossing, as the terminal datum records it. */
export const distinctAssetCrossingData = (
  crossing: ValueFaultPosition | null,
): DistinctAssetCrossing | null => {
  if (crossing === null) return null;
  switch (crossing.kind) {
    case "input":
      return {
        InputAssetFault: {
          input_index: crossing.inputIndex,
          asset_index: crossing.assetIndex,
        },
      };
    case "output":
      return {
        OutputAssetFault: {
          output_index: crossing.outputIndex,
          asset_index: crossing.assetIndex,
        },
      };
    case "mint":
      return { MintAssetFault: { mint_index: crossing.mintIndex } };
  }
};

/** The crossing a terminal datum records, read back from its wire form. */
export const distinctAssetCrossingPosition = (
  crossing: DistinctAssetCrossing | null,
): ValueFaultPosition | null => {
  if (crossing === null) return null;
  if ("InputAssetFault" in crossing)
    return {
      kind: "input",
      inputIndex: crossing.InputAssetFault.input_index,
      assetIndex: crossing.InputAssetFault.asset_index,
    };
  if ("OutputAssetFault" in crossing)
    return {
      kind: "output",
      outputIndex: crossing.OutputAssetFault.output_index,
      assetIndex: crossing.OutputAssetFault.asset_index,
    };
  return { kind: "mint", mintIndex: crossing.MintAssetFault.mint_index };
};
