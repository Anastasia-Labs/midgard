/** Claim custody wire shared by the direct purpose families
 * (`direct-script-derivation/claim.ak`, `claim-purpose.ak`,
 * `redeemer-scan.ak`, `claim-terminal.ak`). Constructor and field order is
 * wire format and mirrors the on-chain declarations exactly. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import {
  DirectPointerStateSchema,
  DirectSourceSelectionSchema,
} from "./direct-purpose-workers.js";
import {
  DirectPurposeCoordinateSchema,
  DirectPurposeSchema,
} from "./direct-script-derivation.js";
import { DirectSourceBoundIndexSchema } from "./direct-source-workers.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";

/** The field-8 pointer scan for one authenticated purpose. */
export const DirectRedeemerScanStateSchema = Data.Object({
  authenticated: DirectSourceSelectionSchema,
  scan: DirectPointerStateSchema,
});
export type DirectRedeemerScanState = Data.Static<
  typeof DirectRedeemerScanStateSchema
>;
export const DirectRedeemerScanState = asDataType<DirectRedeemerScanState>(
  DirectRedeemerScanStateSchema,
);

/** `redeemer_missing` is meaningful only when a source was selected. */
export const DirectRedeemerDecisionStateSchema = Data.Object({
  authenticated: DirectSourceSelectionSchema,
  redeemer_missing: Data.Boolean(),
});
export type DirectRedeemerDecisionState = Data.Static<
  typeof DirectRedeemerDecisionStateSchema
>;
export const DirectRedeemerDecisionState =
  asDataType<DirectRedeemerDecisionState>(DirectRedeemerDecisionStateSchema);

/** 0 LeafPurpose, 1 RedeemerPointer. */
export const DirectClaimTargetSchema = Data.Enum([
  Data.Literal("LeafPurpose"),
  Data.Object({
    RedeemerPointer: Data.Object({
      redeemer_index: Data.Integer(),
      coordinate: DirectPurposeCoordinateSchema,
    }),
  }),
]);
export type DirectClaimTarget = Data.Static<typeof DirectClaimTargetSchema>;
export const DirectClaimTarget = asDataType<DirectClaimTarget>(
  DirectClaimTargetSchema,
);

/** `observed` is null only on a claimed-only entry. */
export const DirectClaimContextSchema = Data.Object({
  bound: DirectSourceBoundIndexSchema,
  observed: Data.Nullable(DirectRedeemerDecisionStateSchema),
  target: DirectClaimTargetSchema,
});
export type DirectClaimContext = Data.Static<typeof DirectClaimContextSchema>;
export const DirectClaimContext = asDataType<DirectClaimContext>(
  DirectClaimContextSchema,
);

/** 0 NotRequired, 1 ImpossiblePurpose, 2 ExistingPurpose. */
export const DirectClaimedPurposeFactSchema = Data.Enum([
  Data.Literal("NotRequired"),
  Data.Object({
    ImpossiblePurpose: Data.Object({
      coordinate: DirectPurposeCoordinateSchema,
    }),
  }),
  Data.Object({
    ExistingPurpose: Data.Object({ purpose: DirectPurposeSchema }),
  }),
]);
export type DirectClaimedPurposeFact = Data.Static<
  typeof DirectClaimedPurposeFactSchema
>;
export const DirectClaimedPurposeFact = asDataType<DirectClaimedPurposeFact>(
  DirectClaimedPurposeFactSchema,
);

export const DirectClaimPurposeStateSchema = Data.Object({
  context: DirectClaimContextSchema,
  purpose: DirectClaimedPurposeFactSchema,
});
export type DirectClaimPurposeState = Data.Static<
  typeof DirectClaimPurposeStateSchema
>;
export const DirectClaimPurposeState = asDataType<DirectClaimPurposeState>(
  DirectClaimPurposeStateSchema,
);

/** 0 NotRequired, 1 ImpossiblePurpose, 2 SourceAbsent, 3 NativeSource,
 * 4 SourcePresent, 5 RedeemerPresent, 6 RedeemerAbsent, 7 RedeemerRepeated,
 * 8 RedeemerOutOfRange. The last two are field-8 walk results for a
 * `RedeemerPointer` target. */
export const DirectClaimedPredicateFactSchema = Data.Enum([
  Data.Literal("NotRequired"),
  Data.Object({
    ImpossiblePurpose: Data.Object({
      coordinate: DirectPurposeCoordinateSchema,
    }),
  }),
  Data.Object({
    SourceAbsent: Data.Object({ selection: DirectSourceSelectionSchema }),
  }),
  Data.Object({
    NativeSource: Data.Object({ selection: DirectSourceSelectionSchema }),
  }),
  Data.Object({
    SourcePresent: Data.Object({ selection: DirectSourceSelectionSchema }),
  }),
  Data.Object({
    RedeemerPresent: Data.Object({
      decision: DirectRedeemerDecisionStateSchema,
    }),
  }),
  Data.Object({
    RedeemerAbsent: Data.Object({
      decision: DirectRedeemerDecisionStateSchema,
    }),
  }),
  Data.Literal("RedeemerRepeated"),
  Data.Literal("RedeemerOutOfRange"),
]);
export type DirectClaimedPredicateFact = Data.Static<
  typeof DirectClaimedPredicateFactSchema
>;
export const DirectClaimedPredicateFact =
  asDataType<DirectClaimedPredicateFact>(DirectClaimedPredicateFactSchema);

/** The terminal's only input. */
export const DirectClaimFinalStateSchema = Data.Object({
  context: DirectClaimContextSchema,
  claim: DirectClaimedPredicateFactSchema,
});
export type DirectClaimFinalState = Data.Static<
  typeof DirectClaimFinalStateSchema
>;
export const DirectClaimFinalState = asDataType<DirectClaimFinalState>(
  DirectClaimFinalStateSchema,
);

export const DirectClaimedRedeemerScanSchema = Data.Object({
  context: DirectClaimContextSchema,
  scan: DirectRedeemerScanStateSchema,
});
export type DirectClaimedRedeemerScan = Data.Static<
  typeof DirectClaimedRedeemerScanSchema
>;
export const DirectClaimedRedeemerScan = asDataType<DirectClaimedRedeemerScan>(
  DirectClaimedRedeemerScanSchema,
);

/** The claim terminal's finalize arguments. */
export const DirectClaimFinalizeArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type DirectClaimFinalizeArgs = Data.Static<
  typeof DirectClaimFinalizeArgsSchema
>;
export const DirectClaimFinalizeArgs = asDataType<DirectClaimFinalizeArgs>(
  DirectClaimFinalizeArgsSchema,
);

export const DirectClaimFinalizeRedeemerSchema = faultProofStepRedeemerSchema(
  DirectClaimFinalizeArgsSchema,
);
export type DirectClaimFinalizeRedeemer = Data.Static<
  typeof DirectClaimFinalizeRedeemerSchema
>;
export const DirectClaimFinalizeRedeemer =
  asDataType<DirectClaimFinalizeRedeemer>(DirectClaimFinalizeRedeemerSchema);

export const DirectClaimFinalDatumSchema = faultProofStepDatumSchema(
  DirectClaimFinalStateSchema,
);
export type DirectClaimFinalDatum = Data.Static<
  typeof DirectClaimFinalDatumSchema
>;
export const DirectClaimFinalDatum = asDataType<DirectClaimFinalDatum>(
  DirectClaimFinalDatumSchema,
);
