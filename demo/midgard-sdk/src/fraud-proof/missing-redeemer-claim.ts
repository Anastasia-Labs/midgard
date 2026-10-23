/** Missing-redeemer claim custody wire (`missing-redeemer/claim.ak`,
 * `missing-redeemer/claim-purpose.ak`). Constructor and field order is wire
 * format and mirrors the on-chain declarations exactly. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { DirectSourceSelectionSchema } from "./direct-purpose-workers.js";
import {
  DirectPurposeCoordinateSchema,
  DirectPurposeSchema,
} from "./direct-script-derivation.js";
import { DirectSourceBoundIndexSchema } from "./direct-source-workers.js";
import {
  MissingRedeemerDecisionStateSchema,
  MissingRedeemerScanStateSchema,
} from "./missing-redeemer.js";

/** `observed` is null only on the claimed-only entry. */
export const MissingRedeemerClaimContextSchema = Data.Object({
  bound: DirectSourceBoundIndexSchema,
  observed: Data.Nullable(MissingRedeemerDecisionStateSchema),
});
export type MissingRedeemerClaimContext = Data.Static<
  typeof MissingRedeemerClaimContextSchema
>;
export const MissingRedeemerClaimContext =
  asDataType<MissingRedeemerClaimContext>(MissingRedeemerClaimContextSchema);

/** 0 NotRequired, 1 ImpossiblePurpose, 2 ExistingPurpose. */
export const MissingRedeemerClaimedPurposeFactSchema = Data.Enum([
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
export type MissingRedeemerClaimedPurposeFact = Data.Static<
  typeof MissingRedeemerClaimedPurposeFactSchema
>;
export const MissingRedeemerClaimedPurposeFact =
  asDataType<MissingRedeemerClaimedPurposeFact>(
    MissingRedeemerClaimedPurposeFactSchema,
  );

export const MissingRedeemerClaimPurposeStateSchema = Data.Object({
  context: MissingRedeemerClaimContextSchema,
  purpose: MissingRedeemerClaimedPurposeFactSchema,
});
export type MissingRedeemerClaimPurposeState = Data.Static<
  typeof MissingRedeemerClaimPurposeStateSchema
>;
export const MissingRedeemerClaimPurposeState =
  asDataType<MissingRedeemerClaimPurposeState>(
    MissingRedeemerClaimPurposeStateSchema,
  );

/** 0 NotRequired, 1 ImpossiblePurpose, 2 SourceAbsent, 3 NativeSource,
 * 4 SourcePresent, 5 RedeemerPresent, 6 RedeemerAbsent. */
export const MissingRedeemerClaimedPredicateFactSchema = Data.Enum([
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
      decision: MissingRedeemerDecisionStateSchema,
    }),
  }),
  Data.Object({
    RedeemerAbsent: Data.Object({
      decision: MissingRedeemerDecisionStateSchema,
    }),
  }),
]);
export type MissingRedeemerClaimedPredicateFact = Data.Static<
  typeof MissingRedeemerClaimedPredicateFactSchema
>;
export const MissingRedeemerClaimedPredicateFact =
  asDataType<MissingRedeemerClaimedPredicateFact>(
    MissingRedeemerClaimedPredicateFactSchema,
  );

/** The terminal's only input. */
export const MissingRedeemerFinalStateSchema = Data.Object({
  context: MissingRedeemerClaimContextSchema,
  claim: MissingRedeemerClaimedPredicateFactSchema,
});
export type MissingRedeemerFinalState = Data.Static<
  typeof MissingRedeemerFinalStateSchema
>;
export const MissingRedeemerFinalState = asDataType<MissingRedeemerFinalState>(
  MissingRedeemerFinalStateSchema,
);

export const MissingRedeemerClaimedRedeemerScanSchema = Data.Object({
  context: MissingRedeemerClaimContextSchema,
  scan: MissingRedeemerScanStateSchema,
});
export type MissingRedeemerClaimedRedeemerScan = Data.Static<
  typeof MissingRedeemerClaimedRedeemerScanSchema
>;
export const MissingRedeemerClaimedRedeemerScan =
  asDataType<MissingRedeemerClaimedRedeemerScan>(
    MissingRedeemerClaimedRedeemerScanSchema,
  );
