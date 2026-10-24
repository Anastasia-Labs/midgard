/** Role-local missing-redeemer wire: family entry, claim custody,
 * adjudication and the claimed field-8 scan (`missing-redeemer/entry.ak`,
 * `claim-start.ak`, `claim-spend.ak`, `claim-mint.ak`, `claim-receive.ak`,
 * `adjudicate.ak`, `step-05.ak`). Constructor and field order is wire format. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { ProofSchema } from "../common.js";
import {
  DirectMintWorkStateSchema,
  DirectProgressArgsSchema,
  DirectScanActionSchema,
} from "./direct-purpose-workers.js";
import {
  DirectFieldCursorSchema,
  DirectReceiveDeclarationActionSchema,
  DirectReceiveDeclarationStateSchema,
  DirectReceiveOutputStateSchema,
  DirectReceiveOutputWitnessSchema,
  DirectScriptSourceSchema,
} from "./direct-script-derivation.js";
import { DirectSourceIndexSchema } from "./direct-source-index-state.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  MissingRedeemerClaimContextSchema,
  MissingRedeemerClaimedRedeemerScanSchema,
  MissingRedeemerClaimPurposeStateSchema,
  MissingRedeemerFinalStateSchema,
} from "./missing-redeemer-claim.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  MembershipCarriageSchema,
} from "./native.js";

const indices = { input_index: Data.Integer(), output_index: Data.Integer() };
const opened = { ...indices, opening: FieldOpeningSchema };

/** 0 Observe, 1 Claim. */
export const MissingRedeemerEntryActionSchema = Data.Enum([
  Data.Object({ Observe: Data.Object(indices) }),
  Data.Object({ Claim: Data.Object(indices) }),
]);
export type MissingRedeemerEntryAction = Data.Static<
  typeof MissingRedeemerEntryActionSchema
>;
export const MissingRedeemerEntryAction =
  asDataType<MissingRedeemerEntryAction>(MissingRedeemerEntryActionSchema);

/** 0 Complete, 1 Spend, 2 SpendOutOfRange, 3 Observe, 4 Mint, 5 Receive. */
export const MissingRedeemerClaimStartActionSchema = Data.Enum([
  Data.Object({ Complete: Data.Object(indices) }),
  Data.Object({ Spend: Data.Object(indices) }),
  Data.Object({ SpendOutOfRange: Data.Object(opened) }),
  Data.Object({ Observe: Data.Object(opened) }),
  Data.Object({ Mint: Data.Object(opened) }),
  Data.Object({ Receive: Data.Object(opened) }),
]);
export type MissingRedeemerClaimStartAction = Data.Static<
  typeof MissingRedeemerClaimStartActionSchema
>;
export const MissingRedeemerClaimStartAction =
  asDataType<MissingRedeemerClaimStartAction>(
    MissingRedeemerClaimStartActionSchema,
  );

export const MissingRedeemerClaimFinishOpeningArgsSchema = Data.Object({
  ...opened,
  checkpoint_bytes: Data.Bytes(),
});

/** The spend start's only action: resolve the raw item and rank a first
 * page of at most the scan batch. */
export const MissingRedeemerClaimSpendOpenActionSchema = Data.Object({
  ...indices,
  raw_item_index: Data.Integer(),
  opening: FieldOpeningSchema,
  descriptor_cbor: Data.Bytes(),
  membership: MembershipCarriageSchema,
  budget: Data.Integer(),
});

export const MissingRedeemerClaimSpendStateSchema = Data.Object({
  context: MissingRedeemerClaimContextSchema,
  source_index: DirectSourceIndexSchema,
  cursor: DirectFieldCursorSchema,
  raw_item_index: Data.Integer(),
  target: Data.Object({
    script_hash: Data.Nullable(Data.Bytes()),
    subject: Data.Bytes(),
  }),
  rank: Data.Integer(),
  complete: Data.Boolean(),
});
export type MissingRedeemerClaimSpendState = Data.Static<
  typeof MissingRedeemerClaimSpendStateSchema
>;
export const MissingRedeemerClaimSpendState =
  asDataType<MissingRedeemerClaimSpendState>(
    MissingRedeemerClaimSpendStateSchema,
  );

export const MissingRedeemerClaimMintStateSchema = Data.Object({
  context: MissingRedeemerClaimContextSchema,
  work: DirectMintWorkStateSchema,
});
export type MissingRedeemerClaimMintState = Data.Static<
  typeof MissingRedeemerClaimMintStateSchema
>;
export const MissingRedeemerClaimMintState =
  asDataType<MissingRedeemerClaimMintState>(
    MissingRedeemerClaimMintStateSchema,
  );

/** 0 Scan, 1 Finish. */
export const MissingRedeemerClaimMintActionSchema = Data.Enum([
  Data.Object({ Scan: Data.Object({ args: DirectProgressArgsSchema }) }),
  Data.Object({
    Finish: Data.Object({ args: MissingRedeemerClaimFinishOpeningArgsSchema }),
  }),
]);

export const MissingRedeemerClaimReceiveOutputStateSchema = Data.Object({
  context: MissingRedeemerClaimContextSchema,
  target_ordinal: Data.Integer(),
  scan: DirectReceiveOutputStateSchema,
});
export type MissingRedeemerClaimReceiveOutputState = Data.Static<
  typeof MissingRedeemerClaimReceiveOutputStateSchema
>;
export const MissingRedeemerClaimReceiveOutputState =
  asDataType<MissingRedeemerClaimReceiveOutputState>(
    MissingRedeemerClaimReceiveOutputStateSchema,
  );

export const MissingRedeemerClaimReceiveDeclarationStateSchema = Data.Object({
  context: MissingRedeemerClaimContextSchema,
  scan: DirectReceiveDeclarationStateSchema,
});
export type MissingRedeemerClaimReceiveDeclarationState = Data.Static<
  typeof MissingRedeemerClaimReceiveDeclarationStateSchema
>;
export const MissingRedeemerClaimReceiveDeclarationState =
  asDataType<MissingRedeemerClaimReceiveDeclarationState>(
    MissingRedeemerClaimReceiveDeclarationStateSchema,
  );

/** 0 Scan, 1 Finish. */
export const MissingRedeemerClaimReceiveOutputActionSchema = Data.Enum([
  Data.Object({
    Scan: Data.Object({
      ...opened,
      checkpoint_bytes: Data.Bytes(),
      witnesses: Data.Array(DirectReceiveOutputWitnessSchema),
    }),
  }),
  Data.Object({ Finish: Data.Object(indices) }),
]);

/** 0 Resolve, 1 Lookup, 2 LookupScan. */
export const MissingRedeemerAdjudicateActionSchema = Data.Enum([
  Data.Object({ Resolve: Data.Object(indices) }),
  Data.Object({
    Lookup: Data.Object({
      ...indices,
      source: Data.Nullable(DirectScriptSourceSchema),
      proof: ProofSchema,
    }),
  }),
  Data.Object({
    LookupScan: Data.Object({
      ...indices,
      source: Data.Nullable(DirectScriptSourceSchema),
      proof: ProofSchema,
      opening: FieldOpeningSchema,
    }),
  }),
]);
export type MissingRedeemerAdjudicateAction = Data.Static<
  typeof MissingRedeemerAdjudicateActionSchema
>;
export const MissingRedeemerAdjudicateAction =
  asDataType<MissingRedeemerAdjudicateAction>(
    MissingRedeemerAdjudicateActionSchema,
  );

/** 0 Scan, 1 Finish. */
export const MissingRedeemerClaimedScanActionSchema = DirectScanActionSchema;

export const MissingRedeemerEntryRedeemerSchema = faultProofStepRedeemerSchema(
  MissingRedeemerEntryActionSchema,
);
export type MissingRedeemerEntryRedeemer = Data.Static<
  typeof MissingRedeemerEntryRedeemerSchema
>;
export const MissingRedeemerEntryRedeemer =
  asDataType<MissingRedeemerEntryRedeemer>(MissingRedeemerEntryRedeemerSchema);

export const MissingRedeemerClaimStartDatumSchema = faultProofStepDatumSchema(
  MissingRedeemerClaimContextSchema,
);
export type MissingRedeemerClaimStartDatum = Data.Static<
  typeof MissingRedeemerClaimStartDatumSchema
>;
export const MissingRedeemerClaimStartDatum =
  asDataType<MissingRedeemerClaimStartDatum>(
    MissingRedeemerClaimStartDatumSchema,
  );
export const MissingRedeemerClaimStartRedeemerSchema =
  faultProofStepRedeemerSchema(MissingRedeemerClaimStartActionSchema);
export type MissingRedeemerClaimStartRedeemer = Data.Static<
  typeof MissingRedeemerClaimStartRedeemerSchema
>;
export const MissingRedeemerClaimStartRedeemer =
  asDataType<MissingRedeemerClaimStartRedeemer>(
    MissingRedeemerClaimStartRedeemerSchema,
  );

export const MissingRedeemerClaimSpendOpenRedeemerSchema =
  faultProofStepRedeemerSchema(MissingRedeemerClaimSpendOpenActionSchema);
export type MissingRedeemerClaimSpendOpenRedeemer = Data.Static<
  typeof MissingRedeemerClaimSpendOpenRedeemerSchema
>;
export const MissingRedeemerClaimSpendOpenRedeemer =
  asDataType<MissingRedeemerClaimSpendOpenRedeemer>(
    MissingRedeemerClaimSpendOpenRedeemerSchema,
  );

export const MissingRedeemerClaimSpendDatumSchema = faultProofStepDatumSchema(
  MissingRedeemerClaimSpendStateSchema,
);
export type MissingRedeemerClaimSpendDatum = Data.Static<
  typeof MissingRedeemerClaimSpendDatumSchema
>;
export const MissingRedeemerClaimSpendDatum =
  asDataType<MissingRedeemerClaimSpendDatum>(
    MissingRedeemerClaimSpendDatumSchema,
  );
export const MissingRedeemerClaimSpendRedeemerSchema =
  faultProofStepRedeemerSchema(DirectScanActionSchema);
export type MissingRedeemerClaimSpendRedeemer = Data.Static<
  typeof MissingRedeemerClaimSpendRedeemerSchema
>;
export const MissingRedeemerClaimSpendRedeemer =
  asDataType<MissingRedeemerClaimSpendRedeemer>(
    MissingRedeemerClaimSpendRedeemerSchema,
  );

export const MissingRedeemerClaimMintDatumSchema = faultProofStepDatumSchema(
  MissingRedeemerClaimMintStateSchema,
);
export type MissingRedeemerClaimMintDatum = Data.Static<
  typeof MissingRedeemerClaimMintDatumSchema
>;
export const MissingRedeemerClaimMintDatum =
  asDataType<MissingRedeemerClaimMintDatum>(
    MissingRedeemerClaimMintDatumSchema,
  );
export const MissingRedeemerClaimMintGrammarRedeemerSchema =
  faultProofStepRedeemerSchema(DirectProgressArgsSchema);
export type MissingRedeemerClaimMintGrammarRedeemer = Data.Static<
  typeof MissingRedeemerClaimMintGrammarRedeemerSchema
>;
export const MissingRedeemerClaimMintGrammarRedeemer =
  asDataType<MissingRedeemerClaimMintGrammarRedeemer>(
    MissingRedeemerClaimMintGrammarRedeemerSchema,
  );
export const MissingRedeemerClaimMintScanRedeemerSchema =
  faultProofStepRedeemerSchema(MissingRedeemerClaimMintActionSchema);
export type MissingRedeemerClaimMintScanRedeemer = Data.Static<
  typeof MissingRedeemerClaimMintScanRedeemerSchema
>;
export const MissingRedeemerClaimMintScanRedeemer =
  asDataType<MissingRedeemerClaimMintScanRedeemer>(
    MissingRedeemerClaimMintScanRedeemerSchema,
  );

export const MissingRedeemerClaimReceiveOutputDatumSchema =
  faultProofStepDatumSchema(MissingRedeemerClaimReceiveOutputStateSchema);
export type MissingRedeemerClaimReceiveOutputDatum = Data.Static<
  typeof MissingRedeemerClaimReceiveOutputDatumSchema
>;
export const MissingRedeemerClaimReceiveOutputDatum =
  asDataType<MissingRedeemerClaimReceiveOutputDatum>(
    MissingRedeemerClaimReceiveOutputDatumSchema,
  );
export const MissingRedeemerClaimReceiveGrammarRedeemerSchema =
  faultProofStepRedeemerSchema(DirectProgressArgsSchema);
export type MissingRedeemerClaimReceiveGrammarRedeemer = Data.Static<
  typeof MissingRedeemerClaimReceiveGrammarRedeemerSchema
>;
export const MissingRedeemerClaimReceiveGrammarRedeemer =
  asDataType<MissingRedeemerClaimReceiveGrammarRedeemer>(
    MissingRedeemerClaimReceiveGrammarRedeemerSchema,
  );
export const MissingRedeemerClaimReceiveOutputRedeemerSchema =
  faultProofStepRedeemerSchema(MissingRedeemerClaimReceiveOutputActionSchema);
export type MissingRedeemerClaimReceiveOutputRedeemer = Data.Static<
  typeof MissingRedeemerClaimReceiveOutputRedeemerSchema
>;
export const MissingRedeemerClaimReceiveOutputRedeemer =
  asDataType<MissingRedeemerClaimReceiveOutputRedeemer>(
    MissingRedeemerClaimReceiveOutputRedeemerSchema,
  );
export const MissingRedeemerClaimReceiveFinishRedeemerSchema =
  faultProofStepRedeemerSchema(MissingRedeemerClaimFinishOpeningArgsSchema);
export type MissingRedeemerClaimReceiveFinishRedeemer = Data.Static<
  typeof MissingRedeemerClaimReceiveFinishRedeemerSchema
>;
export const MissingRedeemerClaimReceiveFinishRedeemer =
  asDataType<MissingRedeemerClaimReceiveFinishRedeemer>(
    MissingRedeemerClaimReceiveFinishRedeemerSchema,
  );
export const MissingRedeemerClaimReceiveDeclarationDatumSchema =
  faultProofStepDatumSchema(MissingRedeemerClaimReceiveDeclarationStateSchema);
export type MissingRedeemerClaimReceiveDeclarationDatum = Data.Static<
  typeof MissingRedeemerClaimReceiveDeclarationDatumSchema
>;
export const MissingRedeemerClaimReceiveDeclarationDatum =
  asDataType<MissingRedeemerClaimReceiveDeclarationDatum>(
    MissingRedeemerClaimReceiveDeclarationDatumSchema,
  );
export const MissingRedeemerClaimReceiveDeclarationRedeemerSchema =
  faultProofStepRedeemerSchema(DirectReceiveDeclarationActionSchema);
export type MissingRedeemerClaimReceiveDeclarationRedeemer = Data.Static<
  typeof MissingRedeemerClaimReceiveDeclarationRedeemerSchema
>;
export const MissingRedeemerClaimReceiveDeclarationRedeemer =
  asDataType<MissingRedeemerClaimReceiveDeclarationRedeemer>(
    MissingRedeemerClaimReceiveDeclarationRedeemerSchema,
  );

export const MissingRedeemerAdjudicateDatumSchema = faultProofStepDatumSchema(
  MissingRedeemerClaimPurposeStateSchema,
);
export type MissingRedeemerAdjudicateDatum = Data.Static<
  typeof MissingRedeemerAdjudicateDatumSchema
>;
export const MissingRedeemerAdjudicateDatum =
  asDataType<MissingRedeemerAdjudicateDatum>(
    MissingRedeemerAdjudicateDatumSchema,
  );
export const MissingRedeemerAdjudicateRedeemerSchema =
  faultProofStepRedeemerSchema(MissingRedeemerAdjudicateActionSchema);
export type MissingRedeemerAdjudicateRedeemer = Data.Static<
  typeof MissingRedeemerAdjudicateRedeemerSchema
>;
export const MissingRedeemerAdjudicateRedeemer =
  asDataType<MissingRedeemerAdjudicateRedeemer>(
    MissingRedeemerAdjudicateRedeemerSchema,
  );

export const MissingRedeemerClaimedScanDatumSchema = faultProofStepDatumSchema(
  MissingRedeemerClaimedRedeemerScanSchema,
);
export type MissingRedeemerClaimedScanDatum = Data.Static<
  typeof MissingRedeemerClaimedScanDatumSchema
>;
export const MissingRedeemerClaimedScanDatum =
  asDataType<MissingRedeemerClaimedScanDatum>(
    MissingRedeemerClaimedScanDatumSchema,
  );
export const MissingRedeemerClaimedScanRedeemerSchema =
  faultProofStepRedeemerSchema(MissingRedeemerClaimedScanActionSchema);
export type MissingRedeemerClaimedScanRedeemer = Data.Static<
  typeof MissingRedeemerClaimedScanRedeemerSchema
>;
export const MissingRedeemerClaimedScanRedeemer =
  asDataType<MissingRedeemerClaimedScanRedeemer>(
    MissingRedeemerClaimedScanRedeemerSchema,
  );

export const MissingRedeemerFinalDatumSchema = faultProofStepDatumSchema(
  MissingRedeemerFinalStateSchema,
);
export type MissingRedeemerFinalDatum = Data.Static<
  typeof MissingRedeemerFinalDatumSchema
>;
export const MissingRedeemerFinalDatum = asDataType<MissingRedeemerFinalDatum>(
  MissingRedeemerFinalDatumSchema,
);
