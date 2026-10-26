/** Claim custody role wire shared by the direct purpose families: claim
 * start, the spend, mint and receive claim domains, adjudication and the
 * claimed field-8 scan (`direct-script-derivation/claim-start.ak`,
 * `claim-spend.ak`, `claim-mint.ak`, `claim-receive.ak`,
 * `claim-adjudicate.ak`). Constructor and field order is wire format. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { ProofSchema } from "../common.js";
import {
  DirectClaimContextSchema,
  DirectClaimedRedeemerScanSchema,
  DirectClaimPurposeStateSchema,
} from "./direct-claim.js";
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
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  MembershipCarriageSchema,
} from "./native.js";

const indices = { input_index: Data.Integer(), output_index: Data.Integer() };
const opened = { ...indices, opening: FieldOpeningSchema };

/** 0 Complete, 1 Spend, 2 SpendOutOfRange, 3 Observe, 4 Mint, 5 Receive. */
export const DirectClaimStartActionSchema = Data.Enum([
  Data.Object({ Complete: Data.Object(indices) }),
  Data.Object({ Spend: Data.Object(indices) }),
  Data.Object({ SpendOutOfRange: Data.Object(opened) }),
  Data.Object({ Observe: Data.Object(opened) }),
  Data.Object({ Mint: Data.Object(opened) }),
  Data.Object({ Receive: Data.Object(opened) }),
]);
export type DirectClaimStartAction = Data.Static<
  typeof DirectClaimStartActionSchema
>;
export const DirectClaimStartAction = asDataType<DirectClaimStartAction>(
  DirectClaimStartActionSchema,
);

export const DirectClaimFinishOpeningArgsSchema = Data.Object({
  ...opened,
  checkpoint_bytes: Data.Bytes(),
});

/** The spend start's only action: resolve the raw item and rank a first
 * page of at most the scan batch. */
export const DirectClaimSpendOpenActionSchema = Data.Object({
  ...indices,
  raw_item_index: Data.Integer(),
  opening: FieldOpeningSchema,
  descriptor_cbor: Data.Bytes(),
  membership: MembershipCarriageSchema,
  budget: Data.Integer(),
});

export const DirectClaimSpendStateSchema = Data.Object({
  context: DirectClaimContextSchema,
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
export type DirectClaimSpendState = Data.Static<
  typeof DirectClaimSpendStateSchema
>;
export const DirectClaimSpendState = asDataType<DirectClaimSpendState>(
  DirectClaimSpendStateSchema,
);

export const DirectClaimMintStateSchema = Data.Object({
  context: DirectClaimContextSchema,
  work: DirectMintWorkStateSchema,
});
export type DirectClaimMintState = Data.Static<
  typeof DirectClaimMintStateSchema
>;
export const DirectClaimMintState = asDataType<DirectClaimMintState>(
  DirectClaimMintStateSchema,
);

/** 0 Scan, 1 Finish. */
export const DirectClaimMintActionSchema = Data.Enum([
  Data.Object({ Scan: Data.Object({ args: DirectProgressArgsSchema }) }),
  Data.Object({
    Finish: Data.Object({ args: DirectClaimFinishOpeningArgsSchema }),
  }),
]);

export const DirectClaimReceiveOutputStateSchema = Data.Object({
  context: DirectClaimContextSchema,
  target_ordinal: Data.Integer(),
  scan: DirectReceiveOutputStateSchema,
});
export type DirectClaimReceiveOutputState = Data.Static<
  typeof DirectClaimReceiveOutputStateSchema
>;
export const DirectClaimReceiveOutputState =
  asDataType<DirectClaimReceiveOutputState>(
    DirectClaimReceiveOutputStateSchema,
  );

export const DirectClaimReceiveDeclarationStateSchema = Data.Object({
  context: DirectClaimContextSchema,
  scan: DirectReceiveDeclarationStateSchema,
});
export type DirectClaimReceiveDeclarationState = Data.Static<
  typeof DirectClaimReceiveDeclarationStateSchema
>;
export const DirectClaimReceiveDeclarationState =
  asDataType<DirectClaimReceiveDeclarationState>(
    DirectClaimReceiveDeclarationStateSchema,
  );

/** 0 Scan, 1 Finish. */
export const DirectClaimReceiveOutputActionSchema = Data.Enum([
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
export const DirectClaimAdjudicateActionSchema = Data.Enum([
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
export type DirectClaimAdjudicateAction = Data.Static<
  typeof DirectClaimAdjudicateActionSchema
>;
export const DirectClaimAdjudicateAction =
  asDataType<DirectClaimAdjudicateAction>(DirectClaimAdjudicateActionSchema);

/** 0 Scan, 1 Finish. */
export const DirectClaimedScanActionSchema = DirectScanActionSchema;

export const DirectClaimStartDatumSchema = faultProofStepDatumSchema(
  DirectClaimContextSchema,
);
export type DirectClaimStartDatum = Data.Static<
  typeof DirectClaimStartDatumSchema
>;
export const DirectClaimStartDatum = asDataType<DirectClaimStartDatum>(
  DirectClaimStartDatumSchema,
);
export const DirectClaimStartRedeemerSchema = faultProofStepRedeemerSchema(
  DirectClaimStartActionSchema,
);
export type DirectClaimStartRedeemer = Data.Static<
  typeof DirectClaimStartRedeemerSchema
>;
export const DirectClaimStartRedeemer = asDataType<DirectClaimStartRedeemer>(
  DirectClaimStartRedeemerSchema,
);

export const DirectClaimSpendOpenRedeemerSchema = faultProofStepRedeemerSchema(
  DirectClaimSpendOpenActionSchema,
);
export type DirectClaimSpendOpenRedeemer = Data.Static<
  typeof DirectClaimSpendOpenRedeemerSchema
>;
export const DirectClaimSpendOpenRedeemer =
  asDataType<DirectClaimSpendOpenRedeemer>(DirectClaimSpendOpenRedeemerSchema);

export const DirectClaimSpendDatumSchema = faultProofStepDatumSchema(
  DirectClaimSpendStateSchema,
);
export type DirectClaimSpendDatum = Data.Static<
  typeof DirectClaimSpendDatumSchema
>;
export const DirectClaimSpendDatum = asDataType<DirectClaimSpendDatum>(
  DirectClaimSpendDatumSchema,
);
export const DirectClaimSpendRedeemerSchema = faultProofStepRedeemerSchema(
  DirectScanActionSchema,
);
export type DirectClaimSpendRedeemer = Data.Static<
  typeof DirectClaimSpendRedeemerSchema
>;
export const DirectClaimSpendRedeemer = asDataType<DirectClaimSpendRedeemer>(
  DirectClaimSpendRedeemerSchema,
);

export const DirectClaimMintDatumSchema = faultProofStepDatumSchema(
  DirectClaimMintStateSchema,
);
export type DirectClaimMintDatum = Data.Static<
  typeof DirectClaimMintDatumSchema
>;
export const DirectClaimMintDatum = asDataType<DirectClaimMintDatum>(
  DirectClaimMintDatumSchema,
);
export const DirectClaimMintGrammarRedeemerSchema =
  faultProofStepRedeemerSchema(DirectProgressArgsSchema);
export type DirectClaimMintGrammarRedeemer = Data.Static<
  typeof DirectClaimMintGrammarRedeemerSchema
>;
export const DirectClaimMintGrammarRedeemer =
  asDataType<DirectClaimMintGrammarRedeemer>(
    DirectClaimMintGrammarRedeemerSchema,
  );
export const DirectClaimMintScanRedeemerSchema = faultProofStepRedeemerSchema(
  DirectClaimMintActionSchema,
);
export type DirectClaimMintScanRedeemer = Data.Static<
  typeof DirectClaimMintScanRedeemerSchema
>;
export const DirectClaimMintScanRedeemer =
  asDataType<DirectClaimMintScanRedeemer>(DirectClaimMintScanRedeemerSchema);

export const DirectClaimReceiveOutputDatumSchema = faultProofStepDatumSchema(
  DirectClaimReceiveOutputStateSchema,
);
export type DirectClaimReceiveOutputDatum = Data.Static<
  typeof DirectClaimReceiveOutputDatumSchema
>;
export const DirectClaimReceiveOutputDatum =
  asDataType<DirectClaimReceiveOutputDatum>(
    DirectClaimReceiveOutputDatumSchema,
  );
export const DirectClaimReceiveGrammarRedeemerSchema =
  faultProofStepRedeemerSchema(DirectProgressArgsSchema);
export type DirectClaimReceiveGrammarRedeemer = Data.Static<
  typeof DirectClaimReceiveGrammarRedeemerSchema
>;
export const DirectClaimReceiveGrammarRedeemer =
  asDataType<DirectClaimReceiveGrammarRedeemer>(
    DirectClaimReceiveGrammarRedeemerSchema,
  );
export const DirectClaimReceiveOutputRedeemerSchema =
  faultProofStepRedeemerSchema(DirectClaimReceiveOutputActionSchema);
export type DirectClaimReceiveOutputRedeemer = Data.Static<
  typeof DirectClaimReceiveOutputRedeemerSchema
>;
export const DirectClaimReceiveOutputRedeemer =
  asDataType<DirectClaimReceiveOutputRedeemer>(
    DirectClaimReceiveOutputRedeemerSchema,
  );
export const DirectClaimReceiveFinishRedeemerSchema =
  faultProofStepRedeemerSchema(DirectClaimFinishOpeningArgsSchema);
export type DirectClaimReceiveFinishRedeemer = Data.Static<
  typeof DirectClaimReceiveFinishRedeemerSchema
>;
export const DirectClaimReceiveFinishRedeemer =
  asDataType<DirectClaimReceiveFinishRedeemer>(
    DirectClaimReceiveFinishRedeemerSchema,
  );
export const DirectClaimReceiveDeclarationDatumSchema =
  faultProofStepDatumSchema(DirectClaimReceiveDeclarationStateSchema);
export type DirectClaimReceiveDeclarationDatum = Data.Static<
  typeof DirectClaimReceiveDeclarationDatumSchema
>;
export const DirectClaimReceiveDeclarationDatum =
  asDataType<DirectClaimReceiveDeclarationDatum>(
    DirectClaimReceiveDeclarationDatumSchema,
  );
export const DirectClaimReceiveDeclarationRedeemerSchema =
  faultProofStepRedeemerSchema(DirectReceiveDeclarationActionSchema);
export type DirectClaimReceiveDeclarationRedeemer = Data.Static<
  typeof DirectClaimReceiveDeclarationRedeemerSchema
>;
export const DirectClaimReceiveDeclarationRedeemer =
  asDataType<DirectClaimReceiveDeclarationRedeemer>(
    DirectClaimReceiveDeclarationRedeemerSchema,
  );

export const DirectClaimAdjudicateDatumSchema = faultProofStepDatumSchema(
  DirectClaimPurposeStateSchema,
);
export type DirectClaimAdjudicateDatum = Data.Static<
  typeof DirectClaimAdjudicateDatumSchema
>;
export const DirectClaimAdjudicateDatum =
  asDataType<DirectClaimAdjudicateDatum>(DirectClaimAdjudicateDatumSchema);
export const DirectClaimAdjudicateRedeemerSchema = faultProofStepRedeemerSchema(
  DirectClaimAdjudicateActionSchema,
);
export type DirectClaimAdjudicateRedeemer = Data.Static<
  typeof DirectClaimAdjudicateRedeemerSchema
>;
export const DirectClaimAdjudicateRedeemer =
  asDataType<DirectClaimAdjudicateRedeemer>(
    DirectClaimAdjudicateRedeemerSchema,
  );

export const DirectClaimedScanDatumSchema = faultProofStepDatumSchema(
  DirectClaimedRedeemerScanSchema,
);
export type DirectClaimedScanDatum = Data.Static<
  typeof DirectClaimedScanDatumSchema
>;
export const DirectClaimedScanDatum = asDataType<DirectClaimedScanDatum>(
  DirectClaimedScanDatumSchema,
);
export const DirectClaimedScanRedeemerSchema = faultProofStepRedeemerSchema(
  DirectClaimedScanActionSchema,
);
export type DirectClaimedScanRedeemer = Data.Static<
  typeof DirectClaimedScanRedeemerSchema
>;
export const DirectClaimedScanRedeemer = asDataType<DirectClaimedScanRedeemer>(
  DirectClaimedScanRedeemerSchema,
);
