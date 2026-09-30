import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { ProofSchema } from "../common.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  MintAuthorizationStep01DatumSchema,
  MintAuthorizationStep02ThreadDatumSchema,
  MintAuthorizationStep03DatumSchema,
  MintAuthorizationStep04StateSchema,
  MintAuthorizationStep05StateSchema,
} from "./mint-authorization.mint-authorization-thread-token-asset-name.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";

/**
 * Twin of `step_03.Args`: `WitnessAbsence` (direction A's inline half,
 * routing into step-04's scan) is constructor 0, `EvaluateUnsatisfied`
 * (direction B, closing straight to step-05) is constructor 1.
 */
export const MintAuthorizationStep03ArgsSchema = Data.Enum([
  Data.Object({
    WitnessAbsence: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      script_tx_wits_opening: FieldOpeningSchema,
    }),
  }),
  Data.Object({
    EvaluateUnsatisfied: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      /** The policy's native payload, pinned by hash to the policy id. */
      script_bytes: Data.Bytes(),
      addr_tx_wits_opening: FieldOpeningSchema,
    }),
  }),
  Data.Object({
    StartUnsatisfied: Data.Object({
      script_length: Data.Integer(),
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      chunk_reference_indices: Data.Array(Data.Integer()),
      addr_tx_wits_opening: FieldOpeningSchema,
    }),
  }),
  Data.Object({
    StartAbsence: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      chunk_reference_indices: Data.Array(Data.Integer()),
      script_tx_wits_opening: FieldOpeningSchema,
    }),
  }),
]);

export type MintAuthorizationStep03Args = Data.Static<
  typeof MintAuthorizationStep03ArgsSchema
>;

export const MintAuthorizationStep03Args =
  asDataType<MintAuthorizationStep03Args>(MintAuthorizationStep03ArgsSchema);

export const MintAuthorizationStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MintAuthorizationStep03ArgsSchema);

export type MintAuthorizationStep03SpendRedeemer = Data.Static<
  typeof MintAuthorizationStep03SpendRedeemerSchema
>;

export const MintAuthorizationStep03SpendRedeemer =
  asDataType<MintAuthorizationStep03SpendRedeemer>(
    MintAuthorizationStep03SpendRedeemerSchema,
  );

// ## Step 04 — direction-A reference-input scan (self-loop)

export const MintAuthorizationStep04DatumSchema = faultProofStepDatumSchema(
  MintAuthorizationStep04StateSchema,
);

export type MintAuthorizationStep04Datum = Data.Static<
  typeof MintAuthorizationStep04DatumSchema
>;

export const MintAuthorizationStep04Datum =
  asDataType<MintAuthorizationStep04Datum>(MintAuthorizationStep04DatumSchema);

/**
 * Twin of `step_04.Args`: `ResolveNext` (self-loop over the cursor) is
 * constructor 0, `AdvanceComplete` (cursor equals the authenticated field-1
 * item count) is constructor 1.
 */
export const MintAuthorizationStep04ArgsSchema = Data.Enum([
  Data.Object({
    ResolveNext: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      reference_inputs_opening: FieldOpeningSchema,
      descriptor_cbor: Data.Bytes(),
      ledger_membership_proof: ProofSchema,
    }),
  }),
  Data.Object({
    AdvanceComplete: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      reference_inputs_opening: FieldOpeningSchema,
    }),
  }),
]);

export type MintAuthorizationStep04Args = Data.Static<
  typeof MintAuthorizationStep04ArgsSchema
>;

export const MintAuthorizationStep04Args =
  asDataType<MintAuthorizationStep04Args>(MintAuthorizationStep04ArgsSchema);

export const MintAuthorizationStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MintAuthorizationStep04ArgsSchema);

export type MintAuthorizationStep04SpendRedeemer = Data.Static<
  typeof MintAuthorizationStep04SpendRedeemerSchema
>;

export const MintAuthorizationStep04SpendRedeemer =
  asDataType<MintAuthorizationStep04SpendRedeemer>(
    MintAuthorizationStep04SpendRedeemerSchema,
  );

// ## Step 05 — finalize

export const MintAuthorizationStep05DatumSchema = faultProofStepDatumSchema(
  MintAuthorizationStep05StateSchema,
);

export type MintAuthorizationStep05Datum = Data.Static<
  typeof MintAuthorizationStep05DatumSchema
>;

export const MintAuthorizationStep05Datum =
  asDataType<MintAuthorizationStep05Datum>(MintAuthorizationStep05DatumSchema);

/** Twin of `step_05.Args`. */
export const MintAuthorizationStep05ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});

export type MintAuthorizationStep05Args = Data.Static<
  typeof MintAuthorizationStep05ArgsSchema
>;

export const MintAuthorizationStep05Args =
  asDataType<MintAuthorizationStep05Args>(MintAuthorizationStep05ArgsSchema);

export const MintAuthorizationStep05SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MintAuthorizationStep05ArgsSchema);

export type MintAuthorizationStep05SpendRedeemer = Data.Static<
  typeof MintAuthorizationStep05SpendRedeemerSchema
>;

export const MintAuthorizationStep05SpendRedeemer =
  asDataType<MintAuthorizationStep05SpendRedeemer>(
    MintAuthorizationStep05SpendRedeemerSchema,
  );

export const MintAuthorizationEvaluateStateSchema = Data.Object({
  policy_id: Data.Bytes(),
  script_length: Data.Integer(),
  raw_length: Data.Integer(),
  signer_start: Data.Integer(),
  signer_count: Data.Integer(),
  signer_index: Data.Integer(),
  preimage_chunk_hashes: Data.Array(Data.Bytes()),
  signer_hashes: Data.Array(Data.Bytes()),
  validity_interval_start: Data.Integer(),
  validity_interval_end: Data.Integer(),
  cursor: Data.Integer(),
  node_count: Data.Integer(),
  stack_root: Data.Bytes(),
  stack_depth: Data.Integer(),
  result: Data.Integer(),
});

export type MintAuthorizationEvaluateState = Data.Static<
  typeof MintAuthorizationEvaluateStateSchema
>;

export const MintAuthorizationEvaluateDatumSchema = faultProofStepDatumSchema(
  MintAuthorizationEvaluateStateSchema,
);

export type MintAuthorizationEvaluateDatum = Data.Static<
  typeof MintAuthorizationEvaluateDatumSchema
>;

export const MintAuthorizationEvaluateDatum =
  asDataType<MintAuthorizationEvaluateDatum>(
    MintAuthorizationEvaluateDatumSchema,
  );

export const MintAuthorizationFrameSchema = Data.Object({
  tail: Data.Bytes(),
  kind: Data.Integer(),
  child_count: Data.Integer(),
  remaining: Data.Integer(),
  valid_count: Data.Integer(),
  required: Data.Integer(),
});

export type MintAuthorizationFrame = Data.Static<
  typeof MintAuthorizationFrameSchema
>;

export const MintAuthorizationEvaluateOperationSchema = Data.Enum([
  Data.Literal("Token"),
  Data.Object({ Frame: Data.Object({ frame: MintAuthorizationFrameSchema }) }),
  Data.Literal("Signer"),
]);

export type MintAuthorizationEvaluateOperation = Data.Static<
  typeof MintAuthorizationEvaluateOperationSchema
>;

export const MintAuthorizationEvaluateArgsSchema = Data.Enum([
  Data.Object({
    Advance: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      chunk_reference_indices: Data.Array(Data.Integer()),
      operations: Data.Array(MintAuthorizationEvaluateOperationSchema),
    }),
  }),
  Data.Object({
    Finalize: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
    }),
  }),
]);

export const MintAuthorizationEvaluateSpendRedeemerSchema =
  faultProofStepRedeemerSchema(MintAuthorizationEvaluateArgsSchema);

export type MintAuthorizationEvaluateSpendRedeemer = Data.Static<
  typeof MintAuthorizationEvaluateSpendRedeemerSchema
>;

export const MintAuthorizationEvaluateSpendRedeemer =
  asDataType<MintAuthorizationEvaluateSpendRedeemer>(
    MintAuthorizationEvaluateSpendRedeemerSchema,
  );

export const MintAuthorizationWitnessScanStateSchema = Data.Object({
  policy_id: Data.Bytes(),
  bad_tx_id: Data.Bytes(),
  prior_ledger_root: Data.Bytes(),
  field_length: Data.Integer(),
  field_chunk_hashes: Data.Array(Data.Bytes()),
  cursor: Data.Integer(),
  item_index: Data.Integer(),
  item_count: Data.Integer(),
});

export type MintAuthorizationWitnessScanState = Data.Static<
  typeof MintAuthorizationWitnessScanStateSchema
>;

export const MintAuthorizationWitnessScanDatumSchema =
  faultProofStepDatumSchema(MintAuthorizationWitnessScanStateSchema);

export type MintAuthorizationWitnessScanDatum = Data.Static<
  typeof MintAuthorizationWitnessScanDatumSchema
>;

export const MintAuthorizationWitnessScanDatum =
  asDataType<MintAuthorizationWitnessScanDatum>(
    MintAuthorizationWitnessScanDatumSchema,
  );

export const MintAuthorizationWitnessScanArgsSchema = Data.Enum([
  Data.Object({
    Advance: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      chunk_reference_indices: Data.Array(Data.Integer()),
    }),
  }),
  Data.Object({
    Finalize: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
    }),
  }),
]);

export const MintAuthorizationWitnessScanSpendRedeemerSchema =
  faultProofStepRedeemerSchema(MintAuthorizationWitnessScanArgsSchema);

export type MintAuthorizationWitnessScanSpendRedeemer = Data.Static<
  typeof MintAuthorizationWitnessScanSpendRedeemerSchema
>;

export const MintAuthorizationWitnessScanSpendRedeemer =
  asDataType<MintAuthorizationWitnessScanSpendRedeemer>(
    MintAuthorizationWitnessScanSpendRedeemerSchema,
  );

// ## Step resolver

export const MINT_AUTHORIZATION_STEP_NAMES = [
  "step_01",
  "step_02",
  "step_03",
  "step_04",
  "step_05",
  "step_06",
  "step_07",
] as const;

export type MintAuthorizationStepName =
  (typeof MINT_AUTHORIZATION_STEP_NAMES)[number];

/**
 * Explicit, exhaustive step-datum resolver. There is no fallback branch:
 * adding a step without adding its schema fails to compile.
 */
export const mintAuthorizationStepDatumSchema = (
  step: MintAuthorizationStepName,
) => {
  switch (step) {
    case "step_01":
      return MintAuthorizationStep01DatumSchema;
    case "step_02":
      return MintAuthorizationStep02ThreadDatumSchema;
    case "step_03":
      return MintAuthorizationStep03DatumSchema;
    case "step_04":
      return MintAuthorizationStep04DatumSchema;
    case "step_05":
      return MintAuthorizationStep05DatumSchema;
    case "step_07":
      return MintAuthorizationWitnessScanDatum;
    case "step_06":
      return MintAuthorizationEvaluateDatumSchema;
  }
};
