/** Downstream wire twins for authenticated direct purpose/source derivation. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema, ProofSchema, ScriptHashSchema } from "../common.js";
import {
  NetworkOrderContextSchema,
  PurposeCoordinate,
  PurposeCoordinateSchema,
} from "./direct-script-derivation-first-fault-seam.js";
import { DirectSourceIndexSchema } from "./direct-source-index-state.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";
import { VerdictSubjectSchema } from "./proof-thread-substrate.js";

export const DirectScriptBoundSchema = Data.Object({
  subject: VerdictSubjectSchema,
  compact_cbor: Data.Bytes(),
  network_order_context: NetworkOrderContextSchema,
  event_pre_root: H32Schema,
});
export type DirectScriptBound = Data.Static<typeof DirectScriptBoundSchema>;
export const DirectScriptBound = asDataType<DirectScriptBound>(
  DirectScriptBoundSchema,
);

export const DirectPurposeCoordinateSchema = PurposeCoordinateSchema;
export type DirectPurposeCoordinate = PurposeCoordinate;
export const DirectPurposeCoordinate = PurposeCoordinate;

export const DirectPurposeSchema = Data.Object({
  coordinate: DirectPurposeCoordinateSchema,
  script_hash: ScriptHashSchema,
  subject: Data.Bytes(),
});
export type DirectPurpose = Data.Static<typeof DirectPurposeSchema>;
export const DirectPurpose = asDataType<DirectPurpose>(DirectPurposeSchema);

export const DirectFieldCursorSchema = Data.Object({
  bound: DirectScriptBoundSchema,
  field_index: Data.Integer(),
  phase: Data.Enum([Data.Literal("Certifying"), Data.Literal("Walking")]),
  checkpoint_hash: H32Schema,
});
export type DirectFieldCursor = Data.Static<typeof DirectFieldCursorSchema>;
export const DirectFieldCursor = asDataType<DirectFieldCursor>(
  DirectFieldCursorSchema,
);

export const DirectReceiveOutputWitnessSchema = Data.Enum([
  Data.Literal("NoCredential"),
  Data.Object({ FirstCredential: Data.Object({ proof: ProofSchema }) }),
  Data.Object({
    RepeatedCredential: Data.Object({
      first_output_index: Data.Integer(),
      proof: ProofSchema,
    }),
  }),
]);
export type DirectReceiveOutputWitness = Data.Static<
  typeof DirectReceiveOutputWitnessSchema
>;
export const DirectReceiveOutputWitness =
  asDataType<DirectReceiveOutputWitness>(DirectReceiveOutputWitnessSchema);

export const DirectReceiveOutputStateSchema = Data.Object({
  cursor: DirectFieldCursorSchema,
  source_index: DirectSourceIndexSchema,
  credential_root: H32Schema,
  credential_count: Data.Integer(),
  complete: Data.Boolean(),
});
export type DirectReceiveOutputState = Data.Static<
  typeof DirectReceiveOutputStateSchema
>;
export const DirectReceiveOutputState = asDataType<DirectReceiveOutputState>(
  DirectReceiveOutputStateSchema,
);

export const DirectReceivePurposeSchema = Data.Object({
  purpose: DirectPurposeSchema,
  first_output_index: Data.Integer(),
});
export type DirectReceivePurpose = Data.Static<
  typeof DirectReceivePurposeSchema
>;
export const DirectReceivePurpose = asDataType<DirectReceivePurpose>(
  DirectReceivePurposeSchema,
);

export const DirectReceiveDeclarationStateSchema = Data.Object({
  bound: DirectScriptBoundSchema,
  source_index: DirectSourceIndexSchema,
  output_root: H32Schema,
  expected_count: Data.Integer(),
  declared_root: H32Schema,
  declared_count: Data.Integer(),
  previous_credential: Data.Bytes(),
  target_ordinal: Data.Nullable(Data.Integer()),
  selected: Data.Nullable(DirectReceivePurposeSchema),
});
export type DirectReceiveDeclarationState = Data.Static<
  typeof DirectReceiveDeclarationStateSchema
>;
export const DirectReceiveDeclarationState =
  asDataType<DirectReceiveDeclarationState>(
    DirectReceiveDeclarationStateSchema,
  );

export const DirectDeclaredCredentialSchema = Data.Object({
  script_hash: ScriptHashSchema,
  first_output_index: Data.Integer(),
  proof: ProofSchema,
});
export type DirectDeclaredCredential = Data.Static<
  typeof DirectDeclaredCredentialSchema
>;
export const DirectDeclaredCredential = asDataType<DirectDeclaredCredential>(
  DirectDeclaredCredentialSchema,
);

export const DirectScriptSourceSchema = Data.Object({
  field_index: Data.Integer(),
  item_index: Data.Integer(),
  source_key: Data.Bytes(),
  language_tag: Data.Integer(),
  script_hash: ScriptHashSchema,
  total_length: Data.Integer(),
  item_commitment: H32Schema,
});
export type DirectScriptSource = Data.Static<typeof DirectScriptSourceSchema>;
export const DirectScriptSource = asDataType<DirectScriptSource>(
  DirectScriptSourceSchema,
);

export const DirectScriptSourceStateSchema = Data.Object({
  cursor: DirectFieldCursorSchema,
  purpose: DirectPurposeSchema,
  selected: Data.Nullable(DirectScriptSourceSchema),
  complete: Data.Boolean(),
});
export type DirectScriptSourceState = Data.Static<
  typeof DirectScriptSourceStateSchema
>;
export const DirectScriptSourceState = asDataType<DirectScriptSourceState>(
  DirectScriptSourceStateSchema,
);

export const DirectResolvedSourceWitnessSchema = Data.Object({
  descriptor_cbor: Data.Bytes(),
  proof: ProofSchema,
});
export type DirectResolvedSourceWitness = Data.Static<
  typeof DirectResolvedSourceWitnessSchema
>;
export const DirectResolvedSourceWitness =
  asDataType<DirectResolvedSourceWitness>(DirectResolvedSourceWitnessSchema);

export const DirectRedeemerSchema = Data.Object({
  item_index: Data.Integer(),
  total_length: Data.Integer(),
  item_commitment: H32Schema,
  data_offset: Data.Integer(),
  data_length: Data.Integer(),
  execution_memory: Data.Integer(),
  execution_steps: Data.Integer(),
});
export type DirectRedeemer = Data.Static<typeof DirectRedeemerSchema>;
export const DirectRedeemer = asDataType<DirectRedeemer>(DirectRedeemerSchema);

export const DirectRedeemerStateSchema = Data.Object({
  cursor: DirectFieldCursorSchema,
  coordinate: DirectPurposeCoordinateSchema,
  selected: Data.Nullable(DirectRedeemerSchema),
  complete: Data.Boolean(),
});
export type DirectRedeemerState = Data.Static<typeof DirectRedeemerStateSchema>;
export const DirectRedeemerState = asDataType<DirectRedeemerState>(
  DirectRedeemerStateSchema,
);

export const DirectReceiveOutputActionSchema = Data.Enum([
  Data.Object({
    Scan: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      outputs_opening: FieldOpeningSchema,
      checkpoint_bytes: Data.Bytes(),
      witnesses: Data.Array(DirectReceiveOutputWitnessSchema),
    }),
  }),
  Data.Object({
    Finish: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
    }),
  }),
]);
export type DirectReceiveOutputAction = Data.Static<
  typeof DirectReceiveOutputActionSchema
>;
export const DirectReceiveOutputAction = asDataType<DirectReceiveOutputAction>(
  DirectReceiveOutputActionSchema,
);

export const DirectReceiveOutputDatumSchema = faultProofStepDatumSchema(
  DirectReceiveOutputStateSchema,
);
export type DirectReceiveOutputDatum = Data.Static<
  typeof DirectReceiveOutputDatumSchema
>;
export const DirectReceiveOutputDatum = asDataType<DirectReceiveOutputDatum>(
  DirectReceiveOutputDatumSchema,
);

export const DirectReceiveOutputRedeemerSchema = faultProofStepRedeemerSchema(
  DirectReceiveOutputActionSchema,
);
export type DirectReceiveOutputRedeemer = Data.Static<
  typeof DirectReceiveOutputRedeemerSchema
>;
export const DirectReceiveOutputRedeemer =
  asDataType<DirectReceiveOutputRedeemer>(DirectReceiveOutputRedeemerSchema);

export const DirectReceiveGrammarArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  outputs_opening: FieldOpeningSchema,
  checkpoint_bytes: Data.Bytes(),
  budget: Data.Integer(),
});
export type DirectReceiveGrammarArgs = Data.Static<
  typeof DirectReceiveGrammarArgsSchema
>;
export const DirectReceiveGrammarArgs = asDataType<DirectReceiveGrammarArgs>(
  DirectReceiveGrammarArgsSchema,
);
export const DirectReceiveGrammarRedeemerSchema = faultProofStepRedeemerSchema(
  DirectReceiveGrammarArgsSchema,
);
export type DirectReceiveGrammarRedeemer = Data.Static<
  typeof DirectReceiveGrammarRedeemerSchema
>;
export const DirectReceiveGrammarRedeemer =
  asDataType<DirectReceiveGrammarRedeemer>(DirectReceiveGrammarRedeemerSchema);

export const DirectPurposeSelectionSchema = Data.Object({
  bound: DirectScriptBoundSchema,
  source_index: DirectSourceIndexSchema,
  coordinate: DirectPurposeCoordinateSchema,
  purpose: Data.Nullable(DirectPurposeSchema),
});
export type DirectPurposeSelection = Data.Static<
  typeof DirectPurposeSelectionSchema
>;
export const DirectPurposeSelection = asDataType<DirectPurposeSelection>(
  DirectPurposeSelectionSchema,
);

export const DirectPurposeSelectionDatumSchema = faultProofStepDatumSchema(
  DirectPurposeSelectionSchema,
);
export type DirectPurposeSelectionDatum = Data.Static<
  typeof DirectPurposeSelectionDatumSchema
>;
export const DirectPurposeSelectionDatum =
  asDataType<DirectPurposeSelectionDatum>(DirectPurposeSelectionDatumSchema);

export const DirectMintPurposeStateSchema = Data.Object({
  cursor: DirectFieldCursorSchema,
  target_ordinal: Data.Integer(),
  previous_policy: Data.Bytes(),
  selected: Data.Nullable(DirectPurposeSchema),
  complete: Data.Boolean(),
});
export type DirectMintPurposeState = Data.Static<
  typeof DirectMintPurposeStateSchema
>;
export const DirectMintPurposeState = asDataType<DirectMintPurposeState>(
  DirectMintPurposeStateSchema,
);

export const DirectReceiveDeclarationActionSchema = Data.Enum([
  Data.Object({
    Scan: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      entries: Data.Array(DirectDeclaredCredentialSchema),
    }),
  }),
  Data.Object({
    Finish: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
    }),
  }),
]);
export type DirectReceiveDeclarationAction = Data.Static<
  typeof DirectReceiveDeclarationActionSchema
>;
export const DirectReceiveDeclarationAction =
  asDataType<DirectReceiveDeclarationAction>(
    DirectReceiveDeclarationActionSchema,
  );
export const DirectReceiveDeclarationDatumSchema = faultProofStepDatumSchema(
  DirectReceiveDeclarationStateSchema,
);
export type DirectReceiveDeclarationDatum = Data.Static<
  typeof DirectReceiveDeclarationDatumSchema
>;
export const DirectReceiveDeclarationDatum =
  asDataType<DirectReceiveDeclarationDatum>(
    DirectReceiveDeclarationDatumSchema,
  );
export const DirectReceiveDeclarationRedeemerSchema =
  faultProofStepRedeemerSchema(DirectReceiveDeclarationActionSchema);
export type DirectReceiveDeclarationRedeemer = Data.Static<
  typeof DirectReceiveDeclarationRedeemerSchema
>;
export const DirectReceiveDeclarationRedeemer =
  asDataType<DirectReceiveDeclarationRedeemer>(
    DirectReceiveDeclarationRedeemerSchema,
  );
