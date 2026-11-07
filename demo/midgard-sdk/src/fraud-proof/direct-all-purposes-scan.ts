/** Wire twins and fact fold of the all-purposes walk
 * (`direct-script-derivation/all-purposes-scan.ak`,
 * `all-purposes-workers.ak`). Constructor and field order is wire format. */
import {
  computeScriptIntegrityHashForLanguages,
  type ScriptLanguageName,
} from "@al-ft/midgard-core";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { ProofSchema } from "../common.js";
import {
  DirectFieldCursorSchema,
  DirectScriptSourceSchema,
} from "./direct-script-derivation.js";
import { DirectSourceIndexSchema } from "./direct-source-index-state.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";

/** Source language tags, as `script_proof_v1.language_tag` names them. */
export const ALL_PURPOSES_NATIVE_LANGUAGE_TAG = 0n;
export const ALL_PURPOSES_PLUTUS_V3_LANGUAGE_TAG = 3n;
export const ALL_PURPOSES_MIDGARD_V1_LANGUAGE_TAG = 128n;

/** Bitmap bits, as `script_language_views_v1.expected_script_integrity_hash`
 * reads them. */
export const ALL_PURPOSES_PLUTUS_V3_LANGUAGE_BIT = 1n;
export const ALL_PURPOSES_MIDGARD_V1_LANGUAGE_BIT = 2n;

export const DirectAllPurposesFactsSchema = Data.Object({
  language_bitmap: Data.Integer(),
  probe: Data.Nullable(Data.Bytes()),
  probe_reached: Data.Boolean(),
});
export type DirectAllPurposesFacts = Data.Static<
  typeof DirectAllPurposesFactsSchema
>;
export const DirectAllPurposesFacts = asDataType<DirectAllPurposesFacts>(
  DirectAllPurposesFactsSchema,
);

export const DirectAllPurposesStateSchema = Data.Object({
  cursor: DirectFieldCursorSchema,
  source_index: DirectSourceIndexSchema,
  facts: DirectAllPurposesFactsSchema,
  complete: Data.Boolean(),
});
export type DirectAllPurposesState = Data.Static<
  typeof DirectAllPurposesStateSchema
>;
export const DirectAllPurposesState = asDataType<DirectAllPurposesState>(
  DirectAllPurposesStateSchema,
);

export const DirectAllPurposesExecutionWitnessSchema = Data.Object({
  source: DirectScriptSourceSchema,
  proof: ProofSchema,
});
export type DirectAllPurposesExecutionWitness = Data.Static<
  typeof DirectAllPurposesExecutionWitnessSchema
>;
export const DirectAllPurposesExecutionWitness =
  asDataType<DirectAllPurposesExecutionWitness>(
    DirectAllPurposesExecutionWitnessSchema,
  );

export const DirectAllPurposesSpendWitnessSchema = Data.Object({
  descriptor_cbor: Data.Bytes(),
  proof: ProofSchema,
  execution: Data.Nullable(DirectAllPurposesExecutionWitnessSchema),
});
export type DirectAllPurposesSpendWitness = Data.Static<
  typeof DirectAllPurposesSpendWitnessSchema
>;
export const DirectAllPurposesSpendWitness =
  asDataType<DirectAllPurposesSpendWitness>(
    DirectAllPurposesSpendWitnessSchema,
  );

export const DirectAllPurposesPendingSpendsSchema = Data.Object({
  resume_script_hash: Data.Bytes(),
  state: DirectAllPurposesStateSchema,
  pending: Data.Array(Data.Bytes()),
});
export type DirectAllPurposesPendingSpends = Data.Static<
  typeof DirectAllPurposesPendingSpendsSchema
>;
export const DirectAllPurposesPendingSpends =
  asDataType<DirectAllPurposesPendingSpends>(
    DirectAllPurposesPendingSpendsSchema,
  );

const indices = { input_index: Data.Integer(), output_index: Data.Integer() };

export const DirectAllPurposesSpendTakeArgsSchema = Data.Object({
  ...indices,
  opening: FieldOpeningSchema,
  checkpoint_bytes: Data.Bytes(),
  count: Data.Integer(),
});
export const DirectAllPurposesSpendResolveArgsSchema = Data.Object({
  ...indices,
  witnesses: Data.Array(DirectAllPurposesSpendWitnessSchema),
});
export const DirectAllPurposesGrammarActionSchema = Data.Enum([
  Data.Object({
    Open: Data.Object({ ...indices, opening: FieldOpeningSchema }),
  }),
  Data.Object({
    Certify: Data.Object({
      ...indices,
      opening: FieldOpeningSchema,
      checkpoint_bytes: Data.Bytes(),
      budget: Data.Integer(),
    }),
  }),
]);
export const DirectAllPurposesExecutionArgsSchema = Data.Object({
  ...indices,
  opening: FieldOpeningSchema,
  checkpoint_bytes: Data.Bytes(),
  witnesses: Data.Array(DirectAllPurposesExecutionWitnessSchema),
});
export const DirectAllPurposesOpenArgsSchema = Data.Object({
  ...indices,
  opening: FieldOpeningSchema,
});
export const DirectAllPurposesReceiveArgsSchema = Data.Object({
  ...indices,
  opening: FieldOpeningSchema,
  checkpoint_bytes: Data.Bytes(),
  witnesses: Data.Array(Data.Nullable(DirectAllPurposesExecutionWitnessSchema)),
});

export type DirectAllPurposesGrammarAction = Data.Static<
  typeof DirectAllPurposesGrammarActionSchema
>;

export const DirectAllPurposesDatumSchema = faultProofStepDatumSchema(
  DirectAllPurposesStateSchema,
);
export type DirectAllPurposesDatum = Data.Static<
  typeof DirectAllPurposesDatumSchema
>;
export const DirectAllPurposesDatum = asDataType<DirectAllPurposesDatum>(
  DirectAllPurposesDatumSchema,
);

export const DirectAllPurposesPendingDatumSchema = faultProofStepDatumSchema(
  DirectAllPurposesPendingSpendsSchema,
);
export type DirectAllPurposesPendingDatum = Data.Static<
  typeof DirectAllPurposesPendingDatumSchema
>;
export const DirectAllPurposesPendingDatum =
  asDataType<DirectAllPurposesPendingDatum>(
    DirectAllPurposesPendingDatumSchema,
  );

export const DirectAllPurposesSpendTakeRedeemerSchema =
  faultProofStepRedeemerSchema(DirectAllPurposesSpendTakeArgsSchema);
export type DirectAllPurposesSpendTakeRedeemer = Data.Static<
  typeof DirectAllPurposesSpendTakeRedeemerSchema
>;
export const DirectAllPurposesSpendTakeRedeemer =
  asDataType<DirectAllPurposesSpendTakeRedeemer>(
    DirectAllPurposesSpendTakeRedeemerSchema,
  );

export const DirectAllPurposesSpendResolveRedeemerSchema =
  faultProofStepRedeemerSchema(DirectAllPurposesSpendResolveArgsSchema);
export type DirectAllPurposesSpendResolveRedeemer = Data.Static<
  typeof DirectAllPurposesSpendResolveRedeemerSchema
>;
export const DirectAllPurposesSpendResolveRedeemer =
  asDataType<DirectAllPurposesSpendResolveRedeemer>(
    DirectAllPurposesSpendResolveRedeemerSchema,
  );

export const DirectAllPurposesGrammarRedeemerSchema =
  faultProofStepRedeemerSchema(DirectAllPurposesGrammarActionSchema);
export type DirectAllPurposesGrammarRedeemer = Data.Static<
  typeof DirectAllPurposesGrammarRedeemerSchema
>;
export const DirectAllPurposesGrammarRedeemer =
  asDataType<DirectAllPurposesGrammarRedeemer>(
    DirectAllPurposesGrammarRedeemerSchema,
  );

export const DirectAllPurposesExecutionRedeemerSchema =
  faultProofStepRedeemerSchema(DirectAllPurposesExecutionArgsSchema);
export type DirectAllPurposesExecutionRedeemer = Data.Static<
  typeof DirectAllPurposesExecutionRedeemerSchema
>;
export const DirectAllPurposesExecutionRedeemer =
  asDataType<DirectAllPurposesExecutionRedeemer>(
    DirectAllPurposesExecutionRedeemerSchema,
  );

export const DirectAllPurposesOpenRedeemerSchema = faultProofStepRedeemerSchema(
  DirectAllPurposesOpenArgsSchema,
);
export type DirectAllPurposesOpenRedeemer = Data.Static<
  typeof DirectAllPurposesOpenRedeemerSchema
>;
export const DirectAllPurposesOpenRedeemer =
  asDataType<DirectAllPurposesOpenRedeemer>(
    DirectAllPurposesOpenRedeemerSchema,
  );

export const DirectAllPurposesReceiveRedeemerSchema =
  faultProofStepRedeemerSchema(DirectAllPurposesReceiveArgsSchema);
export type DirectAllPurposesReceiveRedeemer = Data.Static<
  typeof DirectAllPurposesReceiveRedeemerSchema
>;
export const DirectAllPurposesReceiveRedeemer =
  asDataType<DirectAllPurposesReceiveRedeemer>(
    DirectAllPurposesReceiveRedeemerSchema,
  );

/** One resolved execution: a purpose's script hash and its selected source's
 * language tag. */
export type DirectAllPurposesExecution = Readonly<{
  scriptHash: string;
  languageTag: bigint;
}>;

/** `admit_execution_v1`: a Plutus language sets its bit, a native execution
 * adds none, and the probe is reached by an equal script hash. */
export const admitAllPurposesExecution = (
  facts: DirectAllPurposesFacts,
  execution: DirectAllPurposesExecution,
): DirectAllPurposesFacts => {
  const bit =
    execution.languageTag === ALL_PURPOSES_PLUTUS_V3_LANGUAGE_TAG
      ? ALL_PURPOSES_PLUTUS_V3_LANGUAGE_BIT
      : execution.languageTag === ALL_PURPOSES_MIDGARD_V1_LANGUAGE_TAG
        ? ALL_PURPOSES_MIDGARD_V1_LANGUAGE_BIT
        : 0n;
  return {
    language_bitmap: facts.language_bitmap | bit,
    probe: facts.probe,
    probe_reached:
      facts.probe_reached ||
      (facts.probe !== null && facts.probe === execution.scriptHash),
  };
};

/** The facts of a whole walk over `executions`, from the opening facts. */
export const allPurposesFacts = (
  executions: readonly DirectAllPurposesExecution[],
  probe: string | null = null,
): DirectAllPurposesFacts =>
  executions.reduce(admitAllPurposesExecution, {
    language_bitmap: 0n,
    probe,
    probe_reached: false,
  });

/** The languages a bitmap names, as `script_language_views_v1` reads them.
 * A bitmap outside the two known bits fails, as it does on chain. */
export const allPurposesScriptLanguages = (
  bitmap: bigint,
): readonly ScriptLanguageName[] => {
  if (bitmap < 0n || bitmap > 3n)
    throw new Error(
      `all-purposes language bitmap ${bitmap.toString()} differs`,
    );
  return [
    ...((bitmap & ALL_PURPOSES_PLUTUS_V3_LANGUAGE_BIT) === 0n
      ? []
      : (["PlutusV3"] as const)),
    ...((bitmap & ALL_PURPOSES_MIDGARD_V1_LANGUAGE_BIT) === 0n
      ? []
      : (["MidgardV1"] as const)),
  ];
};

/** Twin of `script_language_views_v1.expected_script_integrity_hash`. */
export const allPurposesExpectedScriptIntegrityHash = (
  redeemerTxWitsHash: Uint8Array,
  bitmap: bigint,
): string =>
  computeScriptIntegrityHashForLanguages(
    redeemerTxWitsHash,
    allPurposesScriptLanguages(bitmap),
  ).toString("hex");
