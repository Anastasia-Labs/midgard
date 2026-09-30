import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema } from "../common.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  H28Schema,
  MissingNativeScriptTxStep02State,
  MissingNativeScriptTxStep03State,
} from "./missing-native-script-tx.missing-native-script-is-absent.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  type MidgardTxInput as MidgardTxInputData,
} from "./native.js";

// ## Step 05 — classify the credential as a native script

/** Mirrors `midgard/fraud_proofs/missing_native_script_tx/step_05.State`. */
export const MissingNativeScriptTxStep05StateSchema = Data.Object({
  expected_missing_script_hash: H28Schema,
  bad_tx_id: H32Schema,
  bad_tx_witness_set_hash: H32Schema,
});

export type MissingNativeScriptTxStep05State = Data.Static<
  typeof MissingNativeScriptTxStep05StateSchema
>;

export const MissingNativeScriptTxStep05State =
  asDataType<MissingNativeScriptTxStep05State>(
    MissingNativeScriptTxStep05StateSchema,
  );

export const MissingNativeScriptTxStep05DatumSchema = faultProofStepDatumSchema(
  MissingNativeScriptTxStep05StateSchema,
);

export type MissingNativeScriptTxStep05Datum = Data.Static<
  typeof MissingNativeScriptTxStep05DatumSchema
>;

export const MissingNativeScriptTxStep05Datum =
  asDataType<MissingNativeScriptTxStep05Datum>(
    MissingNativeScriptTxStep05DatumSchema,
  );

/** Mirrors `midgard/fraud_proofs/missing_native_script_tx/step_05.Args`. */
export const MissingNativeScriptTxStep05ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  missing_native_script_bytes: Data.Bytes(),
});

export type MissingNativeScriptTxStep05Args = Data.Static<
  typeof MissingNativeScriptTxStep05ArgsSchema
>;

export const MissingNativeScriptTxStep05Args =
  asDataType<MissingNativeScriptTxStep05Args>(
    MissingNativeScriptTxStep05ArgsSchema,
  );

export const MissingNativeScriptTxStep05SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptTxStep05ArgsSchema);

export type MissingNativeScriptTxStep05SpendRedeemer = Data.Static<
  typeof MissingNativeScriptTxStep05SpendRedeemerSchema
>;

export const MissingNativeScriptTxStep05SpendRedeemer =
  asDataType<MissingNativeScriptTxStep05SpendRedeemer>(
    MissingNativeScriptTxStep05SpendRedeemerSchema,
  );

// ## Step 06 — open the script witnesses and convict the absence

/**
 * Mirrors `midgard/fraud_proofs/missing_native_script_tx/step_06.State` —
 * identical to step-05's state; the classification happened in between.
 */
export const MissingNativeScriptTxPhaseSchema = Data.Enum([
  Data.Literal("Ready"),
  Data.Object({
    GrammarCertification: Data.Object({ checkpoint_hash: H32Schema }),
  }),
  Data.Object({
    SemanticScan: Data.Object({
      checkpoint_hash: H32Schema,
      required_script_is_present: Data.Boolean(),
    }),
  }),
]);

export type MissingNativeScriptTxPhase = Data.Static<
  typeof MissingNativeScriptTxPhaseSchema
>;

export const MissingNativeScriptTxPhase =
  asDataType<MissingNativeScriptTxPhase>(MissingNativeScriptTxPhaseSchema);

export const MissingNativeScriptTxStep06StateSchema = Data.Object({
  expected_missing_script_hash: H28Schema,
  bad_tx_id: H32Schema,
  bad_tx_witness_set_hash: H32Schema,
  phase: MissingNativeScriptTxPhaseSchema,
});

export type MissingNativeScriptTxStep06State = Data.Static<
  typeof MissingNativeScriptTxStep06StateSchema
>;

export const MissingNativeScriptTxStep06State =
  asDataType<MissingNativeScriptTxStep06State>(
    MissingNativeScriptTxStep06StateSchema,
  );

export const MissingNativeScriptTxStep06DatumSchema = faultProofStepDatumSchema(
  MissingNativeScriptTxStep06StateSchema,
);

export type MissingNativeScriptTxStep06Datum = Data.Static<
  typeof MissingNativeScriptTxStep06DatumSchema
>;

export const MissingNativeScriptTxStep06Datum =
  asDataType<MissingNativeScriptTxStep06Datum>(
    MissingNativeScriptTxStep06DatumSchema,
  );

/**
 * Mirrors `midgard/fraud_proofs/missing_native_script_tx/step_06.Args`.
 *
 * `script_tx_wits_opening` must be the `WitnessFieldOpening` arm — it carries
 * the transaction's `NativeTxWitnessSetCompact` alongside the compact bytes,
 * and the door re-derives it against the **thread-anchored**
 * `bad_tx_witness_set_hash`. Field 6 is variable-width, so tier-3 Certified
 * carriage aborts at `field_item_count` (§8.3 erratum E2 limit 2); the
 * offchain planner never routes into that tier silently.
 */
export const MissingNativeScriptTxStep06ArgsSchema = Data.Enum([
  Data.Object({
    DirectFinalize: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
      script_tx_wits_opening: FieldOpeningSchema,
    }),
  }),
  Data.Object({
    StartGrammarCertification: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      script_tx_wits_opening: FieldOpeningSchema,
      item_budget: Data.Integer(),
    }),
  }),
]);

export type MissingNativeScriptTxStep06Args = Data.Static<
  typeof MissingNativeScriptTxStep06ArgsSchema
>;

export const MissingNativeScriptTxStep06Args =
  asDataType<MissingNativeScriptTxStep06Args>(
    MissingNativeScriptTxStep06ArgsSchema,
  );

export const MissingNativeScriptTxStep06SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptTxStep06ArgsSchema);

export type MissingNativeScriptTxStep06SpendRedeemer = Data.Static<
  typeof MissingNativeScriptTxStep06SpendRedeemerSchema
>;

export const MissingNativeScriptTxStep06SpendRedeemer =
  asDataType<MissingNativeScriptTxStep06SpendRedeemer>(
    MissingNativeScriptTxStep06SpendRedeemerSchema,
  );

// ## Step 07 — grammar certification and semantic-scan transition

export const MissingNativeScriptTxStep07StateSchema =
  MissingNativeScriptTxStep06StateSchema;

export type MissingNativeScriptTxStep07State = MissingNativeScriptTxStep06State;

export const MissingNativeScriptTxStep07State =
  MissingNativeScriptTxStep06State as unknown as MissingNativeScriptTxStep07State;

export const MissingNativeScriptTxStep07DatumSchema = faultProofStepDatumSchema(
  MissingNativeScriptTxStep07StateSchema,
);

export type MissingNativeScriptTxStep07Datum = Data.Static<
  typeof MissingNativeScriptTxStep07DatumSchema
>;

export const MissingNativeScriptTxStep07Datum =
  asDataType<MissingNativeScriptTxStep07Datum>(
    MissingNativeScriptTxStep07DatumSchema,
  );

export const MissingNativeScriptTxStep07ArgsSchema = Data.Enum([
  Data.Object({
    ResumeGrammarCertification: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      script_tx_wits_opening: FieldOpeningSchema,
      checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  }),
  Data.Object({
    StartSemanticScan: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      script_tx_wits_opening: FieldOpeningSchema,
      grammar_checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  }),
]);

export type MissingNativeScriptTxStep07Args = Data.Static<
  typeof MissingNativeScriptTxStep07ArgsSchema
>;

export const MissingNativeScriptTxStep07Args =
  asDataType<MissingNativeScriptTxStep07Args>(
    MissingNativeScriptTxStep07ArgsSchema,
  );

export const MissingNativeScriptTxStep07SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptTxStep07ArgsSchema);

export type MissingNativeScriptTxStep07SpendRedeemer = Data.Static<
  typeof MissingNativeScriptTxStep07SpendRedeemerSchema
>;

export const MissingNativeScriptTxStep07SpendRedeemer =
  asDataType<MissingNativeScriptTxStep07SpendRedeemer>(
    MissingNativeScriptTxStep07SpendRedeemerSchema,
  );

// ## Step 08 — bounded semantic resume/finalize

export const MissingNativeScriptTxStep08StateSchema =
  MissingNativeScriptTxStep06StateSchema;

export type MissingNativeScriptTxStep08State = MissingNativeScriptTxStep06State;

export const MissingNativeScriptTxStep08State =
  MissingNativeScriptTxStep06State as unknown as MissingNativeScriptTxStep08State;

export const MissingNativeScriptTxStep08DatumSchema = faultProofStepDatumSchema(
  MissingNativeScriptTxStep08StateSchema,
);

export type MissingNativeScriptTxStep08Datum = Data.Static<
  typeof MissingNativeScriptTxStep08DatumSchema
>;

export const MissingNativeScriptTxStep08Datum =
  asDataType<MissingNativeScriptTxStep08Datum>(
    MissingNativeScriptTxStep08DatumSchema,
  );

export const MissingNativeScriptTxStep08ArgsSchema = Data.Enum([
  Data.Object({
    ResumeSemanticScan: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      script_tx_wits_opening: FieldOpeningSchema,
      checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  }),
  Data.Object({
    FinalizeSemanticScan: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
      script_tx_wits_opening: FieldOpeningSchema,
      checkpoint_bytes: Data.Bytes(),
      item_budget: Data.Integer(),
    }),
  }),
]);

export type MissingNativeScriptTxStep08Args = Data.Static<
  typeof MissingNativeScriptTxStep08ArgsSchema
>;

export const MissingNativeScriptTxStep08Args =
  asDataType<MissingNativeScriptTxStep08Args>(
    MissingNativeScriptTxStep08ArgsSchema,
  );

export const MissingNativeScriptTxStep08SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptTxStep08ArgsSchema);

export type MissingNativeScriptTxStep08SpendRedeemer = Data.Static<
  typeof MissingNativeScriptTxStep08SpendRedeemerSchema
>;

export const MissingNativeScriptTxStep08SpendRedeemer =
  asDataType<MissingNativeScriptTxStep08SpendRedeemer>(
    MissingNativeScriptTxStep08SpendRedeemerSchema,
  );

// ## Step-state builders (twins of the on-chain forwarding rules)

/** Exactly the state `step-01` writes for `step-02` (`step-01.ak:63-68`). */
export const missingNativeScriptTxStep02StateFromBadTx = ({
  badTxId,
  badTxWitnessSetHash,
}: {
  readonly badTxId: string;
  readonly badTxWitnessSetHash: string;
}): MissingNativeScriptTxStep02State => ({
  bad_tx_id: badTxId.toLowerCase(),
  bad_tx_witness_set_hash: badTxWitnessSetHash.toLowerCase(),
});

/** Exactly the state `step-02` writes for `step-03` (`step-02.ak:85-92`). */
export const missingNativeScriptTxStep03State = ({
  inputWithMissingScript,
  badTxId,
  badTxWitnessSetHash,
}: {
  readonly inputWithMissingScript: MidgardTxInputData;
  readonly badTxId: string;
  readonly badTxWitnessSetHash: string;
}): MissingNativeScriptTxStep03State => ({
  input_with_missing_script: {
    tx_id: inputWithMissingScript.tx_id.toLowerCase(),
    output_index: inputWithMissingScript.output_index,
  },
  bad_tx_id: badTxId.toLowerCase(),
  bad_tx_witness_set_hash: badTxWitnessSetHash.toLowerCase(),
});
