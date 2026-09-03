/**
 * `missing-native-script-tx` fault-proof family (Goal task `Q17`).
 *
 * **Rule.** Every committed transaction that spends an output locked by a
 * native-script credential must carry that script in its script-witness
 * collection (witness field 6): for every spend input whose producing output's
 * payment credential is `ScriptCredential(h)` with
 * `h = versioned_script_hash(NativeCardanoScript, bytes)`, some item of
 * `script_tx_wits` must hash to `h`.
 *
 * **Violation.** A block commits a transaction spending such an output while
 * no script-witness item hashes to the credential. The rule is unconditional
 * over the block's transactions.
 *
 * The proof is an eight-script computation thread. Steps 1–5 bind and
 * classify the accused credential; step 6 either finalizes the bounded direct
 * route or starts the authenticated grammar walk; steps 7–8 own the grammar
 * and semantic resumptions for larger fields.
 *
 * 1. bind the bad transaction to the block's counted `transactions_root` and
 *    forward its id together with the block-committed `witness_set_hash`;
 * 2. open the bad transaction's spend-input field (body field 0) and forward
 *    the accused input;
 * 3. bind the *producing* transaction (the accused input's tx id) the same
 *    way;
 * 4. open the producing transaction's outputs field (body field 2), read the
 *    output the accused input names, and require a script payment credential;
 * 5. lift the credential to a native script: the prover supplies the script
 *    bytes and the step equates `versioned_script_hash` (language tag 0) with
 *    the credential; and
 * 6. open the bad transaction's script-witness field (witness field 6)
 *    against the thread-anchored witness-set hash, finalizing directly at or
 *    below 64 items or starting a 32-item grammar-certification batch;
 * 7. resume grammar certification until terminal, then start the semantic
 *    absence scan; and
 * 8. resume the semantic scan in bounded batches and finalize only at the
 *    authenticated terminal with the presence accumulator still false.
 *
 * This module is the strict TypeScript twin of
 * `onchain/aiken/lib/midgard/fraud-proofs/missing-native-script-tx/step-0{1..8}.ak`.
 * Field order in every `Data.Object` mirrors the aiken record declarations
 * 1:1 — the PlutusData encoding is positional, so re-ordering here would
 * silently produce redeemers the validators reject.
 */
import {
  decodeMidgardVersionedScript,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema } from "../common.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  type MidgardTxInput as MidgardTxInputData,
  MidgardTxInputSchema,
  NativeTxInclusionArgs,
  NativeTxInclusionArgsSchema,
} from "./native.js";

/** Catalogue violation identifier adjudicated by this family. */
export const MISSING_NATIVE_SCRIPT_TX_VIOLATION_ID =
  "missing-native-script-tx" as const;

/**
 * Canonical script-witness field index of a native V1 transaction witness
 * set. The §8.8 door refuses a preimage built for any other field.
 */
export const MISSING_NATIVE_SCRIPT_TX_SCRIPT_TX_WITS_FIELD_INDEX = 6;

/** Direct step-06 fold limit; larger fields use the staged 06→07→08 route. */
export const MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT = 64;

/** Authenticated grammar/semantic items processed per staged transaction. */
export const MISSING_NATIVE_SCRIPT_TX_STAGED_BATCH_LIMIT = 32;

const H28Schema = Data.Bytes({ minLength: 28, maxLength: 28 });

// ## Canonical hashing (twin of `script_proof_v1.versioned_script_hash`)

/**
 * The canonical versioned-script hash of a **native Cardano** script:
 * blake2b-224 over the language-tag byte (`0x00`) followed by the script
 * bytes. This is the value step-04 reads out of the producing output's
 * payment credential and step-05 equates with the prover-supplied preimage
 * (`step-05.ak:73-79`).
 *
 * Delegates to core's `hashMidgardVersionedScript`, which also refuses
 * non-canonical native-script bytes — the same set of preimages the on-chain
 * decoder accepts.
 */
export const missingNativeScriptTxVersionedScriptHash = (
  scriptBytes: Uint8Array,
): string => {
  const decoded = decodeMidgardVersionedScript(
    Buffer.concat([
      Buffer.from([0x82, 0x00]),
      encodeDefiniteBytes(Buffer.from(scriptBytes)),
    ]),
  );
  return hashMidgardVersionedScript(decoded);
};

const encodeDefiniteBytes = (bytes: Buffer): Buffer => {
  // Minimal-length definite byte-string header, the §6.1 canonical form.
  const length = bytes.length;
  if (length < 24) {
    return Buffer.concat([Buffer.from([0x40 + length]), bytes]);
  }
  if (length <= 0xff) {
    return Buffer.concat([Buffer.from([0x58, length]), bytes]);
  }
  if (length <= 0xffff) {
    const header = Buffer.alloc(3);
    header[0] = 0x59;
    header.writeUInt16BE(length, 1);
    return Buffer.concat([header, bytes]);
  }
  const header = Buffer.alloc(5);
  header[0] = 0x5a;
  header.writeUInt32BE(length, 1);
  return Buffer.concat([header, bytes]);
};

// ## Rule (offchain twin of the step-06 fold)

/**
 * The adjudicated absence predicate over the authenticated field-6 preimage:
 * `True` exactly when **no** committed script-witness item hashes to the
 * accused credential. Twin of the `fold_opened_field` in
 * `validators/fraud-proofs/missing-native-script-tx/step-06.ak:127-150`,
 * including its §6.1 canonicality posture: a committed item that does not
 * re-encode to itself (trailing junk, non-minimal length prefix) makes the
 * whole claim unadjudicable and this function **throws**, exactly as the
 * on-chain fold aborts.
 */
export const missingNativeScriptIsAbsent = ({
  scriptTxWitsItems,
  expectedMissingScriptHash,
}: {
  /** The raw per-item encodings of the committed field-6 preimage. */
  readonly scriptTxWitsItems: readonly Uint8Array[];
  /** The accused credential hash (28-byte hex). */
  readonly expectedMissingScriptHash: string;
}): boolean => {
  const expected = expectedMissingScriptHash.toLowerCase();
  for (const [index, item] of scriptTxWitsItems.entries()) {
    const decoded = decodeMidgardVersionedScript(item);
    const reEncoded = encodeMidgardVersionedScript(decoded);
    if (!reEncoded.equals(Buffer.from(item))) {
      throw new Error(
        `Script-witness item ${index.toString()} is not §6.1 canonical: it does not re-encode to the committed bytes.`,
      );
    }
    if (hashMidgardVersionedScript(decoded) === expected) {
      return false;
    }
  }
  return true;
};

// ## Shared step aliases

export const MissingNativeScriptTxStepCancelSchema = FaultProofStepCancelSchema;
export type MissingNativeScriptTxStepCancel = FaultProofStepCancel;
export const MissingNativeScriptTxStepCancel =
  FaultProofStepCancel as unknown as MissingNativeScriptTxStepCancel;

// ## Step 01 — bind the bad transaction
//
// The step-01 UTxO is the initialized fraud proof (its `data` is `None`), so
// it is read with the generic computation-thread step datum. The redeemer is
// the **bare** `NativeTxInclusionArgs` — the family has no published-chunk
// carriage arm on-chain (`lib/…/step-01.ak`: `pub type Args =
// NativeTxInclusionArgs`); emitting the carriage enum would be a positional
// mis-encode.

export const MissingNativeScriptTxStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type MissingNativeScriptTxStep01Datum = Data.Static<
  typeof MissingNativeScriptTxStep01DatumSchema
>;
export const MissingNativeScriptTxStep01Datum =
  MissingNativeScriptTxStep01DatumSchema as unknown as MissingNativeScriptTxStep01Datum;

export const MissingNativeScriptTxStep01ArgsSchema =
  NativeTxInclusionArgsSchema;
export type MissingNativeScriptTxStep01Args = NativeTxInclusionArgs;
export const MissingNativeScriptTxStep01Args =
  NativeTxInclusionArgs as unknown as MissingNativeScriptTxStep01Args;

export const MissingNativeScriptTxStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptTxStep01ArgsSchema);
export type MissingNativeScriptTxStep01SpendRedeemer = Data.Static<
  typeof MissingNativeScriptTxStep01SpendRedeemerSchema
>;
export const MissingNativeScriptTxStep01SpendRedeemer =
  MissingNativeScriptTxStep01SpendRedeemerSchema as unknown as MissingNativeScriptTxStep01SpendRedeemer;

// ## Step 02 — open the bad transaction's spend inputs

/**
 * Mirrors `midgard/fraud_proofs/missing_native_script_tx/step_02.State`.
 * `bad_tx_witness_set_hash` is the value step-01 read off the compact
 * structure the block committed — §3's transaction id does not commit it, so
 * it can only enter the thread here and it is what step-06's `WitnessAnchor`
 * anchors.
 */
export const MissingNativeScriptTxStep02StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  bad_tx_witness_set_hash: H32Schema,
});
export type MissingNativeScriptTxStep02State = Data.Static<
  typeof MissingNativeScriptTxStep02StateSchema
>;
export const MissingNativeScriptTxStep02State =
  MissingNativeScriptTxStep02StateSchema as unknown as MissingNativeScriptTxStep02State;

export const MissingNativeScriptTxStep02DatumSchema = faultProofStepDatumSchema(
  MissingNativeScriptTxStep02StateSchema,
);
export type MissingNativeScriptTxStep02Datum = Data.Static<
  typeof MissingNativeScriptTxStep02DatumSchema
>;
export const MissingNativeScriptTxStep02Datum =
  MissingNativeScriptTxStep02DatumSchema as unknown as MissingNativeScriptTxStep02Datum;

/** Mirrors `midgard/fraud_proofs/missing_native_script_tx/step_02.Args`. */
export const MissingNativeScriptTxStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  bad_input_index: Data.Integer(),
  spend_inputs_opening: FieldOpeningSchema,
});
export type MissingNativeScriptTxStep02Args = Data.Static<
  typeof MissingNativeScriptTxStep02ArgsSchema
>;
export const MissingNativeScriptTxStep02Args =
  MissingNativeScriptTxStep02ArgsSchema as unknown as MissingNativeScriptTxStep02Args;

export const MissingNativeScriptTxStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptTxStep02ArgsSchema);
export type MissingNativeScriptTxStep02SpendRedeemer = Data.Static<
  typeof MissingNativeScriptTxStep02SpendRedeemerSchema
>;
export const MissingNativeScriptTxStep02SpendRedeemer =
  MissingNativeScriptTxStep02SpendRedeemerSchema as unknown as MissingNativeScriptTxStep02SpendRedeemer;

// ## Step 03 — bind the producing transaction

/** Mirrors `midgard/fraud_proofs/missing_native_script_tx/step_03.State`. */
export const MissingNativeScriptTxStep03StateSchema = Data.Object({
  input_with_missing_script: MidgardTxInputSchema,
  bad_tx_id: H32Schema,
  bad_tx_witness_set_hash: H32Schema,
});
export type MissingNativeScriptTxStep03State = Data.Static<
  typeof MissingNativeScriptTxStep03StateSchema
>;
export const MissingNativeScriptTxStep03State =
  MissingNativeScriptTxStep03StateSchema as unknown as MissingNativeScriptTxStep03State;

export const MissingNativeScriptTxStep03DatumSchema = faultProofStepDatumSchema(
  MissingNativeScriptTxStep03StateSchema,
);
export type MissingNativeScriptTxStep03Datum = Data.Static<
  typeof MissingNativeScriptTxStep03DatumSchema
>;
export const MissingNativeScriptTxStep03Datum =
  MissingNativeScriptTxStep03DatumSchema as unknown as MissingNativeScriptTxStep03Datum;

export const MissingNativeScriptTxStep03ArgsSchema =
  NativeTxInclusionArgsSchema;
export type MissingNativeScriptTxStep03Args = NativeTxInclusionArgs;
export const MissingNativeScriptTxStep03Args =
  NativeTxInclusionArgs as unknown as MissingNativeScriptTxStep03Args;

export const MissingNativeScriptTxStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptTxStep03ArgsSchema);
export type MissingNativeScriptTxStep03SpendRedeemer = Data.Static<
  typeof MissingNativeScriptTxStep03SpendRedeemerSchema
>;
export const MissingNativeScriptTxStep03SpendRedeemer =
  MissingNativeScriptTxStep03SpendRedeemerSchema as unknown as MissingNativeScriptTxStep03SpendRedeemer;

// ## Step 04 — open the producing transaction's outputs

/** Mirrors `midgard/fraud_proofs/missing_native_script_tx/step_04.State`. */
export const MissingNativeScriptTxStep04StateSchema = Data.Object({
  producing_tx_id: H32Schema,
  bad_input_output_index: Data.Integer(),
  bad_tx_id: H32Schema,
  bad_tx_witness_set_hash: H32Schema,
});
export type MissingNativeScriptTxStep04State = Data.Static<
  typeof MissingNativeScriptTxStep04StateSchema
>;
export const MissingNativeScriptTxStep04State =
  MissingNativeScriptTxStep04StateSchema as unknown as MissingNativeScriptTxStep04State;

export const MissingNativeScriptTxStep04DatumSchema = faultProofStepDatumSchema(
  MissingNativeScriptTxStep04StateSchema,
);
export type MissingNativeScriptTxStep04Datum = Data.Static<
  typeof MissingNativeScriptTxStep04DatumSchema
>;
export const MissingNativeScriptTxStep04Datum =
  MissingNativeScriptTxStep04DatumSchema as unknown as MissingNativeScriptTxStep04Datum;

/** Mirrors `midgard/fraud_proofs/missing_native_script_tx/step_04.Args`. */
export const MissingNativeScriptTxStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  outputs_opening: FieldOpeningSchema,
});
export type MissingNativeScriptTxStep04Args = Data.Static<
  typeof MissingNativeScriptTxStep04ArgsSchema
>;
export const MissingNativeScriptTxStep04Args =
  MissingNativeScriptTxStep04ArgsSchema as unknown as MissingNativeScriptTxStep04Args;

export const MissingNativeScriptTxStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptTxStep04ArgsSchema);
export type MissingNativeScriptTxStep04SpendRedeemer = Data.Static<
  typeof MissingNativeScriptTxStep04SpendRedeemerSchema
>;
export const MissingNativeScriptTxStep04SpendRedeemer =
  MissingNativeScriptTxStep04SpendRedeemerSchema as unknown as MissingNativeScriptTxStep04SpendRedeemer;

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
  MissingNativeScriptTxStep05StateSchema as unknown as MissingNativeScriptTxStep05State;

export const MissingNativeScriptTxStep05DatumSchema = faultProofStepDatumSchema(
  MissingNativeScriptTxStep05StateSchema,
);
export type MissingNativeScriptTxStep05Datum = Data.Static<
  typeof MissingNativeScriptTxStep05DatumSchema
>;
export const MissingNativeScriptTxStep05Datum =
  MissingNativeScriptTxStep05DatumSchema as unknown as MissingNativeScriptTxStep05Datum;

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
  MissingNativeScriptTxStep05ArgsSchema as unknown as MissingNativeScriptTxStep05Args;

export const MissingNativeScriptTxStep05SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptTxStep05ArgsSchema);
export type MissingNativeScriptTxStep05SpendRedeemer = Data.Static<
  typeof MissingNativeScriptTxStep05SpendRedeemerSchema
>;
export const MissingNativeScriptTxStep05SpendRedeemer =
  MissingNativeScriptTxStep05SpendRedeemerSchema as unknown as MissingNativeScriptTxStep05SpendRedeemer;

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
  MissingNativeScriptTxPhaseSchema as unknown as MissingNativeScriptTxPhase;

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
  MissingNativeScriptTxStep06StateSchema as unknown as MissingNativeScriptTxStep06State;

export const MissingNativeScriptTxStep06DatumSchema = faultProofStepDatumSchema(
  MissingNativeScriptTxStep06StateSchema,
);
export type MissingNativeScriptTxStep06Datum = Data.Static<
  typeof MissingNativeScriptTxStep06DatumSchema
>;
export const MissingNativeScriptTxStep06Datum =
  MissingNativeScriptTxStep06DatumSchema as unknown as MissingNativeScriptTxStep06Datum;

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
  MissingNativeScriptTxStep06ArgsSchema as unknown as MissingNativeScriptTxStep06Args;

export const MissingNativeScriptTxStep06SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptTxStep06ArgsSchema);
export type MissingNativeScriptTxStep06SpendRedeemer = Data.Static<
  typeof MissingNativeScriptTxStep06SpendRedeemerSchema
>;
export const MissingNativeScriptTxStep06SpendRedeemer =
  MissingNativeScriptTxStep06SpendRedeemerSchema as unknown as MissingNativeScriptTxStep06SpendRedeemer;

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
  MissingNativeScriptTxStep07DatumSchema as unknown as MissingNativeScriptTxStep07Datum;

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
  MissingNativeScriptTxStep07ArgsSchema as unknown as MissingNativeScriptTxStep07Args;

export const MissingNativeScriptTxStep07SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptTxStep07ArgsSchema);
export type MissingNativeScriptTxStep07SpendRedeemer = Data.Static<
  typeof MissingNativeScriptTxStep07SpendRedeemerSchema
>;
export const MissingNativeScriptTxStep07SpendRedeemer =
  MissingNativeScriptTxStep07SpendRedeemerSchema as unknown as MissingNativeScriptTxStep07SpendRedeemer;

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
  MissingNativeScriptTxStep08DatumSchema as unknown as MissingNativeScriptTxStep08Datum;

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
  MissingNativeScriptTxStep08ArgsSchema as unknown as MissingNativeScriptTxStep08Args;

export const MissingNativeScriptTxStep08SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptTxStep08ArgsSchema);
export type MissingNativeScriptTxStep08SpendRedeemer = Data.Static<
  typeof MissingNativeScriptTxStep08SpendRedeemerSchema
>;
export const MissingNativeScriptTxStep08SpendRedeemer =
  MissingNativeScriptTxStep08SpendRedeemerSchema as unknown as MissingNativeScriptTxStep08SpendRedeemer;

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

/** Exactly the state `step-03` writes for `step-04` (`step-03.ak:70-77`). */
export const missingNativeScriptTxStep04State = ({
  producingTxId,
  badInputOutputIndex,
  badTxId,
  badTxWitnessSetHash,
}: {
  readonly producingTxId: string;
  readonly badInputOutputIndex: bigint;
  readonly badTxId: string;
  readonly badTxWitnessSetHash: string;
}): MissingNativeScriptTxStep04State => ({
  producing_tx_id: producingTxId.toLowerCase(),
  bad_input_output_index: badInputOutputIndex,
  bad_tx_id: badTxId.toLowerCase(),
  bad_tx_witness_set_hash: badTxWitnessSetHash.toLowerCase(),
});

/**
 * Exactly the state `step-04` writes for `step-05` (`step-04.ak:96-100`),
 * which is also the state `step-05` forwards to `step-06` unchanged.
 */
export const missingNativeScriptTxStep05State = ({
  expectedMissingScriptHash,
  badTxId,
  badTxWitnessSetHash,
}: {
  readonly expectedMissingScriptHash: string;
  readonly badTxId: string;
  readonly badTxWitnessSetHash: string;
}): MissingNativeScriptTxStep05State => ({
  expected_missing_script_hash: expectedMissingScriptHash.toLowerCase(),
  bad_tx_id: badTxId.toLowerCase(),
  bad_tx_witness_set_hash: badTxWitnessSetHash.toLowerCase(),
});

/** Exact state step-05 writes at the step-06 direct/staged routing boundary. */
export const missingNativeScriptTxStep06ReadyState = (
  state: MissingNativeScriptTxStep05State,
): MissingNativeScriptTxStep06State => ({
  ...state,
  phase: "Ready",
});
