import {
  decodeMidgardVersionedScript,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema } from "../common.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
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

export const H28Schema = Data.Bytes({ minLength: 28, maxLength: 28 });

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
  asDataType<MissingNativeScriptTxStep01Datum>(
    MissingNativeScriptTxStep01DatumSchema,
  );

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
  asDataType<MissingNativeScriptTxStep01SpendRedeemer>(
    MissingNativeScriptTxStep01SpendRedeemerSchema,
  );

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
  asDataType<MissingNativeScriptTxStep02State>(
    MissingNativeScriptTxStep02StateSchema,
  );

export const MissingNativeScriptTxStep02DatumSchema = faultProofStepDatumSchema(
  MissingNativeScriptTxStep02StateSchema,
);

export type MissingNativeScriptTxStep02Datum = Data.Static<
  typeof MissingNativeScriptTxStep02DatumSchema
>;

export const MissingNativeScriptTxStep02Datum =
  asDataType<MissingNativeScriptTxStep02Datum>(
    MissingNativeScriptTxStep02DatumSchema,
  );

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
  asDataType<MissingNativeScriptTxStep02Args>(
    MissingNativeScriptTxStep02ArgsSchema,
  );

export const MissingNativeScriptTxStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptTxStep02ArgsSchema);

export type MissingNativeScriptTxStep02SpendRedeemer = Data.Static<
  typeof MissingNativeScriptTxStep02SpendRedeemerSchema
>;

export const MissingNativeScriptTxStep02SpendRedeemer =
  asDataType<MissingNativeScriptTxStep02SpendRedeemer>(
    MissingNativeScriptTxStep02SpendRedeemerSchema,
  );

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
  asDataType<MissingNativeScriptTxStep03State>(
    MissingNativeScriptTxStep03StateSchema,
  );

export const MissingNativeScriptTxStep03DatumSchema = faultProofStepDatumSchema(
  MissingNativeScriptTxStep03StateSchema,
);

export type MissingNativeScriptTxStep03Datum = Data.Static<
  typeof MissingNativeScriptTxStep03DatumSchema
>;

export const MissingNativeScriptTxStep03Datum =
  asDataType<MissingNativeScriptTxStep03Datum>(
    MissingNativeScriptTxStep03DatumSchema,
  );

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
  asDataType<MissingNativeScriptTxStep03SpendRedeemer>(
    MissingNativeScriptTxStep03SpendRedeemerSchema,
  );

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
  asDataType<MissingNativeScriptTxStep04State>(
    MissingNativeScriptTxStep04StateSchema,
  );

export const MissingNativeScriptTxStep04DatumSchema = faultProofStepDatumSchema(
  MissingNativeScriptTxStep04StateSchema,
);

export type MissingNativeScriptTxStep04Datum = Data.Static<
  typeof MissingNativeScriptTxStep04DatumSchema
>;

export const MissingNativeScriptTxStep04Datum =
  asDataType<MissingNativeScriptTxStep04Datum>(
    MissingNativeScriptTxStep04DatumSchema,
  );

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
  asDataType<MissingNativeScriptTxStep04Args>(
    MissingNativeScriptTxStep04ArgsSchema,
  );

export const MissingNativeScriptTxStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingNativeScriptTxStep04ArgsSchema);

export type MissingNativeScriptTxStep04SpendRedeemer = Data.Static<
  typeof MissingNativeScriptTxStep04SpendRedeemerSchema
>;

export const MissingNativeScriptTxStep04SpendRedeemer =
  asDataType<MissingNativeScriptTxStep04SpendRedeemer>(
    MissingNativeScriptTxStep04SpendRedeemerSchema,
  );
