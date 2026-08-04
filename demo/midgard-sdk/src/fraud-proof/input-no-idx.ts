/**
 * `input-no-idx` (`nonExistentInputNoIndex`) fault-proof family (Goal task
 * `Q13`).
 *
 * **Rule.** Every spend input of a committed transaction must name an output
 * that its producing transaction actually created: for an input
 * `(tx_id, output_index)` whose producer `tx_id` is itself committed in the
 * same block, `output_index` must be strictly less than the number of outputs
 * that producer commits.
 *
 * **Violation `input-no-idx`.** A committed transaction spends
 * `(producing_tx_id, output_index)` where `producing_tx_id` *is* committed in
 * the same block — so the preimage of the transaction id exists — yet
 * `output_index >= |producer.outputs|`. The UTxO therefore never existed, and
 * no other family can convict it: `non-existent-input` proves exclusion from
 * the previous block's ledger, which says nothing about an output index of a
 * transaction produced inside this block.
 *
 * The proof is a four-step computation thread:
 *
 * 1. bind the bad transaction to the block's counted `transactions_root` and
 *    forward its canonical `spend_inputs_hash`;
 * 2. open that commitment with the complete spend-inputs preimage and forward
 *    the challenged `(tx_id, output_index)`;
 * 3. bind the producing transaction to the *same* block and forward its
 *    canonical `outputs_hash` alongside the challenged index; and
 * 4. open the outputs commitment with the complete outputs preimage and
 *    require `output_index >= |outputs|`.
 *
 * This module is the strict TypeScript twin of
 * `onchain/aiken/lib/midgard/fraud-proofs/input-no-idx/step-0{1..4}.ak` and of
 * the `MidgardTxOutput` shape in
 * `onchain/aiken/lib/midgard/fraud-proofs/native-tx/types.ak`.
 */
import { Data } from "@lucid-evolution/lucid";

import { H32Schema } from "@/common.js";

import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  MidgardTxInput,
  type MidgardTxInput as MidgardTxInputData,
  MidgardTxInputListSchema,
  MidgardTxInputSchema,
  NativeTxInclusionArgs,
  NativeTxInclusionArgsSchema,
} from "./native.js";

/** Catalogue violation identifier adjudicated by this family. */
export const INPUT_NO_IDX_VIOLATION_ID_V1 = "input-no-idx" as const;

/** Catalogue category name this family is registered under (§5.1 order). */
export const INPUT_NO_IDX_CATALOGUE_CATEGORY_V1 =
  "nonExistentInputNoIndex" as const;

// ## Rule

/**
 * The adjudicated violation predicate, over evidence that has already been
 * authenticated against the block header: the challenged input's producing
 * transaction is committed in the same block, and the index it spends is at
 * or past the end of that producer's canonical outputs list.
 *
 * This is exactly `bad_input_output_index >= list.length(outputs_preimage)`
 * in `validators/fraud-proofs/input-no-idx/step-04.ak`.
 */
export const isInputNoIdxViolationV1 = ({
  badInputOutputIndex,
  producingTxOutputCount,
}: {
  readonly badInputOutputIndex: bigint;
  readonly producingTxOutputCount: number;
}): boolean => badInputOutputIndex >= BigInt(producingTxOutputCount);

/** Canonical evidence record for one challenged spend input. */
export type InputNoIdxEvidenceV1 = {
  readonly violationId: typeof INPUT_NO_IDX_VIOLATION_ID_V1;
  /** Committed transaction that spends the non-existent output. */
  readonly badTxId: string;
  /** Position of the challenged input inside the bad tx's spend-inputs list. */
  readonly badInputsIndex: number;
  /** The challenged input itself. */
  readonly badInput: MidgardTxInputData;
  /** Producing transaction, committed in the same block as the bad tx. */
  readonly producingTxId: string;
  readonly producingTxOutputCount: number;
  readonly isViolation: boolean;
};

/**
 * Builds the evidence record for one challenged input. The caller must have
 * authenticated both transactions against the header's counted
 * `transactions_root`; this function performs no I/O and never throws.
 */
export const inputNoIdxEvidenceFromCommittedTransactionsV1 = ({
  badTxId,
  badInputsIndex,
  badInput,
  producingTxOutputCount,
}: {
  readonly badTxId: string;
  readonly badInputsIndex: number;
  readonly badInput: MidgardTxInputData;
  readonly producingTxOutputCount: number;
}): InputNoIdxEvidenceV1 =>
  Object.freeze({
    violationId: INPUT_NO_IDX_VIOLATION_ID_V1,
    badTxId: badTxId.toLowerCase(),
    badInputsIndex,
    badInput: {
      tx_id: badInput.tx_id.toLowerCase(),
      output_index: badInput.output_index,
    },
    producingTxId: badInput.tx_id.toLowerCase(),
    producingTxOutputCount,
    isViolation: isInputNoIdxViolationV1({
      badInputOutputIndex: badInput.output_index,
      producingTxOutputCount,
    }),
  });

// ## Native output schemas (Aiken `MidgardTxOutput` and its components)
//
// Step 04 carries the producing transaction's complete outputs preimage as
// structured PlutusData and re-encodes it on-chain with
// `encode_midgard_tx_output`, so these schemas must agree constructor for
// constructor with `native-tx/types.ak`.

export const MidgardCredentialSchema = Data.Enum([
  Data.Object({ PubKeyCredential: Data.Tuple([Data.Bytes()]) }),
  Data.Object({ ScriptCredential: Data.Tuple([Data.Bytes()]) }),
]);
export type MidgardCredential = Data.Static<typeof MidgardCredentialSchema>;
export const MidgardCredential =
  MidgardCredentialSchema as unknown as MidgardCredential;

export const MidgardAddressSchema = Data.Object({
  protected: Data.Boolean(),
  network_id: Data.Integer(),
  payment_credential: MidgardCredentialSchema,
  stake_credential: Data.Nullable(MidgardCredentialSchema),
});
export type MidgardAddress = Data.Static<typeof MidgardAddressSchema>;
export const MidgardAddress = MidgardAddressSchema as unknown as MidgardAddress;

/** `MidgardValue { lovelace, assets }`; `assets` is a flat policy/name map. */
export const MidgardValueSchema = Data.Object({
  lovelace: Data.Integer(),
  assets: Data.Map(Data.Bytes(), Data.Integer()),
});
export type MidgardValue = Data.Static<typeof MidgardValueSchema>;
export const MidgardValue = MidgardValueSchema as unknown as MidgardValue;

/** `NativeCardanoScript | PlutusV3Script | MidgardV1Script`, in that order. */
export const MidgardScriptLanguageSchema = Data.Enum([
  Data.Literal("NativeCardanoScript"),
  Data.Literal("PlutusV3Script"),
  Data.Literal("MidgardV1Script"),
]);
export type MidgardScriptLanguage = Data.Static<
  typeof MidgardScriptLanguageSchema
>;
export const MidgardScriptLanguage =
  MidgardScriptLanguageSchema as unknown as MidgardScriptLanguage;

export const MidgardVersionedScriptSchema = Data.Object({
  language: MidgardScriptLanguageSchema,
  script_bytes: Data.Bytes(),
});
export type MidgardVersionedScript = Data.Static<
  typeof MidgardVersionedScriptSchema
>;
export const MidgardVersionedScript =
  MidgardVersionedScriptSchema as unknown as MidgardVersionedScript;

export const MidgardTxOutputSchema = Data.Object({
  address: MidgardAddressSchema,
  value: MidgardValueSchema,
  datum_cbor: Data.Nullable(Data.Bytes()),
  script_ref: Data.Nullable(MidgardVersionedScriptSchema),
});
export type MidgardTxOutput = Data.Static<typeof MidgardTxOutputSchema>;
export const MidgardTxOutput =
  MidgardTxOutputSchema as unknown as MidgardTxOutput;

export const MidgardTxOutputListSchema = Data.Array(MidgardTxOutputSchema);
export type MidgardTxOutputList = Data.Static<typeof MidgardTxOutputListSchema>;
export const MidgardTxOutputList =
  MidgardTxOutputListSchema as unknown as MidgardTxOutputList;

// ## On-chain step schemas (positional agreement with the Aiken step modules)

export const InputNoIdxStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type InputNoIdxStep01Datum = Data.Static<
  typeof InputNoIdxStep01DatumSchema
>;
export const InputNoIdxStep01Datum =
  InputNoIdxStep01DatumSchema as unknown as InputNoIdxStep01Datum;

export const InputNoIdxStep01SpendRedeemerSchema = faultProofStepRedeemerSchema(
  NativeTxInclusionArgsSchema,
);
export type InputNoIdxStep01SpendRedeemer = Data.Static<
  typeof InputNoIdxStep01SpendRedeemerSchema
>;
export const InputNoIdxStep01SpendRedeemer =
  InputNoIdxStep01SpendRedeemerSchema as unknown as InputNoIdxStep01SpendRedeemer;

/** Mirrors `midgard/fraud_proofs/input_no_idx/step_02.State`. */
export const InputNoIdxStep02StateSchema = Data.Object({
  verified_tx_inputs_hash: H32Schema,
});
export type InputNoIdxStep02State = Data.Static<
  typeof InputNoIdxStep02StateSchema
>;
export const InputNoIdxStep02State =
  InputNoIdxStep02StateSchema as unknown as InputNoIdxStep02State;

export const InputNoIdxStep02DatumSchema = faultProofStepDatumSchema(
  InputNoIdxStep02StateSchema,
);
export type InputNoIdxStep02Datum = Data.Static<
  typeof InputNoIdxStep02DatumSchema
>;
export const InputNoIdxStep02Datum =
  InputNoIdxStep02DatumSchema as unknown as InputNoIdxStep02Datum;

export const InputNoIdxStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  inputs_preimage: MidgardTxInputListSchema,
  bad_inputs_index: Data.Integer(),
});
export type InputNoIdxStep02Args = Data.Static<
  typeof InputNoIdxStep02ArgsSchema
>;
export const InputNoIdxStep02Args =
  InputNoIdxStep02ArgsSchema as unknown as InputNoIdxStep02Args;

export const InputNoIdxStep02SpendRedeemerSchema = faultProofStepRedeemerSchema(
  InputNoIdxStep02ArgsSchema,
);
export type InputNoIdxStep02SpendRedeemer = Data.Static<
  typeof InputNoIdxStep02SpendRedeemerSchema
>;
export const InputNoIdxStep02SpendRedeemer =
  InputNoIdxStep02SpendRedeemerSchema as unknown as InputNoIdxStep02SpendRedeemer;

/** Mirrors `midgard/fraud_proofs/input_no_idx/step_03.State`. */
export const InputNoIdxStep03StateSchema = Data.Object({
  bad_input_tx_id: H32Schema,
  bad_input_output_index: Data.Integer(),
});
export type InputNoIdxStep03State = Data.Static<
  typeof InputNoIdxStep03StateSchema
>;
export const InputNoIdxStep03State =
  InputNoIdxStep03StateSchema as unknown as InputNoIdxStep03State;

export const InputNoIdxStep03DatumSchema = faultProofStepDatumSchema(
  InputNoIdxStep03StateSchema,
);
export type InputNoIdxStep03Datum = Data.Static<
  typeof InputNoIdxStep03DatumSchema
>;
export const InputNoIdxStep03Datum =
  InputNoIdxStep03DatumSchema as unknown as InputNoIdxStep03Datum;

/** Step 03 re-enters the shared native inclusion binding. */
export const InputNoIdxStep03SpendRedeemerSchema = faultProofStepRedeemerSchema(
  NativeTxInclusionArgsSchema,
);
export type InputNoIdxStep03SpendRedeemer = Data.Static<
  typeof InputNoIdxStep03SpendRedeemerSchema
>;
export const InputNoIdxStep03SpendRedeemer =
  InputNoIdxStep03SpendRedeemerSchema as unknown as InputNoIdxStep03SpendRedeemer;

/** Mirrors `midgard/fraud_proofs/input_no_idx/step_04.State`. */
export const InputNoIdxStep04StateSchema = Data.Object({
  producing_tx_outputs_hash: H32Schema,
  bad_input_output_index: Data.Integer(),
});
export type InputNoIdxStep04State = Data.Static<
  typeof InputNoIdxStep04StateSchema
>;
export const InputNoIdxStep04State =
  InputNoIdxStep04StateSchema as unknown as InputNoIdxStep04State;

export const InputNoIdxStep04DatumSchema = faultProofStepDatumSchema(
  InputNoIdxStep04StateSchema,
);
export type InputNoIdxStep04Datum = Data.Static<
  typeof InputNoIdxStep04DatumSchema
>;
export const InputNoIdxStep04Datum =
  InputNoIdxStep04DatumSchema as unknown as InputNoIdxStep04Datum;

export const InputNoIdxStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  outputs_preimage: MidgardTxOutputListSchema,
});
export type InputNoIdxStep04Args = Data.Static<
  typeof InputNoIdxStep04ArgsSchema
>;
export const InputNoIdxStep04Args =
  InputNoIdxStep04ArgsSchema as unknown as InputNoIdxStep04Args;

export const InputNoIdxStep04SpendRedeemerSchema = faultProofStepRedeemerSchema(
  InputNoIdxStep04ArgsSchema,
);
export type InputNoIdxStep04SpendRedeemer = Data.Static<
  typeof InputNoIdxStep04SpendRedeemerSchema
>;
export const InputNoIdxStep04SpendRedeemer =
  InputNoIdxStep04SpendRedeemerSchema as unknown as InputNoIdxStep04SpendRedeemer;

export {
  MidgardTxInput as InputNoIdxSpendInput,
  MidgardTxInputSchema as InputNoIdxSpendInputSchema,
  FaultProofStepCancel as InputNoIdxStepCancel,
  FaultProofStepCancelSchema as InputNoIdxStepCancelSchema,
  NativeTxInclusionArgs as InputNoIdxTxInclusionArgs,
  NativeTxInclusionArgsSchema as InputNoIdxTxInclusionArgsSchema,
};

// ## Step-state builders (twins of the on-chain forwarding rules)

/** Exactly the state `step-01` writes for `step-02`. */
export const inputNoIdxStep02StateFromBadTxV1 = (
  badTxSpendInputsHash: string,
): InputNoIdxStep02State => ({
  verified_tx_inputs_hash: badTxSpendInputsHash.toLowerCase(),
});

/** Exactly the state `step-02` writes for `step-03`. */
export const inputNoIdxStep03StateFromEvidenceV1 = (
  evidence: InputNoIdxEvidenceV1,
): InputNoIdxStep03State => ({
  bad_input_tx_id: evidence.badInput.tx_id,
  bad_input_output_index: evidence.badInput.output_index,
});

/** Exactly the state `step-03` writes for `step-04`. */
export const inputNoIdxStep04StateFromEvidenceV1 = ({
  evidence,
  producingTxOutputsHash,
}: {
  readonly evidence: InputNoIdxEvidenceV1;
  readonly producingTxOutputsHash: string;
}): InputNoIdxStep04State => ({
  producing_tx_outputs_hash: producingTxOutputsHash.toLowerCase(),
  bad_input_output_index: evidence.badInput.output_index,
});
