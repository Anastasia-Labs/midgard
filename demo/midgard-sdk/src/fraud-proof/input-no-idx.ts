/**
 * ⚠️ **STALE AS OF #575 — do not build a datum or redeemer from this module
 * and expect chain to accept it. Owner: #579.** The rebind, its three concrete
 * divergences, and why they are not re-derived in this lane are explained once
 * in `docs/fault-proofs/offchain-builder-staleness-575.md`.
 *
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
import {
  buildMidgardBoundedCollectionItemProofV1,
  buildMidgardBoundedCollectionV1,
  encodeCbor,
  type MidgardBoundedCollectionItemProofV1,
  verifyMidgardBoundedCollectionItemProofV1,
} from "@al-ft/midgard-core";
import { CML, Data } from "@lucid-evolution/lucid";

import { H32Schema, PubKeyHashSchema, ScriptHashSchema } from "@/common.js";
import {
  type BoundedCollectionItemProofV1,
  BoundedCollectionItemProofV1Schema,
} from "@/ledger-state.js";

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

/**
 * Release-policy boundary for direct step-02 carriage. This does not restrict
 * the consensus-valid `Complete` constructor.
 */
export const INPUT_NO_IDX_STEP02_DIRECT_INPUT_LIMIT_V1 = 19;

export const inputNoIdxStep02ExecutionModeV1 = (
  itemCount: number,
): "direct" | "fold" =>
  itemCount <= INPUT_NO_IDX_STEP02_DIRECT_INPUT_LIMIT_V1 ? "direct" : "fold";

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
export const InputNoIdxStep02DirectStateSchema = Data.Object({
  verified_tx_inputs_hash: H32Schema,
});
export const InputNoIdxStep02FoldingStateSchema = Data.Object({
  verified_tx_inputs_hash: H32Schema,
  item_count: Data.Integer(),
  next_item_index: Data.Integer(),
  bad_inputs_index: Data.Integer(),
  selected_input: Data.Nullable(MidgardTxInputSchema),
});
export const InputNoIdxStep02StateSchema = Data.Enum([
  Data.Object({ Direct: InputNoIdxStep02DirectStateSchema }),
  Data.Object({ Folding: InputNoIdxStep02FoldingStateSchema }),
]);
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

/** Typed inline datum carried by an Ada-only tier-2 publication output. */
export const PublishedSpendInputsV1Schema = Data.Object({
  version: Data.Integer(),
  computation_thread_policy_id: ScriptHashSchema,
  computation_thread_asset_name: Data.Bytes({ maxLength: 32 }),
  fraud_prover: PubKeyHashSchema,
  verified_tx_inputs_hash: H32Schema,
  item_count: Data.Integer(),
  inputs: MidgardTxInputListSchema,
});
export type PublishedSpendInputsV1 = Data.Static<
  typeof PublishedSpendInputsV1Schema
>;
export const PublishedSpendInputsV1 =
  PublishedSpendInputsV1Schema as unknown as PublishedSpendInputsV1;

export const InputNoIdxStep02CompleteArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  inputs_preimage: MidgardTxInputListSchema,
  bad_inputs_index: Data.Integer(),
});
export const InputNoIdxStep02CompletePublishedArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  publication_reference_input_index: Data.Integer(),
  bad_inputs_index: Data.Integer(),
});
export const InputNoIdxStep02FoldStartArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  bad_inputs_index: Data.Integer(),
  input_cbor: Data.Bytes(),
  collection_proof: BoundedCollectionItemProofV1Schema,
});
export const InputNoIdxStep02FoldNextArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  input_cbor: Data.Bytes(),
  collection_proof: BoundedCollectionItemProofV1Schema,
});
export const InputNoIdxStep02ArgsSchema = Data.Enum([
  Data.Object({ Complete: InputNoIdxStep02CompleteArgsSchema }),
  Data.Object({
    CompletePublished: InputNoIdxStep02CompletePublishedArgsSchema,
  }),
  Data.Object({ FoldStart: InputNoIdxStep02FoldStartArgsSchema }),
  Data.Object({ FoldNext: InputNoIdxStep02FoldNextArgsSchema }),
]);
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
  Direct: {
    verified_tx_inputs_hash: badTxSpendInputsHash.toLowerCase(),
  },
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

// ## Canonical native item encoders
//
// Byte-for-byte twins of `encode_midgard_tx_input` and
// `encode_midgard_tx_output`
// (`onchain/aiken/lib/midgard/fraud-proofs/native-tx/components.ak`). Both
// preimage-opening steps of this family re-derive their bounded-collection
// commitment from these encoders rather than trusting a prepared file, so an
// off-chain builder and the L1 verifier cannot drift.

/** Canonical spend-inputs field index of a native V1 transaction body. */
export const INPUT_NO_IDX_SPEND_INPUTS_FIELD_INDEX_V1 = 0;

/** Canonical outputs field index of a native V1 transaction body. */
export const INPUT_NO_IDX_OUTPUTS_FIELD_INDEX_V1 = 2;

const definiteBytes = (bytes: Buffer): Buffer => {
  const length = bytes.length;
  if (length <= 23) {
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

const definiteMapHeader = (length: number): Buffer => {
  if (length <= 23) {
    return Buffer.from([0xa0 + length]);
  }
  if (length <= 0xff) {
    return Buffer.from([0xb8, length]);
  }
  if (length <= 0xffff) {
    const header = Buffer.alloc(3);
    header[0] = 0xb9;
    header.writeUInt16BE(length, 1);
    return header;
  }
  const header = Buffer.alloc(5);
  header[0] = 0xba;
  header.writeUInt32BE(length, 1);
  return header;
};

const credentialHash = (credential: MidgardCredential): Buffer =>
  Buffer.from(
    "PubKeyCredential" in credential
      ? credential.PubKeyCredential[0]
      : credential.ScriptCredential[0],
    "hex",
  );

const credentialIsScript = (credential: MidgardCredential): boolean =>
  !("PubKeyCredential" in credential);

/** Twin of `encode_midgard_address`. */
export const encodeMidgardAddressCanonicalV1 = (
  address: MidgardAddress,
): Buffer => {
  const networkId = Number(address.network_id);
  if (networkId !== 0 && networkId !== 1) {
    throw new Error(`Midgard address network id ${networkId} is not 0 or 1`);
  }
  const paymentHash = credentialHash(address.payment_credential);
  const stake = address.stake_credential;
  const addressType =
    stake === null
      ? credentialIsScript(address.payment_credential)
        ? 7
        : 6
      : (credentialIsScript(address.payment_credential) ? 1 : 0) +
        (credentialIsScript(stake) ? 2 : 0);
  const header = addressType * 16 + networkId + (address.protected ? 8 : 0);
  return Buffer.concat([
    Buffer.from([header]),
    paymentHash,
    ...(stake === null ? [] : [credentialHash(stake)]),
  ]);
};

/** Twin of `encode_midgard_value`; `assets` keys are `policy_id ++ name`. */
export const encodeMidgardValueCanonicalV1 = (value: MidgardValue): Buffer => {
  if (value.lovelace < 0n) {
    throw new Error("Midgard value lovelace must not be negative");
  }
  const groups: { policyId: Buffer; assets: [Buffer, bigint][] }[] = [];
  for (const [unitHex, quantity] of value.assets.entries()) {
    const unit = Buffer.from(unitHex, "hex");
    if (unit.length < 28) {
      throw new Error(`Midgard asset unit ${unitHex} is shorter than a policy`);
    }
    const policyId = Buffer.from(unit.subarray(0, 28));
    const assetName = Buffer.from(unit.subarray(28));
    const previous = groups.at(-1);
    if (previous !== undefined && previous.policyId.equals(policyId)) {
      previous.assets.push([assetName, quantity]);
    } else {
      groups.push({ policyId, assets: [[assetName, quantity]] });
    }
  }
  return Buffer.concat([
    Buffer.from([0x82]),
    encodeCbor(value.lovelace),
    definiteMapHeader(groups.length),
    ...groups.flatMap((group) => [
      definiteBytes(group.policyId),
      definiteMapHeader(group.assets.length),
      ...group.assets.flatMap(([assetName, quantity]) => [
        definiteBytes(assetName),
        encodeCbor(quantity),
      ]),
    ]),
  ]);
};

const SCRIPT_LANGUAGE_TAG_V1: Readonly<Record<MidgardScriptLanguage, number>> =
  {
    NativeCardanoScript: 0,
    PlutusV3Script: 3,
    MidgardV1Script: 128,
  };

/** Twin of `encode_midgard_versioned_script`. */
export const encodeMidgardVersionedScriptCanonicalV1 = (
  script: MidgardVersionedScript,
): Buffer =>
  Buffer.concat([
    Buffer.from([0x82]),
    encodeCbor(BigInt(SCRIPT_LANGUAGE_TAG_V1[script.language])),
    definiteBytes(Buffer.from(script.script_bytes, "hex")),
  ]);

/** Twin of `encode_midgard_tx_output`. */
export const encodeMidgardTxOutputCanonicalV1 = (
  output: MidgardTxOutput,
): Buffer => {
  const entryCount =
    2 +
    (output.datum_cbor === null ? 0 : 1) +
    (output.script_ref === null ? 0 : 1);
  return Buffer.concat([
    Buffer.from([0xa0 + entryCount, 0x00]),
    definiteBytes(encodeMidgardAddressCanonicalV1(output.address)),
    Buffer.from([0x01]),
    encodeMidgardValueCanonicalV1(output.value),
    ...(output.datum_cbor === null
      ? []
      : [
          Buffer.from([0x02]),
          definiteBytes(Buffer.from(output.datum_cbor, "hex")),
        ]),
    ...(output.script_ref === null
      ? []
      : [
          Buffer.from([0x03]),
          encodeMidgardVersionedScriptCanonicalV1(output.script_ref),
        ]),
  ]);
};

/** Twin of `encode_midgard_tx_input`: canonical Cardano `TransactionInput`. */
export const encodeMidgardTxInputCanonicalV1 = (
  input: MidgardTxInputData,
): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(input.tx_id),
      input.output_index,
    ).to_cbor_bytes(),
  );

/**
 * The `spend_inputs_hash` a native transaction body commits for `inputs`,
 * derived exactly as `bounded_collection_v1.from_items(0, ...)` does on-chain.
 */
export const inputNoIdxSpendInputsCommitmentV1 = (
  inputs: readonly MidgardTxInputData[],
): string =>
  buildMidgardBoundedCollectionV1({
    fieldIndex: INPUT_NO_IDX_SPEND_INPUTS_FIELD_INDEX_V1,
    items: inputs.map(encodeMidgardTxInputCanonicalV1),
  }).commitment.toString("hex");

/** One ordered field-0 opening used by the step-02 fold path. */
export type InputNoIdxSpendInputFoldOpeningV1 = {
  readonly inputCbor: string;
  readonly collectionProof: BoundedCollectionItemProofV1;
};

const collectionProofFromCoreV1 = (
  proof: MidgardBoundedCollectionItemProofV1,
): BoundedCollectionItemProofV1 => ({
  version: BigInt(proof.version),
  field_index: BigInt(proof.fieldIndex),
  item_count: BigInt(proof.itemCount),
  item_index: BigInt(proof.itemIndex),
  item_length: BigInt(proof.itemLength),
  item_commitment: proof.itemCommitment.toString("hex"),
  frontier: proof.frontier.peaks.map((peak) => ({
    height: BigInt(peak.height),
    hash: peak.hash.toString("hex"),
  })),
  siblings: proof.siblings.map((sibling) => sibling.toString("hex")),
});

const proofIntegerV1 = (value: bigint, label: string): number => {
  const exact = Number(value);
  if (!Number.isSafeInteger(exact) || exact < 0 || BigInt(exact) !== value) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return exact;
};

const proofHashV1 = (value: string, label: string): Buffer => {
  if (!/^[0-9a-f]{64}$/u.test(value)) {
    throw new Error(`${label} must be a lowercase 32-byte hexadecimal hash`);
  }
  return Buffer.from(value, "hex");
};

const collectionProofToCoreV1 = (
  proof: BoundedCollectionItemProofV1,
): MidgardBoundedCollectionItemProofV1 => {
  const version = proofIntegerV1(proof.version, "collection proof version");
  if (version !== 1) {
    throw new Error(`unknown bounded-collection proof version ${version}`);
  }
  const itemCount = proofIntegerV1(
    proof.item_count,
    "collection proof item count",
  );
  return {
    version,
    fieldIndex: proofIntegerV1(
      proof.field_index,
      "collection proof field index",
    ),
    itemCount,
    itemIndex: proofIntegerV1(proof.item_index, "collection proof item index"),
    itemLength: proofIntegerV1(
      proof.item_length,
      "collection proof item length",
    ),
    itemCommitment: proofHashV1(
      proof.item_commitment,
      "collection proof item commitment",
    ),
    frontier: {
      count: itemCount,
      peaks: proof.frontier.map((peak) => ({
        height: proofIntegerV1(peak.height, "collection proof peak height"),
        hash: proofHashV1(peak.hash, "collection proof peak hash"),
      })),
    },
    siblings: proof.siblings.map((sibling) =>
      proofHashV1(sibling, "collection proof sibling"),
    ),
  };
};

/**
 * Derives every consecutive field-0 opening from one complete canonical input
 * list. Callers expose the complete list, never fold indices or frontiers.
 */
export const buildInputNoIdxSpendInputFoldOpeningsV1 = (
  inputs: readonly MidgardTxInputData[],
): readonly InputNoIdxSpendInputFoldOpeningV1[] => {
  const inputCbors = inputs.map(encodeMidgardTxInputCanonicalV1);
  const collection = buildMidgardBoundedCollectionV1({
    fieldIndex: INPUT_NO_IDX_SPEND_INPUTS_FIELD_INDEX_V1,
    items: inputCbors,
  });
  return inputCbors.map((inputCbor, itemIndex) => ({
    inputCbor: inputCbor.toString("hex"),
    collectionProof: collectionProofFromCoreV1(
      buildMidgardBoundedCollectionItemProofV1(collection, itemIndex),
    ),
  }));
};

/**
 * Verifies an opening against a complete input list, including canonical item
 * bytes, ordered index, item commitment, and the collection commitment.
 */
export const verifyInputNoIdxSpendInputFoldOpeningV1 = ({
  inputs,
  opening,
}: {
  readonly inputs: readonly MidgardTxInputData[];
  readonly opening: InputNoIdxSpendInputFoldOpeningV1;
}): boolean => {
  try {
    const proof = collectionProofToCoreV1(opening.collectionProof);
    if (
      proof.fieldIndex !== INPUT_NO_IDX_SPEND_INPUTS_FIELD_INDEX_V1 ||
      proof.itemCount !== inputs.length ||
      proof.itemIndex >= inputs.length
    ) {
      return false;
    }
    const canonicalItem = encodeMidgardTxInputCanonicalV1(
      inputs[proof.itemIndex]!,
    );
    if (
      opening.inputCbor !== canonicalItem.toString("hex") ||
      proof.itemLength !== canonicalItem.length
    ) {
      return false;
    }
    const collection = buildMidgardBoundedCollectionV1({
      fieldIndex: INPUT_NO_IDX_SPEND_INPUTS_FIELD_INDEX_V1,
      items: inputs.map(encodeMidgardTxInputCanonicalV1),
    });
    if (
      !proof.itemCommitment.equals(
        collection.items[proof.itemIndex]!.commitment,
      )
    ) {
      return false;
    }
    return verifyMidgardBoundedCollectionItemProofV1({
      expectedCommitment: collection.commitment,
      proof,
    });
  } catch {
    return false;
  }
};

/**
 * The `outputs_hash` a native transaction body commits for `outputs`, derived
 * exactly as `bounded_collection_v1.from_items(2, ...)` does on-chain.
 */
export const inputNoIdxOutputsCommitmentV1 = (
  outputs: readonly MidgardTxOutput[],
): string =>
  buildMidgardBoundedCollectionV1({
    fieldIndex: INPUT_NO_IDX_OUTPUTS_FIELD_INDEX_V1,
    items: outputs.map(encodeMidgardTxOutputCanonicalV1),
  }).commitment.toString("hex");
