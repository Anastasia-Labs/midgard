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
 *    forward its §2.5 anchor — the transaction **id**;
 * 2. open field 0 through the §8.8 door with the prover's chosen carriage and
 *    forward the challenged `(tx_id, output_index)`;
 * 3. bind the producing transaction to the *same* block and forward **its**
 *    anchor alongside the challenged index; and
 * 4. open that transaction's field 2 through the door and require
 *    `output_index >= |outputs|`, on the door's authenticated item count.
 *
 * This module is the strict TypeScript twin of
 * `onchain/aiken/lib/midgard/fraud-proofs/input-no-idx/step-0{1..4}.ak` and of
 * the `MidgardTxOutput` shape in
 * `onchain/aiken/lib/midgard/fraud-proofs/native-tx/types.ak`. Field order in
 * every `Data.Object` mirrors the aiken record declarations 1:1 — the
 * PlutusData encoding is positional, so re-ordering here would silently produce
 * redeemers the validators reject.
 *
 * **Re-derived onto the flat field commitments by #604** (the #575 off-chain
 * builder remediation). What moved is recorded once in
 * `docs/fault-proofs/offchain-builder-staleness-575.md`: thread state carries
 * the §2.5 anchor rather than a per-field collection commitment, and a step
 * redeemer carries a `FieldOpeningV1` rather than a reproduced
 * `..._preimage: List<…>`.
 */
import {
  encodeCbor,
  encodeMidgardSpendInputItemV1,
  midgardFieldCommitmentFromItemsV1,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema } from "@/common.js";

import { FieldOpeningV1Schema } from "./field-opening-v1.js";
import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  MidgardTxInput,
  type MidgardTxInput as MidgardTxInputData,
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

/**
 * Mirrors `midgard/fraud_proofs/input_no_idx/step_02.State` — **one state, one
 * route**.
 *
 * This step used to carry a two-constructor state (`Direct`/`Folding`) whose
 * folding arm streamed the spend-input collection one counted opening at a
 * time, and the state it carried was the field's *collection commitment*. Both
 * are gone. Under §4's flat scheme the thread carries the §2.5 anchor — the
 * transaction id — and the door hashes the preimage once and reads item `n` by
 * arithmetic, so there is nothing left to stream and nothing left to re-hash.
 * See `docs/fault-proofs/offchain-builder-staleness-575.md` §2, divergence 1.
 */
export const InputNoIdxStep02StateSchema = Data.Object({
  verified_tx_id: H32Schema,
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

/**
 * Mirrors `midgard/fraud_proofs/input_no_idx/step_02.Args` — a flat record, not
 * the retired four-arm enum.
 *
 * `Complete` reproduced the whole spend-input list in the redeemer,
 * `CompletePublished` referenced a bespoke `PublishedSpendInputsV1` datum, and
 * `FoldStart`/`FoldNext` streamed the collection with per-item counted proofs.
 * All four existed because the collection had to be reproduced inside the step
 * to re-hash it. The §8.8 door replaced every one of them with a single
 * `FieldOpeningV1` naming one of §8's three carriage tiers, so this step now has
 * exactly one route and the prover's only remaining choice is *how the preimage
 * travels* — which is what `spend_inputs_opening` carries.
 *
 * See `docs/fault-proofs/offchain-builder-staleness-575.md` §2, divergence 2: a
 * builder still emitting a `..._preimage` argument produces a constructor arity
 * the validator cannot decode.
 */
export const InputNoIdxStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  spend_inputs_opening: FieldOpeningV1Schema,
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

/**
 * Mirrors `midgard/fraud_proofs/input_no_idx/step_04.State`.
 *
 * `producing_tx_outputs_hash` became `producing_tx_id` with the rest of
 * divergence 1: step-04 opens the *producing* transaction's field 2 through the
 * door, so what it needs forwarded is that transaction's §2.5 anchor, not a
 * commitment it would have to re-derive a reproduced output list against.
 */
export const InputNoIdxStep04StateSchema = Data.Object({
  producing_tx_id: H32Schema,
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

/**
 * Mirrors `midgard/fraud_proofs/input_no_idx/step_04.Args`.
 *
 * `outputs_preimage` became `outputs_opening`: the producing transaction's
 * field-2 preimage travels under one of §8's carriage tiers instead of being
 * reproduced as a `List<MidgardTxOutput>` in the redeemer. Its authenticated
 * item count is the output count the out-of-range verdict rests on (§5.2).
 */
export const InputNoIdxStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  outputs_opening: FieldOpeningV1Schema,
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

/**
 * Exactly the state `step-01` writes for `step-02`: the §2.5 anchor of the
 * transaction the thread is disputing.
 *
 * The argument is the transaction **id**, not its spend-inputs hash. Step-01
 * reads it off the compact structure the block's `transactions_root` committed,
 * which is the only provenance `BodyAnchor` accepts — anything a later redeemer
 * supplies is the prover's own and anchors nothing.
 */
export const inputNoIdxStep02StateFromBadTxV1 = (
  badTxId: string,
): InputNoIdxStep02State => ({
  verified_tx_id: badTxId.toLowerCase(),
});

/** Exactly the state `step-02` writes for `step-03`. */
export const inputNoIdxStep03StateFromEvidenceV1 = (
  evidence: InputNoIdxEvidenceV1,
): InputNoIdxStep03State => ({
  bad_input_tx_id: evidence.badInput.tx_id,
  bad_input_output_index: evidence.badInput.output_index,
});

/**
 * Exactly the state `step-03` writes for `step-04`: the §2.5 anchor of the
 * *producing* transaction, alongside the challenged output index.
 */
export const inputNoIdxStep04StateFromEvidenceV1 = ({
  evidence,
  producingTxId,
}: {
  readonly evidence: InputNoIdxEvidenceV1;
  readonly producingTxId: string;
}): InputNoIdxStep04State => ({
  producing_tx_id: producingTxId.toLowerCase(),
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

/**
 * Twin of `encode_midgard_tx_input`: the §5.3 field-0/1 item form
 * `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, a FIXED 38 bytes. This is NOT CML's
 * minimal-index `TransactionInput` CBOR — the non-minimal 3-byte index is what
 * makes the item width constant, and `decode_midgard_tx_input_cbor` requires
 * the `0x19` head. Delegating to the core twin keeps the two in lockstep.
 */
export const encodeMidgardTxInputCanonicalV1 = (
  input: MidgardTxInputData,
): Buffer =>
  encodeMidgardSpendInputItemV1({
    txId: Buffer.from(input.tx_id, "hex"),
    outputIndex: Number(input.output_index),
  });

/**
 * The `spend_inputs_hash` a native transaction body commits for `inputs`:
 * `docs/spec/midgard-tx.md` §4's flat `blake2b_256` over the §5.1 preimage the
 * items assemble into, which is what `native_tx_field_access_v1.field_commitment`
 * computes on-chain.
 *
 * §4's hash input carries no field index, so this is *not* specific to field 0 —
 * an identical reference-input list commits to the same value. Field identity is
 * positional, and the §4 positional-identity invariant is what keeps that safe:
 * the caller compares against `body.spend_inputs_hash` from the committed compact
 * structure, never against a free-standing argument.
 */
export const inputNoIdxSpendInputsCommitmentV1 = (
  inputs: readonly MidgardTxInputData[],
): string =>
  midgardFieldCommitmentFromItemsV1(
    inputs.map(encodeMidgardTxInputCanonicalV1),
  ).toString("hex");

// ## The retired counted fold family
//
// `InputNoIdxSpendInputFoldOpeningV1`, `buildInputNoIdxSpendInputFoldOpeningsV1`
// and `verifyInputNoIdxSpendInputFoldOpeningV1` lived here and are **deleted**,
// not re-pointed. They published per-item openings against the counted
// bounded-collection Merkle root, and §4 gives a field one flat hash with no
// per-item openings at all — so the flat rebind did not move them to a new
// commitment, it deleted the concept they published. Their replacement is the
// §8.8 door (`FieldOpeningV1` + one of §8's three carriage tiers), which
// step-02's `Args` now names directly.
//
// The comment that stood here recorded that the swap could not be made in that
// lane because it would move the `fraud_proofs/input_no_idx/step_02` redeemer
// shape. #575 has since moved exactly that shape on-chain, and #604 is the
// off-chain half following it.

/**
 * The `outputs_hash` a native transaction body commits for `outputs`: §4's flat
 * `blake2b_256` over the §5.1 preimage the items assemble into.
 */
export const inputNoIdxOutputsCommitmentV1 = (
  outputs: readonly MidgardTxOutput[],
): string =>
  midgardFieldCommitmentFromItemsV1(
    outputs.map(encodeMidgardTxOutputCanonicalV1),
  ).toString("hex");
