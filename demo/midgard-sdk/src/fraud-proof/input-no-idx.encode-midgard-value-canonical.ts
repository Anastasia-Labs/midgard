import {
  encodeCbor,
  encodeMidgardSpendInputItem,
  midgardFieldCommitmentFromItems,
} from "@al-ft/midgard-core";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { FieldOpeningSchema } from "./field-opening.js";
import {
  type InputNoIdxEvidence,
  InputNoIdxStep02State,
  InputNoIdxStep03State,
  InputNoIdxStep04State,
  MidgardAddress,
  MidgardCredential,
  MidgardScriptLanguage,
  MidgardTxOutput,
  MidgardValue,
  MidgardVersionedScript,
} from "./input-no-idx.input-no-idx-evidence-from-committed-transactions.js";
import {
  faultProofStepRedeemerSchema,
  type MidgardTxInput as MidgardTxInputData,
} from "./native.js";

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
  outputs_opening: FieldOpeningSchema,
});

export type InputNoIdxStep04Args = Data.Static<
  typeof InputNoIdxStep04ArgsSchema
>;

export const InputNoIdxStep04Args = asDataType<InputNoIdxStep04Args>(
  InputNoIdxStep04ArgsSchema,
);

export const InputNoIdxStep04SpendRedeemerSchema = faultProofStepRedeemerSchema(
  InputNoIdxStep04ArgsSchema,
);

export type InputNoIdxStep04SpendRedeemer = Data.Static<
  typeof InputNoIdxStep04SpendRedeemerSchema
>;

export const InputNoIdxStep04SpendRedeemer =
  asDataType<InputNoIdxStep04SpendRedeemer>(
    InputNoIdxStep04SpendRedeemerSchema,
  );

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
export const inputNoIdxStep02StateFromBadTx = (
  badTxId: string,
): InputNoIdxStep02State => ({
  verified_tx_id: badTxId.toLowerCase(),
});

/** Exactly the state `step-02` writes for `step-03`. */
export const inputNoIdxStep03StateFromEvidence = (
  evidence: InputNoIdxEvidence,
): InputNoIdxStep03State => ({
  bad_input_tx_id: evidence.badInput.tx_id,
  bad_input_output_index: evidence.badInput.output_index,
});

/**
 * Exactly the state `step-03` writes for `step-04`: the §2.5 anchor of the
 * *producing* transaction, alongside the challenged output index.
 */
export const inputNoIdxStep04StateFromEvidence = ({
  evidence,
  producingTxId,
}: {
  readonly evidence: InputNoIdxEvidence;
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
export const INPUT_NO_IDX_SPEND_INPUTS_FIELD_INDEX = 0;

/** Canonical outputs field index of a native V1 transaction body. */
export const INPUT_NO_IDX_OUTPUTS_FIELD_INDEX = 2;

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
export const encodeMidgardAddressCanonical = (
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
export const encodeMidgardValueCanonical = (value: MidgardValue): Buffer => {
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

const SCRIPT_LANGUAGE_TAG: Readonly<Record<MidgardScriptLanguage, number>> = {
  NativeCardanoScript: 0,
  PlutusV3Script: 3,
  MidgardV1Script: 128,
};

/** Twin of `encode_midgard_versioned_script`. */
export const encodeMidgardVersionedScriptCanonical = (
  script: MidgardVersionedScript,
): Buffer =>
  Buffer.concat([
    Buffer.from([0x82]),
    encodeCbor(BigInt(SCRIPT_LANGUAGE_TAG[script.language])),
    definiteBytes(Buffer.from(script.script_bytes, "hex")),
  ]);

/** Twin of `encode_midgard_tx_output`. */
export const encodeMidgardTxOutputCanonical = (
  output: MidgardTxOutput,
): Buffer => {
  const entryCount =
    2 +
    (output.datum_cbor === null ? 0 : 1) +
    (output.script_ref === null ? 0 : 1);
  return Buffer.concat([
    Buffer.from([0xa0 + entryCount, 0x00]),
    definiteBytes(encodeMidgardAddressCanonical(output.address)),
    Buffer.from([0x01]),
    encodeMidgardValueCanonical(output.value),
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
          encodeMidgardVersionedScriptCanonical(output.script_ref),
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
export const encodeMidgardTxInputCanonical = (
  input: MidgardTxInputData,
): Buffer =>
  encodeMidgardSpendInputItem({
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
export const inputNoIdxSpendInputsCommitment = (
  inputs: readonly MidgardTxInputData[],
): string =>
  midgardFieldCommitmentFromItems(
    inputs.map(encodeMidgardTxInputCanonical),
  ).toString("hex");
