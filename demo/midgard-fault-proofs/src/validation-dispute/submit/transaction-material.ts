import { CML, coreToTxOutput, type UTxO } from "@lucid-evolution/lucid";

import {
  MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES,
  selectValidationCompleteItemCarriage,
} from "./validity.js";

export const threadAssets = (threadUtxo: UTxO, threadUnit: string) => ({
  lovelace: threadUtxo.assets.lovelace ?? 0n,
  [threadUnit]: 1n,
});

export const requireL1ProofEnvelope = (
  transactionCbor: string,
  label: string,
): void => {
  const bytes = transactionCbor.length / 2;
  if (
    transactionCbor.length % 2 !== 0 ||
    !/^[0-9a-f]+$/u.test(transactionCbor) ||
    bytes > MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES
  ) {
    throw new Error(
      `${label} transaction is ${bytes.toString()} bytes; the complete signed L1 proof transaction must be no larger than ${MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES.toString()} bytes`,
    );
  }
};

/**
 * A deterministic, known-valid ed25519 public key (CML's own documentation
 * example), used only to project a signed transaction's exact byte length
 * before any signature exists. A vkey witness's size is fixed — 32 key bytes
 * plus 64 signature bytes plus CBOR framing — so which key fills the slot
 * cannot change the projection.
 */
const PROJECTION_PLACEHOLDER_VKEY_BECH32 =
  "ed25519_pk1dgaagyh470y66p899txcl3r0jaeaxu6yd7z2dxyk55qcycdml8gszkxze2";

/**
 * Projects the exact byte length the unsigned transaction will have once the
 * prover's signature is attached, without signing anything (#621).
 *
 * The inline observe route decides pre-sign whether its redeemer-carried
 * preimage still fits the L1 proof envelope; deciding on the unsigned bytes
 * alone would under-count by one vkey witness, and guessing a delta is a
 * measurement smell. So the witness slot is filled with a placeholder key and
 * a zeroed 64-byte signature — byte-for-byte the size of the real ones — and
 * the assembled transaction is measured. `requireL1ProofEnvelope` still runs
 * on the actually-signed bytes afterwards, so a projection defect can never
 * ship an oversized transaction; the projection only decides routing.
 */
export const projectSignedL1ProofTransactionBytes = (
  unsignedTransactionCbor: string,
): number => {
  const transaction = CML.Transaction.from_cbor_hex(unsignedTransactionCbor);
  const witnessSet = transaction.witness_set();
  const vkeyWitnesses = CML.VkeywitnessList.new();
  vkeyWitnesses.add(
    CML.Vkeywitness.new(
      CML.PublicKey.from_bech32(PROJECTION_PLACEHOLDER_VKEY_BECH32),
      CML.Ed25519Signature.from_raw_bytes(new Uint8Array(64)),
    ),
  );
  witnessSet.set_vkeywitnesses(vkeyWitnesses);
  const assembled = CML.Transaction.new(
    transaction.body(),
    witnessSet,
    transaction.is_valid(),
    transaction.auxiliary_data(),
  );
  return assembled.to_cbor_hex().length / 2;
};

/**
 * The pre-sign refusal of an inline observe build whose projected signed
 * bytes exceed the L1 proof envelope (#621). The staged-chain orchestration
 * catches exactly this error to fall back to the reference route, so it is a
 * distinct class rather than a message pattern.
 */
export class ValidationInlineDeliveryEnvelopeRefusedError extends Error {
  readonly projectedSignedBytes: number;
  readonly maxTransactionBytes: number;

  constructor({
    label,
    projectedSignedBytes,
    maxTransactionBytes,
  }: {
    readonly label: string;
    readonly projectedSignedBytes: number;
    readonly maxTransactionBytes: number;
  }) {
    super(
      `${label} would sign at ${projectedSignedBytes.toString()} bytes, over the ` +
        `${maxTransactionBytes.toString()}-byte L1 proof envelope; refusing pre-sign. ` +
        "Inline delivery carries the §5.1 preimage in the observe redeemer, so an item " +
        "this large rides the §8 publication route instead.",
    );
    this.projectedSignedBytes = projectedSignedBytes;
    this.maxTransactionBytes = maxTransactionBytes;
  }
}

/** How a tier-1 complete item's preimage reaches the §8.8 door (#621). */
export type ValidationProofItemDelivery = "inline" | "reference";

/**
 * Resolves the delivery route for the CanonicalDecode complete-item path at
 * build time (#619/#621).
 *
 * Since Option B the committed evidence is transition-only, so nothing staged
 * on chain constrains how the preimage reaches the observe stage's §8.8 door:
 * inline in the redeemer or by reference to a §8 proof-item publication, both
 * of which the door authenticates by content. The choice is therefore a
 * builder-local cost decision, resolved here in precedence order — an
 * explicit `proofItemDelivery` request, then a supplied publication out-ref,
 * then the measured `selectValidationCompleteItemCarriage` heuristic.
 *
 * The routing choice exists only inside tier 1. Above §8.3's tier-1 cap the
 * §8.4 partition already names reference inputs of its own, so a delivery
 * request there is a refusal, not a preference — and no routing input can
 * brick an in-flight dispute: an inline build that outgrows the L1 envelope
 * falls back to the reference route pre-sign
 * ({@link ValidationInlineDeliveryEnvelopeRefusedError}).
 *
 * Returns the tier-1 route, or `undefined` when the argument is not a tier-1
 * complete item (tiers 2-3 and every other resolver path, where no such route
 * exists).
 */
export const resolveValidationProofItemDeliveryRoute = ({
  requestedDelivery,
  hasProofItemReferenceOutRef,
  committedCarriage,
  preimageByteLength,
}: {
  readonly requestedDelivery: ValidationProofItemDelivery | undefined;
  readonly hasProofItemReferenceOutRef: boolean;
  /**
   * The staged auxiliary's §8.1 carriage constructor on the complete-item
   * path, `undefined` on every other resolver path.
   */
  readonly committedCarriage: "Inline" | "RawUtxo" | "Certified" | undefined;
  /** The tier-1 preimage's byte length; required when the carriage is `Inline`. */
  readonly preimageByteLength?: number;
}): ValidationProofItemDelivery | undefined => {
  if (committedCarriage === undefined) {
    if (requestedDelivery !== undefined) {
      throw new Error(
        "Validation proof-item delivery routing (`proofItemDelivery`) exists only on the " +
          "CanonicalDecode complete-item path (#621)",
      );
    }
    return undefined;
  }
  if (committedCarriage !== "Inline") {
    if (requestedDelivery !== undefined) {
      throw new Error(
        "Validation proof-item delivery routing is a tier-1 choice between the observe " +
          `redeemer and the §8 publication; tier-${committedCarriage === "RawUtxo" ? "2" : "3"} ` +
          `\`${committedCarriage}\` already names reference inputs (§8.4) and admits no ` +
          "routing override (#621)",
      );
    }
    return undefined;
  }
  if (requestedDelivery === "inline") {
    if (hasProofItemReferenceOutRef) {
      throw new Error(
        'Validation proof-item delivery "inline" contradicts `proofItemReferenceOutRef`: ' +
          "inline delivery carries the preimage in the observe redeemer and reads no " +
          "publication (#621)",
      );
    }
    return "inline";
  }
  if (requestedDelivery === "reference" || hasProofItemReferenceOutRef) {
    return "reference";
  }
  if (preimageByteLength === undefined) {
    throw new Error(
      "Validation proof-item delivery route needs the tier-1 preimage length to apply the " +
        "measured cost heuristic",
    );
  }
  return selectValidationCompleteItemCarriage(preimageByteLength) === "direct"
    ? "inline"
    : "reference";
};

export const findUniqueInlineDatumOutputIndex = ({
  transactionCbor,
  address,
  datum,
  label,
}: {
  readonly transactionCbor: string;
  readonly address: string;
  readonly datum: string;
  readonly label: string;
}): number => {
  const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  const outputs = transaction.body().outputs();
  const matches: number[] = [];
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    if (output.address === address && output.datum === datum) {
      matches.push(index);
    }
  }
  if (matches.length !== 1) {
    throw new Error(
      `${label} must contain exactly one matching inline-datum output; found ${matches.length.toString()}`,
    );
  }
  return matches[0]!;
};
