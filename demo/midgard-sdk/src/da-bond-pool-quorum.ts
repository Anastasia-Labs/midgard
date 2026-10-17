import { CML } from "@lucid-evolution/lucid";

import { DaAvailabilityCommitmentError } from "./availability-challenge.js";
import type { DaParamsDatum } from "./da-attestation.js";
import { DaBondPoolBuildError } from "./da-bond-pool-transactions.js";

/**
 * Offline owner-quorum primitives for the pool's withdrawal steps
 * (`BeginWithdraw`, `CancelWithdraw`, `CompleteWithdraw`). The builder's
 * unsigned transaction travels as CBOR hex; each DA params owner witnesses it
 * with only their own key, possibly on another machine
 * (`witnessDaBondPoolTx`); the coordinator checks the witnesses against the
 * owner quorum (`assertDaBondPoolWitnessQuorum`) and merges them into the
 * transaction (`assembleDaBondPoolTx`) before submitting it. Nothing here
 * needs a Lucid instance or a provider.
 */

type Freeable = { free: () => void };

/** Frees every CML object `own` registered, whatever `run` returns. */
const scoped = <A>(run: (own: <T extends Freeable>(value: T) => T) => A): A => {
  const owned: Freeable[] = [];
  try {
    return run((value) => {
      owned.push(value);
      return value;
    });
  } finally {
    for (const value of owned.reverse()) {
      value.free();
    }
  }
};

const CBOR_HEX = /^(?:[0-9a-f]{2})+$/u;
/** A Conway transaction is a definite four-element array. */
const TRANSACTION_ARRAY_HEADER = "84";

const witnessRefusal = (
  message: string,
  cause: unknown = undefined,
): DaBondPoolBuildError =>
  new DaBondPoolBuildError({ reason: "invalid_witness", message, cause });

const decodeTransaction = (
  own: <T extends Freeable>(value: T) => T,
  txCbor: string,
): { readonly tx: CML.Transaction; readonly bodyHex: string } => {
  const hex = txCbor.toLowerCase();
  if (!CBOR_HEX.test(hex)) {
    throw new DaAvailabilityCommitmentError(
      "DA bond pool transaction must be non-empty CBOR hex",
    );
  }
  let tx: CML.Transaction;
  try {
    tx = own(CML.Transaction.from_cbor_hex(hex));
  } catch (error) {
    throw new DaAvailabilityCommitmentError(
      `DA bond pool transaction CBOR does not decode: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
  const bodyHex = own(tx.body()).to_cbor_hex();
  // The ledger hashes the body bytes exactly as submitted. Refuse a
  // transaction whose body CML would re-encode, so the hash every witness
  // signs is the hash of these bytes.
  if (
    !hex.startsWith(TRANSACTION_ARRAY_HEADER) ||
    hex.slice(
      TRANSACTION_ARRAY_HEADER.length,
      TRANSACTION_ARRAY_HEADER.length + bodyHex.length,
    ) !== bodyHex
  ) {
    throw new DaAvailabilityCommitmentError(
      "DA bond pool transaction body does not round-trip byte for byte",
    );
  }
  return { tx, bodyHex };
};

const bodyHashOf = (
  own: <T extends Freeable>(value: T) => T,
  tx: CML.Transaction,
): CML.TransactionHash => own(CML.hash_transaction(own(tx.body())));

/** The transaction id: blake2b-256 of the body bytes, as hex. */
export const daBondPoolTxBodyHash = (txCbor: string): string =>
  scoped((own) => bodyHashOf(own, decodeTransaction(own, txCbor).tx).to_hex());

/** The body's `required_signers`, as sorted, distinct, lowercase hex. */
export const daBondPoolTxRequiredSigners = (
  txCbor: string,
): readonly string[] =>
  scoped((own) => {
    const { tx } = decodeTransaction(own, txCbor);
    const required = own(tx.body()).required_signers();
    const signers = new Set<string>();
    if (required !== undefined) {
      own(required);
      for (let index = 0; index < required.len(); index += 1) {
        signers.add(own(required.get(index)).to_hex().toLowerCase());
      }
    }
    return [...signers].sort();
  });

/**
 * One owner's witness: a witness set holding exactly one vkey witness, the
 * key's signature over the body hash. Accepts `ed25519_sk` and
 * `ed25519e_sk` bech32 keys. Error messages never carry the key.
 */
export const witnessDaBondPoolTx = (
  txCbor: string,
  privateKeyBech32: string,
): Readonly<{ keyHash: string; witnessSetCbor: string }> =>
  scoped((own) => {
    const { tx } = decodeTransaction(own, txCbor);
    if (
      !privateKeyBech32.startsWith("ed25519_sk1") &&
      !privateKeyBech32.startsWith("ed25519e_sk1")
    ) {
      throw new DaAvailabilityCommitmentError(
        "DA bond pool witness key must be an ed25519_sk or ed25519e_sk bech32 private key",
      );
    }
    let privateKey: CML.PrivateKey;
    try {
      privateKey = own(CML.PrivateKey.from_bech32(privateKeyBech32));
    } catch {
      throw new DaAvailabilityCommitmentError(
        "DA bond pool witness key does not decode as a bech32 private key",
      );
    }
    const witness = own(CML.make_vkey_witness(bodyHashOf(own, tx), privateKey));
    const witnesses = own(CML.VkeywitnessList.new());
    witnesses.add(witness);
    const witnessSet = own(CML.TransactionWitnessSet.new());
    witnessSet.set_vkeywitnesses(witnesses);
    return {
      keyHash: own(own(privateKey.to_public()).hash()).to_hex(),
      witnessSetCbor: witnessSet.to_cbor_hex(),
    };
  });

/**
 * The vkey witnesses in `witnessSetCbor`, each verified against `bodyHash`,
 * as `[vkey hex, key hash hex, witness]`. Throws `invalid_witness` on a
 * witness set that does not decode or any signature that does not verify.
 */
const verifiedVkeyWitnesses = (
  own: <T extends Freeable>(value: T) => T,
  bodyHash: Uint8Array,
  witnessSetCbor: string,
): readonly (readonly [string, string, CML.Vkeywitness])[] => {
  if (!CBOR_HEX.test(witnessSetCbor.toLowerCase())) {
    throw witnessRefusal("DA bond pool witness set must be CBOR hex");
  }
  let witnessSet: CML.TransactionWitnessSet;
  try {
    witnessSet = own(
      CML.TransactionWitnessSet.from_cbor_hex(witnessSetCbor.toLowerCase()),
    );
  } catch (error) {
    throw witnessRefusal("DA bond pool witness set does not decode", error);
  }
  const vkeys = witnessSet.vkeywitnesses();
  if (vkeys === undefined) {
    return [];
  }
  own(vkeys);
  const verified: (readonly [string, string, CML.Vkeywitness])[] = [];
  for (let index = 0; index < vkeys.len(); index += 1) {
    const witness = own(vkeys.get(index));
    const vkey = own(witness.vkey());
    const keyHash = own(vkey.hash()).to_hex().toLowerCase();
    if (!vkey.verify(bodyHash, own(witness.ed25519_signature()))) {
      throw witnessRefusal(
        "DA bond pool witness signature does not verify against the transaction body",
        keyHash,
      );
    }
    verified.push([
      Buffer.from(vkey.to_raw_bytes()).toString("hex"),
      keyHash,
      witness,
    ]);
  }
  return verified;
};

/**
 * The key hashes of every vkey witness in `witnessSetCbor`, after verifying
 * each signature against the body hash of `txCbor`. Throws
 * `DaBondPoolBuildError` `invalid_witness` on a witness set that does not
 * decode or any bad signature.
 */
export const verifiedDaBondPoolWitnessKeyHashes = (
  txCbor: string,
  witnessSetCbor: string,
): readonly string[] =>
  scoped((own) => {
    const { tx } = decodeTransaction(own, txCbor);
    const bodyHash = bodyHashOf(own, tx).to_raw_bytes();
    return verifiedVkeyWitnesses(own, bodyHash, witnessSetCbor).map(
      ([, keyHash]) => keyHash,
    );
  });

/** AC1's refusal: the CLI prints exactly this and submits nothing. */
export const daBondPoolQuorumShortfallMessage = (
  have: number,
  threshold: bigint,
): string =>
  `Refusing to submit: ${have.toString()} distinct DA params owner witness(es), update_threshold is ${threshold.toString()}; nothing was submitted`;

/**
 * Checks, before submission, that the witnesses carry the owner quorum the
 * pool validator counts and every signature the ledger needs:
 *
 * 1. every witness set decodes and every signature verifies
 *    (`invalid_witness`);
 * 2. the DISTINCT owners that witnessed AND are body required signers reach
 *    `update_threshold` (`insufficient_signers`, with
 *    `daBondPoolQuorumShortfallMessage`). The validator counts owners among
 *    the transaction's signatories, which are the body's `required_signers`,
 *    so an owner witness the body does not list never counts, nor does a
 *    non-owner or a second witness from the same owner;
 * 3. every body required signer and every `requiredKeyHashes` entry (the fee
 *    payer's key hash, say) has a verified witness (`missing_witness`).
 *
 * Returns the counted owners and every verified witness key hash, both
 * sorted and distinct.
 */
export const assertDaBondPoolWitnessQuorum = (input: {
  readonly txCbor: string;
  readonly witnessSetCbors: readonly string[];
  readonly daParams: Pick<DaParamsDatum, "owners" | "update_threshold">;
  readonly requiredKeyHashes?: readonly string[];
}): Readonly<{
  ownerKeyHashes: readonly string[];
  witnessKeyHashes: readonly string[];
}> => {
  const witnessed = new Set<string>();
  for (const witnessSetCbor of input.witnessSetCbors) {
    for (const keyHash of verifiedDaBondPoolWitnessKeyHashes(
      input.txCbor,
      witnessSetCbor,
    )) {
      witnessed.add(keyHash);
    }
  }
  const bodySigners = daBondPoolTxRequiredSigners(input.txCbor);
  const owners = new Set(input.daParams.owners.map((key) => key.toLowerCase()));
  const ownerKeyHashes = [...witnessed]
    .filter((keyHash) => owners.has(keyHash) && bodySigners.includes(keyHash))
    .sort();
  if (BigInt(ownerKeyHashes.length) < input.daParams.update_threshold) {
    throw new DaBondPoolBuildError({
      reason: "insufficient_signers",
      message: daBondPoolQuorumShortfallMessage(
        ownerKeyHashes.length,
        input.daParams.update_threshold,
      ),
      cause: `owners_witnessed=${ownerKeyHashes.join(",")}`,
    });
  }
  const required = new Set([
    ...bodySigners,
    ...(input.requiredKeyHashes ?? []).map((key) => key.toLowerCase()),
  ]);
  const missing = [...required].filter((key) => !witnessed.has(key)).sort();
  if (missing.length > 0) {
    throw new DaBondPoolBuildError({
      reason: "missing_witness",
      message: `DA bond pool transaction lacks a witness for required signer(s) ${missing.join(", ")}`,
      cause: missing,
    });
  }
  return { ownerKeyHashes, witnessKeyHashes: [...witnessed].sort() };
};

/**
 * Merges the vkey witnesses of `witnessSetCbors` into the transaction's own
 * witness set and returns the transaction CBOR. The body bytes, the redeemers,
 * the plutus data, the scripts and every other witness field carry over
 * untouched, so the result's body hash is `daBondPoolTxBodyHash(txCbor)`.
 * Each witness is verified first (`invalid_witness`); a vkey already present
 * is kept once.
 */
export const assembleDaBondPoolTx = (
  txCbor: string,
  witnessSetCbors: readonly string[],
): string =>
  scoped((own) => {
    const { tx, bodyHex } = decodeTransaction(own, txCbor);
    const bodyHash = bodyHashOf(own, tx).to_raw_bytes();
    const witnessSet = own(tx.witness_set());
    const merged = own(CML.VkeywitnessList.new());
    const seen = new Set<string>();
    const existing = witnessSet.vkeywitnesses();
    if (existing !== undefined) {
      own(existing);
      for (let index = 0; index < existing.len(); index += 1) {
        const witness = own(existing.get(index));
        const vkey = Buffer.from(own(witness.vkey()).to_raw_bytes()).toString(
          "hex",
        );
        if (!seen.has(vkey)) {
          seen.add(vkey);
          merged.add(witness);
        }
      }
    }
    for (const witnessSetCbor of witnessSetCbors) {
      for (const [vkey, , witness] of verifiedVkeyWitnesses(
        own,
        bodyHash,
        witnessSetCbor,
      )) {
        if (!seen.has(vkey)) {
          seen.add(vkey);
          merged.add(witness);
        }
      }
    }
    witnessSet.set_vkeywitnesses(merged);
    const auxiliaryData = tx.auxiliary_data();
    // Transaction.new consumes the auxiliary data, so it is not registered.
    const assembled = own(
      CML.Transaction.new(
        own(tx.body()),
        witnessSet,
        tx.is_valid(),
        auxiliaryData,
      ),
    ).to_cbor_hex();
    // decodeTransaction pins each body to its raw bytes, so equal body hex
    // means the assembled transaction carries the original body bytes.
    if (decodeTransaction(own, assembled).bodyHex !== bodyHex) {
      throw new DaAvailabilityCommitmentError(
        "DA bond pool assembly changed the transaction body",
      );
    }
    return assembled;
  });
