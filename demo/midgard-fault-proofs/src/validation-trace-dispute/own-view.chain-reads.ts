/** What the chain own view (own-view.chain.ts) decodes from the captured
 * transactions of our resolve moves: who signed one, and what it read at
 * our staging address. */
import {
  midgardCekStagingRunChunks,
  midgardCekStagingRunDatumCbor,
} from "@al-ft/midgard-sdk/fraud-proof";
import { CML } from "@lucid-evolution/lucid";

import { rawToUtxo } from "../workflow/raw-l1-family-derivation.state-queue-topology.js";
import type {
  FraudProofRawL1Transaction,
  FraudProofRawL1Utxo,
} from "../workflow/raw-l1-snapshot.js";

/** An output a resolve move of ours read: whether it is at our staging
 * address and, there, its inline datum and the run it stages, with the
 * run's datum in the bytes a staging output holds (those of the run's
 * encoder, from its chunks); `run` is undefined when it stages none. */
export type ValidationTraceReferenced = Readonly<{
  raw: FraudProofRawL1Utxo;
  ours: boolean;
  datum: string | undefined;
  run: string | undefined;
}>;

/** `raw` as read by a resolve move of ours, our staging address being
 * `stagingAddress`. */
export const validationTraceReferenced = (
  raw: FraudProofRawL1Utxo,
  stagingAddress: string,
): ValidationTraceReferenced => {
  const utxo = rawToUtxo(raw);
  const ours = utxo.address === stagingAddress;
  const datum = ours && typeof utxo.datum === "string" ? utxo.datum : undefined;
  const chunks = datum === undefined ? null : midgardCekStagingRunChunks(datum);
  return Object.freeze({
    raw,
    ours,
    datum,
    run: chunks === null ? undefined : midgardCekStagingRunDatumCbor(chunks),
  });
};

/** Whether the datums `left` and `right` are one datum, however encoded. */
export const sameDatum = (left: string, right: string): boolean => {
  const canonical = (cbor: string) => {
    const data = CML.PlutusData.from_cbor_hex(cbor);
    try {
      return data.to_canonical_cbor_hex();
    } finally {
      data.free();
    }
  };
  return left === right || canonical(left) === canonical(right);
};

/** Whether the key `keyHash` signed `transaction`. */
export const signedBy = (
  transaction: FraudProofRawL1Transaction,
  keyHash: string,
): boolean => {
  const witnesses = CML.TransactionWitnessSet.from_cbor_hex(
    transaction.witnessSetCbor,
  );
  const keys = witnesses.vkeywitnesses();
  try {
    for (let index = 0; index < (keys?.len() ?? 0); index += 1) {
      const witness = keys!.get(index);
      const vkey = witness.vkey();
      const hash = vkey.hash();
      const found = hash.to_hex() === keyHash;
      hash.free();
      vkey.free();
      witness.free();
      if (found) return true;
    }
    return false;
  } finally {
    keys?.free();
    witnesses.free();
  }
};
