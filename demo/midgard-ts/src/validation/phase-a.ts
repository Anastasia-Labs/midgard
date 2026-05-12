/**
 * Phase A - stateless / structural transaction validation.
 *
 * Each transaction is validated independently; no UTxO state is required.
 * All checks are synchronous and safe to run in parallel.
 *
 * Rules enforced here:
 *   R2  - tx hash integrity
 *   R3  - unsupported fields absent (redeemers, plutus_v3_scripts, script_data_hash)
 *   R4  - at least one spend input
 *   R5  - no duplicate inputs within the transaction
 *   R6  - output structure validity (non-negative coin)
 *   R9  - validity interval well-formed
 *   R11 - minimum fee
 *   R13 - required signers have corresponding vkey witnesses
 *   R14 - vkey witness signatures verify against tx body hash
 *   R15 - native scripts present and valid
 *   R19 - is_valid must be true
 *   R20 - no auxiliary data hash
 *   R23 - no minting
 *   R24 - network_id matches config
 *
 * Rules implicitly satisfied by the Transaction type (no dedicated check):
 *   R21 - no certificates (Transaction has no certificates field)
 *   R22 - no non-zero withdrawals (stored as required_observers; zero-ADA
 *          withdrawals are the withdraw-zero trick and are allowed)
 */

import * as CML from "@dcspark/cardano-multiplatform-lib-nodejs";
import { midgardToCml, midgardValueToCml } from "../cardano";
import { encodeTransaction } from "../index";
import { OutputReference } from "../types/primitives";
import {
  PhaseAAccepted,
  PhaseAConfig,
  PhaseAResult,
  ProducedEntry,
  QueuedTx,
  RejectCode,
  RejectCodes,
  RejectedTx,
} from "./types";

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

const reject = (
  txId: Uint8Array,
  code: RejectCode,
  detail: string | null = null,
): RejectedTx => ({ txId, code, detail });

/**
 * Canonical string key for an OutputReference.
 * Format: `<txid-hex>:<index>`  e.g. `"aabb...00:0"`.
 */
export function outRefKey(ref: OutputReference): string {
  return Buffer.from(ref.tx_id).toString("hex") + ":" + ref.index;
}

// ---------------------------------------------------------------------------
// Per-transaction validation
// ---------------------------------------------------------------------------

function validateOne(
  queuedTx: QueuedTx,
  config: PhaseAConfig,
): PhaseAAccepted | RejectedTx {
  const { txId, tx } = queuedTx;
  const body = tx.body;
  const net = config.cardanoNetwork ?? 1;

  // R2 - tx hash integrity
  // TODO: In theory I can just hash the Midgard TransactionBody with black2b-256 to achieve the same thing, no? The conversion to CML is done here just to be sure, but most likely should be dropped.
  let txBodyHash: Uint8Array;
  try {
    const cmlTx = midgardToCml(tx, net);
    txBodyHash = CML.hash_transaction(cmlTx.body()).to_raw_bytes();
  } catch (e) {
    return reject(
      txId,
      RejectCodes.TxHashMismatch,
      `hash computation failed: ${String(e)}`,
    );
  }
  if (
    txBodyHash.length !== txId.length ||
    !txBodyHash.every((b, i) => b === txId[i])
  ) {
    return reject(
      txId,
      RejectCodes.TxHashMismatch,
      `provided ${Buffer.from(txId).toString("hex")} != computed ${Buffer.from(txBodyHash).toString("hex")}`,
    );
  }

  // R19 - is_valid must be true
  if (!tx.is_valid) {
    return reject(txId, RejectCodes.IsValidFalseForbidden);
  }

  // R20 - no auxiliary data hash
  if (body.auxiliary_data_hash !== undefined) {
    return reject(
      txId,
      RejectCodes.AuxDataForbidden,
      "auxiliary_data_hash must be absent",
    );
  }

  // R23 - no minting
  // TODO: R23 from the tx-submission says you can not mint? You should be though, no?
  if (body.mint !== undefined && body.mint.length > 0) {
    return reject(
      txId,
      RejectCodes.MintForbidden,
      "mint field must be absent or empty",
    );
  }

  // R3 - unsupported witness-set fields
  // TODO: R3 from the tx-submission says plutus_v3_scripts is unsupported? You should be able to run the plutus_v3_scripts though, no?
  if (tx.witness_set.redeemers !== undefined) {
    return reject(txId, RejectCodes.UnsupportedFieldNonEmpty, "redeemers");
  }
  if (
    tx.witness_set.plutus_v3_scripts !== undefined &&
    tx.witness_set.plutus_v3_scripts.length > 0
  ) {
    return reject(
      txId,
      RejectCodes.UnsupportedFieldNonEmpty,
      "plutus_v3_scripts",
    );
  }

  // R3 - unsupported body fields
  if (body.script_data_hash !== undefined) {
    return reject(
      txId,
      RejectCodes.UnsupportedFieldNonEmpty,
      "script_data_hash",
    );
  }

  // R24 - network_id must match config when present
  if (
    body.network_id !== undefined &&
    body.network_id !== config.expectedNetworkId
  ) {
    return reject(
      txId,
      RejectCodes.NetworkIdMismatch,
      `${body.network_id} != ${config.expectedNetworkId}`,
    );
  }

  // R11 - minimum fee
  const encodedLen = encodeTransaction(tx).length;
  const minFee = config.minFeeA * BigInt(encodedLen) + config.minFeeB;
  if (body.fee < minFee) {
    return reject(txId, RejectCodes.MinFee, `${body.fee} < ${minFee}`);
  }

  // R4 - at least one spend input
  if (body.inputs.length === 0) {
    return reject(txId, RejectCodes.EmptyInputs);
  }

  // R5 - no duplicate inputs within the transaction
  const seenInputs = new Set<string>();
  for (const input of body.inputs) {
    const key = outRefKey(input);
    if (seenInputs.has(key)) {
      return reject(txId, RejectCodes.DuplicateInputInTx, key);
    }
    seenInputs.add(key);
  }

  // R6 - output structure validity: non-negative coin, sum values
  let outputSum = CML.Value.zero();
  const produced: ProducedEntry[] = [];
  for (let i = 0; i < body.outputs.length; i++) {
    const output = body.outputs[i];
    if (output.value.coin < 0n) {
      return reject(
        txId,
        RejectCodes.InvalidOutput,
        `negative coin in output ${i}`,
      );
    }
    try {
      outputSum = outputSum.checked_add(midgardValueToCml(output.value));
    } catch (e) {
      return reject(
        txId,
        RejectCodes.InvalidOutput,
        `output ${i} value error: ${String(e)}`,
      );
    }
    produced.push({ outRef: { tx_id: txId, index: i }, output });
  }

  // R9 - validity interval well-formed
  const validityIntervalStart = body.validity_interval_start;
  const validityIntervalEnd = body.ttl;
  if (validityIntervalStart !== undefined && validityIntervalStart < 0) {
    return reject(
      txId,
      RejectCodes.InvalidValidityIntervalFormat,
      "validity_interval_start must be non-negative",
    );
  }
  if (validityIntervalEnd !== undefined && validityIntervalEnd < 0) {
    return reject(
      txId,
      RejectCodes.InvalidValidityIntervalFormat,
      "ttl must be non-negative",
    );
  }
  if (
    validityIntervalStart !== undefined &&
    validityIntervalEnd !== undefined &&
    validityIntervalStart > validityIntervalEnd
  ) {
    return reject(
      txId,
      RejectCodes.InvalidValidityIntervalFormat,
      `validity_interval_start ${validityIntervalStart} > ttl ${validityIntervalEnd}`,
    );
  }

  // R14 - vkey witness signatures verify against tx body hash
  const witnessKeyHashes: string[] = [];
  const witnessSignerSet = new Set<string>();
  const witnessSigners = CML.Ed25519KeyHashList.new();

  if (tx.witness_set.vkey_witnesses !== undefined) {
    for (let i = 0; i < tx.witness_set.vkey_witnesses.length; i++) {
      const ww = tx.witness_set.vkey_witnesses[i];
      let pubKey: CML.PublicKey;
      try {
        pubKey = CML.PublicKey.from_bytes(ww.vkey);
      } catch (e) {
        return reject(
          txId,
          RejectCodes.InvalidSignature,
          `vkey witness ${i} invalid key: ${String(e)}`,
        );
      }
      const sig = CML.Ed25519Signature.from_raw_bytes(ww.signature);
      if (!pubKey.verify(txBodyHash, sig)) {
        return reject(
          txId,
          RejectCodes.InvalidSignature,
          `invalid vkey witness #${i}`,
        );
      }
      const keyHash = pubKey.hash();
      const keyHashHex = keyHash.to_hex();
      if (!witnessSignerSet.has(keyHashHex)) {
        witnessSignerSet.add(keyHashHex);
        witnessSigners.add(keyHash);
        witnessKeyHashes.push(keyHashHex);
      }
    }
  }

  // R13 - required signers must have vkey witnesses
  if (body.required_signers !== undefined) {
    for (const signer of body.required_signers) {
      const signerHex = Buffer.from(signer).toString("hex");
      if (!witnessSignerSet.has(signerHex)) {
        return reject(
          txId,
          RejectCodes.MissingRequiredWitness,
          `missing witness for required signer ${signerHex}`,
        );
      }
    }
  }

  // R15 - native scripts present and valid
  const nativeScriptHashes: string[] = [];
  if (tx.witness_set.native_scripts !== undefined) {
    for (let i = 0; i < tx.witness_set.native_scripts.length; i++) {
      let script: CML.NativeScript;
      try {
        script = CML.NativeScript.from_cbor_bytes(
          tx.witness_set.native_scripts[i],
        );
      } catch (e) {
        return reject(
          txId,
          RejectCodes.NativeScriptInvalid,
          `native script ${i} decode failed: ${String(e)}`,
        );
      }
      const hash = script.hash();
      if (hash === undefined) {
        return reject(
          txId,
          RejectCodes.NativeScriptInvalid,
          `native script ${i} hash undefined`,
        );
      }
      nativeScriptHashes.push(hash.to_hex());
      const start =
        validityIntervalStart !== undefined
          ? BigInt(validityIntervalStart)
          : undefined;
      const end =
        validityIntervalEnd !== undefined
          ? BigInt(validityIntervalEnd)
          : undefined;
      if (!script.verify(start, end, witnessSigners)) {
        return reject(
          txId,
          RejectCodes.NativeScriptInvalid,
          `native script ${i} verification failed`,
        );
      }
    }
  }

  return {
    txId,
    tx,
    arrivalSeq: queuedTx.arrivalSeq,
    fee: body.fee,
    validityIntervalStart,
    validityIntervalEnd,
    referenceInputs: body.reference_inputs ?? [],
    outputSum,
    witnessKeyHashes,
    nativeScriptHashes,
    spent: body.inputs,
    produced,
  };
}

// ---------------------------------------------------------------------------
// Batch runner
// ---------------------------------------------------------------------------

export function runPhaseAValidation(
  queuedTxs: readonly QueuedTx[],
  config: PhaseAConfig,
): PhaseAResult {
  const accepted: PhaseAAccepted[] = [];
  const rejected: RejectedTx[] = [];
  for (const queuedTx of queuedTxs) {
    const result = validateOne(queuedTx, config);
    if ("code" in result) {
      rejected.push(result);
    } else {
      accepted.push(result);
    }
  }
  return { accepted, rejected };
}
