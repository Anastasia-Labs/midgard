// Boundary adapter: on-the-wire `@al-ft/midgard-ts` binary `Transaction`
// <-> the internal `MidgardNativeTxFull` (root + preimage-CBOR) model that
// phase-a / phase-b still operate on.
//
// The midgard-ts encoding replaced the old CBOR codec on the wire. This file
// is the single place inside midgard-validation that bridges the two shapes
// so phase-a and phase-b can keep their existing field accesses unchanged.
//
// Once phase-a / phase-b are rewritten to operate on midgard-ts `Transaction`
// directly (Phase 5 main item), this file can be deleted.

import { CML } from "@lucid-evolution/lucid";
import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  EMPTY_SCRIPT_INTEGRITY_HASH,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeMint,
  decodeMidgardNativeScript,
  decodeMidgardVersionedScriptListPreimage,
  encodeCbor,
  encodeMidgardVersionedScriptListPreimage,
  materializeMidgardNativeTxFromCanonical,
  type MidgardNativeTxCanonical,
  type MidgardNativeTxFull,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core/codec";
import {
  decodeTransaction as decodeMidgardTsTransaction,
  decodeTransactionOutput as decodeMidgardTsTxOutput,
  encodeTransaction as encodeMidgardTsTransaction,
  encodeTransactionOutput as encodeMidgardTsTxOutput,
  transactionBodyHash as midgardTsTransactionBodyHash,
  transactionId as midgardTsTransactionId,
  type Mint as MidgardTsMint,
  type Transaction as MidgardTsTransaction,
  type TransactionBody as MidgardTsTransactionBody,
  type VersionedScript as MidgardTsVersionedScript,
} from "@al-ft/midgard-ts";
import type { MidgardNativeTxBodyFull } from "@al-ft/midgard-core/codec";

const cborListBuffers = (preimageCbor: Uint8Array): Buffer[] =>
  decodeMidgardNativeByteListPreimage(preimageCbor);

const isEmptyCborList = (bytes: Uint8Array): boolean =>
  Buffer.from(bytes).equals(EMPTY_CBOR_LIST);

const cborInputToOutRef = (
  b: Uint8Array,
): { tx_id: Uint8Array; index: number } => {
  const i = CML.TransactionInput.from_cbor_bytes(b);
  return {
    tx_id: i.transaction_id().to_raw_bytes(),
    index: Number(i.index()),
  };
};

const outRefToCborInput = (ref: {
  tx_id: Uint8Array;
  index: number;
}): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(ref.tx_id),
      BigInt(ref.index),
    ).to_cbor_bytes(),
  );

const cmlMintToMidgardTsMint = (
  mint: InstanceType<typeof CML.Mint>,
): MidgardTsMint => {
  const result: MidgardTsMint = [];
  const policies = mint.keys();
  for (let i = 0; i < policies.len(); i += 1) {
    const pid = policies.get(i);
    const assets = mint.get_assets(pid)!;
    const names = assets.keys();
    const entries: Array<[Uint8Array, bigint]> = [];
    for (let j = 0; j < names.len(); j += 1) {
      const name = names.get(j);
      entries.push([name.to_raw_bytes(), assets.get(name)!]);
    }
    result.push([pid.to_raw_bytes(), entries]);
  }
  return result;
};

const midgardTsMintToCborMap = (
  mint: MidgardTsMint,
): Map<Buffer, Map<Buffer, bigint>> => {
  const outer = new Map<Buffer, Map<Buffer, bigint>>();
  for (const [pid, entries] of mint) {
    const inner = new Map<Buffer, bigint>();
    for (const [name, amount] of entries) inner.set(Buffer.from(name), amount);
    outer.set(Buffer.from(pid), inner);
  }
  return outer;
};

const nonEmptyOrUndefined = <T>(xs: T[]): T[] | undefined =>
  xs.length === 0 ? undefined : xs;

const coreScriptRefToMidgardTs = (
  s: MidgardVersionedScript,
): MidgardTsVersionedScript => ({
  language: s.language,
  bytes: Buffer.from(s.scriptBytes),
});

const midgardTsScriptRefToCore = (
  s: MidgardTsVersionedScript,
): MidgardVersionedScript => {
  if (s.language === "NativeCardano") {
    const decoded = decodeMidgardNativeScript(s.bytes);
    return {
      language: "NativeCardano",
      scriptBytes: decoded.cbor,
      nativeScript: decoded.script,
    };
  }
  return { language: s.language, scriptBytes: Buffer.from(s.bytes) };
};

export const midgardTsToNativeFull = (
  tx: MidgardTsTransaction,
): MidgardNativeTxFull => {
  const b = tx.body;
  const ws = tx.witness_set;
  const list = (items: Uint8Array[]): Buffer =>
    encodeCbor(items.map((i) => Buffer.from(i)));
  const canonical: MidgardNativeTxCanonical = {
    version: MIDGARD_NATIVE_TX_VERSION,
    // midgard-ts only carries a boolean; non-valid txs are tagged generically.
    validity: tx.is_valid ? "TxIsValid" : "FailedScript",
    body: {
      spendInputsPreimageCbor: list(b.inputs.map(outRefToCborInput)),
      referenceInputsPreimageCbor: list(
        (b.reference_inputs ?? []).map(outRefToCborInput),
      ),
      outputsPreimageCbor: list(
        b.outputs.map((o) => Buffer.from(encodeMidgardTsTxOutput(o))),
      ),
      fee: b.fee,
      validityIntervalStart:
        b.validity_interval_start === undefined
          ? MIDGARD_POSIX_TIME_NONE
          : BigInt(b.validity_interval_start),
      validityIntervalEnd:
        b.ttl === undefined ? MIDGARD_POSIX_TIME_NONE : BigInt(b.ttl),
      requiredObserversPreimageCbor: list(
        (b.required_observers ?? []).map((h) => Buffer.from(h)),
      ),
      requiredSignersPreimageCbor: list(
        (b.required_signers ?? []).map((h) => Buffer.from(h)),
      ),
      mintPreimageCbor:
        b.mint === undefined
          ? EMPTY_CBOR_LIST
          : encodeCbor(midgardTsMintToCborMap(b.mint)),
      scriptIntegrityHash:
        b.script_data_hash === undefined
          ? EMPTY_SCRIPT_INTEGRITY_HASH
          : Buffer.from(b.script_data_hash),
      auxiliaryDataHash:
        b.auxiliary_data_hash === undefined
          ? EMPTY_NULL_ROOT
          : Buffer.from(b.auxiliary_data_hash),
      networkId:
        b.network_id === undefined
          ? MIDGARD_NATIVE_NETWORK_ID_NONE
          : BigInt(b.network_id),
    },
    witnessSet: {
      addrTxWitsPreimageCbor: list(
        (ws.vkey_witnesses ?? []).map((w) =>
          CML.Vkeywitness.new(
            CML.PublicKey.from_bytes(w.vkey),
            CML.Ed25519Signature.from_raw_bytes(w.signature),
          ).to_cbor_bytes(),
        ),
      ),
      scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage(
        (ws.scripts ?? []).map(midgardTsScriptRefToCore),
      ),
      redeemerTxWitsPreimageCbor:
        ws.redeemers === undefined ? EMPTY_CBOR_LIST : Buffer.from(ws.redeemers),
    },
  };
  return materializeMidgardNativeTxFromCanonical(canonical);
};

export const nativeBodyToMidgardTsBody = (
  b: MidgardNativeTxBodyFull,
): MidgardTsTransactionBody => {
  const referenceInputs = cborListBuffers(b.referenceInputsPreimageCbor);
  const requiredSigners = cborListBuffers(b.requiredSignersPreimageCbor);
  const requiredObservers = cborListBuffers(b.requiredObserversPreimageCbor);
  const decodedMint = decodeMidgardNativeMint(b.mintPreimageCbor);
  return {
    inputs: cborListBuffers(b.spendInputsPreimageCbor).map(cborInputToOutRef),
    outputs: cborListBuffers(b.outputsPreimageCbor).map((o) =>
      decodeMidgardTsTxOutput(o),
    ),
    fee: b.fee,
    ttl:
      b.validityIntervalEnd === MIDGARD_POSIX_TIME_NONE
        ? undefined
        : Number(b.validityIntervalEnd),
    auxiliary_data_hash: Buffer.from(b.auxiliaryDataHash).equals(EMPTY_NULL_ROOT)
      ? undefined
      : b.auxiliaryDataHash,
    validity_interval_start:
      b.validityIntervalStart === MIDGARD_POSIX_TIME_NONE
        ? undefined
        : Number(b.validityIntervalStart),
    mint:
      decodedMint === undefined
        ? undefined
        : cmlMintToMidgardTsMint(decodedMint.mint),
    script_data_hash: Buffer.from(b.scriptIntegrityHash).equals(
      EMPTY_SCRIPT_INTEGRITY_HASH,
    )
      ? undefined
      : b.scriptIntegrityHash,
    required_signers: nonEmptyOrUndefined(requiredSigners),
    network_id:
      b.networkId === MIDGARD_NATIVE_NETWORK_ID_NONE
        ? undefined
        : Number(b.networkId),
    reference_inputs: nonEmptyOrUndefined(
      referenceInputs.map(cborInputToOutRef),
    ),
    required_observers: nonEmptyOrUndefined(requiredObservers),
  };
};

export const nativeFullToMidgardTs = (
  tx: MidgardNativeTxFull,
): MidgardTsTransaction => {
  const ws = tx.witnessSet;
  const vkeyWitnessBytes = cborListBuffers(ws.addrTxWitsPreimageCbor);
  return {
    body: nativeBodyToMidgardTsBody(tx.body),
    witness_set: {
      vkey_witnesses: nonEmptyOrUndefined(
        vkeyWitnessBytes.map((wb) => {
          const w = CML.Vkeywitness.from_cbor_bytes(wb);
          return {
            vkey: w.vkey().to_raw_bytes(),
            signature: w.ed25519_signature().to_raw_bytes(),
          };
        }),
      ),
      scripts: nonEmptyOrUndefined(
        decodeMidgardVersionedScriptListPreimage(
          ws.scriptTxWitsPreimageCbor,
          "script_tx_wits",
        ).map(coreScriptRefToMidgardTs),
      ),
      redeemers: isEmptyCborList(ws.redeemerTxWitsPreimageCbor)
        ? undefined
        : ws.redeemerTxWitsPreimageCbor,
    },
    is_valid: tx.compact.validity === "TxIsValid",
  };
};

/**
 * Decode midgard-ts wire-format bytes into the internal `MidgardNativeTxFull`
 * shape. Drop-in replacement for the old `decodeMidgardNativeTxFull` used by
 * phase-a / phase-b before the codec swap.
 */
export const decodeMidgardTxBytesToNativeFull = (
  bytes: Uint8Array,
): MidgardNativeTxFull => midgardTsToNativeFull(decodeMidgardTsTransaction(bytes));

/**
 * Encode a `MidgardNativeTxFull` to midgard-ts wire-format binary bytes.
 * Used by lucid-midgard's builder; lives here so the bridge has one home.
 */
export const encodeMidgardTxBytes = (tx: MidgardNativeTxFull): Buffer =>
  Buffer.from(encodeMidgardTsTransaction(nativeFullToMidgardTs(tx)));

/**
 * Canonical midgard-ts transaction id of a `MidgardNativeTxFull`.
 */
export const midgardTxIdFromNativeFull = (tx: MidgardNativeTxFull): Buffer =>
  Buffer.from(midgardTsTransactionId(nativeFullToMidgardTs(tx)));

/**
 * midgard-ts body hash of a `MidgardNativeTxFull`. This is the hash that
 * vkey witnesses sign in the midgard-ts wire-format world — equal to
 * `midgardTxIdFromNativeFull` (since `transactionId = transactionBodyHash`),
 * but kept as a separate export so signing call sites read clearly.
 */
export const midgardTsBodyHashFromNativeFull = (
  tx: MidgardNativeTxFull,
): Buffer =>
  Buffer.from(midgardTsTransactionBodyHash(nativeBodyToMidgardTsBody(tx.body)));

/**
 * midgard-ts body hash computed from a body-only canonical structure. Test
 * helpers and signers that don't yet have a full tx use this to derive the
 * hash they need to sign over.
 */
export const midgardTsBodyHashFromNativeBody = (
  body: MidgardNativeTxBodyFull,
): Buffer =>
  Buffer.from(midgardTsTransactionBodyHash(nativeBodyToMidgardTsBody(body)));
