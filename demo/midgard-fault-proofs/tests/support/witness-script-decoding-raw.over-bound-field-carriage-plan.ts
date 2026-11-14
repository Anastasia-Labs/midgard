import {
  deriveMidgardNativeTxWitnessSetCompact,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardFieldCarriagePlan,
  midgardFieldCommitment,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  MIDGARD_FIELD_INDEX,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";

import {
  DECODING_SIGNER_KEY,
  decodingItemFromPayload,
} from "./native-script-decoding-emulator.js";

export const FAMILY = "witness-script-decoding";

// ---------------------------------------------------------------------------
// Shapes
// ---------------------------------------------------------------------------

/** The consensus bound on one retained field preimage (`[items]`). */
export const MAXIMUM_FIELD_BYTES = 32_768;

/**
 * A single-item field is `81 59 llll <item>`: the widest item the bound
 * admits is four bytes narrower than the field.
 */
export const MAXIMUM_ITEM_BYTES = MAXIMUM_FIELD_BYTES - 4;

/** The item's own wrapper `82 0t 59 llll` costs five more bytes. */
const MAXIMUM_PAYLOAD_BYTES = MAXIMUM_ITEM_BYTES - 5;

/** `[0, sig-node]`: canonical, two primitive steps. */
export const SIGNATURE_NODE = Buffer.from(
  `8200581c${DECODING_SIGNER_KEY.toString("hex")}`,
  "hex",
);

/** `after 0`: the narrowest canonical leaf token (3 bytes). */
const AFTER_ZERO_NODE = Buffer.from("820400", "hex");

/** `[0, h'']`: the wrapper decodes, the empty payload is the body fault. */
export const emptyPayloadItem = (): Buffer => Buffer.from("820040", "hex");

/** `[1, h'0a']`: language 1 is outside {0, 3, 128}, the header fault. */
export const headerMalformedItem = (): Buffer => Buffer.from("8201410a", "hex");

/**
 * The header fault at the maximum item shape: `[1, <32,759 bytes>]`, a
 * 32,764-byte item in a 32,768-byte field whose nine bounded-item chunks
 * step 02 commits before the wrapper is refused.
 */
export const headerMalformedMaximumItem = (): Buffer => {
  const payload = Buffer.alloc(MAXIMUM_PAYLOAD_BYTES, 0x0a);
  return Buffer.concat([
    Buffer.from("820159", "hex"),
    u16(payload.length),
    payload,
  ]);
};

/** One byte past the aggregate field bound: the adjacent refusal shape. */
export const ADJACENT_FIELD_BYTES = MAXIMUM_FIELD_BYTES + 1;

/**
 * `[1, <32,760 bytes>]`: the same wrapper as the maximum shape in a
 * 32,769-byte field, which no canonical transaction may carry. The field
 * commits like any other, so its chunks publish; the certificate policy
 * refuses `total_length` on chain, and no door can open it.
 */
export const headerMalformedAdjacentItem = (): Buffer => {
  const payload = Buffer.alloc(MAXIMUM_PAYLOAD_BYTES + 1, 0x0a);
  return Buffer.concat([
    Buffer.from("820159", "hex"),
    u16(payload.length),
    payload,
  ]);
};

/**
 * The tier-3 carriage plan an adversarial publisher would build for a field
 * preimage past the aggregate bound: the deterministic §8.4 split, the
 * digest vector and the certificate datum, exactly as `planMidgardFieldCarriage`
 * derives them, minus the off-chain bound the core codec enforces. The chunks
 * are real publications; only the certificate mint can refuse the length, and
 * the lifecycle proves it does so on chain.
 */
export const overBoundFieldCarriagePlan = ({
  owner,
  txId,
  preimage,
}: {
  readonly owner: string;
  readonly txId: string;
  readonly preimage: Buffer;
}): MidgardFieldCarriagePlan => {
  if (preimage.length <= MAXIMUM_FIELD_BYTES)
    throw new Error("the over-bound plan is only for a field past the bound");
  const chunks: Buffer[] = [];
  for (
    let offset = 0;
    offset < preimage.length;
    offset += MIDGARD_CHUNK_BYTES_K
  )
    chunks.push(preimage.subarray(offset, offset + MIDGARD_CHUNK_BYTES_K));
  const commitment = midgardFieldCommitment(preimage);
  const chunkDigests = chunks.map((chunk) => midgardFieldCommitment(chunk));
  return {
    tier: "Certified",
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
    txId: Buffer.from(txId, "hex"),
    totalLength: preimage.length,
    commitment,
    inlinePreimage: null,
    publications: chunks.map((bytes, chunkIndex) => ({
      chunkIndex,
      bytes,
      digest: chunkDigests[chunkIndex]!,
    })),
    certificate: {
      owner: Buffer.from(owner, "hex"),
      txId: Buffer.from(txId, "hex"),
      fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
      fieldHash: commitment,
      totalLength: preimage.length,
      chunkDigests,
    },
    certificateAssetName: Buffer.from(
      MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
    ),
  };
};

const u16 = (value: number): Buffer => {
  const out = Buffer.alloc(2);
  out.writeUInt16BE(value, 0);
  return out;
};

const cborArrayHeader = (count: number): Buffer =>
  count < 24
    ? Buffer.from([0x80 + count])
    : count < 256
      ? Buffer.from([0x98, count])
      : Buffer.concat([Buffer.from([0x99]), u16(count)]);

/**
 * The widest canonical shape the field bound admits: one `all` container
 * over 1,020 signature nodes and 38 `after 0` leaves, 1,059 nodes in a
 * 32,764-byte item that fills the 32,768-byte field. Every node is a
 * primitive token step followed by a frame step (2,119 steps with the
 * finalize), so the scan resumes through 133 sixteen-step segments, most of
 * which open on a frame step.
 */
export const wideCanonicalMaximumItem = (): Buffer => {
  const children = [
    ...Array.from({ length: 1_020 }, () => SIGNATURE_NODE),
    ...Array.from({ length: 38 }, () => AFTER_ZERO_NODE),
  ];
  const payload = Buffer.concat([
    Buffer.from("8201", "hex"),
    cborArrayHeader(children.length),
    ...children,
  ]);
  return decodingItemFromPayload(payload);
};

/**
 * The deepest canonical shape the field bound admits: `depth` nested
 * single-child `all` containers over one signature leaf. At the default
 * depth the field is exactly 32,768 bytes and the stack reaches 10,909,
 * two thirds of the 16,384 consensus depth bound, which no field-6 item can
 * reach (a container costs three bytes per level).
 */
export const DEEP_MAXIMUM_DEPTH = 10_909;

export const deepCanonicalItem = (depth = DEEP_MAXIMUM_DEPTH): Buffer => {
  const level = Buffer.from("820181", "hex");
  const payload = Buffer.concat([
    ...Array.from({ length: depth }, () => level),
    SIGNATURE_NODE,
  ]);
  return decodingItemFromPayload(payload);
};

/** A small canonical `all[sig]` item: four primitive steps, one chunk. */
export const smallCanonicalItem = (): Buffer =>
  decodingItemFromPayload(
    Buffer.concat([Buffer.from("820181", "hex"), SIGNATURE_NODE]),
  );

/** Field 6 as the retained preimage `[items]`. */
export const scriptWitnessField = (items: readonly Buffer[]): Buffer =>
  encodeCbor(items);

/**
 * A native transaction whose only non-empty field is the script-witness
 * field. `fee` separates otherwise identical transactions by id.
 */
export const nativeTxWithScriptWitnesses = (
  items: readonly Buffer[],
  fee: bigint,
): MidgardNativeTxFull =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: scriptWitnessField(items),
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

export type WitnessSetCarriage = {
  readonly witnessSet: NativeTxWitnessSetCompact;
  readonly witnessSetHash: string;
  readonly witnessSetCompactCbor: string;
  readonly compactCbor: string;
};

/** The compact witness-set view every step-02 opening anchors to. */
export const witnessSetCarriageOf = (
  nativeTx: MidgardNativeTxFull,
): WitnessSetCarriage => {
  const compact = deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet);
  return {
    witnessSet: {
      addr_tx_wits_hash: Buffer.from(compact.addrTxWitsHash).toString("hex"),
      script_tx_wits_hash: Buffer.from(compact.scriptTxWitsHash).toString(
        "hex",
      ),
      redeemer_tx_wits_hash: Buffer.from(compact.redeemerTxWitsHash).toString(
        "hex",
      ),
    },
    witnessSetHash: nativeTx.compact.transactionWitnessSetHash.toString("hex"),
    witnessSetCompactCbor:
      encodeMidgardNativeTxWitnessSetCompact(compact).toString("hex"),
    compactCbor: encodeMidgardNativeTxCompact(nativeTx.compact).toString("hex"),
  };
};
