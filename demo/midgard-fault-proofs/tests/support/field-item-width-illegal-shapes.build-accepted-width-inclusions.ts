import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxWitnessSetCompact,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import { encodeMidgardForcedTxCompact } from "@al-ft/midgard-core/codec/forced";
import { Proof } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  nativeTxFromCoreCompact,
  type SubmitStep01TxInclusion,
} from "../../src/step-support.js";
import { l2TransactionSourceCbor } from "./emulator/native-tx.js";

// ---------------------------------------------------------------------------
// Shapes
// ---------------------------------------------------------------------------

/** §5.4 aggregate field ceiling; the largest field the carriage can open. */
export const MAXIMUM_FIELD_BYTES = 32_768;

/** `max_serialized_output_preimage_bytes`: the field-2 width bound. */
export const MAXIMUM_LEGAL_OUTPUT_BYTES = 16_384;

/**
 * A field-2 item is wrapped `59 LLLL` above 255 bytes and the one-item field
 * header is one byte, so the widest single output a 32,768-byte field can
 * carry is 32,764 payload bytes.
 */
export const MAXIMUM_OUTPUT_ITEM_BYTES = MAXIMUM_FIELD_BYTES - 1 - 3;

/**
 * Two items, both `59 LLLL`-wrapped, under a one-byte header: the selected
 * item sits second so opening it walks past the first and reads across the
 * chunk boundary. `16,376 + 16,384` payload bytes fills the field to 32,767.
 */
export const FORCED_FILLER_OUTPUT_BYTES =
  MAXIMUM_FIELD_BYTES - 1 - 3 - 3 - MAXIMUM_LEGAL_OUTPUT_BYTES - 1;

export const widthNativeTx = ({
  outputItems = [],
  mintItems = [],
}: {
  readonly outputItems?: readonly Buffer[];
  readonly mintItems?: readonly Buffer[];
}): MidgardNativeTxFull =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: encodeCbor([...outputItems]),
      fee: 7n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: encodeCbor([...mintItems]),
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

export type WidthShape = {
  readonly label: string;
  readonly nativeTx: MidgardNativeTxFull;
  readonly fieldIndex: 2 | 5;
  readonly itemIndex: number;
  readonly fieldPreimage: Buffer;
  /** What the canonical rule says about the selected item. */
  readonly illegal: boolean;
};

const shape = (
  label: string,
  nativeTx: MidgardNativeTxFull,
  fieldIndex: 2 | 5,
  itemIndex: number,
  illegal: boolean,
): WidthShape => {
  const fieldPreimage = Buffer.from(
    fieldIndex === 2
      ? nativeTx.body.outputsPreimageCbor
      : nativeTx.body.mintPreimageCbor,
  );
  const item = decodeMidgardFieldPreimage(fieldPreimage)[itemIndex];
  if (item === undefined) throw new Error(`${label}: item absent`);
  const actual =
    fieldIndex === 2
      ? item.length > MAXIMUM_LEGAL_OUTPUT_BYTES
      : item.length === 0;
  if (actual !== illegal) throw new Error(`${label}: shape polarity wrong`);
  return { label, nativeTx, fieldIndex, itemIndex, fieldPreimage, illegal };
};

/** Accepted, illegal: the widest output a maximum field can carry. */
export const maximumIllegalOutputShape = (): WidthShape =>
  shape(
    "maximum 32,768-byte output field, one 32,764-byte item (illegal)",
    widthNativeTx({
      outputItems: [Buffer.alloc(MAXIMUM_OUTPUT_ITEM_BYTES, 0xa1)],
    }),
    2,
    0,
    true,
  );

/** Legal at the bound: exactly 16,384 payload bytes, certified carriage. */
export const boundaryLegalOutputShape = (): WidthShape =>
  shape(
    "one 16,384-byte output item at the width bound (legal)",
    widthNativeTx({
      outputItems: [Buffer.alloc(MAXIMUM_LEGAL_OUTPUT_BYTES, 0xb2)],
    }),
    2,
    0,
    false,
  );

/**
 * Forced maximum: a 32,767-byte field whose second item is exactly at the
 * bound. Selecting index 1 walks the whole field under certified carriage.
 */
export const forcedMaximumLegalOutputShape = (): WidthShape =>
  shape(
    "maximum 32,767-byte output field, selected 16,384-byte second item (legal)",
    widthNativeTx({
      outputItems: [
        Buffer.alloc(FORCED_FILLER_OUTPUT_BYTES, 0xc3),
        Buffer.alloc(MAXIMUM_LEGAL_OUTPUT_BYTES, 0xc4),
      ],
    }),
    2,
    1,
    false,
  );

/** Forced honest: an output one byte over the bound, correctly rejected. */
export const forcedIllegalOutputShape = (): WidthShape =>
  shape(
    "one 16,385-byte output item one byte over the bound (illegal)",
    widthNativeTx({
      outputItems: [Buffer.alloc(MAXIMUM_LEGAL_OUTPUT_BYTES + 1, 0xd5)],
    }),
    2,
    0,
    true,
  );

/** Field 5 with a real policy item first and the empty item second. */
export const mintFieldTx = (): MidgardNativeTxFull =>
  widthNativeTx({
    mintItems: [Buffer.alloc(29, 0xe6), Buffer.alloc(0)],
  });

export const emptyMintItemShape = (nativeTx = mintFieldTx()): WidthShape =>
  shape(
    "two-item mint field, empty second item (illegal)",
    nativeTx,
    5,
    1,
    true,
  );

export const nonEmptyMintItemShape = (nativeTx = mintFieldTx()): WidthShape =>
  shape(
    "two-item mint field, 29-byte first item (legal)",
    nativeTx,
    5,
    0,
    false,
  );

export const compactCborHex = (
  nativeTx: MidgardNativeTxFull,
  sourceKind: bigint,
): string =>
  (sourceKind === 1n
    ? encodeMidgardForcedTxCompact
    : encodeMidgardNativeTxCompact)(nativeTx.compact).toString("hex");

export const witnessSetCompactCborHex = (
  nativeTx: MidgardNativeTxFull,
): string =>
  encodeMidgardNativeTxWitnessSetCompact(
    deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
  ).toString("hex");

// ---------------------------------------------------------------------------
// Accepted block: one transactions root over every committed transaction
// ---------------------------------------------------------------------------

export const buildAcceptedWidthInclusions = async (
  transactions: readonly MidgardNativeTxFull[],
): Promise<{
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly inclusions: readonly SubmitStep01TxInclusion[];
}> => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  const ids = transactions.map((nativeTx) =>
    computeMidgardNativeTxId(nativeTx).toString("hex"),
  );
  for (const [index, nativeTx] of transactions.entries()) {
    await trie.insert(
      Buffer.from(ids[index]!, "hex"),
      Buffer.from(l2TransactionSourceCbor(nativeTx), "hex"),
    );
  }
  const transactionsRoot = Buffer.from(trie.hash).toString("hex");
  const inclusions: SubmitStep01TxInclusion[] = [];
  for (const [index, nativeTx] of transactions.entries()) {
    const proof = await trie.prove(Buffer.from(ids[index]!, "hex"));
    inclusions.push({
      nativeTxId: ids[index]!,
      nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
      nativeTxCompactCbor: compactCborHex(nativeTx, 0n),
      l2TransactionSourceCbor: l2TransactionSourceCbor(nativeTx),
      transactionsPhasRoot: transactionsRoot,
      txMembershipProof: Data.from(proof.toCBOR().toString("hex"), Proof),
      txMembershipProofCbor: proof.toCBOR().toString("hex"),
    });
  }
  return {
    transactionsRoot,
    l2TransactionCount: BigInt(transactions.length),
    inclusions,
  };
};
