import {
  CborReadError,
  isNull,
  readArray,
  readBytes,
  readHead,
  readMap,
  readSmallUint,
  readUint,
  skipItem,
  slice,
  untag,
} from "../cbor/reader.js";
import { blake2b256, compareOutRefs } from "../codec.js";
import type {
  Assets,
  BlockSummary,
  OutputSummary,
  OutRef,
  RedeemerPurpose,
  RedeemerSummary,
  TxSummary,
  WithdrawalSummary,
} from "../types.js";
import { readMultiAsset, readOutput } from "./output.js";

export class BlockDecodeError extends Error {
  constructor(message: string, options?: { cause?: unknown }) {
    super(message, options);
    this.name = "BlockDecodeError";
  }
}

const SET_TAG = 258n;

const readOutRefSet = (bytes: Uint8Array, offset: number): OutRef[] =>
  readArray(bytes, untag(bytes, offset, SET_TAG))
    .items.map((item) => {
      const [hash, index] = readArray(bytes, item).items;
      if (hash === undefined || index === undefined)
        throw new CborReadError("input must be [hash, index]", item);
      const txHash = readBytes(bytes, hash);
      if (txHash.length !== 32)
        throw new CborReadError("input hash must be 32 bytes", hash);
      return { txHash, index: readSmallUint(bytes, index) };
    })
    .sort(compareOutRefs);

const readWithdrawals = (
  bytes: Uint8Array,
  offset: number,
): WithdrawalSummary[] =>
  readMap(bytes, offset).entries.map((entry) => ({
    rewardAccount: readBytes(bytes, entry.key),
    amount: readUint(bytes, entry.value),
  }));

const PURPOSES: readonly RedeemerPurpose[] = [
  "spend",
  "mint",
  "cert",
  "reward",
  "voting",
  "proposing",
];

const purposeAt = (bytes: Uint8Array, offset: number): RedeemerPurpose => {
  const tag = readSmallUint(bytes, offset);
  const purpose = PURPOSES[tag];
  if (purpose === undefined)
    throw new CborReadError(`unknown redeemer tag ${tag}`, offset);
  return purpose;
};

const dataSlice = (bytes: Uint8Array, offset: number): Buffer =>
  slice(bytes, offset, skipItem(bytes, offset));

/** Redeemers in the legacy array form or the Conway map form. */
const readRedeemers = (
  bytes: Uint8Array,
  offset: number,
): RedeemerSummary[] => {
  const head = readHead(bytes, offset);
  if (head.major === 4)
    return readArray(bytes, offset).items.map((item) => {
      const [tag, index, data] = readArray(bytes, item).items;
      if (tag === undefined || index === undefined || data === undefined)
        throw new CborReadError(
          "redeemer must be [tag, index, data, units]",
          item,
        );
      return {
        purpose: purposeAt(bytes, tag),
        index: readSmallUint(bytes, index),
        data: dataSlice(bytes, data),
      };
    });
  return readMap(bytes, offset).entries.map((entry) => {
    const [tag, index] = readArray(bytes, entry.key).items;
    const [data] = readArray(bytes, entry.value).items;
    if (tag === undefined || index === undefined || data === undefined)
      throw new CborReadError("redeemer map entry is malformed", entry.key);
    return {
      purpose: purposeAt(bytes, tag),
      index: readSmallUint(bytes, index),
      data: dataSlice(bytes, data),
    };
  });
};

const readWitnessRedeemers = (witness: Buffer): RedeemerSummary[] => {
  for (const entry of readMap(witness, 0).entries)
    if (readSmallUint(witness, entry.key) === 5)
      return readRedeemers(witness, entry.value);
  return [];
};

/** The body fields the follower reads from a transaction body map. */
export type BodyFields = {
  inputs: OutRef[];
  outputs: OutputSummary[];
  collaterals: OutRef[];
  referenceInputs: OutRef[];
  collateralReturn: OutputSummary | null;
  mint: Assets;
  withdrawals: WithdrawalSummary[];
  invalidBefore: number | null;
  invalidAfter: number | null;
};

/** Reads one transaction body map; `body` must be exactly the map's bytes. */
export const readBody = (body: Buffer): BodyFields => {
  const fields: BodyFields = {
    inputs: [],
    outputs: [],
    collaterals: [],
    referenceInputs: [],
    collateralReturn: null,
    mint: new Map(),
    withdrawals: [],
    invalidBefore: null,
    invalidAfter: null,
  };
  const { entries, end } = readMap(body, 0);
  if (end !== body.length)
    throw new CborReadError("trailing bytes after tx body", end);
  for (const { key, value } of entries) {
    switch (readSmallUint(body, key)) {
      case 0:
        fields.inputs = readOutRefSet(body, value);
        break;
      case 1:
        fields.outputs = readArray(body, value).items.map((item) =>
          readOutput(body, item),
        );
        break;
      case 3:
        fields.invalidAfter = readSmallUint(body, value);
        break;
      case 5:
        fields.withdrawals = readWithdrawals(body, value);
        break;
      case 8:
        fields.invalidBefore = readSmallUint(body, value);
        break;
      case 9:
        fields.mint = readMultiAsset(body, value, true);
        break;
      case 13:
        fields.collaterals = readOutRefSet(body, value);
        break;
      case 16:
        fields.collateralReturn = readOutput(body, value);
        break;
      case 18:
        fields.referenceInputs = readOutRefSet(body, value);
        break;
      default:
        break;
    }
  }
  return fields;
};

const readHeader = (
  bytes: Uint8Array,
  offset: number,
): {
  hash: Buffer;
  height: number;
  slot: number;
  parentHash: Buffer | null;
} => {
  const end = skipItem(bytes, offset);
  const [headerBody] = readArray(bytes, offset).items;
  if (headerBody === undefined)
    throw new CborReadError("header must be [body, signature]", offset);
  const [blockNumber, slot, prevHash] = readArray(bytes, headerBody).items;
  if (blockNumber === undefined || slot === undefined || prevHash === undefined)
    throw new CborReadError("header body is too short", headerBody);
  return {
    hash: blake2b256(bytes.subarray(offset, end)),
    height: readSmallUint(bytes, blockNumber),
    slot: readSmallUint(bytes, slot),
    parentHash: isNull(bytes, prevHash) ? null : readBytes(bytes, prevHash),
  };
};

/**
 * Decodes one raw Shelley-family block (Alonzo and later: five elements;
 * earlier: four). Every byte slice is taken from `raw` exactly, so
 * blake2b-256(bodyCbor) is the transaction id and blake2b-256(header) the
 * block hash.
 */
export const decodeBlock = (raw: Uint8Array): BlockSummary => {
  try {
    const block = readArray(raw, 0);
    if (block.end !== raw.length)
      throw new CborReadError("trailing bytes after block", block.end);
    const [headerAt, bodiesAt, witnessesAt, auxAt, invalidAt] = block.items;
    if (
      headerAt === undefined ||
      bodiesAt === undefined ||
      witnessesAt === undefined ||
      auxAt === undefined ||
      block.items.length > 5
    )
      throw new CborReadError("block must have four or five elements", 0);
    const header = readHeader(raw, headerAt);
    const bodies = readArray(raw, bodiesAt).items;
    const witnesses = readArray(raw, witnessesAt).items;
    if (bodies.length !== witnesses.length)
      throw new CborReadError("body and witness counts differ", witnessesAt);
    const aux = new Map<number, Buffer>();
    for (const entry of readMap(raw, auxAt).entries)
      aux.set(
        readSmallUint(raw, entry.key),
        slice(raw, entry.value, skipItem(raw, entry.value)),
      );
    const invalid = new Set<number>();
    if (invalidAt !== undefined)
      for (const item of readArray(raw, invalidAt).items) {
        const index = readSmallUint(raw, item);
        if (index >= bodies.length)
          throw new CborReadError("invalid tx index out of range", item);
        invalid.add(index);
      }
    const txs: TxSummary[] = bodies.map((bodyAt, index) => {
      const witnessAt = witnesses[index] as number;
      const bodyCbor = slice(raw, bodyAt, skipItem(raw, bodyAt));
      const witnessCbor = slice(raw, witnessAt, skipItem(raw, witnessAt));
      const fields = readBody(bodyCbor);
      return {
        hash: blake2b256(bodyCbor),
        index,
        isValid: !invalid.has(index),
        bodyCbor,
        witnessCbor,
        auxCbor: aux.get(index) ?? null,
        ...fields,
        redeemers: readWitnessRedeemers(witnessCbor),
      };
    });
    return {
      point: { slot: header.slot, hash: header.hash },
      height: header.height,
      parentHash: header.parentHash,
      txs,
    };
  } catch (error) {
    if (error instanceof BlockDecodeError) throw error;
    throw new BlockDecodeError(
      `L1 block does not decode: ${error instanceof Error ? error.message : String(error)}`,
      { cause: error },
    );
  }
};
