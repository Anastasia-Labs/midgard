import { blake2b } from "@noble/hashes/blake2.js";

import type {
  Assets,
  OutRef,
  RedeemerPurpose,
  RedeemerSummary,
  WithdrawalSummary,
} from "./types.js";

export const blake2b256 = (bytes: Uint8Array): Buffer =>
  Buffer.from(blake2b(bytes, { dkLen: 32 }));

export const blake2b224 = (bytes: Uint8Array): Buffer =>
  Buffer.from(blake2b(bytes, { dkLen: 28 }));

export const OUT_REF_BYTES = 34;

/** The 34-byte outref encoding of §5.2: tx hash || u16 big-endian index. */
export const encodeOutRef = (outRef: OutRef): Buffer => {
  if (outRef.txHash.length !== 32)
    throw new Error("outref tx hash must be 32 bytes");
  if (
    !Number.isInteger(outRef.index) ||
    outRef.index < 0 ||
    outRef.index > 0xffff
  )
    throw new Error(`outref index ${outRef.index} is outside u16`);
  const bytes = Buffer.alloc(OUT_REF_BYTES);
  outRef.txHash.copy(bytes, 0);
  bytes.writeUInt16BE(outRef.index, 32);
  return bytes;
};

export const decodeOutRef = (bytes: Uint8Array): OutRef => {
  if (bytes.length !== OUT_REF_BYTES)
    throw new Error(`outref must be ${OUT_REF_BYTES} bytes`);
  const buffer = Buffer.from(bytes);
  return {
    txHash: Buffer.from(buffer.subarray(0, 32)),
    index: buffer.readUInt16BE(32),
  };
};

/** A compact in-memory key for an outref (one-byte-per-char string). */
export const outRefKey = (outRef: OutRef): string =>
  encodeOutRef(outRef).toString("latin1");

export const outRefKeyFromParts = (txHash: Buffer, index: number): string =>
  outRefKey({ txHash, index });

/** Ledger set order: by tx hash bytes, then index. */
export const compareOutRefs = (left: OutRef, right: OutRef): number =>
  Buffer.compare(left.txHash, right.txHash) || left.index - right.index;

const byKey = <T>(entries: Iterable<[string, T]>): [string, T][] =>
  [...entries].sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0));

/** Canonical JSON for an asset map: sorted keys, quantities as strings. */
export const assetsToJson = (assets: Assets): string => {
  const object: Record<string, Record<string, string>> = {};
  for (const [policy, names] of byKey(assets.entries())) {
    const inner: Record<string, string> = {};
    for (const [name, quantity] of byKey(names.entries()))
      inner[name] = quantity.toString();
    object[policy] = inner;
  }
  return JSON.stringify(object);
};

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const parseJson = (value: unknown): unknown =>
  typeof value === "string" ? (JSON.parse(value) as unknown) : value;

export const assetsFromJson = (value: unknown): Assets => {
  const object = parseJson(value);
  if (!isRecord(object)) throw new Error("asset JSON must be an object");
  const assets = new Map<string, Map<string, bigint>>();
  for (const [policy, names] of byKey(Object.entries(object))) {
    if (!isRecord(names))
      throw new Error("asset JSON policy must be an object");
    const inner = new Map<string, bigint>();
    for (const [name, quantity] of byKey(Object.entries(names))) {
      if (typeof quantity !== "string")
        throw new Error("asset JSON quantity must be a string");
      inner.set(name, BigInt(quantity));
    }
    assets.set(policy, inner);
  }
  return assets;
};

export const withdrawalsToJson = (
  withdrawals: readonly WithdrawalSummary[],
): string =>
  JSON.stringify(
    Object.fromEntries(
      byKey(
        withdrawals.map((entry): [string, string] => [
          entry.rewardAccount.toString("hex"),
          entry.amount.toString(),
        ]),
      ),
    ),
  );

export const withdrawalsFromJson = (value: unknown): WithdrawalSummary[] => {
  const object = parseJson(value);
  if (!isRecord(object)) throw new Error("withdrawal JSON must be an object");
  return byKey(Object.entries(object)).map(([account, amount]) => {
    if (typeof amount !== "string")
      throw new Error("withdrawal JSON amount must be a string");
    return {
      rewardAccount: Buffer.from(account, "hex"),
      amount: BigInt(amount),
    };
  });
};

const PURPOSE_ORDER: readonly RedeemerPurpose[] = [
  "spend",
  "mint",
  "cert",
  "reward",
  "voting",
  "proposing",
];

export const sortRedeemers = (
  redeemers: readonly RedeemerSummary[],
): RedeemerSummary[] =>
  [...redeemers].sort(
    (a, b) =>
      PURPOSE_ORDER.indexOf(a.purpose) - PURPOSE_ORDER.indexOf(b.purpose) ||
      a.index - b.index,
  );

export const redeemersToJson = (
  redeemers: readonly RedeemerSummary[],
): string =>
  JSON.stringify(
    sortRedeemers(redeemers).map((redeemer) => ({
      purpose: redeemer.purpose,
      index: redeemer.index,
      cbor: redeemer.data.toString("hex"),
    })),
  );

const isPurpose = (value: unknown): value is RedeemerPurpose =>
  typeof value === "string" &&
  (PURPOSE_ORDER as readonly string[]).includes(value);

export const redeemersFromJson = (value: unknown): RedeemerSummary[] => {
  const array = parseJson(value);
  if (!Array.isArray(array)) throw new Error("redeemer JSON must be an array");
  return array.map((entry: unknown) => {
    if (
      !isRecord(entry) ||
      !isPurpose(entry.purpose) ||
      typeof entry.index !== "number" ||
      typeof entry.cbor !== "string"
    )
      throw new Error("redeemer JSON entry is malformed");
    return {
      purpose: entry.purpose,
      index: entry.index,
      data: Buffer.from(entry.cbor, "hex"),
    };
  });
};

/** Mint policies of a tx, lowercase hex, sorted. */
export const mintPolicies = (mint: Assets): string[] => [...mint.keys()].sort();
