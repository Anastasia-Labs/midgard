import { CML } from "@lucid-evolution/lucid";

import { hexToBytes } from "../hex.js";
import {
  assertCanonicalCborRoundTrip,
  compareBytes,
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborMapRaw,
  encodeCborUnsigned,
  readCborArrayHeader,
  readCborBytes,
  readCborMapHeader,
  readCborUnsigned,
} from "./cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";

export type PolicyIdHex = string;
export type AssetNameHex = string;

export type MidgardValue = {
  readonly lovelace: bigint;
  readonly assets: ReadonlyMap<PolicyIdHex, ReadonlyMap<AssetNameHex, bigint>>;
};

const POLICY_ID_LENGTH = 28;
const MAX_ASSET_NAME_LENGTH = 32;

/**
 * The one key order for policy ids and asset names in a Value or a mint field
 * (§5.5, §5.6): plain byte order, Cardano's, and the twin of Aiken's
 * `ledger_output_v1.asset_key_precedes`. Negative when `left` sorts first; on
 * an equal prefix the shorter key sorts first.
 */
export const compareMidgardAssetKeyBytes = (
  left: Uint8Array,
  right: Uint8Array,
): number => compareBytes(left, right);

const fail = (message: string, detail?: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    message,
    detail,
  );
};

const parseValueHex = (value: string, fieldName: string): Buffer => {
  try {
    return hexToBytes(value, { fieldName, allowEmpty: true });
  } catch {
    return fail(`${fieldName} must be hex`, value);
  }
};

export const encodeMidgardValue = (value: MidgardValue): Buffer => {
  const policyEntries = [...value.assets.entries()].map(
    ([policyHex, assets]) => {
      const policy = parseValueHex(policyHex, "value.policy_id");
      if (policy.length !== POLICY_ID_LENGTH) {
        fail("Value policy id must be 28 bytes", policyHex);
      }
      const assetEntries = [...assets.entries()].flatMap(
        ([assetNameHex, quantity]) => {
          const assetName = parseValueHex(assetNameHex, "value.asset_name");
          if (assetName.length > MAX_ASSET_NAME_LENGTH) {
            fail("Value asset name must be at most 32 bytes", assetNameHex);
          }
          if (quantity === 0n) {
            return [];
          }
          return [[assetName, quantity] as const];
        },
      );
      if (assetEntries.length === 0) {
        fail("Value policy asset map cannot be empty", policyHex);
      }
      assetEntries.sort(([left], [right]) =>
        compareMidgardAssetKeyBytes(left, right),
      );
      return [
        policy,
        encodeCborMapRaw(
          assetEntries.map(([assetName, quantity]) => [
            encodeCborBytes(assetName),
            encodeCborUnsigned(quantity),
          ]),
        ),
      ] as const;
    },
  );

  policyEntries.sort(([left], [right]) =>
    compareMidgardAssetKeyBytes(left, right),
  );

  return encodeCborArrayRaw([
    encodeCborUnsigned(value.lovelace),
    encodeCborMapRaw(
      policyEntries.map(([policy, assets]) => [
        encodeCborBytes(policy),
        assets,
      ]),
    ),
  ]);
};

/**
 * Decodes the canonical Midgard value that starts at `offset` and returns the
 * offset just past it. A Value is read structurally, never through the generic
 * CBOR map-key check, because its policy and asset-name keys follow §5.5's
 * plain byte order rather than CBOR's length-first one.
 */
export const decodeMidgardValueAt = (
  bytes: Uint8Array,
  offset: number,
): { readonly value: MidgardValue; readonly nextOffset: number } => {
  const top = readCborArrayHeader(bytes, offset, "value");
  if (top.length !== 2) {
    fail("Midgard value must be [coin, assets]", `length=${top.length}`);
  }
  const coin = readCborUnsigned(bytes, top.nextOffset, "value.coin");
  const assetsHeader = readCborMapHeader(
    bytes,
    coin.nextOffset,
    "value.assets",
  );
  let cursor = assetsHeader.nextOffset;
  let previousPolicy: Buffer | undefined;
  const policies = new Map<string, Map<string, bigint>>();

  for (let i = 0; i < assetsHeader.length; i += 1) {
    const policy = readCborBytes(bytes, cursor, "value.policy_id");
    cursor = policy.nextOffset;
    if (policy.value.length !== POLICY_ID_LENGTH) {
      fail("Value policy id must be 28 bytes", `index=${i}`);
    }
    if (
      previousPolicy !== undefined &&
      compareMidgardAssetKeyBytes(previousPolicy, policy.value) >= 0
    ) {
      fail(
        "Value policies must be sorted by raw policy id bytes",
        `index=${i}`,
      );
    }
    previousPolicy = policy.value;
    const policyHex = policy.value.toString("hex");
    const inner = readCborMapHeader(bytes, cursor, "value.assets.policy");
    cursor = inner.nextOffset;
    if (inner.length === 0) {
      fail("Value policy asset map cannot be empty", policyHex);
    }
    let previousAssetName: Buffer | undefined;
    const assets = new Map<string, bigint>();
    for (let j = 0; j < inner.length; j += 1) {
      const assetName = readCborBytes(bytes, cursor, "value.asset_name");
      cursor = assetName.nextOffset;
      if (assetName.value.length > MAX_ASSET_NAME_LENGTH) {
        fail(
          "Value asset name must be at most 32 bytes",
          `policy=${policyHex}`,
        );
      }
      if (
        previousAssetName !== undefined &&
        compareMidgardAssetKeyBytes(previousAssetName, assetName.value) >= 0
      ) {
        fail(
          "Value asset names must be sorted by raw asset name bytes",
          `policy=${policyHex}`,
        );
      }
      previousAssetName = assetName.value;
      const quantity = readCborUnsigned(bytes, cursor, "value.quantity");
      cursor = quantity.nextOffset;
      if (quantity.value === 0n) {
        fail("Value asset quantity cannot be zero", `policy=${policyHex}`);
      }
      assets.set(assetName.value.toString("hex"), quantity.value);
    }
    policies.set(policyHex, assets);
  }

  const decoded: MidgardValue = {
    lovelace: coin.value,
    assets: policies,
  };
  assertCanonicalCborRoundTrip(
    bytes.subarray(offset, cursor),
    decoded,
    encodeMidgardValue,
    "Midgard value is not canonical",
  );
  return { value: decoded, nextOffset: cursor };
};

export const decodeMidgardValue = (bytes: Uint8Array): MidgardValue => {
  const { value, nextOffset } = decodeMidgardValueAt(bytes, 0);
  if (nextOffset !== bytes.length) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      "Trailing bytes after Midgard value",
      `offset=${nextOffset}`,
    );
  }
  return value;
};

/**
 * `value` as a CML `Value`. The caller owns the returned object and frees it;
 * the intermediate CML objects built here are freed before returning.
 */
export const midgardValueToCmlValue = (value: MidgardValue): CML.Value => {
  const multiasset = CML.MultiAsset.new();
  try {
    for (const [policyHex, assets] of value.assets.entries()) {
      const cmlAssets = CML.MapAssetNameToCoin.new();
      try {
        for (const [assetNameHex, quantity] of assets.entries()) {
          if (quantity <= 0n) {
            fail("Midgard value asset quantity must be positive", assetNameHex);
          }
          const assetName = CML.AssetName.from_raw_bytes(
            Buffer.from(assetNameHex, "hex"),
          );
          try {
            cmlAssets.insert(assetName, quantity);
          } finally {
            assetName.free();
          }
        }
        if (cmlAssets.len() > 0) {
          const policy = CML.ScriptHash.from_hex(policyHex);
          try {
            multiasset.insert_assets(policy, cmlAssets);
          } finally {
            policy.free();
          }
        }
      } finally {
        cmlAssets.free();
      }
    }
    return multiasset.policy_count() === 0
      ? CML.Value.from_coin(value.lovelace)
      : CML.Value.new(value.lovelace, multiasset);
  } finally {
    multiasset.free();
  }
};

const MAX_UINT64 = 0xffff_ffff_ffff_ffffn;
const HEX_PATTERN = /^(?:[0-9a-f]{2})*$/;

/** The byte length of the shortest CBOR head carrying `argument`. */
const cborHeadLength = (argument: bigint): number => {
  if (argument < 24n) return 1;
  if (argument <= 0xffn) return 2;
  if (argument <= 0xffffn) return 3;
  if (argument <= 0xffff_ffffn) return 5;
  return 9;
};

const uint64Argument = (argument: bigint, fieldName: string): bigint =>
  argument >= 0n && argument <= MAX_UINT64
    ? argument
    : fail(
        `${fieldName} must be an unsigned 64-bit integer`,
        argument.toString(),
      );

/** A value key's hex, lower-cased so two spellings of one key collide. */
const valueKeyHex = (hex: string, fieldName: string): string => {
  const lower = hex.toLowerCase();
  return HEX_PATTERN.test(lower)
    ? lower
    : fail(`${fieldName} must be hex`, hex);
};

/**
 * The byte length of `value` as one Cardano output carries it: the ledger
 * descriptor's `cardano_value_size`, which the value-size cap and the
 * withdrawal payout size both read.
 *
 * Computed from the value's shape, without building the CML `Value` whose
 * `to_cbor_bytes()` it equals: a bare coin when no asset remains, otherwise
 * `[coin, {policy => {asset_name => quantity}}]`, every head in its shortest
 * form. Keys name bytes, so two hex spellings of one key are one entry, the
 * later one replacing the earlier as a CML map insert does; a policy with no
 * assets is left out. Key order does not change the length. An integer
 * outside the unsigned 64-bit range throws, where CML would wrap it.
 */
export const midgardCardanoValueCborLength = (value: MidgardValue): number => {
  const coinLength = cborHeadLength(
    uint64Argument(value.lovelace, "Midgard value lovelace"),
  );
  if (value.assets.size === 0) return coinLength;
  const policyLengths = new Map<string, number>();
  for (const [policyHex, assets] of value.assets.entries()) {
    const assetLengths = new Map<string, number>();
    for (const [assetNameHex, quantity] of assets.entries()) {
      if (quantity <= 0n) {
        fail("Midgard value asset quantity must be positive", assetNameHex);
      }
      const assetName = valueKeyHex(assetNameHex, "value.asset_name");
      const nameLength = assetName.length / 2;
      if (nameLength > MAX_ASSET_NAME_LENGTH) {
        fail("Value asset name must be at most 32 bytes", assetNameHex);
      }
      assetLengths.set(
        assetName,
        cborHeadLength(BigInt(nameLength)) +
          nameLength +
          cborHeadLength(uint64Argument(quantity, "Midgard value quantity")),
      );
    }
    if (assetLengths.size > 0) {
      const policy = valueKeyHex(policyHex, "value.policy_id");
      if (policy.length !== POLICY_ID_LENGTH * 2) {
        fail("Value policy id must be 28 bytes", policyHex);
      }
      let policyLength =
        cborHeadLength(BigInt(POLICY_ID_LENGTH)) +
        POLICY_ID_LENGTH +
        cborHeadLength(BigInt(assetLengths.size));
      for (const assetLength of assetLengths.values()) {
        policyLength += assetLength;
      }
      policyLengths.set(policy, policyLength);
    }
  }
  if (policyLengths.size === 0) return coinLength;
  let length =
    cborHeadLength(2n) +
    coinLength +
    cborHeadLength(BigInt(policyLengths.size));
  for (const policyLength of policyLengths.values()) {
    length += policyLength;
  }
  return length;
};
