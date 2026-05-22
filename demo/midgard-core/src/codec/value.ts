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
      assetEntries.sort(([left], [right]) => compareBytes(left, right));
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

  policyEntries.sort(([left], [right]) => compareBytes(left, right));

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

export const decodeMidgardValue = (bytes: Uint8Array): MidgardValue => {
  const top = readCborArrayHeader(bytes, 0, "value");
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
      compareBytes(previousPolicy, policy.value) >= 0
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
        compareBytes(previousAssetName, assetName.value) >= 0
      ) {
        fail(
          "Value asset names must be sorted by raw bytes",
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

  if (cursor !== bytes.length) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      "Trailing bytes after Midgard value",
      `offset=${cursor}`,
    );
  }

  const decoded: MidgardValue = {
    lovelace: coin.value,
    assets: policies,
  };
  assertCanonicalCborRoundTrip(
    bytes,
    decoded,
    encodeMidgardValue,
    "Midgard value is not canonical",
  );
  return decoded;
};

export const midgardValueToCmlValue = (
  value: MidgardValue,
): InstanceType<typeof CML.Value> => {
  const multiasset = CML.MultiAsset.new();
  for (const [policyHex, assets] of value.assets.entries()) {
    const cmlAssets = CML.MapAssetNameToCoin.new();
    let assetCount = 0;
    for (const [assetNameHex, quantity] of assets.entries()) {
      if (quantity <= 0n) {
        fail("Midgard value asset quantity must be positive", assetNameHex);
      }
      cmlAssets.insert(
        CML.AssetName.from_raw_bytes(Buffer.from(assetNameHex, "hex")),
        quantity,
      );
      assetCount += 1;
    }
    if (assetCount > 0) {
      multiasset.insert_assets(CML.ScriptHash.from_hex(policyHex), cmlAssets);
    }
  }
  return multiasset.policy_count() === 0
    ? CML.Value.from_coin(value.lovelace)
    : CML.Value.new(value.lovelace, multiasset);
};
