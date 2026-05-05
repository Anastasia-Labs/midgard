import {
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

const normalizeHex = (value: string, fieldName: string): Buffer => {
  const normalized = value.trim().toLowerCase();
  if (normalized.length % 2 !== 0 || !/^[0-9a-f]*$/.test(normalized)) {
    fail(`${fieldName} must be hex`, value);
  }
  return Buffer.from(normalized, "hex");
};

const assertCanonicalRoundTrip = (
  original: Uint8Array,
  decoded: MidgardValue,
): void => {
  const encoded = encodeMidgardValue(decoded);
  if (!Buffer.from(original).equals(encoded)) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      "Midgard value is not canonical",
    );
  }
};

export const encodeMidgardValue = (value: MidgardValue): Buffer => {
  if (value.lovelace < 0n) {
    fail("Midgard value lovelace must be non-negative");
  }

  const policyEntries = [...value.assets.entries()].map(
    ([policyHex, assets]) => {
      const policy = normalizeHex(policyHex, "value.policy_id");
      if (policy.length !== POLICY_ID_LENGTH) {
        fail("Value policy id must be 28 bytes", policyHex);
      }
      const assetEntries = [...assets.entries()].flatMap(
        ([assetNameHex, quantity]) => {
          const assetName = normalizeHex(assetNameHex, "value.asset_name");
          if (assetName.length > MAX_ASSET_NAME_LENGTH) {
            fail("Value asset name must be at most 32 bytes", assetNameHex);
          }
          if (quantity < 0n) {
            fail("Value asset quantity must be non-negative", assetNameHex);
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
  const assetsHeader = readCborMapHeader(bytes, coin.nextOffset, "value.assets");
  let cursor = assetsHeader.nextOffset;
  let previousPolicy: Buffer | undefined;
  const policies = new Map<string, Map<string, bigint>>();

  for (let i = 0; i < assetsHeader.length; i += 1) {
    const policy = readCborBytes(bytes, cursor, "value.policy_id");
    cursor = policy.nextOffset;
    if (policy.value.length !== POLICY_ID_LENGTH) {
      fail("Value policy id must be 28 bytes", `index=${i}`);
    }
    if (previousPolicy !== undefined && compareBytes(previousPolicy, policy.value) >= 0) {
      fail("Value policies must be sorted by raw policy id bytes", `index=${i}`);
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
        fail("Value asset name must be at most 32 bytes", `policy=${policyHex}`);
      }
      if (
        previousAssetName !== undefined &&
        compareBytes(previousAssetName, assetName.value) >= 0
      ) {
        fail("Value asset names must be sorted by raw bytes", `policy=${policyHex}`);
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
  assertCanonicalRoundTrip(bytes, decoded);
  return decoded;
};

export const midgardValueToAssetMap = (
  value: MidgardValue,
): Map<string, Map<string, bigint>> => {
  const result = new Map<string, Map<string, bigint>>();
  if (value.lovelace !== 0n) {
    result.set("", new Map([["", value.lovelace]]));
  }
  for (const [policy, assets] of value.assets.entries()) {
    result.set(policy, new Map(assets));
  }
  return result;
};

export const addMidgardValues = (
  left: MidgardValue,
  right: MidgardValue,
): MidgardValue => {
  const assets = new Map<string, Map<string, bigint>>();
  const addAssets = (value: MidgardValue): void => {
    for (const [policy, inner] of value.assets.entries()) {
      const target = assets.get(policy) ?? new Map<string, bigint>();
      for (const [assetName, quantity] of inner.entries()) {
        target.set(assetName, (target.get(assetName) ?? 0n) + quantity);
      }
      assets.set(policy, target);
    }
  };
  addAssets(left);
  addAssets(right);
  return { lovelace: left.lovelace + right.lovelace, assets };
};
