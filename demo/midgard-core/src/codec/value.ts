import { CML } from "@lucid-evolution/lucid";

import {
  BinaryReader,
  BinaryWriter,
  readBigI64,
  readBigU64,
  readU64,
  readVarBytesDynamic,
  readVarBytesLen,
  writeBigI64,
  writeBigU64,
  writeU64,
  writeVarBytesDynamic,
  writeVarBytesStatic,
} from "./binary.js";
import {
  HASH28_LENGTH,
  compareBytes,
  readHash28,
  writeHash28,
} from "./binary-types.js";
import { hexToBytes } from "../hex.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";

export type PolicyIdHex = string;
export type AssetNameHex = string;

export type MidgardValue = {
  readonly lovelace: bigint;
  readonly assets: ReadonlyMap<PolicyIdHex, ReadonlyMap<AssetNameHex, bigint>>;
};

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

type SortedAssetEntry = readonly [Buffer, bigint];
type SortedPolicyEntry = readonly [Buffer, readonly SortedAssetEntry[]];

const normalizePolicies = (value: MidgardValue): SortedPolicyEntry[] => {
  const policies: SortedPolicyEntry[] = [];
  for (const [policyHex, assets] of value.assets.entries()) {
    const policy = parseValueHex(policyHex, "value.policy_id");
    if (policy.length !== HASH28_LENGTH) {
      fail("Value policy id must be 28 bytes", policyHex);
    }
    const sortedAssets: SortedAssetEntry[] = [];
    for (const [assetNameHex, quantity] of assets.entries()) {
      if (quantity === 0n) continue;
      const assetName = parseValueHex(assetNameHex, "value.asset_name");
      if (assetName.length > MAX_ASSET_NAME_LENGTH) {
        fail("Value asset name must be at most 32 bytes", assetNameHex);
      }
      if (quantity < 0n) {
        fail("Value asset quantity must be positive", assetNameHex);
      }
      sortedAssets.push([assetName, quantity]);
    }
    if (sortedAssets.length === 0) {
      fail("Value policy asset map cannot be empty", policyHex);
    }
    sortedAssets.sort(([a], [b]) => compareBytes(a, b));
    policies.push([policy, sortedAssets]);
  }
  policies.sort(([a], [b]) => compareBytes(a, b));
  return policies;
};

/**
 * Binary `value` encoding (mirrors the staging-branch `midgard-ts` layout
 * adapted to keep the `MidgardValue` map-of-maps shape).
 *
 * Layout:
 *   Static:
 *     lovelace        (u64)
 *     policy_count    (u64)
 *     for each policy: policy_id (hash28 padded to 32) + asset_count (u64)
 *     for each asset:  asset_name_len (u64)
 *   Dynamic:
 *     for each asset (in policy/asset order): asset_name bytes + pad + qty (u64)
 *
 * No discriminant — even a coin-only value writes `policy_count = 0` and an
 * empty dynamic section.
 */
export const encodeMidgardValue = (value: MidgardValue): Buffer => {
  const sortedPolicies = normalizePolicies(value);
  const sw = new BinaryWriter();
  const dw = new BinaryWriter();
  writeBigU64(sw, value.lovelace);
  writeU64(sw, sortedPolicies.length);
  for (const [policy, assets] of sortedPolicies) {
    writeHash28(sw, policy);
    writeU64(sw, assets.length);
    for (const [name] of assets) writeVarBytesStatic(sw, name);
  }
  for (const [, assets] of sortedPolicies) {
    for (const [name, qty] of assets) {
      writeVarBytesDynamic(dw, name);
      writeBigU64(dw, qty);
    }
  }
  return Buffer.concat([sw.toBytes(), dw.toBytes()]);
};

type ValuePartialAsset = { readonly nameLen: number };
type ValuePartialPolicy = {
  readonly policy: Buffer;
  readonly assets: readonly ValuePartialAsset[];
};

const readMidgardValueStatic = (
  r: BinaryReader,
): { readonly lovelace: bigint; readonly partial: readonly ValuePartialPolicy[] } => {
  const lovelace = readBigU64(r);
  const policyCount = readU64(r);
  const partial: ValuePartialPolicy[] = [];
  for (let i = 0; i < policyCount; i += 1) {
    const policy = readHash28(r);
    const assetCount = readU64(r);
    const assets: ValuePartialAsset[] = [];
    for (let j = 0; j < assetCount; j += 1) {
      assets.push({ nameLen: readVarBytesLen(r) });
    }
    partial.push({ policy, assets });
  }
  return { lovelace, partial };
};

const readMidgardValueDynamic = (
  r: BinaryReader,
  staticPartial: { readonly lovelace: bigint; readonly partial: readonly ValuePartialPolicy[] },
): MidgardValue => {
  const { lovelace, partial } = staticPartial;
  const assets = new Map<PolicyIdHex, ReadonlyMap<AssetNameHex, bigint>>();
  let previousPolicy: Buffer | undefined;
  for (const { policy, assets: pAssets } of partial) {
    if (previousPolicy !== undefined && compareBytes(previousPolicy, policy) >= 0) {
      fail("Value policies must be sorted by raw policy id bytes");
    }
    previousPolicy = policy;
    const inner = new Map<AssetNameHex, bigint>();
    let previousName: Buffer | undefined;
    for (const { nameLen } of pAssets) {
      const name = readVarBytesDynamic(r, nameLen);
      const qty = readBigU64(r);
      if (qty === 0n) fail("Value asset quantity cannot be zero");
      if (previousName !== undefined && compareBytes(previousName, name) >= 0) {
        fail("Value asset names must be sorted by raw bytes");
      }
      previousName = name;
      inner.set(name.toString("hex"), qty);
    }
    assets.set(policy.toString("hex"), inner);
  }
  return { lovelace, assets };
};

export const decodeMidgardValue = (bytes: Uint8Array): MidgardValue => {
  const r = new BinaryReader(bytes);
  const staticPartial = readMidgardValueStatic(r);
  const value = readMidgardValueDynamic(r, staticPartial);
  r.expectEnd("value");
  return value;
};

export const writeMidgardValueStatic = (
  w: BinaryWriter,
  value: MidgardValue,
): void => {
  const sortedPolicies = normalizePolicies(value);
  writeBigU64(w, value.lovelace);
  writeU64(w, sortedPolicies.length);
  for (const [policy, assets] of sortedPolicies) {
    writeHash28(w, policy);
    writeU64(w, assets.length);
    for (const [name] of assets) writeVarBytesStatic(w, name);
  }
  return;
};

export const writeMidgardValueDynamic = (
  w: BinaryWriter,
  value: MidgardValue,
): void => {
  const sortedPolicies = normalizePolicies(value);
  for (const [, assets] of sortedPolicies) {
    for (const [name, qty] of assets) {
      writeVarBytesDynamic(w, name);
      writeBigU64(w, qty);
    }
  }
};

export { readMidgardValueStatic, readMidgardValueDynamic };
export type MidgardValueStaticPartial = ReturnType<typeof readMidgardValueStatic>;

// ---------------------------------------------------------------------------
// Mint (= multiasset<i64> with nonzero quantities).
// Same layout as MidgardValue except:
//   - no leading lovelace
//   - quantities are signed i64 (positive=mint, negative=burn)
//   - the empty mint encodes as policy_count=0 (no dynamic).
// ---------------------------------------------------------------------------

export type MidgardMint = ReadonlyMap<PolicyIdHex, ReadonlyMap<AssetNameHex, bigint>>;

type SortedMintEntry = readonly [Buffer, readonly (readonly [Buffer, bigint])[]];

const normalizeMint = (mint: MidgardMint): SortedMintEntry[] => {
  const policies: SortedMintEntry[] = [];
  for (const [policyHex, assets] of mint.entries()) {
    const policy = parseValueHex(policyHex, "mint.policy_id");
    if (policy.length !== HASH28_LENGTH) {
      fail("Mint policy id must be 28 bytes", policyHex);
    }
    const sortedAssets: (readonly [Buffer, bigint])[] = [];
    for (const [assetNameHex, quantity] of assets.entries()) {
      if (quantity === 0n) continue;
      const assetName = parseValueHex(assetNameHex, "mint.asset_name");
      if (assetName.length > MAX_ASSET_NAME_LENGTH) {
        fail("Mint asset name must be at most 32 bytes", assetNameHex);
      }
      sortedAssets.push([assetName, quantity]);
    }
    if (sortedAssets.length === 0) {
      fail("Mint policy asset map cannot be empty", policyHex);
    }
    sortedAssets.sort(([a], [b]) => compareBytes(a, b));
    policies.push([policy, sortedAssets]);
  }
  policies.sort(([a], [b]) => compareBytes(a, b));
  return policies;
};

export const writeMidgardMintStatic = (
  w: BinaryWriter,
  mint: MidgardMint,
): void => {
  const sorted = normalizeMint(mint);
  writeU64(w, sorted.length);
  for (const [policy, assets] of sorted) {
    writeHash28(w, policy);
    writeU64(w, assets.length);
    for (const [name] of assets) writeVarBytesStatic(w, name);
  }
};

export const writeMidgardMintDynamic = (
  w: BinaryWriter,
  mint: MidgardMint,
): void => {
  const sorted = normalizeMint(mint);
  for (const [, assets] of sorted) {
    for (const [name, qty] of assets) {
      writeVarBytesDynamic(w, name);
      writeBigI64(w, qty);
    }
  }
};

type MintPartialAsset = { readonly nameLen: number };
type MintPartialPolicy = {
  readonly policy: Buffer;
  readonly assets: readonly MintPartialAsset[];
};

export const readMidgardMintStatic = (
  r: BinaryReader,
): { readonly partial: readonly MintPartialPolicy[] } => {
  const policyCount = readU64(r);
  const partial: MintPartialPolicy[] = [];
  for (let i = 0; i < policyCount; i += 1) {
    const policy = readHash28(r);
    const assetCount = readU64(r);
    const assets: MintPartialAsset[] = [];
    for (let j = 0; j < assetCount; j += 1) {
      assets.push({ nameLen: readVarBytesLen(r) });
    }
    partial.push({ policy, assets });
  }
  return { partial };
};

export const readMidgardMintDynamic = (
  r: BinaryReader,
  partial: readonly MintPartialPolicy[],
): MidgardMint => {
  const policies = new Map<PolicyIdHex, Map<AssetNameHex, bigint>>();
  let previousPolicy: Buffer | undefined;
  for (const { policy, assets } of partial) {
    if (previousPolicy !== undefined && compareBytes(previousPolicy, policy) >= 0) {
      fail("Mint policies must be sorted by raw policy id bytes");
    }
    previousPolicy = policy;
    const inner = new Map<AssetNameHex, bigint>();
    let previousName: Buffer | undefined;
    for (const { nameLen } of assets) {
      const name = readVarBytesDynamic(r, nameLen);
      const qty = readBigI64(r);
      if (qty === 0n) fail("Mint asset quantity cannot be zero");
      if (previousName !== undefined && compareBytes(previousName, name) >= 0) {
        fail("Mint asset names must be sorted by raw bytes");
      }
      previousName = name;
      inner.set(name.toString("hex"), qty);
    }
    policies.set(policy.toString("hex"), inner);
  }
  return policies;
};

export const encodeMidgardMint = (mint: MidgardMint): Buffer => {
  const sw = new BinaryWriter();
  writeMidgardMintStatic(sw, mint);
  const dw = new BinaryWriter();
  writeMidgardMintDynamic(dw, mint);
  return Buffer.concat([sw.toBytes(), dw.toBytes()]);
};

export const decodeMidgardMint = (bytes: Uint8Array): MidgardMint => {
  const r = new BinaryReader(bytes);
  const { partial } = readMidgardMintStatic(r);
  const mint = readMidgardMintDynamic(r, partial);
  r.expectEnd("mint");
  return mint;
};

export type MidgardMintStaticPartial = ReturnType<typeof readMidgardMintStatic>;

export const midgardValueToCmlValue = (
  value: MidgardValue,
): CML.Value => {
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
