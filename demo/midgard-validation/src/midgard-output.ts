import { CML } from "@lucid-evolution/lucid";
import {
  MIDGARD_PROTECTED_ADDRESS_HEADER_MASK,
  decodeMidgardAddressBytes,
  decodeMidgardNativeScript,
  encodeMidgardAddressText,
  type MidgardCredential,
  type MidgardTxOutput,
  type MidgardValue,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core/codec";
import {
  decodeTransactionOutput as decodeMidgardTsOutput,
  encodeTransactionOutput as encodeMidgardTsOutput,
  type TransactionOutput as MidgardTsTxOutput,
  type Value as MidgardTsValue,
  type VersionedScript as MidgardTsVersionedScript,
} from "@al-ft/midgard-ts";

export {
  MIDGARD_PROTECTED_ADDRESS_HEADER_MASK,
  type MidgardCredential,
  type MidgardTxOutput,
  type MidgardValue,
};

// Bridge: midgard-ts binary `TransactionOutput` <-> midgard-core `MidgardTxOutput`.
// The on-wire encoding moved from CBOR to midgard-ts binary, but validation and
// downstream node helpers still consume the core shape (`MidgardValue` map,
// `MidgardVersionedScript` with `scriptBytes`). Keep the conversion local so the
// validation package owns one decode path.

const coreValueToMidgardTsValue = (v: MidgardValue): MidgardTsValue => {
  if (v.assets.size === 0) {
    return { type: "Coin", coin: v.lovelace };
  }
  const assets: Array<[Uint8Array, Array<[Uint8Array, bigint]>]> = [];
  for (const [policyHex, names] of v.assets.entries()) {
    const entries: Array<[Uint8Array, bigint]> = [];
    for (const [nameHex, amount] of names.entries()) {
      entries.push([Buffer.from(nameHex, "hex"), amount]);
    }
    assets.push([Buffer.from(policyHex, "hex"), entries]);
  }
  return { type: "MultiAsset", coin: v.lovelace, assets };
};

const midgardTsValueToCoreValue = (v: MidgardTsValue): MidgardValue => {
  if (v.type === "Coin") {
    return { lovelace: v.coin, assets: new Map() };
  }
  const assets = new Map<string, Map<string, bigint>>();
  for (const [policyId, entries] of v.assets) {
    const names = new Map<string, bigint>();
    for (const [name, amount] of entries) {
      names.set(Buffer.from(name).toString("hex"), amount);
    }
    assets.set(Buffer.from(policyId).toString("hex"), names);
  }
  return { lovelace: v.coin, assets };
};

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

const coreOutputToMidgardTs = (
  output: MidgardTxOutput,
): MidgardTsTxOutput => ({
  address: Buffer.from(output.address),
  value: coreValueToMidgardTsValue(output.value),
  datum:
    output.datum === undefined ? undefined : Buffer.from(output.datum.cbor),
  script_ref:
    output.script_ref === undefined
      ? undefined
      : coreScriptRefToMidgardTs(output.script_ref),
});

const midgardTsOutputToCore = (output: MidgardTsTxOutput): MidgardTxOutput => ({
  address: Buffer.from(output.address),
  value: midgardTsValueToCoreValue(output.value),
  ...(output.datum === undefined
    ? {}
    : { datum: { kind: "inline" as const, cbor: Buffer.from(output.datum) } }),
  ...(output.script_ref === undefined
    ? {}
    : { script_ref: midgardTsScriptRefToCore(output.script_ref) }),
});

export const encodeMidgardTxOutput = (output: MidgardTxOutput): Buffer =>
  Buffer.from(encodeMidgardTsOutput(coreOutputToMidgardTs(output)));

export const decodeMidgardTxOutput = (bytes: Uint8Array): MidgardTxOutput =>
  midgardTsOutputToCore(decodeMidgardTsOutput(bytes));

export const midgardOutputAddressText = (output: MidgardTxOutput): string =>
  encodeMidgardAddressText(output.address);

export const midgardOutputProtected = (output: MidgardTxOutput): boolean =>
  decodeMidgardAddressBytes(output.address).protected;

export const midgardOutputPaymentCredential = (
  output: MidgardTxOutput,
): MidgardCredential => decodeMidgardAddressBytes(output.address).paymentCredential;

export const midgardValueToCmlValue = (
  value: MidgardValue,
): InstanceType<typeof CML.Value> => {
  const multiasset = CML.MultiAsset.new();
  for (const [policyId, assets] of value.assets.entries()) {
    const cmlAssets = CML.MapAssetNameToCoin.new();
    let assetCount = 0;
    for (const [assetName, quantity] of assets.entries()) {
      if (quantity <= 0n) {
        continue;
      }
      cmlAssets.insert(
        CML.AssetName.from_raw_bytes(Buffer.from(assetName, "hex")),
        quantity,
      );
      assetCount += 1;
    }
    if (assetCount > 0) {
      multiasset.insert_assets(CML.ScriptHash.from_hex(policyId), cmlAssets);
    }
  }
  return multiasset.policy_count() === 0
    ? CML.Value.from_coin(value.lovelace)
    : CML.Value.new(value.lovelace, multiasset);
};
