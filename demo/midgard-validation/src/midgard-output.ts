import { CML } from "@lucid-evolution/lucid";
import {
  MIDGARD_PROTECTED_ADDRESS_HEADER_MASK,
  decodeMidgardAddressBytes,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
  encodeMidgardTxOutput,
  type MidgardCredential,
  type MidgardTxOutput,
  type MidgardValue,
} from "@al-ft/midgard-core/codec";

export {
  MIDGARD_PROTECTED_ADDRESS_HEADER_MASK,
  decodeMidgardTxOutput,
  encodeMidgardTxOutput,
  type MidgardCredential,
  type MidgardTxOutput,
  type MidgardValue,
};

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
