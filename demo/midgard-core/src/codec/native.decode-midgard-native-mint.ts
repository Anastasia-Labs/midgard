import { CML } from "@lucid-evolution/lucid";

import { decodeMidgardAddressBytes } from "./address.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { decodeMidgardNativeByteListPreimage } from "./native.verify-midgard-native-tx-proof-source.js";
import { MIDGARD_POSIX_TIME_NONE } from "./native-constants.js";
import { midgardRedeemersToCardano } from "./native-redeemer.js";
import { decodeMidgardFieldPreimage } from "./native-tx-field-access.js";
import {
  decodeMidgardFieldItems,
  decodeMidgardSpendInputItem,
} from "./native-tx-field-item-decoders.js";
import { decodeMidgardTxOutput } from "./output.js";
import { midgardValueToCmlValue } from "./value.js";
import {
  decodeMidgardVersionedScriptListPreimage,
  type MidgardVersionedScript,
} from "./versioned-script.js";

/**
 * §5.3 fields 0/1 read back into CML.
 *
 * The item cannot be handed to `CML.TransactionInput.from_cbor_bytes` directly:
 * §5.3 fixes the output index at the 3-byte `19 XXXX` form, which is not
 * minimal CBOR, so CML's strict reader refuses it. The item is decoded through
 * the §5.3 twin and the input rebuilt from its two parts.
 */
export const decodeNativeInputsToCardano = (
  preimageCbor: Uint8Array,
  fieldName: string,
): CML.TransactionInputList => {
  const inputs = CML.TransactionInputList.new();
  for (const item of decodeMidgardFieldPreimage(preimageCbor)) {
    let input;
    try {
      input = decodeMidgardSpendInputItem(item);
    } catch (error) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.CborDecode,
        `${fieldName} item is not a canonical §5.3 input`,
        String(error),
      );
    }
    // The Cardano side re-minimises the index on its own, which is correct — it
    // is a Cardano input now, not a Midgard field item.
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_raw_bytes(input.txId),
        BigInt(input.outputIndex),
      ),
    );
  }
  return inputs;
};

const midgardVersionedScriptToCardano = (
  script: MidgardVersionedScript,
  fieldName: string,
): CML.Script => {
  switch (script.language) {
    case "NativeCardano":
      return CML.Script.new_native(
        CML.NativeScript.from_cbor_bytes(script.scriptBytes),
      );
    case "PlutusV3":
      return CML.Script.new_plutus_v3(
        CML.PlutusV3Script.from_raw_bytes(script.scriptBytes),
      );
    case "MidgardV1":
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
        "MidgardV1 scripts cannot be represented as Cardano script references",
        fieldName,
      );
  }
};

const midgardOutputBytesToCardano = (
  outputBytes: Uint8Array,
  fieldName: string,
): CML.TransactionOutput => {
  const decoded = decodeMidgardTxOutput(outputBytes);
  const address = decodeMidgardAddressBytes(decoded.address);
  if (address.protected) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
      "Protected Midgard addresses cannot be represented as Cardano TxOut addresses",
      fieldName,
    );
  }
  const output = CML.ConwayFormatTxOut.new(
    CML.Address.from_raw_bytes(decoded.address),
    midgardValueToCmlValue(decoded.value),
  );
  if (decoded.datum !== undefined) {
    output.set_datum_option(
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_bytes(decoded.datum.cbor),
      ),
    );
  }
  if (decoded.script_ref !== undefined) {
    output.set_script_reference(
      midgardVersionedScriptToCardano(
        decoded.script_ref,
        `${fieldName}.script_ref`,
      ),
    );
  }
  return CML.TransactionOutput.new_conway_format_tx_out(output);
};

export const decodeNativeOutputsToCardano = (
  preimageCbor: Uint8Array,
): CML.TransactionOutputList => {
  const outputBytes = decodeMidgardNativeByteListPreimage(
    preimageCbor,
    "native.outputs",
  );
  const outputs = CML.TransactionOutputList.new();
  for (let i = 0; i < outputBytes.length; i++) {
    outputs.add(
      midgardOutputBytesToCardano(outputBytes[i], `native.outputs[${i}]`),
    );
  }
  return outputs;
};

export const decodeNativeAddrWitnessesToCardano = (
  preimageCbor: Uint8Array,
): CML.VkeywitnessList | undefined => {
  const witnessBytes = decodeMidgardNativeByteListPreimage(
    preimageCbor,
    "native.addr_tx_wits",
  );
  if (witnessBytes.length === 0) {
    return undefined;
  }
  const witnesses = CML.VkeywitnessList.new();
  for (let i = 0; i < witnessBytes.length; i++) {
    witnesses.add(CML.Vkeywitness.from_cbor_bytes(witnessBytes[i]));
  }
  return witnesses;
};

type DecodedCardanoScripts = {
  readonly nativeScripts?: CML.NativeScriptList;
  readonly plutusV3Scripts?: CML.PlutusV3ScriptList;
};

export type DecodedMidgardNativeMint = {
  readonly mint: CML.Mint;
  readonly policyIds: readonly string[];
  readonly mintedValue: CML.Value;
  readonly burnedValue: CML.Value;
};

export const decodeNativeScriptsToCardano = (
  preimageCbor: Uint8Array,
): DecodedCardanoScripts => {
  const scripts = decodeMidgardVersionedScriptListPreimage(
    preimageCbor,
    "native.script_tx_wits",
  );
  const nativeScripts = CML.NativeScriptList.new();
  const plutusV3Scripts = CML.PlutusV3ScriptList.new();
  for (let i = 0; i < scripts.length; i++) {
    const script = scripts[i];
    switch (script.language) {
      case "NativeCardano":
        nativeScripts.add(CML.NativeScript.from_cbor_bytes(script.scriptBytes));
        break;
      case "PlutusV3":
        plutusV3Scripts.add(
          CML.PlutusV3Script.from_raw_bytes(script.scriptBytes),
        );
        break;
      case "MidgardV1":
        throw new MidgardTxCodecError(
          MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
          "MidgardV1 inline scripts cannot be represented in Cardano witness sets",
          `native.script_tx_wits[${i}]`,
        );
    }
  }
  return {
    nativeScripts: nativeScripts.len() > 0 ? nativeScripts : undefined,
    plutusV3Scripts: plutusV3Scripts.len() > 0 ? plutusV3Scripts : undefined,
  };
};

const valueFromMultiasset = (multiasset: CML.MultiAsset): CML.Value =>
  multiasset.policy_count() === 0
    ? CML.Value.zero()
    : CML.Value.new(0n, multiasset);

export const decodeMidgardNativeMint = (
  preimageCbor: Uint8Array,
): DecodedMidgardNativeMint | undefined => {
  // §5.6: field 5 is the enveloped list of per-policy items. The decoder checks
  // policy-id and asset-name ordering and rejects duplicates, so CML never sees
  // a mint the committed bytes did not canonically spell. An empty field is
  // exactly `80`, like every other field — the retired raw-map form spelled it
  // `a0` and is prohibited.
  const items = decodeMidgardFieldItems(5, preimageCbor).items;
  if (items.length === 0) {
    return undefined;
  }

  const mint = CML.Mint.new();
  for (const item of items) {
    const assets = CML.MapAssetNameToNonZeroInt64.new();
    for (const asset of item.assets) {
      assets.insert(
        CML.AssetName.from_raw_bytes(asset.assetName),
        asset.quantity,
      );
    }
    mint.insert_assets(CML.ScriptHash.from_raw_bytes(item.policyId), assets);
  }

  const policyIds = Array.from({ length: mint.keys().len() }, (_, index) =>
    mint.keys().get(index).to_hex(),
  ).sort((a, b) => a.localeCompare(b));

  return {
    mint,
    policyIds,
    mintedValue: valueFromMultiasset(mint.as_positive_multiasset()),
    burnedValue: valueFromMultiasset(mint.as_negative_multiasset()),
  };
};

export const decodeNativeRedeemersToCardano = (
  preimageCbor: Uint8Array,
): CML.Redeemers | undefined => {
  return midgardRedeemersToCardano(preimageCbor);
};

export type MidgardToCardanoTxEncodingOptions = {
  readonly omitVkeyWitnesses?: boolean;
};

export const assertNativePosixTimeOrNone = (
  value: bigint,
  fieldName: string,
): bigint => {
  if (value < MIDGARD_POSIX_TIME_NONE) {
    throw new Error(
      `${fieldName} must be ${MIDGARD_POSIX_TIME_NONE.toString(10)} or a nonnegative POSIX time`,
    );
  }
  return value;
};
