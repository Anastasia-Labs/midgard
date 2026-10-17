import { CML } from "@lucid-evolution/lucid";

import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { decodeMidgardNativeScript } from "./native-script.js";
import { encodeMidgardFieldPreimage } from "./native-tx-field-access.js";
import {
  encodeMidgardFieldPreimageForField,
  type MidgardTxInput,
} from "./native-tx-field-items.js";
import { encodeMidgardTxOutput, type MidgardTxOutput } from "./output.js";
import { type MidgardValue } from "./value.js";
import { type MidgardVersionedScript } from "./versioned-script.js";

export type CardanoToMidgardNativeConstants = {
  readonly nativeTxVersion: bigint;
  readonly posixTimeNone: bigint;
  readonly networkIdNone: bigint;
};

export const parseCardanoTx = (txBytes: Uint8Array): CML.Transaction => {
  try {
    return CML.Transaction.from_cbor_bytes(txBytes);
  } catch (e) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "Invalid Cardano transaction bytes",
      String(e),
    );
  }
};

type CmlCollectionLike = {
  len(): number;
  get(index: number): unknown;
};

export type CmlMintLike = {
  policy_count(): number;
  keys(): CmlCollectionLike;
  get_assets(
    scriptHash: CML.ScriptHash,
  ): CML.MapAssetNameToNonZeroInt64 | undefined;
};

export const asCmlCallable = (
  value: unknown,
  methodName: "to_cbor_bytes" | "to_raw_bytes",
): (() => Uint8Array) | undefined => {
  if (typeof value !== "object" || value === null) {
    return undefined;
  }
  const method = (value as Record<string, unknown>)[methodName];
  if (typeof method !== "function") {
    return undefined;
  }
  return (method as () => Uint8Array).bind(value);
};

const cmlObjectToBytes = (value: unknown, fieldName: string): Buffer => {
  const toCbor = asCmlCallable(value, "to_cbor_bytes");
  if (toCbor !== undefined) {
    return Buffer.from(toCbor());
  }
  const toRaw = asCmlCallable(value, "to_raw_bytes");
  if (toRaw !== undefined) {
    return Buffer.from(toRaw());
  }
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.SchemaMismatch,
    `Cannot serialize CML value in ${fieldName}`,
  );
};

export const asCollectionLike = (
  value: unknown,
): CmlCollectionLike | undefined => {
  if (typeof value !== "object" || value === null) {
    return undefined;
  }
  const maybeLen = (value as Record<string, unknown>).len;
  const maybeGet = (value as Record<string, unknown>).get;
  if (typeof maybeLen === "function" && typeof maybeGet === "function") {
    return value as CmlCollectionLike;
  }
  return undefined;
};

export const asMintLike = (value: unknown): CmlMintLike | undefined => {
  if (typeof value !== "object" || value === null) {
    return undefined;
  }
  const maybePolicyCount = (value as Record<string, unknown>).policy_count;
  if (typeof maybePolicyCount !== "function") {
    return undefined;
  }
  return value as CmlMintLike;
};

/**
 * The §5.1 preimage of a field whose items are already raw bytes — fields 2, 3,
 * 4 and 7. Fields 0/1 do **not** come through here: their items carry §5.3's
 * fixed 3-byte output index, which is not what CML's `TransactionInput` CBOR
 * spells, so they have their own encoder below.
 */
export const cmlCollectionToPreimageCbor = (
  collection: CmlCollectionLike | undefined,
  fieldName: string,
): Buffer => {
  if (collection === undefined) {
    return encodeMidgardFieldPreimage([]);
  }
  const entries: Buffer[] = [];
  for (let i = 0; i < collection.len(); i++) {
    entries.push(cmlObjectToBytes(collection.get(i), `${fieldName}[${i}]`));
  }
  return encodeMidgardFieldPreimage(entries);
};

/**
 * Fields 0/1 carry §5.3 items (`82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, a fixed
 * 38 bytes), not CML's minimal-index `TransactionInput` CBOR — so a Cardano
 * input list must be re-encoded through the field-item encoder rather than
 * serialized as-is. This is the exact twin of `decodeNativeInputsToCardano`,
 * which decodes these items back into `CML.TransactionInput`s.
 */
export const cmlInputsToSpendInputPreimageCbor = (
  collection: CmlCollectionLike | undefined,
  fieldName: string,
): Buffer => {
  const items: MidgardTxInput[] = [];
  for (let i = 0; collection !== undefined && i < collection.len(); i++) {
    const input = collection.get(i);
    if (!(input instanceof CML.TransactionInput)) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.SchemaMismatch,
        `Cannot serialize CML value in ${fieldName}[${i}]`,
      );
    }
    items.push({
      txId: input.transaction_id().to_raw_bytes(),
      outputIndex: Number(input.index()),
    });
  }
  return encodeMidgardFieldPreimageForField({ fieldIndex: 0, items });
};

const cmlValueToMidgardValue = (value: CML.Value): MidgardValue => {
  const policies = new Map<string, Map<string, bigint>>();
  const multiasset = value.multi_asset();
  if (multiasset !== undefined) {
    const policyIds = multiasset.keys();
    for (let i = 0; i < policyIds.len(); i += 1) {
      const policy = policyIds.get(i);
      const assets = multiasset.get_assets(policy);
      if (assets === undefined) {
        continue;
      }
      const inner = new Map<string, bigint>();
      const assetNames = assets.keys();
      for (let j = 0; j < assetNames.len(); j += 1) {
        const assetName = assetNames.get(j);
        const quantity = assets.get(assetName);
        if (quantity !== undefined && quantity !== 0n) {
          inner.set(
            Buffer.from(assetName.to_raw_bytes()).toString("hex"),
            BigInt(quantity.toString(10)),
          );
        }
      }
      if (inner.size > 0) {
        policies.set(policy.to_hex(), inner);
      }
    }
  }
  return {
    lovelace: value.coin(),
    assets: policies,
  };
};

const cmlScriptToMidgardVersionedScript = (
  script: CML.Script,
  fieldName: string,
): MidgardVersionedScript => {
  const native = script.as_native();
  if (native !== undefined) {
    const decodedNative = decodeMidgardNativeScript(native.to_cbor_bytes());
    return {
      language: "NativeCardano",
      scriptBytes: decodedNative.cbor,
      nativeScript: decodedNative.script,
    };
  }
  const plutusV1 = script.as_plutus_v1();
  if (plutusV1 !== undefined) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
      "Midgard outputs do not support PlutusV1 reference scripts",
      fieldName,
    );
  }
  const plutusV2 = script.as_plutus_v2();
  if (plutusV2 !== undefined) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
      "Midgard outputs do not support PlutusV2 reference scripts",
      fieldName,
    );
  }
  const plutusV3 = script.as_plutus_v3();
  if (plutusV3 !== undefined) {
    return {
      language: "PlutusV3",
      scriptBytes: Buffer.from(plutusV3.to_raw_bytes()),
    };
  }
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.ConversionUnsupportedFeature,
    "Unsupported Cardano script reference for Midgard output",
    fieldName,
  );
};

const cmlOutputToMidgardOutputBytes = (
  output: CML.TransactionOutput,
  fieldName: string,
): Buffer => {
  const datum = output.datum();
  if (datum?.as_hash() !== undefined) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      "Midgard outputs must not use datum hashes; use inline datums",
      fieldName,
    );
  }
  const inlineDatum = datum?.as_datum();
  const scriptRef = output.script_ref();
  const midgardOutput: MidgardTxOutput = {
    address: Buffer.from(output.address().to_raw_bytes()),
    value: cmlValueToMidgardValue(output.amount()),
    ...(inlineDatum === undefined
      ? {}
      : {
          datum: {
            kind: "inline" as const,
            cbor: Buffer.from(inlineDatum.to_cbor_bytes()),
          },
        }),
    ...(scriptRef === undefined
      ? {}
      : {
          script_ref: cmlScriptToMidgardVersionedScript(
            scriptRef,
            `${fieldName}.script_ref`,
          ),
        }),
  };
  return encodeMidgardTxOutput(midgardOutput);
};

export const cmlOutputsToNativePreimageCbor = (
  collection: CmlCollectionLike | undefined,
): Buffer => {
  if (collection === undefined) {
    return encodeMidgardFieldPreimage([]);
  }
  const entries: Buffer[] = [];
  for (let i = 0; i < collection.len(); i += 1) {
    const output = collection.get(i);
    if (!(output instanceof CML.TransactionOutput)) {
      throw new MidgardTxCodecError(
        MidgardTxCodecErrorCodes.SchemaMismatch,
        `Unexpected output in transaction_body.outputs[${i}]`,
      );
    }
    entries.push(
      cmlOutputToMidgardOutputBytes(output, `transaction_body.outputs[${i}]`),
    );
  }
  return encodeMidgardFieldPreimage(entries);
};
