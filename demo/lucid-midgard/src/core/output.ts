import { CML } from "@lucid-evolution/lucid";
import {
  MIDGARD_PROTECTED_ADDRESS_HEADER_MASK,
  decodeMidgardAddressBytes,
  decodeMidgardNativeScript,
  decodeMidgardTxOutput as decodeCoreMidgardTxOutput,
  encodeMidgardAddressText,
  encodeMidgardTxOutput as encodeCoreMidgardTxOutput,
  isProtectedMidgardAddress,
  midgardAddressFromText,
  protectMidgardAddress,
  type MidgardDatum as CoreMidgardDatum,
  type MidgardTxOutput as CoreMidgardTxOutput,
  type MidgardValue as CoreMidgardValue,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core/codec";
import {
  assertNonNegativeAssets,
  cmlValueToAssets,
  normalizeAssets,
  normalizeValueLike,
  type Assets,
  type ValueLike,
} from "./assets.js";
import { BuilderInvariantError } from "./errors.js";
import { normalizeOutRef } from "./out-ref.js";
import type {
  Address,
  MidgardDatum,
  MidgardScript,
  MidgardTxOutput,
  MidgardUtxo,
} from "./types.js";

export { MIDGARD_PROTECTED_ADDRESS_HEADER_MASK };

export type PlutusDataLike =
  | InstanceType<typeof CML.PlutusData>
  | Uint8Array
  | string;

export type ScriptRefLike =
  | InstanceType<typeof CML.Script>
  | Uint8Array
  | string
  | MidgardScript;

export type OutputDatum =
  | { readonly kind: "none" }
  | { readonly kind: "inline"; readonly data: PlutusDataLike }
  | { readonly kind: "hash"; readonly hash: string };

export type OutputKind = "ordinary" | "protected";

export type OutputOptions = {
  readonly kind?: OutputKind;
  readonly datum?: OutputDatum | PlutusDataLike;
  readonly scriptRef?: ScriptRefLike;
};

export type AuthoredOutput = {
  readonly kind: OutputKind;
  readonly address: Address;
  readonly assets: Assets;
  readonly datum?: OutputDatum;
  readonly scriptRef?: ScriptRefLike;
};

export type DecodedMidgardOutput = {
  readonly outputCbor: Buffer;
  readonly address: Address;
  readonly assets: Assets;
  readonly txOutput: MidgardTxOutput;
};

const fromHex = (hex: string, fieldName: string): Buffer => {
  const normalized = hex.trim().toLowerCase();
  if (normalized.length % 2 !== 0 || !/^[0-9a-f]*$/.test(normalized)) {
    throw new BuilderInvariantError(`${fieldName} must be hex`, hex);
  }
  return Buffer.from(normalized, "hex");
};

const canonicalAddressText = (
  address: Address | InstanceType<typeof CML.Address>,
): Address => {
  const bytes =
    typeof address === "string"
      ? midgardAddressFromText(address)
      : Buffer.from(address.to_raw_bytes());
  return encodeMidgardAddressText(bytes);
};

const addressBytesForOutput = (
  address: Address | InstanceType<typeof CML.Address>,
  kind: OutputKind = "ordinary",
): Buffer => {
  const bytes =
    typeof address === "string"
      ? midgardAddressFromText(address)
      : Buffer.from(address.to_raw_bytes());
  return kind === "protected" ? protectMidgardAddress(bytes) : bytes;
};

const outputKindFromAddress = (address: Address): OutputKind =>
  isProtectedMidgardAddress(midgardAddressFromText(address))
    ? "protected"
    : "ordinary";

export const normalizePlutusData = (
  data: PlutusDataLike,
): InstanceType<typeof CML.PlutusData> => {
  if (data instanceof CML.PlutusData) {
    return data;
  }
  const bytes = typeof data === "string" ? fromHex(data, "datum") : data;
  return CML.PlutusData.from_cbor_bytes(bytes);
};

const isMidgardScript = (script: ScriptRefLike): script is MidgardScript =>
  typeof script === "object" &&
  script !== null &&
  !(script instanceof Uint8Array) &&
  !(script instanceof CML.Script) &&
  "type" in script &&
  "script" in script;

const cmlScriptToMidgardVersionedScript = (
  script: InstanceType<typeof CML.Script>,
): MidgardVersionedScript => {
  const native = script.as_native();
  if (native !== undefined) {
    const decoded = decodeMidgardNativeScript(native.to_cbor_bytes());
    return {
      language: "NativeCardano",
      scriptBytes: decoded.cbor,
      nativeScript: decoded.script,
    };
  }
  if (script.as_plutus_v1() !== undefined) {
    throw new BuilderInvariantError(
      "Midgard script references do not support PlutusV1",
    );
  }
  if (script.as_plutus_v2() !== undefined) {
    throw new BuilderInvariantError(
      "Midgard script references do not support PlutusV2",
    );
  }
  const plutusV3 = script.as_plutus_v3();
  if (plutusV3 === undefined) {
    throw new BuilderInvariantError("Unsupported Cardano script reference");
  }
  return {
    language: "PlutusV3",
    scriptBytes: Buffer.from(plutusV3.to_raw_bytes()),
  };
};

const midgardScriptToVersionedScript = (
  scriptRef: MidgardScript,
): MidgardVersionedScript => {
  const bytes = fromHex(scriptRef.script, "scriptRef.script");
  switch (scriptRef.type) {
    case "Native": {
      const decoded = decodeMidgardNativeScript(bytes);
      return {
        language: "NativeCardano",
        scriptBytes: decoded.cbor,
        nativeScript: decoded.script,
      };
    }
    case "PlutusV3":
      return { language: "PlutusV3", scriptBytes: bytes };
    case "MidgardV1":
      return { language: "MidgardV1", scriptBytes: bytes };
  }
};

export const normalizeScriptRef = (
  scriptRef: ScriptRefLike,
): MidgardVersionedScript => {
  if (scriptRef instanceof CML.Script) {
    return cmlScriptToMidgardVersionedScript(scriptRef);
  }
  if (isMidgardScript(scriptRef)) {
    return midgardScriptToVersionedScript(scriptRef);
  }
  const bytes =
    typeof scriptRef === "string" ? fromHex(scriptRef, "scriptRef") : scriptRef;
  return cmlScriptToMidgardVersionedScript(CML.Script.from_cbor_bytes(bytes));
};

const midgardScriptFromCore = (
  script: MidgardVersionedScript,
): MidgardScript => {
  switch (script.language) {
    case "NativeCardano":
      return {
        type: "Native",
        script: Buffer.from(script.scriptBytes).toString("hex"),
      };
    case "PlutusV3":
      return {
        type: "PlutusV3",
        script: Buffer.from(script.scriptBytes).toString("hex"),
      };
    case "MidgardV1":
      return {
        type: "MidgardV1",
        script: Buffer.from(script.scriptBytes).toString("hex"),
      };
  }
};

const normalizeOutputDatum = (
  datum: OutputOptions["datum"],
): OutputDatum | undefined => {
  if (datum === undefined) {
    return undefined;
  }
  if (
    typeof datum === "object" &&
    !(datum instanceof Uint8Array) &&
    !(datum instanceof CML.PlutusData) &&
    "kind" in datum
  ) {
    return datum;
  }
  return { kind: "inline", data: datum };
};

export { normalizeOutputDatum };

const authoredDatumToCore = (
  datum: OutputOptions["datum"],
): CoreMidgardDatum | undefined => {
  const normalized = normalizeOutputDatum(datum);
  if (normalized === undefined || normalized.kind === "none") {
    return undefined;
  }
  if (normalized.kind === "hash") {
    throw new BuilderInvariantError("Midgard outputs must not use datum hashes");
  }
  return {
    kind: "inline",
    cbor: Buffer.from(normalizePlutusData(normalized.data).to_cbor_bytes()),
  };
};

const publicDatumToCore = (
  datum: MidgardTxOutput["datum"],
): CoreMidgardDatum | undefined => {
  if (datum === undefined || datum === null) {
    return undefined;
  }
  return {
    kind: "inline",
    cbor: fromHex(datum.cbor, "output.datum.cbor"),
  };
};

const publicDatumFromCore = (
  datum: CoreMidgardDatum | undefined,
): MidgardDatum | undefined =>
  datum === undefined
    ? undefined
    : { kind: "inline", cbor: Buffer.from(datum.cbor).toString("hex") };

const assetsToMidgardValue = (value: ValueLike): CoreMidgardValue => {
  const normalized = assertNonNegativeAssets(
    normalizeValueLike(value),
    "output.assets",
  );
  const assets = new Map<string, Map<string, bigint>>();
  const lovelace = BigInt(normalized.lovelace ?? 0n);
  for (const [unit, quantityLike] of Object.entries(normalized)) {
    const quantity = BigInt(quantityLike);
    if (unit === "lovelace" || quantity === 0n) {
      continue;
    }
    const unitBytes = fromHex(unit, "asset unit");
    if (unitBytes.length < 28) {
      throw new BuilderInvariantError(
        "Asset unit must include a 28-byte policy id",
        unit,
      );
    }
    const policyId = unit.slice(0, 56);
    const assetName = unit.slice(56);
    if (fromHex(policyId, "asset policy id").length !== 28) {
      throw new BuilderInvariantError("Asset policy id must be 28 bytes", unit);
    }
    if (fromHex(assetName, "asset name").length > 32) {
      throw new BuilderInvariantError("Asset name must be at most 32 bytes", unit);
    }
    const policyAssets = assets.get(policyId) ?? new Map<string, bigint>();
    policyAssets.set(assetName, quantity);
    assets.set(policyId, policyAssets);
  }
  return { lovelace, assets };
};

const midgardValueToAssets = (value: CoreMidgardValue): Assets => {
  const result: Record<string, bigint> = {};
  if (value.lovelace !== 0n) {
    result.lovelace = value.lovelace;
  }
  for (const [policyId, assets] of value.assets.entries()) {
    for (const [assetName, quantity] of assets.entries()) {
      result[`${policyId}${assetName}`] = quantity;
    }
  }
  return normalizeAssets(result);
};

const publicOutputToCore = (output: MidgardTxOutput): CoreMidgardTxOutput => ({
  address: midgardAddressFromText(output.address),
  value: assetsToMidgardValue(output.assets),
  ...(output.datum === undefined || output.datum === null
    ? {}
    : { datum: publicDatumToCore(output.datum) }),
  ...(output.scriptRef === undefined || output.scriptRef === null
    ? {}
    : { script_ref: normalizeScriptRef(output.scriptRef) }),
});

const publicOutputFromCore = (
  output: CoreMidgardTxOutput,
): MidgardTxOutput => ({
  address: encodeMidgardAddressText(output.address),
  assets: midgardValueToAssets(output.value),
  ...(output.datum === undefined
    ? {}
    : { datum: publicDatumFromCore(output.datum) }),
  ...(output.script_ref === undefined
    ? {}
    : { scriptRef: midgardScriptFromCore(output.script_ref) }),
});

export const makeMidgardTxOutput = (
  address: Address | InstanceType<typeof CML.Address>,
  value: ValueLike,
  options: OutputOptions = {},
): MidgardTxOutput => {
  const addressText = encodeMidgardAddressText(
    addressBytesForOutput(address, options.kind ?? "ordinary"),
  );
  return {
    address: addressText,
    assets: assertNonNegativeAssets(normalizeValueLike(value), "output.assets"),
    ...(options.datum === undefined
      ? {}
      : { datum: publicDatumFromCore(authoredDatumToCore(options.datum)) }),
    ...(options.scriptRef === undefined
      ? {}
      : { scriptRef: midgardScriptFromCore(normalizeScriptRef(options.scriptRef)) }),
  };
};

export const protectMidgardOutputCbor = (outputCbor: Uint8Array): Buffer => {
  const output = decodeCoreMidgardTxOutput(outputCbor);
  return encodeCoreMidgardTxOutput({
    ...output,
    address: protectMidgardAddress(output.address),
  });
};

const encodeAuthoredMidgardTxOutput = (
  address: Address | InstanceType<typeof CML.Address>,
  value: ValueLike,
  options: OutputOptions = {},
): Buffer =>
  encodeCoreMidgardTxOutput({
    address: addressBytesForOutput(address, options.kind ?? "ordinary"),
    value: assetsToMidgardValue(value),
    ...(options.datum === undefined
      ? {}
      : { datum: authoredDatumToCore(options.datum) }),
    ...(options.scriptRef === undefined
      ? {}
      : { script_ref: normalizeScriptRef(options.scriptRef) }),
  });

const isMidgardTxOutput = (value: unknown): value is MidgardTxOutput =>
  typeof value === "object" &&
  value !== null &&
  "address" in value &&
  "assets" in value;

export function encodeMidgardTxOutput(output: MidgardTxOutput): Buffer;
export function encodeMidgardTxOutput(
  address: Address | InstanceType<typeof CML.Address>,
  value: ValueLike,
  options?: OutputOptions,
): Buffer;
export function encodeMidgardTxOutput(
  addressOrOutput: Address | InstanceType<typeof CML.Address> | MidgardTxOutput,
  value?: ValueLike,
  options: OutputOptions = {},
): Buffer {
  if (isMidgardTxOutput(addressOrOutput)) {
    return encodeCoreMidgardTxOutput(publicOutputToCore(addressOrOutput));
  }
  if (value === undefined) {
    throw new BuilderInvariantError("Midgard output value is required");
  }
  return encodeAuthoredMidgardTxOutput(addressOrOutput, value, options);
}

export const decodeMidgardTxOutput = (
  outputCbor: Uint8Array,
): DecodedMidgardOutput => {
  const coreOutput = decodeCoreMidgardTxOutput(outputCbor);
  const txOutput = publicOutputFromCore(coreOutput);
  return {
    outputCbor: Buffer.from(outputCbor),
    address: txOutput.address,
    assets: txOutput.assets,
    txOutput,
  };
};

export const authoredOutput = ({
  kind = "ordinary",
  address,
  value,
  datum,
  scriptRef,
}: {
  readonly kind?: OutputKind;
  readonly address: Address;
  readonly value: ValueLike;
  readonly datum?: OutputOptions["datum"];
  readonly scriptRef?: ScriptRefLike;
}): AuthoredOutput => {
  const normalizedDatum = normalizeOutputDatum(datum);
  if (normalizedDatum?.kind === "hash") {
    throw new BuilderInvariantError("Midgard outputs must not use datum hashes");
  }
  const normalizedAddress = encodeMidgardAddressText(
    addressBytesForOutput(address, kind),
  );
  return {
    kind: outputKindFromAddress(normalizedAddress),
    address: normalizedAddress,
    assets: assertNonNegativeAssets(normalizeValueLike(value), "output.assets"),
    datum: normalizedDatum,
    scriptRef,
  };
};

export const decodeMidgardUtxo = ({
  outRef,
  outRefCbor,
  outputCbor,
}: {
  readonly outRef: Pick<MidgardUtxo, "txHash" | "outputIndex">;
  readonly outRefCbor: Uint8Array;
  readonly outputCbor: Uint8Array;
}): MidgardUtxo => {
  const normalizedOutRef = normalizeOutRef(outRef);
  try {
    const decodedInput = CML.TransactionInput.from_cbor_bytes(outRefCbor);
    const decodedIndex = decodedInput.index();
    if (decodedIndex > BigInt(Number.MAX_SAFE_INTEGER)) {
      throw new Error(
        `output index exceeds JavaScript safe integer range: ${decodedIndex.toString()}`,
      );
    }
    const decodedOutRef = normalizeOutRef({
      txHash: decodedInput.transaction_id().to_hex(),
      outputIndex: Number(decodedIndex),
    });
    if (
      decodedOutRef.txHash !== normalizedOutRef.txHash ||
      decodedOutRef.outputIndex !== normalizedOutRef.outputIndex
    ) {
      throw new Error("CBOR does not match txHash/outputIndex");
    }
  } catch (cause) {
    throw new BuilderInvariantError(
      "Invalid UTxO outRefCbor",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
  const decoded = decodeMidgardTxOutput(outputCbor);
  return {
    ...normalizedOutRef,
    output: decoded.txOutput,
    cbor: {
      outRef: Buffer.from(outRefCbor),
      output: Buffer.from(outputCbor),
    },
  };
};

export const utxoOutRefCbor = (utxo: MidgardUtxo): Buffer =>
  utxo.cbor?.outRef === undefined
    ? Buffer.from(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(normalizeOutRef(utxo).txHash),
          BigInt(normalizeOutRef(utxo).outputIndex),
        ).to_cbor_bytes(),
      )
    : Buffer.from(utxo.cbor.outRef);

export const utxoOutputCbor = (utxo: MidgardUtxo): Buffer =>
  utxo.cbor?.output === undefined
    ? encodeMidgardTxOutput(utxo.output)
    : Buffer.from(utxo.cbor.output);

export const utxoAddress = (utxo: MidgardUtxo): Address => utxo.output.address;

export const utxoAssets = (utxo: MidgardUtxo): Assets => utxo.output.assets;

export const utxoProtectedAddress = (utxo: MidgardUtxo): boolean =>
  decodeMidgardAddressBytes(midgardAddressFromText(utxo.output.address)).protected;

export const outputAddressPaymentKeyHash = (
  address: Address,
): string | undefined => {
  const decoded = decodeMidgardAddressBytes(midgardAddressFromText(address));
  return decoded.paymentCredential.kind === "PubKey"
    ? decoded.paymentCredential.hash.toString("hex")
    : undefined;
};

export const outputAddressPaymentScriptHash = (
  address: Address,
): string | undefined => {
  const decoded = decodeMidgardAddressBytes(midgardAddressFromText(address));
  return decoded.paymentCredential.kind === "Script"
    ? decoded.paymentCredential.hash.toString("hex")
    : undefined;
};

export const outputAddressProtected = (address: Address): boolean =>
  decodeMidgardAddressBytes(midgardAddressFromText(address)).protected;
