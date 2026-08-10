import {
  decodeMidgardAddressBytes,
  decodeMidgardNativeScript,
  decodeMidgardSpendInputItemV1,
  decodeMidgardTxOutput as decodeCoreMidgardTxOutput,
  encodeMidgardAddressText,
  encodeMidgardSpendInputItemV1,
  encodeMidgardTxOutput as encodeCoreMidgardTxOutput,
  isProtectedMidgardAddress,
  MIDGARD_PROTECTED_ADDRESS_HEADER_MASK,
  midgardAddressFromText,
  type MidgardDatum as CoreMidgardDatum,
  type MidgardTxOutput as CoreMidgardTxOutput,
  type MidgardValue as CoreMidgardValue,
  type MidgardVersionedScript,
  protectMidgardAddress,
} from "@al-ft/midgard-core/codec";
import { hexToBytes } from "@al-ft/midgard-core/hex";
import { CML } from "@lucid-evolution/lucid";

import {
  assertNonNegativeAssets,
  type Assets,
  normalizeAssets,
  normalizeValueLike,
  type ValueLike,
} from "./assets.js";
import { BuilderInvariantError } from "./errors.js";
import { normalizeOutRef, type OutRef } from "./out-ref.js";
import type {
  Address,
  MidgardDatum,
  MidgardScript,
  MidgardTxOutput,
  MidgardUtxo,
} from "./types.js";

export { MIDGARD_PROTECTED_ADDRESS_HEADER_MASK };

export type PlutusDataLike = CML.PlutusData | Uint8Array | string;

export type ScriptRefLike = CML.Script | Uint8Array | string | MidgardScript;

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

const isCmlPlutusDataLike = (
  data: unknown,
): data is { readonly to_cbor_bytes: () => Uint8Array } =>
  typeof data === "object" &&
  data !== null &&
  typeof (data as { readonly as_integer?: unknown }).as_integer ===
    "function" &&
  typeof (data as { readonly to_cbor_bytes?: unknown }).to_cbor_bytes ===
    "function";

const isCmlScriptLike = (script: unknown): script is CML.Script =>
  typeof script === "object" &&
  script !== null &&
  typeof (script as { readonly as_native?: unknown }).as_native ===
    "function" &&
  typeof (script as { readonly as_plutus_v3?: unknown }).as_plutus_v3 ===
    "function" &&
  typeof (script as { readonly to_cbor_bytes?: unknown }).to_cbor_bytes ===
    "function";

const isOutputDatumOption = (datum: unknown): datum is OutputDatum => {
  if (
    typeof datum !== "object" ||
    datum === null ||
    datum instanceof Uint8Array
  ) {
    return false;
  }
  const kind = (datum as { readonly kind?: unknown }).kind;
  return kind === "none" || kind === "inline" || kind === "hash";
};

const fromHex = (hex: string, fieldName: string): Buffer => {
  try {
    return hexToBytes(hex, { fieldName, allowEmpty: true });
  } catch {
    throw new BuilderInvariantError(`${fieldName} must be hex`, hex);
  }
};

const addressBytesForOutput = (
  address: Address | CML.Address,
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

export const normalizePlutusData = (data: PlutusDataLike): CML.PlutusData => {
  if (data instanceof CML.PlutusData) {
    return data;
  }
  if (isCmlPlutusDataLike(data)) {
    return CML.PlutusData.from_cbor_bytes(data.to_cbor_bytes());
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
  script: CML.Script,
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
  if (
    script.as_plutus_v1() !== undefined ||
    script.as_plutus_v2() !== undefined
  ) {
    throw new BuilderInvariantError(
      "Midgard script references do not support PlutusV1 or PlutusV2",
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
    case "MidgardV1":
      return { language: scriptRef.type, scriptBytes: bytes };
  }
};

export const normalizeScriptRef = (
  scriptRef: ScriptRefLike,
): MidgardVersionedScript => {
  if (scriptRef instanceof CML.Script || isCmlScriptLike(scriptRef)) {
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
    case "MidgardV1":
      return {
        type: script.language,
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
  if (isOutputDatumOption(datum)) {
    return datum;
  }
  return { kind: "inline", data: datum };
};

const authoredDatumToCore = (
  datum: OutputOptions["datum"],
): CoreMidgardDatum | undefined => {
  const normalized = normalizeOutputDatum(datum);
  if (normalized === undefined || normalized.kind === "none") {
    return undefined;
  }
  if (normalized.kind === "hash") {
    throw new BuilderInvariantError(
      "Midgard outputs must not use datum hashes",
    );
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
  const lovelace = normalized.lovelace ?? 0n;
  for (const [unit, quantity] of Object.entries(normalized)) {
    if (unit === "lovelace") {
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
    if (unitBytes.length - 28 > 32) {
      throw new BuilderInvariantError(
        "Asset name must be at most 32 bytes",
        unit,
      );
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
  address: Address | CML.Address,
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
      : {
          scriptRef: midgardScriptFromCore(
            normalizeScriptRef(options.scriptRef),
          ),
        }),
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
  address: Address | CML.Address,
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
  address: Address | CML.Address,
  value: ValueLike,
  options?: OutputOptions,
): Buffer;
export function encodeMidgardTxOutput(
  addressOrOutput: Address | CML.Address | MidgardTxOutput,
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
    throw new BuilderInvariantError(
      "Midgard outputs must not use datum hashes",
    );
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

/**
 * The out-ref's canonical Midgard bytes: the §5.3 field-0/1 item encoding
 * `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16` — a fixed 38 bytes, with the
 * deliberately non-minimal 3-byte output index.
 *
 * One value, one spelling, three consumers: the field-0/1 preimage items
 * (`sortedInputCbors`), the ledger MPF trie key, and the ledger DB `outref`
 * column. On-chain `ledger_outref_key`
 * (`onchain/aiken/lib/midgard/fraud-proofs/transition-trace/proof.ak`) derives
 * the trie key through `encode_midgard_tx_input`, the same encoder — so this
 * must stay `encodeMidgardSpendInputItemV1` and never CML's minimal-index
 * `TransactionInput` CBOR, which is 36 bytes for indices 0–23. See
 * `docs/spec/midgard-tx.md` §5.3.
 */
export const outRefToCbor = (outRef: OutRef): Buffer => {
  const normalized = normalizeOutRef(outRef);
  return encodeMidgardSpendInputItemV1({
    txId: hexToBytes(normalized.txHash, { fieldName: "outRef.txHash" }),
    outputIndex: normalized.outputIndex,
  });
};

const decodeOutRefCbor = (
  outRefCbor: Uint8Array,
): { readonly outRef: OutRef; readonly cbor: Buffer } => {
  try {
    const decoded = decodeMidgardSpendInputItemV1(outRefCbor);
    return {
      outRef: normalizeOutRef({
        txHash: Buffer.from(decoded.txId).toString("hex"),
        outputIndex: decoded.outputIndex,
      }),
      // Re-encode rather than copy the input: the §5.3 form has exactly one
      // spelling, so a decode that round-trips to different bytes is a bug in
      // the caller's bytes, not a shape this builder should carry forward.
      cbor: encodeMidgardSpendInputItemV1(decoded),
    };
  } catch (cause) {
    throw new BuilderInvariantError(
      "Invalid UTxO outRefCbor",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
};

const verifiedOutRefCbor = (outRef: OutRef, outRefCbor: Uint8Array): Buffer => {
  const decoded = decodeOutRefCbor(outRefCbor);
  if (
    decoded.outRef.txHash !== outRef.txHash ||
    decoded.outRef.outputIndex !== outRef.outputIndex
  ) {
    throw new BuilderInvariantError(
      "Invalid UTxO outRefCbor",
      "CBOR does not match txHash/outputIndex",
    );
  }
  return decoded.cbor;
};

const providedOutRefCbor = (
  outRef: OutRef,
  outRefCbor: Uint8Array,
): { readonly outRef: OutRef; readonly cbor: Buffer } => ({
  outRef,
  cbor: verifiedOutRefCbor(outRef, outRefCbor),
});

export const decodeMidgardUtxo = ({
  outRef,
  outRefCbor,
  outputCbor,
}: {
  readonly outRef?: Pick<MidgardUtxo, "txHash" | "outputIndex">;
  readonly outRefCbor: Uint8Array;
  readonly outputCbor: Uint8Array;
}): MidgardUtxo => {
  const decodedOutRef =
    outRef === undefined
      ? decodeOutRefCbor(outRefCbor)
      : providedOutRefCbor(normalizeOutRef(outRef), outRefCbor);
  const decoded = decodeMidgardTxOutput(outputCbor);
  return {
    ...decodedOutRef.outRef,
    output: decoded.txOutput,
    cbor: {
      outRef: decodedOutRef.cbor,
      output: Buffer.from(outputCbor),
    },
  };
};

type OutRefCborInput = OutRef & {
  readonly cbor?: {
    readonly outRef?: Uint8Array;
  };
};

export const utxoOutRefCbor = (utxo: OutRefCborInput): Buffer => {
  if (utxo.cbor?.outRef === undefined) {
    return outRefToCbor(utxo);
  }
  return verifiedOutRefCbor(normalizeOutRef(utxo), utxo.cbor.outRef);
};

export const utxoOutputCbor = (utxo: MidgardUtxo): Buffer =>
  utxo.cbor?.output === undefined
    ? encodeMidgardTxOutput(utxo.output)
    : Buffer.from(utxo.cbor.output);

export const utxoAddress = (utxo: MidgardUtxo): Address => utxo.output.address;

export const utxoAssets = (utxo: MidgardUtxo): Assets => utxo.output.assets;

export const utxoProtectedAddress = (utxo: MidgardUtxo): boolean =>
  decodeMidgardAddressBytes(midgardAddressFromText(utxo.output.address))
    .protected;

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
