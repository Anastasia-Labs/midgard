import {
  decodeMidgardAddressBytes,
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput as decodeCoreMidgardTxOutput,
  encodeMidgardAddressText,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput as encodeCoreMidgardTxOutput,
  midgardAddressFromText,
  type MidgardTxOutput as CoreMidgardTxOutput,
  protectMidgardAddress,
} from "@al-ft/midgard-core/codec";
import { hexToBytes } from "@al-ft/midgard-core/hex";
import { CML } from "@lucid-evolution/lucid";

import {
  assertNonNegativeAssets,
  type Assets,
  normalizeValueLike,
  type ValueLike,
} from "./assets.js";
import { BuilderInvariantError } from "./errors.js";
import { normalizeOutRef, type OutRef } from "./out-ref.js";
import {
  addressBytesForOutput,
  assetsToMidgardValue,
  authoredDatumToCore,
  type AuthoredOutput,
  type DecodedMidgardOutput,
  midgardScriptFromCore,
  midgardValueToAssets,
  normalizeOutputDatum,
  normalizeScriptRef,
  type OutputKind,
  outputKindFromAddress,
  type OutputOptions,
  publicDatumFromCore,
  publicOutputToCore,
  type ScriptRefLike,
} from "./output.assets-to-midgard-value.js";
import type { Address, MidgardTxOutput, MidgardUtxo } from "./types.js";

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
 * must stay `encodeMidgardSpendInputItem` and never CML's minimal-index
 * `TransactionInput` CBOR, which is 36 bytes for indices 0–23. See
 * `docs/spec/midgard-tx.md` §5.3.
 */
export const outRefToCbor = (outRef: OutRef): Buffer => {
  const normalized = normalizeOutRef(outRef);
  return encodeMidgardSpendInputItem({
    txId: hexToBytes(normalized.txHash, { fieldName: "outRef.txHash" }),
    outputIndex: normalized.outputIndex,
  });
};

const decodeOutRefCbor = (
  outRefCbor: Uint8Array,
): { readonly outRef: OutRef; readonly cbor: Buffer } => {
  try {
    const decoded = decodeMidgardSpendInputItem(outRefCbor);
    return {
      outRef: normalizeOutRef({
        txHash: Buffer.from(decoded.txId).toString("hex"),
        outputIndex: decoded.outputIndex,
      }),
      // Re-encode rather than copy the input: the §5.3 form has exactly one
      // spelling, so a decode that round-trips to different bytes is a bug in
      // the caller's bytes, not a shape this builder should carry forward.
      cbor: encodeMidgardSpendInputItem(decoded),
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
