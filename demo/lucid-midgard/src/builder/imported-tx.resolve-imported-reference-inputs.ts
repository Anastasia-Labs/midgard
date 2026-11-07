import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardNativeTxCanonical,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";

import { BuilderInvariantError, SigningError } from "../core/errors.js";
import { outRefLabel } from "../core/out-ref.js";
import {
  decodeMidgardUtxo,
  utxoAddress,
  utxoOutputCbor,
  utxoOutRefCbor,
} from "../core/output.js";
import type { MidgardUtxo } from "../core/types.js";
import {
  assertImportedAddressNetwork,
  type FromTxOptions,
  importedExpectedWitnesses,
  type ImportedTxInput,
  requiredSignerKeyHashesFromTx,
  type UtxoNormalizer,
  validatedNativeInputs,
  validatedNativeOutputs,
} from "./imported-tx.normalize-resolved-spend-inputs.js";
import { type CompleteTxMetadata } from "./metadata.js";
import { cloneUtxo } from "./state.js";
import {
  addrWitnessKeyHashes,
  addrWitnessMetadata,
  decodeImportAddrWitnesses,
  estimatedSignedTxByteLength,
  nonEmptyBytesFromHex,
} from "./witness-bundle.js";

export const assertExpectedAddrWitnesses = ({
  actual,
  expected,
  expectedComplete = true,
  requireComplete,
}: {
  readonly actual: readonly string[];
  readonly expected: readonly string[] | undefined;
  readonly expectedComplete?: boolean;
  readonly requireComplete: boolean;
}): void => {
  if (expected === undefined || !expectedComplete) {
    if (requireComplete) {
      throw new SigningError(
        "Cannot prove expected address witness set",
        "supply resolved spend input pre-state before submitting",
      );
    }
    return;
  }
  const expectedSet = new Set(expected);
  const unexpected = actual.filter((keyHash) => !expectedSet.has(keyHash));
  if (unexpected.length > 0) {
    throw new SigningError(
      "Unexpected address witness",
      unexpected.sort().join(","),
    );
  }
  if (!requireComplete) {
    return;
  }
  const actualSet = new Set(actual);
  const missing = expected.filter((keyHash) => !actualSet.has(keyHash));
  if (missing.length > 0) {
    throw new SigningError(
      "Missing expected address witnesses",
      missing.join(","),
    );
  }
};

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null;

const canonicalImportedTxFromBytes = (
  bytes: Uint8Array,
): MidgardNativeTxFull => {
  let tx: MidgardNativeTxFull;
  try {
    tx = decodeMidgardNativeTxFullFromCanonicalCbor(bytes);
  } catch (cause) {
    throw new BuilderInvariantError(
      "fromTx accepts only Midgard native canonical transaction bytes",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
  const canonical = encodeMidgardNativeTxCanonical(tx);
  if (!canonical.equals(Buffer.from(bytes))) {
    throw new BuilderInvariantError(
      "fromTx requires canonical Midgard native transaction bytes",
    );
  }
  return tx;
};

const canonicalImportedTxFromObject = (
  tx: MidgardNativeTxFull,
): MidgardNativeTxFull => {
  try {
    return decodeMidgardNativeTxFullFromCanonicalCbor(
      encodeMidgardNativeTxCanonical(tx),
    );
  } catch (cause) {
    throw new BuilderInvariantError(
      "fromTx received an invalid Midgard native full transaction object",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
};

export const decodeFromTxInput = (
  input: ImportedTxInput,
): MidgardNativeTxFull => {
  if (input instanceof Uint8Array) {
    return canonicalImportedTxFromBytes(input);
  }
  if (typeof input === "string") {
    return canonicalImportedTxFromBytes(nonEmptyBytesFromHex(input, "txHex"));
  }
  if (isRecord(input)) {
    if ("txHex" in input) {
      if (typeof input.txHex !== "string") {
        throw new BuilderInvariantError("txHex must be a hex string");
      }
      return canonicalImportedTxFromBytes(
        nonEmptyBytesFromHex(input.txHex, "txHex"),
      );
    }
    if ("txCbor" in input) {
      if (input.txCbor instanceof Uint8Array) {
        return canonicalImportedTxFromBytes(input.txCbor);
      }
      if (typeof input.txCbor === "string") {
        return canonicalImportedTxFromBytes(
          nonEmptyBytesFromHex(input.txCbor, "txCbor"),
        );
      }
      throw new BuilderInvariantError("txCbor must be bytes or hex");
    }
    return canonicalImportedTxFromObject(
      input as unknown as MidgardNativeTxFull,
    );
  }
  throw new BuilderInvariantError("Unsupported fromTx input");
};

export const importedTxMetadata = (
  tx: MidgardNativeTxFull,
  options: FromTxOptions,
  normalizeUtxo: UtxoNormalizer,
  expectedNetworkId: number | undefined,
): CompleteTxMetadata => {
  const inputs = validatedNativeInputs(tx);
  const outputs = validatedNativeOutputs(tx, expectedNetworkId);
  const witnesses = decodeImportAddrWitnesses(tx);
  const expected = importedExpectedWitnesses(
    tx,
    options,
    normalizeUtxo,
    expectedNetworkId,
    inputs,
    outputs,
  );
  if (
    witnesses.length > 0 &&
    !expected.complete &&
    !options.allowUnknownExpectedWitnesses &&
    options.partial !== true
  ) {
    throw new SigningError(
      "Cannot import signed transaction with unknown expected address witnesses",
      "supply resolved spend input pre-state",
    );
  }
  assertExpectedAddrWitnesses({
    actual: addrWitnessKeyHashes(witnesses),
    expected: expected.keyHashes,
    expectedComplete: expected.complete,
    requireComplete: witnesses.length > 0 && expected.complete,
  });
  const txBytes = encodeMidgardNativeTxCanonical(tx);
  return {
    fee: tx.body.fee,
    inputCount: inputs.spendInputRefs.length,
    referenceInputCount: inputs.referenceInputRefs.length,
    outputCount: outputs.length,
    requiredSignerCount: requiredSignerKeyHashesFromTx(tx).length,
    txByteLength: txBytes.length,
    feeIterations: 0,
    balanced: false,
    expectedAddrWitnessCount: expected.complete
      ? expected.keyHashes.length
      : undefined,
    expectedAddrWitnessKeyHashes: expected.keyHashes,
    expectedAddrWitnessesComplete: expected.complete,
    estimatedSignedTxByteLength: expected.complete
      ? estimatedSignedTxByteLength(tx, expected.keyHashes.length)
      : undefined,
    ...addrWitnessMetadata(witnesses),
  };
};

export const localUtxosFromTx = (
  tx: MidgardNativeTxFull,
  expectedNetworkId?: number,
): readonly MidgardUtxo[] => {
  const txId = computeMidgardNativeTxId(tx).toString("hex");
  return validatedNativeOutputs(tx, expectedNetworkId).map(
    ({ outputCbor }, index) => {
      const outRef = { txHash: txId, outputIndex: index };
      return decodeMidgardUtxo({
        outRef,
        outRefCbor: utxoOutRefCbor(outRef),
        outputCbor,
      });
    },
  );
};

export const localUtxoAt = (
  tx: MidgardNativeTxFull,
  outputIndex: number,
  expectedNetworkId?: number,
): MidgardUtxo => {
  if (!Number.isSafeInteger(outputIndex) || outputIndex < 0) {
    throw new BuilderInvariantError(
      "Invalid local output index",
      outputIndex.toString(),
    );
  }
  const outputs = localUtxosFromTx(tx, expectedNetworkId);
  const output = outputs[outputIndex];
  if (output === undefined) {
    throw new BuilderInvariantError(
      "Local output index is out of range",
      outputIndex.toString(),
    );
  }
  return cloneUtxo(output);
};

export type ResolvedReferenceInputContext = {
  readonly inputs: readonly MidgardUtxo[];
  readonly outputsByOutRef: ReadonlyMap<string, Uint8Array>;
};

export const referenceOutputsByOutRef = (
  inputs: readonly MidgardUtxo[],
): ReadonlyMap<string, Uint8Array> => {
  const outputs = new Map<string, Uint8Array>();
  for (const input of inputs) {
    const key = Buffer.from(utxoOutRefCbor(input)).toString("hex");
    if (outputs.has(key)) {
      throw new BuilderInvariantError(
        "Duplicate resolved reference input",
        key,
      );
    }
    outputs.set(key, Buffer.from(utxoOutputCbor(input)));
  }
  return outputs;
};

export const resolveImportedReferenceInputs = (
  tx: MidgardNativeTxFull,
  options: FromTxOptions,
  normalizeUtxo: UtxoNormalizer,
  expectedNetworkId: number | undefined,
): ResolvedReferenceInputContext => {
  const referenceRefs = validatedNativeInputs(tx).referenceInputRefs;
  if (options.resolvedReferenceInputs === undefined) {
    if (referenceRefs.length > 0) {
      throw new BuilderInvariantError(
        "Missing resolved reference input",
        "every body reference input requires an exact resolved UTxO",
      );
    }
    return { inputs: [], outputsByOutRef: new Map() };
  }

  const required = new Set(referenceRefs.map(outRefLabel));
  const byLabel = new Map<string, MidgardUtxo>();
  for (const [index, input] of options.resolvedReferenceInputs.entries()) {
    const normalized = normalizeUtxo(input);
    assertImportedAddressNetwork(
      utxoAddress(normalized),
      expectedNetworkId,
      `resolvedReferenceInputs[${index.toString()}]`,
    );
    const label = outRefLabel(normalized);
    if (byLabel.has(label)) {
      throw new BuilderInvariantError(
        "Duplicate resolved reference input",
        label,
      );
    }
    if (!required.has(label)) {
      throw new BuilderInvariantError(
        "Unexpected resolved reference input",
        label,
      );
    }
    byLabel.set(label, normalized);
  }

  const ordered: MidgardUtxo[] = [];
  for (const ref of referenceRefs) {
    const label = outRefLabel(ref);
    const input = byLabel.get(label);
    if (input === undefined) {
      throw new BuilderInvariantError(
        "Missing resolved reference input",
        label,
      );
    }
    ordered.push(input);
  }
  return {
    inputs: ordered,
    outputsByOutRef: referenceOutputsByOutRef(ordered),
  };
};
