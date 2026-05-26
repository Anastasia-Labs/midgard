import { CML } from "@lucid-evolution/lucid";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeHash28ListPreimage,
  decodeMidgardNativeInputsPreimageAsCbor,
  decodeMidgardNativeTxFull,
  encodeMidgardNativeTxFull,
  verifyMidgardNativeTxFullConsistency,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";
import {
  decodeMidgardTxOutput,
  decodeMidgardUtxo,
  outputAddressPaymentKeyHash,
  outputAddressProtected,
  utxoAddress,
  utxoOutRefCbor,
} from "../core/output.js";
import { BuilderInvariantError, SigningError } from "../core/errors.js";
import {
  compareOutRefs,
  normalizeOutRef,
  outRefLabel,
  type OutRef,
} from "../core/out-ref.js";
import type { MidgardUtxo } from "../core/types.js";
import { assertAddressNetwork } from "../wallet.js";
import {
  paymentPubKeyHashFromUtxo,
  type CompleteTxMetadata,
} from "./metadata.js";
import { normalizeHashHex } from "./normalizers.js";
import { cloneUtxo } from "./state.js";
import {
  addrWitnessKeyHashes,
  addrWitnessMetadata,
  decodeImportAddrWitnesses,
  estimatedSignedTxByteLength,
  nonEmptyBytesFromHex,
} from "./witness-bundle.js";

export type ImportedTxInput =
  | MidgardNativeTxFull
  | Uint8Array
  | string
  | { readonly txCbor: Uint8Array | string }
  | { readonly txHex: string };

export type FromTxOptions = {
  readonly resolvedSpendInputs?: readonly MidgardUtxo[];
  readonly allowUnexpectedResolvedInputs?: boolean;
  readonly allowUnknownExpectedWitnesses?: boolean;
  readonly partial?: boolean;
};

export type UtxoNormalizer = (utxo: MidgardUtxo) => MidgardUtxo;

type ValidatedNativeInputs = {
  readonly spendInputRefs: readonly OutRef[];
  readonly referenceInputRefs: readonly OutRef[];
};

type ValidatedNativeOutput = {
  readonly outputCbor: Buffer;
  readonly decoded: ReturnType<typeof decodeMidgardTxOutput>;
};

const assertImportedAddressNetwork = (
  address: string,
  expectedNetworkId: number | undefined,
  context: string,
): void => {
  try {
    assertAddressNetwork(address, expectedNetworkId);
  } catch (cause) {
    if (cause instanceof BuilderInvariantError) {
      throw new BuilderInvariantError(
        `${context} address network mismatch`,
        cause.detail,
      );
    }
    throw cause;
  }
};

const outRefFromCbor = (inputCbor: Uint8Array, fieldName: string): OutRef => {
  try {
    const input = CML.TransactionInput.from_cbor_bytes(inputCbor);
    if (!Buffer.from(input.to_cbor_bytes()).equals(Buffer.from(inputCbor))) {
      throw new Error("input CBOR is not canonical");
    }
    return normalizeOutRef({
      txHash: input.transaction_id().to_hex(),
      outputIndex: Number(input.index()),
    });
  } catch (cause) {
    throw new BuilderInvariantError(
      `Invalid ${fieldName} input CBOR`,
      cause instanceof Error ? cause.message : String(cause),
    );
  }
};

const decodeOrderedInputOutRefs = (
  preimageCbor: Uint8Array,
  fieldName: string,
): readonly OutRef[] => {
  const refs = decodeMidgardNativeInputsPreimageAsCbor(
    preimageCbor,
    fieldName,
  ).map((inputCbor, index) =>
    outRefFromCbor(inputCbor, `${fieldName}[${index.toString()}]`),
  );
  const seen = new Map<string, number>();
  let previous: OutRef | undefined;
  refs.forEach((ref, index) => {
    const label = outRefLabel(ref);
    const firstIndex = seen.get(label);
    if (firstIndex !== undefined) {
      throw new BuilderInvariantError(
        `Duplicate ${fieldName} input`,
        `${fieldName}[${index.toString()}] duplicates ${fieldName}[${firstIndex.toString()}]: ${label}`,
      );
    }
    seen.set(label, index);
    if (previous !== undefined && compareOutRefs(previous, ref) >= 0) {
      throw new BuilderInvariantError(
        `${fieldName} must be lexicographically ordered`,
        `${fieldName}[${index.toString()}]=${label} must sort after ${fieldName}[${(index - 1).toString()}]=${outRefLabel(previous)}`,
      );
    }
    previous = ref;
  });
  return refs;
};

const validatedNativeInputs = (
  tx: MidgardNativeTxFull,
): ValidatedNativeInputs => {
  const spendInputRefs = decodeOrderedInputOutRefs(
    tx.body.spendInputsPreimage,
    "native.spend_inputs",
  );
  const referenceInputRefs = decodeOrderedInputOutRefs(
    tx.body.referenceInputsPreimage,
    "native.reference_inputs",
  );
  const spendLabels = new Map(
    spendInputRefs.map((ref, index) => [outRefLabel(ref), index]),
  );
  for (const [index, ref] of referenceInputRefs.entries()) {
    const label = outRefLabel(ref);
    const spendIndex = spendLabels.get(label);
    if (spendIndex !== undefined) {
      throw new BuilderInvariantError(
        "Input cannot be both native spend and reference input",
        `native.reference_inputs[${index.toString()}] overlaps native.spend_inputs[${spendIndex.toString()}]: ${label}`,
      );
    }
  }
  return { spendInputRefs, referenceInputRefs };
};

export const nativeInputOutRefs = (
  tx: MidgardNativeTxFull,
): readonly OutRef[] =>
  validatedNativeInputs(tx).spendInputRefs;

const nativeOutputBytes = (tx: MidgardNativeTxFull): readonly Buffer[] =>
  decodeMidgardNativeByteListPreimage(
    tx.body.outputsPreimage,
    "native.outputs",
  );

const validatedNativeOutputs = (
  tx: MidgardNativeTxFull,
  expectedNetworkId: number | undefined,
): readonly ValidatedNativeOutput[] =>
  nativeOutputBytes(tx).map((outputCbor, index) => {
    let decoded: ReturnType<typeof decodeMidgardTxOutput>;
    try {
      decoded = decodeMidgardTxOutput(outputCbor);
    } catch (cause) {
      throw new BuilderInvariantError(
        `Invalid native output at index ${index.toString()}`,
        cause instanceof Error ? cause.message : String(cause),
      );
    }
    assertImportedAddressNetwork(
      decoded.address,
      expectedNetworkId,
      `native.outputs[${index.toString()}]`,
    );
    return { outputCbor, decoded };
  });

const requiredSignerKeyHashesFromTx = (
  tx: MidgardNativeTxFull,
): readonly string[] =>
  decodeMidgardNativeHash28ListPreimage(
    tx.body.requiredSignersPreimage,
    "native.required_signers",
  ).map((bytes, index) =>
    normalizeHashHex(
      bytes.toString("hex"),
      `native.required_signers[${index.toString()}]`,
      28,
    ),
  );

const protectedOutputKeyHashesFromTx = (
  outputs: readonly ValidatedNativeOutput[],
): readonly string[] =>
  outputs.flatMap(({ decoded }) => {
    if (!outputAddressProtected(decoded.address)) {
      return [];
    }
    const keyHash = outputAddressPaymentKeyHash(decoded.address);
    return keyHash === undefined ? [] : [keyHash];
  });

const normalizeResolvedSpendInputs = (
  spendRefs: readonly OutRef[],
  options: FromTxOptions,
  normalizeUtxo: UtxoNormalizer,
  expectedNetworkId: number | undefined,
): {
  readonly inputs: readonly MidgardUtxo[];
  readonly complete: boolean;
} => {
  if (options.resolvedSpendInputs === undefined) {
    return { inputs: [], complete: spendRefs.length === 0 };
  }

  const required = new Set(spendRefs.map(outRefLabel));
  const byLabel = new Map<string, MidgardUtxo>();
  for (const [index, input] of options.resolvedSpendInputs.entries()) {
    const normalized = normalizeUtxo(input);
    assertImportedAddressNetwork(
      utxoAddress(normalized),
      expectedNetworkId,
      `resolvedSpendInputs[${index.toString()}]`,
    );
    const label = outRefLabel(normalized);
    if (byLabel.has(label)) {
      throw new BuilderInvariantError("Duplicate resolved spend input", label);
    }
    if (!required.has(label) && !options.allowUnexpectedResolvedInputs) {
      throw new BuilderInvariantError("Unexpected resolved spend input", label);
    }
    byLabel.set(label, normalized);
  }

  const ordered: MidgardUtxo[] = [];
  for (const ref of spendRefs) {
    const label = outRefLabel(ref);
    const input = byLabel.get(label);
    if (input === undefined) {
      throw new BuilderInvariantError("Missing resolved spend input", label);
    }
    ordered.push(input);
  }
  return { inputs: ordered, complete: true };
};

const importedExpectedWitnesses = (
  tx: MidgardNativeTxFull,
  options: FromTxOptions,
  normalizeUtxo: UtxoNormalizer,
  expectedNetworkId: number | undefined,
  inputs: ValidatedNativeInputs,
  outputs: readonly ValidatedNativeOutput[],
): {
  readonly keyHashes: readonly string[];
  readonly complete: boolean;
} => {
  const keyHashes = new Set<string>(requiredSignerKeyHashesFromTx(tx));
  for (const keyHash of protectedOutputKeyHashesFromTx(outputs)) {
    keyHashes.add(keyHash);
  }
  const resolved = normalizeResolvedSpendInputs(
    inputs.spendInputRefs,
    options,
    normalizeUtxo,
    expectedNetworkId,
  );
  for (const input of resolved.inputs) {
    const keyHash = paymentPubKeyHashFromUtxo(input);
    if (keyHash !== undefined) {
      keyHashes.add(keyHash);
    }
  }
  return {
    keyHashes: [...keyHashes].sort(),
    complete: resolved.complete,
  };
};

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
    tx = decodeMidgardNativeTxFull(bytes);
  } catch (cause) {
    throw new BuilderInvariantError(
      "fromTx accepts only Midgard native transaction envelope bytes",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
  const canonical = encodeMidgardNativeTxFull(tx);
  if (!canonical.equals(Buffer.from(bytes))) {
    throw new BuilderInvariantError(
      "fromTx requires canonical Midgard native transaction envelope bytes",
    );
  }
  return tx;
};

const canonicalImportedTxFromObject = (
  tx: MidgardNativeTxFull,
): MidgardNativeTxFull => {
  try {
    verifyMidgardNativeTxFullConsistency(tx);
    return decodeMidgardNativeTxFull(encodeMidgardNativeTxFull(tx));
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
  const txBytes = encodeMidgardNativeTxFull(tx);
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
