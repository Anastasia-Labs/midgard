import type { MidgardCekProgramMaterialEntry } from "@al-ft/midgard-core/cek-proof";
import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardSpendInputItem,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";

import { BuilderInvariantError } from "../core/errors.js";
import { compareOutRefs, type OutRef, outRefLabel } from "../core/out-ref.js";
import {
  decodeMidgardTxOutput,
  outputAddressPaymentKeyHash,
  outputAddressProtected,
  utxoAddress,
} from "../core/output.js";
import type { MidgardUtxo } from "../core/types.js";
import { assertAddressNetwork } from "../wallet.js";
import { paymentPubKeyHashFromUtxo } from "./metadata.js";

export type ImportedTxInput =
  | MidgardNativeTxFull
  | Uint8Array
  | string
  | { readonly txCbor: Uint8Array | string }
  | { readonly txHex: string };

export type FromTxOptions = {
  readonly resolvedSpendInputs?: readonly MidgardUtxo[];
  readonly resolvedReferenceInputs?: readonly MidgardUtxo[];
  /** Exact canonical V1 sidecar material when importing raw transaction bytes. */
  readonly programMaterial?: readonly MidgardCekProgramMaterialEntry[];
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

export const assertImportedAddressNetwork = (
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

/**
 * Field-0/1 items are §5.3's fixed-index form, so canonicality is decided by
 * that decoder — it asserts the 38-byte width and the `0x19` index head, which
 * is exactly the "one valid byte form" rule (§6.1). A CML round-trip cannot
 * decide it: CML preserves whatever index width it was handed, so a minimal
 * 36-byte item would round-trip equal and pass a check that must reject it.
 */
const outRefFromCbor = (inputCbor: Uint8Array, fieldName: string): OutRef => {
  try {
    const decoded = decodeMidgardSpendInputItem(inputCbor);
    return {
      txHash: Buffer.from(decoded.txId).toString("hex"),
      outputIndex: decoded.outputIndex,
    };
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
  const refs = decodeMidgardNativeByteListPreimage(preimageCbor, fieldName).map(
    (inputCbor, index) =>
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

export const validatedNativeInputs = (
  tx: MidgardNativeTxFull,
): ValidatedNativeInputs => {
  const spendInputRefs = decodeOrderedInputOutRefs(
    tx.body.spendInputsPreimageCbor,
    "native.spend_inputs",
  );
  const referenceInputRefs = decodeOrderedInputOutRefs(
    tx.body.referenceInputsPreimageCbor,
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
): readonly OutRef[] => validatedNativeInputs(tx).spendInputRefs;

const nativeOutputBytes = (tx: MidgardNativeTxFull): readonly Buffer[] =>
  decodeMidgardNativeByteListPreimage(
    tx.body.outputsPreimageCbor,
    "native.outputs",
  );

export const validatedNativeOutputs = (
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

export const requiredSignerKeyHashesFromTx = (
  tx: MidgardNativeTxFull,
): readonly string[] =>
  decodeMidgardNativeByteListPreimage(
    tx.body.requiredSignersPreimageCbor,
    "native.required_signers",
  ).map((bytes, index) => {
    if (bytes.length !== 28) {
      throw new BuilderInvariantError(
        `native.required_signers[${index.toString()}] must be a 28-byte hex string`,
        bytes.toString("hex"),
      );
    }
    return bytes.toString("hex");
  });

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

export const importedExpectedWitnesses = (
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
