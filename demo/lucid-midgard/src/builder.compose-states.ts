import { assetsEqual } from "@al-ft/midgard-core/assets";
import { normalizeHex } from "@al-ft/midgard-core/hex";

import { CompleteTx } from "./builder.complete-tx.js";
import type { BuilderState } from "./builder/context.js";
import { type ImportedTxInput } from "./builder/imported-tx.js";
import {
  assertNoDuplicateStrings,
  assertUniqueUtxos,
  cloneOutput,
  cloneScripts,
  cloneUtxo,
} from "./builder/state.js";
import { BuilderInvariantError } from "./core/errors.js";
import { normalizeOutRef, outRefLabel } from "./core/out-ref.js";
import {
  decodeMidgardTxOutput,
  utxoAddress,
  utxoOutputCbor,
  utxoOutRefCbor,
} from "./core/output.js";
import type {
  BuilderScriptState,
  TrustedReferenceScriptMetadata,
} from "./core/scripts.js";
import { type MidgardUtxo, type WalletInputSource } from "./core/types.js";
import { assertAddressNetwork } from "./wallet.js";

export type FromTxInput = CompleteTx | ImportedTxInput;

export type ChainResult = readonly [
  newWalletUtxos: readonly MidgardUtxo[],
  derivedOutputs: readonly MidgardUtxo[],
  tx: CompleteTx,
];

export type UtxosByOutRefOptions = {
  readonly missing?: "error" | "omit";
};

export type DatumOfOptions =
  | {
      readonly expectedHash?: string;
    }
  | string;

export const referenceOutputMapsEqual = (
  left: ReadonlyMap<string, Uint8Array>,
  right: ReadonlyMap<string, Uint8Array>,
): boolean => {
  if (left.size !== right.size) return false;
  for (const [key, value] of left) {
    const other = right.get(key);
    if (other === undefined || !Buffer.from(value).equals(Buffer.from(other))) {
      return false;
    }
  }
  return true;
};

export type ReadFromOptions = {
  readonly trustedReferenceScripts?: readonly TrustedReferenceScriptMetadata[];
};

const maxOptionalBigInt = (
  left: bigint | undefined,
  right: bigint | undefined,
): bigint | undefined =>
  left === undefined
    ? right
    : right === undefined
      ? left
      : left > right
        ? left
        : right;

const minOptionalBigInt = (
  left: bigint | undefined,
  right: bigint | undefined,
): bigint | undefined =>
  left === undefined
    ? right
    : right === undefined
      ? left
      : left < right
        ? left
        : right;

const assertComposableScriptState = (scripts: BuilderScriptState): void => {
  assertNoDuplicateStrings(
    scripts.datumWitnesses
      .map((datum) => datum.hash)
      .filter((hash): hash is string => hash !== undefined),
    "Duplicate datum witness",
  );
  assertNoDuplicateStrings(
    scripts.observers.map((observer) => observer.scriptHash),
    "Duplicate observer intent",
  );
  assertNoDuplicateStrings(
    scripts.receiveRedeemers.map((entry) => entry.scriptHash),
    "Duplicate receive redeemer",
  );
  assertNoDuplicateStrings(
    scripts.spendRedeemers.map(outRefLabel),
    "Duplicate spend redeemer",
  );
  assertNoDuplicateStrings(
    scripts.referenceScriptMetadata.map(outRefLabel),
    "Duplicate trusted reference script metadata",
  );
  assertNoDuplicateStrings(
    scripts.mints
      .filter((mint) => mint.redeemer !== undefined)
      .map((mint) => mint.policyId),
    "Duplicate mint redeemer for policy",
  );
};

export const composeStates = (
  states: readonly BuilderState[],
): BuilderState => {
  const [first, ...rest] = states;
  if (first === undefined) {
    throw new BuilderInvariantError("compose requires at least one builder");
  }
  let validityIntervalStart = first.validityIntervalStart;
  let validityIntervalEnd = first.validityIntervalEnd;
  let minimumFee = first.minimumFee;
  for (const state of rest) {
    if (state.networkId !== first.networkId) {
      throw new BuilderInvariantError(
        "Cannot compose builders with different state network ids",
        `left=${String(first.networkId)} right=${String(state.networkId)}`,
      );
    }
    validityIntervalStart = maxOptionalBigInt(
      validityIntervalStart,
      state.validityIntervalStart,
    );
    validityIntervalEnd = minOptionalBigInt(
      validityIntervalEnd,
      state.validityIntervalEnd,
    );
    minimumFee = maxOptionalBigInt(minimumFee, state.minimumFee);
  }
  const requiredSigners = states.flatMap((state) => state.requiredSigners);
  assertNoDuplicateStrings(requiredSigners, "Duplicate required signer");
  const scriptStates = states.map((state) => cloneScripts(state.scripts));
  const scripts: BuilderScriptState = {
    spendRedeemers: scriptStates.flatMap((scripts) => scripts.spendRedeemers),
    referenceScriptMetadata: scriptStates.flatMap(
      (scripts) => scripts.referenceScriptMetadata,
    ),
    scripts: scriptStates.flatMap((scripts) => scripts.scripts),
    datumWitnesses: scriptStates.flatMap((scripts) => scripts.datumWitnesses),
    mints: scriptStates.flatMap((scripts) => scripts.mints),
    observers: scriptStates.flatMap((scripts) => scripts.observers),
    receiveRedeemers: scriptStates.flatMap(
      (scripts) => scripts.receiveRedeemers,
    ),
  };
  assertComposableScriptState(scripts);
  const composed: BuilderState = {
    spendInputs: states.flatMap((state) => state.spendInputs.map(cloneUtxo)),
    referenceInputs: states.flatMap((state) =>
      state.referenceInputs.map(cloneUtxo),
    ),
    outputs: states.flatMap((state) => state.outputs.map(cloneOutput)),
    requiredSigners,
    validityIntervalStart,
    validityIntervalEnd,
    minimumFee,
    networkId: first.networkId,
    scripts,
    composition: {
      fragmentCount: states.reduce(
        (count, state) => count + (state.composition?.fragmentCount ?? 1),
        0,
      ),
    },
  };
  assertUniqueUtxos(composed.spendInputs, composed.referenceInputs);
  assertValidityInterval(composed);
  return composed;
};

const hasDefinedProperty = <K extends PropertyKey>(
  value: object,
  property: K,
): value is object & Record<K, unknown> =>
  Object.prototype.hasOwnProperty.call(value, property) &&
  (value as Record<K, unknown>)[property] !== undefined;

const scriptHex = (
  scriptRef: NonNullable<MidgardUtxo["output"]["scriptRef"]>,
): string => {
  try {
    return normalizeHex(scriptRef.script, {
      fieldName: "scriptRef.script",
      allowEmpty: true,
    });
  } catch {
    throw new BuilderInvariantError("UTxO scriptRef script must be hex");
  }
};

const scriptRefsCompatible = (
  supplied: MidgardUtxo["output"]["scriptRef"],
  decoded: MidgardUtxo["output"]["scriptRef"],
): boolean => {
  if (supplied === undefined) {
    return true;
  }
  if (supplied === null) {
    return decoded === undefined || decoded === null;
  }
  if (decoded === undefined || decoded === null) {
    return false;
  }
  return (
    scriptHex(supplied) === scriptHex(decoded) && supplied.type === decoded.type
  );
};

export const normalizeUtxo = (utxo: MidgardUtxo): MidgardUtxo => {
  const normalized = normalizeOutRef(utxo);
  const outRefCbor = utxoOutRefCbor(utxo);

  let decodedOutput: ReturnType<typeof decodeMidgardTxOutput>;
  try {
    decodedOutput = decodeMidgardTxOutput(utxoOutputCbor(utxo));
  } catch (cause) {
    throw new BuilderInvariantError(
      "Invalid UTxO outputCbor",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
  if (utxo.output.address !== decodedOutput.address) {
    throw new BuilderInvariantError("UTxO address does not match output CBOR");
  }
  if (!assetsEqual(utxo.output.assets, decodedOutput.assets)) {
    throw new BuilderInvariantError("UTxO assets do not match output CBOR");
  }
  const datum = hasDefinedProperty(utxo.output, "datum")
    ? utxo.output.datum
    : undefined;
  const decodedDatum = decodedOutput.txOutput.datum;
  if (
    datum !== undefined &&
    JSON.stringify(datum) !== JSON.stringify(decodedDatum ?? null)
  ) {
    throw new BuilderInvariantError("UTxO datum does not match output CBOR");
  }
  const scriptRef = hasDefinedProperty(utxo.output, "scriptRef")
    ? utxo.output.scriptRef
    : undefined;
  if (!scriptRefsCompatible(scriptRef, decodedOutput.txOutput.scriptRef)) {
    throw new BuilderInvariantError(
      "UTxO scriptRef does not match output CBOR",
    );
  }

  return {
    txHash: normalized.txHash,
    outputIndex: normalized.outputIndex,
    output: {
      address: decodedOutput.txOutput.address,
      assets: { ...decodedOutput.txOutput.assets },
      datum: datum === undefined ? decodedOutput.txOutput.datum : datum,
      scriptRef:
        scriptRef === undefined ? decodedOutput.txOutput.scriptRef : scriptRef,
    },
    cbor: {
      outRef: outRefCbor,
      output: Buffer.from(decodedOutput.outputCbor),
    },
  };
};

export const normalizeWalletInputUtxos = (
  utxos: readonly MidgardUtxo[],
  source: WalletInputSource,
  expectedNetworkId: number | undefined,
): readonly MidgardUtxo[] => {
  const seen = new Set<string>();
  return utxos.map((utxo) => {
    const normalized = normalizeUtxo(utxo);
    assertAddressNetwork(utxoAddress(normalized), expectedNetworkId);
    const label = outRefLabel(normalized);
    if (seen.has(label)) {
      throw new BuilderInvariantError(`Duplicate ${source} UTxO outref`, label);
    }
    seen.add(label);
    return normalized;
  });
};

export const normalizeSignerKeyHash = (signer: string): string | undefined => {
  try {
    return normalizeHex(signer, {
      fieldName: "required signer key hash",
      byteLength: 28,
    });
  } catch {
    return undefined;
  }
};

export const assertValidityInterval = (state: BuilderState): void => {
  if (
    state.validityIntervalStart !== undefined &&
    state.validityIntervalEnd !== undefined &&
    state.validityIntervalStart > state.validityIntervalEnd
  ) {
    throw new BuilderInvariantError(
      "validityIntervalStart must be less than or equal to validityIntervalEnd",
      `${state.validityIntervalStart.toString()} > ${state.validityIntervalEnd.toString()}`,
    );
  }
};
