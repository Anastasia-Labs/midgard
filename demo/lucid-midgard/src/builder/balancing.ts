import {
  encodeMidgardNativeTxFull,
  materializeMidgardNativeTxFromCanonical,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";

import {
  addAssets,
  isZeroAssets,
  normalizeAssets,
  subtractAssets,
  type Assets,
} from "../core/assets.js";
import {
  BuilderInvariantError,
  InsufficientFundsError,
} from "../core/errors.js";
import { compareOutRefs, outRefLabel } from "../core/out-ref.js";
import {
  authoredOutput,
  utxoAssets as utxoOutputAssets,
  utxoProtectedAddress,
} from "../core/output.js";
import type { Address, MidgardUtxo, WalletInputSource } from "../core/types.js";
import { cloneOutput, cloneUtxo } from "./state.js";
import {
  buildCanonicalUnsignedTx,
  type ScriptMaterialization,
} from "./unsigned-tx.js";
import { estimatedSignedTxByteLength } from "./witness-bundle.js";
import {
  expectedAddrWitnessKeyHashes,
  type CompleteTxMetadata,
} from "./metadata.js";
import type { BuilderState } from "./context.js";

export type ResolvedWalletInputs = {
  readonly source: WalletInputSource;
  readonly inputs: readonly MidgardUtxo[];
  readonly overrideGeneration?: number;
};

export type BalancedCompletionInputs = {
  readonly changeAddress: Address;
  readonly feePolicy: FeePolicy;
  readonly walletInputs: ResolvedWalletInputs;
};

export type FeePolicy = {
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
};

type MintLike = {
  readonly policyId: string;
  readonly assets: Assets;
};

export const mintDeltaAssets = (mints: readonly MintLike[]): Assets => {
  let total: Assets = {};
  for (const mint of mints) {
    total = addAssets(
      total,
      Object.fromEntries(
        Object.entries(mint.assets).map(([assetName, quantity]) => [
          `${mint.policyId}${assetName}`,
          quantity,
        ]),
      ),
    );
  }
  return total;
};

const splitSignedAssets = (
  assets: Assets,
): { readonly positive: Assets; readonly negative: Assets } => {
  const positive: Record<string, bigint> = {};
  const negative: Record<string, bigint> = {};
  for (const [unit, amount] of Object.entries(normalizeAssets(assets))) {
    if (amount > 0n) {
      positive[unit] = amount;
    } else if (amount < 0n) {
      negative[unit] = -amount;
    }
  }
  return { positive, negative };
};

const subtractAssetsFloor = (left: Assets, right: Assets): Assets => {
  const result: Record<string, bigint> = {};
  for (const [unit, amount] of Object.entries(normalizeAssets(left))) {
    const remaining = amount - BigInt(right[unit] ?? 0n);
    if (remaining > 0n) {
      result[unit] = remaining;
    }
  }
  return result;
};

const addFeeToAssets = (assets: Assets, fee: bigint): Assets =>
  fee === 0n ? assets : addAssets(assets, { lovelace: fee });

const balanceSides = ({
  inputs,
  outputs,
  fee,
  mintDelta,
}: {
  readonly inputs: Assets;
  readonly outputs: Assets;
  readonly fee: bigint;
  readonly mintDelta: Assets;
}): { readonly available: Assets; readonly required: Assets } => {
  const mint = splitSignedAssets(mintDelta);
  return {
    available: addAssets(inputs, mint.positive),
    required: addAssets(addFeeToAssets(outputs, fee), mint.negative),
  };
};

export const sumAssets = (items: readonly Assets[]): Assets =>
  items.reduce<Assets>((acc, assets) => addAssets(acc, assets), {});

const jsonAssets = (assets: Assets): Record<string, string> =>
  Object.fromEntries(
    Object.entries(assets).map(([unit, amount]) => [unit, amount.toString(10)]),
  );

export const assertBalancedWithoutChange = (
  inputTotal: Assets,
  outputTotal: Assets,
  fee: bigint,
  mintDelta: Assets,
): void => {
  const { available, required } = balanceSides({
    inputs: inputTotal,
    outputs: outputTotal,
    fee,
    mintDelta,
  });
  const remainder = subtractAssets(available, required);
  if (!isZeroAssets(remainder)) {
    throw new BuilderInvariantError(
      "Explicit completion requires balanced inputs, outputs, and fee",
      JSON.stringify({ remainder: jsonAssets(remainder) }, null, 2),
    );
  }
};

const requiredAssetsWithFee = (outputsTotal: Assets, fee: bigint): Assets =>
  fee === 0n ? outputsTotal : addAssets(outputsTotal, { lovelace: fee });

const utxoAssets = (inputs: readonly MidgardUtxo[]): Assets =>
  sumAssets(inputs.map(utxoOutputAssets));

const markFeeIncluded = (cause: InsufficientFundsError, fee: bigint): never => {
  throw new InsufficientFundsError({
    unit: cause.unit,
    required: cause.required,
    available: cause.available,
    feeIncluded: fee > 0n,
  });
};

const subtractAssetsWithFeeContext = (
  left: Assets,
  right: Assets,
  fee: bigint,
): Assets => {
  try {
    return subtractAssets(left, right);
  } catch (cause) {
    if (cause instanceof InsufficientFundsError) {
      markFeeIncluded(cause, fee);
    }
    throw cause;
  }
};

const trySubtractAssets = (left: Assets, right: Assets): Assets | undefined => {
  try {
    return subtractAssets(left, right);
  } catch (cause) {
    if (cause instanceof InsufficientFundsError) {
      return undefined;
    }
    throw cause;
  }
};

const assetCoverageScore = (
  assets: Assets,
  required: Assets,
): {
  readonly tokenKinds: number;
  readonly tokenQuantity: bigint;
  readonly lovelace: bigint;
} => {
  let tokenKinds = 0;
  let tokenQuantity = 0n;
  for (const [unit, requiredQuantity] of Object.entries(required)) {
    if (unit === "lovelace") {
      continue;
    }
    const available = BigInt(assets[unit] ?? 0n);
    if (available > 0n) {
      tokenKinds += 1;
      tokenQuantity +=
        available < requiredQuantity ? available : requiredQuantity;
    }
  }
  return {
    tokenKinds,
    tokenQuantity,
    lovelace: BigInt(assets.lovelace ?? 0n),
  };
};

const compareUtxosByCoverage = (
  left: MidgardUtxo,
  right: MidgardUtxo,
  required: Assets,
): number => {
  const leftScore = assetCoverageScore(utxoOutputAssets(left), required);
  const rightScore = assetCoverageScore(utxoOutputAssets(right), required);
  if (leftScore.tokenKinds !== rightScore.tokenKinds) {
    return rightScore.tokenKinds - leftScore.tokenKinds;
  }
  if (leftScore.tokenQuantity !== rightScore.tokenQuantity) {
    return leftScore.tokenQuantity > rightScore.tokenQuantity ? -1 : 1;
  }
  if (leftScore.lovelace !== rightScore.lovelace) {
    return leftScore.lovelace > rightScore.lovelace ? -1 : 1;
  }
  return compareOutRefs(left, right);
};

const selectDeterministicInputs = (
  explicitInputs: readonly MidgardUtxo[],
  candidateInputs: readonly MidgardUtxo[],
  required: Assets,
  fee: bigint,
): readonly MidgardUtxo[] => {
  const selected = [...explicitInputs].sort(compareOutRefs);
  const selectedLabels = new Set(selected.map(outRefLabel));
  const candidates = [...candidateInputs]
    .filter((candidate) => !selectedLabels.has(outRefLabel(candidate)))
    .filter((candidate) => !utxoProtectedAddress(candidate))
    .sort((left, right) => compareUtxosByCoverage(left, right, required));

  for (const candidate of candidates) {
    if (trySubtractAssets(utxoAssets(selected), required) !== undefined) {
      break;
    }
    selected.push(candidate);
    selectedLabels.add(outRefLabel(candidate));
  }

  if (trySubtractAssets(utxoAssets(selected), required) === undefined) {
    subtractAssetsWithFeeContext(utxoAssets(selected), required, fee);
  }

  return selected;
};

export const assertNoPresetInputOverlap = (
  explicitInputs: readonly MidgardUtxo[],
  presetInputs: readonly MidgardUtxo[],
): void => {
  const explicit = new Set(explicitInputs.map(outRefLabel));
  for (const input of presetInputs) {
    const label = outRefLabel(input);
    if (explicit.has(label)) {
      throw new BuilderInvariantError(
        "Completion preset wallet input duplicates an explicit spend input",
        label,
      );
    }
  }
};

const addChangeOutput = (
  state: BuilderState,
  changeAddress: Address,
  changeAssets: Assets,
): {
  readonly outputs: BuilderState["outputs"];
  readonly changeOutputIndex?: number;
} =>
  isZeroAssets(changeAssets)
    ? { outputs: state.outputs.map(cloneOutput) }
    : {
        outputs: [
          ...state.outputs.map(cloneOutput),
          authoredOutput({
            kind: "ordinary",
            address: changeAddress,
            value: changeAssets,
          }),
        ],
        changeOutputIndex: state.outputs.length,
      };

const maxBigInt = (left: bigint, right: bigint): bigint =>
  left > right ? left : right;

export const buildBalancedCompletion = ({
  state,
  resolved,
  initialFee,
  maxFeeIterations,
  deriveScriptMaterialization,
}: {
  readonly state: BuilderState;
  readonly resolved: BalancedCompletionInputs;
  readonly initialFee: bigint;
  readonly maxFeeIterations: number;
  readonly deriveScriptMaterialization: (
    state: BuilderState,
  ) => ScriptMaterialization;
}): {
  readonly tx: MidgardNativeTxFull;
  readonly metadata: Omit<CompleteTxMetadata, "localValidation">;
} => {
  const { changeAddress, feePolicy, walletInputs } = resolved;
  const candidateInputs = walletInputs.inputs;
  const outputsTotal = sumAssets(state.outputs.map((output) => output.assets));
  const mintDelta = mintDeltaAssets(state.scripts.mints);
  const mint = splitSignedAssets(mintDelta);
  let selectedInputs = [...state.spendInputs].sort(compareOutRefs);
  let fee = initialFee;

  for (let iteration = 1; iteration <= maxFeeIterations; iteration += 1) {
    const required = subtractAssetsFloor(
      addAssets(requiredAssetsWithFee(outputsTotal, fee), mint.negative),
      mint.positive,
    );
    selectedInputs = [
      ...selectDeterministicInputs(
        selectedInputs,
        candidateInputs,
        required,
        fee,
      ),
    ];
    const sides = balanceSides({
      inputs: utxoAssets(selectedInputs),
      outputs: outputsTotal,
      fee,
      mintDelta,
    });
    const changeAssets = subtractAssetsWithFeeContext(
      sides.available,
      sides.required,
      fee,
    );
    const { outputs, changeOutputIndex } = addChangeOutput(
      state,
      changeAddress,
      changeAssets,
    );
    const candidateState: BuilderState = {
      ...state,
      spendInputs: selectedInputs.map(cloneUtxo),
      outputs,
    };
    const candidateTx = materializeMidgardNativeTxFromCanonical(
      buildCanonicalUnsignedTx(
        candidateState,
        fee,
        deriveScriptMaterialization(candidateState),
      ),
    );
    const expectedWitnessKeyHashes =
      expectedAddrWitnessKeyHashes(candidateState);
    const expectedAddrWitnessCount = expectedWitnessKeyHashes.length;
    const estimatedSignedLength = estimatedSignedTxByteLength(
      candidateTx,
      expectedAddrWitnessCount,
    );
    const nextFee = maxBigInt(
      feePolicy.minFeeA * BigInt(estimatedSignedLength) + feePolicy.minFeeB,
      state.minimumFee ?? 0n,
    );
    if (nextFee === fee) {
      return {
        tx: candidateTx,
        metadata: {
          fee,
          inputCount: candidateState.spendInputs.length,
          referenceInputCount: candidateState.referenceInputs.length,
          outputCount: candidateState.outputs.length,
          requiredSignerCount: candidateState.requiredSigners.length,
          txByteLength: encodeMidgardNativeTxFull(candidateTx).length,
          feeIterations: iteration,
          balanced: true,
          changeAddress,
          changeAssets,
          changeOutputIndex,
          expectedAddrWitnessCount,
          expectedAddrWitnessKeyHashes: expectedWitnessKeyHashes,
          estimatedSignedTxByteLength: estimatedSignedLength,
          walletInputSource: walletInputs.source,
          walletInputCount: candidateInputs.length,
          utxoOverrideGeneration: walletInputs.overrideGeneration,
        },
      };
    }
    fee = nextFee;
  }

  throw new BuilderInvariantError(
    "Fee convergence failed",
    `maxFeeIterations=${maxFeeIterations.toString()}`,
  );
};
