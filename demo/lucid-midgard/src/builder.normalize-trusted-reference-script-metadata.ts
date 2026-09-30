import { type LocalPreflightPhase } from "./builder.midgard-json-safe.js";
import { type FeePolicy } from "./builder/balancing.js";
import type { BuilderState } from "./builder/context.js";
import { paymentPubKeyHashFromUtxo } from "./builder/metadata.js";
import {
  normalizeHashHex,
  normalizeNonNegativeBigInt,
} from "./builder/normalizers.js";
import {
  normalizeScriptHash,
  normalizeScriptLanguage,
} from "./builder/script-materialization.js";
import { BuilderInvariantError } from "./core/errors.js";
import { normalizeOutRef, type OutRef, outRefLabel } from "./core/out-ref.js";
import type { TrustedReferenceScriptMetadata } from "./core/scripts.js";
import {
  type CompleteOptions,
  type LocalValidationReport,
  type MidgardUtxo,
  type WalletInputSource,
} from "./core/types.js";
import { type MidgardWallet } from "./wallet.js";

export const rejectRuntimeKindOption = (
  options: object | undefined,
  methodName: string,
): void => {
  if (options !== undefined && "kind" in options) {
    throw new BuilderInvariantError(
      `${methodName} does not accept an output kind option; use the explicit helper`,
    );
  }
};

const normalizeTrustedReferenceScriptMetadata = (
  metadata: TrustedReferenceScriptMetadata,
): TrustedReferenceScriptMetadata => {
  if (typeof metadata !== "object" || metadata === null) {
    throw new BuilderInvariantError(
      "Reference script metadata must be an object",
    );
  }
  const candidate = metadata as {
    readonly txHash?: unknown;
    readonly outputIndex?: unknown;
    readonly language?: unknown;
    readonly scriptHash?: unknown;
    readonly scriptCborHash?: unknown;
  };
  if (typeof candidate.txHash !== "string") {
    throw new BuilderInvariantError(
      "Reference script metadata txHash must be a string",
    );
  }
  if (
    typeof candidate.outputIndex !== "number" ||
    !Number.isSafeInteger(candidate.outputIndex)
  ) {
    throw new BuilderInvariantError(
      "Reference script metadata outputIndex must be a safe integer",
    );
  }
  if (typeof candidate.scriptHash !== "string") {
    throw new BuilderInvariantError(
      "Reference script metadata scriptHash must be a string",
    );
  }
  if (
    candidate.scriptCborHash !== undefined &&
    typeof candidate.scriptCborHash !== "string"
  ) {
    throw new BuilderInvariantError(
      "Reference script metadata scriptCborHash must be a string when present",
    );
  }
  const outRef = normalizeOutRef({
    txHash: candidate.txHash,
    outputIndex: candidate.outputIndex,
  } as OutRef);
  return {
    ...outRef,
    language: normalizeScriptLanguage(
      candidate.language,
      "Reference script metadata language",
    ),
    scriptHash: normalizeScriptHash(
      candidate.scriptHash,
      "reference script metadata scriptHash",
    ),
    scriptCborHash:
      candidate.scriptCborHash === undefined
        ? undefined
        : normalizeHashHex(
            candidate.scriptCborHash,
            "reference script metadata scriptCborHash",
            32,
          ),
  };
};

export const normalizeTrustedReferenceScriptMetadataList = (
  metadata:
    | TrustedReferenceScriptMetadata
    | readonly TrustedReferenceScriptMetadata[],
): readonly TrustedReferenceScriptMetadata[] => {
  const entries = Array.isArray(metadata) ? metadata : [metadata];
  return entries.map(normalizeTrustedReferenceScriptMetadata);
};

export const assertWalletOwnsInputs = async (
  wallet: MidgardWallet | undefined,
  inputs: readonly MidgardUtxo[],
  source: WalletInputSource,
): Promise<void> => {
  if (inputs.length === 0) {
    return;
  }
  if (wallet === undefined) {
    throw new BuilderInvariantError(
      `${source} wallet inputs require a selected wallet`,
    );
  }
  const walletKeyHash = await wallet.keyHash();
  for (const input of inputs) {
    const inputKeyHash = paymentPubKeyHashFromUtxo(input);
    if (inputKeyHash === undefined) {
      throw new BuilderInvariantError(
        `${source} wallet input is not spendable by a public-key wallet`,
        outRefLabel(input),
      );
    }
    if (inputKeyHash !== walletKeyHash) {
      throw new BuilderInvariantError(
        `${source} wallet input does not belong to the selected wallet`,
        `outref=${outRefLabel(input)} wallet_key_hash=${walletKeyHash} input_key_hash=${inputKeyHash}`,
      );
    }
  }
};

const shouldBalance = (options: CompleteOptions): boolean =>
  options.changeAddress !== undefined ||
  options.feePolicy !== undefined ||
  options.maxFeeIterations !== undefined;

export const shouldBalanceWithWalletDefault = (
  options: CompleteOptions,
  hasSelectedWallet: boolean,
): boolean => shouldBalance(options) || hasSelectedWallet;

export const resolveMaxFeeIterations = (value: number | undefined): number => {
  if (value === undefined) {
    return 20;
  }
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new BuilderInvariantError(
      "maxFeeIterations must be a non-negative safe integer",
    );
  }
  return value;
};

export const normalizeFeePolicy = (feePolicy: {
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
}): FeePolicy => ({
  minFeeA: normalizeNonNegativeBigInt(feePolicy.minFeeA, "minFeeA"),
  minFeeB: normalizeNonNegativeBigInt(feePolicy.minFeeB, "minFeeB"),
});

const maxBigInt = (left: bigint, right: bigint): bigint =>
  left > right ? left : right;

export const resolveInitialFee = (
  state: BuilderState,
  fee: bigint | number | undefined,
): bigint =>
  maxBigInt(
    normalizeNonNegativeBigInt(fee ?? 0n, "fee"),
    state.minimumFee ?? 0n,
  );

export const validationLevel = (
  options: CompleteOptions,
): "none" | LocalPreflightPhase => options.localValidation ?? "none";

export const assertAcceptedLocalValidation = (
  report: LocalValidationReport,
  txIdHex: string,
): void => {
  if (
    report.rejected.length === 0 &&
    report.acceptedTxIds.length === 1 &&
    report.acceptedTxIds[0] === txIdHex
  ) {
    return;
  }
  throw new BuilderInvariantError(
    `Local ${report.phase} validation rejected transaction`,
    JSON.stringify(
      {
        expectedTxId: txIdHex,
        acceptedTxIds: report.acceptedTxIds,
        rejected: report.rejected,
      },
      null,
      2,
    ),
  );
};
