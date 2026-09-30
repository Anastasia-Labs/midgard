import { isZeroAssets, normalizeAssets } from "@al-ft/midgard-core/assets";
import {
  decodeMidgardAddressBytes,
  encodeMidgardAddressText,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import { type Assets } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  parseAdditionalAssetSpecs,
  parseLovelaceAmount,
} from "../asset-specs.js";
import { compareOutRefs } from "../tx-context.js";
import {
  defaultMidgardNodeEndpoint,
  fetchNodeUtxosByAddress,
  formatJson,
  type NodeUtxo,
  parseNodeEndpoint,
} from "./command-utils.js";

export type SubmitL2TransferConfig = {
  readonly l2Address: string;
  readonly lovelace: bigint;
  readonly additionalAssets: Readonly<Assets>;
  readonly nodeEndpoint: string;
  readonly submitRequestTimeoutMs?: number;
  readonly utxoRequestTimeoutMs?: number;
  readonly networkId: bigint;
};

export type SubmitL2TransferResult = {
  readonly txId: string;
  readonly status: string;
  readonly senderAddress: string;
  readonly destinationAddress: string;
  readonly selectedInputs: readonly string[];
  readonly requestedAssets: Readonly<Assets>;
  readonly changeAssets: Readonly<Assets>;
  readonly walletSeedSource: string;
  readonly nodeEndpoint: string;
};

export type PreparedL2Transfer = Omit<SubmitL2TransferResult, "status"> & {
  readonly signedTxCbor: string;
};

export type PreparedL2TerminalDrain = {
  readonly txId: string;
  readonly signedTxCbor: string;
  readonly senderAddress: string;
  readonly destinationAddress: string;
  readonly selectedInputs: readonly string[];
  readonly requestedLovelace: bigint;
  readonly feeLovelace: bigint;
  readonly signedTxBytes: number;
};

export type NativeTransferSubmitRetryPolicy = {
  readonly maxAttempts: number;
  readonly initialDelayMs: number;
  readonly maxDelayMs: number;
  readonly sleep?: (delayMs: number) => Promise<void>;
};

/** Fanout-only resilience for commit-ambiguous admission transport failures. */
export const FANOUT_NATIVE_TRANSFER_SUBMIT_RETRY_POLICY = {
  maxAttempts: 3,
  initialDelayMs: 250,
  maxDelayMs: 1_000,
} as const satisfies NativeTransferSubmitRetryPolicy;

export class RetryableNativeTransferSubmitError extends Error {}

export const isDurableAdmissionFailure = (
  status: number,
  body: string,
): boolean => {
  if (status !== 500) return false;
  try {
    const parsed = JSON.parse(body) as { readonly error?: unknown };
    return parsed.error === "durable transaction admission failed";
  } catch {
    return false;
  }
};

export const validateRetryPolicy = (
  policy: NativeTransferSubmitRetryPolicy,
): void => {
  for (const [name, value] of Object.entries({
    maxAttempts: policy.maxAttempts,
    initialDelayMs: policy.initialDelayMs,
    maxDelayMs: policy.maxDelayMs,
  })) {
    if (
      !Number.isSafeInteger(value) ||
      value < (name === "maxAttempts" ? 1 : 0)
    ) {
      throw new Error(
        `Native transfer submit retry ${name} must be ${name === "maxAttempts" ? "a positive" : "a non-negative"} safe integer.`,
      );
    }
  }
};

/**
 * Converts an unknown failure into an `Error` with a stable prefix.
 */
export const toError = (cause: unknown, prefix: string): Error =>
  cause instanceof Error
    ? new Error(`${prefix}: ${cause.message}`)
    : new Error(`${prefix}: ${String(cause)}`);

/**
 * Reduces a required-asset set by the contribution made from one candidate
 * input.
 */
const reduceRequiredAssets = (
  remaining: Readonly<Assets>,
  contribution: Readonly<Assets>,
): Readonly<Assets> => {
  const reduced: Assets = {};
  for (const [unit, missing] of Object.entries(remaining)) {
    if (missing <= 0n) {
      continue;
    }
    const available = contribution[unit] ?? 0n;
    if (available < missing) {
      reduced[unit] = missing - available;
    }
  }
  return reduced;
};

/**
 * Orders candidate inputs by how well they cover the currently missing assets.
 */
const compareAssetsByCoverage = (
  lhs: Readonly<Assets>,
  rhs: Readonly<Assets>,
  required: Readonly<Assets>,
): number => {
  /**
   * Scores candidate UTxOs for transfer-input selection.
   */
  const score = (assets: Readonly<Assets>) => {
    let requiredTokenKinds = 0;
    let requiredTokenQuantity = 0n;
    for (const [unit, amount] of Object.entries(required)) {
      if (unit === "lovelace") {
        continue;
      }
      const present = assets[unit] ?? 0n;
      if (present > 0n) {
        requiredTokenKinds += 1;
        requiredTokenQuantity += present < amount ? present : amount;
      }
    }
    return {
      requiredTokenKinds,
      requiredTokenQuantity,
      lovelace: assets.lovelace ?? 0n,
    };
  };

  const left = score(lhs);
  const right = score(rhs);

  if (left.requiredTokenKinds !== right.requiredTokenKinds) {
    return right.requiredTokenKinds - left.requiredTokenKinds;
  }
  if (left.requiredTokenQuantity !== right.requiredTokenQuantity) {
    return left.requiredTokenQuantity > right.requiredTokenQuantity ? -1 : 1;
  }
  if (left.lovelace !== right.lovelace) {
    return left.lovelace > right.lovelace ? -1 : 1;
  }
  return 0;
};

/**
 * Parses CLI-style transfer arguments into a normalized transfer config.
 */
export const parseSubmitL2TransferConfig = ({
  l2Address,
  lovelace,
  assetSpecs,
  nodeEndpoint,
  submitRequestTimeoutMs,
  utxoRequestTimeoutMs,
}: {
  readonly l2Address: string;
  readonly lovelace: string;
  readonly assetSpecs: readonly string[];
  readonly nodeEndpoint?: string;
  readonly submitRequestTimeoutMs?: number;
  readonly utxoRequestTimeoutMs?: number;
}): SubmitL2TransferConfig => {
  let addressBytes: ReturnType<typeof midgardAddressFromText>;
  try {
    addressBytes = midgardAddressFromText(l2Address);
  } catch (cause) {
    throw new Error(
      `Invalid L2 address "${l2Address.trim()}": ${String(cause)}`,
    );
  }
  const addressDetails = decodeMidgardAddressBytes(addressBytes);

  return {
    l2Address: encodeMidgardAddressText(addressBytes),
    lovelace: parseLovelaceAmount(
      lovelace,
      "Transfer lovelace amount must be greater than zero.",
    ),
    additionalAssets: parseAdditionalAssetSpecs(assetSpecs),
    nodeEndpoint: parseNodeEndpoint(
      nodeEndpoint ?? defaultMidgardNodeEndpoint(),
    ),
    ...(submitRequestTimeoutMs === undefined ? {} : { submitRequestTimeoutMs }),
    ...(utxoRequestTimeoutMs === undefined ? {} : { utxoRequestTimeoutMs }),
    networkId: BigInt(addressDetails.networkId),
  };
};

/**
 * Builds the requested transfer asset map from the normalized command config.
 */
export const buildRequestedAssets = (
  config: SubmitL2TransferConfig,
): Readonly<Assets> => ({
  lovelace: config.lovelace,
  ...config.additionalAssets,
});

/**
 * Selects a deterministic set of wallet inputs that covers the requested asset
 * set.
 */
export const selectTransferInputs = (
  utxos: readonly NodeUtxo[],
  requestedAssets: Readonly<Assets>,
): readonly NodeUtxo[] => {
  const ranked = [...utxos].sort((lhs, rhs) => {
    const coverageComparison = compareAssetsByCoverage(
      lhs.assets,
      rhs.assets,
      requestedAssets,
    );
    if (coverageComparison !== 0) {
      return coverageComparison;
    }
    return compareOutRefs(lhs, rhs);
  });

  const selected: NodeUtxo[] = [];
  let remaining: Readonly<Assets> = normalizeAssets(requestedAssets);

  for (const utxo of ranked) {
    const nextRemaining = reduceRequiredAssets(remaining, utxo.assets);
    if (Object.keys(nextRemaining).length === Object.keys(remaining).length) {
      continue;
    }
    selected.push(utxo);
    remaining = nextRemaining;
    if (isZeroAssets(remaining)) {
      break;
    }
  }

  if (!isZeroAssets(remaining)) {
    throw new Error(
      `Insufficient Midgard L2 funds for requested transfer. Missing ${formatJson(remaining)}.`,
    );
  }

  return selected.sort(compareOutRefs);
};

/**
 * Queries the node's public `/utxos` endpoint and decodes the returned wallet
 * UTxOs.
 */
export const fetchNodeUtxos = (
  nodeEndpoint: string,
  address: string,
  timeoutMs?: number,
): Effect.Effect<readonly NodeUtxo[], Error> =>
  Effect.tryPromise({
    try: () =>
      fetchNodeUtxosByAddress(
        nodeEndpoint,
        address,
        timeoutMs === undefined ? undefined : { timeoutMs },
      ),
    catch: (cause) =>
      new Error(`Failed to fetch Midgard UTxOs: ${String(cause)}`),
  });
