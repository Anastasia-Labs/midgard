/**
 * Native user-transfer command support for Midgard L2.
 * This module derives a wallet from a seed phrase, queries the live Midgard
 * ledger view for spendable UTxOs, constructs a balanced Midgard-native
 * transaction with explicit change, and submits it through the node's public
 * `/submit` endpoint.
 */
import { isZeroAssets, normalizeAssets } from "@al-ft/midgard-core/assets";
import {
  encodeMidgardCekProgramMaterialSidecarV1,
  encodeMidgardProofSubmissionV1,
} from "@al-ft/midgard-core/cek-proof";
import {
  decodeMidgardAddressBytes,
  encodeMidgardAddressText,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import {
  processedTxFromValidatedTx,
  type QueuedTx,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import {
  type Assets,
  getAddressDetails,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  parseAdditionalAssetSpecs,
  parseLovelaceAmount,
} from "@/asset-specs.js";
import {
  decodeNodeUtxo,
  defaultMidgardNodeEndpoint,
  fetchNodeUtxosByAddress,
  formatJson,
  networkIdFromName,
  type NodeUtxo,
  parseNodeEndpoint,
  type ResolvedWalletSeedPhrase,
  walletNetworkFromId,
} from "@/commands/command-utils.js";
import {
  buildTerminalDrainTx,
  buildTransferTxWithMinFee,
  type BuiltTransferTx,
} from "@/commands/transfer-build-core.js";
import * as MempoolDB from "@/database/mempool.js";
import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
import * as TxRejectionsDB from "@/database/txRejections.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
  ContractDeploymentIdentity,
  Database as DatabaseService,
  Lucid,
  NodeConfig as NodeConfigService,
  WriteBehind,
} from "@/services/index.js";
import { compareOutRefs, outRefLabel } from "@/tx-context.js";

export {
  buildTerminalDrainTx,
  buildTransferTx,
  buildTransferTxWithMinFee,
  type BuiltTransferTx,
  makeStaticMidgardProvider,
  makeTransferMidgard,
  type PrivateKeyInput,
  type TransferNetworkName,
} from "@/commands/transfer-build-core.js";

export type SubmissionMode = "api" | "local";

export type SubmitL2TransferConfig = {
  readonly l2Address: string;
  readonly lovelace: bigint;
  readonly additionalAssets: Readonly<Assets>;
  readonly nodeEndpoint: string;
  readonly submitRequestTimeoutMs?: number;
  readonly utxoRequestTimeoutMs?: number;
  readonly networkId: bigint;
  readonly submissionMode: SubmissionMode;
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

class RetryableNativeTransferSubmitError extends Error {}

const sleep = (delayMs: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, delayMs));

const isDurableAdmissionFailure = (status: number, body: string): boolean => {
  if (status !== 500) return false;
  try {
    const parsed = JSON.parse(body) as { readonly error?: unknown };
    return parsed.error === "durable transaction admission failed";
  } catch {
    return false;
  }
};

const validateRetryPolicy = (policy: NativeTransferSubmitRetryPolicy): void => {
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
const toError = (cause: unknown, prefix: string): Error =>
  cause instanceof Error
    ? new Error(`${prefix}: ${cause.message}`)
    : new Error(`${prefix}: ${String(cause)}`);

/**
 * Parses the submission mode from CLI or environment input.
 */
const parseSubmissionMode = (value: string | undefined): SubmissionMode => {
  const normalized = value?.trim().toLowerCase() ?? "api";
  if (normalized === "api" || normalized === "local") {
    return normalized;
  }
  throw new Error(
    `Invalid submission mode "${value}". Expected "api" or "local".`,
  );
};

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
  submissionMode,
}: {
  readonly l2Address: string;
  readonly lovelace: string;
  readonly assetSpecs: readonly string[];
  readonly nodeEndpoint?: string;
  readonly submitRequestTimeoutMs?: number;
  readonly utxoRequestTimeoutMs?: number;
  readonly submissionMode?: string;
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
    submissionMode: parseSubmissionMode(submissionMode),
  };
};

/**
 * Builds the requested transfer asset map from the normalized command config.
 */
const buildRequestedAssets = (
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

/**
 * Reads the sender's spendable UTxOs from the local mempool-ledger view rather
 * than the public HTTP API.
 */
export const fetchLocalUtxos = (
  address: string,
): Effect.Effect<readonly NodeUtxo[], Error, DatabaseService> =>
  MempoolLedgerDB.retrieveSpendableByAddress(address).pipe(
    Effect.map((entries) =>
      entries.map((entry) =>
        decodeNodeUtxo({
          outref: entry.outref.toString("hex"),
          outputCbor: entry.output.toString("hex"),
        }),
      ),
    ),
    Effect.mapError((cause) =>
      toError(cause, "Failed to fetch local Midgard UTxOs"),
    ),
  );

/**
 * Submits a Midgard-native transfer through the node's public `/submit`
 * endpoint and verifies the returned tx id.
 */
export const submitNativeTransferTx = (
  nodeEndpoint: string,
  txHex: string,
  expectedTxIdHex: string,
  requestTimeoutMs?: number,
  retryPolicy?: NativeTransferSubmitRetryPolicy,
): Effect.Effect<{ readonly txId: string; readonly status: string }, Error> =>
  Effect.tryPromise({
    try: async () => {
      const policy = retryPolicy ?? {
        maxAttempts: 1,
        initialDelayMs: 0,
        maxDelayMs: 0,
      };
      validateRetryPolicy(policy);
      const txBytes = Buffer.from(txHex, "hex");
      const submissionBytes = encodeMidgardProofSubmissionV1({
        transactionCbor: txBytes,
        programMaterial: [],
      });
      for (let attempt = 1; attempt <= policy.maxAttempts; attempt += 1) {
        const controller =
          requestTimeoutMs === undefined ? undefined : new AbortController();
        const timeout =
          controller === undefined
            ? undefined
            : setTimeout(() => controller.abort(), requestTimeoutMs);
        try {
          let response: Response;
          try {
            response = await fetch(`${nodeEndpoint}/submit`, {
              method: "POST",
              headers: {
                "content-type": "application/vnd.midgard.v1+cbor",
              },
              body: submissionBytes,
              ...(controller === undefined
                ? {}
                : { signal: controller.signal }),
            });
          } catch (cause) {
            throw new RetryableNativeTransferSubmitError(
              `Midgard node transfer submit transport failed: ${String(cause)}`,
            );
          }
          let responseText: string;
          try {
            responseText = await response.text();
          } catch (cause) {
            throw new RetryableNativeTransferSubmitError(
              `Midgard node transfer submit response read failed: ${String(cause)}`,
            );
          }
          if (!response.ok) {
            const message = `Midgard node transfer submit failed (${response.status}): ${responseText}`;
            if (isDurableAdmissionFailure(response.status, responseText)) {
              throw new RetryableNativeTransferSubmitError(message);
            }
            throw new Error(message);
          }
          let parsed: {
            readonly txId?: unknown;
            readonly status?: unknown;
          };
          try {
            parsed = JSON.parse(responseText) as typeof parsed;
          } catch {
            throw new Error("Midgard node submit response must be valid JSON.");
          }
          if (
            typeof parsed.txId !== "string" ||
            typeof parsed.status !== "string"
          ) {
            throw new Error(
              "Midgard node submit response must contain string txId/status fields.",
            );
          }
          if (parsed.txId !== expectedTxIdHex) {
            throw new Error(
              `Midgard node returned mismatched txId ${parsed.txId} (expected ${expectedTxIdHex}).`,
            );
          }
          return {
            txId: parsed.txId,
            status: parsed.status,
          };
        } catch (cause) {
          if (
            !(cause instanceof RetryableNativeTransferSubmitError) ||
            attempt === policy.maxAttempts
          ) {
            throw cause;
          }
          const delayMs = Math.min(
            policy.initialDelayMs * 2 ** (attempt - 1),
            policy.maxDelayMs,
          );
          await (policy.sleep ?? sleep)(delayMs);
        } finally {
          if (timeout !== undefined) {
            clearTimeout(timeout);
          }
        }
      }
      throw new Error("Native transfer submit retry loop exhausted");
    },
    catch: (cause) =>
      new Error(`Failed to submit Midgard-native transfer: ${String(cause)}`),
  });

/**
 * Converts a built transfer into the queue-entry shape expected by validation.
 */
export const toQueuedTx = (built: BuiltTransferTx): QueuedTx => ({
  txId: built.txId,
  txCbor: built.txCbor,
  programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecarV1([]),
  arrivalSeq: 0n,
  createdAt: new Date(),
});

/**
 * Runs the transfer through local validation and inserts it directly into the
 * mempool tables when accepted.
 */
export const submitNativeTransferLocally = (
  built: BuiltTransferTx,
): Effect.Effect<
  { readonly txId: string; readonly status: string },
  Error | DatabaseError,
  | DatabaseService
  | NodeConfigService
  | ContractDeploymentIdentity
  | Lucid
  | WriteBehind
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfigService;
    const deploymentIdentity = yield* ContractDeploymentIdentity;
    const { api: lucid } = yield* Lucid;
    const phaseA = yield* runPhaseAValidation([toQueuedTx(built)], {
      expectedNetworkId: nodeConfig.NETWORK === "Mainnet" ? 1n : 0n,
      minFeeA: nodeConfig.MIN_FEE_A,
      minFeeB: nodeConfig.MIN_FEE_B,
      concurrency: 1,
      strictnessProfile: nodeConfig.VALIDATION_STRICTNESS_PROFILE,
      consensusProfile: deploymentIdentity.consensusProfile,
    });

    const preStateEntries = yield* MempoolLedgerDB.retrieveSpendable;
    const preState = new Map<string, Buffer>();
    for (const entry of preStateEntries) {
      preState.set(entry.outref.toString("hex"), entry.output);
    }

    const phaseB = yield* runPhaseBValidationWithPatch(
      phaseA.accepted,
      preState,
      {
        nowCardanoSlotNo: BigInt(lucid.currentSlot()),
        bucketConcurrency: nodeConfig.VALIDATION_G4_BUCKET_CONCURRENCY,
        enforceScriptBudget: true,
      },
    );
    const rejected = [...phaseA.rejected, ...phaseB.rejected];
    if (rejected.length > 0) {
      yield* TxRejectionsDB.insertMany(
        rejected.map((entry) => ({
          tx_id: entry.txId,
          reject_code: entry.code,
          reject_detail: entry.detail,
        })),
      );
      const first = rejected[0]!;
      return yield* Effect.fail(
        new Error(
          `Local Midgard transfer validation rejected ${built.txIdHex}: ${first.code} (${first.detail})`,
        ),
      );
    }

    if (phaseB.accepted.length !== 1) {
      return yield* Effect.fail(
        new Error(
          `Expected exactly one accepted transfer, got ${phaseB.accepted.length}.`,
        ),
      );
    }

    yield* MempoolDB.insertMultiple(
      phaseB.accepted.map(processedTxFromValidatedTx),
    );
    return {
      txId: built.txIdHex,
      status: "accepted-local",
    };
  });

/**
 * End-to-end L2 transfer submission program.
 *
 * The flow derives the sender wallet, gathers available inputs, builds a
 * fee-balanced Midgard-native transfer, and submits it through either the HTTP
 * API or the local validation path.
 */
const prepareL2TransferWithBuiltProgram = ({
  config,
  resolvedWalletSeedPhrase,
  assertWalletAddress,
}: {
  readonly config: SubmitL2TransferConfig;
  readonly resolvedWalletSeedPhrase: ResolvedWalletSeedPhrase;
  readonly assertWalletAddress?: (walletAddress: string) => void;
}): Effect.Effect<
  { readonly prepared: PreparedL2Transfer; readonly built: BuiltTransferTx },
  Error | DatabaseError,
  | DatabaseService
  | NodeConfigService
  | ContractDeploymentIdentity
  | Lucid
  | WriteBehind
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfigService;
    const deploymentIdentity = yield* ContractDeploymentIdentity;
    const nodeNetworkId = networkIdFromName(nodeConfig.NETWORK);
    if (config.networkId !== nodeNetworkId) {
      return yield* Effect.fail(
        new Error(
          `Destination address network id ${config.networkId.toString()} does not match configured Midgard node network ${nodeConfig.NETWORK} (network id ${nodeNetworkId.toString()}).`,
        ),
      );
    }
    const wallet = yield* Effect.try({
      try: () =>
        walletFromSeed(resolvedWalletSeedPhrase.seedPhrase, {
          network: walletNetworkFromId(config.networkId),
        }),
      catch: (cause) =>
        toError(cause, "Failed to derive wallet from seed phrase"),
    });
    const senderAddress = wallet.address;
    yield* Effect.sync(() => assertWalletAddress?.(senderAddress));
    const senderDetails = getAddressDetails(senderAddress);
    if (!senderDetails.paymentCredential) {
      return yield* Effect.fail(
        new Error("Derived sender address must include a payment credential."),
      );
    }
    if (BigInt(senderDetails.networkId) !== config.networkId) {
      return yield* Effect.fail(
        new Error(
          `Sender wallet network id ${senderDetails.networkId} does not match destination network id ${config.networkId.toString()}.`,
        ),
      );
    }

    const requestedAssets = buildRequestedAssets(config);
    const availableUtxos =
      config.submissionMode === "local"
        ? yield* fetchLocalUtxos(senderAddress)
        : yield* fetchNodeUtxos(
            config.nodeEndpoint,
            senderAddress,
            config.utxoRequestTimeoutMs,
          );
    if (availableUtxos.length === 0) {
      return yield* Effect.fail(
        new Error(
          `No Midgard L2 UTxOs found for sender address ${senderAddress}.`,
        ),
      );
    }

    const built = yield* Effect.tryPromise({
      try: () =>
        buildTransferTxWithMinFee({
          senderAddress,
          destinationAddress: config.l2Address,
          signer: wallet.paymentKey,
          availableUtxos,
          requestedAssets,
          network: nodeConfig.NETWORK,
          networkId: config.networkId,
          minFeeA: nodeConfig.MIN_FEE_A,
          minFeeB: nodeConfig.MIN_FEE_B,
          consensusProfile: deploymentIdentity.consensusProfile,
        }),
      catch: (cause) =>
        toError(cause, "Failed to build Midgard-native transfer"),
    });
    return {
      built,
      prepared: {
        txId: built.txIdHex,
        signedTxCbor: built.txHex,
        senderAddress,
        destinationAddress: config.l2Address,
        selectedInputs: built.selectedInputs.map(outRefLabel),
        requestedAssets: built.requestedAssets,
        changeAssets: built.changeAssets,
        walletSeedSource: resolvedWalletSeedPhrase.resolvedFrom,
        nodeEndpoint: config.nodeEndpoint,
      },
    };
  });

/** Builds and signs an L2 transfer without submitting it. */
export const prepareL2TransferProgram = (args: {
  readonly config: SubmitL2TransferConfig;
  readonly resolvedWalletSeedPhrase: ResolvedWalletSeedPhrase;
  readonly assertWalletAddress?: (walletAddress: string) => void;
}): Effect.Effect<
  PreparedL2Transfer,
  Error | DatabaseError,
  | DatabaseService
  | NodeConfigService
  | ContractDeploymentIdentity
  | Lucid
  | WriteBehind
> =>
  prepareL2TransferWithBuiltProgram(args).pipe(
    Effect.map(({ prepared }) => prepared),
  );

/** Builds and signs an all-input, exact-zero source sweep without submitting. */
export const prepareL2TerminalDrainProgram = ({
  destinationAddress,
  nodeEndpoint,
  requestTimeoutMs,
  networkId,
  feeCap,
  maxFeeIterations,
  resolvedWalletSeedPhrase,
  assertWalletAddress,
}: {
  readonly destinationAddress: string;
  readonly nodeEndpoint: string;
  readonly requestTimeoutMs?: number;
  readonly networkId: bigint;
  readonly feeCap?: bigint;
  readonly maxFeeIterations?: number;
  readonly resolvedWalletSeedPhrase: ResolvedWalletSeedPhrase;
  readonly assertWalletAddress?: (walletAddress: string) => void;
}): Effect.Effect<
  PreparedL2TerminalDrain,
  Error | DatabaseError,
  | DatabaseService
  | NodeConfigService
  | ContractDeploymentIdentity
  | Lucid
  | WriteBehind
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfigService;
    const deploymentIdentity = yield* ContractDeploymentIdentity;
    const nodeNetworkId = networkIdFromName(nodeConfig.NETWORK);
    if (networkId !== nodeNetworkId)
      return yield* Effect.fail(
        new Error(
          "Terminal drain network id does not match configured Midgard node network.",
        ),
      );
    const wallet = yield* Effect.try({
      try: () =>
        walletFromSeed(resolvedWalletSeedPhrase.seedPhrase, {
          network: walletNetworkFromId(networkId),
        }),
      catch: (cause) =>
        toError(cause, "Failed to derive terminal drain wallet"),
    });
    const senderAddress = wallet.address;
    yield* Effect.sync(() => assertWalletAddress?.(senderAddress));
    if (BigInt(getAddressDetails(destinationAddress).networkId) !== networkId)
      return yield* Effect.fail(
        new Error("Terminal drain destination network id mismatch."),
      );
    const availableUtxos = yield* fetchNodeUtxos(
      nodeEndpoint,
      senderAddress,
      requestTimeoutMs,
    );
    if (availableUtxos.length === 0)
      return yield* Effect.fail(
        new Error(
          "No Midgard L2 UTxOs found for terminal drain source " +
            senderAddress +
            ".",
        ),
      );
    const built = yield* Effect.tryPromise({
      try: () =>
        buildTerminalDrainTx({
          senderAddress,
          destinationAddress,
          signer: wallet.paymentKey,
          availableUtxos,
          network: nodeConfig.NETWORK,
          networkId,
          minFeeA: nodeConfig.MIN_FEE_A,
          minFeeB: nodeConfig.MIN_FEE_B,
          consensusProfile: deploymentIdentity.consensusProfile,
          ...(feeCap === undefined ? {} : { feeCap }),
          ...(maxFeeIterations === undefined ? {} : { maxFeeIterations }),
        }),
      catch: (cause) =>
        toError(cause, "Failed to build terminal drain transaction"),
    });
    return {
      txId: built.txIdHex,
      signedTxCbor: built.txHex,
      senderAddress,
      destinationAddress,
      selectedInputs: built.selectedInputs.map(outRefLabel),
      requestedLovelace: built.requestedAssets.lovelace ?? 0n,
      feeLovelace: built.fee,
      signedTxBytes: built.txCbor.length,
    };
  });

/** End-to-end L2 transfer submission program. */
export const submitL2TransferProgram = ({
  config,
  resolvedWalletSeedPhrase,
  assertWalletAddress,
  apiSubmitRetryPolicy,
}: {
  readonly config: SubmitL2TransferConfig;
  readonly resolvedWalletSeedPhrase: ResolvedWalletSeedPhrase;
  readonly assertWalletAddress?: (walletAddress: string) => void;
  readonly apiSubmitRetryPolicy?: NativeTransferSubmitRetryPolicy;
}): Effect.Effect<
  SubmitL2TransferResult,
  Error | DatabaseError,
  | DatabaseService
  | NodeConfigService
  | ContractDeploymentIdentity
  | Lucid
  | WriteBehind
> =>
  Effect.gen(function* () {
    const { prepared, built } = yield* prepareL2TransferWithBuiltProgram({
      config,
      resolvedWalletSeedPhrase,
      assertWalletAddress,
    });
    const submitResult =
      config.submissionMode === "local"
        ? yield* submitNativeTransferLocally(built)
        : yield* submitNativeTransferTx(
            config.nodeEndpoint,
            prepared.signedTxCbor,
            prepared.txId,
            config.submitRequestTimeoutMs,
            apiSubmitRetryPolicy,
          );
    const { signedTxCbor: _signedTxCbor, ...publicResult } = prepared;
    return {
      ...publicResult,
      txId: submitResult.txId,
      status: submitResult.status,
    };
  });
