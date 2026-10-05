import { encodeMidgardProofSubmission } from "@al-ft/midgard-core/cek-proof";
import { getAddressDetails, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  ContractDeploymentIdentity,
  NodeConfig as NodeConfigService,
} from "../services/index.js";
import { sleep } from "../sleep.js";
import { outRefLabel } from "../tx-context.js";
import {
  networkIdFromName,
  type ResolvedWalletSeedPhrase,
  walletNetworkFromId,
} from "./command-utils.js";
import {
  buildRequestedAssets,
  fetchNodeUtxos,
  isDurableAdmissionFailure,
  type NativeTransferSubmitRetryPolicy,
  type PreparedL2Transfer,
  ResumableNativeTransferSubmitError,
  RetryableNativeTransferSubmitError,
  type SubmitL2TransferConfig,
  toError,
  validateRetryPolicy,
} from "./submit-l2-transfer.compare-assets-by-coverage.js";
import { buildTransferTxWithMinFee } from "./transfer-build-core.js";

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
      const submissionBytes = encodeMidgardProofSubmission({
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
            if (response.status >= 500) {
              throw new ResumableNativeTransferSubmitError(message);
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
    catch: (cause) => {
      const message = `Failed to submit Midgard-native transfer: ${String(cause)}`;
      return cause instanceof ResumableNativeTransferSubmitError
        ? new ResumableNativeTransferSubmitError(message)
        : new Error(message);
    },
  });

/** The signing wallet of an L2 transfer, resolved and network-checked. */
export type L2TransferSender = {
  readonly senderAddress: string;
  readonly paymentKey: string;
};

/**
 * Derives the transfer signer and checks that it, the destination and the
 * node all use one network. It reads nothing from the node.
 */
export const resolveL2TransferSenderProgram = ({
  config,
  resolvedWalletSeedPhrase,
  assertWalletAddress,
}: {
  readonly config: SubmitL2TransferConfig;
  readonly resolvedWalletSeedPhrase: ResolvedWalletSeedPhrase;
  readonly assertWalletAddress?: (walletAddress: string) => void;
}): Effect.Effect<L2TransferSender, Error, NodeConfigService> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfigService;
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
    return { senderAddress, paymentKey: wallet.paymentKey };
  });

/**
 * Selects the sender's inputs from the node's public `/utxos` endpoint, less
 * the outputs `config.excludedOutRefs` names, and builds and signs a
 * fee-balanced Midgard-native transfer without submitting it.
 */
export const buildL2TransferForSenderProgram = ({
  config,
  sender,
  walletSeedSource,
}: {
  readonly config: SubmitL2TransferConfig;
  readonly sender: L2TransferSender;
  readonly walletSeedSource: string;
}): Effect.Effect<
  PreparedL2Transfer,
  Error,
  NodeConfigService | ContractDeploymentIdentity
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfigService;
    const deploymentIdentity = yield* ContractDeploymentIdentity;
    const { senderAddress } = sender;
    const requestedAssets = buildRequestedAssets(config);
    const nodeUtxos = yield* fetchNodeUtxos(
      config.nodeEndpoint,
      senderAddress,
      config.utxoRequestTimeoutMs,
    );
    const excluded = new Set(config.excludedOutRefs);
    const availableUtxos = nodeUtxos.filter(
      (utxo) => !excluded.has(outRefLabel(utxo)),
    );
    if (availableUtxos.length === 0) {
      const skipped = nodeUtxos.length - availableUtxos.length;
      return yield* Effect.fail(
        new Error(
          `No Midgard L2 UTxOs found for sender address ${senderAddress}${skipped === 0 ? "" : ` outside the ${skipped.toString()} excluded by --exclude-out-ref`}.`,
        ),
      );
    }

    const built = yield* Effect.tryPromise({
      try: () =>
        buildTransferTxWithMinFee({
          senderAddress,
          destinationAddress: config.l2Address,
          signer: sender.paymentKey,
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
      txId: built.txIdHex,
      signedTxCbor: built.txHex,
      senderAddress,
      destinationAddress: config.l2Address,
      selectedInputs: built.selectedInputs.map(outRefLabel),
      requestedAssets: built.requestedAssets,
      changeAssets: built.changeAssets,
      walletSeedSource,
      nodeEndpoint: config.nodeEndpoint,
    };
  });

/**
 * Builds and signs an L2 transfer without submitting it.
 *
 * The flow derives the sender wallet, gathers its inputs from the node's
 * public `/utxos` endpoint, and builds a fee-balanced Midgard-native transfer.
 */
export const prepareL2TransferProgram = ({
  config,
  resolvedWalletSeedPhrase,
  assertWalletAddress,
}: {
  readonly config: SubmitL2TransferConfig;
  readonly resolvedWalletSeedPhrase: ResolvedWalletSeedPhrase;
  readonly assertWalletAddress?: (walletAddress: string) => void;
}): Effect.Effect<
  PreparedL2Transfer,
  Error,
  NodeConfigService | ContractDeploymentIdentity
> =>
  Effect.gen(function* () {
    const sender = yield* resolveL2TransferSenderProgram({
      config,
      resolvedWalletSeedPhrase,
      assertWalletAddress,
    });
    return yield* buildL2TransferForSenderProgram({
      config,
      sender,
      walletSeedSource: resolvedWalletSeedPhrase.resolvedFrom,
    });
  });
