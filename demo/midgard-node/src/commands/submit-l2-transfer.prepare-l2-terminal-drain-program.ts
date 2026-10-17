import { getAddressDetails, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  ContractDeploymentIdentity,
  NodeConfig as NodeConfigService,
} from "../services/index.js";
import { outRefLabel } from "../tx-context.js";
import {
  networkIdFromName,
  type ResolvedWalletSeedPhrase,
  walletNetworkFromId,
} from "./command-utils.js";
import {
  fetchNodeUtxos,
  type NativeTransferSubmitRetryPolicy,
  type PreparedL2TerminalDrain,
  type SubmitL2TransferConfig,
  type SubmitL2TransferResult,
  toError,
} from "./submit-l2-transfer.compare-assets-by-coverage.js";
import {
  prepareL2TransferProgram,
  submitNativeTransferTx,
} from "./submit-l2-transfer.submit-native-transfer-tx.js";
import { buildTerminalDrainTx } from "./transfer-build-core.js";

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
  Error,
  NodeConfigService | ContractDeploymentIdentity
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
  Error,
  NodeConfigService | ContractDeploymentIdentity
> =>
  Effect.gen(function* () {
    const prepared = yield* prepareL2TransferProgram({
      config,
      resolvedWalletSeedPhrase,
      assertWalletAddress,
    });
    const submitResult = yield* submitNativeTransferTx(
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
