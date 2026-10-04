import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type DaAvailabilityOperationBuild,
  daAvailabilityOperationLimits,
  type DaAvailabilityParameters,
} from "@al-ft/midgard-sdk";
import {
  calculateMinLovelaceFromUTxO,
  type LucidEvolution,
  type ProtocolParameters,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";

import { buildWithWatcherProtocolParameterRefresh } from "../funding/protocol-parameter-retry.js";

const parameterDigest = (lucid: LucidEvolution): string =>
  computeDeploymentManifestJsonDigest(
    JSON.parse(
      JSON.stringify(
        lucid.config().protocolParameters,
        (_key, value: unknown) =>
          typeof value === "bigint" ? value.toString() : value,
      ),
    ) as unknown,
  );

export const createWatcherAvailabilityParameterRefresh =
  (lucid: LucidEvolution) => async (): Promise<boolean> => {
    const before = parameterDigest(lucid);
    const provider = lucid.config().provider;
    if (provider === undefined)
      throw new Error("Availability operation omitted local provider");
    // The same admitted local provider retains its network and native query
    // authority. Lucid refreshes balancing parameters and cost models.
    await lucid.switchProvider(provider);
    return before !== parameterDigest(lucid);
  };

export const minimumWatcherAvailabilityChange = (
  protocol: ProtocolParameters,
  walletAddress: string,
): bigint =>
  calculateMinLovelaceFromUTxO(protocol.coinsPerUtxoByte, {
    address: walletAddress,
    assets: { lovelace: 2_000_000n },
    txHash: "00".repeat(32),
    outputIndex: 0,
  });

export const watcherAvailabilityRefreshedOperation = (
  operation: {
    action: string;
    completesWorkflow?: boolean;
    build(): Promise<TxSignBuilder | DaAvailabilityOperationBuild>;
  },
  select: () => Promise<{
    action: string;
    build(): Promise<TxSignBuilder | DaAvailabilityOperationBuild>;
  }>,
  refresh: () => Promise<boolean>,
  assertCurrent: () => void,
) => ({
  ...operation,
  build: () =>
    buildWithWatcherProtocolParameterRefresh({
      assertCurrent,
      refresh,
      build: async () => {
        // Re-select live wallet inputs and recompute min-ADA/collateral.
        const selected = await select();
        if (selected.action !== operation.action)
          throw new Error(
            "Parameter refresh changed availability action; reconcile before building",
          );
        return await selected.build();
      },
    }),
});

export const createWatcherAvailabilityProtocolRuntime = (
  lucid: LucidEvolution,
) => ({
  refresh: createWatcherAvailabilityParameterRefresh(lucid),
  minimumChange: minimumWatcherAvailabilityChange,
  transactionLimits: (parameters: DaAvailabilityParameters) =>
    daAvailabilityOperationLimits(lucid, parameters),
  refreshedOperation: <Args extends readonly unknown[]>(
    operation: Parameters<typeof watcherAvailabilityRefreshedOperation>[0],
    select: (
      ...args: Args
    ) => ReturnType<
      Parameters<typeof watcherAvailabilityRefreshedOperation>[1]
    >,
    args: Args,
    assertCurrent: () => void,
  ) =>
    watcherAvailabilityRefreshedOperation(
      operation,
      () => select(...args),
      createWatcherAvailabilityParameterRefresh(lucid),
      assertCurrent,
    ),
});
