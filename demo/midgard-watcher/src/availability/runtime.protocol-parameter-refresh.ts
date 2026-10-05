import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import {
  calculateMinLovelaceFromUTxO,
  type LucidEvolution,
  type ProtocolParameters,
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

/** Owns an isolated attempt Lucid. A late refresh can only mutate this attempt.
 * The same admitted local provider retains its network and native query
 * authority; Lucid refreshes balancing parameters and cost models. */
export const refreshWatcherAvailabilityAttempt = async (
  lucid: LucidEvolution,
  scope: DaAvailabilityReadScope,
  assertCurrent: () => void,
): Promise<boolean> => {
  assertCurrent();
  scope.assertCurrent();
  const before = parameterDigest(lucid);
  const provider = lucid.config().provider;
  if (provider === undefined)
    throw new Error("Availability attempt omitted provider");
  await scope.read(() => lucid.switchProvider(provider));
  assertCurrent();
  scope.assertCurrent();
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

/** Re-select exact funding and min-ADA after one changed-parameter refresh.
 * Signing, durable intent writes and submission stay outside this wrapper. */
export const buildWatcherAvailabilityAttempt = async <T>(input: {
  lucid: LucidEvolution;
  scope: DaAvailabilityReadScope;
  assertCurrent: () => void;
  build: () => Promise<T>;
}): Promise<T> =>
  await buildWithWatcherProtocolParameterRefresh({
    assertCurrent: () => {
      input.assertCurrent();
      input.scope.assertCurrent();
    },
    refresh: () =>
      refreshWatcherAvailabilityAttempt(
        input.lucid,
        input.scope,
        input.assertCurrent,
      ),
    build: () => input.scope.read(input.build),
  });
