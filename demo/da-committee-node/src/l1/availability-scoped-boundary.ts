import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";

import {
  committeeScopedFetch,
  committeeScopedOgmiosRpc,
  type CommitteeSourceReadLimits,
} from "../availability/scoped-transports.js";
import { fetchKupoCheckpoint } from "./provider.local-node-chain-authority-from-config.js";
import {
  assertNetworkMagic,
  KUPMIOS_TIP_ALIGNMENT_ATTEMPTS,
  KUPMIOS_TIP_ALIGNMENT_RETRY_MS,
} from "./provider.ogmios-rpc-session.js";
import {
  type CanonicalChainPoint,
  getRecord,
  safeBlockHash,
  safeSlot,
} from "./provider.parse-persisted-chain-sync-state.js";
import type { StateQueueReplayWebSocketFactory } from "./state-queue-replay-provider.open-rpc.js";

/** Configured network identity, Kupo checkpoint and exact selected-chain tip
 * share the passed scope. Native height comes from chain-sync's Tip schema. */
export const scopedKupmiosCurrentPoint = async (
  input: Readonly<{
    network: string;
    kupoUrl: string;
    ogmiosUrl: string;
    networkMagic?: number;
    scope: DaAvailabilityReadScope;
    limits: CommitteeSourceReadLimits;
    fetchImpl?: typeof fetch;
    webSocketFactory?: StateQueueReplayWebSocketFactory;
  }>,
): Promise<CanonicalChainPoint & Readonly<{ blockHeight: number }>> => {
  const fetchImpl = committeeScopedFetch(
    input.scope,
    input.limits,
    input.fetchImpl,
  );
  for (let attempt = 1; attempt <= KUPMIOS_TIP_ALIGNMENT_ATTEMPTS; attempt++) {
    input.scope.assertCurrent();
    const checkpoint = await fetchKupoCheckpoint(input.kupoUrl, fetchImpl);
    const rpc = await committeeScopedOgmiosRpc(
      input.ogmiosUrl,
      input.scope,
      input.limits,
      input.webSocketFactory,
    );
    try {
      const genesis = getRecord(
        await rpc.request("queryNetwork/genesisConfiguration", {
          era: "shelley",
        }),
        "Ogmios genesis configuration",
      );
      assertNetworkMagic(
        input.network,
        safeSlot(
          genesis.networkMagic ?? genesis.network_magic,
          "Ogmios network magic",
        ),
        "Ogmios",
        input.networkMagic,
      );
      const found = getRecord(
        await rpc.request("findIntersection", {
          points: [{ slot: checkpoint.slot, id: checkpoint.blockHash }],
        }),
        "Ogmios exact checkpoint intersection",
      );
      const intersection = getRecord(
        found.intersection,
        "Ogmios checkpoint intersection",
      );
      const tip = getRecord(found.tip, "Ogmios selected-chain tip");
      if (
        intersection.slot !== checkpoint.slot ||
        intersection.id !== checkpoint.blockHash
      )
        throw new Error(
          "Ogmios intersection differs from the exact Kupo checkpoint",
        );
      const slot = safeSlot(tip.slot, "Ogmios selected tip slot");
      const blockHash = safeBlockHash(tip.id, "Ogmios selected tip hash");
      const blockHeight = safeSlot(
        tip.height,
        "Ogmios selected tip native height",
      );
      input.scope.assertCurrent();
      if (slot === checkpoint.slot && blockHash === checkpoint.blockHash)
        return {
          network: input.network,
          slot,
          blockHash,
          blockHeight,
          providerSource: `kupmios:${input.kupoUrl}|${input.ogmiosUrl}`,
          observedAt: new Date().toISOString(),
        };
      if (attempt === KUPMIOS_TIP_ALIGNMENT_ATTEMPTS)
        throw new Error(
          "Scoped Kupmios surfaces did not align at one canonical point",
        );
    } finally {
      rpc.close();
    }
    await input.scope.read(
      (signal) =>
        new Promise<void>((resolve, reject) => {
          const abort = () => {
            clearTimeout(timer);
            const reason: unknown = signal.reason;
            reject(
              reason instanceof Error
                ? reason
                : new Error("Scoped Kupmios alignment aborted", {
                    cause: reason,
                  }),
            );
          };
          const timer = setTimeout(() => {
            signal.removeEventListener("abort", abort);
            resolve();
          }, KUPMIOS_TIP_ALIGNMENT_RETRY_MS);
          signal.addEventListener("abort", abort, { once: true });
        }),
    );
  }
  throw new Error("Scoped Kupmios alignment exhausted");
};
