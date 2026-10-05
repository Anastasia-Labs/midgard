import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";

import type {
  ChainSyncCursor,
  ChainSyncReplayProvider,
} from "./provider.parse-persisted-chain-sync-state.js";
import { samePersistedCursor } from "./provider.same-persisted-cursor.js";

/** Forward-only cursor progress may outrun a service scan. A rollback may not
 * authorize actuation until that service durably consumes its generation.
 * The caller must additionally prove its configured source is healthy. */
export const readAvailabilityCursor = async (
  provider: Pick<
    ChainSyncReplayProvider,
    "currentChainSyncCursor" | "loadConsumedChainSyncCursor"
  >,
  scope: DaAvailabilityReadScope,
): Promise<ChainSyncCursor> => {
  const before = await scope.read(() => provider.currentChainSyncCursor());
  const consumed = await scope.read(() =>
    provider.loadConsumedChainSyncCursor(),
  );
  const after = await scope.read(() => provider.currentChainSyncCursor());
  if (!samePersistedCursor(before, after))
    throw new Error(
      "Availability cursor changed while checking its consumed generation",
    );
  if (
    consumed === undefined ||
    consumed.sequence > after.sequence ||
    consumed.rollbackGeneration !== after.rollbackGeneration ||
    consumed.point.network !== after.point.network ||
    (consumed.sequence === after.sequence &&
      !samePersistedCursor(consumed, after))
  )
    throw new Error(
      "Availability cursor requires durable consumption of its rollback generation",
    );
  scope.assertCurrent();
  return after;
};
