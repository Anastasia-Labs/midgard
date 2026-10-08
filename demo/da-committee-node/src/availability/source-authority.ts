import { type CommitteeConfig, l1SourceAuthorityDigest } from "../config.js";
import type { CommitteeStore } from "../store.js";

/**
 * The chain authority every responder transaction needs: the committee
 * store bound to this committee's configured L1 source (its network and node
 * authority). Answering a challenge spends, so it is a new
 * decision. Reading the retained bytes that answer is not gated on this:
 * those stay servable to any holder that can act (see
 * `retainedAvailabilityPayload`).
 */
export const assertAvailabilityResponderSourceHealthy = async (
  store: Pick<CommitteeStore, "getL1SourceState">,
  config: Pick<CommitteeConfig, "network" | "nativeLedger">,
): Promise<void> => {
  const source = await store.getL1SourceState();
  if (
    source?.status !== "healthy" ||
    source.sourceMode !== "local_node" ||
    source.network !== config.network ||
    source.authoritySha256 !== l1SourceAuthorityDigest(config)
  ) {
    throw new Error(
      "Availability responder requires the committee store bound to its configured L1 source",
    );
  }
};
