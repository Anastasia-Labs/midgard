import {
  type CommitteeL1ClientConfig,
  l1SourceAuthorityDigest,
} from "../config.js";
import type { CommitteeStore } from "../store.js";

/**
 * The chain authority every responder transaction needs: this committee
 * node's own local-node L1 source, healthy and bound to the configured
 * authority. Answering a challenge spends, so it is a new decision, and a
 * quarantined source refuses it here until recovery clears the quarantine.
 * Reading the retained bytes that answer is not gated on this: those stay
 * servable to any holder that can act (see `retainedAvailabilityPayload`).
 */
export const assertAvailabilityResponderSourceHealthy = async (
  store: Pick<CommitteeStore, "getL1SourceState">,
  config: Pick<CommitteeL1ClientConfig, "network" | "l1Source">,
): Promise<void> => {
  const source = await store.getL1SourceState();
  if (
    source?.status !== "healthy" ||
    source.sourceMode !== "local_node" ||
    source.network !== config.network ||
    source.authoritySha256 !==
      l1SourceAuthorityDigest(config.network, config.l1Source)
  ) {
    throw new Error(
      "Availability responder requires a healthy authenticated committee node L1 source; rollback recovery must finish first",
    );
  }
};
