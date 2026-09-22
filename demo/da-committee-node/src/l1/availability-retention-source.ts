import * as SDK from "@al-ft/midgard-sdk";

import { l1SourceAuthorityDigest, type WatcherConfig } from "../config.js";
import type { WatcherStore } from "../store.js";
import type { RetentionScanOptions } from "../store/retention.js";
import type { StateQueueProvider } from "./state-queue-scanner.js";

/** Release and chain authority remain native; durable records are revalidated on every scan. */
export const availabilityRetentionSourceFromStore = async (
  config: WatcherConfig,
  store: WatcherStore,
  provider: StateQueueProvider,
  nowMs: number,
): Promise<
  NonNullable<RetentionScanOptions["availabilityChallengeAuthority"]>
> => {
  const authority: SDK.DaAvailabilityRetentionAuthority = {
    deploymentIdentityDigest: config.deploymentFingerprint,
    stateQueuePolicyId: config.stateQueuePolicyId,
    stateQueueAddress: config.stateQueueAddress,
    availabilityPolicyId:
      config.midgardNodeDeployment.availabilityChallenge.policyId,
    minimumFinalityDepth: BigInt(config.finalityDepth),
  };
  const sourceAuthoritySha256 = l1SourceAuthorityDigest(
    config.network,
    config.l1Source,
  );
  const source = await store.getL1SourceState();
  const terminalEvidence = new Map<
    string,
    {
      readonly transition: SDK.StateQueueAuthenticatedTransition;
      readonly evidence: SDK.DaAvailabilityRetentionEvidence;
      readonly authority: SDK.DaAvailabilityRetentionAuthority;
    }
  >();
  if (
    source?.status === "healthy" &&
    source.sourceMode === "local_node" &&
    source.authoritySha256 === sourceAuthoritySha256
  ) {
    for (const header of await store.listStateQueueHeaders()) {
      const terminal = header.availabilityRetention;
      if (terminal === undefined) continue;
      const transition = SDK.parseStateQueueAuthenticatedTransition(
        terminal.transition,
      );
      const evidence =
        transition === null
          ? null
          : SDK.parseDaAvailabilityRetentionEvidence(
              terminal.evidence,
              transition,
              authority,
            );
      if (
        transition !== null &&
        evidence !== null &&
        evidence.headerHash === header.headerHash
      )
        terminalEvidence.set(header.headerHash, {
          transition,
          evidence,
          authority,
        });
    }
  }
  return {
    deploymentFingerprint: config.deploymentFingerprint,
    capability: "deployed_unobserved",
    terminalEvidence,
    withCurrentTerminalEvidence: async (headerHash, transitionDigest) => {
      if (
        provider.withCurrentRetentionAuthority === undefined ||
        store.deleteDaPayloadIfCurrentTerminal === undefined
      )
        return false;
      return provider.withCurrentRetentionAuthority(() =>
        store.deleteDaPayloadIfCurrentTerminal!({
          headerHash,
          transitionDigest,
          authority,
          sourceAuthoritySha256,
          nowMs,
          retentionDays: config.daTransport.retentionDays,
        }),
      );
    },
  };
};
