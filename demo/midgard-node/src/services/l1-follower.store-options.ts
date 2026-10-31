/**
 * The node follower's store options: every projection's tables and hooks, the
 * node's tracked set, and the retained depth its commits' anchors need.
 */
import {
  type FactStoreOptions,
  type FollowerProjection,
  projectionStoreOptions,
} from "@al-ft/midgard-l1-follower";

import { followerRetainedDepth } from "../database/commit-anchor.js";
import { nodeIntentTrackedSet } from "./l1-follower.intents.js";

export const nodeFollowerStoreOptions = (
  input: Readonly<{
    projections: readonly FollowerProjection[];
    securityParameter: number;
    commitEventDepth: number;
    protocolPaymentCredentials: readonly string[];
    hubOraclePolicyId: string;
  }>,
): FactStoreOptions =>
  projectionStoreOptions(
    input.projections,
    {
      securityParameter: input.securityParameter,
      // Keeps every commit's anchor block while the follower runs up to k
      // blocks past the commit's planning view.
      retainedDepth: followerRetainedDepth(
        input.securityParameter,
        input.commitEventDepth,
      ),
      // The protocol-init tx qualifies through the hub oracle mint; the rest
      // is the intent journal's invariant (§8.2).
      trackedSet: nodeIntentTrackedSet({
        protocolPaymentCredentials: input.protocolPaymentCredentials,
        hubOraclePolicyId: input.hubOraclePolicyId,
      }),
    },
    "postgres",
  );
