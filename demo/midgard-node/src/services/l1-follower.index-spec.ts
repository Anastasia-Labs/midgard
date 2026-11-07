/**
 * The node's L1 index, defined once: what its follower store tracks,
 * records and seeds. Every process that opens the node's follower store
 * builds its options from `nodeIndexSpec`, the writer (`listen`'s follower)
 * and the readers alike, so the protocol set a store records is the one
 * every opener configures: a start never finds an added item, never resets
 * the store's facts (class A) and never deletes a wallet's seed rows.
 *
 * - Tracked set (recorded): the protocol validators' payment credentials
 *   and the hub-oracle policy, for the intent journal's invariant (§8.2)
 *   and the protocol-init tx, and every projection's own set (event lists,
 *   state queue, operator lists, tx orders).
 * - Wallets: the node's own wallets and reference-script addresses, tracked
 *   by address and seeded at the cursor (§5.3 step 4), never recorded.
 * - Projections: their tracked sets, tables and derivations; the writer
 *   binds their runtime inputs (`nodeFollowerProjections`).
 *
 * Building it reads nothing: it needs no database or node connection, and
 * no origin (`nodeIndexPlan`), so a role's reader opens with it before its
 * follower has run.
 */
import {
  DEPLOYMENT_MANIFEST_L1_FINALITY,
  type DeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type FactStoreOptions,
  mergeTrackedSets,
  projectionStoreOptions,
  type TrackedSet,
} from "@al-ft/midgard-l1-follower";

import { followerRetainedDepth } from "../database/commit-anchor.js";
import {
  nodeSeededAddresses,
  protocolPaymentCredentials,
} from "./intent-journal.tracked-set.js";
import { type NodeIndexPlan, nodeIndexPlan } from "./l1-follower.plan.js";
import {
  type NodeFollowerProjections,
  nodeFollowerProjections,
} from "./l1-follower.projections.js";

/** The index's protocol part: everything but the node's own wallets. */
export type NodeProtocolIndex = Readonly<{
  /** The set the store records and compares at each start. */
  trackedSet: TrackedSet;
  projections: NodeFollowerProjections;
  /** k: the manifest's automaticRecoveryMaxDepth. */
  securityParameter: number;
}>;

export type NodeIndexSpec = NodeProtocolIndex &
  Readonly<{
    /** Tracked by address and seeded, never recorded. */
    wallets: readonly Buffer[];
  }>;

/**
 * The index's protocol part, from the deployment alone (no node keys): the
 * set the store records, its projections and k.
 */
export const nodeProtocolIndex = (
  contracts: Parameters<typeof protocolPaymentCredentials>[0],
  plan: NodeIndexPlan,
): NodeProtocolIndex => {
  const projections = nodeFollowerProjections(plan);
  return {
    trackedSet: mergeTrackedSets(
      {
        addresses: new Set(),
        paymentCredentials: new Set(protocolPaymentCredentials(contracts)),
        // The protocol-init tx qualifies through the hub oracle mint.
        policies: new Set([plan.hubOraclePolicyId]),
      },
      ...projections.projections.flatMap((projection) =>
        projection.trackedSet === undefined ? [] : [projection.trackedSet],
      ),
    ),
    projections,
    securityParameter: plan.securityParameter,
  };
};

/** The node's L1 index from its configuration, deployment and follower plan. */
export const nodeIndexSpec = (
  config: Parameters<typeof nodeSeededAddresses>[0],
  contracts: Parameters<typeof protocolPaymentCredentials>[0],
  plan: NodeIndexPlan,
): NodeIndexSpec => ({
  ...nodeProtocolIndex(contracts, plan),
  wallets: nodeSeededAddresses(config),
});

/** k: the manifest's automaticRecoveryMaxDepth, the default finality's
 * without a manifest. */
export const nodeSecurityParameter = (
  identity: Readonly<{
    manifest?: Pick<DeploymentManifest, "l1Finality"> | undefined;
  }>,
): number =>
  (identity.manifest?.l1Finality ?? DEPLOYMENT_MANIFEST_L1_FINALITY)
    .automaticRecoveryMaxDepth;

/**
 * The node's index from its configuration and deployment alone, or the
 * missing piece that stops it: the one its follower records (`followL1`).
 */
export const nodeIndexFromDeployment = (
  config: Parameters<typeof nodeSeededAddresses>[0] &
    Readonly<{ NETWORK: Parameters<typeof nodeIndexPlan>[0]["network"] }>,
  contracts: Parameters<typeof nodeIndexPlan>[0]["contracts"] &
    Parameters<typeof protocolPaymentCredentials>[0],
  identity: Parameters<typeof nodeSecurityParameter>[0],
):
  | Readonly<{ kind: "index"; index: NodeIndexSpec }>
  | Readonly<{ kind: "unconfigured"; detail: string }> => {
  const plan = nodeIndexPlan({
    network: config.NETWORK,
    contracts,
    securityParameter: nodeSecurityParameter(identity),
  });
  return plan.kind === "unconfigured"
    ? plan
    : { kind: "index", index: nodeIndexSpec(config, contracts, plan.plan) };
};

/**
 * The writer's store options: the index with every projection's tables and
 * hooks, and the retained depth its commits' anchors need.
 */
export const nodeFollowerStoreOptions = (
  index: NodeIndexSpec,
  commitEventDepth: number,
): FactStoreOptions => ({
  ...projectionStoreOptions(
    index.projections.projections,
    {
      securityParameter: index.securityParameter,
      // Keeps every commit's anchor block while the follower runs up to k
      // blocks past the commit's planning view.
      retainedDepth: followerRetainedDepth(
        index.securityParameter,
        commitEventDepth,
      ),
      trackedSet: index.trackedSet,
    },
    "postgres",
  ),
  // Seeded (§5.3 step 4), never in the tracked-set record.
  wallets: index.wallets,
});
