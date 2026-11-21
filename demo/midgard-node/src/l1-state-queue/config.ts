/**
 * What the node's landed state queue (plan §5.5 P1, N2) reads from the
 * deployment: the state-queue address and its NFT policy.
 */
import type { TrackedSet } from "@al-ft/midgard-l1-follower";
import { getAddressDetails } from "@lucid-evolution/lucid";

/** The most nodes (root excluded) a healthy landed queue may hold. */
export const STATE_QUEUE_MAX_NODES = 10_000;

export type StateQueueProjectionConfig = Readonly<{
  /** The state-queue spending address, as raw address bytes (hex). */
  address: string;
  /** The state-queue NFT policy (56 hex). */
  policyId: string;
  /** The node cap: more live nodes make the queue unhealthy (`over_cap`). */
  maxNodes: number;
}>;

/** The config of a deployment's state queue (its authenticated validator). */
export const stateQueueProjectionConfig = (
  stateQueue: Readonly<{ spendingScriptAddress: string; policyId: string }>,
  maxNodes: number = STATE_QUEUE_MAX_NODES,
): StateQueueProjectionConfig => ({
  address: getAddressDetails(
    stateQueue.spendingScriptAddress,
  ).address.hex.toLowerCase(),
  policyId: stateQueue.policyId.toLowerCase(),
  maxNodes,
});

/**
 * The follower tracked set P1 needs: every output at the queue address, and
 * the txs minting under its policy. The facts hold whatever third parties
 * pay to the address; the projection reads only outputs under the policy.
 */
export const stateQueueTrackedSet = (
  config: StateQueueProjectionConfig,
): TrackedSet => ({
  addresses: new Set([config.address]),
  paymentCredentials: new Set(),
  policies: new Set([config.policyId]),
});
