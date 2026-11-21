import { type FactStore, isFinal } from "@al-ft/midgard-l1-follower";

/**
 * Whether a block `depth` deep is past the depth an own proof's completion
 * waits for: k + 1 (automaticRecoveryMaxDepth + 1), one block beyond the
 * deepest rollback the store rewinds. A workflow records `completed` only
 * past it, so the inclusion wait and the release of a hold no objective owns
 * both stop at it.
 */
export const beyondCompletionDepth = (
  depth: number,
  securityParameter: number,
): boolean => isFinal(depth, { securityParameter: securityParameter + 1 });

/** The follower store a proof retention reads and pins through. */
export type WatcherProofRetentionStore = Pick<
  FactStore,
  | "transaction"
  | "pinRetained"
  | "securityParameter"
  | "dialect"
  | "pointStatus"
>;

export type WatcherBlockDepth = Readonly<{
  /**
   * The depth of a block on the follower's chain (1 at the tip), or `null`
   * when the chain does not hold it: a rollback replaced it, or the store
   * cannot tell (pruned, not started).
   */
  blockDepth(
    point: Readonly<{ slot: string; blockHash: string }>,
  ): Promise<number | null>;
}>;

export const followerBlockDepth = (
  store: Pick<FactStore, "pointStatus">,
): WatcherBlockDepth => ({
  blockDepth: async ({ slot, blockHash }) => {
    const status = await store.pointStatus({
      slot: Number(slot),
      hash: Buffer.from(blockHash, "hex"),
    });
    return status.kind === "canonical" ? status.depth : null;
  },
});
