import type { AvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";

/**
 * The transactions a reconciliation pass may read: the actor's pending,
 * included, conflicting and confirmed intents in this deployment (a
 * confirmed ancestor is read once its descendant leaves the chain), and its
 * confirmed anchors in every deployment. An expired intent is never read.
 */
const watcherAvailabilityJournalTransactions = (
  journal: Pick<
    AvailabilityOperationJournal,
    "actorSnapshot" | "finalizedAnchors"
  >,
  actor: string,
  deploymentIdentity: string,
): string[] =>
  [
    ...new Set([
      ...journal
        .actorSnapshot(actor, deploymentIdentity)
        .retainedAttempts.filter(({ state }) => state !== "expired")
        .map(({ txHash }) => txHash),
      ...journal.finalizedAnchors(actor).map(({ intent }) => intent.txHash),
    ]),
  ].sort();

/**
 * One reconciliation pass over transactions the follower keeps for it.
 * Before the pass, `pin` holds exactly the journal's transactions in the
 * follower, so none the pass reads has been pruned; the next pass releases
 * what this one retired or expired. Every pass pins what the one before it
 * persisted, long before the follower could prune its transaction.
 */
export const reconcileWatcherAvailabilityJournal = async (
  context: SDK.DaAvailabilityOperationContext,
  pin: (txHashes: readonly string[]) => Promise<void>,
): Promise<readonly SDK.DaAvailabilityOperationResult[]> => {
  await pin(
    watcherAvailabilityJournalTransactions(
      context.journal,
      context.actor,
      context.deploymentIdentity,
    ),
  );
  return await SDK.reconcileDaAvailabilityOperations(context);
};
