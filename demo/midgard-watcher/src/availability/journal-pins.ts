import type { AvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

/**
 * The transactions a reconciliation pass may read: the actor's pending,
 * included, conflicting and confirmed intents in this deployment (a
 * confirmed ancestor is read once its descendant leaves the chain), its
 * confirmed anchors in every deployment, and the foreign terminal step of
 * each released workflow row not yet final, which the release check re-reads
 * until the row settles. An expired intent is never read.
 */
const watcherAvailabilityJournalTransactions = (
  journal: Pick<
    AvailabilityOperationJournal,
    "actorSnapshot" | "finalizedAnchors" | "unsettledReleases"
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
      ...journal.unsettledReleases(actor).map(({ release }) => release.txHash),
    ]),
  ].sort();

/**
 * One reconciliation pass over transactions the follower keeps for it.
 * Before the pass, `pin` holds exactly the journal's transactions in the
 * follower, so none the pass reads has been pruned; the next pass releases
 * what this one retired or expired. A transaction persisted after the pass
 * pinned is pinned when it is submitted
 * ({@link pinnedWatcherAvailabilitySubmit}).
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

/**
 * Pins exactly the journal's transactions plus `extra`: a transaction the
 * journal is about to depend on is pinned before the write that records it.
 */
export const pinWatcherAvailabilityJournal =
  (
    context: Pick<
      SDK.DaAvailabilityOperationContext,
      "journal" | "actor" | "deploymentIdentity"
    >,
    pin: (txHashes: readonly string[]) => Promise<void>,
  ) =>
  async (extra: readonly string[] = []): Promise<void> => {
    await pin(
      [
        ...new Set([
          ...watcherAvailabilityJournalTransactions(
            context.journal,
            context.actor,
            context.deploymentIdentity,
          ),
          ...extra,
        ]),
      ].sort(),
    );
  };

/**
 * `submit`, with the journal's transactions pinned first, the submitted one
 * among them. A pass pins at its start, but an actuated step is signed,
 * persisted and submitted later in the pass; unpinned, it could land, have
 * its outputs spent and be pruned before a watcher down past the
 * follower's retained depth ran its next pass, which would then no longer
 * find it.
 */
export const pinnedWatcherAvailabilitySubmit =
  (
    context: Pick<
      SDK.DaAvailabilityOperationContext,
      "journal" | "actor" | "deploymentIdentity"
    >,
    pin: (txHashes: readonly string[]) => Promise<void>,
    submit: (signedCbor: string) => Promise<string>,
  ) =>
  async (signedCbor: string): Promise<string> => {
    const txHash = CML.hash_transaction(
      CML.Transaction.from_cbor_hex(signedCbor).body(),
    ).to_hex();
    await pinWatcherAvailabilityJournal(context, pin)([txHash]);
    return await submit(signedCbor);
  };
