/**
 * The follower-change driver's operator-set hook (NC14, D-N7). On every
 * driver run it brings the operator set up to the follower's view (only the
 * rows that changed since its last read; a load after a rewind or a prune),
 * derives this operator's membership from it and publishes both.
 *
 * - Membership is a view of the facts: a removal raises `operator_removed`
 *   and a later read without it (a rollback that undid it, a re-activation)
 *   clears it. No journal, no provider, no exit.
 * - The evidence that this operator was once active outlives the facts'
 *   retention through the activity record (`activity.ts`), written the first
 *   time the set holds an own active-node row, at any landed depth, and
 *   replaced or cleared once its block reads orphaned.
 * - A malformed directory holds `/readyz` on `operator_set_unhealthy`; the
 *   membership is still derived from this operator's own nodes.
 */
import {
  blockAtOrBeforeSlotIn,
  type DepthParameters,
  type FactStore,
  pointStatusIn,
  type SqlTx,
  type StoredBlock,
} from "@al-ft/midgard-l1-follower";
import { depth, levelAtDepth } from "@al-ft/midgard-l1-follower/heads";

import type { DriverHold, DriverHook } from "../l1-events/driver.js";
import type { OperatorActivityRecord, RecordedActivity } from "./activity.js";
import {
  classifyOperatorMembership,
  type OperatorMembership,
  type RemovalPoint,
} from "./membership.js";
import type { OperatorSet, OperatorSetMirror } from "./set.js";

/** The `/readyz` reason of a directory that is not well formed. */
export const OPERATOR_SET_UNHEALTHY = "operator_set_unhealthy";

/** What one hook run read and derived. */
export type OperatorSetRun = Readonly<{
  set: OperatorSet;
  membership: OperatorMembership;
  /** The fact rows the set read this run (0 when nothing changed). */
  rowsRead: number;
  /** Whether the set loaded the live outputs instead of the changes. */
  loaded: boolean;
}>;

const blockAt = async (
  tx: SqlTx,
  slot: number,
): Promise<StoredBlock | null> => {
  const block = await blockAtOrBeforeSlotIn(tx, slot);
  return block !== null && block.slot === slot ? block : null;
};

export const operatorSetHook =
  (options: {
    readonly store: Pick<FactStore, "dialect" | "transaction">;
    readonly mirror: OperatorSetMirror;
    readonly depth: DepthParameters;
    readonly activity: OperatorActivityRecord;
    /** Publishes the run; runs before the hold is returned. */
    readonly publish: (run: OperatorSetRun) => Promise<void>;
  }): DriverHook =>
  async () => {
    const { store, mirror, activity } = options;
    const recorded = await activity.read();
    const read = await store.transaction("read", async (tx) => {
      const refreshed = await mirror.refresh(tx, store.dialect);
      if (refreshed.kind !== "ok") return refreshed;
      const { set } = refreshed;
      const depthOf = (block: StoredBlock) =>
        depth(set.view.height, block.height);
      // A recorded activation counts unless the facts show it orphaned.
      const recordedStatus =
        recorded === null
          ? null
          : await pointStatusIn(tx, store.dialect, recorded.point);
      const recordedCounts =
        recordedStatus !== null &&
        (recordedStatus.kind === "canonical" ||
          recordedStatus.kind === "point_beyond_retention");
      // New evidence when there is no counting record: the deepest own
      // active-node row's creating block on the stored chain, or, when that
      // block is below the retained window, the view itself (its chain holds
      // the row).
      let evidence: RecordedActivity | null = null;
      if (!recordedCounts && set.ownActivity.length > 0) {
        const created = set.ownActivity
          .flatMap((own) => (own.createdSlot === null ? [] : [own.createdSlot]))
          .sort((a, b) => a - b);
        for (const slot of created) {
          const block = await blockAt(tx, slot);
          if (block !== null) {
            evidence = {
              point: { slot: block.slot, hash: block.hash },
              height: block.height,
            };
            break;
          }
        }
        evidence ??= { point: set.view.point, height: set.view.height };
      }
      const orphaned = recordedStatus?.kind === "point_not_canonical";
      // Where the removal sits: the own retired node, or the last spend of
      // the own active node.
      const spends = set.ownActivity.flatMap((own) =>
        own.spentSlot === null ? [] : [own.spentSlot],
      );
      const removalSlot =
        set.ownRetired?.createdSlot ??
        (spends.length === 0 ? null : Math.max(...spends));
      const removalBlock =
        removalSlot === null ? null : await blockAt(tx, removalSlot);
      const removedAt: RemovalPoint | null =
        removalBlock === null
          ? null
          : {
              slot: removalBlock.slot,
              depth: depthOf(removalBlock),
              level:
                levelAtDepth(depthOf(removalBlock), options.depth) ??
                "not on the chain",
            };
      return {
        kind: "ok" as const,
        set,
        rowsRead: refreshed.rowsRead,
        loaded: refreshed.loaded,
        recordedCounts,
        evidence,
        orphaned,
        removedAt,
      };
    });
    // No view yet: the first run after the follower has one reads it.
    if (read.kind !== "ok") return undefined;
    if (read.evidence !== null) await activity.write(read.evidence);
    else if (read.orphaned) await activity.clear();
    const membership = classifyOperatorMembership(
      read.set,
      read.recordedCounts || read.evidence !== null,
      read.removedAt,
    );
    await options.publish({
      set: read.set,
      membership,
      rowsRead: read.rowsRead,
      loaded: read.loaded,
    });
    const hold: DriverHold | undefined =
      read.set.unhealthy === null
        ? undefined
        : { reason: OPERATOR_SET_UNHEALTHY, detail: read.set.unhealthy };
    return hold;
  };
