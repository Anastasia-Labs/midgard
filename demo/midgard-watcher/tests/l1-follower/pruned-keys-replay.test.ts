/**
 * The pruned-key record hooks are `record` prune hooks: they run in every
 * prune step, in the transaction of the D-t deletes they record, while the
 * tracked-set record's `replaying` mark is set as well as when it is not.
 * In both dialects, with the mark set, a prune step past k records the
 * followed unit and the removed header whose closed history rows it deletes,
 * and the unpinned read of the unit minted again takes `beyond_retention`.
 */
import type { FactStore } from "@al-ft/midgard-l1-follower";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  WATCHER_PRUNED_HEADERS_TABLE,
  WATCHER_PRUNED_UNITS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
  WATCHER_UNIT_HISTORY_TABLE,
} from "../../src/l1-follower/tables.js";
import { reasonOf } from "../support/l1-follower-raw-reads-fixture.js";
import { postgresSchemas } from "../support/postgres-schemas.js";
import {
  closeRemovedHeaders,
  FOLLOWED_UNIT,
  removedHeader,
} from "../support/proof-retention-removed-header.js";

afterEach(closeRemovedHeaders);
const schemas = postgresSchemas();
afterAll(() => schemas.dropAll());

const setReplayMark = (store: FactStore, replaying: boolean) =>
  store.transaction("write", (tx) =>
    tx.query(
      "UPDATE l1_follower_tracked_set SET replaying = ? WHERE id = 1 RETURNING id",
      [replaying ? 1 : 0],
    ),
  );

describe.each(["sqlite", "postgres"] as const)(
  "pruned-key record hooks while a store reset replays (%s)",
  (dialect) => {
    it("with the replay mark set, a prune step past k records the keys whose closed history rows it deletes, and the unpinned read of the unit minted again is beyond_retention", async () => {
      const r = await removedHeader({
        pin: false,
        resolveAtIngest: true,
        followUnit: true,
        ...(dialect === "postgres" ? { open: await schemas.open() } : {}),
      });
      expect(r.h.store.dialect.name).toBe(dialect);
      expect(await setReplayMark(r.h.store, true)).toHaveLength(1);
      expect(await r.h.store.trackedSetRecord()).toMatchObject({
        replaying: true,
      });
      expect(
        await r.count(WATCHER_UNIT_HISTORY_TABLE, "unit", FOLLOWED_UNIT),
      ).toBe(2);
      expect(
        await r.count(
          WATCHER_QUEUE_UNIT_HISTORY_TABLE,
          "header_hash",
          r.header,
        ),
      ).toBeGreaterThan(0);
      await r.passK();
      // The D-t deletes ran, and so did the record hooks beside them.
      expect(
        await r.count(WATCHER_UNIT_HISTORY_TABLE, "unit", FOLLOWED_UNIT),
      ).toBe(0);
      expect(
        await r.count(
          WATCHER_QUEUE_UNIT_HISTORY_TABLE,
          "header_hash",
          r.header,
        ),
      ).toBe(0);
      expect(
        await r.count(WATCHER_PRUNED_UNITS_TABLE, "unit", FOLLOWED_UNIT),
      ).toBe(1);
      expect(
        await r.count(WATCHER_PRUNED_HEADERS_TABLE, "header_hash", r.header),
      ).toBe(1);
      await r.remint();
      expect(
        reasonOf(
          await r.h.reads().unitHistoryAtPoint(FOLLOWED_UNIT, r.h.tipPoint()),
        ),
      ).toBe("beyond_retention");
      // The mark held throughout.
      expect(await r.h.store.trackedSetRecord()).toMatchObject({
        replaying: true,
      });
    });
  },
);
