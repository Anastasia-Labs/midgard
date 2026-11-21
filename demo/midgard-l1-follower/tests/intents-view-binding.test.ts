import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  currentViewIn,
  decideSubmitIn,
  type DialectName,
  type FactStore,
  openPostgresFactStore,
  openSqliteFactStore,
  readIntentEventsIn,
  recordIntentIn,
  type RecordIntentResult,
  type View,
} from "../src/index.js";
import {
  encodeSimTx,
  SIM_ORIGIN,
  type SimTx,
  simTxHash,
} from "../src/testing/index.js";
import {
  actionOf,
  type Harness,
  harnessStoreOptions,
  open,
  s6,
  spend,
  u,
} from "./support/intents-harness.js";
import { testDatabases } from "./support/postgres.js";

const databases = testDatabases();

afterAll(async () => {
  await databases.dropAll();
});

const stores: Record<DialectName, () => Promise<FactStore>> = {
  sqlite: () =>
    Promise.resolve(
      openSqliteFactStore({
        ...harnessStoreOptions("sqlite"),
        path: ":memory:",
      }),
    ),
  postgres: async () => {
    const database = await databases.create();
    return openPostgresFactStore({
      ...harnessStoreOptions("postgres"),
      connection: { connectionString: database.url },
    });
  },
};

/**
 * §15 I5: S5 and S6 against a planner racing a rewind. A role plans at a
 * view V = (g, P_b), records the signed bytes with `builtAt = V`, and sends
 * only on an S6 decision taken in one transaction with the view check.
 */
describe.each(["sqlite", "postgres"] as const)(
  "intent view binding (%s)",
  (dialect) => {
    let h: Harness;

    afterEach(async () => {
      await h.store.close();
    });

    const start = async (): Promise<Harness> => {
      h = await open(stores[dialect]);
      await h.store.initialize(SIM_ORIGIN);
      return h;
    };

    const view = async (): Promise<View> => {
      const current = await h.store.transaction("read", (sql) =>
        currentViewIn(sql, h.store.dialect),
      );
      if (current === null) throw new Error("no view");
      return current;
    };

    const recordAt = (tx: SimTx, builtAt: View): Promise<RecordIntentResult> =>
      h.store.transaction("write", (sql) =>
        recordIntentIn(sql, h.store.dialect, {
          family: "commit",
          workflowKey: `commit:${simTxHash(tx).toString("hex")}`,
          txCbor: encodeSimTx(tx),
          isOwnOutput: (output) => output.address.equals(u.trackedAddress),
          builtAt,
        }),
      );

    const decide = (tx: SimTx) =>
      h.store.transaction("write", (sql) =>
        decideSubmitIn(sql, h.store.dialect, simTxHash(tx)),
      );

    const eventKinds = async (tx: SimTx) =>
      (
        await h.store.transaction("read", (sql) =>
          readIntentEventsIn(sql, simTxHash(tx)),
        )
      ).map((event) => event.kind);

    it("records a plan whose point a rewind removed as stale_at_write and never sends it", async () => {
      await start();
      const input = await h.fund();
      await h.forward();
      const planned = await view();
      // The rewind lands between plan and record and removes P_b.
      await h.forward();
      await h.backward(2);
      await h.forward();
      const tx = spend(h.chain, input);
      const recorded = await recordAt(tx, planned);
      expect(recorded).toMatchObject({ kind: "recorded", stale: true });
      expect(await eventKinds(tx)).toEqual(["signed", "stale_at_write"]);
      // No send on its own write: S6's decision holds it...
      expect(await decide(tx)).toMatchObject({
        kind: "hold",
        reason: "stale_at_write",
      });
      // ...and S6's reconciler, under the current view, abandons it when the
      // family no longer wants it, without a send.
      let sent = 0;
      const reconciler = s6(h.store, {
        wanted: () => Promise.resolve(false),
        submit: () => {
          sent += 1;
          return Promise.resolve({ kind: "accepted" });
        },
      });
      expect(await actionOf(reconciler, tx)).toBe("abandon");
      expect(sent).toBe(0);
      expect(await decide(tx)).toMatchObject({
        kind: "hold",
        reason: "abandoned",
      });
      expect(await eventKinds(tx)).not.toContain("submit_attempt");
    });

    it("records a planner's own staleness as stale_at_write even at a valid view", async () => {
      await start();
      const input = await h.fund();
      const tx = spend(h.chain, input);
      const recorded = await h.store.transaction("write", async (sql) =>
        recordIntentIn(sql, h.store.dialect, {
          family: "commit",
          workflowKey: "commit:planned-across-a-rewind",
          txCbor: encodeSimTx(tx),
          isOwnOutput: (output) => output.address.equals(u.trackedAddress),
          builtAt: (await currentViewIn(sql, h.store.dialect))!,
          staleBecause: "planned_across_rewind",
        }),
      );
      expect(recorded).toMatchObject({ kind: "recorded", stale: true });
      const events = await h.store.transaction("read", (sql) =>
        readIntentEventsIn(sql, simTxHash(tx)),
      );
      expect(events.map((event) => event.kind)).toEqual([
        "signed",
        "stale_at_write",
      ]);
      expect(events[1]?.detail).toMatchObject({
        reason: "planned_across_rewind",
      });
      expect(await decide(tx)).toMatchObject({ kind: "hold" });
    });

    it("accepts a view on the fast path: the generation has not moved", async () => {
      await start();
      const input = await h.fund();
      const planned = await view();
      await h.forward();
      await h.forward();
      const tx = spend(h.chain, input);
      expect(await recordAt(tx, planned)).toMatchObject({
        kind: "recorded",
        stale: false,
      });
      expect(await eventKinds(tx)).toEqual(["signed"]);
      expect(await decide(tx)).toMatchObject({ kind: "send" });
      expect(await eventKinds(tx)).toEqual(["signed", "submit_attempt"]);
    });

    it("accepts a view on the point path after an unrelated rollback", async () => {
      await start();
      const input = await h.fund();
      await h.forward();
      const planned = await view();
      // A rewind above P_b: the generation moves, P_b stays canonical.
      await h.forward();
      await h.forward();
      await h.backward(1);
      expect((await view()).generation).toBe(planned.generation + 1);
      const tx = spend(h.chain, input);
      expect(await recordAt(tx, planned)).toMatchObject({
        kind: "recorded",
        stale: false,
      });
      expect(await decide(tx)).toMatchObject({ kind: "send" });
    });

    it("holds a send whose view a rewind removed between record and submit", async () => {
      await start();
      const input = await h.fund();
      await h.forward();
      const tx = spend(h.chain, input);
      expect(await recordAt(tx, await view())).toMatchObject({
        kind: "recorded",
        stale: false,
      });
      await h.forward();
      await h.backward(2);
      expect(await decide(tx)).toMatchObject({
        kind: "hold",
        reason: "view_stale",
      });
      expect(await eventKinds(tx)).toEqual(["signed"]);
      // S6 decides under the current view: the input survived, it is wanted,
      // so the reconciler sends the exact bytes.
      const sent: Buffer[] = [];
      const reconciler = s6(h.store, {
        submit: (intent) => {
          sent.push(intent.txCbor);
          return Promise.resolve({ kind: "accepted" });
        },
      });
      expect(await actionOf(reconciler, tx)).toBe("resubmit");
      expect(sent.map((bytes) => bytes.equals(encodeSimTx(tx)))).toEqual([
        true,
      ]);
    });

    it("sends nothing when a rewind removes the reconciler pass's point mid-pass", async () => {
      await start();
      const input = await h.fund();
      await h.forward();
      const tx = spend(h.chain, input);
      await h.record(tx);
      let sent = 0;
      let rewound = false;
      const reconciler = s6(h.store, {
        wanted: async () => {
          if (!rewound) {
            rewound = true;
            await h.backward(1);
            await h.forward();
          }
          return true;
        },
        submit: () => {
          sent += 1;
          return Promise.resolve({ kind: "accepted" });
        },
      });
      expect(await actionOf(reconciler, tx)).toBe("wait_tip_moved");
      expect(sent).toBe(0);
      expect(await eventKinds(tx)).toEqual(["signed"]);
      // The next pass decides at the new tip and sends.
      expect(await actionOf(reconciler, tx)).toBe("resubmit");
      expect(sent).toBe(1);
    });
  },
);
