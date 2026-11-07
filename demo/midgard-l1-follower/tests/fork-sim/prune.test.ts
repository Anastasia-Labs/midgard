import fc from "fast-check";
import { describe, expect, it } from "vitest";

import { applyChainSyncEvent } from "../../src/follow/chain-sync.js";
import {
  type DialectName,
  type FactStore,
  type FollowerProjection,
  type MigrationSet,
  openSqliteFactStore,
} from "../../src/index.js";
import {
  buildForkSteps,
  forkCorpus,
  type ForkScenario,
  forkScenarioArbitrary,
  runForkScenario,
  SIM_ORIGIN,
  simStoreOptions,
} from "../../src/testing/index.js";
import { FIXTURE_PROJECTION, openSqlite, SIM_K } from "../support/fork-sim.js";

const corpus = forkCorpus(SIM_K);
const pruneCorpus = corpus.filter((entry) =>
  entry.scenario.episodes.some((episode) => episode.prune === true),
);

/**
 * A projection that pins some txs and their blocks through `retentionPins`:
 * an owner-retained append-only table recording every qualifying tx whose
 * hash starts with an even byte.
 */
const PIN_PROJECTION: FollowerProjection = {
  name: "pins",
  temporalTables: [
    {
      name: "sim_tx_pins",
      shape: "append_only",
      slotColumn: "slot",
      retention: { kind: "owner", description: "kept for the pin test" },
    },
  ],
  migrations: (dialect: DialectName): MigrationSet => ({
    namespace: "pins",
    migrations: [
      {
        id: "0001_sim_tx_pins",
        sql: `-- class: D-t; retention: owner (the pin test keeps every row)
CREATE TABLE sim_tx_pins (tx_hash ${dialect === "postgres" ? "bytea" : "BLOB"} PRIMARY KEY, slot ${dialect === "postgres" ? "bigint" : "INTEGER"} NOT NULL);`,
      },
    ],
  }),
  derivations: [
    {
      name: "pins",
      writes: ["sim_tx_pins"],
      apply: async ({ tx, block, qualified }) => {
        for (const entry of qualified)
          if ((entry.tx.hash[0] ?? 1) % 2 === 0)
            await tx.query(
              "INSERT INTO sim_tx_pins (tx_hash, slot) VALUES (?, ?)",
              [entry.tx.hash, block.point.slot],
            );
      },
    },
  ],
  retentionPins: {
    txs: [{ table: "sim_tx_pins", column: "tx_hash" }],
    blocks: [{ table: "sim_tx_pins", column: "slot" }],
  },
};

/** Opens the store under test without the projections' retention pins. */
const openWithoutPins: typeof openSqlite = (optionsFor) =>
  openSqlite((dialect) => {
    const { retentionPins: _ignored, ...options } = optionsFor(dialect);
    return options;
  });

describe("fork simulator with pruning", () => {
  it("has prune cases in the corpus", () => {
    expect(pruneCorpus.length).toBeGreaterThanOrEqual(6);
  });

  it.each(pruneCorpus.map((entry) => [entry.name, entry.scenario] as const))(
    "corpus: %s prunes rows and keeps every retained row",
    async (_, scenario) => {
      const outcome = await runForkScenario(scenario, {
        open: openSqlite,
        k: SIM_K,
        projections: [FIXTURE_PROJECTION],
      });
      expect(outcome).toMatchObject({ ok: true });
      expect(outcome.stats.prunes).toBe(
        2 * scenario.episodes.filter((e) => e.prune === true).length,
      );
      // Not vacuous: rows were pruned and then compared.
      expect(outcome.stats.prunedRows).toBeGreaterThan(0);
    },
  );

  it("holds for random scenarios with prunes (fast-check)", async () => {
    let prunedRows = 0;
    await fc.assert(
      fc.asyncProperty(forkScenarioArbitrary(SIM_K), async (scenario) => {
        const outcome = await runForkScenario(scenario, {
          open: openSqlite,
          k: SIM_K,
          projections: [FIXTURE_PROJECTION, PIN_PROJECTION],
        });
        if (!outcome.ok)
          throw new Error(`step ${outcome.step}: ${outcome.reason}`);
        prunedRows += outcome.stats.prunedRows;
      }),
      { numRuns: 60, seed: 0x9_e7 },
    );
    expect(prunedRows).toBeGreaterThan(0);
  });

  it("keeps rows a projection pins, and fails when they are pruned", async () => {
    const failures: string[] = [];
    for (const { scenario } of pruneCorpus) {
      const pinned = await runForkScenario(scenario, {
        open: openSqlite,
        k: SIM_K,
        projections: [FIXTURE_PROJECTION, PIN_PROJECTION],
      });
      expect(pinned).toMatchObject({ ok: true });
      const unpinned = await runForkScenario(scenario, {
        open: openWithoutPins,
        k: SIM_K,
        projections: [FIXTURE_PROJECTION, PIN_PROJECTION],
      });
      if (!unpinned.ok) failures.push(unpinned.reason);
    }
    // The store without the pins prunes pinned txs (or their blocks), and
    // the retention check sees it.
    expect(failures.length).toBeGreaterThan(0);
    expect(failures.join("\n")).toMatch(
      /l1_(txs|blocks): \d+ retained rows pruned/u,
    );
  });

  it("fails when prune removes a row retention keeps", async () => {
    const outcome = await runForkScenario(pruneCorpus[0]!.scenario, {
      open: openSqlite,
      k: SIM_K,
      projections: [FIXTURE_PROJECTION],
      // A rogue writer deletes a live output after each event: retention
      // keeps every live output, so the pruned comparison must fail.
      prepare: (store) => {
        const prune = store.prune;
        (store as { prune: FactStore["prune"] }).prune = async (budget) => {
          await store.transaction("write", (tx) =>
            tx.query(
              "DELETE FROM l1_outputs WHERE rowid IN (SELECT rowid FROM l1_outputs WHERE spent_slot IS NULL LIMIT 1)",
            ),
          );
          return prune(budget);
        };
      },
    });
    expect(outcome).toMatchObject({ ok: false });
    expect(outcome.ok ? "" : outcome.reason).toMatch(/retained rows pruned/u);
  });
});

/** A prune before a depth-k rollback over a long lead, then one after it. */
const E7_SCENARIO: ForkScenario = corpus.find(
  (entry) => entry.name === `reland pruned around a depth-${SIM_K} rollback`,
)!.scenario;

describe("E7: pruned rows, then a rewind that drops the prune boundary", () => {
  it("rewinds within k over pruned rows, and every read stays safe", async () => {
    const { steps } = buildForkSteps(E7_SCENARIO, [FIXTURE_PROJECTION]);
    const store = openSqliteFactStore({
      ...simStoreOptions([FIXTURE_PROJECTION], SIM_K, "sqlite"),
      path: ":memory:",
    });
    try {
      expect((await store.start()).kind).toBe("ready");
      expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
      const outputs = async () =>
        store.transaction("read", async (tx) =>
          (
            await tx.query(
              "SELECT tx_hash, output_index, spent_tx FROM l1_outputs",
            )
          ).map((row) => ({
            txHash: Buffer.from(row.tx_hash as Uint8Array),
            index: Number(row.output_index),
            spentTx:
              row.spent_tx === null
                ? null
                : Buffer.from(row.spent_tx as Uint8Array),
          })),
        );
      const key = (o: { txHash: Buffer; index: number }) =>
        `${o.txHash.toString("hex")}#${o.index}`;
      let checked = false;
      for (let i = 0; i < steps.length && !checked; i += 1) {
        const step = steps[i]!;
        const applied = await applyChainSyncEvent(store, step.event);
        expect(["applied", "rewound"]).toContain(applied.result.kind);
        if (step.prune !== true || step.event.kind !== "roll_forward") continue;
        // The old branch's last block: prune, then the depth-k rollback.
        const before = await outputs();
        let s = SIM_ORIGIN.point.slot;
        for (;;) {
          const pruned = await store.prune(2);
          if ("kind" in pruned) throw new Error(`prune: ${pruned.kind}`);
          s = pruned.prunedThroughSlot;
          if (pruned.done) break;
        }
        const left = new Set((await outputs()).map(key));
        const gone = before.filter((o) => !left.has(key(o)));
        expect(gone.length).toBeGreaterThan(0);
        const boundary = await store.blockAtOrBeforeSlot(s);
        expect(boundary?.slot).toBe(s);

        const rollback = steps[i + 1]!;
        expect(rollback.event.kind).toBe("roll_backward");
        const rewound = await applyChainSyncEvent(store, rollback.event);
        expect(rewound.result.kind).toBe("rewound");
        const cursor = (await store.cursor())!;
        // The rewind moved the boundary (k below the cursor) under the
        // rows pruned before: they now sit within k of the cursor.
        expect(cursor.height - SIM_K).toBeLessThan(boundary!.height);
        expect(cursor.prunedThroughSlot).toBe(s);

        for (const output of gone) {
          // A pruned output reads as unknown, never as live or unspent.
          expect(await store.spenderOf(output)).toEqual({ kind: "unknown" });
          expect(await store.output(output)).toBeNull();
          expect(store.isTrackedLive(output)).toBe(false);
        }
        const prunedTxs = gone.flatMap((o) =>
          o.spentTx === null ? [] : [o.spentTx],
        );
        let absentTxs = 0;
        for (const hash of prunedTxs)
          if ((await store.txByHash(hash)) === null) absentTxs += 1;
        expect(absentTxs).toBeGreaterThan(0);
        // Reads at a point below the retained window are refused.
        const below = { slot: s - 1, hash: Buffer.alloc(32) };
        expect((await store.pointStatus(below)).kind).toBe(
          "point_beyond_retention",
        );
        expect(
          (
            await store.liveUtxos(
              { by: "address", address: Buffer.alloc(29) },
              below,
            )
          ).kind,
        ).toBe("point_beyond_retention");
        // A rewind below the retained window is R1, not a silent rewind.
        const deeper = await store.rewind({
          slot: SIM_ORIGIN.point.slot,
          hash: SIM_ORIGIN.point.hash,
        });
        expect(deeper).toMatchObject({
          kind: "intervention",
          reason: "rollback_beyond_k",
        });
        expect((await store.checkInvariants()).ok).toBe(true);
        checked = true;
      }
      expect(checked).toBe(true);
    } finally {
      await store.close();
    }
  });

  it("the full scenario stays equal to a fresh replay within retention", async () => {
    const outcome = await runForkScenario(E7_SCENARIO, {
      open: openSqlite,
      k: SIM_K,
      projections: [FIXTURE_PROJECTION],
    });
    expect(outcome).toMatchObject({ ok: true });
    expect(outcome.stats.prunedRows).toBeGreaterThan(0);
  });
});
