import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import type { ChainSyncEvent } from "@al-ft/l1-node-transport";
import { afterAll, describe, expect, it } from "vitest";

import {
  type BlockSummary,
  FOLLOWER_MIGRATION_FAILED,
  type FollowerProjection,
  openSqliteFactStore,
} from "../src/index.js";
import {
  buildForkSteps,
  forkCorpus,
  simStoreOptions,
} from "../src/testing/index.js";
import { fixtureMigrations } from "./support/fixture.js";
import { follow, script } from "./support/follow-loop.js";
import { FIXTURE_PROJECTION, SIM_K } from "./support/fork-sim.js";
import { testDatabases } from "./support/postgres.js";
import { resetAdapters } from "./support/reset-stores.js";
import { chain, fill, ORIGIN } from "./support/small-chain.js";

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-store-recovery-"));

afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

const [b1, b2] = chain() as [BlockSummary, BlockSummary, BlockSummary];

describe.each(resetAdapters(databases, scratch))(
  "an R5 store recovers on a clean start ($name)",
  (adapter) => {
    it("refuses prune while broken, and a start whose invariants hold clears it", async () => {
      const location = await adapter.create();
      const store = location.store();
      const backend = location.backend();
      try {
        expect(await store.start()).toMatchObject({ kind: "ready" });
        expect(await store.initialize(ORIGIN)).toMatchObject({
          kind: "initialized",
        });
        expect(await store.applyBlock(b1)).toMatchObject({ kind: "applied" });
        // A fact above the cursor breaks INV5.
        const above = [fill(0x0e, 36), fill(0x0f), b1.point.slot + 100];
        await backend.transaction("write", (tx) =>
          tx.query(
            "INSERT INTO l1_protocol_init (one_shot, tx_hash, slot) VALUES (?, ?, ?)",
            above,
          ),
        );
        expect(await store.start()).toMatchObject({
          kind: "intervention",
          reason: "store_integrity",
        });
        expect(await store.prune()).toMatchObject({ kind: "error" });
        expect(await store.applyBlock(b2)).toMatchObject({
          kind: "intervention",
          reason: "store_integrity",
        });
        // The operator repairs the rows; the next start finds them sound.
        await backend.transaction("write", (tx) =>
          tx.query("DELETE FROM l1_protocol_init WHERE slot = ?", [above[2]!]),
        );
        expect(await store.start()).toMatchObject({ kind: "ready" });
        expect(await store.prune()).toMatchObject({ done: true });
        expect(await store.applyBlock(b2)).toMatchObject({ kind: "applied" });
      } finally {
        await backend.close();
        await store.close();
      }
    });
  },
);

describe("followChain: a migration the store refuses at start", () => {
  const events: readonly ChainSyncEvent[] = buildForkSteps(
    forkCorpus(SIM_K)[0]!.scenario,
    [FIXTURE_PROJECTION],
  ).steps.map((step) => step.event);

  it("stops at once under a named reason, and the process stays up", async () => {
    // The store applied the fixture's migration; the next build ships the
    // same migration id with different text.
    const path = join(scratch, "changed-migration.sqlite");
    const first = openSqliteFactStore({
      ...simStoreOptions([FIXTURE_PROJECTION], SIM_K, "sqlite"),
      path,
    });
    expect(await first.start()).toMatchObject({ kind: "ready" });
    await first.close();
    const changed: FollowerProjection = {
      ...FIXTURE_PROJECTION,
      migrations: (dialect) => {
        const set = fixtureMigrations(dialect);
        return {
          ...set,
          migrations: set.migrations.map((migration, index) =>
            index === 0
              ? {
                  ...migration,
                  sql: `${migration.sql}\n-- edited after it was applied\n`,
                }
              : migration,
          ),
        };
      },
    };
    const store = openSqliteFactStore({
      ...simStoreOptions([changed], SIM_K, "sqlite"),
      path,
    });
    const id = fixtureMigrations("sqlite").migrations[0]!.id;
    try {
      const { statuses } = await follow({
        store,
        script: script(events),
        // The loop returns by itself once it stops.
        until: () => false,
      });
      const stuck = statuses.find((status) => status.stuck !== null)!;
      expect(stuck.stuck).toMatchObject({ at: "migration", failures: 1 });
      expect(stuck.readiness[0]).toEqual({
        reason: FOLLOWER_MIGRATION_FAILED,
        detail: `the store refused its migrations: migration ${fixtureMigrations("sqlite").namespace}/${id} changed after it was applied`,
      });
      expect(stuck.state).toBe("intervention");
    } finally {
      await store.close();
    }
  });
});
