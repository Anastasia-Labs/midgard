import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterAll, describe, expect, it } from "vitest";

import { createWalletSeeder } from "../src/index.js";
import { testDatabases } from "./support/postgres.js";
import {
  chain,
  point,
  started,
  storeAdapters,
  TRACKED,
} from "./support/small-chain.js";
import { fakeLedger, preOrigin } from "./support/wallet-seed-ledger.js";

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-seeder-status-"));

afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

// The status is the seeder's, not the store's: one store kind is enough.
const sqlite = storeAdapters(databases, scratch).filter(
  ({ name }) => name === "sqlite",
);

describe.each(sqlite)("wallet seeder status ($name)", (adapter) => {
  it("tells a seed read in flight, with the point it reads at, from a seed never started", async () => {
    const { store } = await started(adapter, 4, 2);
    try {
      let reading!: () => void;
      const readStarted = new Promise<void>((resolve) => {
        reading = resolve;
      });
      let answer!: () => void;
      const answered = new Promise<void>((resolve) => {
        answer = resolve;
      });
      const ledger = fakeLedger({
        utxos: () => [preOrigin(0xd1, TRACKED)],
        acquirable: () => true,
        duringQuery: async () => {
          reading();
          await answered;
        },
      });
      const seeder = createWalletSeeder({ store, ledger, wallets: [TRACKED] });
      try {
        expect(seeder.status()).toMatchObject({
          kind: "pending",
          reason: "not_initialized",
        });
        const step = seeder.step();
        await readStarted;
        const cursor = point(chain()[1]);
        const during = seeder.status();
        expect(during).toMatchObject({
          kind: "pending",
          reason: "in_flight",
          inFlight: { at: cursor },
        });
        expect(during.kind === "pending" ? during.detail : "").toContain(
          `slot ${cursor.slot}`,
        );
        answer();
        expect(await step).toEqual({ kind: "ready" });
        expect(seeder.status()).toEqual({ kind: "ready" });
      } finally {
        answer();
        seeder.close();
      }
    } finally {
      await store.close();
    }
  });
});
