import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import { Journal } from "../src/devnet-stack/journal.js";
import {
  backBondPoolOnce,
  type BondPool,
  type BondStatus,
} from "../src/devnet-stack/stack.js";

const REQUIRED = 500_000_000n;

/** A pool on L1: a top-up adds its amount to the backing. */
const ledgerPool = (backing: bigint) => {
  const topUps: bigint[] = [];
  const pool: BondPool = {
    status: async (): Promise<BondStatus> => ({
      belowBond: backing < REQUIRED,
      backing: String(backing),
      requiredBacking: String(REQUIRED),
    }),
    topUp: async (lovelace) => {
      topUps.push(lovelace);
      backing += lovelace;
    },
  };
  return {
    pool,
    topUps,
    slash: (lovelace: bigint) => {
      backing -= lovelace;
    },
  };
};

const dirs: string[] = [];
const journal = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-bond-pool-"));
  dirs.push(dir);
  return join(dir, "journal.json");
};

afterEach(() => {
  for (const dir of dirs.splice(0)) rmSync(dir, { recursive: true });
});

describe("backBondPoolOnce", () => {
  it("backs an empty pool by its shortfall on the first deploy", async () => {
    const chain = ledgerPool(0n);
    const status = await backBondPoolOnce(chain.pool, new Journal(journal()));
    expect(chain.topUps).toEqual([REQUIRED]);
    expect(status.belowBond).toBe(false);
  });

  it("never tops up a pool that was slashed after its first backing", async () => {
    const path = journal();
    const chain = ledgerPool(0n);
    await backBondPoolOnce(chain.pool, new Journal(path));
    chain.slash(REQUIRED);
    // A later start, from the journal on disk.
    const status = await backBondPoolOnce(chain.pool, new Journal(path));
    expect(chain.topUps).toEqual([REQUIRED]);
    expect(status).toEqual({
      belowBond: true,
      backing: "0",
      requiredBacking: String(REQUIRED),
    });
  });

  it("records a pool that was already backed without paying", async () => {
    const path = journal();
    const chain = ledgerPool(REQUIRED);
    await backBondPoolOnce(chain.pool, new Journal(path));
    chain.slash(1n);
    await backBondPoolOnce(chain.pool, new Journal(path));
    expect(chain.topUps).toEqual([]);
  });

  it("refuses when the first backing leaves the pool short", async () => {
    const chain = ledgerPool(0n);
    const pool: BondPool = { ...chain.pool, topUp: async () => {} };
    await expect(
      backBondPoolOnce(pool, new Journal(journal())),
    ).rejects.toThrow(/still below its bond after its first backing/);
  });
});
