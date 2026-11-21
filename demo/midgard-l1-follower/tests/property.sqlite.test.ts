import { describe, expect, it } from "vitest";

import { openSqliteFactStore } from "../src/index.js";
import { matching } from "./support/matchers.js";
import { runProperty } from "./support/property.js";

const OPS = Number(process.env.L1_FOLLOWER_PROPERTY_OPS ?? "10000");

describe("fact-store property test (SQLite adapter)", () => {
  it(`equals a fresh replay after each of ${OPS} random apply/rollback steps`, async () => {
    const outcome = await runProperty({
      open: async (optionsFor) =>
        openSqliteFactStore({ ...optionsFor("sqlite"), path: ":memory:" }),
      seed: 0x5eed_0001,
      ops: OPS,
      k: 8,
    });
    console.info("sqlite property", JSON.stringify(outcome));
    expect(outcome).toMatchObject({ ok: true });
  });
});

describe("the property test catches a deliberately broken rewind (SQLite adapter)", () => {
  const broken = async (
    fault:
      | "skip_temporal_truncation"
      | "temporal_cut_below_target"
      | "skip_unspend_cache_patch",
  ) =>
    runProperty({
      open: async (optionsFor) =>
        openSqliteFactStore({ ...optionsFor("sqlite"), path: ":memory:" }),
      seed: 0x5eed_0004,
      ops: 2_000,
      k: 8,
      fault,
    });

  it("D-t rows left above the target: the post-rewind check returns R5", async () => {
    expect(await broken("skip_temporal_truncation")).toMatchObject({
      ok: false,
      reason: matching(/store_integrity.*REGISTRY/u),
    });
  });

  it("D-t cut one slot too low: the state differs from a fresh replay", async () => {
    expect(await broken("temporal_cut_below_target")).toMatchObject({
      ok: false,
      reason: matching(/differs from a fresh replay: fixture_/u),
    });
  });

  it("un-spent outrefs missing from the cache: the cache differs from a fresh load", async () => {
    expect(await broken("skip_unspend_cache_patch")).toMatchObject({
      ok: false,
      reason: matching(/tracked-outref cache/u),
    });
  });
});

describe("fact-store property test with interleaved pruning (SQLite adapter)", () => {
  it("matches a fresh replay inside the retained window and keeps INV1-INV6", async () => {
    const outcome = await runProperty({
      open: async (optionsFor) =>
        openSqliteFactStore({ ...optionsFor("sqlite"), path: ":memory:" }),
      seed: 0x5eed_0005,
      ops: 3_000,
      k: 8,
      pruneEvery: 3,
      pruneBudget: 7,
      rollbackProbability: 0.15,
    });
    console.info("sqlite prune property", JSON.stringify(outcome));
    expect(outcome).toMatchObject({ ok: true });
  });
});
