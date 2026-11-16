/**
 * The verified confirmed ledger between full verification runs: a pass
 * records the frontier, the generation it read (bumped by every write to
 * `confirmed_ledger` and every frontier write, migration 0038) and a digest
 * of each landed delta it checked to the tail. A fold of the next block, on
 * the record's generation, of the delta the run checked, that applies,
 * advances it to that block (root and the generation the fold left), so a
 * commit on the tail builds without waiting for the next run. A fold on
 * another generation, of another delta, of a delta that does not apply, or
 * of a block that is not the record's child does not, and commits wait for
 * the next run. A pass that stays below the recorded generation is ignored,
 * a divergence below it is never replayed, and a record never verifies a
 * frontier of another generation, even at the same header and root (an
 * unfold and refold during a run). Folds made while a run reads, and the
 * fold log's cap, are in `confirmed-ledger-advance.replay.test.ts`.
 */
import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { landedDeltaDigest } from "../src/landed-blocks/delta-digest.js";
import { CONFIRMED_LEDGER_VERIFICATION_SOURCE } from "../src/mpf/confirmed-ledger-verification.js";
import { activeLivenessReasons } from "../src/services/liveness-halt.js";
import {
  A,
  B,
  C,
  commitAt,
  expectBuilds,
  expectWaits,
  F,
  fold,
  H1,
  H2,
  node,
  output,
  record,
  recorded,
  scenario,
  stored,
  verify,
} from "./confirmed-ledger-advance.fixture.js";

describe("the verified confirmed ledger advances on a clean fold", () => {
  it("advances to the folded block after a clean fold, so a commit on the tail builds without waiting", async () => {
    const s = await scenario();
    const globals = await node();
    const pass = await verify(s.atF("9"));
    expect(pass.kind).toBe("verified");
    await record(globals, pass);
    expectBuilds(await commitAt(globals, s.atF("9"), H2), s.r2);

    // Before the fold advances it, the folded frontier is not verified.
    expectWaits(await commitAt(globals, s.atH1("12"), H2));

    await fold(globals, s.h1, "9", "12");
    expect(await recorded(globals)).toMatchObject({
      headerHash: H1,
      utxosRoot: s.r1,
      generation: "12",
    });
    expectBuilds(await commitAt(globals, s.atH1("12"), H2), s.r2);

    // Inductively: the next clean fold advances again, on the delta the
    // same run checked.
    await fold(globals, s.h2, "12", "15");
    expectBuilds(await commitAt(globals, s.atH2("15"), H2), s.r2);
  }, 60_000);

  it("does not advance on a delta whose digest differs from the one the last run verified", async () => {
    const s = await scenario();
    const globals = await node();
    await record(globals, await verify(s.atF("9")));
    // The stored delta changed after the run: it produces another output at
    // C's outref.
    const changed = {
      ...s.h1,
      produced: [{ ...C, output: output(7_000_000) }],
    };
    await fold(globals, changed, "9", "12");
    expect(await recorded(globals)).toMatchObject({
      headerHash: F,
      generation: "9",
    });
    expectWaits(await commitAt(globals, s.atH1("12"), H2));
  }, 60_000);

  it("does not advance on a fold that read another generation than the recorded one", async () => {
    const s = await scenario();
    const globals = await node();
    await record(globals, await verify(s.atF("9")));
    // `confirmed_ledger` was written between the run and the fold.
    await fold(globals, s.h1, "10", "12");
    expect(await recorded(globals)).toMatchObject({
      headerHash: F,
      generation: "9",
    });
    expectWaits(await commitAt(globals, s.atH1("12"), H2));
  }, 60_000);

  it("does not advance on a delta that does not apply, whatever digest the record holds", async () => {
    const s = await scenario();
    const globals = await node();
    // Spends and produces the same outref; and an empty delta that changes
    // the root.
    for (const bad of [
      { ...s.h1, produced: [C, { ...A, output: output(8_000_000) }] },
      { ...s.h1, spent: [], produced: [] },
    ]) {
      await Effect.runPromise(
        Ref.set(globals.VERIFIED_CONFIRMED_LEDGER, {
          headerHash: F,
          utxosRoot: s.r0,
          generation: "9",
          tailDigests: new Map([[H1, landedDeltaDigest(bad)]]),
        }),
      );
      await fold(globals, bad, "9", "12");
      expect(await recorded(globals)).toMatchObject({
        headerHash: F,
        generation: "9",
      });
    }
  }, 60_000);

  it("does not advance a fold of a block that is not the recorded frontier's child", async () => {
    const s = await scenario();
    const globals = await node();
    await record(globals, await verify(s.atF("9")));
    await fold(globals, s.h2, "9", "12");
    expect(await recorded(globals)).toMatchObject({
      headerHash: F,
      generation: "9",
    });
  }, 60_000);
});

describe("the recorded generation", () => {
  it("leaves the record and readiness on a run's outcome below the recorded generation, whatever it found", async () => {
    const s = await scenario();
    const globals = await node();
    await record(globals, await verify(s.atF("9")));
    await fold(globals, s.h1, "9", "12");

    // A run that read generation 9 finishes after the fold advanced the
    // record to 12: its pass is replayed over that fold to 12, and its
    // divergence is never replayed, raises nothing and leaves the record
    // (it is only counted). The generations compare as numbers (9 < 12),
    // not as text.
    await record(globals, await verify(s.atF("9")));
    expect(await recorded(globals)).toMatchObject({
      headerHash: H1,
      generation: "12",
    });
    const diverged = await verify({
      ...s.atF("9"),
      confirmed: stored([A, { ...B, output: output(9_000_000) }]),
    });
    expect(diverged.kind).toBe("diverged");
    await record(globals, diverged);
    expect(await recorded(globals)).toMatchObject({
      headerHash: H1,
      generation: "12",
    });
    expect(
      (await Effect.runPromise(activeLivenessReasons(globals))).filter(
        (each) => each.source === CONFIRMED_LEDGER_VERIFICATION_SOURCE,
      ),
    ).toEqual([]);
    expectBuilds(await commitAt(globals, s.atH1("12"), H2), s.r2);

    // One at or above it is recorded.
    const later = await verify({ ...s.atH1("12"), generation: "100" });
    await record(globals, later);
    expect(await recorded(globals)).toMatchObject({
      headerHash: H1,
      generation: "100",
    });
  }, 60_000);

  it("never verifies a frontier unfolded and refolded during a run: the run's pass is stale", async () => {
    const s = await scenario();
    const globals = await node();
    // A run reads H1 at generation 12. While it runs H1 is unfolded (F at
    // 13) and folded again (H1 at 16): the same header and root, another
    // generation. Its pass, recorded after, does not verify H1 at 16.
    const stale = await verify(s.atH1("12"));
    expect(stale.kind).toBe("verified");
    await record(globals, stale);
    expectWaits(await commitAt(globals, s.atH1("16"), H2));

    // A run at 16 recorded first: the stale pass is ignored, and the commit
    // builds.
    const fresh = await node();
    await record(fresh, await verify(s.atH1("16")));
    await record(fresh, stale);
    expect(await recorded(fresh)).toMatchObject({ generation: "16" });
    expectBuilds(await commitAt(fresh, s.atH1("16"), H2), s.r2);
  }, 60_000);
});
