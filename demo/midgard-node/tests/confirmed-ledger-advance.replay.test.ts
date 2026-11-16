/**
 * Every committed fold is logged, and the verified record is replayed over
 * the log when a pass is recorded: a fold made while a run read moves that
 * run's pass on, up to and past a record below it, and the replay stops at
 * a fold of another delta, of a delta that does not apply, or of a block
 * that is not the record's child. A fold past the log's cap drops the log
 * and makes a full run due, which catches the record up so a commit on the
 * folded frontier builds; a current record keeps advancing past the cap.
 */
import { Effect, Fiber } from "effect";
import { describe, expect, it } from "vitest";

import { landedDeltaDigest } from "../src/landed-blocks/delta-digest.js";
import type { LandedLedger } from "../src/landed-blocks/ledger.js";
import {
  CONFIRMED_LEDGER_FOLD_LOG_CAP,
  confirmedLedgerRunDue,
} from "../src/mpf/confirmed-ledger-advance.js";
import type { ConfirmedLedgerVerification } from "../src/mpf/confirmed-ledger-run.js";
import { confirmedLedgerVerificationLoop } from "../src/mpf/confirmed-ledger-verification.js";
import { Globals } from "../src/services/globals.js";
import {
  A,
  C,
  commitAt,
  D,
  expectBuilds,
  expectWaits,
  F,
  fold,
  H1,
  H2,
  node,
  output,
  outRef,
  record,
  recorded,
  row,
  scenario,
  verify,
} from "./confirmed-ledger-advance.fixture.js";

describe("a fold committed while a run reads", () => {
  it("moves the run's pass on when it is recorded: the record ends at the folded block", async () => {
    const s = await scenario();
    const globals = await node();
    // The run reads generation 9; H1's fold (9 to 12) commits before it
    // records, with no record yet to advance.
    const pass = await verify(s.atF("9"));
    await fold(globals, s.h1, "9", "12");
    expect(await recorded(globals)).toBe(undefined);
    await record(globals, pass);
    expect(await recorded(globals)).toMatchObject({
      headerHash: H1,
      utxosRoot: s.r1,
      generation: "12",
    });
    expectBuilds(await commitAt(globals, s.atH1("12"), H2), s.r2);

    // And the next fold advances it, on the digests the pass carried.
    await fold(globals, s.h2, "12", "15");
    expectBuilds(await commitAt(globals, s.atH2("15"), H2), s.r2);
  }, 60_000);

  it("moves a pass below the record up to it, replacing the record and its digests", async () => {
    const s = await scenario();
    const globals = await node();
    // A first run read F at 9 before H2 landed: it checked H1's delta only.
    await record(globals, await verify({ ...s.atF("9"), chain: [s.h1] }));
    await fold(globals, s.h1, "9", "12");
    expect(await recorded(globals)).toMatchObject({ generation: "12" });
    // The next run read F at 9 too, with H2 landed: its pass replays over
    // H1's fold to 12, the record's generation, and replaces the record.
    await record(globals, await verify(s.atF("9")));
    expect(await recorded(globals)).toMatchObject({
      headerHash: H1,
      generation: "12",
    });
    expect((await recorded(globals))?.tailDigests.get(H2)).toBe(
      landedDeltaDigest(s.h2),
    );
    // So H2's fold advances it, which the first run's digests could not.
    await fold(globals, s.h2, "12", "15");
    expect(await recorded(globals)).toMatchObject({
      headerHash: H2,
      generation: "15",
    });
  }, 60_000);

  it("stops the replay at a fold of another delta, of a delta that does not apply, or of a block that is not the record's child", async () => {
    const s = await scenario();
    for (const during of [
      // Another delta than the run checked.
      [{ ...s.h1, produced: [{ ...C, output: output(7_000_000) }] }],
      // A delta that does not apply: it spends and produces one outref.
      [{ ...s.h1, produced: [C, { ...A, output: output(8_000_000) }] }],
      // A block that does not build on F.
      [s.h2],
    ]) {
      const globals = await node();
      const pass = await verify(s.atF("9"));
      for (const folded of during) await fold(globals, folded, "9", "12");
      await record(globals, pass);
      expect(await recorded(globals)).toMatchObject({
        headerHash: F,
        generation: "9",
      });
      expectWaits(await commitAt(globals, s.atH1("12"), H2));
    }
    // A clean fold before a bad one: the replay stops after the clean one.
    const globals = await node();
    const pass = await verify(s.atF("9"));
    await fold(globals, s.h1, "9", "12");
    await fold(
      globals,
      { ...s.h2, produced: [{ ...D, output: output(6_000_000) }] },
      "12",
      "15",
    );
    await record(globals, pass);
    expect(await recorded(globals)).toMatchObject({
      headerHash: H1,
      generation: "12",
    });
  }, 60_000);
});

describe("the fold log past its cap", () => {
  /** A chain of `length` blocks after block 0, each spending one outref;
   * block k at generation k + 1. Roots are labels: nothing here derives
   * one. */
  const longChain = (length: number) => {
    const header = (k: number) => k.toString(16).padStart(56, "0");
    const root = (k: number) => k.toString(16).padStart(64, "0");
    const rows = Array.from({ length }, (_, index) => {
      const k = index + 1;
      return row(
        header(k),
        { headerHash: header(k - 1), utxosRoot: root(k - 1) },
        root(k),
        { spent: [outRef(k % 250)], produced: [] },
      );
    });
    /** What a run that read block `k` passes, with every block's digest. */
    const passAt = (k: number): ConfirmedLedgerVerification => ({
      kind: "verified",
      frontier: { headerHash: header(k), utxosRoot: root(k) },
      generation: (k + 1).toString(),
      tail: { headerHash: header(length), utxosRoot: root(length) },
      tailDigests: new Map(
        rows.map((each) => [each.headerHash, landedDeltaDigest(each)]),
      ),
      entries: 0,
      durationMs: 0,
    });
    /** The landed ledger with every block folded. */
    const folded: LandedLedger = {
      frontier: { headerHash: header(length), utxosRoot: root(length) },
      generation: (length + 1).toString(),
      confirmed: [],
      chain: [],
    };
    return { rows, passAt, folded, root };
  };
  const PAST = CONFIRMED_LEDGER_FOLD_LOG_CAP + 44;

  it("drops the log, so a pass that needed a dropped fold stops; the run it makes due catches the record up and the commit builds", async () => {
    const chain = longChain(PAST);
    const globals = await node();
    // A run reads block 0; every block folds before it records.
    const pass = chain.passAt(0);
    for (const [index, each] of chain.rows.entries())
      await fold(globals, each, (index + 1).toString(), (index + 2).toString());
    expect(await Effect.runPromise(confirmedLedgerRunDue(globals))).toBe(true);
    await record(globals, pass);
    expect(await recorded(globals)).toMatchObject({ generation: "1" });
    expectWaits(
      await commitAt(globals, chain.folded, chain.folded.frontier.headerHash),
    );

    // The fiber takes the due run, which reads the folded frontier.
    const loop = Effect.runFork(
      Effect.provideService(
        confirmedLedgerVerificationLoop({
          verify: Effect.succeed(chain.passAt(PAST)),
          frontier: Effect.succeed({
            ...chain.folded.frontier,
            generation: chain.folded.generation,
          }),
          watchIntervalMs: 10,
        }),
        Globals,
        globals,
      ),
    );
    try {
      await expect
        .poll(async () => (await recorded(globals))?.generation, {
          timeout: 10_000,
        })
        .toBe((PAST + 1).toString());
    } finally {
      await Effect.runPromise(Fiber.interrupt(loop));
    }
    expect(await Effect.runPromise(confirmedLedgerRunDue(globals))).toBe(false);
    expectBuilds(
      await commitAt(globals, chain.folded, chain.folded.frontier.headerHash),
      chain.root(PAST),
    );
  }, 60_000);

  it("keeps advancing a current record past the cap: the fold that overflows starts the new log", async () => {
    const chain = longChain(PAST);
    const globals = await node();
    await record(globals, chain.passAt(0));
    for (const [index, each] of chain.rows.entries())
      await fold(globals, each, (index + 1).toString(), (index + 2).toString());
    expect(await recorded(globals)).toMatchObject({
      generation: (PAST + 1).toString(),
    });
    expectBuilds(
      await commitAt(globals, chain.folded, chain.folded.frontier.headerHash),
      chain.root(PAST),
    );
  }, 60_000);
});
