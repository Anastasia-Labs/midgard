/**
 * The verified confirmed ledger between full verification runs: a pass
 * records the frontier, the generation it read (bumped by every write to
 * `confirmed_ledger` and every frontier write, migration 0038) and a digest
 * of each landed delta it checked to the tail. A clean fold of the next
 * block, on the recorded generation, of the delta the run checked, then
 * advances the record to that block (root and the generation the fold
 * left), so a commit on the tail builds without waiting for the next run.
 * A fold on another generation, of another delta, or of a delta that does
 * not apply leaves the record, and commits wait for the next run. A run's
 * outcome below the recorded generation is ignored, and a record never
 * verifies a frontier of another generation, even at the same header and
 * root (an unfold and refold during a run).
 */
import { Effect, Either, Ref } from "effect";
import { describe, expect, it } from "vitest";

import type * as Ledger from "../src/database/utils/ledger.js";
import { landedDeltaDigest } from "../src/landed-blocks/delta-digest.js";
import type { LandedLedger } from "../src/landed-blocks/ledger.js";
import type { LandedBlockRow } from "../src/landed-blocks/store.js";
import { advanceVerifiedConfirmedLedger } from "../src/mpf/confirmed-ledger-advance.js";
import { deriveConfirmedLedgerRoots } from "../src/mpf/confirmed-ledger-roots.js";
import {
  CONFIRMED_LEDGER_VERIFICATION_SOURCE,
  type ConfirmedLedgerVerification,
  recordConfirmedLedgerVerification,
  verifyLandedLedger,
} from "../src/mpf/confirmed-ledger-verification.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/ledger-hydration.js";
import { Globals } from "../src/services/globals.js";
import { activeLivenessReasons } from "../src/services/liveness-halt.js";
import {
  CommitBaseUnverifiedError,
  verifiedLandedLedgerAt,
} from "../src/workers/commit-block-header.landed-commit-base.js";
import {
  parseVerifiedFrontier,
  verifiedFrontierWorkerInput,
} from "../src/workers/utils/commit-block-header.js";

/** A Midgard output holding `lovelace` (2^16 to 2^32, the CBOR width it is
 * encoded at). */
const output = (lovelace: number) =>
  Buffer.from(
    `a200583900899d9f267b0acf680fe51468ad46dca83b5e4d28e0dad6c68dbf0e2ea04755ad14d178a134575bba463b566827d339fdbdf0ec961814491c01821a${lovelace
      .toString(16)
      .padStart(8, "0")}a0`,
    "hex",
  );
const outRef = (label: number) =>
  Buffer.from(
    `825820${label.toString(16).padStart(2, "0").repeat(32)}19${label
      .toString(16)
      .padStart(4, "0")}`,
    "hex",
  );
const entry = (label: number, lovelace = 1_000_000 + label) => ({
  outref: outRef(label),
  output: output(lovelace),
});
const stored = (entries: readonly Ledger.MinimalEntry[]) =>
  entries.map((each) => ({ ...each, tx_id: Buffer.alloc(32), address: "a" }));
const rootOf = (entries: readonly Ledger.MinimalEntry[]) =>
  Effect.runPromise(computeLedgerMpfRootFromLedgerEntries([...entries]));

const F = "f0".repeat(28);
const H1 = "b1".repeat(28);
const H2 = "b2".repeat(28);
const A = entry(1);
const B = entry(2);
const C = entry(3);
const D = entry(4);

const row = (
  headerHash: string,
  parent: Readonly<{ headerHash: string; utxosRoot: string }>,
  utxosRoot: string,
  delta: Pick<LandedBlockRow, "spent" | "produced">,
): LandedBlockRow => ({
  headerHash,
  parentHeaderHash: parent.headerHash,
  parentUtxosRoot: parent.utxosRoot,
  utxosRoot,
  kind: "foreign",
  state: "processed",
  applied: true,
  depositIds: [],
  withdrawals: [],
  forcedIds: [],
  txIds: [],
  ...delta,
});

/** `confirmed_ledger` holds A and B at F (root r0); H1 spends A and
 * produces C (r1), H2 spends B and produces D (r2). */
const scenario = async () => {
  const r0 = await rootOf([A, B]);
  const r1 = await rootOf([B, C]);
  const r2 = await rootOf([C, D]);
  const f = { headerHash: F, utxosRoot: r0 };
  const h1 = row(H1, f, r1, { spent: [A.outref], produced: [C] });
  const h2 = row(H2, { headerHash: H1, utxosRoot: r1 }, r2, {
    spent: [B.outref],
    produced: [D],
  });
  /** The landed ledger at F, at `generation`. */
  const atF = (generation: string): LandedLedger => ({
    frontier: f,
    generation,
    confirmed: stored([A, B]),
    chain: [h1, h2],
  });
  /** The landed ledger once H1 is folded, at `generation`. */
  const atH1 = (generation: string): LandedLedger => ({
    frontier: { headerHash: H1, utxosRoot: r1 },
    generation,
    confirmed: stored([B, C]),
    chain: [h2],
  });
  /** The landed ledger once H2 is folded too, at `generation`. */
  const atH2 = (generation: string): LandedLedger => ({
    frontier: { headerHash: H2, utxosRoot: r2 },
    generation,
    confirmed: stored([C, D]),
    chain: [],
  });
  return { r0, r1, r2, h1, h2, atF, atH1, atH2 };
};

const node = () => Effect.runPromise(Effect.provide(Globals, Globals.Default));
type Node = Awaited<ReturnType<typeof node>>;

/** A full run's outcome on `landed`, its roots derived in this thread. */
const verify = (landed: LandedLedger) =>
  Effect.runPromise(verifyLandedLedger(landed, deriveConfirmedLedgerRoots));

const record = (globals: Node, outcome: ConfirmedLedgerVerification) =>
  Effect.runPromise(recordConfirmedLedgerVerification(globals, outcome));

const recorded = (globals: Node) =>
  Effect.runPromise(Ref.get(globals.VERIFIED_CONFIRMED_LEDGER));

/** The fold of `folded` committed, on the frontier generation `read`,
 * leaving `left`. */
const fold = (
  globals: Node,
  folded: LandedBlockRow,
  read: string,
  left: string,
) =>
  Effect.runPromise(
    advanceVerifiedConfirmedLedger(globals, {
      row: folded,
      readGeneration: read,
      generation: left,
    }),
  );

/** What a commit worker resolves at `headerHash` on `landed`, from what the
 * main thread recorded, posted and read back as the worker reads it. */
const commitAt = async (
  globals: Node,
  landed: LandedLedger,
  headerHash: string,
) => {
  const posted = verifiedFrontierWorkerInput(await recorded(globals));
  return Effect.runPromise(
    Effect.either(
      Effect.flatMap(parseVerifiedFrontier(posted.verifiedFrontier), (pair) =>
        verifiedLandedLedgerAt(landed, headerHash, pair),
      ),
    ),
  );
};

const expectBuilds = (
  outcome: Awaited<ReturnType<typeof commitAt>>,
  root: string,
) => {
  expect(Either.isRight(outcome)).toBe(true);
  expect(Either.getOrThrow(outcome)?.root).toBe(root);
};

const expectWaits = (outcome: Awaited<ReturnType<typeof commitAt>>) => {
  expect(Either.isLeft(outcome)).toBe(true);
  expect((outcome as Either.Left<unknown, unknown>).left).toBeInstanceOf(
    CommitBaseUnverifiedError,
  );
};

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
    // record to 12: its pass is ignored, and its divergence raises nothing
    // and leaves the record (it is only counted). The generations compare
    // as numbers (9 < 12), not as text.
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
