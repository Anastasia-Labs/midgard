/**
 * The ledger, landed rows and node of the verified confirmed ledger's
 * tests (`confirmed-ledger-advance.test.ts`,
 * `confirmed-ledger-advance.replay.test.ts`): a small ledger and chain
 * after its frontier, a node's record and fold log, and what a commit
 * worker resolves from the record.
 */
import { Effect, Either, Ref } from "effect";
import { expect } from "vitest";

import type * as Ledger from "../src/database/utils/ledger.js";
import type { LandedLedger } from "../src/landed-blocks/ledger.js";
import type { LandedBlockRow } from "../src/landed-blocks/store.js";
import { advanceVerifiedConfirmedLedger } from "../src/mpf/confirmed-ledger-advance.js";
import { deriveConfirmedLedgerRoots } from "../src/mpf/confirmed-ledger-roots.js";
import {
  type ConfirmedLedgerVerification,
  verifyLandedLedger,
} from "../src/mpf/confirmed-ledger-run.js";
import { recordConfirmedLedgerVerification } from "../src/mpf/confirmed-ledger-verification.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/ledger-hydration.js";
import { Globals } from "../src/services/globals.globals.js";
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
export const output = (lovelace: number) =>
  Buffer.from(
    `a200583900899d9f267b0acf680fe51468ad46dca83b5e4d28e0dad6c68dbf0e2ea04755ad14d178a134575bba463b566827d339fdbdf0ec961814491c01821a${lovelace
      .toString(16)
      .padStart(8, "0")}a0`,
    "hex",
  );
export const outRef = (label: number) =>
  Buffer.from(
    `825820${label.toString(16).padStart(2, "0").repeat(32)}19${label
      .toString(16)
      .padStart(4, "0")}`,
    "hex",
  );
export const entry = (label: number, lovelace = 1_000_000 + label) => ({
  outref: outRef(label),
  output: output(lovelace),
});
export const stored = (entries: readonly Ledger.MinimalEntry[]) =>
  entries.map((each) => ({ ...each, tx_id: Buffer.alloc(32), address: "a" }));
export const rootOf = (entries: readonly Ledger.MinimalEntry[]) =>
  Effect.runPromise(computeLedgerMpfRootFromLedgerEntries([...entries]));

export const F = "f0".repeat(28);
export const H1 = "b1".repeat(28);
export const H2 = "b2".repeat(28);
export const A = entry(1);
export const B = entry(2);
export const C = entry(3);
export const D = entry(4);

export const row = (
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
export const scenario = async () => {
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

export const node = () =>
  Effect.runPromise(Effect.provide(Globals, Globals.Default));
export type Node = Awaited<ReturnType<typeof node>>;

/** A full run's outcome on `landed`, its roots derived in this thread. */
export const verify = (landed: LandedLedger) =>
  Effect.runPromise(verifyLandedLedger(landed, deriveConfirmedLedgerRoots));

export const record = (globals: Node, outcome: ConfirmedLedgerVerification) =>
  Effect.runPromise(recordConfirmedLedgerVerification(globals, outcome));

export const recorded = (globals: Node) =>
  Effect.runPromise(Ref.get(globals.VERIFIED_CONFIRMED_LEDGER));

/** The fold of `folded` committed, on the frontier generation `read`,
 * leaving `left`. */
export const fold = (
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
export const commitAt = async (
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

export const expectBuilds = (
  outcome: Awaited<ReturnType<typeof commitAt>>,
  root: string,
) => {
  expect(Either.isRight(outcome)).toBe(true);
  expect(Either.getOrThrow(outcome)?.root).toBe(root);
};

export const expectWaits = (outcome: Awaited<ReturnType<typeof commitAt>>) => {
  expect(Either.isLeft(outcome)).toBe(true);
  expect((outcome as Either.Left<unknown, unknown>).left).toBeInstanceOf(
    CommitBaseUnverifiedError,
  );
};
