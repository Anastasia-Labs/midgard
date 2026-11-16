/**
 * The landed ledger a commit on a queue tail builds on, from the frontier
 * the main thread verified `confirmed_ledger` at: the deltas after it are
 * applied, each checked to build on the root before it and to apply
 * cleanly. A delta that spends an output the ledger before it does not
 * hold, produces one it already holds, names an outref twice or as both
 * spent and produced, or changes the root exactly when it is empty, is a
 * `commit_base_landed` divergence; a frontier that is not the verified one
 * waits for the verification.
 */
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import type { LandedLedger } from "../src/landed-blocks/ledger.js";
import type { LandedBlockRow } from "../src/landed-blocks/store.js";
import { LocalLedgerDivergenceError } from "../src/mpf/local-ledger-divergence.js";
import {
  CommitBaseUnverifiedError,
  verifiedLandedLedgerAt,
} from "../src/workers/commit-block-header.landed-commit-base.js";

const outRef = (label: number) => Buffer.alloc(36, label);
const entry = (label: number, output = `output-${label.toString()}`) => ({
  outref: outRef(label),
  output: Buffer.from(output),
});

const F = { headerHash: "f0".repeat(28), utxosRoot: "a0".repeat(32) };
/** The frontier generation the ledger is read at, and F verified at it. */
const GENERATION = "7";
const VERIFIED_F = { ...F, generation: GENERATION };
const H1 = "b1".repeat(28);
const H2 = "b2".repeat(28);

const row = (
  headerHash: string,
  parentHeaderHash: string,
  parentUtxosRoot: string,
  utxosRoot: string,
  delta: Pick<LandedBlockRow, "spent" | "produced">,
): LandedBlockRow => ({
  headerHash,
  parentHeaderHash,
  parentUtxosRoot,
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

/** `confirmed_ledger` holds outputs 1 and 2 at F; H1 spends 1 and produces
 * 3, H2 spends 3 and produces 4. */
const landed = (
  overrides: {
    h1?: Partial<LandedBlockRow>;
    h2?: Partial<LandedBlockRow>;
  } = {},
): LandedLedger => ({
  frontier: F,
  generation: GENERATION,
  confirmed: [entry(1), entry(2)].map((each) => ({
    ...each,
    tx_id: Buffer.alloc(32),
    address: "addr",
  })),
  chain: [
    {
      ...row(H1, F.headerHash, F.utxosRoot, "a1".repeat(32), {
        spent: [outRef(1)],
        produced: [entry(3)],
      }),
      ...overrides.h1,
    },
    {
      ...row(H2, H1, "a1".repeat(32), "a2".repeat(32), {
        spent: [outRef(3)],
        produced: [entry(4)],
      }),
      ...overrides.h2,
    },
  ],
});

/** The base at `headerHash` on `ledger`, from the frontier `verified`
 * (none when undefined); F at the ledger's generation unless given. */
const at = (
  ledger: LandedLedger,
  headerHash: string,
  ...verified:
    | []
    | [
        | { headerHash: string; utxosRoot: string; generation: string }
        | undefined,
      ]
) =>
  Effect.runPromise(
    Effect.either(
      verifiedLandedLedgerAt(
        ledger,
        headerHash,
        verified.length === 0 ? VERIFIED_F : verified[0],
      ),
    ),
  );

const keys = (outcome: Awaited<ReturnType<typeof at>>) =>
  [...(Either.getOrThrow(outcome)?.ledger.keys() ?? [])].sort();

const divergence = (outcome: Awaited<ReturnType<typeof at>>) => {
  expect(Either.isLeft(outcome)).toBe(true);
  const error = (outcome as Either.Left<unknown, unknown>).left;
  expect(error).toBeInstanceOf(LocalLedgerDivergenceError);
  return error as LocalLedgerDivergenceError;
};

describe("a landed commit base from the verified frontier", () => {
  it("applies each delta after the frontier, ending at the tail's root", async () => {
    const outcome = await at(landed(), H2);
    expect(Either.getOrThrow(outcome)?.root).toBe("a2".repeat(32));
    expect(keys(outcome)).toEqual(
      [outRef(2), outRef(4)].map((each) => each.toString("hex")).sort(),
    );
    expect(keys(await at(landed(), F.headerHash))).toEqual(
      [outRef(1), outRef(2)].map((each) => each.toString("hex")).sort(),
    );
  });

  it("is off the chain for a header no processed row names", async () => {
    expect(Either.getOrThrow(await at(landed(), "c3".repeat(28)))).toBe(
      undefined,
    );
  });

  it("refuses a delta that spends an output the ledger before it does not hold", async () => {
    const error = divergence(
      await at(landed({ h2: { spent: [outRef(3), outRef(1)] } }), H2),
    );
    expect(error).toMatchObject({
      check: "commit_base_landed",
      headerHash: H2,
      root: "a2".repeat(32),
    });
    expect(error.fault).toContain(
      `spends ${outRef(1).toString("hex")}, which the ledger before it does not hold`,
    );
  });

  it("refuses a delta that produces an output the ledger before it already holds", async () => {
    const error = divergence(
      await at(landed({ h1: { produced: [entry(3), entry(2)] } }), H2),
    );
    expect(error).toMatchObject({
      check: "commit_base_landed",
      headerHash: H1,
      root: "a1".repeat(32),
    });
    expect(error.fault).toContain(
      `produces ${outRef(2).toString("hex")}, which the ledger before it already holds`,
    );
  });

  it("refuses an empty delta whose root is not the root before it", async () => {
    const error = divergence(
      await at(landed({ h1: { spent: [], produced: [] } }), H2),
    );
    expect(error).toMatchObject({
      check: "commit_base_landed",
      headerHash: H1,
      root: "a1".repeat(32),
    });
    expect(error.fault).toContain(
      `is empty, its root ${"a1".repeat(32)} is not the root ${F.utxosRoot} before it`,
    );
  });

  it("refuses a delta that is not empty and keeps the root before it", async () => {
    const error = divergence(
      await at(landed({ h1: { utxosRoot: F.utxosRoot } }), H1),
    );
    expect(error).toMatchObject({
      check: "commit_base_landed",
      headerHash: H1,
    });
    expect(error.fault).toContain(
      `is not empty, its root is the root ${F.utxosRoot} before it`,
    );
  });

  it("applies an empty delta that keeps the root before it", async () => {
    const outcome = await at(
      landed({ h1: { spent: [], produced: [], utxosRoot: F.utxosRoot } }),
      H1,
    );
    expect(Either.getOrThrow(outcome)?.root).toBe(F.utxosRoot);
    expect(keys(outcome)).toEqual(
      [outRef(1), outRef(2)].map((each) => each.toString("hex")).sort(),
    );
  });

  it("refuses a delta that produces one outref twice", async () => {
    const error = divergence(
      await at(
        landed({ h1: { produced: [entry(3, "x"), entry(3, "y")] } }),
        H2,
      ),
    );
    expect(error).toMatchObject({
      check: "commit_base_landed",
      headerHash: H1,
    });
    expect(error.fault).toContain(
      `produces ${outRef(3).toString("hex")} twice`,
    );
  });

  it("refuses a delta that spends one outref twice", async () => {
    const error = divergence(
      await at(landed({ h1: { spent: [outRef(1), outRef(1)] } }), H2),
    );
    expect(error).toMatchObject({
      check: "commit_base_landed",
      headerHash: H1,
    });
    expect(error.fault).toContain(`spends ${outRef(1).toString("hex")} twice`);
  });

  it("refuses a delta that spends an outref and produces it again", async () => {
    const error = divergence(
      await at(
        landed({ h1: { spent: [outRef(2)], produced: [entry(2, "changed")] } }),
        H2,
      ),
    );
    expect(error).toMatchObject({
      check: "commit_base_landed",
      headerHash: H1,
    });
    expect(error.fault).toContain(
      `spends and produces ${outRef(2).toString("hex")}`,
    );
  });

  it("refuses a row that does not build on the root before it", async () => {
    const error = divergence(
      await at(landed({ h2: { parentUtxosRoot: "ee".repeat(32) } }), H2),
    );
    expect(error).toMatchObject({
      check: "commit_base_landed",
      headerHash: H2,
    });
    expect(error.fault).toContain(
      `builds on root ${"ee".repeat(32)}, the root before it is ${"a1".repeat(32)}`,
    );
  });
});

describe("a landed commit base before its frontier is verified", () => {
  it("waits while no verification has passed, or the last was at another frontier or generation", async () => {
    for (const verified of [
      undefined,
      { ...VERIFIED_F, headerHash: "e0".repeat(28) },
      { ...VERIFIED_F, utxosRoot: "e1".repeat(32) },
      // The same frontier and root, read before `confirmed_ledger` was
      // written again.
      { ...VERIFIED_F, generation: "6" },
    ]) {
      const outcome = await at(landed(), H2, verified);
      expect(Either.isLeft(outcome)).toBe(true);
      const error = (outcome as Either.Left<unknown, unknown>).left;
      expect(error).toBeInstanceOf(CommitBaseUnverifiedError);
      expect(error).toMatchObject({ headerHash: H2 });
    }
  });
});
