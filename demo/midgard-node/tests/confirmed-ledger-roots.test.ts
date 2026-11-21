/**
 * The verification of the landed ledger checks two roots from one trie
 * built in its worker thread: `confirmed_ledger`'s against the frontier's
 * stored root (`confirmed_ledger`), then, with the processed landed deltas
 * applied in order, the tail's against the tail row's stored root
 * (`confirmed_ledger_tail`). Each mismatch is reported on its own, so a
 * corrupted `confirmed_ledger` entry that a tail row spends, absent from
 * the tail's ledger, is still caught at the frontier, and a corrupted
 * landed delta is caught at the tail.
 */
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import type * as Ledger from "../src/database/utils/ledger.js";
import type { LandedLedger } from "../src/landed-blocks/ledger.js";
import type { LandedBlockRow } from "../src/landed-blocks/store.js";
import { verifyLandedLedger } from "../src/mpf/confirmed-ledger-run.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/ledger-hydration.js";

/** A Midgard output holding `lovelace` (below 2^32). */
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
  entries.map((each) => ({
    ...each,
    tx_id: Buffer.alloc(32),
    address: "addr",
  }));
const rootOf = (entries: readonly Ledger.MinimalEntry[]) =>
  Effect.runPromise(computeLedgerMpfRootFromLedgerEntries([...entries]));

const F = "f0".repeat(28);
const H1 = "b1".repeat(28);

const A = entry(1);
const B = entry(2);
const C = entry(3);

/** At F `confirmed_ledger` should hold A and B; H1 spends A and produces
 * C. `confirmed` and `produced` replace what is stored. */
const landed = async ({
  confirmed = [A, B],
  produced = [C],
}: {
  confirmed?: readonly Ledger.MinimalEntry[];
  produced?: readonly Ledger.MinimalEntry[];
} = {}): Promise<LandedLedger> => {
  const r0 = await rootOf([A, B]);
  const r1 = await rootOf([B, C]);
  const row: LandedBlockRow = {
    headerHash: H1,
    parentHeaderHash: F,
    parentUtxosRoot: r0,
    utxosRoot: r1,
    kind: "foreign",
    state: "processed",
    applied: true,
    spent: [A.outref],
    produced: [...produced],
    depositIds: [],
    withdrawals: [],
    forcedIds: [],
    txIds: [],
  };
  return {
    frontier: { headerHash: F, utxosRoot: r0 },
    generation: "1",
    confirmed: stored(confirmed),
    chain: [row],
  };
};

const verify = (ledger: LandedLedger) =>
  Effect.runPromise(verifyLandedLedger(ledger));

describe("the landed ledger's two roots", () => {
  it("verifies a ledger whose frontier and tail both derive their stored roots", async () => {
    const outcome = await verify(await landed());
    expect(outcome).toMatchObject({
      kind: "verified",
      frontier: { headerHash: F },
      tail: { headerHash: H1 },
    });
  }, 60_000);

  it("catches a corrupted confirmed_ledger entry that a tail row spends, at the frontier", async () => {
    const corrupted = { ...A, output: output(9_000_000) };
    const ledger = await landed({ confirmed: [corrupted, B] });
    const outcome = await verify(ledger);
    expect(outcome.kind).toBe("diverged");
    expect(outcome).toMatchObject({
      divergences: [
        {
          check: "confirmed_ledger",
          headerHash: F,
          root: ledger.frontier.utxosRoot,
          derivedRoot: await rootOf([corrupted, B]),
        },
      ],
    });
  }, 60_000);

  it("catches a corrupted produced output in a landed row, at the tail", async () => {
    const corrupted = { ...C, output: output(9_000_000) };
    const ledger = await landed({ produced: [corrupted] });
    const outcome = await verify(ledger);
    expect(outcome.kind).toBe("diverged");
    expect(outcome).toMatchObject({
      divergences: [
        {
          check: "confirmed_ledger_tail",
          headerHash: H1,
          root: ledger.chain[0]!.utxosRoot,
          derivedRoot: await rootOf([B, corrupted]),
        },
      ],
    });
  }, 60_000);

  it("reports both when both diverge, the frontier's first", async () => {
    const ledger = await landed({
      confirmed: [{ ...A, output: output(9_000_000) }, B],
      produced: [{ ...C, output: output(8_000_000) }],
    });
    const outcome = await verify(ledger);
    expect(outcome.kind).toBe("diverged");
    expect(outcome).toMatchObject({
      divergences: [
        { check: "confirmed_ledger", headerHash: F },
        { check: "confirmed_ledger_tail", headerHash: H1 },
      ],
    });
  }, 60_000);

  it("reports a tail delta that does not apply under the tail's check, and still checks the frontier", async () => {
    const ledger = await landed({ produced: [B] });
    const outcome = await verify(ledger);
    expect(outcome.kind).toBe("diverged");
    expect(outcome).toMatchObject({
      divergences: [
        {
          check: "confirmed_ledger_tail",
          headerHash: H1,
          fault: expect.stringContaining(
            `produces ${B.outref.toString("hex")}, which the ledger before it already holds`,
          ) as unknown as string,
        },
      ],
    });
  }, 60_000);

  it("checks only the frontier when no processed row follows it", async () => {
    const ledger = { ...(await landed()), chain: [] };
    expect(await verify(ledger)).toMatchObject({
      kind: "verified",
      tail: { headerHash: F },
    });
    const outcome = await verify({
      ...ledger,
      confirmed: stored([A, { ...B, output: output(7_000_000) }]),
    });
    expect(outcome).toMatchObject({
      kind: "diverged",
      divergences: [{ check: "confirmed_ledger", headerHash: F }],
    });
  }, 60_000);
});
