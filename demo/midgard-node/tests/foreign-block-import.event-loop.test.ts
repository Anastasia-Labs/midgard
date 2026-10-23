/**
 * A foreign block's import, and an import check's rebuild, give the event
 * loop a turn between their transactions and between their runs: a timer
 * keeps firing while they work, so the longest stretch without one is
 * about one transaction's (one run's) work, not the whole import's.
 */
import { evaluateMidgardCekVerdict } from "@al-ft/midgard-validation";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  importCheckOutRefs,
  rebuildAndEvaluate,
} from "../src/import-check/rebuild.js";
import {
  committedBlock,
  fundedBy,
  type LedgerEntry,
} from "./import-check.fixture.js";

/**
 * Runs `work` while a 1 ms timer ticks. Returns how long it took, and the
 * longest stretch in it without a tick.
 */
const timed = async <A>(work: () => Promise<A>) => {
  const ticks: number[] = [];
  const timer = setInterval(() => ticks.push(performance.now()), 1);
  const start = performance.now();
  let value: A;
  let end: number;
  try {
    value = await work();
    end = performance.now();
  } finally {
    clearInterval(timer);
  }
  const marks = [start, ...ticks.filter((tick) => tick < end), end];
  let longest = 0;
  for (let index = 1; index < marks.length; index += 1)
    longest = Math.max(longest, marks[index]! - marks[index - 1]!);
  return { value, total: end - start, longest };
};

/** `count` outputs locked by an accepting script, from `byte` on. */
const funded = (byte: number, count: number): LedgerEntry[] =>
  Array.from({ length: count }, (_, index) => fundedBy(byte + index, "accept"));

/** Outputs every transaction references, so each has work of its own. */
const referenced = funded(0x80, 40);
const references = referenced.map((entry) => entry.outRef);

/** Each stretch is about one unit's work; the whole run is `units` of
 * them, plus the work before the first and after the last. */
const UNITS = 16;
const STRETCH_UNITS = 5;

describe("foreign import and import checks keep the event loop turning", () => {
  it("gives the event loop a turn between a block's transactions", async () => {
    const spent = funded(0x10, UNITS);
    const block = await committedBlock(
      [...spent, ...referenced],
      spent.map((entry) => ({
        input: entry.outRef,
        references,
        body: "accept" as const,
        next: "accept" as const,
      })),
    );
    const run = await timed(() => block.imported());
    expect(run.value._tag).toBe("Right");
    const perTransaction = run.total / UNITS;
    expect(run.longest).toBeLessThan(STRETCH_UNITS * perTransaction);
  });

  it("gives the event loop a turn between a check's runs, with an evaluator that answers at once", async () => {
    const spent = funded(0x10, UNITS);
    const block = await committedBlock(
      [...spent, ...referenced],
      [
        {
          input: spent[0]!.outRef,
          extra: spent.slice(1).map((entry) => entry.outRef),
          references,
          body: "accept",
          next: "accept",
          // The transaction's declared cpu, shared between its runs.
          cpu: 1_000_000_000n / BigInt(UNITS),
        },
      ],
    );
    const imported = await block.imported();
    if (imported._tag === "Left") throw imported.left;
    const { context, checks, outputs } = imported.right.scriptChecks;
    const [check] = checks;
    expect(check!.runs).toBe(UNITS);
    const material = {
      transactionCbor: check!.transactionCbor,
      context,
      outputs: importCheckOutRefs(check!.transactionCbor).map((outRef) => ({
        outRef,
        output: outputs.get(outRef.toString("hex"))!,
      })),
    };
    const rebuilt = () =>
      Effect.runPromise(
        rebuildAndEvaluate(check!.transactionId, material, (request) =>
          Effect.sync(() => ({
            kind: "evaluated" as const,
            result: evaluateMidgardCekVerdict(request),
          })),
        ),
      );
    // The process's first evaluation pays a one-time warm-up longer than
    // several runs' work: rebuild the check once before timing it.
    expect(await rebuilt()).toEqual({ kind: "accepted" });
    const run = await timed(rebuilt);
    expect(run.value).toEqual({ kind: "accepted" });
    const perRun = run.total / UNITS;
    expect(run.longest).toBeLessThan(STRETCH_UNITS * perRun);
  });
});
