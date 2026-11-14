/**
 * The replayed ledger over a block whose script run has no verdict because
 * the evaluator itself failed: that says nothing about the block, so the
 * derivation fails with a local fault (`ReplayLocalFault`), which its caller
 * retries, and the block is never refused for it. A script that runs to a
 * failure within its bounds still refuses the block. The evaluator failure
 * is injected at the pinned UPLC evaluator.
 */
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { isReplayLocalFault } from "../src/block-replay.faults.js";
import { ledgerEntriesRoot } from "../src/ledger-at.js";
import {
  createReplayedLedger,
  type LedgerCheckpoint,
  type ReplayedLedgerRefusal,
  type ReplayedLedgerSource,
} from "../src/replayed-ledger.js";
import { reconstructDaPayload } from "../src/transition-trace/reconstruct.js";
import { plutusExecutionBlockFixture } from "./helpers/plutus-execution-block-fixture.js";

/** The injected evaluator failure, and how often the evaluator ran. */
const evaluator = vi.hoisted(() => ({
  fault: undefined as Error | undefined,
  runs: 0,
}));

vi.mock("@lucid-evolution/uplc", async (importOriginal) => {
  const actual = await importOriginal<typeof import("@lucid-evolution/uplc")>();
  return {
    ...actual,
    eval_program_raw: (
      ...args: Parameters<typeof actual.eval_program_raw>
    ): ReturnType<typeof actual.eval_program_raw> => {
      evaluator.runs += 1;
      if (evaluator.fault !== undefined) throw evaluator.fault;
      return actual.eval_program_raw(...args);
    },
  };
});

afterEach(() => {
  evaluator.fault = undefined;
  evaluator.runs = 0;
});

/** A ledger whose checkpoint is the block's predecessor, and a source
 * serving the block. */
const scene = async (scenario: "passing" | "failing") => {
  const fixture = await plutusExecutionBlockFixture(scenario);
  const { payload } = await reconstructDaPayload({
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
  });
  const header = payload.block_body.header;
  const headerHash = payload.block_body.header_hash;
  const refusals: ReplayedLedgerRefusal[] = [];
  const writes: LedgerCheckpoint[] = [];
  const ledger = createReplayedLedger({
    checkpoints: {
      read: async () => ({
        headerHash: header.prevHeaderHash,
        entries: fixture.predecessorEntries,
      }),
      write: async (checkpoint) => {
        writes.push(checkpoint);
      },
    },
    onRefusal: (refusal) => refusals.push(refusal),
  });
  expect(
    await ledger.restore({
      headerHash: header.prevHeaderHash,
      utxosRoot: header.prevUtxosRoot,
    }),
  ).toBe("restored");
  const source: ReplayedLedgerSource = {
    payloadAt: async (hash) => (hash === headerHash ? payload : undefined),
    materialFor: async () => ({
      blockTimeMs: Number(header.endTime),
      replayUserEvent: () => Effect.die("the block has no user event"),
      verifyForcedSource: () => Effect.die("the block has no forced order"),
    }),
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
  };
  return {
    ledger,
    source,
    headerHash,
    head: { headerHash, utxosRoot: header.utxosRoot },
    refusals,
    writes,
  };
};

/** `promise`'s rejection, which must be a local fault naming `headerHash`. */
const localFault = async (promise: Promise<unknown>, headerHash: string) => {
  const error: unknown = await promise.then(
    (value: unknown) => {
      throw new Error(`the derivation did not fail: ${String(value)}`);
    },
    (rejection: unknown) => rejection,
  );
  expect(isReplayLocalFault(error)).toBe(true);
  expect((error as Error).message).toContain(headerHash);
  expect((error as Error).message).toContain("has no script verdict");
  expect((error as { cause?: unknown }).cause).toMatchObject({
    _tag: "ScriptEvaluationIndeterminate",
  });
};

describe("the replayed ledger over a script run with no verdict", () => {
  it("fails the lookup with a local fault, refuses nothing, and derives the ledger once the evaluator answers", async () => {
    const s = await scene("passing");
    evaluator.fault = new Error("wasm runtime unreachable");
    await localFault(s.ledger.ledgerAt(s.source)(s.headerHash), s.headerHash);
    expect(evaluator.runs).toBeGreaterThan(0);
    expect(s.refusals).toEqual([]);

    evaluator.fault = undefined;
    const entries = await s.ledger.ledgerAt(s.source)(s.headerHash);
    if (entries === undefined) throw new Error("the block was not derived");
    expect(await ledgerEntriesRoot(entries)).toBe(s.head.utxosRoot);
    expect(s.refusals).toEqual([]);
  });

  it("fails the checkpoint advance with a local fault rather than holding it, and moves it once the evaluator answers", async () => {
    const s = await scene("passing");
    evaluator.fault = new Error("wasm runtime unreachable");
    await localFault(s.ledger.advance(s.head, s.source), s.headerHash);
    expect(s.writes).toEqual([]);
    expect(s.refusals).toEqual([]);

    evaluator.fault = undefined;
    expect(await s.ledger.advance(s.head, s.source)).toBe(true);
    expect(s.writes.map(({ headerHash }) => headerHash)).toEqual([
      s.headerHash,
    ]);
  });

  it("still refuses a block whose script runs to a failure within its bounds", async () => {
    const s = await scene("failing");
    expect(await s.ledger.ledgerAt(s.source)(s.headerHash)).toBeUndefined();
    expect(evaluator.runs).toBeGreaterThan(0);
    expect(s.refusals).toMatchObject([
      { headerHash: s.headerHash, kind: "replay-refused" },
    ]);
    expect(await s.ledger.advance(s.head, s.source)).toBe(false);
    expect(s.writes).toEqual([]);
  });
});
