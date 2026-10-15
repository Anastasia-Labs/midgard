/**
 * A foreign block's Plutus transactions are applied by their committed
 * verdicts: the import runs no script and returns each transaction's runs
 * for the import checker. A transaction the local evaluator rejects, or
 * cannot finish, is followed, and the next block spending its output
 * imports; an honest block imports as before.
 */
import {
  evaluateMidgardCekVerdict,
  type MidgardCekVerdictRequest,
} from "@al-ft/midgard-validation";
import { describe, expect, it } from "vitest";

import { committedBlock, fundedBy } from "./import-check.fixture.js";

const imported = async (block: Awaited<ReturnType<typeof committedBlock>>) => {
  const result = await block.imported();
  if (result._tag === "Left") throw result.left;
  return result.right;
};

/** The local evaluator's verdict on one run, as the checker gets it. */
const localVerdict = async (run: MidgardCekVerdictRequest) =>
  evaluateMidgardCekVerdict(run);

describe("foreign import by committed verdict", () => {
  it("follows a transaction the local evaluator rejects, and imports the next block spending its output", async () => {
    const parent = [fundedBy(0x31, "reject")];
    const first = await committedBlock(parent, [
      { input: parent[0]!.outRef, body: "reject", next: "accept" },
    ]);
    const result = await imported(first);
    expect(result.root).toBe(first.payload.block_body.header.utxosRoot);
    expect(result.scriptChecks).toHaveLength(1);
    const [check] = result.scriptChecks;
    expect(check!.transactionId).toEqual(first.txIds[0]);
    expect(check!.runs).toHaveLength(1);
    expect((await localVerdict(check!.runs[0]!)).kind).toBe("script_invalid");

    const next = await committedBlock(first.ledger, [
      { input: first.produced[0]!, body: "accept", next: "accept" },
    ]);
    const nextResult = await imported(next);
    expect(nextResult.root).toBe(next.payload.block_body.header.utxosRoot);
    expect(nextResult.scriptChecks).toHaveLength(1);
  });

  it("follows a transaction the local evaluator cannot finish within its budget, and imports the next block spending its output", async () => {
    const parent = [fundedBy(0x32, "loop")];
    const first = await committedBlock(parent, [
      {
        input: parent[0]!.outRef,
        body: "loop",
        next: "accept",
        cpu: 10n ** 7n,
      },
    ]);
    const result = await imported(first);
    const run = result.scriptChecks[0]!.runs[0]!;
    const local = await localVerdict(run);
    // The run stops at its budget: it never finishes within what it declared.
    expect(
      local.kind === "script_invalid" || local.budget.cpu > 10n ** 7n,
    ).toBe(true);

    const next = await committedBlock(first.ledger, [
      { input: first.produced[0]!, body: "accept", next: "accept" },
    ]);
    expect((await imported(next)).root).toBe(
      next.payload.block_body.header.utxosRoot,
    );
  });

  it("imports several transactions in less time than one local run of them takes", async () => {
    const parent = [0x40, 0x41, 0x42, 0x43].map((byte) =>
      fundedBy(byte, "loop"),
    );
    const block = await committedBlock(
      parent,
      parent.map((entry) => ({
        input: entry.outRef,
        body: "loop" as const,
        next: "accept" as const,
        cpu: 10n ** 10n,
      })),
    );
    const startedAt = performance.now();
    const result = await imported(block);
    const importMs = performance.now() - startedAt;
    expect(result.scriptChecks.map((check) => check.transactionId)).toEqual(
      block.txIds,
    );
    const runStartedAt = performance.now();
    await localVerdict(result.scriptChecks[0]!.runs[0]!);
    const runMs = performance.now() - runStartedAt;
    expect(importMs).toBeLessThan(runMs);
  });

  it("imports an honest block unchanged, with its runs for the checker", async () => {
    const parent = [fundedBy(0x33, "accept"), fundedBy(0x34, "accept")];
    const block = await committedBlock(
      parent,
      parent.map((entry) => ({
        input: entry.outRef,
        body: "accept" as const,
        next: "accept" as const,
      })),
    );
    const result = await imported(block);
    expect(result.root).toBe(block.payload.block_body.header.utxosRoot);
    expect(result.scriptChecks.map((check) => check.position)).toEqual([0, 1]);
    for (const check of result.scriptChecks)
      for (const run of check.runs)
        expect((await localVerdict(run)).kind).toBe("accepted");
  });
});
