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

import {
  committedBlock,
  forcedBlock,
  fundedBy,
} from "./import-check.fixture.js";

const imported = async (
  block: Pick<Awaited<ReturnType<typeof committedBlock>>, "imported">,
) => {
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
    expect(result.scriptChecks.checks).toHaveLength(1);
    const [check] = result.scriptChecks.checks;
    expect(check!.transactionId).toEqual(first.txIds[0]);
    expect(check!.runs).toBe(1);
    expect((await localVerdict(first.runs[0]![0]!)).kind).toBe(
      "script_invalid",
    );

    const next = await committedBlock(first.ledger, [
      { input: first.produced[0]!, body: "accept", next: "accept" },
    ]);
    const nextResult = await imported(next);
    expect(nextResult.root).toBe(next.payload.block_body.header.utxosRoot);
    expect(nextResult.scriptChecks.checks).toHaveLength(1);
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
    expect(result.scriptChecks.checks).toHaveLength(1);
    const local = await localVerdict(first.runs[0]![0]!);
    // The run stops at its budget: it never finishes within what it declared.
    expect(
      local.kind === "script_invalid" ||
        (local.kind === "accepted" && local.budget.cpu > 10n ** 7n),
    ).toBe(true);

    const next = await committedBlock(first.ledger, [
      { input: first.produced[0]!, body: "accept", next: "accept" },
    ]);
    expect((await imported(next)).root).toBe(
      next.payload.block_body.header.utxosRoot,
    );
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
    expect(result.scriptChecks.checks.map((check) => check.position)).toEqual([
      0, 1,
    ]);
    for (const runs of block.runs)
      for (const run of runs)
        expect((await localVerdict(run)).kind).toBe("accepted");
  });

  it("keeps each transaction once, and each output its checks spend or reference once per block", async () => {
    const spent = [0x81, 0x82, 0x83, 0x84].map((byte) =>
      fundedBy(byte, "accept"),
    );
    const referenced = [0x91, 0x92, 0x93].map((byte) =>
      fundedBy(byte, "accept"),
    );
    const references = referenced.map((entry) => entry.outRef);
    const block = await committedBlock(
      [...spent, ...referenced],
      [
        {
          input: spent[0]!.outRef,
          extra: [spent[1]!.outRef, spent[2]!.outRef],
          references,
          body: "accept",
          next: "accept",
        },
        {
          input: spent[3]!.outRef,
          references,
          body: "accept",
          next: "accept",
        },
      ],
    );
    const { scriptChecks } = await imported(block);
    expect(scriptChecks.checks.map((check) => check.runs)).toEqual([3, 1]);
    expect(
      scriptChecks.checks.map((check) => check.transactionCbor.length),
    ).toEqual(
      block.payload.block_body.transaction_preimages.map(
        ([, preimage]) => preimage.length / 2,
      ),
    );
    // Four spent outputs and three referenced by both transactions, once.
    expect([...scriptChecks.outputs.keys()].sort()).toEqual(
      [...spent, ...referenced]
        .map((entry) => entry.outRef.toString("hex"))
        .sort(),
    );
  });

  it("replays a forced transaction's script spend in full: it imports with the rejection a full replay gives it, and leaves no check", async () => {
    const parent = [fundedBy(0x35, "reject")];
    const block = await forcedBlock(parent, {
      input: parent[0]!.outRef,
      body: "reject",
      next: "accept",
    });
    expect(block.rejection.code).toBe("E_PLUTUS_SCRIPT_INVALID");
    const result = await imported(block);
    expect(result.root).toBe(block.payload.block_body.header.utxosRoot);
    expect(result.scriptChecks.checks).toEqual([]);
  });
});
