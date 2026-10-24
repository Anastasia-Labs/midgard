/**
 * On honest blocks, the import by committed verdicts is the full replay:
 * every transition and every ledger byte the block replay produces is the
 * same whether each script runs or each transaction is applied by its
 * committed verdict. The committed replay builds no script context; the
 * checker then rebuilds, from the material stored per transaction, exactly
 * the runs the replay met, with the contexts a full replay builds.
 */
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { replayBlockEvents } from "@al-ft/midgard-fault-proofs/block-replay";
import {
  evaluateMidgardCekVerdict,
  type MidgardCekVerdictRequest,
  type MidgardCommittedScriptRun,
  validationMachineLedgerRoot,
} from "@al-ft/midgard-validation";
import { outRefFromTxId } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { type ImportCheckEvaluator } from "../src/import-check/evaluator.js";
import {
  checker,
  entries,
  processBlock,
  useCheckerDatabase,
} from "./import-check.checker.fixture.js";
import {
  committedBlock,
  fundedBy,
  type LedgerEntry,
  type Spend,
} from "./import-check.fixture.js";

useCheckerDatabase();

type Block = Awaited<ReturnType<typeof committedBlock>>;

/** `block`'s replay on `parent`: running every script, or by the committed
 * verdicts. Returns each transition, the ledger after it, and each run the
 * committed replay built. */
const replay = (
  block: Block,
  parent: readonly LedgerEntry[],
  committed: boolean,
) => {
  const transitions: unknown[] = [];
  const runs: MidgardCommittedScriptRun[] = [];
  return Effect.runPromise(
    replayBlockEvents({
      header: block.payload.block_body.header,
      payload: block.payload,
      parentEntries: parent,
      ledgerRoot: (ledger) =>
        Effect.promise(async () =>
          (await validationMachineLedgerRoot(ledger)).toString("hex"),
        ),
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      blockTimeMs: 2,
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      replayUserEvent: () => Effect.die("no user event in this block"),
      verifyForcedSource: () => Effect.die("no forced event in this block"),
      onReplayedEvent: (event) => transitions.push(event),
      ...(committed
        ? {
            committedVerdicts: {
              onScriptRun: (run) => void runs.push(run),
              onTransaction: () => undefined,
            },
          }
        : {}),
    }),
  ).then((ledger) => ({ ledger, transitions, runs }));
};

const identityOf = (run: MidgardCommittedScriptRun) => ({
  scriptBytes: Buffer.from(run.scriptBytes).toString("hex"),
  executionBudget: run.executionBudget,
  executionIndex: run.executionIndex,
  maxSteps: run.maxSteps,
  scriptLanguage: run.scriptLanguage,
});

const bytesOf = (run: MidgardCekVerdictRequest) => ({
  scriptBytes: Buffer.from(run.scriptBytes).toString("hex"),
  contextCbor: Buffer.from(run.contextCbor).toString("hex"),
  executionBudget: run.executionBudget,
  executionIndex: run.executionIndex,
});

/** The parent ledger and honest spends: several inputs, reference inputs
 * shared between transactions, and a transaction spending an output of an
 * earlier one in the block. */
const honest = async () => {
  const parent = Array.from({ length: 6 }, (_, index) =>
    fundedBy(0x71 + index, "accept"),
  );
  const first: Spend = {
    input: parent[0]!.outRef,
    references: [parent[4]!.outRef, parent[5]!.outRef],
    body: "accept",
    next: "accept",
  };
  const firstOutput = outRefFromTxId(
    (await committedBlock(parent, [first])).txIds[0]!,
    0n,
  );
  const spends: Spend[] = [
    first,
    {
      input: parent[1]!.outRef,
      extra: [parent[2]!.outRef],
      references: [parent[4]!.outRef],
      body: "accept",
      next: "accept",
    },
    {
      input: firstOutput,
      extra: [parent[3]!.outRef],
      references: [parent[5]!.outRef],
      body: "accept",
      next: "accept",
    },
  ];
  return { parent, block: await committedBlock(parent, spends) };
};

describe("import by committed verdicts on honest blocks", () => {
  it("replays every transition and ledger byte as the full replay", async () => {
    const { parent, block } = await honest();
    const full = await replay(block, parent, false);
    const byVerdict = await replay(block, parent, true);
    expect(full.transitions).toHaveLength(3);
    expect(byVerdict.transitions).toEqual(full.transitions);
    expect(byVerdict.ledger).toEqual(full.ledger);
    expect(byVerdict.runs).toHaveLength(5);
    expect(byVerdict.runs.filter((run) => "contextCbor" in run)).toEqual([]);
  }, 120_000);

  it("rebuilds from the stored material exactly the runs the replay met, with a full replay's contexts", async () => {
    const { parent, block } = await honest();
    const built = (await replay(block, parent, true)).runs;
    await processBlock(block);
    const rebuilt: MidgardCekVerdictRequest[] = [];
    const recording: ImportCheckEvaluator = {
      backlogShare: 1,
      evaluate: (run) =>
        Effect.sync(() => {
          rebuilt.push(run);
          return {
            kind: "evaluated" as const,
            result: evaluateMidgardCekVerdict(run),
          };
        }),
    };
    expect(await checker(recording).importPass()).toEqual([]);
    expect(rebuilt.map(identityOf)).toEqual(built.map(identityOf));
    expect(rebuilt.map(bytesOf)).toEqual(block.runs.flat().map(bytesOf));
    expect(await entries()).toEqual([]);
  }, 120_000);
});
