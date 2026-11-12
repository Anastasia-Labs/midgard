/** Complete replays that read the predecessor ledger once per block, however
 * many of its entries or events they visit: the ledger-relative input scan
 * walks the block's steps once for every event's pre-state, editing one trie
 * of the predecessor, and the resolved-output prior view proves every entry
 * from one trie. Results are those of a one-off reconstruction. Work is
 * counted as in `helpers/trie-leaves.ts`. */
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { ledgerBeforeTransactionEvent } from "../src/event-ledger.js";
import { deriveResolvedOutputPriorLedgerReplay } from "../src/resolved-output-non-canonical/resolved-output-non-canonical.js";
import { keyValuePhasProof } from "../src/transition-trace/phas.js";
import {
  completeCanonicalReplayPredecessorLedger,
  detectLedgerRelativeMissingInputs,
  EVENT_PRE_STATE_UNAVAILABLE_VIOLATION_ID,
} from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import { inputExistenceChronologyFixture } from "./helpers/input-existence-chronology-fixture.js";
import { plutusExecutionEventsBlockFixture } from "./helpers/plutus-execution-events-fixture.js";
import { filler, trieLeaves } from "./helpers/trie-leaves.js";

type Block = Awaited<ReturnType<typeof plutusExecutionEventsBlockFixture>>;

const FILLER = 300;

const predecessorOf = (block: Block) =>
  completeCanonicalReplayPredecessorLedger({
    evidence: block.evidence,
    context: block.context,
  })!;

const spendScan = async (block: Block) =>
  await detectLedgerRelativeMissingInputs({
    evidence: block.evidence,
    context: block.context,
    kind: "spend",
  });

describe("complete replay: the predecessor ledger is read once per block", () => {
  it("walks the block's steps once for every event's input scan", async () => {
    const spends = ["passing", "passing", "passing", "passing"] as const;
    const [small, large] = await Promise.all([
      plutusExecutionEventsBlockFixture(spends),
      plutusExecutionEventsBlockFixture(spends, {
        predecessorFiller: filler(FILLER),
      }),
    ]);
    const [smallScan, largeScan] = [
      await trieLeaves(() => spendScan(small)),
      await trieLeaves(() => spendScan(large)),
    ];
    expect(smallScan.result).toStrictEqual([]);
    expect(largeScan.result).toStrictEqual([]);
    // Events at steps 0 to 3: one walk applies steps 0, 1 and 2 to one trie
    // of the predecessor.
    expect((largeScan.leaves - smallScan.leaves) / FILLER).toBe(1);
  });

  it("builds one trie of the predecessor for the ledger before a single event, however many steps precede it", async () => {
    const spends = ["passing", "passing", "passing", "passing"] as const;
    const [small, large] = await Promise.all([
      plutusExecutionEventsBlockFixture(spends),
      plutusExecutionEventsBlockFixture(spends, {
        predecessorFiller: filler(FILLER),
      }),
    ]);
    const lastBefore = async (block: Block) =>
      await trieLeaves(() =>
        ledgerBeforeTransactionEvent({
          block: block.evidence,
          eventKey: {
            L2TransactionEventKey: { tx_id: block.txs.at(-1)!.txId },
          },
          stepIndex: 3n,
          predecessor: predecessorOf(block),
        }),
      );
    const [smallLedger, largeLedger] = [
      await lastBefore(small),
      await lastBefore(large),
    ];
    expect(largeLedger.result.entries).toHaveLength(
      smallLedger.result.entries.length + FILLER,
    );
    expect((largeLedger.leaves - smallLedger.leaves) / FILLER).toBe(1);
  });

  it("holds every event after a step whose committed root its ledger does not open, in source order", async () => {
    // The events at steps 1 to 5 follow the foreign root. The block's source
    // order follows its transaction ids, which differ between runs; a block
    // whose source order is its step order is rebuilt, so that findings in
    // step order cannot pass.
    const build = () =>
      plutusExecutionEventsBlockFixture(
        ["passing", "passing", "passing", "passing", "passing", "passing"],
        { foreignPostRootAt: 0 },
      );
    let block = await build();
    while (
      block.evidence.reconstruction.transactions.every(
        ({ txId }, index) => txId === block.txs[index]!.txId,
      )
    )
      block = await build();
    const held = new Set(block.txs.slice(1).map(({ txId }) => txId));
    const expected = block.evidence.reconstruction.transactions.flatMap(
      ({ txId }, position) =>
        held.has(txId)
          ? [
              {
                detectionId: `${EVENT_PRE_STATE_UNAVAILABLE_VIOLATION_ID}:spend:${position}:${txId}`,
                position: BigInt(position),
                diagnostic:
                  "input classification event-pre-state hold: post_root_changed",
              },
            ]
          : [],
    );
    expect(
      (await spendScan(block)).map(({ detectionId, position, diagnostic }) => ({
        detectionId,
        position,
        diagnostic,
      })),
    ).toStrictEqual(expected);
  });

  it("refuses a forced event after normal ones by its own phase order, not by a normal step's failure", async () => {
    // Steps: normal A, normal C spending A's input again, normal D, then
    // forced B. The ledger before B stops at A, of a later phase, before it
    // reaches the step D's ledger fails at.
    const block = await inputExistenceChronologyFixture(
      "forced_after_failed_normal",
      0,
    );
    const detections = await detectLedgerRelativeMissingInputs({
      evidence: block.evidence,
      context: block.context,
      kind: "spend",
    });
    expect(
      detections.map(({ violationId, diagnostic }) => [
        violationId,
        diagnostic,
      ]),
    ).toStrictEqual(
      expect.arrayContaining([
        [
          EVENT_PRE_STATE_UNAVAILABLE_VIOLATION_ID,
          "input classification event-pre-state hold: transaction_input_absent",
        ],
        ["non-existent-input", expect.stringContaining("spend input 0")],
      ]),
    );
    expect(detections).toHaveLength(3);
    expect(
      detections.find(({ detectionId }) =>
        detectionId.endsWith(block.target.txId),
      ),
    ).toMatchObject({
      detectionId: `${EVENT_PRE_STATE_UNAVAILABLE_VIOLATION_ID}:spend:0:${block.target.txId}`,
      diagnostic:
        "input classification event-pre-state hold: phase_order_changed",
    });
  });

  it("proves every prior output from the one trie of the predecessor", async () => {
    const [small, large] = await Promise.all([
      plutusExecutionEventsBlockFixture(["passing"], {
        predecessorFiller: filler(8),
      }),
      plutusExecutionEventsBlockFixture(["passing"], {
        predecessorFiller: filler(8 + FILLER),
      }),
    ]);
    const derive = async (block: Block) =>
      await trieLeaves(() =>
        deriveResolvedOutputPriorLedgerReplay({
          block: block.evidence,
          predecessor: predecessorOf(block),
        }),
      );
    const [smallView, largeView] = [await derive(small), await derive(large)];
    expect((largeView.leaves - smallView.leaves) / FILLER).toBe(0);

    const predecessor = predecessorOf(small);
    expect(smallView.result.priorRoot).toBe(predecessor.utxosRoot);
    expect(smallView.result.outputs.size).toBe(predecessor.trie.entries.length);
    for (const output of smallView.result.outputs.values()) {
      const key = encodeMidgardSpendInputItem({
        txId: Buffer.from(output.transactionId, "hex"),
        outputIndex: output.outputIndex,
      });
      const entry = predecessor.trie.entries.find((candidate) =>
        candidate.key.equals(key),
      );
      expect(entry?.value.toString("hex")).toBe(output.descriptorCborHex);
      expect(output.outputCborHex).toBe(
        predecessor.outputs.get(key.toString("hex"))?.toString("hex"),
      );
      expect(output.membershipProof).toStrictEqual(
        await keyValuePhasProof(predecessor.trie, key, entry!.value),
      );
    }
  });
});
