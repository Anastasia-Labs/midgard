/** Complete replays that do no work the size of the predecessor ledger,
 * however many of its entries or events they visit: the ledger-relative
 * input scan walks the block's steps on the stored predecessor for every
 * event's pre-state, and the resolved-output prior view reads only the
 * outputs the block names, with their proofs from the ledger. Results are
 * those of a one-off reconstruction. Work is counted as in
 * `helpers/trie-leaves.ts`. */
import { decodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { ledgerBeforeStep } from "../src/event-ledger.ledger.js";
import { readResolvedOutputPriorLedger } from "../src/resolved-output-non-canonical/resolved-output-non-canonical.js";
import { keyValuePhasProof } from "../src/transition-trace/phas.js";
import {
  completeCanonicalReplayPredecessorStoreLedger,
  detectLedgerRelativeMissingInputs,
  EVENT_PRE_STATE_UNAVAILABLE_VIOLATION_ID,
} from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import { referenceLedgerOf } from "./helpers/event-ledger-reference.js";
import { inputExistenceChronologyFixture } from "./helpers/input-existence-chronology-fixture.js";
import { plutusExecutionEventsBlockFixture } from "./helpers/plutus-execution-events-fixture.js";
import { filler, trieLeaves } from "./helpers/trie-leaves.js";
import { entriesOf } from "./support/fixture-ledger.js";

type Block = Awaited<ReturnType<typeof plutusExecutionEventsBlockFixture>>;

const FILLER = 300;

const predecessorOf = (block: Block) =>
  completeCanonicalReplayPredecessorStoreLedger({
    evidence: block.evidence,
    context: block.context,
  })!;

const spendScan = async (block: Block) =>
  await detectLedgerRelativeMissingInputs({
    evidence: block.evidence,
    context: block.context,
    kind: "spend",
  });

describe("complete replay: no work the size of the predecessor ledger", () => {
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
    // Events at steps 0 to 3: the walk to each of them builds no trie the
    // size of the predecessor, however large it is.
    expect(largeScan.leaves).toBe(smallScan.leaves);
  });

  it("builds no trie of the predecessor for the ledger before a single event, however many steps precede it", async () => {
    const spends = ["passing", "passing", "passing", "passing"] as const;
    const [small, large] = await Promise.all([
      plutusExecutionEventsBlockFixture(spends),
      plutusExecutionEventsBlockFixture(spends, {
        predecessorFiller: filler(FILLER),
      }),
    ]);
    const lastBefore = async (block: Block) =>
      await trieLeaves(() =>
        ledgerBeforeStep({
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
    expect(await entriesOf(largeLedger.result)).toHaveLength(
      (await entriesOf(smallLedger.result)).length + FILLER,
    );
    expect(largeLedger.leaves).toBe(smallLedger.leaves);
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

  it("reads only the outputs the block names from the predecessor ledger", async () => {
    const [small, large] = await Promise.all([
      plutusExecutionEventsBlockFixture(["passing"], {
        predecessorFiller: filler(8),
      }),
      plutusExecutionEventsBlockFixture(["passing"], {
        predecessorFiller: filler(8 + FILLER),
      }),
    ]);
    const read = async (block: Block) => {
      const predecessor = completeCanonicalReplayPredecessorStoreLedger({
        evidence: block.evidence,
        context: block.context,
      });
      return await trieLeaves(() =>
        readResolvedOutputPriorLedger({
          block: block.evidence,
          predecessor,
        }),
      );
    };
    const [smallView, largeView] = [await read(small), await read(large)];
    // No trie the size of the predecessor is built, however large it is.
    expect(smallView.leaves).toBe(0);
    expect(largeView.leaves).toBe(0);

    const predecessor = await referenceLedgerOf(predecessorOf(small));
    const fillerKeys = new Set(
      filler(8).map(({ outRef }) => outRef.toString("hex")),
    );
    const named = predecessor.trie.entries.filter(
      ({ key }) => !fillerKeys.has(key.toString("hex")),
    );
    expect(named.length).toBeGreaterThan(0);
    expect(smallView.result.priorRoot).toBe(predecessor.utxosRoot);
    expect([...largeView.result.outputs.keys()]).toStrictEqual([
      ...smallView.result.outputs.keys(),
    ]);
    expect(smallView.result.outputs.size).toBe(named.length);
    for (const entry of named) {
      const { txId, outputIndex } = decodeMidgardSpendInputItem(entry.key);
      const output = smallView.result.outputs.get(
        `${Buffer.from(txId).toString("hex")}#${outputIndex.toString()}`,
      );
      expect(output?.descriptorCborHex).toBe(entry.value.toString("hex"));
      expect(output?.outputCborHex).toBe(
        predecessor.outputs.get(entry.key.toString("hex"))?.toString("hex"),
      );
      expect(output?.membershipProof).toStrictEqual(
        await keyValuePhasProof(predecessor.trie, entry.key, entry.value),
      );
    }
  });
});
