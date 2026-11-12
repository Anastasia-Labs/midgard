/**
 * The block's one walk reads its reconstruction's header, so a block whose
 * own header differs is refused, never walked under either. The walk keeps
 * what it finds of the block, a refusal or a non-canonical output, for every
 * later reader; a walk that failed otherwise leaves the shared walks, and the
 * next reader walks afresh.
 */
import { describe, expect, it } from "vitest";

import {
  eventLedgerBefore,
  EventLedgerOutputNotCanonical,
} from "../src/event-ledger.js";
import type { CheckedLedger } from "../src/ledger-at.js";
import { completeCanonicalReplayPredecessorLedger } from "../src/workflow/complete-replay.js";
import {
  block,
  NATIVE,
  OTHER,
  SPENT,
} from "./helpers/event-ledger-committed-steps.js";
import { referenceLedgerBefore } from "./helpers/event-ledger-reference.js";

const twoSpends = async () =>
  await block([
    { input: { spendInputs: [SPENT.outRef], fee: 7n }, applied: true },
    { input: { spendInputs: [OTHER.outRef], fee: 7n }, applied: true },
  ]);

/** `admitted`, counting the reads of its trie's leaves: each walk reads
 * them once, as its first step builds its trie. With `failFirst` the first
 * read fails. */
const counted = (admitted: CheckedLedger, failFirst: boolean) => {
  let reads = 0;
  const predecessor: CheckedLedger = {
    ...admitted,
    trie: {
      ...admitted.trie,
      get entries() {
        reads += 1;
        if (failFirst && reads === 1) throw new Error("leaves unavailable");
        return admitted.trie.entries;
      },
    },
  };
  return { predecessor, reads: () => reads };
};

describe("the block's shared walk", () => {
  it("refuses a block whose header differs from its reconstruction's", async () => {
    const fixture = await twoSpends();
    const { header } = fixture.evidence;
    for (const changed of [
      { ...header, prevUtxosRoot: "ee".repeat(32) },
      { ...header, transitionStepCount: header.transitionStepCount + 1n },
    ])
      await expect(
        eventLedgerBefore({
          block: { ...fixture.evidence, header: changed },
          eventKey: fixture.key(1),
          stepIndex: 1n,
          predecessor: completeCanonicalReplayPredecessorLedger(fixture)!,
        }),
      ).rejects.toThrow(
        "event ledger: block header differs from its reconstruction's",
      );
  });

  it("walks afresh for the next reader after a failure the block does not cause", async () => {
    const fixture = await twoSpends();
    const admitted = completeCanonicalReplayPredecessorLedger(fixture)!;
    const { predecessor } = counted(admitted, true);
    const before = async () =>
      await eventLedgerBefore({
        block: fixture.evidence,
        eventKey: fixture.key(1),
        stepIndex: 1n,
        predecessor,
      });
    await expect(before()).rejects.toThrow("leaves unavailable");
    const reference = await referenceLedgerBefore({
      block: fixture.evidence,
      eventKey: fixture.key(1),
      stepIndex: 1n,
      predecessor: admitted,
    });
    const ledger = await before();
    expect(ledger.utxosRoot).toBe(reference.utxosRoot);
    expect(ledger.entries).toStrictEqual(reference.entries);
  });

  it("walks a block whose output is not canonical once for every reader", async () => {
    // Step 1 pays an output that no ledger can hold; its operator's root
    // leaves it out.
    const fixture = await block([
      { input: { spendInputs: [SPENT.outRef], fee: 7n }, applied: true },
      {
        input: {
          spendInputs: [OTHER.outRef],
          outputs: [Buffer.from("a0", "hex")],
          fee: 7n,
        },
        applied: false,
      },
      { input: { spendInputs: [NATIVE.outRef], fee: 7n }, applied: true },
    ]);
    const { predecessor, reads } = counted(
      completeCanonicalReplayPredecessorLedger(fixture)!,
      false,
    );
    for (let reader = 0; reader < 2; reader += 1)
      await expect(
        eventLedgerBefore({
          block: fixture.evidence,
          eventKey: fixture.key(2),
          stepIndex: 2n,
          predecessor,
        }),
      ).rejects.toThrow(EventLedgerOutputNotCanonical);
    expect(reads()).toBe(1);
  });
});
