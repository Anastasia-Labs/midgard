/**
 * The block's one walk reads its reconstruction's header, so a block whose
 * own header differs is refused, never walked under either. A walk that
 * failed for a reason the block does not cause leaves the shared walks, and
 * the next reader walks afresh. (What the walk finds of the block, a refusal
 * or a non-canonical output, it keeps for every later reader:
 * `tests/event-ledger.ledger.test.ts`.)
 */
import { describe, expect, it } from "vitest";

import { ledgerBeforeStep } from "../src/event-ledger.ledger.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  ledgerAt,
} from "../src/ledger-store/index.js";
import { completeCanonicalReplayPredecessorStoreLedger } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import { block, OTHER, SPENT } from "./helpers/event-ledger-committed-steps.js";
import {
  ledgerSnapshot,
  referenceSnapshot,
} from "./helpers/event-ledger-ledger-snapshots.js";
import { referenceLedgerBefore } from "./helpers/event-ledger-reference.js";
import { entriesOf } from "./support/fixture-ledger.js";

const twoSpends = async () =>
  await block([
    { input: { spendInputs: [SPENT.outRef], fee: 7n }, applied: true },
    { input: { spendInputs: [OTHER.outRef], fee: 7n }, applied: true },
  ]);

describe("the block's shared walk", () => {
  it("refuses a block whose header differs from its reconstruction's", async () => {
    const fixture = await twoSpends();
    const { header } = fixture.evidence;
    for (const changed of [
      { ...header, prevUtxosRoot: "ee".repeat(32) },
      { ...header, transitionStepCount: header.transitionStepCount + 1n },
    ])
      await expect(
        ledgerBeforeStep({
          block: { ...fixture.evidence, header: changed },
          eventKey: fixture.key(1),
          stepIndex: 1n,
          predecessor: completeCanonicalReplayPredecessorStoreLedger(fixture)!,
        }),
      ).rejects.toThrow(
        "event ledger: block header differs from its reconstruction's",
      );
  });

  it("walks afresh for the next reader after a failure the block does not cause", async () => {
    const fixture = await twoSpends();
    const held = completeCanonicalReplayPredecessorStoreLedger(fixture)!;
    const storage = createMemoryLedgerStorage();
    await createLedgerStore(storage).admit(await entriesOf(held), held.root);
    // The first node read fails, as an unreachable store does.
    let reads = 0;
    const predecessor = ledgerAt(held.root, {
      node: async (hash) => {
        reads += 1;
        if (reads === 1) throw new Error("store unavailable");
        return (await storage.readNodes([hash]))[0];
      },
      outputs: async (outRefs) => await storage.readOutputs(outRefs),
    });
    const request = {
      block: fixture.evidence,
      eventKey: fixture.key(1),
      stepIndex: 1n,
      predecessor,
    };
    await expect(ledgerBeforeStep(request)).rejects.toThrow(
      "store unavailable",
    );
    const probes = [SPENT.outRef, OTHER.outRef];
    expect(
      await ledgerSnapshot(await ledgerBeforeStep(request), probes),
    ).toStrictEqual(
      await referenceSnapshot(
        await referenceLedgerBefore({ ...request, predecessor: held }),
        probes,
      ),
    );
  });
});
