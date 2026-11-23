/**
 * The ledger store walk (`ledgerBeforeStep`) over small committed blocks:
 * each step's ledger opens the root its predecessor step commits, reads and
 * proves as the from-scratch reference does, refuses what the reference
 * refuses, is shared by every reader of the block, and treats a predecessor
 * that stops being readable as a local fault the next reader recovers from.
 * `tests/event-ledger.differential.test.ts` compares the walk with the
 * reference over seeded blocks.
 */
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { describe, expect, it } from "vitest";

import { isReplayLocalFault } from "../src/block-replay.faults.js";
import { ledgerBeforeStep } from "../src/event-ledger.ledger.js";
import {
  EventLedgerOutputNotCanonical,
  EventLedgerReplayRefusal,
} from "../src/event-ledger.steps.js";
import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type Ledger,
  ledgerAt,
  LedgerNodeMissing,
  LedgerOutputMissing,
} from "../src/ledger-store/index.js";
import { completeCanonicalReplayPredecessorStoreLedger } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import { outRefCbor } from "./helpers/canonical-block-evidence-fixture.js";
import {
  block,
  NATIVE,
  OTHER,
  SPENT,
} from "./helpers/event-ledger-committed-steps.js";
import {
  ledgerSnapshot,
  outcome,
  referenceSnapshot,
} from "./helpers/event-ledger-ledger-snapshots.js";
import { referenceLedgerBefore } from "./helpers/event-ledger-reference.js";
import { hex, outRef } from "./helpers/event-ledger-seeded-blocks.js";
import { entriesOf, memoryLedger } from "./support/fixture-ledger.js";
import { oracleLedgerRoot } from "./support/ledger-root-oracle.js";

/** Three applied steps over the predecessor `NATIVE, SPENT, OTHER`: the
 * first spends `SPENT` and pays one output, the second spends `OTHER`. */
const threeSteps = async () =>
  await block([
    {
      input: {
        spendInputs: [SPENT.outRef],
        outputs: [makeOutput(1_500_000n)],
        fee: 1n,
      },
      applied: true,
    },
    {
      input: {
        spendInputs: [OTHER.outRef],
        outputs: [makeOutput(2_500_000n)],
        fee: 2n,
      },
      applied: true,
    },
    {
      input: { spendInputs: [outRefCbor(0xa1, 0n)], fee: 3n },
      applied: true,
    },
  ]);

const traceStep = (evidence: CanonicalBlockEvidence, index: number) =>
  evidence.reconstruction.traceByStepIndex.get(BigInt(index))!.value;

describe("ledger store walk", () => {
  it("gives each step the root its predecessor step commits", async () => {
    const fixture = await threeSteps();
    const predecessor = completeCanonicalReplayPredecessorStoreLedger(fixture)!;
    for (const index of [1, 2]) {
      const ledger = await ledgerBeforeStep({
        block: fixture.evidence,
        eventKey: fixture.key(index),
        stepIndex: BigInt(index),
        predecessor,
      });
      expect(ledger.root).toBe(
        traceStep(fixture.evidence, index - 1).post_utxos_root,
      );
    }
  });

  it("reads and proves a spent, a produced and an untouched out-ref as the reference does", async () => {
    const fixture = await threeSteps();
    const keys = [SPENT.outRef, outRef(fixture.txs[0]!.txId, 0), OTHER.outRef];
    const request = {
      block: fixture.evidence,
      eventKey: fixture.key(1),
      stepIndex: 1n,
      predecessor: completeCanonicalReplayPredecessorStoreLedger(fixture)!,
    };
    const ledger = await ledgerBeforeStep(request);
    const read = await ledger.read(keys);
    expect(read.get(hex(keys[0]!))!.output).toBeUndefined();
    expect(read.get(hex(keys[1]!))!.output).toBeDefined();
    expect(await ledgerSnapshot(ledger, keys)).toStrictEqual(
      await referenceSnapshot(await referenceLedgerBefore(request), keys),
    );
  });

  it("refuses an input a step spends twice at that step", async () => {
    const fixture = await block([
      {
        input: { spendInputs: [SPENT.outRef, SPENT.outRef], fee: 1n },
        applied: true,
      },
      { input: { spendInputs: [OTHER.outRef], fee: 2n }, applied: true },
    ]);
    expect(
      await outcome(
        async () =>
          await ledgerBeforeStep({
            block: fixture.evidence,
            eventKey: fixture.key(1),
            stepIndex: 1n,
            predecessor:
              completeCanonicalReplayPredecessorStoreLedger(fixture)!,
          }),
        async () => "ledger",
      ),
    ).toMatchObject({
      refused: {
        name: "EventLedgerReplayRefusal",
        reason: "transaction_input_absent",
        step: 0n,
      },
    });
  });
});

/** `ledger` in a memory store of its own, read through a source that counts
 * the predecessor nodes and outputs every walk reads. */
const countedPredecessor = async (ledger: Ledger) => {
  const storage = createMemoryLedgerStorage();
  await createLedgerStore(storage).admit(await entriesOf(ledger), ledger.root);
  let reads = 0;
  const predecessor = ledgerAt(ledger.root, {
    node: async (hash) => {
      reads += 1;
      return (await storage.readNodes([hash]))[0];
    },
    outputs: async (outRefs) => {
      reads += outRefs.length;
      return await storage.readOutputs(outRefs);
    },
  });
  return { predecessor, reads: () => reads };
};

describe("the block's shared ledger store walk", () => {
  it("applies each step once for every reader", async () => {
    const fixture = await threeSteps();
    const { predecessor, reads } = await countedPredecessor(
      completeCanonicalReplayPredecessorStoreLedger(fixture)!,
    );
    const before = async () =>
      await ledgerBeforeStep({
        block: fixture.evidence,
        eventKey: fixture.key(2),
        stepIndex: 2n,
        predecessor,
      });
    const first = await before();
    const walked = reads();
    expect(walked).toBeGreaterThan(0);
    expect(await before()).toBe(first);
    expect(reads()).toBe(walked);
  });

  // Step 1 pays an output that no ledger can hold, or spends an input the
  // ledger does not hold; its operator's root leaves its effects out.
  it.each([
    ["non-canonical output", [Buffer.from("a0", "hex")], OTHER.outRef],
    ["absent input", [], outRefCbor(0x5f, 0n)],
  ])("keeps a block's %s for every reader", async (_, outputs, input) => {
    const fixture = await block([
      { input: { spendInputs: [SPENT.outRef], fee: 7n }, applied: true },
      { input: { spendInputs: [input], outputs, fee: 7n }, applied: false },
      { input: { spendInputs: [NATIVE.outRef], fee: 7n }, applied: true },
    ]);
    const { predecessor, reads } = await countedPredecessor(
      completeCanonicalReplayPredecessorStoreLedger(fixture)!,
    );
    const before = () =>
      ledgerBeforeStep({
        block: fixture.evidence,
        eventKey: fixture.key(2),
        stepIndex: 2n,
        predecessor,
      }).catch((error: unknown) => error);
    const found = await before();
    expect(found).toBeInstanceOf(
      outputs.length > 0
        ? EventLedgerOutputNotCanonical
        : EventLedgerReplayRefusal,
    );
    const walked = reads();
    expect(await before()).toBe(found);
    expect(reads()).toBe(walked);
  });
});

describe("ledger store walk from a predecessor the block does not start from", () => {
  it.each([
    ["another ledger", true],
    ["no ledger", false],
  ] as const)("refuses %s as the reference does", async (_, supplied) => {
    const fixture = await threeSteps();
    // The predecessor without `NATIVE`: a ledger with another root.
    const other = await memoryLedger(
      [SPENT, OTHER],
      await oracleLedgerRoot([SPENT, OTHER]),
    );
    const request = {
      block: fixture.evidence,
      eventKey: fixture.key(1),
      stepIndex: 1n,
      predecessor: supplied ? other : undefined,
    };
    const expected = await outcome(
      () => referenceLedgerBefore(request),
      async () => "ledger",
    );
    expect(expected).toHaveProperty("refused");
    expect(
      await outcome(
        () => ledgerBeforeStep(request),
        async () => "ledger",
      ),
    ).toStrictEqual(expected);
  });
});

describe("ledger store walk over a predecessor that stops being readable", () => {
  const request = (
    fixture: Awaited<ReturnType<typeof threeSteps>>,
    index: number,
    predecessor: Ledger,
  ) =>
    ledgerBeforeStep({
      block: fixture.evidence,
      eventKey: fixture.key(index),
      stepIndex: BigInt(index),
      predecessor,
    });

  it("is a local fault when retain dropped the predecessor before the walk", async () => {
    const fixture = await threeSteps();
    const entries = await entriesOf(
      completeCanonicalReplayPredecessorStoreLedger(fixture)!,
    );
    const root = fixture.evidence.header.prevUtxosRoot;
    const store = createLedgerStore(createMemoryLedgerStorage());
    const predecessor = await store.admit(entries, root);
    await store.retain(new Set());
    const error: unknown = await request(fixture, 1, predecessor).catch(
      (cause: unknown) => cause,
    );
    expect(isReplayLocalFault(error)).toBe(true);
    expect((error as { cause: unknown }).cause).toBeInstanceOf(
      LedgerNodeMissing,
    );
  });

  it("is a local fault when retain deletes it mid-walk, and the next request walks afresh", async () => {
    const fixture = await threeSteps();
    const entries = await entriesOf(
      completeCanonicalReplayPredecessorStoreLedger(fixture)!,
    );
    const root = fixture.evidence.header.prevUtxosRoot;
    const store = createLedgerStore(createMemoryLedgerStorage());
    const predecessor = await store.admit(entries, root);
    expect((await request(fixture, 1, predecessor)).root).toBe(
      traceStep(fixture.evidence, 0).post_utxos_root,
    );
    // Step 1 spends `OTHER`, whose output only the store holds.
    await store.retain(new Set());
    const error: unknown = await request(fixture, 2, predecessor).catch(
      (cause: unknown) => cause,
    );
    expect(isReplayLocalFault(error)).toBe(true);
    // The store holds the predecessor's nodes and outputs again.
    await store.admit(entries, root);
    expect((await request(fixture, 2, predecessor)).root).toBe(
      traceStep(fixture.evidence, 1).post_utxos_root,
    );
  });

  it("is a local fault when a held entry's output is gone", async () => {
    const fixture = await threeSteps();
    const held = completeCanonicalReplayPredecessorStoreLedger(fixture)!;
    const storage = createMemoryLedgerStorage();
    await createLedgerStore(storage).admit(await entriesOf(held), held.root);
    const predecessor = ledgerAt(held.root, {
      node: async (hash) => (await storage.readNodes([hash]))[0],
      outputs: async (outRefs) => outRefs.map(() => undefined),
    });
    const error: unknown = await request(fixture, 1, predecessor).catch(
      (cause: unknown) => cause,
    );
    expect(isReplayLocalFault(error)).toBe(true);
    expect((error as { cause: unknown }).cause).toBeInstanceOf(
      LedgerOutputMissing,
    );
  });
});
