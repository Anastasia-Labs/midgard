/**
 * The ledger store walk (`ledgerBeforeStep`, `ledgerBeforeTraceStep`)
 * against the checked-ledger walk (`eventLedgerBefore`,
 * `eventLedgerBeforeStep`), which applies the same steps to a mutable trie
 * and reads a merged view: for every request of seeded faulty blocks and of
 * the input-existence chronologies, the same root, outputs and proofs, or the
 * same refusal. A predecessor that stops being readable is a local fault the
 * next reader recovers from.
 */
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { describe, expect, it } from "vitest";

import { isReplayLocalFault } from "../src/block-replay.faults.js";
import {
  eventLedgerBefore,
  eventLedgerBeforeStep,
  EventLedgerOutputNotCanonical,
  EventLedgerReplayRefusal,
} from "../src/event-ledger.js";
import {
  ledgerBeforeStep,
  ledgerBeforeTraceStep,
} from "../src/event-ledger.ledger.js";
import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import {
  type CheckedLedger,
  checkLedgerAgainstRoot,
  ledgerEntriesRoot,
} from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type Ledger,
  ledgerAt,
  LedgerNodeMissing,
  LedgerOutputMissing,
} from "../src/ledger-store/index.js";
import { completeCanonicalReplayPredecessorLedger } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import { outRefCbor } from "./helpers/canonical-block-evidence-fixture.js";
import {
  block,
  NATIVE,
  OTHER,
  SPENT,
} from "./helpers/event-ledger-committed-steps.js";
import {
  admitted,
  ledgerSnapshot,
  type Outcome,
  outcome,
  viewSnapshot,
} from "./helpers/event-ledger-ledger-snapshots.js";
import {
  afresh,
  FAULTS,
  hex,
  outRef,
  type Request,
  requests,
  seeded,
  seededBlock,
  shuffled,
  withdrawalOf,
} from "./helpers/event-ledger-seeded-blocks.js";
import {
  inputChronologies,
  inputExistenceChronologyFixture,
} from "./helpers/input-existence-chronology-fixture.js";

/**
 * Compares, for every request, a fresh checked-ledger walk with: one ledger
 * store walk asked every request in drawn order, some concurrently, and a
 * fresh ledger store walk. Then, for every step, the trace-relative forms.
 * Returns the refusal reasons met.
 */
const compareWalks = async ({
  evidence,
  predecessor,
  probes,
  seed,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly predecessor: CheckedLedger | undefined;
  readonly probes: readonly Buffer[];
  readonly seed: number;
}) => {
  const ledger =
    predecessor === undefined ? undefined : await admitted(predecessor);
  const draw = seeded(seed);
  const asked = shuffled(draw, requests(evidence, draw));
  const asLedger = (walked: CanonicalBlockEvidence, request: Request) =>
    outcome(
      () =>
        ledgerBeforeStep({ block: walked, ...request, predecessor: ledger }),
      (found) => ledgerSnapshot(found, probes),
    );
  const concurrent = afresh(evidence);
  const shared = new Map<Request, Promise<Outcome>>();
  for (let start = 0; start < asked.length; ) {
    const batch = asked.slice(start, (start += 1 + draw(4)));
    const settled = batch.map((request) => asLedger(concurrent, request));
    for (const [index, request] of batch.entries())
      shared.set(request, settled[index]!);
    await Promise.all(settled);
  }
  const seen = new Set<string>();
  for (const request of asked) {
    const expected = await outcome(
      () =>
        eventLedgerBefore({ block: afresh(evidence), ...request, predecessor }),
      (view) => viewSnapshot(view, probes),
    );
    expect(await shared.get(request)).toStrictEqual(expected);
    expect(await asLedger(afresh(evidence), request)).toStrictEqual(expected);
    seen.add(
      "refused" in expected
        ? String(expected.refused.reason ?? expected.refused.message)
        : "ledger",
    );
  }
  const { reconstruction } = evidence;
  for (
    let step = 0n;
    step <= reconstruction.header.transitionStepCount;
    step += 1n
  )
    expect(
      await outcome(
        () => ledgerBeforeTraceStep(reconstruction, step, ledger),
        (found) => ledgerSnapshot(found, probes),
      ),
    ).toStrictEqual(
      await outcome(
        () =>
          eventLedgerBeforeStep(
            afresh(evidence).reconstruction,
            step,
            predecessor,
          ),
        (view) => viewSnapshot(view, probes),
      ),
    );
  return seen;
};

describe("ledger store walk against the checked-ledger walk", () => {
  it.each(FAULTS)("seeded blocks with %s", async (fault) => {
    const seen = new Set<string>();
    for (let seed = 1; seed <= 3; seed += 1) {
      const blockSeed = FAULTS.indexOf(fault) * 100 + seed;
      const fixture = await seededBlock(blockSeed, fault);
      for (const reason of await compareWalks({
        evidence: fixture.evidence,
        predecessor: completeCanonicalReplayPredecessorLedger(fixture),
        probes: fixture.probes,
        seed: blockSeed,
      }))
        seen.add(reason);
    }
    // Each fault's own refusals are met, so its comparisons are not vacuous.
    const met = {
      none: ["ledger", "selected_pre_root_changed", "step_out_of_range"],
      absent_input: ["transaction_input_absent"],
      repeated_input: ["transaction_input_absent"],
      rejected: ["ledger"],
      output_present: ["transaction_output_already_present"],
      foreign_post_root: ["post_root_changed"],
      foreign_pre_root: [
        "transition_link_changed",
        "selected_pre_root_changed",
      ],
      non_canonical_output: ["not a canonical ledger entry"],
    }[fault];
    for (const reason of met)
      expect([...seen].some((found) => found.includes(reason))).toBe(true);
  });

  it.each([
    ["forced_after_failed_normal", undefined, "transaction_input_absent"],
    ["withdrawal_consumed", withdrawalOf(0x51, true), "ledger"],
    ["withdrawal_untouched", withdrawalOf(0x51, true), "ledger"],
    [
      "withdrawal_untouched",
      withdrawalOf(0x5f, true),
      "withdrawal_input_absent",
    ],
    ["withdrawal_consumed", withdrawalOf(0x51, false), "ledger"],
    ...inputChronologies
      .filter((scenario) => !scenario.startsWith("withdrawal_"))
      .map((scenario) => [scenario, undefined, "ledger"] as const),
  ] as const)(
    "the %s chronology (withdrawal %#)",
    async (scenario, withdrawal, met) => {
      const fixture = await inputExistenceChronologyFixture(
        scenario,
        0,
        undefined,
        withdrawal?.committed,
      );
      const keys = [
        ...fixture.predecessorEntries.map(({ outRef: key }) => key),
        fixture.selected,
        outRefCbor(0x5f, 0n),
      ];
      const seen = await compareWalks({
        evidence: fixture.evidence,
        predecessor: fixture.predecessor,
        probes: [...new Map(keys.map((key) => [hex(key), key])).values()],
        seed: scenario.length * 7 + (withdrawal?.seed ?? 0),
      });
      expect(seen).toContain(met);
    },
  );
});

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
    const predecessor = await admitted(
      completeCanonicalReplayPredecessorLedger(fixture)!,
    );
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
      expect(ledger.root).toBe((await fixture.before(index)).utxosRoot);
    }
  });

  it("reads and proves a spent, a produced and an untouched out-ref as the checked view does", async () => {
    const fixture = await threeSteps();
    const keys = [SPENT.outRef, outRef(fixture.txs[0]!.txId, 0), OTHER.outRef];
    const ledger = await ledgerBeforeStep({
      block: fixture.evidence,
      eventKey: fixture.key(1),
      stepIndex: 1n,
      predecessor: await admitted(
        completeCanonicalReplayPredecessorLedger(fixture)!,
      ),
    });
    const view = await fixture.before(1);
    const read = await ledger.read(keys);
    expect(read.get(hex(keys[0]!))!.output).toBeUndefined();
    expect(read.get(hex(keys[1]!))!.output).toBeDefined();
    expect(await ledgerSnapshot(ledger, keys)).toStrictEqual(
      await viewSnapshot(view, keys),
    );
  });

  it("refuses an input a step spends twice, as the checked walk does", async () => {
    const fixture = await block([
      {
        input: { spendInputs: [SPENT.outRef, SPENT.outRef], fee: 1n },
        applied: true,
      },
      { input: { spendInputs: [OTHER.outRef], fee: 2n }, applied: true },
    ]);
    const request = { eventKey: fixture.key(1), stepIndex: 1n };
    const checked = completeCanonicalReplayPredecessorLedger(fixture)!;
    const expected = await outcome(
      () =>
        eventLedgerBefore({
          block: fixture.evidence,
          ...request,
          predecessor: checked,
        }),
      async () => "ledger",
    );
    expect(expected).toMatchObject({
      refused: { reason: "transaction_input_absent", step: 0n },
    });
    expect(
      await outcome(
        async () =>
          await ledgerBeforeStep({
            block: fixture.evidence,
            ...request,
            predecessor: await admitted(checked),
          }),
        async () => "ledger",
      ),
    ).toStrictEqual(expected);
  });
});

/** `checked` in a memory store, read through a source that counts the
 * predecessor nodes and outputs every walk reads. */
const countedPredecessor = async (checked: CheckedLedger) => {
  const storage = createMemoryLedgerStorage();
  await createLedgerStore(storage).admit(checked.entries, checked.utxosRoot);
  let reads = 0;
  const predecessor = ledgerAt(checked.utxosRoot, {
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
      completeCanonicalReplayPredecessorLedger(fixture)!,
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
      completeCanonicalReplayPredecessorLedger(fixture)!,
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
  ] as const)("refuses %s as the checked walk does", async (_, supplied) => {
    const fixture = await threeSteps();
    const checked = completeCanonicalReplayPredecessorLedger(fixture)!;
    // The predecessor without `NATIVE`: a ledger with another root.
    const other = await checkLedgerAgainstRoot({
      entries: checked.entries.slice(1),
      utxosRoot: await ledgerEntriesRoot(checked.entries.slice(1)),
      label: "other ledger",
    });
    const request = { eventKey: fixture.key(1), stepIndex: 1n };
    const expected = await outcome(
      () =>
        eventLedgerBefore({
          block: fixture.evidence,
          ...request,
          predecessor: supplied ? other : undefined,
        }),
      async () => "ledger",
    );
    expect(expected).toHaveProperty("refused");
    expect(
      await outcome(
        async () =>
          await ledgerBeforeStep({
            block: fixture.evidence,
            ...request,
            predecessor: supplied ? await admitted(other) : undefined,
          }),
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
    const checked = completeCanonicalReplayPredecessorLedger(fixture)!;
    const store = createLedgerStore(createMemoryLedgerStorage());
    const predecessor = await store.admit(checked.entries, checked.utxosRoot);
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
    const checked = completeCanonicalReplayPredecessorLedger(fixture)!;
    const store = createLedgerStore(createMemoryLedgerStorage());
    const predecessor = await store.admit(checked.entries, checked.utxosRoot);
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
    await store.admit(checked.entries, checked.utxosRoot);
    expect((await request(fixture, 2, predecessor)).root).toBe(
      traceStep(fixture.evidence, 1).post_utxos_root,
    );
  });

  it("is a local fault when a held entry's output is gone", async () => {
    const fixture = await threeSteps();
    const checked = completeCanonicalReplayPredecessorLedger(fixture)!;
    const storage = createMemoryLedgerStorage();
    await createLedgerStore(storage).admit(checked.entries, checked.utxosRoot);
    const predecessor = ledgerAt(checked.utxosRoot, {
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
