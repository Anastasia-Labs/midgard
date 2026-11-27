/**
 * A validation-trace replay reads its event's pre-state ledger at the keys
 * the transaction names, and builds its mutation steps on that ledger's own
 * trie: the steps, and the trace committed over them, are byte for byte what
 * the whole pre-state ledger gives. A ledger read that answers a value the
 * root does not commit is refused, and a ledger no longer readable is this
 * process's own fault.
 */
import type { UTxOStatePatch } from "@al-ft/midgard-validation/phase-b";
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { mutationStepsOver } from "@al-ft/midgard-validation/tests/validation-ledger-trie";
import { canonicalTransitionEffectFromStatePatch } from "@al-ft/midgard-validation/transition-effect";
import {
  commitValidationTraceV1,
  ledgerEntriesPreState,
  transitionEffectToLedgerOps,
  type ValidationMachineReplayInput,
} from "@al-ft/midgard-validation/validation-machine";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { isReplayLocalFault } from "../src/block-replay.faults.js";
import { type LedgerEntry, ledgerEntryLeaf } from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type Ledger,
  ledgerAt,
  LedgerNodeMissing,
  LedgerOutputMissing,
  type StoredNode,
} from "../src/ledger-store/index.js";
import {
  acceptedLedgerMutation,
  plutusReplayInput,
  replayPlutusVerdict,
} from "../src/validation-trace-dispute/replay.js";
import { outRefCbor } from "./helpers/canonical-block-evidence-fixture.js";
import {
  depositSeed,
  plutusExecutionBlockFixture,
} from "./helpers/plutus-execution-block-fixture.js";
import { keyCredential } from "./helpers/plutus-execution-seeds.js";
import { oracleLedgerRoot } from "./support/ledger-root-oracle.js";
import {
  eventAndLedger,
  l1SlotConfig,
} from "./support/validation-trace-claim-material.js";

/** Every entry `ledger` holds, ascending by out-ref, each with the
 * descriptor its trie leaf holds: the whole-ledger reference. */
const wholeEntries = async (ledger: Ledger): Promise<LedgerEntry[]> => {
  const entries: LedgerEntry[] = [];
  for await (const entry of ledger.entries())
    entries.push(ledgerEntryLeaf(entry, "whole pre-state").entry);
  return entries.sort((left, right) =>
    Buffer.compare(left.outRef, right.outRef),
  );
};

/** `entries` in a memory store, read through `source`'s view of its nodes
 * and outputs. */
const stored = async (
  entries: readonly LedgerEntry[],
  source: {
    readonly node?: (node: StoredNode) => StoredNode | undefined;
    readonly output?: (output: Buffer) => Buffer | undefined;
  } = {},
): Promise<Ledger> => {
  const root = await oracleLedgerRoot(entries);
  const storage = createMemoryLedgerStorage();
  await createLedgerStore(storage).admit(entries, root);
  return ledgerAt(root, {
    node: async (hash) => {
      const node = (await storage.readNodes([hash]))[0];
      return node === undefined || source.node === undefined
        ? node
        : source.node(node);
    },
    outputs: async (outRefs) =>
      (await storage.readOutputs(outRefs)).map((output) =>
        output === undefined || source.output === undefined
          ? output
          : source.output(output),
      ),
  });
};

/** The steps the whole pre-state ledger's trie gives for `patch`. */
const wholeLedgerSteps = async (
  entries: readonly LedgerEntry[],
  patch: UTxOStatePatch,
) =>
  await mutationStepsOver({
    initialEntries: entries,
    operations: transitionEffectToLedgerOps(
      canonicalTransitionEffectFromStatePatch(patch),
    ),
  });

/** `input` over the whole pre-state ledger: every entry its witness, and no
 * pre-state port, so input resolution proves against a trie of them all. */
const wholeLedgerInput = (
  input: ValidationMachineReplayInput,
  entries: readonly LedgerEntry[],
): ValidationMachineReplayInput => {
  const { preStateLedger, ...rest } = input;
  return { ...rest, ledgerWitnessEntries: entries };
};

const committed = async (input: ValidationMachineReplayInput) =>
  await Effect.runPromise(commitValidationTraceV1(input));

const entry = (txIdByte: number, index: bigint, lovelace: bigint) => ({
  outRef: outRefCbor(txIdByte, index),
  output: makeOutput(lovelace),
});

const MANY = Array.from({ length: 96 }, (_, index) =>
  entry(0x10 + (index % 48), BigInt(index >> 4), 2_000_000n + BigInt(index)),
);

const DELETES_AND_INSERTS = {
  deletedOutRefs: [MANY[3]!, MANY[40]!, MANY[41]!, MANY[95]!].map(
    ({ outRef }) => outRef.toString("hex"),
  ),
  upsertedOutRefs: [
    entry(0xe0, 0n, 3_000_000n),
    entry(0xe0, 1n, 3_000_001n),
    entry(0x11, 7n, 3_000_002n),
  ].map(({ outRef, output }) => [outRef.toString("hex"), output] as const),
};

/** Rejects with a local fault caused by `cause`. */
const expectLocalFault = async (
  run: Promise<unknown>,
  cause: abstract new (...args: never[]) => Error,
) => {
  const error: unknown = await run.then(
    () => {
      throw new Error("the read succeeded");
    },
    (rejection: unknown) => rejection,
  );
  expect(isReplayLocalFault(error)).toBe(true);
  expect((error as { cause: unknown }).cause).toBeInstanceOf(cause);
};

describe("a validation-trace replay over its event pre-state", () => {
  it.each([
    [
      "deletes and inserts across a many-entry ledger",
      MANY,
      DELETES_AND_INSERTS,
    ],
    [
      "empties a single-entry ledger",
      [entry(0x20, 0n, 2_000_000n)],
      {
        deletedOutRefs: [outRefCbor(0x20, 0n).toString("hex")],
        upsertedOutRefs: [],
      },
    ],
    [
      "changes nothing",
      MANY.slice(0, 5),
      { deletedOutRefs: [], upsertedOutRefs: [] },
    ],
  ] as const)(
    "builds the mutation steps the whole ledger's trie builds when it %s",
    async (_, entries, patch) => {
      const steps = await wholeLedgerSteps(entries, patch);
      const mutation = await acceptedLedgerMutation(
        await stored(entries),
        patch,
      );
      expect(mutation.ledgerMutationSteps).toStrictEqual(steps);
      expect(mutation.postUtxosRoot).toBe(
        steps.at(-1)?.postRoot.toString("hex") ??
          (await oracleLedgerRoot(entries)),
      );
    },
  );

  it("refuses a mutation over a ledger read that answers a deleted entry with a value its root does not commit", async () => {
    const deleted = MANY[40]!.outRef.toString("hex");
    let altered = 0;
    const ledger = await stored(MANY, {
      node: (node) => {
        if (node.__kind !== "Leaf" || node.key !== deleted) return node;
        altered += 1;
        return { ...node, value: `${node.value.slice(0, -2)}00` };
      },
    });
    await expect(
      acceptedLedgerMutation(ledger, DELETES_AND_INSERTS),
    ).rejects.toThrow(
      "bounded MPF proof fold disagrees with the applied ledger mutation",
    );
    expect(altered).toBeGreaterThan(0);
  });

  it("names a mutation over a ledger whose nodes are no longer readable a local fault", async () => {
    await expectLocalFault(
      acceptedLedgerMutation(
        await stored(MANY, { node: () => undefined }),
        DELETES_AND_INSERTS,
      ),
      LedgerNodeMissing,
    );
  });

  it("builds an accepted event's mutation steps and trace as the whole ledger does", async () => {
    const block = await plutusExecutionBlockFixture("passing");
    const { event, ledger } = await eventAndLedger(block);
    const whole = await wholeEntries(ledger);
    const replay = {
      header: block.evidence.header,
      event,
      ledger,
      l1SlotConfig,
    } as const;
    const verdict = await replayPlutusVerdict(
      await plutusReplayInput({ ...replay, verdict: { verdict: "rejected" } }),
    );
    if (verdict.verdict !== "accepted") throw new Error("not accepted");
    const mutation = await acceptedLedgerMutation(ledger, verdict.statePatch);
    expect(mutation.ledgerMutationSteps).toStrictEqual(
      await wholeLedgerSteps(whole, verdict.statePatch),
    );
    const input = await plutusReplayInput({
      ...replay,
      verdict: { verdict: "accepted", mutation },
    });
    const trace = await committed(input);
    expect(trace.descriptor.verdict).toBe("accepted");
    expect(trace).toStrictEqual(
      await committed(wholeLedgerInput(input, whole)),
    );
  }, 300_000);

  it("commits a rejected event's trace as the whole ledger does", async () => {
    const block = await plutusExecutionBlockFixture("failing");
    const { event, ledger } = await eventAndLedger(block);
    const input = await plutusReplayInput({
      header: block.evidence.header,
      event,
      ledger,
      l1SlotConfig,
      verdict: { verdict: "rejected" },
    });
    const trace = await committed(input);
    expect(trace.descriptor.verdict).toBe("rejected");
    expect(trace).toStrictEqual(
      await committed(wholeLedgerInput(input, await wholeEntries(ledger))),
    );
  }, 300_000);

  it("reads every input a transaction spends or references, and proves them and commits its trace as the whole ledger does", async () => {
    // Three spent outputs, and the key-owned seed the fixture's predecessor
    // also holds as a reference input: every entry of the predecessor.
    const block = await plutusExecutionBlockFixture("failing", {
      passingSpends: 2,
      referenceInputs: [depositSeed(0x62, keyCredential()).outRef],
    });
    const { event, ledger } = await eventAndLedger(block);
    const whole = await wholeEntries(ledger);
    expect(whole).toHaveLength(4);
    const input = await plutusReplayInput({
      header: block.evidence.header,
      event,
      ledger,
      l1SlotConfig,
      verdict: { verdict: "rejected" },
    });
    expect(
      [...input.ledgerWitnessEntries].sort((left, right) =>
        Buffer.compare(left.outRef, right.outRef),
      ),
    ).toStrictEqual(whole);
    // Each held key and one the ledger does not hold, proved together.
    const keys = [...whole.map(({ outRef }) => outRef), outRefCbor(0x77, 0n)];
    const descriptors = new Map(
      whole.map((held) => [
        held.outRef.toString("hex"),
        ledgerEntryLeaf(held, "whole pre-state").leaf,
      ]),
    );
    expect(await input.preStateLedger!.proveResolution(keys)).toStrictEqual(
      await ledgerEntriesPreState(whole, descriptors).proveResolution(keys),
    );
    const trace = await committed(input);
    expect(trace.descriptor.verdict).toBe("rejected");
    expect(trace).toStrictEqual(
      await committed(wholeLedgerInput(input, whole)),
    );
  }, 300_000);

  it("names a replay input over a pre-state no longer readable a local fault", async () => {
    const block = await plutusExecutionBlockFixture("failing");
    const { event, ledger } = await eventAndLedger(block);
    const whole = await wholeEntries(ledger);
    let nodes = true;
    const input = (preState: Ledger) =>
      plutusReplayInput({
        header: block.evidence.header,
        event,
        ledger: preState,
        l1SlotConfig,
        verdict: { verdict: "rejected" },
      });
    await expectLocalFault(
      input(await stored(whole, { output: () => undefined })),
      LedgerOutputMissing,
    );
    const readable = await input(
      await stored(whole, { node: (node) => (nodes ? node : undefined) }),
    );
    const resolved = readable.ledgerWitnessEntries.map(({ outRef }) => outRef);
    expect(resolved.length).toBeGreaterThan(0);
    nodes = false;
    await expectLocalFault(
      readable.preStateLedger!.proveResolution(resolved),
      LedgerNodeMissing,
    );
    await expectLocalFault(
      input(await stored(whole, { node: () => undefined })),
      LedgerNodeMissing,
    );
  }, 300_000);
});
