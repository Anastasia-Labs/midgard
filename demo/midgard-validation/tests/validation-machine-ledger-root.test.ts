import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  committedLedgerRoot,
  exactTrieRoot,
  ledgerDeltaProofFrameAuxiliary,
  validationMachineLedgerRoot,
} from "../src/validation-machine/ledger-mutation.js";
import { makeOutput, outRefFromByte } from "./validation-fixtures.js";

describe("validation ledger roots at the header boundary", () => {
  it("uses the protocol genesis root for an empty ledger and an empty mutation", async () => {
    const store = new Store(undefined);
    await store.ready();
    const trie = new Trie(store);
    expect(exactTrieRoot(trie)).toEqual(Buffer.alloc(32));
    expect(await validationMachineLedgerRoot([])).toEqual(
      Buffer.from(EMPTY_MERKLE_TREE_ROOT, "hex"),
    );
    expect(await validationMachineLedgerRoot([])).not.toEqual(
      exactTrieRoot(trie),
    );
    expect(
      await buildValidationMachineLedgerMutationSteps({
        initialEntries: [],
        operations: [],
      }),
    ).toEqual([]);
  });

  it("names the empty ledger by the committed root at both ends of a mutation, and by the MPF sentinel inside its proof fold", async () => {
    const entry = {
      outRef: outRefFromByte(0x41),
      output: makeOutput(10_000_000n),
    };
    const insertion = buildValidationMachineLedgerInsertOp({
      key: entry.outRef,
      outputCbor: entry.output,
    });
    const steps = await buildValidationMachineLedgerMutationSteps({
      initialEntries: [],
      operations: [insertion, { type: "delete", key: entry.outRef }],
    });
    expect(steps).toHaveLength(2);
    const [insert, remove] = steps;
    const emptyLedgerRoot = Buffer.from(EMPTY_MERKLE_TREE_ROOT, "hex");
    const populatedRoot = await validationMachineLedgerRoot([entry]);
    expect(insert!.preRoot).toEqual(emptyLedgerRoot);
    expect(insert!.postRoot).toEqual(populatedRoot);
    expect(insert!.proofFoldTrace.terminal).toMatchObject({
      excludingRoot: Buffer.alloc(32),
      includingRoot: populatedRoot,
    });
    expect(remove!.preRoot).toEqual(populatedRoot);
    expect(remove!.postRoot).toEqual(emptyLedgerRoot);
    expect(remove!.proofFoldTrace.terminal).toMatchObject({
      includingRoot: populatedRoot,
      excludingRoot: Buffer.alloc(32),
    });
    expect(await validationMachineLedgerRoot([])).toEqual(emptyLedgerRoot);
  });

  it("translates only the MPF empty sentinel into the committed empty-ledger root", () => {
    const emptyLedgerRoot = Buffer.from(EMPTY_MERKLE_TREE_ROOT, "hex");
    expect(committedLedgerRoot(Buffer.alloc(32))).toEqual(emptyLedgerRoot);
    expect(committedLedgerRoot(emptyLedgerRoot)).toEqual(emptyLedgerRoot);
    const populated = Buffer.alloc(32, 0x5a);
    expect(committedLedgerRoot(populated)).toEqual(populated);
  });

  it("carries a deletion's group opening on its terminal proof frame only", async () => {
    const entries = Array.from({ length: 48 }, (_, index) => ({
      outRef: outRefFromByte(index + 1),
      output: makeOutput(10_000_000n),
    }));
    const insertion = buildValidationMachineLedgerInsertOp({
      key: outRefFromByte(0x80),
      outputCbor: makeOutput(10_000_000n),
    });
    const steps = await buildValidationMachineLedgerMutationSteps({
      initialEntries: entries,
      operations: [
        insertion,
        ...entries.map(({ outRef }) => ({
          type: "delete" as const,
          key: outRef,
        })),
      ],
    });
    let opened = 0;
    for (const step of steps) {
      const frames = step.proofFoldTrace.steps.map((foldStep) =>
        ledgerDeltaProofFrameAuxiliary(step, foldStep),
      );
      const terminal = step.proofFoldTrace.frames.at(-1);
      const carried = frames.filter(({ opening }) => opening.length > 0);
      if (step.operation.type === "insert") {
        expect(step.deletionOpening).toHaveLength(0);
        expect(carried).toHaveLength(0);
        continue;
      }
      if (step.deletionOpening.length === 0) {
        expect(carried).toHaveLength(0);
        continue;
      }
      opened += 1;
      expect(terminal?.step.kind).toBe("branch");
      expect(carried).toHaveLength(1);
      expect(carried[0]!.frame.frameIndex).toBe(
        step.proofFoldTrace.frames.length - 1,
      );
      expect(carried[0]!.opening).toEqual(step.deletionOpening);
    }
    expect(opened).toBeGreaterThan(0);
  });
});
