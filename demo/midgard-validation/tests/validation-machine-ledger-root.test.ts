import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  exactTrieRoot,
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

  it("keeps the MPF empty sentinel inside first-deposit insertion and last-entry deletion proofs", async () => {
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
    const populatedRoot = await validationMachineLedgerRoot([entry]);
    expect(insert!.preRoot).toEqual(Buffer.alloc(32));
    expect(insert!.postRoot).toEqual(populatedRoot);
    expect(insert!.proofFoldTrace.terminal).toMatchObject({
      excludingRoot: Buffer.alloc(32),
      includingRoot: populatedRoot,
    });
    expect(remove!.preRoot).toEqual(populatedRoot);
    expect(remove!.postRoot).toEqual(Buffer.alloc(32));
    expect(remove!.proofFoldTrace.terminal).toMatchObject({
      includingRoot: populatedRoot,
      excludingRoot: Buffer.alloc(32),
    });
    expect(await validationMachineLedgerRoot([])).toEqual(
      Buffer.from(EMPTY_MERKLE_TREE_ROOT, "hex"),
    );
  });
});
