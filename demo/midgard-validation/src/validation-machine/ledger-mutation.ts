/**
 * Ledger entries, ledger operations, and the mutation steps that replay them against the trie.
 */

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardMpfProofFoldTraceV1,
  type MidgardMpfProofFoldTraceV1,
  parseMidgardMpfProofJsonV1,
} from "@al-ft/midgard-core";

import { buildCanonicalMidgardLedgerEntryOutputMaterialV1 } from "../ledger-output-descriptor.js";

export type ValidationMachineLedgerEntry = {
  readonly outRef: Buffer;
  readonly output: Buffer;
};

export type ValidationMachineLedgerOp =
  | { readonly type: "delete"; readonly key: Buffer }
  /** Insert values are exact canonical Midgard ledger output descriptors. */
  | { readonly type: "insert"; readonly key: Buffer; readonly value: Buffer };

export type ValidationMachineLedgerMutationStep = {
  readonly operation: ValidationMachineLedgerOp;
  readonly preRoot: Buffer;
  readonly postRoot: Buffer;
  /** Canonical bounded-frame form consumed by the deployed resolver chain. */
  readonly proofFoldTrace: MidgardMpfProofFoldTraceV1;
};

export type ValidationMachineValueMutationStep = {
  readonly unit: Buffer;
  readonly quantityDelta: bigint;
  readonly oldDelta: bigint | null;
  readonly preAssetRoot: Buffer;
  readonly postAssetRoot: Buffer;
  /** Membership/non-membership witness for unit against preAssetRoot. */
  readonly proofCbor: Buffer;
  readonly postSeenAssetCount: number;
  readonly postNonzeroAssetCount: number;
};

export const exactTrieRoot = (trie: Trie): Buffer =>
  trie.hash == null ? Buffer.alloc(32) : Buffer.from(trie.hash);

export const buildValidationMachineLedgerInsertOpV1 = ({
  key,
  outputCbor,
}: {
  readonly key: Uint8Array;
  readonly outputCbor: Uint8Array;
}): Extract<ValidationMachineLedgerOp, { readonly type: "insert" }> => ({
  type: "insert",
  key: Buffer.from(key),
  value: buildCanonicalMidgardLedgerEntryOutputMaterialV1({
    outRef: key,
    outputCbor,
  }).descriptorCbor,
});

export const buildValidationMachineLedgerMutationSteps = async (input: {
  readonly initialEntries: readonly ValidationMachineLedgerEntry[];
  readonly operations: readonly ValidationMachineLedgerOp[];
}): Promise<readonly ValidationMachineLedgerMutationStep[]> => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const entry of [...input.initialEntries].sort((left, right) =>
    Buffer.compare(left.outRef, right.outRef),
  )) {
    await trie.insert(
      entry.outRef,
      buildCanonicalMidgardLedgerEntryOutputMaterialV1({
        outRef: entry.outRef,
        outputCbor: entry.output,
      }).descriptorCbor,
    );
  }
  const steps: ValidationMachineLedgerMutationStep[] = [];
  for (const operation of input.operations) {
    steps.push(
      await applyValidationMachineLedgerMutationStepV1(trie, operation),
    );
  }
  return steps;
};

export const applyValidationMachineLedgerMutationStepV1 = async (
  trie: Trie,
  operation: ValidationMachineLedgerOp,
): Promise<ValidationMachineLedgerMutationStep> => {
  const preRoot = exactTrieRoot(trie);
  const mutationValue =
    operation.type === "insert"
      ? Buffer.from(operation.value)
      : await trie.get(operation.key);
  if (mutationValue === undefined) {
    throw new Error(
      "cannot construct a ledger deletion proof for an absent key",
    );
  }
  const proof = await trie.prove(operation.key, operation.type === "insert");
  const proofFoldTrace = buildMidgardMpfProofFoldTraceV1({
    key: operation.key,
    value: mutationValue,
    steps: parseMidgardMpfProofJsonV1(proof.toJSON()),
  });
  if (operation.type === "delete") {
    await trie.delete(operation.key);
  } else {
    await trie.insert(operation.key, operation.value);
  }
  const postRoot = exactTrieRoot(trie);
  const foldPreRoot =
    operation.type === "delete"
      ? proofFoldTrace.terminal.includingRoot
      : proofFoldTrace.terminal.excludingRoot;
  const foldPostRoot =
    operation.type === "delete"
      ? proofFoldTrace.terminal.excludingRoot
      : proofFoldTrace.terminal.includingRoot;
  if (!foldPreRoot.equals(preRoot) || !foldPostRoot.equals(postRoot)) {
    throw new Error(
      "bounded MPF proof fold disagrees with the applied ledger mutation",
    );
  }
  return {
    operation,
    preRoot,
    postRoot,
    proofFoldTrace,
  };
};
