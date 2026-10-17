/**
 * Ledger entries, ledger operations, and the mutation steps that replay them against the trie.
 */

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardMpfDeletionOpening,
  buildMidgardMpfProofFoldTrace,
  computeHash32,
  type MidgardMpfProofFoldStep,
  type MidgardMpfProofFoldTrace,
  parseMidgardMpfProofJson,
} from "@al-ft/midgard-core";

import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "../ledger-output-descriptor.js";

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
  readonly proofFoldTrace: MidgardMpfProofFoldTrace;
  /** A deletion's terminal-Branch group opening; empty for an insertion. */
  readonly deletionOpening: Buffer;
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

const MPF_EMPTY_ROOT = Buffer.alloc(32);
const COMMITTED_EMPTY_LEDGER_ROOT = computeHash32(Buffer.alloc(0));

/**
 * The ledger root Midgard commits for the trie an MPF library root names; twin
 * of `mpf_proof_v1.committed_root`. MPF names the empty trie with 32 zero
 * bytes, while a Midgard header commits `blake2b256(empty)` for an empty
 * ledger. Every ledger root the validation machine carries, and so every root
 * a claim compares with a block's `utxos_root`, is in the committed encoding,
 * so an emptied ledger has exactly one name. Proof folds keep the library
 * encoding.
 */
export const committedLedgerRoot = (libraryRoot: Buffer): Buffer =>
  libraryRoot.equals(MPF_EMPTY_ROOT)
    ? Buffer.from(COMMITTED_EMPTY_LEDGER_ROOT)
    : Buffer.from(libraryRoot);

export const buildValidationMachineLedgerInsertOp = ({
  key,
  outputCbor,
}: {
  readonly key: Uint8Array;
  readonly outputCbor: Uint8Array;
}): Extract<ValidationMachineLedgerOp, { readonly type: "insert" }> => ({
  type: "insert",
  key: Buffer.from(key),
  value: buildCanonicalMidgardLedgerEntryOutputMaterial({
    outRef: key,
    outputCbor,
  }).descriptorCbor,
});

const createLedgerTrie = async (
  entries: readonly ValidationMachineLedgerEntry[],
): Promise<Trie> => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const entry of [...entries].sort((left, right) =>
    Buffer.compare(left.outRef, right.outRef),
  )) {
    await trie.insert(
      entry.outRef,
      buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef: entry.outRef,
        outputCbor: entry.output,
      }).descriptorCbor,
    );
  }
  return trie;
};

/** Reconstructs the header's descriptor root from complete raw ledger preimages. */
export const validationMachineLedgerRoot = async (
  entries: readonly ValidationMachineLedgerEntry[],
): Promise<Buffer> => {
  const trie = await createLedgerTrie(entries);
  return committedLedgerRoot(exactTrieRoot(trie));
};

export const buildValidationMachineLedgerMutationSteps = async (input: {
  readonly initialEntries: readonly ValidationMachineLedgerEntry[];
  readonly operations: readonly ValidationMachineLedgerOp[];
}): Promise<readonly ValidationMachineLedgerMutationStep[]> => {
  const trie = await createLedgerTrie(input.initialEntries);
  const steps: ValidationMachineLedgerMutationStep[] = [];
  for (const operation of input.operations) {
    steps.push(await applyValidationMachineLedgerMutationStep(trie, operation));
  }
  return steps;
};

export const applyValidationMachineLedgerMutationStep = async (
  trie: Trie,
  operation: ValidationMachineLedgerOp,
): Promise<ValidationMachineLedgerMutationStep> => {
  const libraryPreRoot = exactTrieRoot(trie);
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
  const steps = parseMidgardMpfProofJson(proof.toJSON());
  const deletionOpening =
    operation.type === "delete"
      ? await buildMidgardMpfDeletionOpening(trie, operation.key, steps)
      : Buffer.alloc(0);
  const proofFoldTrace = buildMidgardMpfProofFoldTrace({
    key: operation.key,
    value: mutationValue,
    steps,
    ...(operation.type === "delete" ? { deletionOpening } : {}),
  });
  if (operation.type === "delete") {
    await trie.delete(operation.key);
  } else {
    await trie.insert(operation.key, operation.value);
  }
  const libraryPostRoot = exactTrieRoot(trie);
  const foldPreRoot =
    operation.type === "delete"
      ? proofFoldTrace.terminal.includingRoot
      : proofFoldTrace.terminal.excludingRoot;
  const foldPostRoot =
    operation.type === "delete"
      ? proofFoldTrace.terminal.excludingRoot
      : proofFoldTrace.terminal.includingRoot;
  if (
    !foldPreRoot.equals(libraryPreRoot) ||
    !foldPostRoot.equals(libraryPostRoot)
  ) {
    throw new Error(
      "bounded MPF proof fold disagrees with the applied ledger mutation",
    );
  }
  return {
    operation,
    preRoot: committedLedgerRoot(libraryPreRoot),
    postRoot: committedLedgerRoot(libraryPostRoot),
    proofFoldTrace,
    deletionOpening,
  };
};

/** The `ledgerDeltaProofFrame` auxiliary for one fold step of `step`: a
 * deletion's terminal frame carries the group opening, every other frame an
 * empty one. */
export const ledgerDeltaProofFrameAuxiliary = (
  step: ValidationMachineLedgerMutationStep,
  foldStep: MidgardMpfProofFoldStep,
) => ({
  kind: "ledgerDeltaProofFrame" as const,
  frame: foldStep.frame,
  siblings: foldStep.membership.siblings,
  opening:
    step.operation.type === "delete" &&
    foldStep.frame.frameIndex === step.proofFoldTrace.frames.length - 1
      ? step.deletionOpening
      : Buffer.alloc(0),
});
