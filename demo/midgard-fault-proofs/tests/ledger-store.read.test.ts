/**
 * The read-side helpers: `ledgerAt`, `ledgerTrie`, `ledgerProof`. Expected
 * proof bytes and mutation steps come from whole-trie functions
 * (`keyValuePhasProof`, `keyValuePhasNonMembershipProof`, and the
 * validation test support's `mutationStepsOver`), which build a fresh trie
 * from the whole entry set and never read the store; proofs are also folded by
 * midgard-core's MPF proof fold, the TypeScript twin of the on-chain verifier.
 */
import {
  buildMidgardMpfProofFoldTrace,
  type MidgardMpfProofStep,
} from "@al-ft/midgard-core/mpf-proof-fold";
import * as SDK from "@al-ft/midgard-sdk/core";
import { mutationStepsOver } from "@al-ft/midgard-validation/tests/validation-ledger-trie";
import {
  applyValidationLedgerMutations,
  buildValidationMachineLedgerInsertOp,
  type ValidationMachineLedgerOp,
} from "@al-ft/midgard-validation/validation-machine";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { type LedgerEntry, ledgerEntryLeaf } from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  EMPTY_LEDGER_ROOT,
  type Ledger,
  ledgerAt,
  LedgerNodeMissing,
  ledgerProof,
  type LedgerSource,
  type LedgerStorage,
  ledgerTrie,
} from "../src/ledger-store/index.js";
import {
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../src/transition-trace/phas.js";
import {
  event,
  expectLedgerMatches,
  storedNodesOf,
  testLedgerEntries,
  testOutRef,
} from "./support/ledger-store-conformance.js";

const proofHex = (proof: SDK.Proof): string => Data.to(proof, SDK.Proof);

const leafOf = (entry: LedgerEntry): Buffer =>
  ledgerEntryLeaf(entry, "reference").leaf;

/** Today's trie root object over `entries`. */
const referenceRoot = async (entries: readonly LedgerEntry[]) =>
  await keyValuePhasRootWithCount(
    entries.map((entry) => ({ key: entry.outRef, value: leafOf(entry) })),
  );

/** The ledger of `entries` built by `apply` on the empty ledger, persisted. */
const persistedLedger = async (entries: readonly LedgerEntry[]) => {
  const storage = createMemoryLedgerStorage();
  const store = createLedgerStore(storage);
  const built = await (
    await store.at(EMPTY_LEDGER_ROOT)
  ).apply(entries.map((entry) => ({ type: "insert" as const, ...entry })));
  await store.persist(built);
  return { storage, store, ledger: await store.at(built.root) };
};

const sourceOver = (storage: LedgerStorage): LedgerSource => ({
  node: async (hash) => (await storage.readNodes([hash]))[0],
  outputs: async (outRefs) => await storage.readOutputs(outRefs),
});

/** The steps of an SDK proof, as midgard-core's fold reads them. */
const foldSteps = (proof: SDK.Proof): MidgardMpfProofStep[] =>
  proof.map((step) => {
    if ("Branch" in step)
      return {
        kind: "branch",
        skip: Number(step.Branch.skip),
        neighbors: Buffer.from(step.Branch.neighbors, "hex"),
      };
    if ("Fork" in step)
      return {
        kind: "fork",
        skip: Number(step.Fork.skip),
        neighbor: {
          nibble: Number(step.Fork.neighbor.nibble),
          prefix: Buffer.from(step.Fork.neighbor.prefix, "hex"),
          root: Buffer.from(step.Fork.neighbor.root, "hex"),
        },
      } as MidgardMpfProofStep;
    return {
      kind: "leaf",
      skip: Number(step.Leaf.skip),
      key: Buffer.from(step.Leaf.key, "hex"),
      value: Buffer.from(step.Leaf.value, "hex"),
    } as MidgardMpfProofStep;
  });

/** The ledger root `proof` opens for `key`: with `value` when it proves
 * membership, without the key when it proves absence. */
const foldedRoot = (
  proof: SDK.Proof,
  key: Buffer,
  value: Buffer | undefined,
): string => {
  const { terminal } = buildMidgardMpfProofFoldTrace({
    key,
    value: value ?? Buffer.alloc(0),
    steps: foldSteps(proof),
  });
  const root = Buffer.from(
    value === undefined ? terminal.excludingRoot : terminal.includingRoot,
  ).toString("hex");
  return root === "00".repeat(32) ? EMPTY_LEDGER_ROOT : root;
};

const ABSENT = [testOutRef(900_001), testOutRef(900_002)];

describe("ledgerProof", () => {
  it.each([0, 1, 2, 1_000, 10_000])(
    "gives today's proof bytes at %i entries, and each proof opens the ledger's root",
    async (size) => {
      const entries = testLedgerEntries(0, size);
      const { ledger } = await persistedLedger(entries);
      const reference = await referenceRoot(entries);
      expect(ledger.root).toBe(reference.root);
      const members = [0, 1, size >> 1, size - 1]
        .filter((index) => index >= 0 && index < size)
        .map((index) => entries[index]!);
      for (const entry of members) {
        const proof = await ledgerProof(ledger, entry.outRef);
        expect(proofHex(proof)).toBe(
          proofHex(
            await keyValuePhasProof(reference, entry.outRef, leafOf(entry)),
          ),
        );
        expect(foldedRoot(proof, entry.outRef, leafOf(entry))).toBe(
          ledger.root,
        );
      }
      for (const key of ABSENT) {
        const proof = await ledgerProof(ledger, key);
        expect(proofHex(proof)).toBe(
          proofHex(await keyValuePhasNonMembershipProof(reference, key)),
        );
        expect(foldedRoot(proof, key, undefined)).toBe(ledger.root);
      }
    },
    120_000,
  );

  it("proves absence of a key the ledger's own overlay deleted", async () => {
    const entries = testLedgerEntries(0, 40);
    const { ledger } = await persistedLedger(entries);
    const deleted = entries[7]!;
    const after = await ledger.apply(event([7], []));
    const reference = await referenceRoot(
      entries.filter((entry) => entry !== deleted),
    );
    const proof = await ledgerProof(after, deleted.outRef);
    expect(proofHex(proof)).toBe(
      proofHex(await keyValuePhasNonMembershipProof(reference, deleted.outRef)),
    );
    expect(foldedRoot(proof, deleted.outRef, undefined)).toBe(after.root);
    // The ledger it was applied to still proves membership.
    expect(
      foldedRoot(
        await ledgerProof(ledger, deleted.outRef),
        deleted.outRef,
        leafOf(deleted),
      ),
    ).toBe(ledger.root);
  });
});

describe("ledgerTrie", () => {
  it("gives the whole-trie mutation steps when handed to applyValidationLedgerMutations", async () => {
    const entries = testLedgerEntries(0, 40);
    const { ledger } = await persistedLedger(entries);
    const insert = (entry: LedgerEntry): ValidationMachineLedgerOp =>
      buildValidationMachineLedgerInsertOp({
        key: entry.outRef,
        outputCbor: entry.output,
      });
    const operations: ValidationMachineLedgerOp[] = [
      { type: "delete", key: entries[0]!.outRef },
      ...testLedgerEntries(100, 3).map(insert),
      { type: "delete", key: entries[1]!.outRef },
      { type: "delete", key: testOutRef(101) },
      insert(entries[0]!),
    ];
    const expected = await mutationStepsOver({
      initialEntries: entries,
      operations,
    });
    const steps = await applyValidationLedgerMutations(
      await ledgerTrie(ledger),
      operations,
    );
    expect(steps).toEqual(expected);
    // The trie's edits stayed in the trie.
    await expectLedgerMatches(ledger, entries);
  });
});

describe("ledgerAt", () => {
  it("reads a ledger over a node and output source, and refuses a node the source lacks", async () => {
    const entries = testLedgerEntries(0, 40);
    const { storage, store, ledger } = await persistedLedger(entries);
    const source = sourceOver(storage);
    const read: Ledger = ledgerAt(ledger.root, source);
    await expectLedgerMatches(read, entries);
    await expect(store.persist(read)).rejects.toThrow(
      /does not belong to this store/,
    );

    const target = entries[11]!.outRef.toString("hex");
    const [missing] = [...(await storedNodesOf(storage, ledger.root))].find(
      ([, node]) => node.__kind === "Leaf" && node.key === target,
    )!;
    const lacking = ledgerAt(ledger.root, {
      ...source,
      node: async (hash) =>
        hash === missing ? undefined : await source.node(hash),
    });
    const refusal = await lacking
      .read([entries[11]!.outRef])
      .catch((error: unknown) => error);
    expect(refusal).toBeInstanceOf(LedgerNodeMissing);
    expect((refusal as LedgerNodeMissing).hash).toBe(missing);
  });
});
