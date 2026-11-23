/**
 * The inputs a direct script derivation resolves are the transaction's spent
 * and referenced inputs in the ledger before its own step: an earlier step's
 * spend removes one and its output adds one, and each held input carries the
 * output, leaf and membership proof that ledger has for it. A store that no
 * longer holds that ledger is this process's local fault.
 */
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { describe, expect, it } from "vitest";

import { isReplayLocalFault } from "../src/block-replay.faults.js";
import { checkedLedgerAsLedger } from "../src/checked-ledger-store.js";
import { prepareDirectScriptMaterial } from "../src/direct-script-derivation/material.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  LedgerNodeMissing,
} from "../src/ledger-store/index.js";
import { keyValuePhasProof } from "../src/transition-trace/phas.js";
import { completeCanonicalReplayPredecessorLedger } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import {
  buildFixtureTransaction,
  type FixtureTransactionInput,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import {
  block,
  NATIVE,
  OTHER,
  SPENT,
} from "./helpers/event-ledger-committed-steps.js";
import { referenceLedgerBefore } from "./helpers/event-ledger-reference.js";
import { outRef } from "./helpers/event-ledger-seeded-blocks.js";

type Prepare = () => ReturnType<typeof prepareDirectScriptMaterial>;

/** Step 0 spends `SPENT` and creates the output, at index 1, that step 1
 * spends with `OTHER`; step 1 also references `NATIVE`, the spent `SPENT`
 * and an out-ref no ledger holds. The held inputs differ in output index, so
 * each leaf is its own input's. */
const twoSteps = async () => {
  const creating: FixtureTransactionInput = {
    spendInputs: [SPENT.outRef],
    outputs: [makeOutput(1_000_000n), makeOutput(999_993n)],
    fee: 7n,
  };
  const created = outRef(buildFixtureTransaction(creating).txId, 1);
  const spent = [OTHER.outRef, created].sort(Buffer.compare);
  const referenced = [NATIVE.outRef, SPENT.outRef, outRefCbor(0xaf, 0n)].sort(
    Buffer.compare,
  );
  const fixture = await block([
    { input: creating, applied: true },
    {
      input: {
        spendInputs: spent,
        referenceInputs: referenced,
        outputs: [makeOutput(3_999_986n)],
        fee: 7n,
      },
      applied: true,
    },
  ]);
  return { fixture, created, spent, referenced };
};

describe("a direct script derivation's resolved inputs", () => {
  it("are the transaction's inputs as the ledger before its step holds them", async () => {
    const { fixture, created, spent, referenced } = await twoSteps();
    const predecessor = completeCanonicalReplayPredecessorLedger(fixture)!;
    const before = await referenceLedgerBefore({
      block: fixture.evidence,
      predecessor,
      eventKey: fixture.key(1),
      stepIndex: 1n,
    });
    expect(before.utxosRoot).not.toBe(predecessor.utxosRoot);
    const material = await prepareDirectScriptMaterial({
      current: fixture.evidence.reconstruction,
      predecessor: await checkedLedgerAsLedger(predecessor),
      eventKey: fixture.key(1),
    });
    expect(material.bound.event_pre_root).toBe(before.utxosRoot);
    const held = [...spent, ...referenced].filter((key) =>
      before.outputs.has(key.toString("hex")),
    );
    // `SPENT`, which the block starts from, is gone; `created` is new.
    expect(held.map((key) => key.toString("hex")).sort()).toEqual(
      [OTHER.outRef, created, NATIVE.outRef]
        .map((key) => key.toString("hex"))
        .sort(),
    );
    expect(
      new Map(
        await Promise.all(
          held.map(
            async (key) =>
              [
                key.toString("hex"),
                {
                  ...material.resolvedInputs.byKey.get(key.toString("hex")),
                  proof: await material.resolvedInputs.proof(key),
                },
              ] as const,
          ),
        ),
      ),
    ).toStrictEqual(
      new Map(
        await Promise.all(
          held.map(async (key) => {
            const { value } = before.trie.entries.find((entry) =>
              entry.key.equals(key),
            )!;
            return [
              key.toString("hex"),
              {
                output: before.outputs.get(key.toString("hex"))!,
                leaf: value,
                proof: await keyValuePhasProof(before.trie, key, value),
              },
            ] as const;
          }),
        ),
      ),
    );
    expect(material.resolvedInputs.byKey.size).toBe(held.length);
    await expect(material.resolvedInputs.proof(SPENT.outRef)).rejects.toThrow(
      "proof of an input the ledger does not hold",
    );
  });

  it.each([
    [
      "reading them",
      async (dropLedger: () => Promise<void>, prepare: Prepare) => {
        await dropLedger();
        await prepare();
      },
    ],
    [
      "proving one",
      async (dropLedger: () => Promise<void>, prepare: Prepare) => {
        const material = await prepare();
        await dropLedger();
        await material.resolvedInputs.proof(SPENT.outRef);
      },
    ],
  ] as const)(
    "are this process's local fault %s when the store no longer holds the ledger",
    async (_, run) => {
      // Step 0 resolves its inputs in the predecessor itself, which `retain`
      // of nothing deletes.
      const { fixture } = await twoSteps();
      const predecessor = completeCanonicalReplayPredecessorLedger(fixture)!;
      const store = createLedgerStore(createMemoryLedgerStorage());
      const ledger = await store.admit(
        predecessor.entries,
        predecessor.utxosRoot,
      );
      const error: unknown = await run(
        async () => {
          await store.retain(new Set());
        },
        async () =>
          await prepareDirectScriptMaterial({
            current: fixture.evidence.reconstruction,
            predecessor: ledger,
            eventKey: fixture.key(0),
          }),
      ).catch((cause: unknown) => cause);
      expect(isReplayLocalFault(error)).toBe(true);
      expect((error as { cause: unknown }).cause).toBeInstanceOf(
        LedgerNodeMissing,
      );
    },
  );
});
