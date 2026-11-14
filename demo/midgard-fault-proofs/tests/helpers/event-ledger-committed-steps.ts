/**
 * Committed blocks of normal transactions over a three-entry predecessor
 * ledger, for tests of what the event-ledger walk's refusals mean to the
 * families that read it: `NATIVE` holds a native script that does not
 * decode, `SPENT` and `OTHER` are plain key-owned outputs.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";

import { eventLedgerBefore } from "../../src/event-ledger.js";
import { ledgerEntriesRoot, type LedgerEntry } from "../../src/ledger-at.js";
import { classifyCanonicalBlockViolations } from "../../src/workflow/classification.js";
import {
  type CompleteCanonicalReplay,
  completeCanonicalReplayPredecessorLedger,
  createCompleteCanonicalReplayUnion,
} from "../../src/workflow/complete-replay.js";
import { CanonicalReplayPrerequisiteError } from "../../src/workflow/replay-prerequisite.js";
import {
  buildFixtureTransaction,
  type FixtureTransactionInput,
  outRefCbor,
} from "./canonical-block-evidence-fixture.js";
import { committedL2BlockFixture } from "./committed-l2-block-fixture.js";
import {
  hex,
  outRef,
  undecodableNativeOutput,
} from "./event-ledger-seeded-blocks.js";

export const NATIVE: LedgerEntry = {
  outRef: outRefCbor(0xa1, 0n),
  output: undecodableNativeOutput,
};
export const SPENT: LedgerEntry = {
  outRef: outRefCbor(0xa2, 0n),
  output: makeOutput(2_000_000n),
};
export const OTHER: LedgerEntry = {
  outRef: outRefCbor(0xa3, 0n),
  output: makeOutput(3_000_000n),
};

export type Step = Readonly<{
  input: FixtureTransactionInput;
  /** Whether the operator's committed root applies the step's effects. */
  applied: boolean;
  /** The operator's committed pre root, when it is not the root the earlier
   * steps leave. */
  pre?: string;
}>;

/** The steps over `predecessor`, committed with the operator's roots. */
export const block = async (
  steps: readonly Step[],
  predecessor: readonly LedgerEntry[] = [NATIVE, SPENT, OTHER],
) => {
  const txs = steps.map(({ input }) => buildFixtureTransaction(input));
  const ledger = new Map(
    predecessor.map((entry) => [hex(entry.outRef), entry]),
  );
  const roots = [await ledgerEntriesRoot(predecessor)];
  for (const [index, { input, applied }] of steps.entries()) {
    if (applied) {
      for (const key of input.spendInputs) ledger.delete(hex(key));
      for (const [outputIndex, output] of (input.outputs ?? []).entries()) {
        const key = outRef(txs[index]!.txId, outputIndex);
        ledger.set(hex(key), { outRef: key, output });
      }
    }
    roots.push(await ledgerEntriesRoot([...ledger.values()]));
  }
  const fixture = await committedL2BlockFixture({
    predecessorEntries: predecessor,
    txs,
    steps: txs.map((_, index) => ({
      pre: steps[index]!.pre ?? roots[index]!,
      post: roots[index + 1]!,
    })),
  });
  const key = (index: number): SDK.EventKey => ({
    L2TransactionEventKey: { tx_id: txs[index]!.txId },
  });
  return {
    ...fixture,
    txs,
    key,
    /** Step `index`'s transactions-root position, which findings name. */
    position: (index: number) =>
      BigInt(
        fixture.evidence.reconstruction.transactions.findIndex(
          ({ txId }) => txId === txs[index]!.txId,
        ),
      ),
    /** The block's walk's ledger before step `index`. */
    before: async (index: number) =>
      await eventLedgerBefore({
        block: fixture.evidence,
        eventKey: key(index),
        stepIndex: BigInt(index),
        predecessor: completeCanonicalReplayPredecessorLedger(fixture)!,
      }),
    replay: async (members: readonly CompleteCanonicalReplay[]) => {
      const replayer =
        members.length === 1
          ? members[0]!
          : createCompleteCanonicalReplayUnion(members);
      const { detections } = await replayer.replay(
        fixture.evidence,
        fixture.context,
      );
      return {
        detections,
        classification: await classifyCanonicalBlockViolations({
          evidence: fixture.evidence,
          detections,
          minimumConfirmationDepth: 1,
        }),
      };
    },
  };
};

/** The distinct prerequisites a replay leaves undischarged: each candidate
 * of a refused event names its own. */
export const prerequisites = async (run: () => Promise<unknown>) => {
  const error = await run().then(
    () => undefined,
    (cause: unknown) => cause,
  );
  if (!(error instanceof CanonicalReplayPrerequisiteError))
    throw new Error(`expected a prerequisite, got ${String(error)}`);
  const distinct = new Map(
    error.failures.map(({ eventKeyCbor, prerequisite }) => [
      `${eventKeyCbor}:${prerequisite}`,
      { eventKey: Data.from(eventKeyCbor, SDK.EventKey), prerequisite },
    ]),
  );
  return [...distinct.values()];
};
