/** A thread of ours on a header removed against a record this run did not
 * create, as the orchestrator rows see it: the chain our L1 port reads, a
 * cancel port over it, and `makeAdapter`'s family with its header removed. */
import { expect, vi } from "vitest";

import type { FraudProofWorkflowJournalEntry } from "../src/workflow/journal.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowPreflight,
} from "../src/workflow/orchestrator.js";
import {
  type OwnThreadReclaimPort,
  withOwnThreadReclaim,
} from "../src/workflow/own-thread-reclaim.with-own-thread-reclaim.js";
import { FraudProofResolvedByAnotherRecordError } from "../src/workflow/resolved-by-another-record.js";
import {
  makeAdapter,
  REFERENCE_OUT_REF,
  REFERENCE_SCRIPT_HASH,
  RELEASE_FINALITY_POLICY,
  run,
} from "./workflow.make-adapter.js";

export const FINAL_DEPTH =
  RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth + 2;
export const OUR_THREAD = `${"d7".repeat(32)}#1`;
export const RECLAIM_TX = "d8".repeat(32);
export const THREAD_LOVELACE = "9000000";

export const resolution = {
  removalTxHash: "c5".repeat(32),
  referencedProofTokenOutRef: `${"c4".repeat(32)}#0`,
  confirmationDepth: FINAL_DEPTH,
};

export const kinds = (entries: readonly FraudProofWorkflowJournalEntry[]) =>
  entries.map(({ event }) => event.kind);

/** The chain as our L1 port reads it: our thread standing until its cancel
 * lands, then that cancel at its depth. */
export const chain = ({
  thread = OUR_THREAD,
  reclaimTx = RECLAIM_TX,
}: {
  readonly thread?: string;
  readonly reclaimTx?: string;
} = {}) => {
  const state: {
    standing: boolean;
    reclaimDepth: number | undefined;
  } = { standing: true, reclaimDepth: undefined };
  return {
    state,
    thread,
    reclaimTx,
    land: (depth: number) => {
      state.standing = false;
      state.reclaimDepth = depth;
    },
    threads: () => ({
      ...(state.standing
        ? {
            standing: {
              threadOutRef: thread,
              step: 2 as const,
              lovelace: THREAD_LOVELACE,
            },
          }
        : {}),
      spends:
        state.reclaimDepth === undefined
          ? []
          : [
              {
                txHash: reclaimTx,
                threadOutRef: thread,
                confirmationDepth: state.reclaimDepth,
              },
            ],
    }),
  };
};

export type Ledger = ReturnType<typeof chain>;

/** A cancel port over `ledger`, as one process instance holds it. */
export const reclaimPort = (
  ledger: Ledger,
  preflight: OwnThreadReclaimPort["preflight"] = async ({ actionId }) =>
    ({
      actionId,
      txHash: ledger.reclaimTx,
      scriptExecution: "reference_scripts",
      localUplcEvaluation: { status: "passed", evaluator: "uplc-v1" },
      referenceScripts: [
        {
          role: "family-step-cancel",
          outRef: REFERENCE_OUT_REF,
          scriptHash: REFERENCE_SCRIPT_HASH,
        },
      ],
    }) satisfies FraudProofWorkflowPreflight,
) => ({
  observe: vi.fn(async () => ledger.threads()),
  preflight: vi.fn(preflight),
  submit: vi.fn(
    async ({ preflight: built }: { readonly preflight: { txHash: string } }) =>
      built.txHash,
  ),
});

/** `makeAdapter`'s family, its header removed against another prover's
 * record while `removed()` says so, at the removal's depth `depth()`, with
 * our thread named as the stage derivation names it. */
export const removedByAnotherProver = (
  ledger: Ledger,
  removed: () => boolean = () => true,
  depth: () => number = () => FINAL_DEPTH,
): FraudProofFamilyWorkflowAdapter => {
  const adapter = makeAdapter();
  return {
    ...adapter,
    observe: async (context) => {
      if (!removed()) return await adapter.observe(context);
      const standing = ledger.threads().standing;
      const removal = { ...resolution, confirmationDepth: depth() };
      throw new FraudProofResolvedByAnotherRecordError(
        standing === undefined
          ? removal
          : { ...removal, standingThread: standing },
      );
    },
  };
};

export const decorate = (
  base: FraudProofFamilyWorkflowAdapter,
  reclaim: OwnThreadReclaimPort,
) =>
  withOwnThreadReclaim({
    category: "doubleSpend",
    base,
    reclaim,
    releaseFinality: { policy: RELEASE_FINALITY_POLICY },
  });

export const expectReclaimedThenResolved = (
  outcome: Awaited<ReturnType<typeof run>>,
  expected: typeof resolution,
  ledger?: Ledger,
) => {
  expect(outcome.kind).toBe("resolved_by_another_record");
  if (outcome.kind !== "resolved_by_another_record")
    throw new Error("not resolved");
  expect(outcome.resolution).toEqual(expected);
  const thread = ledger?.thread ?? OUR_THREAD;
  expect(outcome.reclaimed).toEqual([
    {
      actionId: `reclaim:${thread}`,
      txHash: ledger?.reclaimTx ?? RECLAIM_TX,
      threadOutRef: thread,
      lovelace: THREAD_LOVELACE,
    },
  ]);
  // A reclaim is never our conviction: no terminal, no completion.
  expect("terminal" in outcome).toBe(false);
  expect(kinds(outcome.entries)).not.toContain("completed");
  expect(kinds(outcome.entries)).not.toContain("terminal_included");
  return outcome;
};
