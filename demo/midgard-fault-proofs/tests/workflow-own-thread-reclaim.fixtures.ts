/** A thread of ours on a header removed against a record this run did not
 * create, as the orchestrator rows see it: the chain our L1 port reads, a
 * cancel port over it, and `makeAdapter`'s family with its header removed. */
import { expect, vi } from "vitest";

import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowPreflight,
} from "../src/workflow/orchestrator.js";
import {
  type OwnThreadReclaimPort,
  withOwnThreadReclaim,
} from "../src/workflow/own-thread-reclaim.with-own-thread-reclaim.js";
import { FraudProofResolvedByAnotherRecordError } from "../src/workflow/resolved-by-another-record.js";
import { bindWorkflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { signedWorkflowTransactionFixture } from "./support/signed-workflow-transaction.js";
import {
  createFixtureChain,
  makeAdapter,
  REFERENCE_OUT_REF,
  REFERENCE_SCRIPT_HASH,
  RELEASE_FINALITY_POLICY,
  run,
} from "./workflow.make-adapter.js";

export const FINAL_DEPTH =
  RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth + 2;
export const OUR_THREAD = `${"d7".repeat(32)}#1`;
export const THREAD_LOVELACE = "9000000";

export const resolution = {
  removalTxHash: "c5".repeat(32),
  referencedProofTokenOutRef: `${"c4".repeat(32)}#0`,
  confirmationDepth: FINAL_DEPTH,
};

/** The chain as our L1 port reads it: our thread standing until its cancel
 * lands, spent by the cancel while the node's mempool holds it, then that
 * cancel at its depth. `l1` is the same chain the family's own transactions
 * land on. */
export const chain = async ({
  thread = OUR_THREAD,
}: {
  readonly thread?: string;
} = {}) => {
  const state: {
    standing: boolean;
    pending: boolean;
    reclaimDepth: number | undefined;
  } = { standing: true, pending: false, reclaimDepth: undefined };
  const reclaim = await signedWorkflowTransactionFixture();
  const reclaimTx = reclaim.input.transactionHash;
  const l1 = createFixtureChain();
  return {
    state,
    thread,
    reclaimTx,
    reclaimSigned: reclaim.signed,
    l1,
    /** The node's mempool takes the cancel. */
    accept: () => {
      if (state.standing) state.pending = true;
      l1.accept(reclaimTx);
    },
    land: (depth: number) => {
      state.standing = false;
      state.pending = false;
      state.reclaimDepth = depth;
      l1.land(reclaimTx);
    },
    threads: () => ({
      ...(state.standing
        ? {
            standing: {
              threadOutRef: thread,
              step: 2 as const,
              lovelace: THREAD_LOVELACE,
              ...(state.pending ? { pendingSpend: reclaimTx } : {}),
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

export type Ledger = Awaited<ReturnType<typeof chain>>;

/** A cancel port over `ledger`, as one process instance holds it: a
 * submitted cancel waits in the node's mempool until `ledger.land`. */
export const reclaimPort = (
  ledger: Ledger,
  preflight: OwnThreadReclaimPort["preflight"] = async ({ actionId }) =>
    bindWorkflowPreflightTransaction(
      {
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
      } satisfies FraudProofWorkflowPreflight,
      ledger.reclaimSigned,
    ),
) => ({
  observe: vi.fn(async () => ledger.threads()),
  preflight: vi.fn(preflight),
  submit: vi.fn(
    async ({
      preflight: built,
    }: {
      readonly preflight: { txHash: string };
    }) => {
      ledger.accept();
      return built.txHash;
    },
  ),
});

/** `makeAdapter`'s family, its header removed against another prover's
 * record while `removed()` says so, at the removal's depth `depth()`, with
 * our thread named as the stage derivation names it. */
export const removedByAnotherProver = (
  ledger: Ledger,
  removed: () => boolean = () => true,
  depth: () => number = () => FINAL_DEPTH,
  /** Whether the family's own submissions land at once. */
  lands = true,
): FraudProofFamilyWorkflowAdapter => {
  const adapter = makeAdapter({ chain: ledger.l1, lands });
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

/** Resolved against `expected` once our thread on `ledger` is reclaimed by
 * exactly one landed cancel, and never as our conviction. */
export const expectReclaimedThenResolved = (
  outcome: Awaited<ReturnType<typeof run>>,
  expected: typeof resolution,
  ledger: Ledger,
) => {
  expect(outcome.kind).toBe("resolved_by_another_record");
  if (outcome.kind !== "resolved_by_another_record")
    throw new Error("not resolved");
  expect(outcome.resolution).toEqual(expected);
  const { standing, spends } = ledger.threads();
  expect(standing).toBeUndefined();
  expect(spends).toEqual([
    expect.objectContaining({
      txHash: ledger.reclaimTx,
      threadOutRef: ledger.thread,
    }),
  ]);
  // A reclaim is never our conviction: no terminal.
  expect("terminal" in outcome).toBe(false);
  return outcome;
};
