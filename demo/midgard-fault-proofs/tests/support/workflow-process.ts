/**
 * One watcher process as the fault-proof workflow sees it in a test: the
 * wallet wiring of each objective, a fresh run of an objective at each
 * invocation, and every transaction the process's adapters were asked to
 * submit, oldest first. The workflow keeps no record of what it sends; this
 * one is the test's, taken at each adapter `recording` wraps, just before its
 * submit (an installed family's through `recordingRecord`). A restart is a
 * new process: it starts with nothing sent.
 */
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";

import type { FraudProofFamilyWorkflowAdapter } from "../../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import { workflowPreflightTransaction } from "../../src/workflow/transaction-boundary.js";
import {
  createWorkflowObjectiveWallet,
  createWorkflowRun,
  type WorkflowObjectiveWallet,
  type WorkflowRun,
} from "../../src/workflow/workflow-run.js";

export type WorkflowObjective = Readonly<{
  category: FraudProofCatalogueCategoryName;
  headerHash: string;
}>;

/** A transaction an adapter of the process was asked to submit. */
export type SentTransaction = Readonly<{
  actionId: string;
  txHash: string;
  signedTransactionCborHex: string;
}>;

export type TestWorkflowProcess = Readonly<{
  /** The objective's wallet wiring, the same object each call. */
  objectiveWallet: (objective: WorkflowObjective) => WorkflowObjectiveWallet;
  /** A fresh invocation of the objective over its wallet wiring. */
  run: (objective: WorkflowObjective) => WorkflowRun;
  /** The adapter, with every submission it is asked for recorded. */
  recording: (
    adapter: FraudProofFamilyWorkflowAdapter,
  ) => FraudProofFamilyWorkflowAdapter;
  /** Every transaction a recorded adapter was asked to submit, oldest
   * first, whether or not it landed. */
  sent: () => readonly SentTransaction[];
}>;

export const createTestWorkflowProcess = (): TestWorkflowProcess => {
  const wallets = new Map<string, WorkflowObjectiveWallet>();
  const sent: SentTransaction[] = [];
  const objectiveWallet = (
    objective: WorkflowObjective,
  ): WorkflowObjectiveWallet => {
    const key = `${objective.category}:${objective.headerHash}`;
    let wallet = wallets.get(key);
    if (wallet === undefined) {
      wallet = createWorkflowObjectiveWallet(objective);
      wallets.set(key, wallet);
    }
    return wallet;
  };
  return Object.freeze({
    objectiveWallet,
    run: (objective) => createWorkflowRun(objectiveWallet(objective)),
    recording: (adapter) =>
      Object.freeze({
        ...adapter,
        submit: async (input: Parameters<typeof adapter.submit>[0]) => {
          const signed = workflowPreflightTransaction(input.preflight);
          if (signed !== undefined)
            sent.push(
              Object.freeze({
                actionId: input.action.actionId,
                txHash: input.preflight.txHash,
                signedTransactionCborHex: signed.toCBOR(),
              }),
            );
          return await adapter.submit(input);
        },
      }),
    sent: () => Object.freeze([...sent]),
  });
};

/**
 * The family application record, with the adapter of each workflow it runs
 * passed through `recording` first: an installed runner built over it
 * (`createFamilyApplicationWorkflowRunner`) records what the family submits.
 */
export const recordingRecord = <Record extends object>(
  record: Record,
  recording: (
    adapter: FraudProofFamilyWorkflowAdapter,
  ) => FraudProofFamilyWorkflowAdapter,
): Record => {
  const { execute } = record as unknown as {
    readonly execute: (input: {
      readonly workflow: { readonly adapter: FraudProofFamilyWorkflowAdapter };
    }) => Promise<unknown>;
  };
  return Object.freeze({
    ...record,
    execute: async (input: {
      readonly workflow: { readonly adapter: FraudProofFamilyWorkflowAdapter };
    }) =>
      await execute({
        ...input,
        workflow: {
          ...input.workflow,
          adapter: recording(input.workflow.adapter),
        },
      }),
  });
};
