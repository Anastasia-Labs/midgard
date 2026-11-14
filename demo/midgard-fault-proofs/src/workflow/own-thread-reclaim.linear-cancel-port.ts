import type { LucidEvolution, Script, UTxO } from "@lucid-evolution/lucid";

import { submitLinearFaultCancel } from "../linear-fault-cancel.js";
import {
  type LinearFaultStepContract,
  linearFaultStepLabel,
} from "../linear-fault-family.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofFamilyL1ObservationPort } from "./family-l1-observation.js";
import type { FraudProofWorkflowPreflight } from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import type { OwnThreadReclaimPort } from "./own-thread-reclaim.with-own-thread-reclaim.js";
import {
  bindWorkflowPreflightTransaction,
  captureLocallyEvaluatedTransaction,
  type FraudProofPreSubmitBoundary,
  LOCAL_UPLC_EVALUATOR,
  type LocallyEvaluatedTransaction,
  requireReferenceOnlyScriptWitnesses,
  submitCapturedTransaction,
} from "./transaction-boundary.js";

/** What a deployment needs to cancel one of our threads: the family's step
 * contracts and the thread policy, read when first used. */
export type LinearCancelContracts = Readonly<{
  steps: readonly LinearFaultStepContract[];
  computationThread: Readonly<{
    policyId: string;
    mintingScript?: Script;
  }>;
}>;

/** A family's own cancel of one thread, built and signed up to the
 * pre-submit boundary it is handed. */
export type LinearCancelBuilder = (input: {
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary: FraudProofPreSubmitBoundary;
}) => Promise<unknown>;

/**
 * The family's cancel as the reclaim of our own thread: the linear cancel,
 * which spends only a thread whose datum names our key, burns its token and
 * pays the rest to our wallet. A family whose thread positions sit off its
 * linear chain hands in its own cancel as `cancel`. It is captured at the
 * pre-submit boundary and only those exact bytes are submitted.
 */
export const createLinearCancelReclaimPort = ({
  family,
  lucid,
  signer,
  categoryId,
  contracts,
  stepReferences,
  witnessReferenceScripts,
  cancel,
  l1,
}: {
  readonly family: string;
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly categoryId: string;
  readonly contracts: () => LinearCancelContracts;
  /** One reference script per step, at the step's place; a position the
   * deployment did not publish is `undefined`. */
  readonly stepReferences: readonly (UTxO | undefined)[];
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly cancel?: LinearCancelBuilder;
  readonly l1: Required<
    Pick<
      FraudProofFamilyL1ObservationPort<never>,
      "observeOwnThreads" | "observeSignedTransaction"
    >
  > &
    Pick<
      FraudProofFamilyL1ObservationPort<never>,
      "rebroadcastSignedTransaction"
    >;
}): OwnThreadReclaimPort => {
  const captured = new Map<string, LocallyEvaluatedTransaction>();
  const unavailable = (): string | undefined => {
    if (contracts().computationThread.mintingScript === undefined)
      return "the deployment binds no computation-thread minting script";
    if (witnessReferenceScripts.computationThreadMint === undefined)
      return "the deployment supplied no computation-thread minting reference script";
    return undefined;
  };
  return Object.freeze({
    observe: async ({ headerHash }) =>
      await l1.observeOwnThreads({ headerHash }),
    unavailable,
    preflight: async ({ actionId, threadOutRef, step }) => {
      const missing = unavailable();
      if (missing !== undefined) throw new Error(missing);
      const { steps, computationThread } = contracts();
      const referenceScriptUtxo = stepReferences[step - 1];
      if (referenceScriptUtxo === undefined)
        throw new Error(
          `${linearFaultStepLabel(family, step - 1)} has no reference script`,
        );
      const transaction = await captureLocallyEvaluatedTransaction(
        async (boundary) => {
          if (cancel !== undefined) {
            await cancel({
              threadOutRef,
              referenceScriptUtxo,
              preSubmitBoundary: boundary,
            });
            return;
          }
          await submitLinearFaultCancel({
            lucid,
            family,
            steps,
            computationThread: {
              policyId: computationThread.policyId,
              mintingScript: computationThread.mintingScript!,
            },
            categoryId,
            signer,
            threadOutRef,
            referenceScriptUtxo,
            witnessReferenceScripts,
            preSubmitBoundary: boundary,
            awaitConfirmation: false,
          });
        },
      );
      requireReferenceOnlyScriptWitnesses({
        transaction,
        label: `${family} thread reclaim`,
      });
      captured.set(transaction.txHash, transaction);
      return bindWorkflowPreflightTransaction(
        {
          actionId,
          txHash: transaction.txHash,
          scriptExecution: "reference_scripts",
          localUplcEvaluation: {
            status: "passed",
            evaluator: LOCAL_UPLC_EVALUATOR,
          },
          referenceScripts: transaction.referenceScripts,
        } satisfies FraudProofWorkflowPreflight,
        transaction.signed,
      );
    },
    submit: async ({ preflight }) => {
      const transaction = captured.get(preflight.txHash);
      if (transaction === undefined)
        throw new Error(
          `${family} thread reclaim has no captured body for ${preflight.txHash}`,
        );
      try {
        return await submitCapturedTransaction(transaction);
      } finally {
        captured.delete(preflight.txHash);
      }
    },
    observeSignedTransaction: async (input) =>
      await l1.observeSignedTransaction(input),
    ...(l1.rebroadcastSignedTransaction === undefined
      ? {}
      : {
          rebroadcastSignedTransaction: async (
            input: Parameters<
              NonNullable<OwnThreadReclaimPort["rebroadcastSignedTransaction"]>
            >[0],
          ) => await l1.rebroadcastSignedTransaction!(input),
        }),
  });
};
