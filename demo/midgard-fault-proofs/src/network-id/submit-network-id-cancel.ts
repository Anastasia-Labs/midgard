/** Explicit Q35 prover cancellation from any physical step of the family. */
import type { UTxO } from "@lucid-evolution/lucid";

import { submitLinearFaultCancel } from "../linear-fault-cancel.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  NETWORK_ID_CATEGORY_LABEL,
  type NetworkIdContracts,
  type NetworkIdStepContract,
} from "./contracts.js";

/**
 * The family's physical thread positions in chain order. The forced door and
 * the forced outputs scan sit between the two linear steps; both are present
 * only on deployments that carry the wrongful-rejection direction, and the
 * scan self-loops, so a cancellation may land on it from any batch.
 */
export type NetworkIdPhysicalStep =
  | "step01"
  | "forcedStep"
  | "forcedScan"
  | "step02";

export const networkIdPhysicalSteps = (
  contracts: NetworkIdContracts,
): readonly (readonly [NetworkIdPhysicalStep, NetworkIdStepContract])[] => {
  const forced: (readonly [NetworkIdPhysicalStep, NetworkIdStepContract])[] =
    [];
  if (contracts.forcedStep !== undefined)
    forced.push(["forcedStep", contracts.forcedStep]);
  if (contracts.forcedScan !== undefined)
    forced.push(["forcedScan", contracts.forcedScan]);
  return [
    ["step01", contracts.steps[0]],
    ...forced,
    ["step02", contracts.steps[1]],
  ];
};

export type SubmitNetworkIdCancelParams = {
  readonly lucid: Parameters<typeof submitLinearFaultCancel>[0]["lucid"];
  readonly contracts: NetworkIdContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** Published reference script for the step the thread is locked at. */
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
};

export type SubmitNetworkIdCancelResult = Awaited<
  ReturnType<typeof submitLinearFaultCancel>
> & {
  readonly cancelledStep: NetworkIdPhysicalStep;
};

export const submitNetworkIdCancel = async (
  params: SubmitNetworkIdCancelParams,
): Promise<SubmitNetworkIdCancelResult> => {
  if (params.witnessReferenceScripts == null) {
    throw new Error(
      "network-id cancellation requires published step and computation-thread reference scripts",
    );
  }
  const physical = networkIdPhysicalSteps(params.contracts);
  const result = await submitLinearFaultCancel({
    lucid: params.lucid,
    family: NETWORK_ID_CATEGORY_LABEL,
    steps: physical.map(([, step]) => step),
    computationThread: params.contracts.computationThread,
    categoryId: params.categoryId,
    signer: params.signer,
    threadOutRef: params.threadOutRef,
    referenceScriptUtxo: params.referenceScriptUtxo,
    witnessReferenceScripts: params.witnessReferenceScripts,
    preSubmitBoundary: params.preSubmitBoundary,
    awaitConfirmation: params.awaitConfirmation,
  });
  const cancelled = physical[result.cancelledStepIndex];
  if (cancelled === undefined)
    throw new Error("network-id: cancelled an unknown physical step");
  return { ...result, cancelledStep: cancelled[0] };
};
