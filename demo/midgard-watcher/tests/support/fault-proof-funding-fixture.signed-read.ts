import {
  computeFraudProofRawL1PointId,
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  type FraudProofWorkflowReconcileResult,
  type FraudProofWorkflowTerminalVerifier,
  type SignedTransactionRecoveryObservation,
} from "@al-ft/midgard-fault-proofs";
import { vi } from "vitest";

const tipBlock = {
  slot: "100",
  blockHash: "c1".repeat(32),
  blockNo: "100",
};
const tip = { ...tipBlock, pointId: computeFraudProofRawL1PointId(tipBlock) };

/**
 * The fixture chain's authenticated answer for one exact signed transaction,
 * written as the answer the workflow should read back. Inclusion at the tip
 * reads as the result; a deep inclusion or a retired absence carries its
 * canonical receipt; a plain absence is impossible at the tip.
 */
export const signedReadObservation = (
  input: Readonly<{
    transactionHash: string;
    signedTransactionCborHex: string;
  }>,
  answer: FraudProofWorkflowReconcileResult,
): SignedTransactionRecoveryObservation => {
  const at = (
    status: SignedTransactionRecoveryObservation["status"],
    reason: string = status,
  ) => ({
    ...input,
    status,
    canonicalPoint: tip,
    releaseFinalPoint: tip,
    inputs: [],
    reason,
  });
  if (answer.kind === "unknown" || answer.kind === "conflict")
    return at(answer.kind, answer.reason);
  const { retirement } = answer;
  if (answer.kind === "not_found")
    return retirement === undefined
      ? at("expired_at_tip")
      : {
          ...at(
            retirement.reason === "included" ? "expired" : retirement.reason,
          ),
          canonicalPoint: retirement.canonicalPoint,
          releaseFinalPoint: retirement.releaseFinalPoint,
        };
  if (retirement !== undefined)
    return {
      ...at("included"),
      inclusionPoint: retirement.releaseFinalPoint,
      canonicalPoint: retirement.canonicalPoint,
      releaseFinalPoint: retirement.releaseFinalPoint,
    };
  return answer.kind === "confirmed"
    ? { ...at("included"), inclusionPoint: tip }
    : at("pending");
};

/**
 * The fixture's one chain: the family's own reads and the workflow's signed
 * reads of recorded attempts both answer from `chain`. A row that needs the
 * two to disagree overrides the family's reconcile alone.
 */
export const createFixtureChain = () => {
  const chain = vi.fn(
    async (txHash: string): Promise<FraudProofWorkflowReconcileResult> => ({
      kind: "confirmed",
      txHash,
    }),
  );
  const terminalVerify = vi.fn<FraudProofWorkflowTerminalVerifier["verify"]>(
    async ({ candidate }) => candidate,
  );
  const observeSignedTransaction = vi.fn<
    FraudProofWorkflowTerminalVerifier["observeSignedTransaction"]
  >(async (input) =>
    signedReadObservation(input, await chain(input.transactionHash)),
  );
  const terminalVerifier: FraudProofWorkflowTerminalVerifier = {
    verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
    verify: terminalVerify,
    observeSignedTransaction,
  };
  return { chain, terminalVerify, observeSignedTransaction, terminalVerifier };
};
