/** Deployed accepted and wrongful-forced-rejection missing-signature paths. */
import type { Script } from "@lucid-evolution/lucid";

/** Human-readable family label used in every local failure message. */
export const MISSING_SIGNATURE_CATEGORY_LABEL = "missing-signature";

/** Blueprint titles of every physical step validator. */
export const MISSING_SIGNATURE_BLUEPRINT_TITLES = {
  forcedStep: "fraud_proofs/missing_signature/forced_step.main.spend",
  forcedSigner: "fraud_proofs/missing_signature/forced_signer.main.spend",
  forcedWitness: "fraud_proofs/missing_signature/forced_witness.main.spend",
  step01: "fraud_proofs/missing_signature/step_01.main.spend",
  step02: "fraud_proofs/missing_signature/step_02.main.spend",
  step03: "fraud_proofs/missing_signature/step_03.main.spend",
  step04: "fraud_proofs/missing_signature/step_04.main.spend",
} as const;

/** One deployed step of the `missing-signature` chain. */
export type MissingSignatureStepContract = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/** Already-resolved contracts for both proof directions. */
export type MissingSignatureContracts = {
  readonly forcedStep: MissingSignatureStepContract;
  readonly forcedSigner: MissingSignatureStepContract;
  readonly forcedWitness: MissingSignatureStepContract;
  /** Steps 01..04, in order. */
  readonly steps: readonly [
    MissingSignatureStepContract,
    MissingSignatureStepContract,
    MissingSignatureStepContract,
    MissingSignatureStepContract,
  ];
  readonly computationThread: {
    readonly policyId: string;
    readonly mintingScript: Script;
  };
  readonly fraudProof: {
    readonly policyId: string;
    readonly mintingScript: Script;
    readonly spendingScriptAddress: string;
  };
  readonly hubOraclePolicyId: string;
  readonly stateQueuePolicyId: string;
  /** Real certificate policy used by all body and witness field openings. */
  readonly fieldPreimageCertificatePolicyId: string;
};
