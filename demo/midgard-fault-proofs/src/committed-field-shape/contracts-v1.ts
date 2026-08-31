import type { Script } from "@lucid-evolution/lucid";

import type { FaultProofClaimRegistryContractV1 } from "../claim-registry-transaction-v1.js";

/** Human-readable family label used in fail-closed submission errors. */
export const COMMITTED_FIELD_SHAPE_CATEGORY_LABEL = "committed-field-shape";

/** Applied blueprint titles, in proof-chain order. */
export const COMMITTED_FIELD_SHAPE_BLUEPRINT_TITLES_V1 = {
  step01: "fraud_proofs/committed_field_shape/step_01.main.spend",
  step02: "fraud_proofs/committed_field_shape/step_02.main.spend",
} as const;

export type CommittedFieldShapeStepContractV1 = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/**
 * Explicit pre-registration deployment record. There is deliberately no
 * category id here: the test reservation is supplied by the emulator sidecar,
 * while production allocation remains a registration-wave decision.
 */
export type CommittedFieldShapeContractsV1 = {
  readonly steps: readonly [
    CommittedFieldShapeStepContractV1,
    CommittedFieldShapeStepContractV1,
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
  /**
   * The applied `claim_registry.spend` validator. Every arm of
   * `computation_thread.mint` requires the claim-registry input in the same
   * transaction, so each submitter resolves its mutation from here.
   */
  readonly claimRegistry: FaultProofClaimRegistryContractV1;
  readonly stateQueuePolicyId: string;
  readonly fieldPreimageCertificatePolicyId: string;
};
