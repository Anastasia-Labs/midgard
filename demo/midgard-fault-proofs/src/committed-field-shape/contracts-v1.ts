import type { Script } from "@lucid-evolution/lucid";

/** Human-readable family label used in fail-closed submission errors. */
export const COMMITTED_FIELD_SHAPE_CATEGORY_LABEL = "committed-field-shape";

/** Applied blueprint titles, in proof-chain order. */
export const COMMITTED_FIELD_SHAPE_BLUEPRINT_TITLES = {
  step01: "fraud_proofs/committed_field_shape/step_01.main.spend",
  step02: "fraud_proofs/committed_field_shape/step_02.main.spend",
} as const;

export type CommittedFieldShapeStepContract = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/**
 * Explicit pre-registration deployment record. There is deliberately no
 * category id here: the test reservation is supplied by the emulator sidecar,
 * while production allocation remains a registration-wave decision.
 */
export type CommittedFieldShapeContracts = {
  readonly steps: readonly [
    CommittedFieldShapeStepContract,
    CommittedFieldShapeStepContract,
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
  readonly fieldPreimageCertificatePolicyId: string;
};
