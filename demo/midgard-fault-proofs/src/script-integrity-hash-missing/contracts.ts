import type { Script } from "@lucid-evolution/lucid";

export type ScriptIntegrityHashMissingStepContract = {
  readonly blueprintTitle?: string;
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
  readonly referenceOutRef?: string;
};

export type ScriptIntegrityHashMissingContracts = {
  readonly steps: readonly [
    ScriptIntegrityHashMissingStepContract,
    ScriptIntegrityHashMissingStepContract,
    ScriptIntegrityHashMissingStepContract,
    ScriptIntegrityHashMissingStepContract,
    ScriptIntegrityHashMissingStepContract,
    ScriptIntegrityHashMissingStepContract,
    ScriptIntegrityHashMissingStepContract,
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
  readonly fieldPreimageCertificatePolicyId: string;
  readonly fieldPreimageCertificateMintingScript?: Script;
  readonly hubOraclePolicyId: string;
  readonly stateQueuePolicyId: string;
};
