import type { MissingNativeScriptTxContracts } from "../missing-native-script-tx/contracts-v1.js";
import type { MissingNativeScriptUtxoContracts } from "./contracts-v1.js";

/**
 * The Q33 and Q17 staged field-6 validators deliberately share the exact
 * grammar/semantic checkpoint ABI. Adapt only the step numbering here so the
 * already-audited transaction builder is reused without duplicating its
 * reference-script and checkpoint validation logic.
 */
export const missingNativeScriptUtxoStagedContracts = (
  contracts: MissingNativeScriptUtxoContracts,
): MissingNativeScriptTxContracts => {
  const unused = contracts.steps[0];
  return {
    steps: [
      unused,
      unused,
      unused,
      unused,
      unused,
      contracts.steps[4],
      contracts.steps[5],
      contracts.steps[6],
    ],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    hubOraclePolicyId: contracts.hubOraclePolicyId,
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fieldPreimageCertificatePolicyId:
      contracts.fieldPreimageCertificatePolicyId,
  };
};
