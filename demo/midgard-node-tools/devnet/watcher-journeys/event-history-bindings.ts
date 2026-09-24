import {
  bindFraudProofWorkflowDeployment,
  TRANSITION_TRACE_WORKFLOW_DATUM_SCHEMAS,
} from "@al-ft/midgard-fault-proofs";
import {
  CrossBlockDuplicateEventStep02DatumSchema,
  FabricatedDepositStep02Datum,
  FabricatedDepositStep03Datum,
  FabricatedDepositStep04Datum,
  FabricatedWithdrawalStep02Datum,
  FabricatedWithdrawalStep03Datum,
  FabricatedWithdrawalStep04Datum,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";

/** Bind every event reader to the same signed deployment and applied scripts. */
export const bindJourneyEventAuthorities = async (
  input: Pick<
    Parameters<typeof bindFraudProofWorkflowDeployment>[0],
    | "manifest"
    | "blueprintJson"
    | "deploymentInfo"
    | "headerHash"
    | "proverCredential"
  >,
) => {
  const [transition, settlement, deposit, withdrawal] = await Promise.all([
    bindFraudProofWorkflowDeployment({
      ...input,
      category: "transitionTrace",
      stepDatumSchemas: TRANSITION_TRACE_WORKFLOW_DATUM_SCHEMAS,
    }),
    bindFraudProofWorkflowDeployment({
      ...input,
      category: "crossBlockDuplicateEvent",
      stepDatumSchemas: [
        FraudProofComputationThreadStepDatum,
        CrossBlockDuplicateEventStep02DatumSchema,
      ],
    }),
    bindFraudProofWorkflowDeployment({
      ...input,
      category: "fabricatedDeposit",
      stepDatumSchemas: [
        FraudProofComputationThreadStepDatum,
        FabricatedDepositStep02Datum,
        FabricatedDepositStep03Datum,
        FabricatedDepositStep04Datum,
      ],
    }),
    bindFraudProofWorkflowDeployment({
      ...input,
      category: "fabricatedWithdrawal",
      stepDatumSchemas: [
        FraudProofComputationThreadStepDatum,
        FabricatedWithdrawalStep02Datum,
        FabricatedWithdrawalStep03Datum,
        FabricatedWithdrawalStep04Datum,
      ],
    }),
  ]);
  const depositHistory =
    deposit.resolvedContracts.contracts.fabricatedDeposit?.history;
  const withdrawalHistory =
    withdrawal.resolvedContracts.contracts.fabricatedWithdrawal?.history;
  if (
    [settlement, deposit, withdrawal].some(
      (binding) =>
        binding.deploymentFingerprint !== transition.deploymentFingerprint,
    ) ||
    depositHistory === undefined ||
    withdrawalHistory === undefined
  )
    throw new Error(
      "Event fixture authorities differ from the applied deployment",
    );
  return {
    transition,
    settlement,
    history: { deposit: depositHistory, withdrawal: withdrawalHistory },
  };
};
