import {
  buildExecutionNativeScriptInvalidChain,
  parseFaultProofBlueprint,
  type SpendingValidator,
} from "@al-ft/midgard-sdk";
import { type Data, type Network, type Script } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export const EXECUTION_NATIVE_SCRIPT_INVALID_BLUEPRINT_TITLES = Object.freeze(
  Array.from(
    { length: 6 },
    (_, index) =>
      `fraud_proofs/execution_native_script_invalid/step_${String(index + 1).padStart(2, "0")}.main.spend`,
  ),
);
export const EXECUTION_NATIVE_SCRIPT_INVALID_ACCEPTED_PRELUDE_TITLES =
  Object.freeze([
    "fraud_proofs/execution_native_script_invalid/accepted_reconstruction_init.main.spend",
    "fraud_proofs/execution_native_script_invalid/accepted_spend_prefix.main.spend",
    "fraud_proofs/execution_native_script_invalid/accepted_mint_prefix.main.spend",
    "fraud_proofs/execution_native_script_invalid/accepted_observer_prefix.main.spend",
    "fraud_proofs/execution_native_script_invalid/accepted_receive_prefix.main.spend",
    "fraud_proofs/execution_native_script_invalid/accepted_inline_source.main.spend",
    "fraud_proofs/execution_native_script_invalid/accepted_reference_source.main.spend",
  ] as const);
export const EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL =
  "execution-native-script-invalid" as const;
export type ExecutionNativeScriptInvalidStepContract = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;
export type ExecutionNativeScriptInvalidContracts = Readonly<{
  steps: readonly ExecutionNativeScriptInvalidStepContract[];
  acceptedPrelude?: readonly ExecutionNativeScriptInvalidStepContract[];
  computationThread: {
    readonly policyId: string;
    readonly mintingScript: Script;
  };
  fraudProof: {
    readonly policyId: string;
    readonly mintingScript: Script;
    readonly spendingScriptAddress: string;
  };
  hubOraclePolicyId: string;
  stateQueuePolicyId: string;
  fieldPreimageCertificatePolicyId: string;
}>;
type Blueprint = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;

export type ExecutionNativeScriptInvalidAppliedScripts =
  readonly ExecutionNativeScriptInvalidStepContract[] &
    Readonly<{
      acceptedPrelude: readonly ExecutionNativeScriptInvalidStepContract[];
    }>;

export const applyExecutionNativeScriptInvalidScripts = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  hubOracleScriptHash,
  fieldPreimageCertificatePolicyId,
}: {
  blueprint: Blueprint;
  network: Network;
  computationThreadPolicyId: string;
  fraudProofPolicyId: string;
  fraudProofTokenAddressData: Data;
  hubOracleScriptHash: string;
  fieldPreimageCertificatePolicyId: string;
}): ExecutionNativeScriptInvalidAppliedScripts => {
  const { steps } = Effect.runSync(
    buildExecutionNativeScriptInvalidChain({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId: hubOracleScriptHash,
      computationThread: { policyId: computationThreadPolicyId },
      fraudProof: { policyId: fraudProofPolicyId },
      fraudProofTokenAddressData,
      fieldPreimageCertificatePolicyId,
    }),
  );
  const titles = [
    ...EXECUTION_NATIVE_SCRIPT_INVALID_BLUEPRINT_TITLES,
    ...EXECUTION_NATIVE_SCRIPT_INVALID_ACCEPTED_PRELUDE_TITLES,
  ];
  const adapt = (step: SpendingValidator, index: number) =>
    Object.freeze({
      blueprintTitle: titles[index]!,
      spendingScript: step.spendingScript,
      spendingScriptHash: step.spendingScriptHash,
      spendingScriptAddress: step.spendingScriptAddress,
      referenceOutRef: `${"0".repeat(64)}#0`,
    });
  const logicalSteps = [
    adapt(steps[0], 0),
    adapt(steps[1], 1),
    adapt(steps[2], 2),
    adapt(steps[3], 3),
    adapt(steps[4], 4),
    adapt(steps[5], 5),
  ];
  Object.defineProperty(logicalSteps, "acceptedPrelude", {
    value: Object.freeze([
      adapt(steps[6], 6),
      adapt(steps[7], 7),
      adapt(steps[8], 8),
      adapt(steps[9], 9),
      adapt(steps[10], 10),
      adapt(steps[11], 11),
      adapt(steps[12], 12),
    ]),
    enumerable: false,
  });
  return logicalSteps as unknown as ExecutionNativeScriptInvalidAppliedScripts;
};
