import {
  buildUnusedRedeemerChain,
  parseFaultProofBlueprint,
  type SpendingValidator,
} from "@al-ft/midgard-sdk";
import { type Data, type Network, type Script } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export const UNUSED_REDEEMER_BLUEPRINT_TITLES = Object.freeze([
  "fraud_proofs/unused_redeemer/step_01.main.spend",
  "fraud_proofs/unused_redeemer/step_02.main.spend",
  "fraud_proofs/unused_redeemer/step_02a.main.spend",
  "fraud_proofs/unused_redeemer/step_02b.main.spend",
  "fraud_proofs/unused_redeemer/step_02c.main.spend",
  "fraud_proofs/unused_redeemer/step_03.main.spend",
  "fraud_proofs/unused_redeemer/step_04.main.spend",
  "fraud_proofs/unused_redeemer/step_05.main.spend",
  "fraud_proofs/unused_redeemer/step_06.main.spend",
] as const);
export type UnusedRedeemerStepContract = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;
export type UnusedRedeemerContracts = Readonly<{
  steps: readonly [
    UnusedRedeemerStepContract,
    UnusedRedeemerStepContract,
    UnusedRedeemerStepContract,
    UnusedRedeemerStepContract,
    UnusedRedeemerStepContract,
    UnusedRedeemerStepContract,
    UnusedRedeemerStepContract,
    UnusedRedeemerStepContract,
    UnusedRedeemerStepContract,
  ];
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
}>;
type Blueprint = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;
export const applyUnusedRedeemerScripts = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  hubOracleScriptHash,
}: {
  blueprint: Blueprint;
  network: Network;
  computationThreadPolicyId: string;
  fraudProofPolicyId: string;
  fraudProofTokenAddressData: Data;
  hubOracleScriptHash: string;
}): UnusedRedeemerContracts["steps"] => {
  const { steps } = Effect.runSync(
    buildUnusedRedeemerChain({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId: hubOracleScriptHash,
      computationThread: { policyId: computationThreadPolicyId },
      fraudProof: { policyId: fraudProofPolicyId },
      fraudProofTokenAddressData,
    }),
  );
  const titles = Object.values(UNUSED_REDEEMER_BLUEPRINT_TITLES);
  const adapt = (step: SpendingValidator, index: number) =>
    Object.freeze({
      blueprintTitle: titles[index]!,
      spendingScript: step.spendingScript,
      spendingScriptHash: step.spendingScriptHash,
      spendingScriptAddress: step.spendingScriptAddress,
      referenceOutRef: `${"0".repeat(64)}#0`,
    });
  return [
    adapt(steps[0], 0),
    adapt(steps[1], 1),
    adapt(steps[2], 2),
    adapt(steps[3], 3),
    adapt(steps[4], 4),
    adapt(steps[5], 5),
    adapt(steps[6], 6),
    adapt(steps[7], 7),
    adapt(steps[8], 8),
  ];
};
