import {
  buildDistinctAssetAccumulationLimitChain,
  parseFaultProofBlueprint,
  type SpendingValidator,
} from "@al-ft/midgard-sdk";
import { type Data, type Network, type Script } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export const DISTINCT_ASSET_ACCUMULATION_LIMIT_BLUEPRINT_TITLES = Object.freeze(
  [
    "fraud_proofs/distinct_asset_accumulation_limit/step_01.main.spend",
    "fraud_proofs/distinct_asset_accumulation_limit/step_02.main.spend",
    "fraud_proofs/distinct_asset_accumulation_limit/step_03.main.spend",
    "fraud_proofs/distinct_asset_accumulation_limit/step_04.main.spend",
    "fraud_proofs/distinct_asset_accumulation_limit/step_05.main.spend",
    "fraud_proofs/distinct_asset_accumulation_limit/step_06.main.spend",
  ] as const,
);
export type DistinctAssetAccumulationStepContract = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
}>;
export type DistinctAssetAccumulationContracts = Readonly<{
  steps: readonly [
    DistinctAssetAccumulationStepContract,
    DistinctAssetAccumulationStepContract,
    DistinctAssetAccumulationStepContract,
    DistinctAssetAccumulationStepContract,
    DistinctAssetAccumulationStepContract,
    DistinctAssetAccumulationStepContract,
  ];
  computationThread: Readonly<{ policyId: string; mintingScript: Script }>;
  fraudProof: Readonly<{
    policyId: string;
    mintingScript: Script;
    spendingScriptAddress: string;
  }>;
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
export const applyDistinctAssetAccumulationLimitScripts = ({
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
}): readonly [
  DistinctAssetAccumulationStepContract,
  DistinctAssetAccumulationStepContract,
  DistinctAssetAccumulationStepContract,
  DistinctAssetAccumulationStepContract,
  DistinctAssetAccumulationStepContract,
  DistinctAssetAccumulationStepContract,
] => {
  const { steps } = Effect.runSync(
    buildDistinctAssetAccumulationLimitChain({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId: hubOracleScriptHash,
      computationThread: { policyId: computationThreadPolicyId },
      fraudProof: { policyId: fraudProofPolicyId },
      fraudProofTokenAddressData,
    }),
  );
  const titles = Object.values(
    DISTINCT_ASSET_ACCUMULATION_LIMIT_BLUEPRINT_TITLES,
  );
  const adapt = (step: SpendingValidator, index: number) =>
    Object.freeze({
      blueprintTitle: titles[index]!,
      spendingScript: step.spendingScript,
      spendingScriptHash: step.spendingScriptHash,
      spendingScriptAddress: step.spendingScriptAddress,
    });
  return [
    adapt(steps[0], 0),
    adapt(steps[1], 1),
    adapt(steps[2], 2),
    adapt(steps[3], 3),
    adapt(steps[4], 4),
    adapt(steps[5], 5),
  ];
};
