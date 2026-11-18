import {
  buildMintDeclaredAssetLimitChain,
  parseFaultProofBlueprint,
  type SpendingValidator,
} from "@al-ft/midgard-sdk";
import { type Data, type Network, type Script } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export const MINT_DECLARED_ASSET_LIMIT_BLUEPRINT_TITLES = Object.freeze([
  "fraud_proofs/mint_declared_asset_limit/step_01.main.spend",
  "fraud_proofs/mint_declared_asset_limit/step_02.main.spend",
  "fraud_proofs/mint_declared_asset_limit/step_03.main.spend",
  "fraud_proofs/mint_declared_asset_limit/step_04.main.spend",
] as const);

export type MintDeclaredAssetLimitStepContract = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;

export type MintDeclaredAssetLimitContracts = Readonly<{
  steps: readonly [
    MintDeclaredAssetLimitStepContract,
    MintDeclaredAssetLimitStepContract,
    MintDeclaredAssetLimitStepContract,
    MintDeclaredAssetLimitStepContract,
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
  fieldPreimageCertificatePolicyId: string;
  fieldPreimageCertificateMintingScript: Script;
}>;

export type MintDeclaredAssetLimitBlueprint = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;

/** Applies the four scripts backwards, in their blueprint-declared order. */
export const applyMintDeclaredAssetLimitScripts = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOracleScriptHash,
}: {
  readonly blueprint: MintDeclaredAssetLimitBlueprint;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOracleScriptHash: string;
}): MintDeclaredAssetLimitContracts["steps"] => {
  const { steps } = Effect.runSync(
    buildMintDeclaredAssetLimitChain({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId: hubOracleScriptHash,
      computationThread: { policyId: computationThreadPolicyId },
      fraudProof: { policyId: fraudProofPolicyId },
      fraudProofTokenAddressData,
      fieldPreimageCertificatePolicyId,
    }),
  );
  const titles = Object.values(MINT_DECLARED_ASSET_LIMIT_BLUEPRINT_TITLES);
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
  ];
};
