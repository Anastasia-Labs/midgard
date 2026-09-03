import {
  applyParamsToScript,
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

export const MISSING_REDEEMER_BLUEPRINT_TITLES = Object.freeze([
  "fraud_proofs/missing_redeemer/step_01.main.spend",
  "fraud_proofs/missing_redeemer/step_02.main.spend",
  "fraud_proofs/missing_redeemer/step_02a.main.spend",
  "fraud_proofs/missing_redeemer/step_02b.main.spend",
  "fraud_proofs/missing_redeemer/step_03.main.spend",
  "fraud_proofs/missing_redeemer/step_04.main.spend",
  "fraud_proofs/missing_redeemer/step_05.main.spend",
] as const);
export type MissingRedeemerStepContract = Readonly<{
  blueprintTitle: string;
  spendingScript: Script;
  spendingScriptHash: string;
  spendingScriptAddress: string;
  referenceOutRef: string;
}>;
export type MissingRedeemerContracts = Readonly<{
  steps: readonly [
    MissingRedeemerStepContract,
    MissingRedeemerStepContract,
    MissingRedeemerStepContract,
    MissingRedeemerStepContract,
    MissingRedeemerStepContract,
    MissingRedeemerStepContract,
    MissingRedeemerStepContract,
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
type Blueprint = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;

export const applyMissingRedeemerScripts = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOracleScriptHash,
}: {
  readonly blueprint: Blueprint;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOracleScriptHash: string;
}): MissingRedeemerContracts["steps"] => {
  const apply = (
    index: number,
    parameters: readonly Data[],
  ): MissingRedeemerStepContract => {
    const title = MISSING_REDEEMER_BLUEPRINT_TITLES[index]!;
    const entry = blueprint.validators.find(
      (candidate) => candidate.title === title,
    );
    if (entry === undefined)
      throw new Error(`missingRedeemer: blueprint omitted ${title}`);
    if ((entry.parameters?.length ?? 0) !== parameters.length)
      throw new Error(`missingRedeemer: ${title} parameter arity changed`);
    const spendingScript: Script = {
      type: "PlutusV3",
      script: applyParamsToScript(entry.compiledCode, [...parameters]),
    };
    return Object.freeze({
      blueprintTitle: title,
      spendingScript,
      spendingScriptHash: validatorToScriptHash(spendingScript),
      spendingScriptAddress: validatorToAddress(network, spendingScript),
      referenceOutRef: `${"0".repeat(64)}#0`,
    });
  };
  const s5 = apply(6, [
    fraudProofPolicyId,
    fraudProofTokenAddressData,
    computationThreadPolicyId,
  ]);
  const s4 = apply(5, [
    s5.spendingScriptHash,
    computationThreadPolicyId,
    fieldPreimageCertificatePolicyId,
  ]);
  const s3 = apply(4, [
    s4.spendingScriptHash,
    computationThreadPolicyId,
    fieldPreimageCertificatePolicyId,
  ]);
  const s2b = apply(3, [s3.spendingScriptHash, computationThreadPolicyId]);
  const s2a = apply(2, [s2b.spendingScriptHash, computationThreadPolicyId]);
  const s2 = apply(1, [s2a.spendingScriptHash, computationThreadPolicyId]);
  const s1 = apply(0, [
    s2.spendingScriptHash,
    computationThreadPolicyId,
    hubOracleScriptHash,
  ]);
  return [s1, s2, s2a, s2b, s3, s4, s5];
};
