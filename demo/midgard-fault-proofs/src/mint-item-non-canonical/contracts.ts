import {
  applyParamsToScript,
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

export const MINT_ITEM_NON_CANONICAL_BLUEPRINT_TITLES = {
  step01: "fraud_proofs/mint_item_non_canonical/step_01.main.spend",
  step02: "fraud_proofs/mint_item_non_canonical/step_02.main.spend",
  step03: "fraud_proofs/mint_item_non_canonical/step_03.main.spend",
  step04: "fraud_proofs/mint_item_non_canonical/step_04.main.spend",
} as const;

export type MintItemNonCanonicalStepContract = {
  readonly blueprintTitle: string;
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
  readonly referenceOutRef: string;
};

export type MintItemNonCanonicalContracts = {
  readonly steps: readonly [
    MintItemNonCanonicalStepContract,
    MintItemNonCanonicalStepContract,
    MintItemNonCanonicalStepContract,
    MintItemNonCanonicalStepContract,
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
  readonly fieldPreimageCertificateMintingScript: Script;
};

export type MintItemNonCanonicalBlueprint = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;

const applyExact = (
  blueprint: MintItemNonCanonicalBlueprint,
  title: string,
  parameters: readonly Data[],
): Script => {
  const validator = blueprint.validators.find((entry) => entry.title === title);
  if (validator === undefined)
    throw new Error(`mintItemNonCanonical: blueprint omitted ${title}`);
  if ((validator.parameters?.length ?? 0) !== parameters.length)
    throw new Error(`mintItemNonCanonical: ${title} parameter arity changed`);
  return {
    type: "PlutusV3",
    script: applyParamsToScript(validator.compiledCode, [...parameters]),
  };
};

/**
 * Applies the four scripts backwards, in their blueprint-declared order. The
 * registered SDK chain applies the same parameters; the family's tests pin
 * the two applications against each other.
 */
export const applyMintItemNonCanonicalScripts = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOracleScriptHash,
}: {
  readonly blueprint: MintItemNonCanonicalBlueprint;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOracleScriptHash: string;
}): MintItemNonCanonicalContracts["steps"] => {
  for (const [name, value] of Object.entries({
    computationThreadPolicyId,
    fraudProofPolicyId,
    fieldPreimageCertificatePolicyId,
    hubOracleScriptHash,
  })) {
    if (!/^[0-9a-f]{56}$/u.test(value))
      throw new Error(
        `mintItemNonCanonical: ${name} must be a 28-byte lowercase hash`,
      );
  }
  const titles = Object.values(MINT_ITEM_NON_CANONICAL_BLUEPRINT_TITLES);
  const applied = (
    index: number,
    parameters: readonly Data[],
  ): MintItemNonCanonicalStepContract => {
    const blueprintTitle = titles[index]!;
    const spendingScript = applyExact(blueprint, blueprintTitle, parameters);
    return Object.freeze({
      blueprintTitle,
      spendingScript,
      spendingScriptHash: validatorToScriptHash(spendingScript),
      spendingScriptAddress: validatorToAddress(network, spendingScript),
      referenceOutRef: `${"00".repeat(32)}#${index.toString()}`,
    });
  };
  const step04 = applied(3, [
    fraudProofPolicyId,
    fraudProofTokenAddressData,
    computationThreadPolicyId,
  ]);
  const step03 = applied(2, [
    step04.spendingScriptHash,
    computationThreadPolicyId,
  ]);
  const step02 = applied(1, [
    step03.spendingScriptHash,
    computationThreadPolicyId,
    fieldPreimageCertificatePolicyId,
  ]);
  const step01 = applied(0, [
    step02.spendingScriptHash,
    computationThreadPolicyId,
    hubOracleScriptHash,
  ]);
  return [step01, step02, step03, step04];
};
