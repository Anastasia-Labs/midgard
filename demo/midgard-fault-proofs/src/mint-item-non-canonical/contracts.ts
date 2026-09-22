import {
  buildMintItemNonCanonicalChain,
  parseFaultProofBlueprint,
  type SpendingValidator,
} from "@al-ft/midgard-sdk";
import { type Data, type Network, type Script } from "@lucid-evolution/lucid";
import { Effect } from "effect";

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
  const { steps } = Effect.runSync(
    buildMintItemNonCanonicalChain({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId: hubOracleScriptHash,
      computationThread: { policyId: computationThreadPolicyId },
      fraudProof: { policyId: fraudProofPolicyId },
      fraudProofTokenAddressData,
      fieldPreimageCertificatePolicyId,
    }),
  );
  const titles = Object.values(MINT_ITEM_NON_CANONICAL_BLUEPRINT_TITLES);
  const adapt = (step: SpendingValidator, index: number) =>
    Object.freeze({
      blueprintTitle: titles[index]!,
      spendingScript: step.spendingScript,
      spendingScriptHash: step.spendingScriptHash,
      spendingScriptAddress: step.spendingScriptAddress,
      referenceOutRef: `${"00".repeat(32)}#${index.toString()}`,
    });
  return [
    adapt(steps[0], 0),
    adapt(steps[1], 1),
    adapt(steps[2], 2),
    adapt(steps[3], 3),
  ];
};
