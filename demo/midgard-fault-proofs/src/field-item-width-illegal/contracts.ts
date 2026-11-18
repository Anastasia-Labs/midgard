import {
  buildFieldGrammarCertificateValidator,
  buildFieldItemWidthIllegalChain,
  parseFaultProofBlueprint,
  type SpendingValidator,
} from "@al-ft/midgard-sdk";
import { type Data, type Network, type Script } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export const FIELD_ITEM_WIDTH_ILLEGAL_BLUEPRINT_TITLES = {
  step01: "fraud_proofs/field_item_width_illegal/step_01.main.spend",
  step02: "fraud_proofs/field_item_width_illegal/step_02.main.spend",
  step03: "fraud_proofs/field_item_width_illegal/step_03.main.spend",
} as const;

export type FieldItemWidthIllegalStepContract = {
  readonly blueprintTitle: string;
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
  readonly referenceOutRef: string;
};

export type FieldItemWidthIllegalContracts = {
  readonly steps: readonly [
    FieldItemWidthIllegalStepContract,
    FieldItemWidthIllegalStepContract,
    FieldItemWidthIllegalStepContract,
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

export type FieldItemWidthIllegalBlueprint = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;

/**
 * Applies the three scripts backwards, in their blueprint-declared parameter
 * order. The registered SDK chain
 * (`@al-ft/midgard-sdk` `buildFieldItemWidthIllegalChain`) applies the same
 * parameters; the family's tests pin the two applications against each other
 * so a drift in either is a red test, not a silently different deployed hash.
 */
export const applyFieldItemWidthIllegalScripts = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOracleScriptHash,
}: {
  readonly blueprint: FieldItemWidthIllegalBlueprint;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOracleScriptHash: string;
}): FieldItemWidthIllegalContracts["steps"] => {
  const parsedBlueprint = parseFaultProofBlueprint(blueprint);
  const { steps } = Effect.runSync(
    buildFieldItemWidthIllegalChain({
      blueprint: parsedBlueprint,
      network,
      hubOraclePolicyId: hubOracleScriptHash,
      computationThread: { policyId: computationThreadPolicyId },
      fraudProof: { policyId: fraudProofPolicyId },
      fraudProofTokenAddressData,
      fieldPreimageCertificatePolicyId,
      fieldGrammarCertificatePolicyId: buildFieldGrammarCertificateValidator({
        blueprint: parsedBlueprint,
        network,
        fieldPreimageCertificatePolicyId,
      }).policyId,
    }),
  );
  const titles = Object.values(FIELD_ITEM_WIDTH_ILLEGAL_BLUEPRINT_TITLES);
  const adapt = (step: SpendingValidator, index: number) =>
    Object.freeze({
      blueprintTitle: titles[index]!,
      spendingScript: step.spendingScript,
      spendingScriptHash: step.spendingScriptHash,
      spendingScriptAddress: step.spendingScriptAddress,
      referenceOutRef: `${"00".repeat(32)}#${index.toString()}`,
    });
  return [adapt(steps[0], 0), adapt(steps[1], 1), adapt(steps[2], 2)];
};
