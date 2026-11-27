import {
  parseFaultProofBlueprint,
  type SpendingValidator,
} from "@al-ft/midgard-sdk/core";
import {
  buildFieldGrammarCertificateValidator,
  buildTransactionOutputNonCanonicalChain,
} from "@al-ft/midgard-sdk/fraud-proof";
import { type Data, type Network, type Script } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export const TRANSACTION_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES = {
  step01: "fraud_proofs/transaction_output_non_canonical/step_01.main.spend",
  step02: "fraud_proofs/transaction_output_non_canonical/step_02.main.spend",
  step03: "fraud_proofs/transaction_output_non_canonical/step_03.main.spend",
  step03Datum:
    "fraud_proofs/transaction_output_non_canonical/step_03_datum.main.spend",
  step04: "fraud_proofs/transaction_output_non_canonical/step_04.main.spend",
} as const;

export type TransactionOutputNonCanonicalStepContract = {
  readonly blueprintTitle: string;
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
  readonly referenceOutRef: string;
};

export type TransactionOutputNonCanonicalContracts = {
  readonly steps: readonly [
    TransactionOutputNonCanonicalStepContract,
    TransactionOutputNonCanonicalStepContract,
    TransactionOutputNonCanonicalStepContract,
    TransactionOutputNonCanonicalStepContract,
    TransactionOutputNonCanonicalStepContract,
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

export type TransactionOutputNonCanonicalBlueprint = Readonly<{
  validators: readonly Readonly<{
    title: string;
    compiledCode: string;
    parameters?: readonly unknown[];
  }>[];
}>;

/**
 * Applies the five scripts backwards, in their blueprint-declared order. The
 * registered SDK chain applies the same parameters; the family's tests pin
 * the two applications against each other.
 */
export const applyTransactionOutputNonCanonicalScripts = ({
  blueprint,
  network,
  computationThreadPolicyId,
  fraudProofPolicyId,
  fraudProofTokenAddressData,
  fieldPreimageCertificatePolicyId,
  hubOracleScriptHash,
}: {
  readonly blueprint: TransactionOutputNonCanonicalBlueprint;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly hubOracleScriptHash: string;
}): TransactionOutputNonCanonicalContracts["steps"] => {
  const parsedBlueprint = parseFaultProofBlueprint(blueprint);
  const { steps } = Effect.runSync(
    buildTransactionOutputNonCanonicalChain({
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
  const titles = Object.values(
    TRANSACTION_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES,
  );
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
    adapt(steps[4], 4),
  ];
};
