import {
  buildTransactionOutputNonCanonicalChain,
  parseFaultProofBlueprint,
  type SpendingValidator,
} from "@al-ft/midgard-sdk";
import {
  type Data,
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { TransactionOutputSubmissionAdapter } from "./transaction-output-non-canonical.js";

export const TRANSACTION_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES = {
  step01: "fraud_proofs/transaction_output_non_canonical/step_01.main.spend",
  step02: "fraud_proofs/transaction_output_non_canonical/step_02.main.spend",
  step03: "fraud_proofs/transaction_output_non_canonical/step_03.main.spend",
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
 * Applies the four scripts backwards, in their blueprint-declared order. The
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
  const { steps } = Effect.runSync(
    buildTransactionOutputNonCanonicalChain({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId: hubOracleScriptHash,
      computationThread: { policyId: computationThreadPolicyId },
      fraudProof: { policyId: fraudProofPolicyId },
      fraudProofTokenAddressData,
      fieldPreimageCertificatePolicyId,
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
  ];
};

export type TransactionOutputNonCanonicalManifest = {
  readonly schemaVersion: "transaction-output-non-canonical-production-manifest-v1";
  readonly category: "transactionOutputNonCanonical";
  readonly categoryId: string;
  readonly network: Network;
  readonly contracts: TransactionOutputNonCanonicalContracts;
};

const requireHex = (value: string, bytes: number, label: string): void => {
  if (!new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(value)) {
    throw new Error(
      `transactionOutputNonCanonical: ${label} must be ${bytes.toString()} bytes of lowercase hex`,
    );
  }
};

/**
 * Loads only explicitly supplied, already-applied production scripts. This is
 * intentionally independent of the central catalogue: registration supplies
 * the manifest, while this loader proves title/hash/address/reference binding.
 */
export const loadTransactionOutputNonCanonical = (
  manifest: TransactionOutputNonCanonicalManifest,
): TransactionOutputNonCanonicalManifest => {
  if (
    manifest.schemaVersion !==
      "transaction-output-non-canonical-production-manifest-v1" ||
    manifest.category !== "transactionOutputNonCanonical"
  ) {
    throw new Error(
      "transactionOutputNonCanonical: wrong production manifest identity",
    );
  }
  requireHex(manifest.categoryId, 4, "category id");
  const expectedTitles = Object.values(
    TRANSACTION_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES,
  );
  manifest.contracts.steps.forEach((step, index) => {
    if (step.blueprintTitle !== expectedTitles[index]) {
      throw new Error(
        `transactionOutputNonCanonical: step ${(index + 1).toString()} blueprint title mismatch`,
      );
    }
    const hash = validatorToScriptHash(step.spendingScript);
    if (hash !== step.spendingScriptHash) {
      throw new Error(
        `transactionOutputNonCanonical: step ${(index + 1).toString()} script hash mismatch`,
      );
    }
    if (
      validatorToAddress(manifest.network, step.spendingScript) !==
      step.spendingScriptAddress
    ) {
      throw new Error(
        `transactionOutputNonCanonical: step ${(index + 1).toString()} address mismatch`,
      );
    }
    if (!/^[0-9a-f]{64}#[0-9]+$/u.test(step.referenceOutRef)) {
      throw new Error(
        `transactionOutputNonCanonical: step ${(index + 1).toString()} reference out-ref is not canonical`,
      );
    }
  });
  requireHex(
    manifest.contracts.computationThread.policyId,
    28,
    "computation-thread policy id",
  );
  requireHex(
    manifest.contracts.fraudProof.policyId,
    28,
    "fraud-proof policy id",
  );
  requireHex(manifest.contracts.hubOraclePolicyId, 28, "hub-oracle policy id");
  requireHex(
    manifest.contracts.stateQueuePolicyId,
    28,
    "state-queue policy id",
  );
  requireHex(
    manifest.contracts.fieldPreimageCertificatePolicyId,
    28,
    "field-preimage-certificate policy id",
  );
  return Object.freeze(manifest);
};

export type TransactionOutputNonCanonicalSubmitters =
  TransactionOutputSubmissionAdapter & {
    /** Init must be the registered-category computation-thread mint. */
    readonly submitInitIsRegisteredCategoryMint: true;
    /** Removal must consume the minted proof through canonical removal. */
    readonly removalIsCanonicalFraudProofSpend: true;
  };

export const bindTransactionOutputNonCanonicalSubmitters = ({
  manifest,
  submitters,
}: {
  readonly manifest: TransactionOutputNonCanonicalManifest;
  readonly submitters: TransactionOutputNonCanonicalSubmitters;
}): TransactionOutputNonCanonicalSubmitters => {
  loadTransactionOutputNonCanonical(manifest);
  if (
    submitters.submitInitIsRegisteredCategoryMint !== true ||
    submitters.removalIsCanonicalFraudProofSpend !== true
  ) {
    throw new Error(
      "transactionOutputNonCanonical: production submitter topology is incomplete",
    );
  }
  return submitters;
};
