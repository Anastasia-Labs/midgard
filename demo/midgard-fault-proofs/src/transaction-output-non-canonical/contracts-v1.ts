import {
  type Network,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import type { TransactionOutputSubmissionAdapterV1 } from "./transaction-output-non-canonical-v1.js";

export const TRANSACTION_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES_V1 = {
  step01: "fraud_proofs/transaction_output_non_canonical/step_01.main.spend",
  step02: "fraud_proofs/transaction_output_non_canonical/step_02.main.spend",
  step03: "fraud_proofs/transaction_output_non_canonical/step_03.main.spend",
  step04: "fraud_proofs/transaction_output_non_canonical/step_04.main.spend",
} as const;

export type TransactionOutputNonCanonicalStepContractV1 = {
  readonly blueprintTitle: string;
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
  readonly referenceOutRef: string;
};

export type TransactionOutputNonCanonicalContractsV1 = {
  readonly steps: readonly [
    TransactionOutputNonCanonicalStepContractV1,
    TransactionOutputNonCanonicalStepContractV1,
    TransactionOutputNonCanonicalStepContractV1,
    TransactionOutputNonCanonicalStepContractV1,
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

export type TransactionOutputNonCanonicalProductionManifestV1 = {
  readonly schemaVersion: "transaction-output-non-canonical-production-manifest-v1";
  readonly category: "transactionOutputNonCanonical";
  readonly categoryId: string;
  readonly network: Network;
  readonly contracts: TransactionOutputNonCanonicalContractsV1;
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
export const loadTransactionOutputNonCanonicalProductionV1 = (
  manifest: TransactionOutputNonCanonicalProductionManifestV1,
): TransactionOutputNonCanonicalProductionManifestV1 => {
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
    TRANSACTION_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES_V1,
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

export type TransactionOutputNonCanonicalProductionSubmittersV1 =
  TransactionOutputSubmissionAdapterV1 & {
    /** Init must be the registered-category computation-thread mint. */
    readonly submitInitIsRegisteredCategoryMint: true;
    /** Removal must consume the minted proof through canonical removal. */
    readonly removalIsCanonicalFraudProofSpend: true;
  };

export const bindTransactionOutputNonCanonicalProductionSubmittersV1 = ({
  manifest,
  submitters,
}: {
  readonly manifest: TransactionOutputNonCanonicalProductionManifestV1;
  readonly submitters: TransactionOutputNonCanonicalProductionSubmittersV1;
}): TransactionOutputNonCanonicalProductionSubmittersV1 => {
  loadTransactionOutputNonCanonicalProductionV1(manifest);
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
