import type { Network, Script } from "@lucid-evolution/lucid";
import {
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

export const SCRIPT_INTEGRITY_HASH_MISSING_BLUEPRINT_TITLES_V1 = [
  "fraud_proofs/script_integrity_hash_missing/step_01.main.spend",
  "fraud_proofs/script_integrity_hash_missing/step_02.main.spend",
  "fraud_proofs/script_integrity_hash_missing/step_03.main.spend",
  "fraud_proofs/script_integrity_hash_missing/script_grammar.main.spend",
  "fraud_proofs/script_integrity_hash_missing/script_scan.main.spend",
  "fraud_proofs/script_integrity_hash_missing/redeemer_grammar.main.spend",
  "fraud_proofs/script_integrity_hash_missing/step_04.main.spend",
] as const;

export type ScriptIntegrityHashMissingStepContractV1 = {
  readonly blueprintTitle?: string;
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
  readonly referenceOutRef?: string;
};

export type ScriptIntegrityHashMissingContractsV1 = {
  readonly steps: readonly [
    ScriptIntegrityHashMissingStepContractV1,
    ScriptIntegrityHashMissingStepContractV1,
    ScriptIntegrityHashMissingStepContractV1,
    ScriptIntegrityHashMissingStepContractV1,
    ScriptIntegrityHashMissingStepContractV1,
    ScriptIntegrityHashMissingStepContractV1,
    ScriptIntegrityHashMissingStepContractV1,
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
  readonly fieldPreimageCertificatePolicyId: string;
  readonly fieldPreimageCertificateMintingScript?: Script;
  readonly hubOraclePolicyId: string;
  readonly stateQueuePolicyId: string;
};

export type ScriptIntegrityHashMissingProductionManifestV1 = {
  readonly schemaVersion: "script-integrity-hash-missing-production-manifest-v1";
  readonly category: "scriptIntegrityHashMissing";
  readonly categoryId: string;
  readonly network: Network;
  readonly contracts: ScriptIntegrityHashMissingContractsV1;
};

const requireHex = (value: string, bytes: number, label: string): void => {
  if (!new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(value)) {
    throw new Error(
      `scriptIntegrityHashMissing: ${label} must be ${bytes.toString()} bytes of lowercase hex`,
    );
  }
};

/** Family-local loader. Registration is deliberately a separate central change. */
export const loadScriptIntegrityHashMissingProductionV1 = (
  manifest: ScriptIntegrityHashMissingProductionManifestV1,
): ScriptIntegrityHashMissingProductionManifestV1 => {
  if (
    manifest.schemaVersion !==
      "script-integrity-hash-missing-production-manifest-v1" ||
    manifest.category !== "scriptIntegrityHashMissing"
  ) {
    throw new Error(
      "scriptIntegrityHashMissing: wrong production manifest identity",
    );
  }
  requireHex(manifest.categoryId, 4, "category id");
  manifest.contracts.steps.forEach((step, index) => {
    if (
      step.blueprintTitle !==
      SCRIPT_INTEGRITY_HASH_MISSING_BLUEPRINT_TITLES_V1[index]
    ) {
      throw new Error(
        `scriptIntegrityHashMissing: physical step ${(index + 1).toString()} title mismatch`,
      );
    }
    if (
      validatorToScriptHash(step.spendingScript) !== step.spendingScriptHash
    ) {
      throw new Error(
        `scriptIntegrityHashMissing: physical step ${(index + 1).toString()} hash mismatch`,
      );
    }
    if (
      validatorToAddress(manifest.network, step.spendingScript) !==
      step.spendingScriptAddress
    ) {
      throw new Error(
        `scriptIntegrityHashMissing: physical step ${(index + 1).toString()} address mismatch`,
      );
    }
    if (
      step.referenceOutRef === undefined ||
      !/^[0-9a-f]{64}#[0-9]+$/u.test(step.referenceOutRef)
    ) {
      throw new Error(
        `scriptIntegrityHashMissing: physical step ${(index + 1).toString()} reference out-ref is not canonical`,
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
  requireHex(
    manifest.contracts.fieldPreimageCertificatePolicyId,
    28,
    "field certificate policy id",
  );
  requireHex(manifest.contracts.hubOraclePolicyId, 28, "hub-oracle policy id");
  requireHex(
    manifest.contracts.stateQueuePolicyId,
    28,
    "state-queue policy id",
  );
  return Object.freeze(manifest);
};
