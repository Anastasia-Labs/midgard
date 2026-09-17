import type { UTxO } from "@lucid-evolution/lucid";

import { FIELD_ITEM_WIDTH_ILLEGAL_WORKFLOW } from "../../src/field-item-width-illegal/workflow.js";
import { OUTPUT_REFERENCE_SCRIPT_DECODING_WORKFLOW } from "../../src/output-reference-script-decoding/authenticated-workflow.js";
import { PROTECTED_OUTPUT_SIGNER_MISSING_WORKFLOW } from "../../src/protected-output-signer-missing/authenticated-workflow.js";
import { SPEND_INPUT_SIGNER_MISSING_WORKFLOW } from "../../src/spend-input-signer-missing/authenticated-workflow.js";
import { createCanonicalFamilyArtifactPort } from "../../src/workflow/manifest-bound-family-recovery.js";

const versions = {
  fieldItemWidthIllegal: FIELD_ITEM_WIDTH_ILLEGAL_WORKFLOW,
  outputReferenceScriptDecoding: OUTPUT_REFERENCE_SCRIPT_DECODING_WORKFLOW,
  protectedOutputSignerMissing: PROTECTED_OUTPUT_SIGNER_MISSING_WORKFLOW,
  spendInputSignerMissing: SPEND_INPUT_SIGNER_MISSING_WORKFLOW,
} as const;

type AssemblyFixture = Readonly<{
  binding: {
    resolvedContracts: { contracts: Readonly<Record<string, unknown>> };
    contractEntries: {
      hubOracleMint: { scriptHash: string };
      stateQueueMint: { scriptHash: string };
    };
    fieldPreimageCertificate: {
      policyId: string;
      mintingScript: unknown;
    } | null;
  };
  config: {
    lucid: unknown;
    signer: unknown;
    referenceScripts: {
      steps: readonly UTxO[];
      witnesses: unknown;
      fieldPreimageCertificateMint: UTxO;
    };
  };
}>;

/**
 * Per-run family inputs for the shared assembly table. The real material port
 * deliberately refuses evidence derivation: these tests exercise assembly and
 * delegation, while each family's replay and lifecycle tests admit real data.
 */
export const createAuthenticatedFamilyAssemblyRuntimeFixture = (
  category: string,
  { binding, config }: AssemblyFixture,
) => {
  if (!Object.prototype.hasOwnProperty.call(versions, category))
    return undefined;
  const referenceScripts = {
    ...Object.fromEntries(
      config.referenceScripts.steps.map((utxo, index) => [
        `step${(index + 1).toString().padStart(2, "0")}`,
        utxo,
      ]),
    ),
    witnesses: config.referenceScripts.witnesses,
    fieldPreimageCertificateMint:
      config.referenceScripts.fieldPreimageCertificateMint,
  };
  return {
    config: {
      schemaVersion: versions[category as keyof typeof versions],
      lucid: config.lucid,
      signer: config.signer,
      binding,
      contracts: {
        ...(binding.resolvedContracts.contracts[category] as object),
        computationThread:
          binding.resolvedContracts.contracts.computationThread,
        fraudProof: binding.resolvedContracts.contracts.fraudProof,
        hubOraclePolicyId: binding.contractEntries.hubOracleMint.scriptHash,
        stateQueuePolicyId: binding.contractEntries.stateQueueMint.scriptHash,
        fieldPreimageCertificatePolicyId:
          binding.fieldPreimageCertificate?.policyId,
        fieldPreimageCertificateMintingScript:
          binding.fieldPreimageCertificate?.mintingScript,
      },
      referenceScripts,
    },
    material: createCanonicalFamilyArtifactPort(async () => {
      throw new Error(
        "Assembly fixture cannot derive canonical fault-proof evidence",
      );
    }),
  };
};
