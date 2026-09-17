import { credentialToAddress, type UTxO } from "@lucid-evolution/lucid";
import { vi } from "vitest";

import * as deployment from "../../src/workflow/deployment-manifest-binding.js";
import { familyStepContractNames } from "../../src/workflow/family-definition.js";
import { FAMILY_DEFINITIONS } from "../../src/workflow/family-definitions.js";
import * as observations from "../../src/workflow/family-l1-observation.js";
import { bindManifestBoundFamilyWorkflow } from "../../src/workflow/manifest-bound-family-assembly.js";
import { FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY } from "../../src/workflow/raw-l1-snapshot.js";
import type { customWorkflowRecoveryFixture } from "./custom-workflow-recovery.js";

/**
 * Acquire a real assembly context for a saved-journal fixture. Manifest loading,
 * script identities and provider creation are isolated here; the binding and
 * its definition identity remain owned by the production assembly.
 */
export const bindFamilyRecoveryFixture = async (
  workflow: Awaited<
    ReturnType<typeof customWorkflowRecoveryFixture>
  >["workflow"],
) => {
  const category = workflow.binding.definition.category;
  if (!(category in FAMILY_DEFINITIONS))
    throw new Error(
      `Recovery fixture has no family definition for ${category}`,
    );
  const definition =
    FAMILY_DEFINITIONS[category as keyof typeof FAMILY_DEFINITIONS];
  const owner = "aa".repeat(28);
  const address = credentialToAddress("Preprod", { type: "Key", hash: owner });
  const script = { type: "PlutusV3", script: "49480100002221200101" } as const;
  const reference: UTxO = {
    txHash: "22".repeat(32),
    outputIndex: 0,
    address,
    assets: { lovelace: 2_000_000n },
    scriptRef: script,
  };
  const binding = {
    ...workflow.binding,
    network: "Preprod",
    fieldPreimageCertificate: {
      policyId: "55".repeat(28),
      mintingScript: script,
    },
    cardanoProtocolParameters: { maxTxSize: 16384 },
  };
  const l1 = {
    ...workflow.l1,
    rawL1: { authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY },
  };
  const bind = vi
    .spyOn(deployment, "bindFraudProofWorkflowDeployment")
    .mockResolvedValue(binding as never);
  const references = vi
    .spyOn(deployment, "requireManifestBoundReferenceScriptUtxo")
    .mockImplementation(({ utxo }) => utxo);
  const observe = vi
    .spyOn(observations, "createFraudProofFamilyLocalKupmiosL1ObservationPort")
    .mockReturnValue(l1 as never);
  try {
    return await bindManifestBoundFamilyWorkflow(
      definition as never,
      {
        manifest: {},
        blueprintJson: "{}",
        deploymentInfo: {},
        headerHash: workflow.binding.definition.headerHash,
        lucid: {},
        signer: { paymentKeyHash: owner, address },
        referenceScripts: {
          steps: familyStepContractNames(definition).map(() => reference),
          witnesses: Object.fromEntries(
            definition.witnessRoles.map((role) => [role, reference]),
          ),
          fieldPreimageCertificateMint: reference,
        },
        auxiliaryReferenceScripts: Object.fromEntries(
          Object.keys(definition.auxiliaryReferenceScripts ?? {}).map(
            (role) => [role, reference],
          ),
        ),
        source: {},
        stateQueueMutationLeaseCoordinator:
          workflow.stateQueueMutationLeaseCoordinator,
      } as never,
    );
  } finally {
    bind.mockRestore();
    references.mockRestore();
    observe.mockRestore();
  }
};
