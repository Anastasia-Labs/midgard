import { credentialToAddress, type UTxO } from "@lucid-evolution/lucid";
import { vi } from "vitest";

import * as deployment from "../../src/workflow/deployment-manifest-binding.js";
import { FAMILY_APPLICATION_REGISTRY } from "../../src/workflow/family-application-registry.js";
import { familyStepContractNames } from "../../src/workflow/family-definition.js";
import * as observations from "../../src/workflow/family-l1-observation.js";
import { bindManifestBoundFamilyWorkflow } from "../../src/workflow/manifest-bound-family-assembly.js";
import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.js";
import { FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY } from "../../src/workflow/raw-l1-snapshot.js";
import type { customWorkflowRecoveryFixture } from "./custom-workflow-recovery.js";
import {
  fieldGrammarCertificateValidatorFixture,
  realFaultProofBlueprint,
} from "./field-grammar-certificate-deployment-fixture.js";

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
  const definition = FAMILY_APPLICATION_REGISTRY[category].definition;
  if (definition === undefined)
    throw new Error(
      `Recovery fixture has no family definition for ${category}`,
    );
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
  const certificatePolicyId = "55".repeat(28);
  // A family that reads through the field grammar certificate resolves the
  // real certificate at startup: its published script and minting policy are
  // the blueprint's over this deployment's field-preimage certificate.
  const grammar =
    definition.fieldGrammarCertificate === true
      ? (() => {
          const validator = fieldGrammarCertificateValidatorFixture({
            network: "Preprod",
            fieldPreimageCertificatePolicyId: certificatePolicyId,
          });
          const outRef = `${"33".repeat(32)}#0`;
          return {
            blueprint: realFaultProofBlueprint(),
            referenceScriptsByContract: {
              fieldGrammarCertificateSpend: {
                outRef,
                scriptHash: validator.spendingScriptHash,
              },
              fieldGrammarCertificateMint: {
                outRef,
                scriptHash: validator.policyId,
              },
            },
            reference: {
              ...reference,
              txHash: "33".repeat(32),
              scriptRef: validator.spendingScript,
            } satisfies UTxO,
          };
        })()
      : undefined;
  const binding = {
    ...workflow.binding,
    network: "Preprod",
    fieldPreimageCertificate: {
      policyId: certificatePolicyId,
      mintingScript: script,
    },
    cardanoProtocolParameters: { maxTxSize: 16384 },
    ...(grammar === undefined
      ? {}
      : {
          blueprint: grammar.blueprint,
          referenceScriptsByContract: {
            ...(workflow.binding as { referenceScriptsByContract?: object })
              .referenceScriptsByContract,
            ...grammar.referenceScriptsByContract,
          },
        }),
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
    .spyOn(observations, "createFraudProofFamilyL1ObservationPort")
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
          ...(grammar === undefined
            ? {}
            : { fieldGrammarCertificateSpend: grammar.reference }),
        },
        auxiliaryReferenceScripts: Object.fromEntries(
          Object.keys(definition.auxiliaryReferenceScripts ?? {}).map(
            (role) => [role, reference],
          ),
        ),
        l1Source: {},
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

/** Runs a family's shared executor over a saved-journal fixture as read-only
 * recovery, under a real assembly context. */
export const runBoundFamilyRecovery = async (
  execute: unknown,
  input: Awaited<ReturnType<typeof customWorkflowRecoveryFixture>>,
): Promise<FraudProofWorkflowRunResult> =>
  await (execute as (value: unknown) => Promise<FraudProofWorkflowRunResult>)({
    ...input,
    workflow: {
      ...input.workflow,
      deployment: await bindFamilyRecoveryFixture(input.workflow),
      config: {
        binding: input.workflow.binding,
        // Read-only recovery must never use a provider or these builder-only inputs.
        lucid: {},
        signer: {
          paymentKeyHash: "aa".repeat(28),
          address: "unused-readonly-address",
        },
        contracts: {},
        referenceScripts: {},
      },
    },
    decisionDigest: input.workflow.decisionDigest,
    sources: [],
  });
