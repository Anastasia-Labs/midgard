import { parseFaultProofBlueprint } from "@al-ft/midgard-sdk/core";
import {
  buildScriptDecodeValidator,
  SCRIPT_DECODE_STAKER_CLASS_NAMES,
  SCRIPT_DECODE_STAKER_MEMBERS,
} from "@al-ft/midgard-sdk/fraud-proof";
import { credentialToAddress, type UTxO } from "@lucid-evolution/lucid";
import { vi } from "vitest";

import { proofMaterialsAddress } from "../../src/funding/proof-materials-address.js";
import { DaLibp2pRetainedDaSource } from "../../src/transition-trace/fetch.da-libp2p-retained-da-source.js";
import * as deployment from "../../src/workflow/deployment-manifest-binding.js";
import { FAMILY_APPLICATION_REGISTRY } from "../../src/workflow/family-application-registry.family-application-registry.js";
import { familyStepContractNames } from "../../src/workflow/family-definition.family-definition.js";
import * as observations from "../../src/workflow/family-l1-observation.js";
import { bindManifestBoundFamilyWorkflow } from "../../src/workflow/manifest-bound-family-assembly.assemble-bound-manifest-bound-family-workflow.js";
import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.js";
import { FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY } from "../../src/workflow/raw-l1-snapshot.compute-fraud-proof-raw-l1-snapshot-evidence-digest.js";
import {
  WORKFLOW_RUNNER_FACTORIES,
  WORKFLOW_RUNTIME_CONFIG,
} from "../../src/workflow/runtime.js";
import { SCRIPT_DECODE_SPEND_ROLE } from "../../src/workflow/script-decode-prerequisite.deployment.js";
import type { customWorkflowRecoveryFixture } from "./custom-workflow-recovery.js";
import { familyCommonInfrastructureForTest } from "./family-common-infrastructure.js";
import {
  fieldGrammarCertificateValidatorFixture,
  realFaultProofBlueprint,
} from "./field-grammar-certificate-deployment-fixture.js";
import { WALLET_ONLY_SEND_ALWAYS_OPEN } from "./wallet-only-send-always-open.js";
import { createTestWorkflowProcess } from "./workflow-process.js";

type RecoveryFixture = Awaited<
  ReturnType<typeof customWorkflowRecoveryFixture>
>;

const owner = "aa".repeat(28);
const address = credentialToAddress("Preprod", { type: "Key", hash: owner });
const signer = {
  paymentKeyHash: owner,
  address,
  proofMaterialsAddress: proofMaterialsAddress("Preprod", owner),
};
const script = { type: "PlutusV3", script: "49480100002221200101" } as const;
const reference: UTxO = {
  txHash: "22".repeat(32),
  outputIndex: 0,
  address,
  assets: { lovelace: 2_000_000n },
  scriptRef: script,
};
const certificatePolicyId = "55".repeat(28);

/**
 * Stands in for the manifest binding, the reference-script UTxO check and the
 * L1 observation port of a recovery fixture's deployment until
 * `restore`. `binding` extends the bound deployment, as a family whose
 * constructor reads more of it needs.
 */
const standInFamilyDeployment = (
  workflow: RecoveryFixture["workflow"],
  binding: Readonly<Record<string, unknown>> = {},
) => {
  const category = workflow.binding.definition.category;
  const definition = FAMILY_APPLICATION_REGISTRY[category].definition;
  if (definition === undefined)
    throw new Error(
      `Recovery fixture has no family definition for ${category}`,
    );
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
  const bound = {
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
    ...binding,
  };
  const l1 = {
    ...workflow.l1,
    rawL1: { authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY },
  };
  const bind = vi
    .spyOn(deployment, "bindFraudProofWorkflowDeployment")
    .mockResolvedValue(bound as never);
  const references = vi
    .spyOn(deployment, "requireManifestBoundReferenceScriptUtxo")
    .mockImplementation(({ utxo }) => utxo);
  const observe = vi
    .spyOn(observations, "createFraudProofFamilyL1ObservationPort")
    .mockReturnValue(l1 as never);
  return {
    definition,
    grammar,
    restore: () => {
      bind.mockRestore();
      references.mockRestore();
      observe.mockRestore();
    },
  };
};

/**
 * Acquire a real assembly context for a recovery fixture. Manifest loading,
 * script identities and provider creation are isolated here; the binding and
 * its definition identity remain owned by the production assembly.
 */
export const bindFamilyRecoveryFixture = async (
  workflow: RecoveryFixture["workflow"],
) => {
  const { definition, grammar, restore } = standInFamilyDeployment(workflow);
  try {
    return await bindManifestBoundFamilyWorkflow(
      definition as never,
      {
        manifest: {},
        blueprintJson: "{}",
        deploymentInfo: {},
        headerHash: workflow.binding.definition.headerHash,
        lucid: {},
        signer,
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
    restore();
  }
};

/** The script-decode members a family that records script decodes resolves:
 * the blueprint's validator and stakers, each as its published reference. */
const scriptDecodeReferences = (): Readonly<Record<string, UTxO>> => {
  const validator = buildScriptDecodeValidator(
    parseFaultProofBlueprint(realFaultProofBlueprint()),
    "Preprod",
  );
  return Object.fromEntries([
    [
      SCRIPT_DECODE_SPEND_ROLE,
      { ...reference, scriptRef: validator.spendingScript },
    ],
    ...SCRIPT_DECODE_STAKER_CLASS_NAMES.map((name) => [
      SCRIPT_DECODE_STAKER_MEMBERS[name],
      { ...reference, scriptRef: validator.stakers[name].withdrawalScript },
    ]),
  ]);
};

/**
 * Runs a recovery fixture as a restarted watcher does under reconciliation
 * authority: the family's registered runner, under the fixture's
 * reconciliation-only permit and over a fresh process's in-flight set,
 * binds the family's config from the loaded infrastructure and builds its
 * workflow with the family's own constructor. Only the manifest binding,
 * the reference-script UTxOs and the L1 observation port are stood in for.
 */
export const prepareRegisteredFamilyReconciliation = (
  input: RecoveryFixture,
  {
    decisionDigest = input.decision.decisionDigest,
  }: Readonly<{
    /** The decision the invocation, and the infrastructure loaded for it,
     * claim; the fixture's permit admitted `input.decision`. */
    decisionDigest?: string;
  }> = {},
): (() => Promise<FraudProofWorkflowRunResult>) => {
  const { binding } = input.workflow;
  const { category, headerHash } = binding.definition;
  const { deploymentFingerprint } = binding;
  // The invocation is ready now: a permit revoked after this point is
  // refused by the runner itself.
  const sends = createTestWorkflowProcess().sends({ category, headerHash });
  return async () => {
    const stepCount = familyStepContractNames(
      FAMILY_APPLICATION_REGISTRY[category].definition!,
    ).length;
    const { grammar, restore } = standInFamilyDeployment(input.workflow, {
      // The chain the family's constructor lays its contracts out from.
      resolvedContracts: {
        contracts: {
          [category]: {
            steps: Array.from({ length: stepCount }, () => ({})),
          },
          computationThread: {},
          fraudProof: {},
        },
        // The queue a decision-digest family removes the faulty header from.
        stateQueuePolicyId: "77".repeat(28),
      },
      contractEntries: { hubOracleMint: { scriptHash: "66".repeat(28) } },
      definition: {
        ...binding.definition,
        stateQueue: { policyId: "77".repeat(28) },
      },
    });
    try {
      const published: Readonly<Record<string, UTxO>> = {
        ...scriptDecodeReferences(),
        ...(grammar === undefined
          ? {}
          : { fieldGrammarCertificateSpend: grammar.reference }),
      };
      const runner = WORKFLOW_RUNNER_FACTORIES[category](async () => ({
        schemaVersion: WORKFLOW_RUNTIME_CONFIG,
        infrastructure: familyCommonInfrastructureForTest(
          { headerHash, decisionDigest },
          {
            signer: signer as never,
            stateQueueMutationLeaseCoordinator: input.workflow
              .stateQueueMutationLeaseCoordinator as never,
          },
        ),
        resolveReferenceScript: async ({ contractName }) =>
          published[contractName] ?? reference,
        // Reconciliation reads no retained DA.
        retainedDaSources: [
          new DaLibp2pRetainedDaSource({
            deploymentFingerprint,
            peers: [{ peerId: "12D3KooWreconciliationOnly" }],
            transport: {
              request: async () => {
                throw new Error("reconciliation fetched retained DA");
              },
            },
          }),
        ],
        close: async () => undefined,
      }));
      return (await runner.runOrResume({
        walletOnlySend: WALLET_ONLY_SEND_ALWAYS_OPEN,
        category,
        deploymentFingerprint,
        headerHash,
        decisionDigest,
        actuationPermit: input.controller.permit,
        sends,
        runtimeConfigPath: "/fixture/reconciliation-runtime.json",
      })) as FraudProofWorkflowRunResult;
    } finally {
      restore();
    }
  };
};

/** Prepares and at once runs a registered family's reconciliation. */
export const runRegisteredFamilyReconciliation = async (
  input: RecoveryFixture,
  options?: Parameters<typeof prepareRegisteredFamilyReconciliation>[1],
): Promise<FraudProofWorkflowRunResult> =>
  await prepareRegisteredFamilyReconciliation(input, options)();
