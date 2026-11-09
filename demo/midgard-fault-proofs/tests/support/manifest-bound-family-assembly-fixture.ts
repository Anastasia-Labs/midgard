import {
  credentialToAddress,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { type Mock, type MockInstance, vi } from "vitest";

import * as inspection from "../../src/inspect-contracts.js";
import * as deployment from "../../src/workflow/deployment-manifest-binding.js";
import {
  type FamilyCategory,
  familyStepContractNames,
} from "../../src/workflow/family-definition.js";
import * as observations from "../../src/workflow/family-l1-observation.js";
import { FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT } from "../../src/workflow/family-l1-observation.js";
import * as fieldCarriage from "../../src/workflow/field-carriage-prerequisite.js";
import { assembleManifestBoundFamilyWorkflow } from "../../src/workflow/manifest-bound-family-assembly.js";
import * as proofChunks from "../../src/workflow/proof-chunk-prerequisite.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../../src/workflow/raw-l1-publication-observation.js";
import { FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY } from "../../src/workflow/raw-l1-snapshot.js";
import { cursorAssemblyRuntimeFixture } from "./cursor-assembly-runtime-fixtures.js";
import { createAuthenticatedFamilyAssemblyRuntimeFixture } from "./family-assembly-runtime-fixtures.js";
import {
  fieldGrammarCertificateValidatorFixture,
  realFaultProofBlueprint,
} from "./field-grammar-certificate-deployment-fixture.js";
import { REGISTERED_FAMILY_DEFINITIONS } from "./registered-family-definitions.js";

export const categories = [...REGISTERED_FAMILY_DEFINITIONS.keys()];
export const definitionOf = (category: FamilyCategory) =>
  REGISTERED_FAMILY_DEFINITIONS.get(category)!;
const OWNER = "11".repeat(28);
export const HEADER = "aa".repeat(28);
export const SCRIPT = {
  type: "PlutusV3",
  script: "49480100002221200101",
} as const;
export const CERTIFICATE_POLICY = "55".repeat(28);

/**
 * A fake deployment binding that resolves every reference script the
 * definition can ask for, plus a fake L1 port whose provider never starts.
 * Verified-manifest loading and provider startup are isolated; the assembly,
 * the exact manifest out-ref/script checks and the family's own port factory
 * stay real. Step contract names come from whichever spec the definition's
 * arm uses.
 */
export const fixture = (category: FamilyCategory) => {
  const definition = definitionOf(category);
  const stepNames = familyStepContractNames(definition);
  const address = credentialToAddress("Preprod", { type: "Key", hash: OWNER });
  // A definition that reads through the field grammar certificate binds the
  // compiled certificate over the manifest's field-preimage policy, so its
  // fixture publishes exactly that script.
  const grammar =
    definition.fieldGrammarCertificate === true
      ? fieldGrammarCertificateValidatorFixture({
          network: "Preprod",
          fieldPreimageCertificatePolicyId: CERTIFICATE_POLICY,
        })
      : undefined;
  const names = [
    ...stepNames,
    ...definition.witnessRoles,
    "fieldPreimageCertificateMint",
    ...(grammar === undefined ? [] : ["fieldGrammarCertificateSpend"]),
    ...Object.values(definition.auxiliaryReferenceScripts ?? {}),
  ];
  const scriptOf = (name: string) =>
    name === "fieldGrammarCertificateSpend" && grammar !== undefined
      ? grammar.spendingScript
      : SCRIPT;
  const utxoAt = (index: number): UTxO => ({
    txHash: "22".repeat(32),
    outputIndex: index,
    address,
    assets: { lovelace: 2_000_000n },
    scriptRef: scriptOf(names[index]!),
  });
  const references = new Map(names.map((name, index) => [name, utxoAt(index)]));
  const steps = stepNames.map((name) => references.get(name)!);
  const witnesses = Object.fromEntries(
    definition.witnessRoles.map((role) => [role, references.get(role)!]),
  );
  const contractSteps = steps.map((_step, index) => ({
    spendingScriptAddress: address,
    spendingScriptHash: `${index.toString().padStart(2, "0")}`.repeat(28),
  }));
  const binding = {
    deploymentFingerprint: "44".repeat(32),
    network: "Preprod",
    blueprint: grammar === undefined ? {} : realFaultProofBlueprint(),
    deploymentInfo: {},
    releaseFinality: { policy: { confirmationDepth: 1 } },
    releaseEconomics: { policy: { fraudProverRewardLovelace: "1000000" } },
    cardanoProtocolParameters: { maxTxSize: 16384 },
    fieldPreimageCertificate: {
      policyId: CERTIFICATE_POLICY,
      mintingScript: SCRIPT,
    },
    referenceScriptsByContract: Object.fromEntries([
      ...names.map((name) => {
        const utxo = references.get(name)!;
        return [
          name,
          {
            outRef: `${utxo.txHash}#${utxo.outputIndex.toString()}`,
            scriptHash: validatorToScriptHash(scriptOf(name)),
          },
        ];
      }),
      ...(grammar === undefined
        ? []
        : [
            [
              "fieldGrammarCertificateMint",
              { outRef: `${"23".repeat(32)}#0`, scriptHash: grammar.policyId },
            ],
          ]),
    ]),
    definition: { category, headerHash: HEADER, proverCredential: OWNER },
    contractEntries: {
      hubOracleMint: { scriptHash: "88".repeat(28) },
      stateQueueMint: { scriptHash: "33".repeat(28) },
    },
    resolvedContracts: {
      contracts: {
        [category]: {
          steps: contractSteps,
          firstStep: contractSteps[0],
          acceptedStep02: contractSteps[1],
          forcedStep02: contractSteps[2],
          certificateStep03: contractSteps[3],
        },
        computationThread: { policyId: "66".repeat(28) },
        fraudProof: {
          policyId: "77".repeat(28),
          mintingScript: SCRIPT,
          spendingScriptAddress: address,
        },
      },
      stateQueuePolicyId: "33".repeat(28),
      hubOraclePolicyId: "88".repeat(28),
      category: { categoryId: "0000000b" },
    },
  };
  vi.spyOn(deployment, "bindFraudProofWorkflowDeployment").mockResolvedValue(
    binding as unknown as Awaited<
      ReturnType<typeof deployment.bindFraudProofWorkflowDeployment>
    >,
  );
  // Spies are typed through vitest's exported names, so the fixture's
  // inferred type stays portable.
  const bindReference: MockInstance<
    typeof deployment.requireManifestBoundReferenceScriptUtxo
  > = vi.spyOn(deployment, "requireManifestBoundReferenceScriptUtxo");
  const observation = { fake: "observation" };
  const l1 = {
    portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
    category,
    rawL1: { authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY },
    publications: {
      observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
      observeExact: vi.fn() as Mock,
    },
    observeHeader: vi.fn(async () => observation) as Mock,
    observeBoundary: vi.fn() as Mock,
    transactionConfirmed: vi.fn(async () => false) as Mock,
    observe: vi.fn() as Mock,
  };
  const observe: MockInstance<
    typeof observations.createFraudProofFamilyL1ObservationPort
  > = vi
    .spyOn(observations, "createFraudProofFamilyL1ObservationPort")
    .mockReturnValue(l1 as never);
  const withField: MockInstance<
    typeof fieldCarriage.withFieldCarriagePrerequisite
  > = vi.spyOn(fieldCarriage, "withFieldCarriagePrerequisite");
  const withChunks: MockInstance<
    typeof proofChunks.withProofChunkPrerequisite
  > = vi.spyOn(proofChunks, "withProofChunkPrerequisite");
  const config = {
    manifest: {},
    blueprintJson: "{}",
    deploymentInfo: {},
    headerHash: HEADER,
    lucid: { fake: "lucid" },
    signer: { address, paymentKeyHash: OWNER },
    referenceScripts: {
      steps,
      witnesses,
      fieldPreimageCertificateMint: references.get(
        "fieldPreimageCertificateMint",
      )!,
      ...(grammar === undefined
        ? {}
        : {
            fieldGrammarCertificateSpend: references.get(
              "fieldGrammarCertificateSpend",
            )!,
          }),
    },
    l1Source: {},
    auxiliaryReferenceScripts: Object.fromEntries(
      Object.entries(definition.auxiliaryReferenceScripts ?? {}).map(
        ([role, contractName]) => [role, references.get(contractName)!],
      ),
    ),
    stateQueueMutationLeaseCoordinator: { acquire: vi.fn() as Mock },
  };
  vi.spyOn(
    inspection,
    "parseContractDeploymentReferenceScriptAuthPolicyId",
  ).mockReturnValue("ab".repeat(28));
  const runtime =
    createAuthenticatedFamilyAssemblyRuntimeFixture(category, {
      binding,
      config,
    }) ??
    cursorAssemblyRuntimeFixture({ category, binding, l1, config }) ??
    (category === "minAda"
      ? {}
      : category === "transitionTrace"
        ? { config, cell: {} }
        : undefined);
  const assemble = () =>
    assembleManifestBoundFamilyWorkflow(
      definition as never,
      config as never,
      runtime,
    );
  return {
    definition,
    runtime,
    stepNames,
    binding,
    config,
    references,
    l1,
    observation,
    assemble,
    bindReference,
    observe,
    withField,
    withChunks,
  };
};
