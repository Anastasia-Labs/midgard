import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import { SCRIPT_INTEGRITY_HASH_MISSING_COMPLETE_CANONICAL_REPLAY_V1 } from "../workflow/complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  releaseFinalityAuthorityFromDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "../workflow/deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifierV1,
  createFraudProofFamilyLocalKupmiosL1ObservationPortV1,
  type FraudProofFamilyL1ObservationPortV1,
} from "../workflow/family-l1-observation-v1.js";
import {
  DirectoryFraudProofWorkflowJournalStoreV1,
  type FraudProofWorkflowJournalStoreV1,
} from "../workflow/journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfigV1 } from "../workflow/local-kupmios-http-ogmios-source-v1.js";
import {
  createFraudProofWorkflowRegistryV1,
  type FraudProofFamilyWorkflowAdapterV1,
  type FraudProofWorkflowRunResultV1,
  type FraudProofWorkflowTerminalVerifierV1,
  runFraudProofWorkflowFromRetainedDaV1,
} from "../workflow/orchestrator-v1.js";
import {
  assertProductionWorkflowJournalActuationV1,
  bindProductionWorkflowActuationJournalV1,
  productionWorkflowActuationDecisionDigestV1,
} from "../workflow/production-actuation-permit-v1.js";
import {
  PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
  type ProductionWorkflowAdapterReadinessInputV1,
  type ProductionWorkflowAdapterRunnerV1,
} from "../workflow/production-adapters-v1.js";
import {
  createProductionCursorFamilyWorkflowAdapterV1,
  type ProductionCursorFamilyTransactionPortV1,
} from "../workflow/production-cursor-family-adapter-v1.js";
import type { ProductionCursorFamilySpecV1 } from "../workflow/production-cursor-family-state-v1.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePortV1,
  type ProductionFieldCarriageRequirementV1,
  withProductionFieldCarriagePrerequisiteV1,
} from "../workflow/production-field-carriage-prerequisite-v1.js";
import { bindProductionWorkflowFundingReservationJournalV1 } from "../workflow/production-funding-reservation-permit-v1.js";
import type { FraudProofReleaseFinalityAuthorityV1 } from "../workflow/release-finality-policy-v1.js";
import type { ScriptIntegrityHashMissingContractsV1 } from "./contracts-v1.js";
import {
  createScriptIntegrityHashMissingTransactionPortV1,
  scriptIntegrityHashMissingFieldRequirementV1,
  type ScriptIntegrityHashMissingWorkflowReferenceScriptsV1,
} from "./production-actuator-v1.js";
import { admitProductionScriptIntegrityHashMissingArtifactV1 } from "./production-artifact-v1.js";
import { ScriptIntegrityStepDatumsV1 } from "./schemas-v1.js";

export const SCRIPT_INTEGRITY_HASH_MISSING_WORKFLOW_IDENTITY_V1 =
  "script-integrity-hash-missing-production-v1" as const;

export const SCRIPT_INTEGRITY_HASH_MISSING_MANIFEST_CONTRACTS_V1 =
  Object.freeze({
    steps: [
      "fraudProofScriptIntegrityHashMissing",
      "fraudProofScriptIntegrityHashMissingStep02",
      "fraudProofScriptIntegrityHashMissingStep03",
      "fraudProofScriptIntegrityHashMissingScriptGrammar",
      "fraudProofScriptIntegrityHashMissingScriptScan",
      "fraudProofScriptIntegrityHashMissingRedeemerGrammar",
      "fraudProofScriptIntegrityHashMissingStep04",
    ],
    witnesses: {
      computationThreadMint: "computationThreadMint",
      fraudProofMint: "fraudProofMint",
      phasMembershipWithdraw: "phasMembershipWithdraw",
      chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
      pexcludesWithdraw: "pexcludesWithdraw",
    },
    fieldPreimageCertificateMint: "fieldPreimageCertificateMint",
    removal: {
      correctionLockSpend: "correctionLockSpend",
      stateQueueSpend: "stateQueueSpend",
      stateQueueMint: "stateQueueMint",
      stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
      activeOperatorsSpend: "activeOperatorsSpend",
      activeOperatorsMint: "activeOperatorsMint",
      retiredOperatorsSpend: "retiredOperatorsSpend",
      retiredOperatorsMint: "retiredOperatorsMint",
      schedulerSpend: "schedulerSpend",
    },
  } as const);

export const SCRIPT_INTEGRITY_HASH_MISSING_CURSOR_SPEC_V1: ProductionCursorFamilySpecV1<"scriptIntegrityHashMissing"> =
  Object.freeze({
    category: "scriptIntegrityHashMissing",
    stepCount: 7,
    successors: Object.freeze({
      1: [2] as const,
      2: [3] as const,
      3: [4, 7] as const,
      4: [4, 5] as const,
      5: [5, 6] as const,
      6: [6, 7] as const,
      7: ["proof_token"] as const,
    }),
  });

export type { ScriptIntegrityHashMissingWorkflowReferenceScriptsV1 } from "./production-actuator-v1.js";

export type ScriptIntegrityHashMissingRemovalReferenceScriptsV1 = Readonly<{
  correctionLockSpend: UTxO;
  stateQueueSpend: UTxO;
  stateQueueMint: UTxO;
  stateQueueFraudRemovalWithdraw: UTxO;
  activeOperatorsSpend: UTxO;
  activeOperatorsMint: UTxO;
  retiredOperatorsSpend: UTxO;
  retiredOperatorsMint: UTxO;
  schedulerSpend: UTxO;
}>;

export type ManifestBoundScriptIntegrityHashMissingWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: ScriptIntegrityHashMissingWorkflowReferenceScriptsV1 &
    Readonly<{ removal: ScriptIntegrityHashMissingRemovalReferenceScriptsV1 }>;
}>;

export type ManifestBoundScriptIntegrityHashMissingWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"scriptIntegrityHashMissing">;
  l1: FraudProofFamilyL1ObservationPortV1<"scriptIntegrityHashMissing">;
  transactions: ProductionCursorFamilyTransactionPortV1<"scriptIntegrityHashMissing">;
  adapter: FraudProofFamilyWorkflowAdapterV1;
  terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
  decisionDigest: string;
}>;

export const createManifestBoundScriptIntegrityHashMissingWorkflowV1 = async (
  config: ManifestBoundScriptIntegrityHashMissingWorkflowConfigV1,
): Promise<ManifestBoundScriptIntegrityHashMissingWorkflowV1> => {
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("scriptIntegrityHashMissing decision digest is malformed");
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "scriptIntegrityHashMissing",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: ScriptIntegrityStepDatumsV1,
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.scriptIntegrityHashMissing;
  const certificate = binding.fieldPreimageCertificate;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    chain.steps.length !== 7 ||
    certificate === null ||
    stateQueuePolicyId === undefined
  )
    throw new Error(
      "scriptIntegrityHashMissing manifest omitted required contracts",
    );
  const ref = (name: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName: name,
      utxo,
    });
  const steps = SCRIPT_INTEGRITY_HASH_MISSING_MANIFEST_CONTRACTS_V1.steps.map(
    (name, index) => ref(name, config.referenceScripts.steps[index]!),
  ) as unknown as ScriptIntegrityHashMissingWorkflowReferenceScriptsV1["steps"];
  const witnesses = Object.fromEntries(
    Object.entries(
      SCRIPT_INTEGRITY_HASH_MISSING_MANIFEST_CONTRACTS_V1.witnesses,
    ).map(([role, name]) => [
      role,
      ref(
        name,
        config.referenceScripts.witnesses[
          role as keyof FaultProofWitnessReferenceScriptsV1
        ]!,
      ),
    ]),
  ) as Required<FaultProofWitnessReferenceScriptsV1>;
  for (const [role, name] of Object.entries(
    SCRIPT_INTEGRITY_HASH_MISSING_MANIFEST_CONTRACTS_V1.removal,
  ))
    ref(
      name,
      config.referenceScripts.removal[
        role as keyof ScriptIntegrityHashMissingRemovalReferenceScriptsV1
      ],
    );
  const references = Object.freeze({
    steps: Object.freeze(steps),
    witnesses: Object.freeze(witnesses),
    fieldPreimageCertificateMint: ref(
      SCRIPT_INTEGRITY_HASH_MISSING_MANIFEST_CONTRACTS_V1.fieldPreimageCertificateMint,
      config.referenceScripts.fieldPreimageCertificateMint,
    ),
  });
  const contracts: ScriptIntegrityHashMissingContractsV1 = Object.freeze({
    steps: chain.steps,
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: {
      policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
      mintingScript:
        binding.resolvedContracts.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
    },
    fieldPreimageCertificatePolicyId: certificate.policyId,
    fieldPreimageCertificateMintingScript: certificate.mintingScript,
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  if (l1.publications === undefined)
    throw new Error(
      "scriptIntegrityHashMissing raw L1 publication authority is unavailable",
    );
  const transactions = createScriptIntegrityHashMissingTransactionPortV1({
    binding,
    lucid: config.lucid,
    signer: config.signer,
    contracts,
    references,
    lease: config.stateQueueMutationLeaseCoordinator,
  });
  let adapter = createProductionCursorFamilyWorkflowAdapterV1({
    spec: SCRIPT_INTEGRITY_HASH_MISSING_CURSOR_SPEC_V1,
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  adapter = withProductionFieldCarriagePrerequisiteV1({
    category: "scriptIntegrityHashMissing",
    base: adapter,
    prerequisite: createAuthenticatedFieldCarriagePrerequisitePortV1({
      category: "scriptIntegrityHashMissing",
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications: l1.publications,
      requirementForAction: ({ action, artifact }) => {
        const planned = scriptIntegrityHashMissingFieldRequirementV1({
          actionStage: action.input.stage,
          artifact,
          owner: config.signer.paymentKeyHash,
        });
        if (planned === null) return null;
        const admitted = admitProductionScriptIntegrityHashMissingArtifactV1(
          artifact,
          config.signer.paymentKeyHash,
        );
        return {
          planned,
          compactCbor: admitted.evidence.nativeTxCompactCbor,
          witnessSetCompactCbor: admitted.evidence.witnessSetCompactCbor,
          certificate: {
            policyId: certificate.policyId,
            mintingScript: certificate.mintingScript,
            referenceScriptUtxo: references.fieldPreimageCertificateMint,
          },
        } satisfies ProductionFieldCarriageRequirementV1;
      },
      transactionConfirmed: async ({ headerHash, txHash }) =>
        await l1.transactionConfirmed({ headerHash, txHash }),
    }),
  });
  return Object.freeze({
    binding,
    l1,
    transactions,
    adapter,
    terminalVerifier:
      createFraudProofFamilyAuthenticatedL1TerminalVerifierV1(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBindingV1(binding),
    decisionDigest: config.decisionDigest,
  });
};

export const executeManifestBoundScriptIntegrityHashMissingWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundScriptIntegrityHashMissingWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
}): Promise<FraudProofWorkflowRunResultV1> => {
  if (
    productionWorkflowActuationDecisionDigestV1(journal) !==
    workflow.decisionDigest
  )
    throw new Error(
      "scriptIntegrityHashMissing journal actuation permit changed decision digest",
    );
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDaV1({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: SCRIPT_INTEGRITY_HASH_MISSING_COMPLETE_CANONICAL_REPLAY_V1,
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [workflow.adapter],
      launchScope: ["scriptIntegrityHashMissing"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};

export type LoadedScriptIntegrityHashMissingProductionWorkflowV1 = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundScriptIntegrityHashMissingWorkflowConfigV1;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadScriptIntegrityHashMissingProductionWorkflowV1 = (input: {
  readonly runtimeConfigPath: string;
  readonly invocation: ProductionWorkflowAdapterReadinessInputV1;
}) => Promise<LoadedScriptIntegrityHashMissingProductionWorkflowV1>;

/** Standard central-loader-compatible family runner; no evidence or actuator callbacks. */
export const createScriptIntegrityHashMissingProductionWorkflowRunnerSurfaceV1 =
  ({
    loadRuntimeConfig,
  }: {
    readonly loadRuntimeConfig: LoadScriptIntegrityHashMissingProductionWorkflowV1;
  }): ProductionWorkflowAdapterRunnerV1 =>
    Object.freeze({
      runnerVersion: PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
      runOrResume: async (invocation) => {
        if (invocation.category !== "scriptIntegrityHashMissing")
          throw new Error(
            `scriptIntegrityHashMissing production runner category mismatch: ${invocation.category}`,
          );
        const journal = bindProductionWorkflowFundingReservationJournalV1({
          permit: invocation.fundingReservationPermit,
          journal: bindProductionWorkflowActuationJournalV1({
            journal: new DirectoryFraudProofWorkflowJournalStoreV1(
              invocation.journalDirectory,
            ),
            permit: invocation.actuationPermit,
            decisionDigest: invocation.decisionDigest,
            deploymentFingerprint: invocation.deploymentFingerprint,
            category: "scriptIntegrityHashMissing",
            headerHash: invocation.headerHash,
          }),
        });
        assertProductionWorkflowJournalActuationV1({
          journal,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category: "scriptIntegrityHashMissing",
          headerHash: invocation.headerHash,
          checkpoint: "runner_start",
        });
        const loaded = await loadRuntimeConfig({
          runtimeConfigPath: invocation.runtimeConfigPath,
          invocation,
        });
        if (typeof loaded.close !== "function")
          throw new Error(
            "scriptIntegrityHashMissing runtime omitted its transport disposer",
          );
        try {
          if (
            loaded.schemaVersion !==
            "midgard-production-fraud-proof-runtime-config-v1"
          )
            throw new Error(
              "scriptIntegrityHashMissing runtime config has an unsupported schema",
            );
          if (
            loaded.retainedDaSources.length === 0 ||
            loaded.retainedDaSources.some(
              (source) => !(source instanceof DaLibp2pRetainedDaSource),
            )
          )
            throw new Error(
              "scriptIntegrityHashMissing runner requires concrete public retained-DA sources",
            );
          const workflow =
            await createManifestBoundScriptIntegrityHashMissingWorkflowV1(
              loaded.config,
            );
          if (
            workflow.binding.deploymentFingerprint !==
              invocation.deploymentFingerprint ||
            workflow.binding.definition.category !==
              "scriptIntegrityHashMissing" ||
            workflow.binding.definition.headerHash !== invocation.headerHash ||
            workflow.decisionDigest !== invocation.decisionDigest
          )
            throw new Error(
              "scriptIntegrityHashMissing manifest-bound workflow identity differs from invocation",
            );
          return await executeManifestBoundScriptIntegrityHashMissingWorkflowV1(
            {
              workflow,
              sources: loaded.retainedDaSources,
              journal,
            },
          );
        } finally {
          await loaded.close();
        }
      },
    });
