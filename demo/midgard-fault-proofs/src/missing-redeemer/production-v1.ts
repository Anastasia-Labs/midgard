import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { fetchCanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "../workflow/deployment-manifest-binding-v1.js";
import { createFraudProofFamilyLocalKupmiosL1ObservationPortV1 } from "../workflow/family-l1-observation-v1.js";
import {
  computeFraudProofWorkflowIdV1,
  DirectoryFraudProofWorkflowJournalStoreV1,
  FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
  type FraudProofWorkflowIdentityV1,
  type FraudProofWorkflowJournalEventV1,
  type FraudProofWorkflowJournalStoreV1,
} from "../workflow/journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfigV1 } from "../workflow/local-kupmios-http-ogmios-source-v1.js";
import { bindProductionWorkflowActuationJournalV1 } from "../workflow/production-actuation-permit-v1.js";
import {
  PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
  type ProductionWorkflowAdapterReadinessInputV1,
  type ProductionWorkflowAdapterRunnerV1,
} from "../workflow/production-adapters-v1.js";
import { bindProductionWorkflowFundingReservationJournalV1 } from "../workflow/production-funding-reservation-permit-v1.js";
import { submitCapturedTransactionV1 } from "../workflow/transaction-boundary-v1.js";
import {
  MISSING_REDEEMER_BLUEPRINT_TITLES_V1,
  type MissingRedeemerContractsV1,
} from "./contracts-v1.js";
import { createMissingRedeemerDirectoryJournalV1 } from "./directory-journal-v1.js";
import {
  MISSING_REDEEMER_CATEGORY_V1,
  missingRedeemerEvidenceIdentityV1,
} from "./family-v1.js";
import {
  createMissingRedeemerActuatorV1,
  type MissingRedeemerActuatorActionV1,
  type MissingRedeemerWorkflowReferencesV1,
} from "./production-actuator-v1.js";
import {
  type MissingRedeemerProductionArtifactV1,
  replayMissingRedeemerProductionV1,
} from "./production-replay-v1.js";
import {
  MissingRedeemerStep02aDatumV1Schema,
  MissingRedeemerStep02bDatumV1Schema,
  MissingRedeemerStep02DatumV1Schema,
  MissingRedeemerStep03DatumV1Schema,
  MissingRedeemerStep04DatumV1Schema,
  MissingRedeemerStep05DatumV1Schema,
} from "./schemas-v1.js";
import { planMissingRedeemerStagedWalkV1 } from "./staged-plan-v1.js";
import type { MissingRedeemerDurableStateV1 } from "./workflow-v1.js";
import { runMissingRedeemerWorkflowV1 } from "./workflow-v1.js";

export const MISSING_REDEEMER_PRODUCTION_CONFIG_KEYS_V1 = Object.freeze([
  "manifest",
  "blueprintJson",
  "deploymentInfo",
  "headerHash",
  "lucid",
  "signer",
  "source",
  "decisionDigest",
  "stateQueueMutationLeaseCoordinator",
  "referenceScripts",
] as const);

export const MISSING_REDEEMER_STEP_DATUM_SCHEMAS_V1 = Object.freeze([
  FraudProofComputationThreadStepDatum,
  MissingRedeemerStep02DatumV1Schema,
  MissingRedeemerStep02aDatumV1Schema,
  MissingRedeemerStep02bDatumV1Schema,
  MissingRedeemerStep03DatumV1Schema,
  MissingRedeemerStep04DatumV1Schema,
  MissingRedeemerStep05DatumV1Schema,
] as const);

export type MissingRedeemerRemovalReferenceScriptsV1 = Readonly<{
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

export type MissingRedeemerWorkflowReferenceScriptsV1 = Readonly<{
  steps: MissingRedeemerWorkflowReferencesV1["steps"];
  witnesses: Required<FaultProofWitnessReferenceScriptsV1>;
  fieldPreimageCertificateMint: UTxO;
  removal: MissingRedeemerRemovalReferenceScriptsV1;
}>;

export type ManifestBoundMissingRedeemerWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: MissingRedeemerWorkflowReferenceScriptsV1;
}>;

export type ManifestBoundMissingRedeemerWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"missingRedeemer">;
  lucid: LucidEvolution;
  decisionDigest: string;
  l1: ReturnType<typeof createFraudProofFamilyLocalKupmiosL1ObservationPortV1>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  actuator: ReturnType<typeof createMissingRedeemerActuatorV1>;
}>;

const manifestContracts = Object.freeze({
  steps: [
    "fraudProofMissingRedeemer",
    "fraudProofMissingRedeemerStep02",
    "fraudProofMissingRedeemerStep02a",
    "fraudProofMissingRedeemerStep02b",
    "fraudProofMissingRedeemerStep03",
    "fraudProofMissingRedeemerStep04",
    "fraudProofMissingRedeemerStep05",
  ],
  witnesses: {
    computationThreadMint: "computationThreadMint",
    fraudProofMint: "fraudProofMint",
    phasMembershipWithdraw: "phasMembershipWithdraw",
    chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
    pexcludesWithdraw: "pexcludesWithdraw",
  },
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

/** Strict manifest-bound construction; proof evidence is never configurable. */
export const createManifestBoundMissingRedeemerWorkflowV1 = async (
  config: ManifestBoundMissingRedeemerWorkflowConfigV1,
): Promise<ManifestBoundMissingRedeemerWorkflowV1> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...MISSING_REDEEMER_PRODUCTION_CONFIG_KEYS_V1].sort().join("\0")
  )
    throw new Error(
      "missingRedeemer production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("missingRedeemer decision digest is malformed");
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: MISSING_REDEEMER_CATEGORY_V1,
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: MISSING_REDEEMER_STEP_DATUM_SCHEMAS_V1,
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.missingRedeemer;
  const certificate = binding.fieldPreimageCertificate;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    chain.steps.length !== 7 ||
    certificate === null ||
    stateQueuePolicyId === undefined
  )
    throw new Error("missingRedeemer manifest omitted required contracts");
  const bind = (name: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName: name,
      utxo,
    });
  const steps = manifestContracts.steps.map((name, index) =>
    bind(name, config.referenceScripts.steps[index]!),
  ) as unknown as MissingRedeemerWorkflowReferencesV1["steps"];
  const witnesses = Object.fromEntries(
    Object.entries(manifestContracts.witnesses).map(([role, name]) => [
      role,
      bind(
        name,
        config.referenceScripts.witnesses[
          role as keyof FaultProofWitnessReferenceScriptsV1
        ]!,
      ),
    ]),
  ) as Required<FaultProofWitnessReferenceScriptsV1>;
  for (const [role, name] of Object.entries(manifestContracts.removal))
    bind(
      name,
      config.referenceScripts.removal[
        role as keyof MissingRedeemerRemovalReferenceScriptsV1
      ],
    );
  bind(
    "fieldPreimageCertificateMint",
    config.referenceScripts.fieldPreimageCertificateMint,
  );
  const contracts: MissingRedeemerContractsV1 = Object.freeze({
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: MISSING_REDEEMER_BLUEPRINT_TITLES_V1[index]!,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as MissingRedeemerContractsV1["steps"],
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: {
      policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
      mintingScript:
        binding.resolvedContracts.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
    fieldPreimageCertificateMintingScript: certificate.mintingScript,
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  return Object.freeze({
    binding,
    lucid: config.lucid,
    decisionDigest: config.decisionDigest,
    l1,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    actuator: createMissingRedeemerActuatorV1({
      binding,
      lucid: config.lucid,
      signer: config.signer,
      contracts,
      references: { steps, witnesses },
      stateQueueMutationLeaseCoordinator:
        config.stateQueueMutationLeaseCoordinator,
    }),
  });
};

export type LoadedMissingRedeemerProductionWorkflowV1 = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundMissingRedeemerWorkflowConfigV1;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;

export type LoadMissingRedeemerProductionWorkflowV1 = (input: {
  runtimeConfigPath: string;
  invocation: ProductionWorkflowAdapterReadinessInputV1;
}) => Promise<LoadedMissingRedeemerProductionWorkflowV1>;

const appendProductionEvent = async (
  journal: FraudProofWorkflowJournalStoreV1,
  workflowId: string,
  identity: FraudProofWorkflowIdentityV1,
  event: FraudProofWorkflowJournalEventV1,
) => {
  const sequence = (await journal.load(workflowId)).length;
  await journal.append(
    {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
      workflowId,
      identity,
      sequence,
      recordedAt: new Date().toISOString(),
      event,
    },
    sequence,
  );
};

const countConfirmedActions = (
  entries: readonly Readonly<{ event: FraudProofWorkflowJournalEventV1 }>[],
  prefix: string,
) =>
  entries.filter(
    ({ event }) =>
      event.kind === "confirmed" && event.actionId.startsWith(prefix),
  ).length;

const productionActionFor = async ({
  workflow,
  artifact,
  entries,
}: {
  workflow: ManifestBoundMissingRedeemerWorkflowV1;
  artifact: MissingRedeemerProductionArtifactV1;
  entries: readonly Readonly<{ event: FraudProofWorkflowJournalEventV1 }>[];
}): Promise<
  | Readonly<{ action: MissingRedeemerActuatorActionV1; actionId: string }>
  | "removed"
> => {
  const stage = (
    await workflow.l1.observe({
      headerHash: workflow.binding.definition.headerHash,
    })
  ).stage;
  if (stage.kind === "not_started")
    return {
      action: {
        stage: "init",
        stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
      },
      actionId: "missingRedeemer:init",
    };
  if (stage.kind === "proof_token")
    return {
      action: {
        stage: "remove",
        nextRemovalOutRef: stage.nextRemovalOutRef,
        fraudProofOutRef: stage.fraudProofOutRef,
      },
      actionId: "missingRedeemer:remove",
    };
  if (stage.kind === "removed") return "removed";
  const threadOutRef = stage.threadOutRef;
  const fixed = (["step_01", "step_02", "step_02a", "step_02b"] as const)[
    stage.step - 1
  ];
  if (fixed !== undefined)
    return {
      action:
        fixed === "step_01"
          ? {
              stage: fixed,
              threadOutRef,
              stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
            }
          : { stage: fixed, threadOutRef },
      actionId: `missingRedeemer:${fixed}`,
    };
  if (stage.step === 5) {
    const count = countConfirmedActions(entries, "missingRedeemer:field:");
    if (artifact.evidence.carriage !== "Certified")
      return {
        action: { stage: "field", threadOutRef, action: { kind: "direct" } },
        actionId: "missingRedeemer:field:direct",
      };
    const staged = planMissingRedeemerStagedWalkV1({
      transactionId: artifact.evidence.subject.transaction_id,
      fieldPreimageCbor: artifact.evidence.fieldPreimageHex,
    });
    if (count > staged.grammar.length)
      throw new Error(
        "missingRedeemer grammar journal passed terminal checkpoint",
      );
    const fieldAction =
      count === 0
        ? ({ kind: "grammar_start" } as const)
        : count === staged.grammar.length
          ? ({ kind: "grammar_finish" } as const)
          : ({ kind: "grammar_resume", ordinal: count } as const);
    return {
      action: { stage: "field", threadOutRef, action: fieldAction },
      actionId: `missingRedeemer:field:${count.toString()}`,
    };
  }
  if (stage.step === 6) {
    const count = countConfirmedActions(entries, "missingRedeemer:scan:");
    return {
      action: { stage: "scan", threadOutRef },
      actionId: `missingRedeemer:scan:${count.toString()}`,
    };
  }
  if (stage.step === 7)
    return {
      action: { stage: "finalize", threadOutRef },
      actionId: "missingRedeemer:finalize",
    };
  throw new Error("missingRedeemer observed impossible step");
};

/** One retained-DA-derived, locally evaluated, intent-journaled action. */
export const executeManifestBoundMissingRedeemerWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundMissingRedeemerWorkflowV1;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStoreV1;
}) => {
  const headerHash = workflow.binding.definition.headerHash;
  const block = await fetchCanonicalBlockEvidenceV1({
    observation: await workflow.l1.observeHeader({ headerHash }),
    sources,
  });
  const candidate = (await replayMissingRedeemerProductionV1(block))[0];
  if (candidate === undefined)
    throw new Error(
      "missingRedeemer complete replay found no canonical violation",
    );
  const artifact = candidate.artifact;
  const identity: FraudProofWorkflowIdentityV1 = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    category: MISSING_REDEEMER_CATEGORY_V1,
    target: { kind: "state_queue_header", headerHash },
    decisionDigest: workflow.decisionDigest,
  };
  const workflowId = computeFraudProofWorkflowIdV1(identity);
  let entries = await journal.load(workflowId);
  if (entries.length === 0) {
    await appendProductionEvent(journal, workflowId, identity, {
      kind: "started",
    });
    entries = await journal.load(workflowId);
  }
  const pending = [...entries]
    .reverse()
    .find(({ event }) => event.kind === "submission_intent");
  const intent =
    pending?.event.kind === "submission_intent" ? pending.event : undefined;
  if (
    intent !== undefined &&
    !entries.some(
      ({ event }) =>
        event.kind === "confirmed" && event.actionId === intent.actionId,
    )
  ) {
    if (
      !(await workflow.l1.transactionConfirmed({
        headerHash,
        txHash: intent.txHash,
      }))
    )
      return { kind: "pending" as const, workflowId, txHash: intent.txHash };
    await appendProductionEvent(journal, workflowId, identity, {
      kind: "confirmed",
      actionId: intent.actionId,
      txHash: intent.txHash,
    });
    entries = await journal.load(workflowId);
  }
  const selected = await productionActionFor({ workflow, artifact, entries });
  if (selected === "removed") return { kind: "completed" as const, workflowId };
  const captured = await workflow.actuator.capture({
    action: selected.action,
    artifact,
  });
  await appendProductionEvent(journal, workflowId, identity, {
    kind: "preflight_passed",
    actionId: selected.actionId,
    txHash: captured.transaction.txHash,
    localEvaluator: "lucid-evolution-local-uplc-v1",
    referenceScripts: captured.transaction.referenceScripts,
  });
  await appendProductionEvent(journal, workflowId, identity, {
    kind: "submission_intent",
    actionId: selected.actionId,
    actionInput: {
      schemaVersion: "midgard-production-cursor-family-action-v1",
      category: MISSING_REDEEMER_CATEGORY_V1,
      stage: selected.action.stage,
    },
    attempt: 1,
    txHash: captured.transaction.txHash,
  });
  const submitted = await submitCapturedTransactionV1(captured.transaction);
  if (submitted !== captured.transaction.txHash)
    throw new Error("missingRedeemer provider substituted transaction");
  await appendProductionEvent(journal, workflowId, identity, {
    kind: "submitted",
    actionId: selected.actionId,
    attempt: 1,
    txHash: submitted,
  });
  return { kind: "pending" as const, workflowId, txHash: submitted };
};

export const runOrResumeManifestBoundMissingRedeemerWorkflowV1 = async (input: {
  workflow: ManifestBoundMissingRedeemerWorkflowV1;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStoreV1;
}) => {
  if (Object.keys(input).sort().join(",") !== "journal,sources,workflow")
    throw new Error("missingRedeemer runner rejects caller-authored evidence");
  return await executeManifestBoundMissingRedeemerWorkflowV1(input);
};

/** Loader-compatible surface; central admission remains fixed-category only. */
export const createMissingRedeemerProductionWorkflowRunnerSurfaceV1 = ({
  loadRuntimeConfig,
}: {
  loadRuntimeConfig: LoadMissingRedeemerProductionWorkflowV1;
}): ProductionWorkflowAdapterRunnerV1 =>
  Object.freeze({
    runnerVersion: PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
    runOrResume: async (invocation) => {
      if (invocation.category !== MISSING_REDEEMER_CATEGORY_V1)
        throw new Error("missingRedeemer runner category changed");
      const loaded = await loadRuntimeConfig({
        runtimeConfigPath: invocation.runtimeConfigPath,
        invocation,
      });
      try {
        if (
          loaded.retainedDaSources.length === 0 ||
          loaded.retainedDaSources.some(
            (source) => !(source instanceof DaLibp2pRetainedDaSource),
          )
        )
          throw new Error(
            "missingRedeemer requires concrete public retained DA",
          );
        const workflow = await createManifestBoundMissingRedeemerWorkflowV1(
          loaded.config,
        );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error("missingRedeemer runtime binding changed invocation");
        const journal = bindProductionWorkflowFundingReservationJournalV1({
          permit: invocation.fundingReservationPermit,
          journal: bindProductionWorkflowActuationJournalV1({
            journal: new DirectoryFraudProofWorkflowJournalStoreV1(
              invocation.journalDirectory,
            ),
            permit: invocation.actuationPermit,
            decisionDigest: invocation.decisionDigest,
            deploymentFingerprint: invocation.deploymentFingerprint,
            category: MISSING_REDEEMER_CATEGORY_V1,
            headerHash: invocation.headerHash,
          }),
        });
        return await executeManifestBoundMissingRedeemerWorkflowV1({
          workflow,
          sources: loaded.retainedDaSources,
          journal,
        });
      } finally {
        await loaded.close();
      }
    },
  });

export interface MissingRedeemerProductionObservationPortV1 {
  observe(identity: string): Promise<MissingRedeemerDurableStateV1>;
  stateQueueBlockOutRef(headerHash: string): Promise<string>;
  removalOutRefs(
    headerHash: string,
  ): Promise<Readonly<{ nextRemovalOutRef: string; fraudProofOutRef: string }>>;
  transactionConfirmed(txHash: string): Promise<boolean>;
}
export type MissingRedeemerProductionRunnerConfigV1 = Readonly<{
  journalDirectory: string;
  actuator: ReturnType<typeof createMissingRedeemerActuatorV1>;
  observation: MissingRedeemerProductionObservationPortV1;
}>;

/**
 * Package-owned production runner. Evidence is accepted only as the strict
 * retained-DA artifact emitted by production replay; runtime configuration is
 * limited to durable storage and chain infrastructure.
 */
export const createMissingRedeemerProductionRunnerV1 = async (
  config: MissingRedeemerProductionRunnerConfigV1,
) => {
  const journal = await createMissingRedeemerDirectoryJournalV1(
    config.journalDirectory,
  );
  return Object.freeze({
    run: async (
      artifact: MissingRedeemerProductionArtifactV1,
    ): Promise<"removed" | "cancelled"> => {
      const staged = planMissingRedeemerStagedWalkV1({
        transactionId: artifact.evidence.subject.transaction_id,
        fieldPreimageCbor: artifact.evidence.fieldPreimageHex,
      });
      return await runMissingRedeemerWorkflowV1({
        evidence: artifact.evidence,
        journal,
        actuator: {
          observe: async (identity) =>
            await config.observation.observe(identity),
          submit: async ({ identity, action, scanCursor }) => {
            const observed = await config.observation.observe(identity);
            let concrete: MissingRedeemerActuatorActionV1;
            if (action === "init")
              concrete = {
                stage: "init",
                stateQueueBlockOutRef:
                  await config.observation.stateQueueBlockOutRef(
                    artifact.headerHash,
                  ),
              };
            else if (action === "bind")
              concrete = {
                stage: "step_01",
                threadOutRef: observed.outputReference!,
                stateQueueBlockOutRef:
                  await config.observation.stateQueueBlockOutRef(
                    artifact.headerHash,
                  ),
              };
            else if (action === "authenticatePurpose")
              concrete = {
                stage: "step_02",
                threadOutRef: observed.outputReference!,
              };
            else if (action === "authenticateTrace")
              concrete = {
                stage: "step_02a",
                threadOutRef: observed.outputReference!,
              };
            else if (action === "authenticateSelection")
              concrete = {
                stage: "step_02b",
                threadOutRef: observed.outputReference!,
              };
            else if (action === "openRedeemers") {
              const fieldAction =
                artifact.evidence.carriage !== "Certified"
                  ? { kind: "direct" as const }
                  : scanCursor === 0
                    ? { kind: "grammar_start" as const }
                    : scanCursor < staged.grammar.length
                      ? { kind: "grammar_resume" as const, ordinal: scanCursor }
                      : { kind: "grammar_finish" as const };
              concrete = {
                stage: "field",
                threadOutRef: observed.outputReference!,
                action: fieldAction,
              };
            } else if (action === "scan")
              concrete = {
                stage: "scan",
                threadOutRef: observed.outputReference!,
              };
            else if (action === "finalize")
              concrete = {
                stage: "finalize",
                threadOutRef: observed.outputReference!,
              };
            else {
              const refs = await config.observation.removalOutRefs(
                artifact.headerHash,
              );
              concrete = { stage: "remove", ...refs };
            }
            if (
              ("threadOutRef" in concrete && concrete.threadOutRef == null) ||
              observed.stage === "cancelled"
            )
              throw new Error(
                "missingRedeemer observed cursor cannot execute action",
              );
            const captured = await config.actuator.capture({
              action: concrete,
              artifact,
            });
            const submitted = await submitCapturedTransactionV1(
              captured.transaction,
            );
            if (submitted !== captured.transaction.txHash)
              throw new Error(
                "missingRedeemer provider substituted transaction identity",
              );
            if (!(await config.observation.transactionConfirmed(submitted)))
              throw new Error(
                "missingRedeemer submitted transaction is unresolved",
              );
            const next = await config.observation.observe(identity);
            if (next.txHash !== submitted)
              throw new Error(
                "missingRedeemer confirmed cursor changed transaction identity",
              );
            return next;
          },
        },
      });
    },
    cancel: async (
      artifact: MissingRedeemerProductionArtifactV1,
    ): Promise<"cancelled"> => {
      const identity = missingRedeemerEvidenceIdentityV1(artifact.evidence);
      const observed = await config.observation.observe(identity);
      const stepIndex = [
        "step01",
        "step02",
        "step02a",
        "step02b",
        "step03",
        "step04",
        "step05",
      ].indexOf(observed.stage);
      if (stepIndex < 0 || observed.outputReference === null)
        throw new Error("missingRedeemer observed stage cannot cancel");
      const captured = await config.actuator.capture({
        action: {
          stage: "cancel",
          threadOutRef: observed.outputReference,
          stepIndex,
        },
        artifact,
      });
      const submitted = await submitCapturedTransactionV1(captured.transaction);
      if (
        submitted !== captured.transaction.txHash ||
        !(await config.observation.transactionConfirmed(submitted))
      )
        throw new Error("missingRedeemer cancellation is unresolved");
      const next = await config.observation.observe(identity);
      if (next.stage !== "cancelled" || next.txHash !== submitted)
        throw new Error("missingRedeemer cancellation cursor changed");
      const entries = await journal.load(identity);
      await journal.append(identity, entries.length, next);
      return "cancelled";
    },
  });
};
