import {
  decodeMidgardNativeTxCompact,
  deriveMidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core";
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  type CanonicalBlockEvidence,
  fetchCanonicalBlockEvidence,
} from "../evidence/canonical-block-evidence.js";
import { requireLinearFaultThreadUtxo } from "../linear-fault-family.js";
import { buildTrieView, requireProof } from "../prepare-double-spend.js";
import {
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import {
  DaLibp2pRetainedDaSource,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  assertWorkflowJournalActuation,
  bindWorkflowActuationJournal,
} from "../workflow/actuation-permit.js";
import {
  WORKFLOW_ADAPTER_RUNNER,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
} from "../workflow/adapters.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import { createFraudProofFamilyLocalKupmiosL1ObservationPort } from "../workflow/family-l1-observation.js";
import { bindWorkflowFundingReservationJournal } from "../workflow/funding-reservation-permit.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalStore,
  journalJsonDigest,
  type JournalJsonObject,
} from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import {
  detectRedeemerCanonicityFromCanonicalBlock,
  type RedeemerCanonicityDetection,
} from "./authenticated-workflow.js";
import {
  REDEEMER_CANONICITY_BLUEPRINT_TITLES,
  type RedeemerCanonicityContracts,
} from "./contracts.js";
import {
  RedeemerCanonicityStep02DatumSchema,
  RedeemerCanonicityStep03DatumSchema,
} from "./schemas.js";
import {
  submitRedeemerCanonicityStep01Accepted,
  submitRedeemerCanonicityStep01Forced,
} from "./submit-step-01.js";
import { submitRedeemerCanonicityStep02 } from "./submit-step-02.js";
import { submitRedeemerCanonicityStep03 } from "./submit-step-03.js";
import type {
  RedeemerCanonicityActuator,
  RedeemerCanonicityDurableState,
  RedeemerCanonicityJournal,
} from "./workflow.js";
import { runRedeemerCanonicityWorkflow } from "./workflow.js";

export const REDEEMER_CANONICITY_CONFIG_KEYS = Object.freeze([
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

export type RedeemerCanonicityRemovalReferenceScripts = Readonly<{
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
export type RedeemerCanonicityWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
  removal: RedeemerCanonicityRemovalReferenceScripts;
}>;
export type ManifestBoundRedeemerCanonicityWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: RedeemerCanonicityWorkflowReferenceScripts;
}>;

type Binding = FraudProofWorkflowDeploymentBinding<"redeemerCanonicity">;
export type ManifestBoundRedeemerCanonicityWorkflow = Readonly<{
  binding: Binding;
  l1: ReturnType<typeof createFraudProofFamilyLocalKupmiosL1ObservationPort>;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: RedeemerCanonicityContracts;
  referenceScripts: RedeemerCanonicityWorkflowReferenceScripts;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

/** Strict manifest/reference binding whose input admits no callback authority. */
export const createManifestBoundRedeemerCanonicityWorkflow = async (
  config: ManifestBoundRedeemerCanonicityWorkflowConfig,
): Promise<ManifestBoundRedeemerCanonicityWorkflow> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...REDEEMER_CANONICITY_CONFIG_KEYS].sort().join("\0")
  )
    throw new Error(
      "redeemerCanonicity production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("redeemerCanonicity decision digest is malformed");
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "redeemerCanonicity",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      RedeemerCanonicityStep02DatumSchema,
      RedeemerCanonicityStep03DatumSchema,
    ],
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.redeemerCanonicity;
  const certificate = binding.fieldPreimageCertificate;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    chain.steps.length !== 3 ||
    certificate === null ||
    stateQueuePolicyId === undefined
  )
    throw new Error("redeemerCanonicity manifest omitted required contracts");
  const bind = (name: string, utxo: UTxO) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName: name,
      utxo,
    });
  const stepNames = [
    "fraudProofRedeemerCanonicity",
    "fraudProofRedeemerCanonicityStep02",
    "fraudProofRedeemerCanonicityStep03",
  ] as const;
  const steps = stepNames.map((name, index) =>
    bind(name, config.referenceScripts.steps[index]!),
  ) as unknown as RedeemerCanonicityWorkflowReferenceScripts["steps"];
  const witnessNames = {
    computationThreadMint: "computationThreadMint",
    fraudProofMint: "fraudProofMint",
    phasMembershipWithdraw: "phasMembershipWithdraw",
    chunkedVerifyWithdraw: "chunkedVerifyWithdraw",
    pexcludesWithdraw: "pexcludesWithdraw",
  } as const;
  const witnesses = Object.fromEntries(
    Object.entries(witnessNames).map(([role, name]) => [
      role,
      bind(
        name,
        config.referenceScripts.witnesses[
          role as keyof FaultProofWitnessReferenceScripts
        ]!,
      ),
    ]),
  ) as Required<FaultProofWitnessReferenceScripts>;
  bind(
    "fieldPreimageCertificateMint",
    config.referenceScripts.fieldPreimageCertificateMint,
  );
  for (const [name, utxo] of Object.entries(config.referenceScripts.removal))
    bind(name, utxo);
  const contracts: RedeemerCanonicityContracts = {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: REDEEMER_CANONICITY_BLUEPRINT_TITLES[index]!,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as RedeemerCanonicityContracts["steps"],
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: binding.resolvedContracts.contracts.fraudProof,
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
    fieldPreimageCertificateMintingScript: certificate.mintingScript,
  };
  return Object.freeze({
    binding,
    l1: createFraudProofFamilyLocalKupmiosL1ObservationPort({
      source: config.source,
      releaseFinality: binding.releaseFinality,
      releaseEconomics: binding.releaseEconomics,
      definition: binding.definition,
    }),
    lucid: config.lucid,
    signer: config.signer,
    contracts,
    referenceScripts: {
      ...config.referenceScripts,
      steps,
      witnesses,
    },
    decisionDigest: config.decisionDigest,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
};

const selectDetection = (
  detections: readonly RedeemerCanonicityDetection[],
): RedeemerCanonicityDetection => {
  const selected = [...detections].sort((left, right) =>
    left.position === right.position
      ? left.detectionId.localeCompare(right.detectionId)
      : left.position < right.position
        ? -1
        : 1,
  )[0];
  if (selected === undefined)
    throw new Error(
      "redeemerCanonicity retained DA contains no closing detection",
    );
  return selected;
};

/**
 * Family-local actuator factory. The concrete transaction driver is built
 * inside the trusted runtime loader from Lucid, signer and bound references;
 * it is never accepted in the public JSON/config surface.
 */
export type RedeemerCanonicityRuntimeDependencies = Readonly<{
  journal: RedeemerCanonicityJournal;
}>;

const createConcreteActuator = ({
  workflow,
  detection,
  block,
}: {
  workflow: ManifestBoundRedeemerCanonicityWorkflow;
  detection: RedeemerCanonicityDetection;
  block: CanonicalBlockEvidence;
}): RedeemerCanonicityActuator => {
  const observed = async (): Promise<RedeemerCanonicityDurableState> => {
    const stage = (
      await workflow.l1.observe({
        headerHash: workflow.binding.definition.headerHash,
      })
    ).stage;
    if (stage.kind === "removed")
      return {
        stage: "removed",
        decodeCursor: 0,
        txHash: "",
        outputReference: null,
      };
    if (stage.kind === "proof_token")
      return {
        stage: "proven",
        decodeCursor: 0,
        txHash: "",
        outputReference: stage.nextRemovalOutRef,
      };
    if (stage.kind === "not_started")
      return {
        stage: "none",
        decodeCursor: 0,
        txHash: "",
        outputReference: stage.stateQueueBlockOutRef,
      };
    return {
      stage: (["step01", "step02", "step03"] as const)[stage.step - 1]!,
      decodeCursor: 0,
      txHash: "",
      outputReference: stage.threadOutRef,
    };
  };
  const material = () => {
    if (detection.source === "accepted") {
      const tx = block.transactions.find(
        (value) => value.nodeTxId === detection.evidence.subject.transaction_id,
      );
      if (tx === undefined)
        throw new Error("redeemerCanonicity accepted source disappeared");
      return deriveMidgardNativeTxFaultEvidenceMaterial(
        Buffer.from(tx.txCbor, "hex"),
      );
    }
    const tx = block.reconstruction.forcedTransactions.find(
      (value) =>
        value.value.tx_id === detection.evidence.subject.transaction_id,
    );
    if (tx === undefined)
      throw new Error("redeemerCanonicity forced source disappeared");
    return deriveMidgardNativeTxFaultEvidenceMaterial(tx.fullTransactionCbor);
  };
  return Object.freeze({
    observe: async () => await observed(),
    submit: async ({ action, evidence }) => {
      const before = await workflow.l1.observe({
        headerHash: workflow.binding.definition.headerHash,
      });
      const stage = before.stage;
      const categoryId = workflow.binding.resolvedContracts.category.categoryId;
      if (action === "init") {
        if (stage.kind !== "not_started")
          throw new Error("redeemerCanonicity init stage changed");
        await submitInit({
          lucid: workflow.lucid,
          blueprint: workflow.binding.blueprint,
          deploymentInfo: workflow.binding.deploymentInfo,
          network: workflow.binding.network,
          signer: workflow.signer,
          fraudCategory: "redeemerCanonicity",
          fraudulentBlockOutRef: stage.stateQueueBlockOutRef,
          fraudulentHeaderHash: workflow.binding.definition.headerHash,
          witnessReferenceScripts: workflow.referenceScripts.witnesses,
          awaitConfirmation: true,
        });
      } else if (action === "bind") {
        if (stage.kind !== "step" || stage.step !== 1)
          throw new Error("redeemerCanonicity bind stage changed");
        if (detection.source === "accepted") {
          const transaction = block.transactions.find(
            (value) => value.nodeTxId === evidence.subject.transaction_id,
          );
          if (transaction === undefined)
            throw new Error("redeemerCanonicity accepted source disappeared");
          const txMaterial = material();
          const trie = await buildTrieView(
            block.transactions.map((value) => ({
              key: Buffer.from(value.nodeTxId, "hex"),
              value: Buffer.from(value.l2TransactionSourceCbor, "hex"),
            })),
          );
          const inclusion = parseSubmitStep01TxInclusion({
            nativeTxId: transaction.nodeTxId,
            nativeTx: nativeTxFromCoreCompact(txMaterial.compact),
            nativeTxCompactCbor:
              txMaterial.proofSource.compactCbor.toString("hex"),
            l2TransactionSourceCbor: transaction.l2TransactionSourceCbor,
            transactionsPhasRoot: trie.root,
            txMembershipProofCbor: requireProof(
              trie,
              Buffer.from(transaction.nodeTxId, "hex"),
              "redeemer-canonicity transaction",
            ),
          });
          const { threadUtxo, threadToken } =
            await requireLinearFaultThreadUtxo({
              lucid: workflow.lucid,
              contracts: workflow.contracts,
              categoryId,
              family: "redeemer-canonicity",
              stepIndex: 0,
              threadOutRef: stage.threadOutRef,
            });
          await submitRedeemerCanonicityStep01Accepted({
            lucid: workflow.lucid,
            blueprint: workflow.binding.blueprint,
            network: workflow.binding.network,
            contracts: workflow.contracts,
            signer: workflow.signer,
            finding: evidence,
            threadUtxo,
            threadToken,
            stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
            txInclusion: inclusion,
            referenceScriptUtxo: workflow.referenceScripts.steps[0],
            witnessReferenceScripts: workflow.referenceScripts.witnesses,
          });
        } else {
          const forced = block.reconstruction.forcedTransactions.find(
            (value) => value.value.tx_id === evidence.subject.transaction_id,
          );
          if (forced === undefined)
            throw new Error("redeemerCanonicity forced source disappeared");
          const membership = await buildForcedTransactionLeafMembershipProof({
            reconstruction: block.reconstruction,
            eventKey: {
              ForcedTransactionEventKey: { tx_order_id: forced.key },
            },
          });
          const compact = decodeMidgardNativeTxCompact(
            Buffer.from(forced.value.source.compact_cbor, "hex"),
          );
          await submitRedeemerCanonicityStep01Forced({
            lucid: workflow.lucid,
            contracts: workflow.contracts,
            categoryId,
            signer: workflow.signer,
            threadOutRef: stage.threadOutRef,
            finding: evidence,
            forcedSource: { header: block.header, membership, direction: 1n },
            witnessSetHash: Buffer.from(
              compact.transactionWitnessSetHash,
            ).toString("hex"),
            referenceScriptUtxo: workflow.referenceScripts.steps[0],
          });
        }
      } else if (action === "decode") {
        if (stage.kind !== "step" || stage.step !== 2)
          throw new Error("redeemerCanonicity decode stage changed");
        const txMaterial = material();
        await submitRedeemerCanonicityStep02({
          lucid: workflow.lucid,
          contracts: workflow.contracts,
          categoryId,
          signer: workflow.signer,
          threadOutRef: stage.threadOutRef,
          evidence,
          nativeTxCompactCbor:
            txMaterial.proofSource.compactCbor.toString("hex"),
          witnessSetCompactCbor:
            txMaterial.proofSource.witnessSetCompactCbor.toString("hex"),
          referenceScriptUtxo: workflow.referenceScripts.steps[1],
          certificateReferenceScriptUtxo:
            workflow.referenceScripts.fieldPreimageCertificateMint,
        });
      } else if (action === "finalize") {
        if (stage.kind !== "step" || stage.step !== 3)
          throw new Error("redeemerCanonicity finalize stage changed");
        await submitRedeemerCanonicityStep03({
          lucid: workflow.lucid,
          contracts: workflow.contracts,
          categoryId,
          signer: workflow.signer,
          threadOutRef: stage.threadOutRef,
          evidence,
          referenceScriptUtxo: workflow.referenceScripts.steps[2],
          witnessReferenceScripts: workflow.referenceScripts.witnesses,
        });
      } else {
        if (stage.kind !== "proof_token")
          throw new Error("redeemerCanonicity removal stage changed");
        await submitRemoveFraudulentBlock({
          lucid: workflow.lucid,
          blueprint: workflow.binding.blueprint,
          deploymentInfo: workflow.binding.deploymentInfo,
          network: workflow.binding.network,
          signer: workflow.signer,
          fraudCategory: "redeemerCanonicity",
          fraudulentHeaderHash: workflow.binding.definition.headerHash,
          stateQueueMutationLeaseCoordinator:
            workflow.stateQueueMutationLeaseCoordinator,
          fraudProverRewardLovelace: BigInt(
            workflow.binding.releaseEconomics.policy.fraudProverRewardLovelace,
          ),
          requireReferenceScripts: true,
          awaitConfirmation: true,
        });
      }
      return await observed();
    },
  });
};

export const executeManifestBoundRedeemerCanonicityWorkflow = async ({
  workflow,
  sources,
  runtime,
}: {
  readonly workflow: ManifestBoundRedeemerCanonicityWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly runtime: RedeemerCanonicityRuntimeDependencies;
}): Promise<"removed" | "cancelled"> => {
  const headerHash = workflow.binding.definition.headerHash;
  const canonical = await fetchCanonicalBlockEvidence({
    observation: await workflow.l1.observeHeader({ headerHash }),
    sources,
  });
  const detection = selectDetection(
    detectRedeemerCanonicityFromCanonicalBlock(canonical),
  );
  return await runRedeemerCanonicityWorkflow({
    evidence: detection.evidence,
    journal: runtime.journal,
    actuator: createConcreteActuator({ workflow, detection, block: canonical }),
  });
};

export const runOrResumeManifestBoundRedeemerCanonicityWorkflow =
  executeManifestBoundRedeemerCanonicityWorkflow;

export const createManifestBoundRedeemerCanonicityRuntime = ({
  workflow,
  sources,
  runtime,
}: {
  readonly workflow: ManifestBoundRedeemerCanonicityWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly runtime: RedeemerCanonicityRuntimeDependencies;
}) =>
  Object.freeze({
    runOrResume: async () =>
      await executeManifestBoundRedeemerCanonicityWorkflow({
        workflow,
        sources,
        runtime,
      }),
  });

export type LoadedRedeemerCanonicityWorkflow = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundRedeemerCanonicityWorkflowConfig;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;
export type LoadRedeemerCanonicityWorkflow = (input: {
  readonly runtimeConfigPath: string;
  readonly invocation: WorkflowAdapterReadinessInput;
}) => Promise<LoadedRedeemerCanonicityWorkflow>;

const journalAdapter = (
  journal: FraudProofWorkflowJournalStore,
  entryIdentity: FraudProofWorkflowIdentity,
): RedeemerCanonicityJournal => ({
  load: async (identity) =>
    (await journal.load(identity)).flatMap((entry) => {
      const value = entry.event;
      if (value.kind !== "prepared") return [];
      return [value.artifact as unknown as RedeemerCanonicityDurableState];
    }),
  append: async (identity, expectedLength, state) => {
    const entries = await journal.load(identity);
    const artifact = state as unknown as JournalJsonObject;
    await journal.append(
      {
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
        workflowId: identity,
        identity: entries[0]?.identity ?? entryIdentity,
        sequence: entries.length,
        recordedAt: new Date().toISOString(),
        event: {
          kind: "prepared",
          artifact,
          artifactDigest: journalJsonDigest(artifact),
        },
      },
      expectedLength,
    );
  },
});

export const createRedeemerCanonicityWorkflowRunnerSurface = ({
  loadRuntimeConfig,
}: {
  readonly loadRuntimeConfig: LoadRedeemerCanonicityWorkflow;
}): WorkflowAdapterRunner =>
  Object.freeze({
    runnerVersion: WORKFLOW_ADAPTER_RUNNER,
    runOrResume: async (invocation) => {
      if (String(invocation.category) !== "redeemerCanonicity")
        throw new Error(
          "redeemerCanonicity production runner category mismatch",
        );
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
            "redeemerCanonicity has no public retained-DA source",
          );
        const durable = bindWorkflowFundingReservationJournal({
          permit: invocation.fundingReservationPermit,
          journal: bindWorkflowActuationJournal({
            journal: new DirectoryFraudProofWorkflowJournalStore(
              invocation.journalDirectory,
            ),
            permit: invocation.actuationPermit,
            decisionDigest: invocation.decisionDigest,
            deploymentFingerprint: invocation.deploymentFingerprint,
            category: "redeemerCanonicity",
            headerHash: invocation.headerHash,
          }),
        });
        assertWorkflowJournalActuation({
          journal: durable,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category: "redeemerCanonicity",
          headerHash: invocation.headerHash,
          checkpoint: "runner_start",
        });
        const workflow = await createManifestBoundRedeemerCanonicityWorkflow(
          loaded.config,
        );
        if (
          workflow.binding.deploymentFingerprint !==
            invocation.deploymentFingerprint ||
          workflow.binding.definition.headerHash !== invocation.headerHash ||
          workflow.decisionDigest !== invocation.decisionDigest
        )
          throw new Error(
            "redeemerCanonicity runtime identity differs from invocation",
          );
        return await executeManifestBoundRedeemerCanonicityWorkflow({
          workflow,
          sources: loaded.retainedDaSources,
          runtime: {
            journal: journalAdapter(durable, {
              schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
              deploymentFingerprint: invocation.deploymentFingerprint,
              category: "redeemerCanonicity",
              target: {
                kind: "state_queue_header",
                headerHash: invocation.headerHash,
              },
              decisionDigest: invocation.decisionDigest,
            }),
          },
        });
      } finally {
        await loaded.close();
      }
    },
  });

export const createRedeemerCanonicityWorkflowRunnerFactory =
  createRedeemerCanonicityWorkflowRunnerSurface;
