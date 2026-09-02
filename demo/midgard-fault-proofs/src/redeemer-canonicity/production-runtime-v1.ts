import {
  decodeMidgardNativeTxCompactV1,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
} from "@al-ft/midgard-core";
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  type CanonicalBlockEvidenceV1,
  fetchCanonicalBlockEvidenceV1,
} from "../evidence/canonical-block-evidence-v1.js";
import { requireLinearFaultThreadUtxoV1 } from "../linear-fault-family-v1.js";
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
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "../workflow/deployment-manifest-binding-v1.js";
import { createFraudProofFamilyLocalKupmiosL1ObservationPortV1 } from "../workflow/family-l1-observation-v1.js";
import {
  DirectoryFraudProofWorkflowJournalStoreV1,
  FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
  type FraudProofWorkflowIdentityV1,
  type FraudProofWorkflowJournalStoreV1,
  journalJsonDigestV1,
  type JournalJsonObjectV1,
} from "../workflow/journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfigV1 } from "../workflow/local-kupmios-http-ogmios-source-v1.js";
import {
  assertProductionWorkflowJournalActuationV1,
  bindProductionWorkflowActuationJournalV1,
} from "../workflow/production-actuation-permit-v1.js";
import {
  PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
  type ProductionWorkflowAdapterReadinessInputV1,
  type ProductionWorkflowAdapterRunnerV1,
} from "../workflow/production-adapters-v1.js";
import { bindProductionWorkflowFundingReservationJournalV1 } from "../workflow/production-funding-reservation-permit-v1.js";
import {
  REDEEMER_CANONICITY_BLUEPRINT_TITLES_V1,
  type RedeemerCanonicityContractsV1,
} from "./contracts-v1.js";
import {
  detectRedeemerCanonicityFromCanonicalBlockV1,
  type RedeemerCanonicityDetectionV1,
} from "./production-workflow-v1.js";
import {
  RedeemerCanonicityStep02DatumV1Schema,
  RedeemerCanonicityStep03DatumV1Schema,
} from "./schemas-v1.js";
import {
  submitRedeemerCanonicityStep01AcceptedV1,
  submitRedeemerCanonicityStep01ForcedV1,
} from "./submit-step-01-v1.js";
import { submitRedeemerCanonicityStep02V1 } from "./submit-step-02-v1.js";
import { submitRedeemerCanonicityStep03V1 } from "./submit-step-03-v1.js";
import type {
  RedeemerCanonicityActuatorV1,
  RedeemerCanonicityDurableStateV1,
  RedeemerCanonicityJournalV1,
} from "./workflow-v1.js";
import { runRedeemerCanonicityWorkflowV1 } from "./workflow-v1.js";

export const REDEEMER_CANONICITY_PRODUCTION_CONFIG_KEYS_V1 = Object.freeze([
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

export type RedeemerCanonicityRemovalReferenceScriptsV1 = Readonly<{
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
export type RedeemerCanonicityWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScriptsV1>;
  fieldPreimageCertificateMint: UTxO;
  removal: RedeemerCanonicityRemovalReferenceScriptsV1;
}>;
export type ManifestBoundRedeemerCanonicityWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: RedeemerCanonicityWorkflowReferenceScriptsV1;
}>;

type Binding = FraudProofWorkflowDeploymentBindingV1<"redeemerCanonicity">;
export type ManifestBoundRedeemerCanonicityWorkflowV1 = Readonly<{
  binding: Binding;
  l1: ReturnType<typeof createFraudProofFamilyLocalKupmiosL1ObservationPortV1>;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: RedeemerCanonicityContractsV1;
  referenceScripts: RedeemerCanonicityWorkflowReferenceScriptsV1;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

/** Strict manifest/reference binding whose input admits no callback authority. */
export const createManifestBoundRedeemerCanonicityWorkflowV1 = async (
  config: ManifestBoundRedeemerCanonicityWorkflowConfigV1,
): Promise<ManifestBoundRedeemerCanonicityWorkflowV1> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...REDEEMER_CANONICITY_PRODUCTION_CONFIG_KEYS_V1].sort().join("\0")
  )
    throw new Error(
      "redeemerCanonicity production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("redeemerCanonicity decision digest is malformed");
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "redeemerCanonicity",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      RedeemerCanonicityStep02DatumV1Schema,
      RedeemerCanonicityStep03DatumV1Schema,
    ],
  });
  assertManifestBoundWorkflowSignerV1({
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
    requireManifestBoundReferenceScriptUtxoV1({
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
  ) as unknown as RedeemerCanonicityWorkflowReferenceScriptsV1["steps"];
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
          role as keyof FaultProofWitnessReferenceScriptsV1
        ]!,
      ),
    ]),
  ) as Required<FaultProofWitnessReferenceScriptsV1>;
  bind(
    "fieldPreimageCertificateMint",
    config.referenceScripts.fieldPreimageCertificateMint,
  );
  for (const [name, utxo] of Object.entries(config.referenceScripts.removal))
    bind(name, utxo);
  const contracts: RedeemerCanonicityContractsV1 = {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: REDEEMER_CANONICITY_BLUEPRINT_TITLES_V1[index]!,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as RedeemerCanonicityContractsV1["steps"],
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: binding.resolvedContracts.contracts.fraudProof,
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
    fieldPreimageCertificateMintingScript: certificate.mintingScript,
  };
  return Object.freeze({
    binding,
    l1: createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
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
  detections: readonly RedeemerCanonicityDetectionV1[],
): RedeemerCanonicityDetectionV1 => {
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
export type RedeemerCanonicityProductionRuntimeDependenciesV1 = Readonly<{
  journal: RedeemerCanonicityJournalV1;
}>;

const createConcreteActuator = ({
  workflow,
  detection,
  block,
}: {
  workflow: ManifestBoundRedeemerCanonicityWorkflowV1;
  detection: RedeemerCanonicityDetectionV1;
  block: CanonicalBlockEvidenceV1;
}): RedeemerCanonicityActuatorV1 => {
  const observed = async (): Promise<RedeemerCanonicityDurableStateV1> => {
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
      return deriveMidgardNativeTxFaultEvidenceMaterialV1(
        Buffer.from(tx.txCbor, "hex"),
      );
    }
    const tx = block.reconstruction.forcedTransactions.find(
      (value) =>
        value.value.tx_id === detection.evidence.subject.transaction_id,
    );
    if (tx === undefined)
      throw new Error("redeemerCanonicity forced source disappeared");
    return deriveMidgardNativeTxFaultEvidenceMaterialV1(tx.fullTransactionCbor);
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
            await requireLinearFaultThreadUtxoV1({
              lucid: workflow.lucid,
              contracts: workflow.contracts,
              categoryId,
              family: "redeemer-canonicity",
              stepIndex: 0,
              threadOutRef: stage.threadOutRef,
            });
          await submitRedeemerCanonicityStep01AcceptedV1({
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
          const compact = decodeMidgardNativeTxCompactV1(
            Buffer.from(forced.value.source.compact_cbor, "hex"),
          );
          await submitRedeemerCanonicityStep01ForcedV1({
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
        await submitRedeemerCanonicityStep02V1({
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
        await submitRedeemerCanonicityStep03V1({
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

export const executeManifestBoundRedeemerCanonicityWorkflowV1 = async ({
  workflow,
  sources,
  runtime,
}: {
  readonly workflow: ManifestBoundRedeemerCanonicityWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly runtime: RedeemerCanonicityProductionRuntimeDependenciesV1;
}): Promise<"removed" | "cancelled"> => {
  const headerHash = workflow.binding.definition.headerHash;
  const canonical = await fetchCanonicalBlockEvidenceV1({
    observation: await workflow.l1.observeHeader({ headerHash }),
    sources,
  });
  const detection = selectDetection(
    detectRedeemerCanonicityFromCanonicalBlockV1(canonical),
  );
  return await runRedeemerCanonicityWorkflowV1({
    evidence: detection.evidence,
    journal: runtime.journal,
    actuator: createConcreteActuator({ workflow, detection, block: canonical }),
  });
};

export const runOrResumeManifestBoundRedeemerCanonicityWorkflowV1 =
  executeManifestBoundRedeemerCanonicityWorkflowV1;

export const createManifestBoundRedeemerCanonicityProductionRuntimeV1 = ({
  workflow,
  sources,
  runtime,
}: {
  readonly workflow: ManifestBoundRedeemerCanonicityWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly runtime: RedeemerCanonicityProductionRuntimeDependenciesV1;
}) =>
  Object.freeze({
    runOrResume: async () =>
      await executeManifestBoundRedeemerCanonicityWorkflowV1({
        workflow,
        sources,
        runtime,
      }),
  });

export type LoadedRedeemerCanonicityProductionWorkflowV1 = Readonly<{
  schemaVersion: "midgard-production-fraud-proof-runtime-config-v1";
  config: ManifestBoundRedeemerCanonicityWorkflowConfigV1;
  retainedDaSources: readonly DaLibp2pRetainedDaSource[];
  close: () => Promise<void>;
}>;
export type LoadRedeemerCanonicityProductionWorkflowV1 = (input: {
  readonly runtimeConfigPath: string;
  readonly invocation: ProductionWorkflowAdapterReadinessInputV1;
}) => Promise<LoadedRedeemerCanonicityProductionWorkflowV1>;

const journalAdapter = (
  journal: FraudProofWorkflowJournalStoreV1,
  entryIdentity: FraudProofWorkflowIdentityV1,
): RedeemerCanonicityJournalV1 => ({
  load: async (identity) =>
    (await journal.load(identity)).flatMap((entry) => {
      const value = entry.event;
      if (value.kind !== "prepared") return [];
      return [value.artifact as unknown as RedeemerCanonicityDurableStateV1];
    }),
  append: async (identity, expectedLength, state) => {
    const entries = await journal.load(identity);
    const artifact = state as unknown as JournalJsonObjectV1;
    await journal.append(
      {
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
        workflowId: identity,
        identity: entries[0]?.identity ?? entryIdentity,
        sequence: entries.length,
        recordedAt: new Date().toISOString(),
        event: {
          kind: "prepared",
          artifact,
          artifactDigest: journalJsonDigestV1(artifact),
        },
      },
      expectedLength,
    );
  },
});

export const createRedeemerCanonicityProductionWorkflowRunnerSurfaceV1 = ({
  loadRuntimeConfig,
}: {
  readonly loadRuntimeConfig: LoadRedeemerCanonicityProductionWorkflowV1;
}): ProductionWorkflowAdapterRunnerV1 =>
  Object.freeze({
    runnerVersion: PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
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
        const durable = bindProductionWorkflowFundingReservationJournalV1({
          permit: invocation.fundingReservationPermit,
          journal: bindProductionWorkflowActuationJournalV1({
            journal: new DirectoryFraudProofWorkflowJournalStoreV1(
              invocation.journalDirectory,
            ),
            permit: invocation.actuationPermit,
            decisionDigest: invocation.decisionDigest,
            deploymentFingerprint: invocation.deploymentFingerprint,
            category: "redeemerCanonicity",
            headerHash: invocation.headerHash,
          }),
        });
        assertProductionWorkflowJournalActuationV1({
          journal: durable,
          deploymentFingerprint: invocation.deploymentFingerprint,
          category: "redeemerCanonicity",
          headerHash: invocation.headerHash,
          checkpoint: "runner_start",
        });
        const workflow = await createManifestBoundRedeemerCanonicityWorkflowV1(
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
        return await executeManifestBoundRedeemerCanonicityWorkflowV1({
          workflow,
          sources: loaded.retainedDaSources,
          runtime: {
            journal: journalAdapter(durable, {
              schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
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

export const createRedeemerCanonicityProductionWorkflowRunnerFactoryV1 =
  createRedeemerCanonicityProductionWorkflowRunnerSurfaceV1;
