import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import {
  FraudProofComputationThreadStepDatum,
  MIDGARD_FIELD_INDEX,
  MinAdaStep02DatumSchema,
  MinAdaStep03DatumSchema,
  MinAdaStep04DatumSchema,
  MinAdaStep05DatumSchema,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import { parseContractDeploymentReferenceScriptAuthPolicyId } from "../inspect-contracts.js";
import { resolvePublishedProofChunks } from "../publish-proof-chunks.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  createMinAdaCompleteCanonicalReplayFromHistoricalCorpus,
  requireCompleteCanonicalReplayDecision,
} from "../workflow/complete-replay.js";
import {
  createCursorFamilyWorkflowAdapter,
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "../workflow/family-l1-observation.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  type FieldCarriageRequirement,
  withFieldCarriagePrerequisite,
} from "../workflow/field-carriage-prerequisite.js";
import {
  type HistoricalNativeScriptCheckpointStore,
  type HistoricalNativeScriptCorpus,
  type HistoricalNativeScriptHistorySource,
  requireHistoricalNativeScriptHistoryAuthority,
  resolveHistoricalNativeScriptCorpus,
} from "../workflow/historical-native-script-corpus.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflow,
} from "../workflow/orchestrator.js";
import {
  createAuthenticatedProofChunkPrerequisitePort,
  withProofChunkPrerequisite,
} from "../workflow/proof-chunk-prerequisite.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import {
  admitMinAdaArtifact,
  type AdmittedMinAdaArtifact,
  type MinAdaTxArtifact,
  prepareMinAdaArtifact,
} from "./artifact.js";
import type { MinAdaContracts } from "./contracts.js";
import type { PreparedMinAdaTx } from "./prepare.js";
import { submitMinAdaInit } from "./submit-init.js";
import {
  submitMinAdaTxStep01,
  submitMinAdaUtxoStep01,
} from "./submit-step-01.js";
import {
  submitMinAdaTxStep02,
  submitMinAdaUtxoStep02,
} from "./submit-step-02.js";
import { submitMinAdaUtxoStep03 } from "./submit-step-03.js";
import { submitMinAdaUtxoStep04 } from "./submit-step-04.js";
import { submitMinAdaStep05 } from "./submit-step-05.js";
import { MIN_ADA_CURSOR_SPEC } from "./workflow-spec.js";

export type MinAdaWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO];
  yields: Readonly<{ tx: UTxO; utxo: UTxO }>;
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfig = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"minAda">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: MinAdaContracts;
  references: MinAdaWorkflowReferenceScripts;
  historicalCorpus(): HistoricalNativeScriptCorpus;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

type AdmittedTx = Readonly<{
  artifact: MinAdaTxArtifact;
  prepared: PreparedMinAdaTx;
}>;
const isTx = (admitted: AdmittedMinAdaArtifact): admitted is AdmittedTx =>
  admitted.artifact.kind === "min-ada-tx";

const witnessSet = (admitted: AdmittedTx) => {
  const compact = deriveMidgardNativeTxWitnessSetCompact(
    decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(admitted.prepared.nativeTxCanonicalCbor, "hex"),
    ).witnessSet,
  );
  return {
    addr_tx_wits_hash: Buffer.from(compact.addrTxWitsHash).toString("hex"),
    script_tx_wits_hash: Buffer.from(compact.scriptTxWitsHash).toString("hex"),
    redeemer_tx_wits_hash: Buffer.from(compact.redeemerTxWitsHash).toString(
      "hex",
    ),
  };
};

const txFieldPlan = (admitted: AdmittedTx, owner: string) =>
  planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.outputs,
    anchorTxId: admitted.prepared.badTxId,
    nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
    itemCbors: admitted.prepared.outputItemCbors.map((item) =>
      Buffer.from(item, "hex"),
    ),
    owner,
    publish: true,
    witnessSet: witnessSet(admitted),
    label: "min-ada transaction field 2",
  });

const resolveField = async ({
  config,
  admitted,
}: {
  readonly config: BoundConfig;
  readonly admitted: AdmittedTx;
}) => {
  const planned = txFieldPlan(admitted, config.signer.paymentKeyHash);
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned,
  });
  if (publications === undefined) {
    throw new Error("min-ada field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid: config.lucid,
    network: config.binding.network,
    planned,
    certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
  });
  if (planned.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("min-ada field certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};

const transactionPort = (
  config: BoundConfig,
): CursorFamilyTransactionPort<"minAda"> => ({
  portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
  category: "minAda",
  prepare: async ({ evidence, classification }) =>
    await prepareMinAdaArtifact({
      evidence,
      historicalNativeScriptCorpus: config.historicalCorpus(),
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitMinAdaArtifact(artifact);
    if (admitted.artifact.headerHash !== config.binding.definition.headerHash) {
      throw new Error("min-ada artifact changed the bound header");
    }
    const input = cursorFamilyActionInput({
      category: "minAda",
      action,
    });
    const categoryId = config.binding.resolvedContracts.category.categoryId;
    const threadOutRef = () => cursorStringField(input, "threadOutRef");
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMinAdaInit({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              fraudulentBlockOutRef: cursorStringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: admitted.artifact.headerHash,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_01") {
      const chunks = isTx(admitted)
        ? await resolvePublishedProofChunks({
            lucid: config.lucid,
            address: config.signer.address,
            proofCbor: admitted.prepared.txInclusion.txMembershipProofCbor,
          })
        : [];
      if (chunks === undefined) {
        throw new Error("min-ada transaction proof disappeared");
      }
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            const shared = {
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              network: config.binding.network,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              stateQueueBlockOutRef: cursorStringField(
                input,
                "stateQueueBlockOutRef",
              ),
              prepared: admitted.prepared,
              referenceScriptUtxo: config.references.steps[0],
              preSubmitBoundary,
              awaitConfirmation: false,
            } as const;
            if (isTx(admitted)) {
              await submitMinAdaTxStep01({
                ...shared,
                blueprint: config.binding.blueprint,
                prepared: admitted.prepared,
                publishedProofChunks: chunks,
                witnessReferenceScripts: config.references.witnesses,
              });
            } else {
              await submitMinAdaUtxoStep01({
                ...shared,
                prepared: admitted.prepared,
              });
            }
          },
        ),
      });
    }
    if (input.stage === "step_02") {
      if (isTx(admitted)) {
        const carriage = await resolveField({ config, admitted });
        return Object.freeze({
          transaction: await captureLocallyEvaluatedTransaction(
            async (preSubmitBoundary) => {
              await submitMinAdaTxStep02({
                lucid: config.lucid,
                contracts: config.contracts,
                categoryId,
                signer: config.signer,
                threadOutRef: threadOutRef(),
                prepared: admitted.prepared,
                publishedCarriageUtxos: carriage.publications,
                ...(carriage.certificate === undefined
                  ? {}
                  : { certificateUtxo: carriage.certificate }),
                referenceScriptUtxo: config.references.steps[1],
                yieldReferenceScriptUtxo: config.references.yields.tx,
                preSubmitBoundary,
                awaitConfirmation: false,
              });
            },
          ),
        });
      }
      const chunks = await resolvePublishedProofChunks({
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.prepared.postMembershipProofCbor,
      });
      if (chunks === undefined) {
        throw new Error("min-ada post-membership proof disappeared");
      }
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMinAdaUtxoStep02({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              prepared: admitted.prepared,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.references.steps[1],
              yieldReferenceScriptUtxo: config.references.yields.utxo,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_03") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMinAdaUtxoStep03({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              coinsPerUtxoByte: BigInt(
                config.binding.cardanoProtocolParameters.coinsPerUtxoByte,
              ),
              referenceScriptUtxo: config.references.steps[2],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_04" && !isTx(admitted)) {
      const chunks = await resolvePublishedProofChunks({
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.prepared.predecessorNonMembershipProofCbor,
      });
      if (chunks === undefined) {
        throw new Error("min-ada predecessor proof disappeared");
      }
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMinAdaUtxoStep04({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              predecessorNonMembershipProofCbor:
                admitted.prepared.predecessorNonMembershipProofCbor,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.references.steps[3],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_05") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMinAdaStep05({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              referenceScriptUtxo: config.references.steps[4],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "remove") {
      return await captureCursorRemoval({
        category: "minAda",
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        deploymentInfo: config.binding.deploymentInfo,
        network: config.binding.network,
        signer: config.signer,
        headerHash: admitted.artifact.headerHash,
        input,
        stateQueueMutationLeaseCoordinator:
          config.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      });
    }
    throw new Error(
      `min-ada ${admitted.artifact.kind} cannot execute ${input.stage}`,
    );
  },
});

export type ManifestBoundMinAdaWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: MinAdaWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  historicalNativeScriptCheckpointStore: HistoricalNativeScriptCheckpointStore;
  historicalNativeScriptHistorySource: HistoricalNativeScriptHistorySource;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundMinAdaWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"minAda">;
  l1: FraudProofFamilyL1ObservationPort<"minAda">;
  transactions: CursorFamilyTransactionPort<"minAda">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  historicalNativeScriptCheckpointStore: HistoricalNativeScriptCheckpointStore;
  historicalNativeScriptHistorySource: HistoricalNativeScriptHistorySource;
}>;

type HistoricalCorpusCell = {
  value?: HistoricalNativeScriptCorpus;
};

const historicalCorpusCells = new WeakMap<object, HistoricalCorpusCell>();

export const createManifestBoundMinAdaWorkflow = async (
  config: ManifestBoundMinAdaWorkflowConfig,
): Promise<ManifestBoundMinAdaWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "minAda",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      MinAdaStep02DatumSchema,
      MinAdaStep03DatumSchema,
      MinAdaStep04DatumSchema,
      MinAdaStep05DatumSchema,
    ],
  });
  requireHistoricalNativeScriptHistoryAuthority({
    deploymentFingerprint: binding.deploymentFingerprint,
    checkpointStore: config.historicalNativeScriptCheckpointStore,
    historySource: config.historicalNativeScriptHistorySource,
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.minAda;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  const certificate = binding.fieldPreimageCertificate;
  if (
    chain === undefined ||
    stateQueuePolicyId === undefined ||
    certificate === null
  ) {
    throw new Error("min-ada manifest omitted required contracts");
  }
  const stepNames = [
    "fraudProofMinAda",
    "fraudProofMinAdaStep02",
    "fraudProofMinAdaStep03",
    "fraudProofMinAdaStep04",
    "fraudProofMinAdaStep05",
  ] as const;
  const steps = stepNames.map((contractName, index) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName,
      utxo: config.referenceScripts.steps[index]!,
    }),
  ) as unknown as MinAdaWorkflowReferenceScripts["steps"];
  const witness = <Name extends keyof FaultProofWitnessReferenceScripts>(
    name: Name,
    contractName: string,
  ) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName,
      utxo: config.referenceScripts.witnesses[name],
    });
  const references: MinAdaWorkflowReferenceScripts = Object.freeze({
    steps: Object.freeze(steps),
    yields: Object.freeze({
      tx: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofMinAdaStep02TxWithdraw",
        utxo: config.referenceScripts.yields.tx,
      }),
      utxo: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofMinAdaStep02UtxoWithdraw",
        utxo: config.referenceScripts.yields.utxo,
      }),
    }),
    witnesses: Object.freeze({
      computationThreadMint: witness(
        "computationThreadMint",
        "computationThreadMint",
      ),
      fraudProofMint: witness("fraudProofMint", "fraudProofMint"),
      phasMembershipWithdraw: witness(
        "phasMembershipWithdraw",
        "phasMembershipWithdraw",
      ),
      chunkedVerifyWithdraw: witness(
        "chunkedVerifyWithdraw",
        "chunkedVerifyWithdraw",
      ),
      pexcludesWithdraw: witness("pexcludesWithdraw", "pexcludesWithdraw"),
    }),
    fieldPreimageCertificateMint: requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName: "fieldPreimageCertificateMint",
      utxo: config.referenceScripts.fieldPreimageCertificateMint,
    }),
  });
  const contracts: MinAdaContracts = Object.freeze({
    steps: chain.steps,
    yields: chain.yields,
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
    referenceScriptAuthPolicyId:
      parseContractDeploymentReferenceScriptAuthPolicyId(
        binding.deploymentInfo,
        "reference-script-auth minting",
      ),
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  if (l1.rawL1 === undefined)
    throw new Error("min-ada raw L1 authority is unavailable");
  const corpusCell: HistoricalCorpusCell = {};
  const bound: BoundConfig = {
    binding,
    lucid: config.lucid,
    signer: config.signer,
    contracts,
    references,
    historicalCorpus: () => {
      const corpus = corpusCell.value;
      if (corpus === undefined) {
        throw new Error(
          "min-ada history was not derived from this workflow's public authority",
        );
      }
      return corpus;
    },
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  };
  const transactions = transactionPort(bound);
  let adapter = createCursorFamilyWorkflowAdapter({
    spec: MIN_ADA_CURSOR_SPEC,
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const fieldPrerequisite = createAuthenticatedFieldCarriagePrerequisitePort({
    category: "minAda",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    requirementForAction: ({ action, artifact }) => {
      if (action.input.stage !== "step_02") return null;
      const admitted = admitMinAdaArtifact(artifact);
      if (!isTx(admitted)) return null;
      return {
        planned: txFieldPlan(admitted, config.signer.paymentKeyHash),
        compactCbor: admitted.prepared.nativeTxCompactCbor,
        certificate: {
          policyId: certificate.policyId,
          mintingScript: certificate.mintingScript,
          referenceScriptUtxo: references.fieldPreimageCertificateMint,
        },
      } satisfies FieldCarriageRequirement;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withFieldCarriagePrerequisite({
    category: "minAda",
    base: adapter,
    prerequisite: fieldPrerequisite,
  });
  const proofPrerequisite = createAuthenticatedProofChunkPrerequisitePort({
    category: "minAda",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    proofCborForAction: ({ action, artifact }) => {
      const admitted = admitMinAdaArtifact(artifact);
      return action.input.stage === "step_01" && isTx(admitted)
        ? admitted.prepared.txInclusion.txMembershipProofCbor
        : action.input.stage === "step_02" && !isTx(admitted)
          ? admitted.prepared.postMembershipProofCbor
          : action.input.stage === "step_04" && !isTx(admitted)
            ? admitted.prepared.predecessorNonMembershipProofCbor
            : null;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withProofChunkPrerequisite({
    category: "minAda",
    base: adapter,
    prerequisite: proofPrerequisite,
  });
  const workflow = Object.freeze({
    binding,
    l1,
    transactions,
    adapter,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
    historicalNativeScriptCheckpointStore:
      config.historicalNativeScriptCheckpointStore,
    historicalNativeScriptHistorySource:
      config.historicalNativeScriptHistorySource,
  });
  historicalCorpusCells.set(workflow, corpusCell);
  return workflow;
};

export const runOrResumeManifestBoundMinAdaWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMinAdaWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  const evidence = await fetchCanonicalBlockEvidence({
    observation,
    sources,
    minimumConfirmationDepth:
      workflow.binding.releaseFinality.policy.confirmationDepth,
  });
  const corpus = await resolveHistoricalNativeScriptCorpus({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    checkpointStore: workflow.historicalNativeScriptCheckpointStore,
    historySource: workflow.historicalNativeScriptHistorySource,
    currentEvidence: evidence,
    sources,
  });
  const cell = historicalCorpusCells.get(workflow);
  if (cell === undefined) {
    throw new Error(
      "min-ada workflow was not created by its manifest-bound constructor",
    );
  }
  if (
    cell.value !== undefined &&
    cell.value.corpusDigest !== corpus.corpusDigest
  ) {
    throw new Error("min-ada authenticated history changed across resume");
  }
  cell.value = corpus;
  const replayer =
    createMinAdaCompleteCanonicalReplayFromHistoricalCorpus(corpus);
  const decision = await replayer.replay(evidence);
  const detections = requireCompleteCanonicalReplayDecision({
    evidence,
    replayer,
    decision,
  });
  return await runFraudProofWorkflow({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    evidence,
    detections,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["minAda"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
