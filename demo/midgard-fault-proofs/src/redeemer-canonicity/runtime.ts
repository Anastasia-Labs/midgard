import {
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxWitnessSetCompact,
  deriveMidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core";
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { deriveRejectedTransactionFaultEvidenceMaterial } from "../evidence/rejected-transaction.js";
import {
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import { requireLinearFaultThreadUtxo } from "../linear-fault-family.js";
import { buildTrieView, requireProof } from "../prepare-double-spend.js";
import { type StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
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
  workflowActuationDecisionDigest,
  workflowJournalIsReconciliationOnly,
} from "../workflow/actuation-permit.js";
import {
  WORKFLOW_ADAPTER_RUNNER,
  type WorkflowAdapterReadinessInput,
  type WorkflowAdapterRunner,
} from "../workflow/adapters.js";
import { REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import {
  createCursorFamilyWorkflowAdapter,
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyCapturedAction,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
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
} from "../workflow/family-l1-observation.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  withFieldCarriagePrerequisite,
} from "../workflow/field-carriage-prerequisite.js";
import { bindWorkflowFundingReservationJournal } from "../workflow/funding-reservation-permit.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalStore,
  journalJsonDigest,
  type JournalJsonObject,
} from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  resumeRecordedFraudProofWorkflow,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import { continuePendingWorkflow } from "../workflow/pending-continuation.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import {
  detectRedeemerCanonicityFromCanonicalBlock,
  type RedeemerCanonicityDetection,
} from "./authenticated-workflow.js";
import {
  REDEEMER_CANONICITY_BLUEPRINT_TITLES,
  type RedeemerCanonicityContracts,
} from "./contracts.js";
import { prepareRedeemerCanonicityEvidence } from "./family.js";
import {
  RedeemerCanonicityStep01SourceSchema,
  RedeemerCanonicityStep02DatumSchema,
  RedeemerCanonicityStep03DatumSchema,
  RedeemerCanonicityVerdictSubjectSchema,
} from "./schemas.js";
import {
  submitRedeemerCanonicityStep01Accepted,
  submitRedeemerCanonicityStep01Forced,
} from "./submit-step-01.js";
import { submitRedeemerCanonicityStep02 } from "./submit-step-02.js";
import { submitRedeemerCanonicityStep03 } from "./submit-step-03.js";

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
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
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
  const core = Object.freeze({
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
  const adapter = createRedeemerCanonicityWorkflowAdapter(core);
  return Object.freeze({
    ...core,
    adapter,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(
      core.l1,
    ),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
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

type RedeemerWorkflowCore = Omit<
  ManifestBoundRedeemerCanonicityWorkflow,
  "adapter" | "terminalVerifier" | "releaseFinalityAuthority"
>;

export const prepareRedeemerCanonicityWorkflowArtifact = async (
  block: CanonicalBlockEvidence,
): Promise<JournalJsonObject> => {
  const detection = selectDetection(
    detectRedeemerCanonicityFromCanonicalBlock(block),
  );
  const accepted =
    detection.source === "accepted"
      ? block.transactions.find(
          (tx) => tx.nodeTxId === detection.evidence.subject.transaction_id,
        )
      : undefined;
  const forced =
    detection.source === "forced"
      ? block.reconstruction.forcedTransactions.find(
          (tx) => tx.value.tx_id === detection.evidence.subject.transaction_id,
        )
      : undefined;
  const material =
    accepted !== undefined
      ? deriveMidgardNativeTxFaultEvidenceMaterial(
          Buffer.from(accepted.txCbor, "hex"),
        )
      : deriveRejectedTransactionFaultEvidenceMaterial(
          forced!.fullTransactionCbor,
        );
  const trie =
    accepted === undefined
      ? undefined
      : await buildTrieView(
          block.transactions.map((tx) => ({
            key: Buffer.from(tx.nodeTxId, "hex"),
            value: Buffer.from(tx.l2TransactionSourceCbor, "hex"),
          })),
        );
  const membership =
    forced === undefined
      ? undefined
      : await buildForcedTransactionLeafMembershipProof({
          reconstruction: block.reconstruction,
          eventKey: { ForcedTransactionEventKey: { tx_order_id: forced.key } },
        });
  return {
    schemaVersion: "midgard-redeemer-canonicity-workflow-artifact-v1",
    headerHash: block.headerHash,
    detectionId: detection.detectionId,
    subjectCbor: Data.to(
      detection.evidence.subject as never,
      RedeemerCanonicityVerdictSubjectSchema as never,
    ),
    redeemerIndex: detection.evidence.redeemerIndex,
    fieldPreimageHex: detection.evidence.fieldPreimageHex,
    fieldCommitmentHex: detection.evidence.fieldCommitmentHex,
    nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
    witnessSetCompactCbor:
      material.proofSource.witnessSetCompactCbor.toString("hex"),
    accepted:
      accepted === undefined
        ? null
        : {
            nativeTxId: accepted.nodeTxId,
            nativeTxCompactCbor:
              material.proofSource.compactCbor.toString("hex"),
            l2TransactionSourceCbor: accepted.l2TransactionSourceCbor,
            transactionsPhasRoot: trie!.root,
            txMembershipProofCbor: requireProof(
              trie!,
              Buffer.from(accepted.nodeTxId, "hex"),
              "redeemer-canonicity transaction",
            ),
          },
    forcedSourceCbor:
      forced === undefined
        ? null
        : Data.to(
            {
              ForcedSource: {
                input_index: 0n,
                output_index: 0n,
                header: block.header,
                membership,
                direction: detection.evidence.subject.direction,
              },
            } as never,
            RedeemerCanonicityStep01SourceSchema as never,
          ),
  };
};

export const admitRedeemerWorkflowArtifact = (artifact: JournalJsonObject) => {
  if (
    artifact.schemaVersion !==
      "midgard-redeemer-canonicity-workflow-artifact-v1" ||
    typeof artifact.headerHash !== "string" ||
    !/^[0-9a-f]{56}$/u.test(artifact.headerHash)
  )
    throw new Error("redeemerCanonicity prepared artifact changed");
  const subject = Data.from(
    cursorStringField(artifact, "subjectCbor"),
    RedeemerCanonicityVerdictSubjectSchema as never,
  ) as Parameters<
    typeof prepareRedeemerCanonicityEvidence
  >[0]["finding"]["subject"];
  const evidence = prepareRedeemerCanonicityEvidence({
    finding: { subject, redeemerIndex: Number(artifact.redeemerIndex) },
    fieldPreimage: Buffer.from(
      cursorStringField(artifact, "fieldPreimageHex"),
      "hex",
    ),
    committedFieldHashHex: cursorStringField(artifact, "fieldCommitmentHex"),
  });
  const nativeTxCompactCbor = cursorStringField(
      artifact,
      "nativeTxCompactCbor",
    ),
    witnessSetCompactCbor = cursorStringField(
      artifact,
      "witnessSetCompactCbor",
    );
  const accepted =
    artifact.accepted === null
      ? null
      : parseSubmitStep01TxInclusion({
          ...(artifact.accepted as JournalJsonObject),
          nativeTx: nativeTxFromCoreCompact(
            decodeMidgardNativeTxCompact(
              Buffer.from(nativeTxCompactCbor, "hex"),
            ),
          ),
        });
  const forced =
    artifact.forcedSourceCbor === null
      ? null
      : (
          Data.from(
            cursorStringField(artifact, "forcedSourceCbor"),
            RedeemerCanonicityStep01SourceSchema as never,
          ) as {
            ForcedSource: Parameters<
              typeof submitRedeemerCanonicityStep01Forced
            >[0]["forcedSource"];
          }
        ).ForcedSource;
  if (
    (accepted === null) === (forced === null) ||
    (subject.source_kind === 0n) !== (accepted !== null)
  )
    throw new Error("redeemerCanonicity prepared source changed");
  return {
    artifact,
    evidence,
    nativeTxCompactCbor,
    witnessSetCompactCbor,
    accepted,
    forced,
  };
};

const redeemerField = (
  workflow: RedeemerWorkflowCore,
  artifact: JournalJsonObject,
) => {
  const admitted = admitRedeemerWorkflowArtifact(artifact);
  const witnessSet = decodeMidgardNativeTxWitnessSetCompact(
    Buffer.from(admitted.witnessSetCompactCbor, "hex"),
  );
  return {
    admitted,
    planned: planFaultProofFieldOpening({
      fieldIndex: 8,
      anchorTxId: admitted.evidence.subject.transaction_id,
      nativeTxCompactCbor: admitted.nativeTxCompactCbor,
      itemCbors: decodeMidgardFieldPreimage(
        Buffer.from(admitted.evidence.fieldPreimageHex, "hex"),
      ),
      owner: workflow.signer.paymentKeyHash,
      witnessSet: {
        addr_tx_wits_hash: Buffer.from(witnessSet.addrTxWitsHash).toString(
          "hex",
        ),
        script_tx_wits_hash: Buffer.from(witnessSet.scriptTxWitsHash).toString(
          "hex",
        ),
        redeemer_tx_wits_hash: Buffer.from(
          witnessSet.redeemerTxWitsHash,
        ).toString("hex"),
      },
      anchorWitnessSetHash: Buffer.from(
        decodeMidgardNativeTxCompact(
          Buffer.from(admitted.nativeTxCompactCbor, "hex"),
        ).transactionWitnessSetHash,
      ).toString("hex"),
      label: "redeemer-canonicity field opening",
    }),
  };
};

const captureRedeemerAction = async (
  workflow: RedeemerWorkflowCore,
  action: FraudProofWorkflowAction,
  artifact: JournalJsonObject,
): Promise<CursorFamilyCapturedAction> => {
  const admitted = admitRedeemerWorkflowArtifact(artifact);
  const input = action.input;
  const categoryId = workflow.binding.resolvedContracts.category.categoryId;
  if (input.stage === "remove")
    return await captureCursorRemoval({
      category: "redeemerCanonicity",
      lucid: workflow.lucid,
      blueprint: workflow.binding.blueprint,
      deploymentInfo: workflow.binding.deploymentInfo,
      network: workflow.binding.network,
      signer: workflow.signer,
      headerHash: cursorStringField(artifact, "headerHash"),
      input: input as { stage: string },
      stateQueueMutationLeaseCoordinator:
        workflow.stateQueueMutationLeaseCoordinator,
      fraudProverRewardLovelace: BigInt(
        workflow.binding.releaseEconomics.policy.fraudProverRewardLovelace,
      ),
    });
  const transaction = await captureLocallyEvaluatedTransaction(
    async (preSubmitBoundary) => {
      const common = {
        lucid: workflow.lucid,
        contracts: workflow.contracts,
        signer: workflow.signer,
        categoryId,
        preSubmitBoundary,
        awaitConfirmation: false,
      };
      if (input.stage === "init")
        await submitInit({
          ...common,
          blueprint: workflow.binding.blueprint,
          deploymentInfo: workflow.binding.deploymentInfo,
          network: workflow.binding.network,
          fraudCategory: "redeemerCanonicity",
          fraudulentBlockOutRef: cursorStringField(
            input,
            "stateQueueBlockOutRef",
          ),
          fraudulentHeaderHash: cursorStringField(artifact, "headerHash"),
          witnessReferenceScripts: workflow.referenceScripts.witnesses,
        });
      else if (input.stage === "step_01") {
        if (admitted.accepted !== null) {
          const { threadUtxo, threadToken } =
            await requireLinearFaultThreadUtxo({
              lucid: workflow.lucid,
              contracts: workflow.contracts,
              categoryId,
              family: "redeemer-canonicity",
              stepIndex: 0,
              threadOutRef: cursorStringField(input, "threadOutRef"),
            });
          await submitRedeemerCanonicityStep01Accepted({
            ...common,
            blueprint: workflow.binding.blueprint,
            network: workflow.binding.network,
            finding: admitted.evidence,
            threadUtxo,
            threadToken,
            stateQueueBlockOutRef: cursorStringField(
              input,
              "stateQueueBlockOutRef",
            ),
            txInclusion: admitted.accepted,
            referenceScriptUtxo: workflow.referenceScripts.steps[0],
            witnessReferenceScripts: workflow.referenceScripts.witnesses,
          });
        } else
          await submitRedeemerCanonicityStep01Forced({
            ...common,
            threadOutRef: cursorStringField(input, "threadOutRef"),
            finding: admitted.evidence,
            forcedSource: admitted.forced!,
            witnessSetHash: Buffer.from(
              decodeMidgardNativeTxCompact(
                Buffer.from(admitted.nativeTxCompactCbor, "hex"),
              ).transactionWitnessSetHash,
            ).toString("hex"),
            referenceScriptUtxo: workflow.referenceScripts.steps[0],
          });
      } else if (input.stage === "step_02") {
        const { planned } = redeemerField(workflow, artifact);
        const publishedCarriageUtxos =
          await resolveFaultProofFieldCarriagePublications({
            lucid: workflow.lucid,
            publisherAddress: workflow.signer.address,
            planned,
          });
        if (publishedCarriageUtxos === undefined)
          throw new Error("redeemerCanonicity field publications disappeared");
        const certificateUtxo = await resolveFaultProofFieldPreimageCertificate(
          {
            lucid: workflow.lucid,
            network: workflow.binding.network,
            planned,
            certificatePolicyId:
              workflow.contracts.fieldPreimageCertificatePolicyId,
          },
        );
        if (planned.plan.tier === "Certified" && certificateUtxo === undefined)
          throw new Error("redeemerCanonicity certificate disappeared");
        await submitRedeemerCanonicityStep02({
          ...common,
          threadOutRef: cursorStringField(input, "threadOutRef"),
          evidence: admitted.evidence,
          nativeTxCompactCbor: admitted.nativeTxCompactCbor,
          witnessSetCompactCbor: admitted.witnessSetCompactCbor,
          publishedCarriageUtxos,
          certificateUtxo,
          referenceScriptUtxo: workflow.referenceScripts.steps[1],
          certificateReferenceScriptUtxo:
            workflow.referenceScripts.fieldPreimageCertificateMint,
        });
      } else if (input.stage === "step_03")
        await submitRedeemerCanonicityStep03({
          ...common,
          threadOutRef: cursorStringField(input, "threadOutRef"),
          evidence: admitted.evidence,
          referenceScriptUtxo: workflow.referenceScripts.steps[2],
          witnessReferenceScripts: workflow.referenceScripts.witnesses,
        });
      else throw new Error("redeemerCanonicity action changed");
    },
  );
  return { transaction };
};

const createRedeemerCanonicityWorkflowAdapter = (
  workflow: RedeemerWorkflowCore,
): FraudProofFamilyWorkflowAdapter =>
  withFieldCarriagePrerequisite({
    category: "redeemerCanonicity",
    prerequisite: createAuthenticatedFieldCarriagePrerequisitePort({
      category: "redeemerCanonicity",
      lucid: workflow.lucid,
      network: workflow.binding.network,
      signer: workflow.signer,
      publications: workflow.l1.publications,
      transactionConfirmed: async ({ headerHash, txHash }) =>
        await workflow.l1.transactionConfirmed({ headerHash, txHash }),
      requirementForAction: ({ action, artifact }) => {
        if (action.input.stage !== "step_02") return null;
        const { planned, admitted } = redeemerField(workflow, artifact);
        return {
          planned,
          compactCbor: admitted.nativeTxCompactCbor,
          witnessSetCompactCbor: admitted.witnessSetCompactCbor,
          certificate: {
            policyId: workflow.contracts.fieldPreimageCertificatePolicyId,
            mintingScript:
              workflow.contracts.fieldPreimageCertificateMintingScript,
            referenceScriptUtxo:
              workflow.referenceScripts.fieldPreimageCertificateMint,
          },
        };
      },
    }),
    base: createCursorFamilyWorkflowAdapter({
      spec: {
        category: "redeemerCanonicity",
        stepCount: 3,
        successors: { 1: [2], 2: [3], 3: ["proof_token"] },
      },
      l1: workflow.l1,
      stateQueueMutationLeaseCoordinator:
        workflow.stateQueueMutationLeaseCoordinator,
      transactions: {
        portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
        category: "redeemerCanonicity",
        validatePreparedArtifact: async ({ evidence, artifact }) => {
          if (
            journalJsonDigest(
              await prepareRedeemerCanonicityWorkflowArtifact(evidence),
            ) !== journalJsonDigest(artifact)
          )
            throw new Error(
              "prepared family artifact differs from retained evidence",
            );
        },
        prepare: async ({ evidence }) =>
          await prepareRedeemerCanonicityWorkflowArtifact(evidence),
        capture: async ({ action, artifact }) =>
          await captureRedeemerAction(workflow, action, artifact),
      },
    }),
  });

export type RedeemerCanonicityRuntimeDependencies = Readonly<{
  journal: FraudProofWorkflowJournalStore;
}>;
export const executeManifestBoundRedeemerCanonicityWorkflow = async ({
  workflow,
  sources,
  runtime,
}: {
  readonly workflow: ManifestBoundRedeemerCanonicityWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly runtime: RedeemerCanonicityRuntimeDependencies;
}): Promise<FraudProofWorkflowRunResult> => {
  const journal = runtime.journal;
  if (workflowActuationDecisionDigest(journal) !== workflow.decisionDigest)
    throw new Error("redeemerCanonicity journal changed decision digest");
  if (workflowJournalIsReconciliationOnly(journal))
    return await resumeRecordedFraudProofWorkflow({
      deploymentFingerprint: workflow.binding.deploymentFingerprint,
      category: "redeemerCanonicity",
      headerHash: workflow.binding.definition.headerHash,
      journal,
      adapter: workflow.adapter,
      terminalVerifier: workflow.terminalVerifier,
      releaseFinalityAuthority: workflow.releaseFinalityAuthority,
    });
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["redeemerCanonicity"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
export const runOrResumeManifestBoundRedeemerCanonicityWorkflow =
  executeManifestBoundRedeemerCanonicityWorkflow;
export const createManifestBoundRedeemerCanonicityRuntime = (
  input: Parameters<typeof executeManifestBoundRedeemerCanonicityWorkflow>[0],
) =>
  Object.freeze({
    runOrResume: async () =>
      await executeManifestBoundRedeemerCanonicityWorkflow(input),
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
        return await continuePendingWorkflow({
          invocation,
          journal: durable,
          execute: () =>
            executeManifestBoundRedeemerCanonicityWorkflow({
              workflow,
              sources: loaded.retainedDaSources,
              runtime: {
                journal: durable,
              },
            }),
        });
      } finally {
        await loaded.close();
      }
    },
  });

export const createRedeemerCanonicityWorkflowRunnerFactory =
  createRedeemerCanonicityWorkflowRunnerSurface;
