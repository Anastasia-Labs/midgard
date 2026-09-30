import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { CommittedFieldShapeContracts } from "../committed-field-shape/contracts.js";
import { submitCommittedFieldShapeInit } from "../committed-field-shape/submit-committed-field-shape-init.js";
import { submitCommittedFieldShapeStep01 } from "../committed-field-shape/submit-committed-field-shape-step-01.js";
import { submitCommittedFieldShapeStep02 } from "../committed-field-shape/submit-committed-field-shape-step-02.js";
import {
  admitCanonicalEvidenceForProofBuild,
  type CanonicalEvidenceBuilderInput,
} from "../evidence/prepare-from-evidence.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  requireProof,
  requireTransactionsRootMatch,
  transactionSourceTrieItem,
} from "../prepare-double-spend.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { CanonicalBlockClassification } from "./classification.js";
import {
  admitCommittedFieldShapeArtifact,
  COMMITTED_FIELD_SHAPE_ARTIFACT,
  type CommittedFieldShapeArtifact,
  fieldIndexFromClassification,
  record,
} from "./committed-field-shape.parse-artifact.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import {
  type LinearFamilyAssemblyContext,
  type LinearFamilyReferenceScripts,
  type ManifestBoundLinearFamilyWorkflow,
  type ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.js";
import { normalizeJournalJson } from "./journal.js";
import {
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import { type FraudProofWorkflowAction } from "./orchestrator.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

const prepareArtifactFromEvidence = async ({
  evidence,
  classification,
}: CanonicalEvidenceBuilderInput & {
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  > & { readonly category: "committedFieldShape" };
}): Promise<CommittedFieldShapeArtifact> => {
  const admitted = admitCanonicalEvidenceForProofBuild(evidence);
  if (
    classification.headerHash !== admitted.headerHash ||
    classification.selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(
      "committed-field-shape classification differs from canonical evidence",
    );
  }
  const transactionIndex = Number(classification.selected.position);
  const transaction = admitted.transactions[transactionIndex];
  if (transaction === undefined) {
    throw new Error(
      "committed-field-shape classification selected an absent transaction",
    );
  }
  const fieldIndex = fieldIndexFromClassification({
    classification,
    transactionIndex,
    nodeTxId: transaction.nodeTxId,
  });
  const decoded = await Promise.all(
    admitted.transactions.map(decodeTransactionMaterial),
  );
  const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
  if (trie.root !== evidence.reconstruction.rootData.transactions.phasRoot) {
    throw new Error(
      "committed-field-shape canonical source leaves differ from reconstructed DA",
    );
  }
  await requireTransactionsRootMatch({
    sourceRoot: trie.root,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    count: BigInt(decoded.length),
  });
  const proof = requireProof(
    trie,
    Buffer.from(transaction.nodeTxId, "hex"),
    "committed-field-shape transaction",
  );
  const artifact = normalizeJournalJson({
    schemaVersion: COMMITTED_FIELD_SHAPE_ARTIFACT,
    headerHash: admitted.headerHash,
    committedTransactionsRoot: admitted.expectedTransactionsRoot,
    l2TransactionCount: decoded.length,
    transactionsPhasRoot: trie.root,
    selectedTransactionIndex: transactionIndex,
    selectedFieldIndex: fieldIndex,
    txMembershipProofCbor: proof,
    transactions: admitted.transactions.map((item) => ({
      nodeTxId: item.nodeTxId,
      txCbor: item.txCbor,
      l2TransactionSourceCbor: item.l2TransactionSourceCbor,
    })),
  }) as CommittedFieldShapeArtifact;
  await admitCommittedFieldShapeArtifact(artifact);
  return Object.freeze(artifact);
};

export const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
] as const;

export type CommittedFieldShapeWorkflowReferenceScripts =
  LinearFamilyReferenceScripts<
    "committedFieldShape",
    (typeof WITNESS_ROLES)[number],
    false
  >;

export type AssemblyContext = LinearFamilyAssemblyContext<
  "committedFieldShape",
  (typeof WITNESS_ROLES)[number],
  false
>;

export type BoundCommittedFieldShapeTransactionsConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  network: FraudProofWorkflowDeploymentBinding<"committedFieldShape">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: CommittedFieldShapeContracts;
  category: FraudProofWorkflowDeploymentBinding<"committedFieldShape">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBinding<"committedFieldShape">["catalogue"];
  referenceScripts: CommittedFieldShapeWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
  deploymentInfo: unknown;
}>;

export type CommittedFieldShapeBuilderSet = Readonly<{
  init: typeof submitCommittedFieldShapeInit;
  step01: typeof submitCommittedFieldShapeStep01;
  step02: typeof submitCommittedFieldShapeStep02;
  remove: typeof submitRemoveFraudulentBlock;
}>;

export const productionBuilders: CommittedFieldShapeBuilderSet = Object.freeze({
  init: submitCommittedFieldShapeInit,
  step01: submitCommittedFieldShapeStep01,
  step02: submitCommittedFieldShapeStep02,
  remove: submitRemoveFraudulentBlock,
});

const requiredAction = (
  action: FraudProofWorkflowAction,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "committed-field-shape workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "committedFieldShape" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("committed-field-shape workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  name: string,
): string => {
  const value = input[name];
  if (typeof value !== "string") {
    throw new Error(`committed-field-shape workflow action omitted ${name}`);
  }
  return value;
};

export const createBoundTransactionPort = ({
  config,
  builders,
}: {
  readonly config: BoundCommittedFieldShapeTransactionsConfig;
  readonly builders: CommittedFieldShapeBuilderSet;
}): LinearFamilyTransactionPort<"committedFieldShape"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "committedFieldShape",
  prepare: async ({ evidence, classification }) =>
    await prepareArtifactFromEvidence({ evidence, classification }),
  capture: async ({ action, artifact }) => {
    const admitted = await admitCommittedFieldShapeArtifact(artifact);
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error(
        "committed-field-shape artifact targets a different manifest-bound header",
      );
    }
    const input = requiredAction(action);
    if (input.stage === "init") {
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          await builders.init({
            lucid: config.lucid,
            blueprint: config.blueprint,
            network: config.network,
            contracts: config.contracts,
            category: config.category,
            catalogue: config.catalogue,
            signer: config.signer,
            fraudulentBlockOutRef: stringField(input, "stateQueueBlockOutRef"),
            fraudulentHeaderHash: config.headerHash,
            witnessReferenceScripts: config.referenceScripts.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "step_01") {
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          await builders.step01({
            lucid: config.lucid,
            blueprint: config.blueprint,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            network: config.network,
            signer: config.signer,
            threadOutRef: stringField(input, "threadOutRef"),
            stateQueueBlockOutRef: stringField(input, "stateQueueBlockOutRef"),
            txInclusion: admitted.txInclusion,
            prepared: admitted.prepared,
            referenceScriptUtxo: config.referenceScripts.steps[0],
            witnessReferenceScripts: config.referenceScripts.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "step_02") {
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          await builders.step02({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            signer: config.signer,
            threadOutRef: stringField(input, "threadOutRef"),
            referenceScriptUtxo: config.referenceScripts.steps[1],
            witnessReferenceScripts: config.referenceScripts.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "remove") {
      let mutationLease: StateQueueMutationLease | undefined;
      const retainingCoordinator: StateQueueMutationLeaseCoordinator = {
        acquire: async () => {
          const acquired =
            await config.stateQueueMutationLeaseCoordinator.acquire();
          mutationLease = acquired;
          return acquired;
        },
      };
      const nextRemovalOutRef = stringField(input, "nextRemovalOutRef");
      const fraudProofOutRef = stringField(input, "fraudProofOutRef");
      const transaction = await captureLocallyEvaluatedTransaction(
        async (boundary) => {
          await builders.remove({
            lucid: config.lucid,
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            fraudCategory: "committedFieldShape",
            fraudulentHeaderHash: config.headerHash,
            requireReferenceScripts: true,
            stateQueueMutationLeaseCoordinator: retainingCoordinator,
            fraudProverRewardLovelace: config.fraudProverRewardLovelace,
            preSubmitBoundary: async (built) => {
              if (
                !workflowTransactionInputOutRefs(built.signed).includes(
                  nextRemovalOutRef,
                )
              ) {
                throw new Error(
                  "committed-field-shape removal does not consume the authenticated next queue input",
                );
              }
              if (
                !workflowTransactionReferenceInputOutRefs(
                  built.signed,
                ).includes(fraudProofOutRef)
              ) {
                throw new Error(
                  "committed-field-shape removal does not reference the authenticated retained proof token",
                );
              }
              await boundary(built);
            },
          });
        },
      );
      return Object.freeze({
        transaction,
        ...(mutationLease === undefined ? {} : { mutationLease }),
      });
    }
    throw new Error(
      `committed-field-shape workflow action has unsupported stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundCommittedFieldShapeWorkflowConfig =
  ManifestBoundLinearFamilyWorkflowConfig<
    "committedFieldShape",
    (typeof WITNESS_ROLES)[number],
    false
  >;

export type ManifestBoundCommittedFieldShapeWorkflow =
  ManifestBoundLinearFamilyWorkflow<"committedFieldShape", false>;
