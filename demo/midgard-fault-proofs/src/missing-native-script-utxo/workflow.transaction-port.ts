import { resolvePublishedProofChunks } from "../publish-proof-chunks.js";
import { parseSubmitStep01TxInclusion } from "../step-support.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import {
  admitMissingNativeScriptUtxoArtifact,
  prepareMissingNativeScriptUtxoArtifact,
} from "./artifact.js";
import { submitMissingNativeScriptUtxoInit } from "./submit-init.js";
import { submitMissingNativeScriptUtxoStep01 } from "./submit-step-01.js";
import { submitMissingNativeScriptUtxoStep02 } from "./submit-step-02.js";
import { submitMissingNativeScriptUtxoStep03 } from "./submit-step-03.js";
import { submitMissingNativeScriptUtxoStep04 } from "./submit-step-04.js";
import { submitMissingNativeScriptUtxoStep05 } from "./submit-step-05.js";
import {
  submitMissingNativeScriptUtxoStep05StartGrammar,
  submitMissingNativeScriptUtxoStep06,
} from "./submit-step-06.js";
import { submitMissingNativeScriptUtxoStep07 } from "./submit-step-07.js";
import {
  type BoundConfig,
  bytes,
  direct,
  resolveField,
  scriptFieldPlan,
  spendFieldPlan,
  spendInputs,
  witnessSet,
} from "./workflow.resolve-field.js";

export const transactionPort = (
  config: BoundConfig,
): CursorFamilyTransactionPort<"missingNativeScriptUtxo"> => ({
  portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
  category: "missingNativeScriptUtxo",
  prepare: async ({ evidence, classification }) =>
    await prepareMissingNativeScriptUtxoArtifact({
      evidence,
      historicalNativeScriptCorpus: config.historicalCorpus(),
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitMissingNativeScriptUtxoArtifact(artifact);
    if (admitted.artifact.headerHash !== config.binding.definition.headerHash) {
      throw new Error(
        "missing-native-script-utxo artifact changed the bound header",
      );
    }
    const input = cursorFamilyActionInput({
      category: "missingNativeScriptUtxo",
      action,
    });
    const categoryId = config.binding.resolvedContracts.category.categoryId;
    const threadOutRef = () => cursorStringField(input, "threadOutRef");
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptUtxoInit({
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
      const chunks = await resolvePublishedProofChunks({
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.prepared.txInclusion.txMembershipProofCbor,
      });
      if (chunks === undefined)
        throw new Error("missing-native-script-utxo tx proof disappeared");
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptUtxoStep01({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              stateQueueBlockOutRef: cursorStringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: parseSubmitStep01TxInclusion(
                admitted.prepared.txInclusion,
              ),
              prevUtxosRoot: admitted.prepared.prevUtxosRoot,
              referenceScriptUtxo: config.references.steps[0],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_02") {
      const carriage = await resolveField({
        config,
        planned: spendFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptUtxoStep02({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
              spendInputs: spendInputs(admitted),
              badInputIndex: admitted.prepared.badInputIndex,
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : { certificateUtxo: carriage.certificate }),
              referenceScriptUtxo: config.references.steps[1],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_03") {
      const chunks = await resolvePublishedProofChunks({
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.prepared.membershipProofCbor,
      });
      if (chunks === undefined)
        throw new Error(
          "missing-native-script-utxo membership proof disappeared",
        );
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptUtxoStep03({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              prepared: admitted.prepared,
              referenceScriptUtxo: config.references.steps[2],
              publishedProofChunks: chunks,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_04") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptUtxoStep04({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              missingNativeScriptBytes:
                admitted.prepared.missingNativeScriptBytes,
              referenceScriptUtxo: config.references.steps[3],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_05") {
      const carriage = await resolveField({
        config,
        planned: scriptFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            const shared = {
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
              witnessSet: witnessSet(admitted),
              scriptTxWitsItems: bytes(
                admitted.prepared.scriptWitnessItemCbors,
              ),
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : { certificateUtxo: carriage.certificate }),
              referenceScriptUtxo: config.references.steps[4],
              preSubmitBoundary,
              awaitConfirmation: false,
            } as const;
            if (direct(admitted)) {
              await submitMissingNativeScriptUtxoStep05({
                ...shared,
                scriptWitnessItems: shared.scriptTxWitsItems,
                witnessReferenceScripts: config.references.witnesses,
              });
            } else {
              await submitMissingNativeScriptUtxoStep05StartGrammar(shared);
            }
          },
        ),
      });
    }
    if (input.stage === "step_06") {
      const carriage = await resolveField({
        config,
        planned: scriptFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptUtxoStep06({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
              witnessSet: witnessSet(admitted),
              scriptTxWitsItems: bytes(
                admitted.prepared.scriptWitnessItemCbors,
              ),
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : { certificateUtxo: carriage.certificate }),
              referenceScriptUtxo: config.references.steps[5],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_07") {
      const carriage = await resolveField({
        config,
        planned: scriptFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptUtxoStep07({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
              witnessSet: witnessSet(admitted),
              scriptTxWitsItems: bytes(
                admitted.prepared.scriptWitnessItemCbors,
              ),
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : { certificateUtxo: carriage.certificate }),
              referenceScriptUtxo: config.references.steps[6],
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
        category: "missingNativeScriptUtxo",
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
      `missing-native-script-utxo unsupported stage ${input.stage}`,
    );
  },
});
