import { submitInit } from "../submit-init.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import {
  historicalNativeScriptPreimageFromCorpus,
  requireHistoricalNativeScriptCorpusPreimage,
} from "../workflow/historical-native-script-corpus.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import {
  admitMissingNativeScriptTxArtifact,
  prepareMissingNativeScriptTxArtifact,
} from "./artifact.js";
import { resolveHistoricalNativeScriptEvidence } from "./historical-script.js";
import { submitMissingNativeScriptTxStep01 } from "./submit-missing-native-script-tx-step-01.js";
import { submitMissingNativeScriptTxStep02 } from "./submit-missing-native-script-tx-step-02.js";
import { submitMissingNativeScriptTxStep03 } from "./submit-missing-native-script-tx-step-03.js";
import { submitMissingNativeScriptTxStep04 } from "./submit-missing-native-script-tx-step-04.js";
import { submitMissingNativeScriptTxStep05 } from "./submit-missing-native-script-tx-step-05.js";
import { submitMissingNativeScriptTxStep06 } from "./submit-missing-native-script-tx-step-06.js";
import { submitMissingNativeScriptTxStep06StartGrammar } from "./submit-missing-native-script-tx-step-06-staged.js";
import { submitMissingNativeScriptTxStep07 } from "./submit-missing-native-script-tx-step-07.js";
import { submitMissingNativeScriptTxStep08 } from "./submit-missing-native-script-tx-step-08.js";
import {
  type BoundConfig,
  direct,
  expectedScriptHashFromDetection,
  outputFieldPlan,
  resolveField,
  scriptFieldPlan,
  spendFieldPlan,
} from "./workflow.resolve-field.js";

export const transactionPort = (
  config: BoundConfig,
): CursorFamilyTransactionPort<"missingNativeScriptTx"> => ({
  portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
  category: "missingNativeScriptTx",
  prepare: async ({ evidence, classification }) => {
    const corpus = config.historicalCorpus();
    const expectedScriptHash = expectedScriptHashFromDetection(
      classification.selected.detectionId,
    );
    const preimage = historicalNativeScriptPreimageFromCorpus({
      corpus,
      scriptHash: expectedScriptHash,
    });
    if (preimage === null) {
      throw new Error(
        "missing-native-script-tx complete history omitted the script preimage",
      );
    }
    const admittedPreimage =
      requireHistoricalNativeScriptCorpusPreimage(preimage);
    if (
      admittedPreimage.providerRosterDigest !==
      config.historicalSourceRoster.applicationOverlayDigest
    ) {
      throw new Error(
        "missing-native-script-tx L1 and retained-history authorities came from different application overlays",
      );
    }
    const corroboration = await resolveHistoricalNativeScriptEvidence({
      roster: config.historicalSourceRoster,
      expectedScriptHash,
      throughPoint: config.historicalThroughPoint(),
      releaseFinality: config.binding.releaseFinality,
      retainedDaCorroboratingScriptBytes: Buffer.from(
        admittedPreimage.scriptBytesHex,
        "hex",
      ),
    });
    return await prepareMissingNativeScriptTxArtifact({
      evidence,
      classification,
      historicalNativeScriptCorpus: corpus,
      historicalL1Corroboration: corroboration,
    });
  },
  capture: async ({ action, artifact }) => {
    const admitted = await admitMissingNativeScriptTxArtifact({
      value: artifact,
      historicalNativeScriptCorpus: config.historicalCorpus(),
      historicalSourceRoster: config.historicalSourceRoster,
      historicalThroughPoint: config.historicalThroughPoint(),
      releaseFinality: config.binding.releaseFinality,
    });
    if (admitted.artifact.headerHash !== config.binding.definition.headerHash) {
      throw new Error(
        "missing-native-script-tx artifact changed the bound header",
      );
    }
    const input = cursorFamilyActionInput({
      category: "missingNativeScriptTx",
      action,
    });
    const categoryId = config.binding.resolvedContracts.category.categoryId;
    const threadOutRef = () => cursorStringField(input, "threadOutRef");
    const evidence = admitted.evidence;
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitInit({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              fraudCategory: "missingNativeScriptTx",
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
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptTxStep01({
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
              txInclusion: evidence.badTxInclusion,
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
            await submitMissingNativeScriptTxStep02({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              nativeTxCompactCbor: evidence.badTxInclusion.nativeTxCompactCbor,
              spendInputs: evidence.badTxSpendInputs,
              badInputIndex: evidence.badInputIndex,
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
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptTxStep03({
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
              txInclusion: evidence.producingTxInclusion,
              referenceScriptUtxo: config.references.steps[2],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_04") {
      const carriage = await resolveField({
        config,
        planned: outputFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMissingNativeScriptTxStep04({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              nativeTxCompactCbor:
                evidence.producingTxInclusion.nativeTxCompactCbor,
              outputItemCbors: evidence.producingOutputItemCbors,
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : { certificateUtxo: carriage.certificate }),
              referenceScriptUtxo: config.references.steps[3],
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
            await submitMissingNativeScriptTxStep05({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              missingNativeScriptBytes: evidence.missingNativeScriptBytes,
              referenceScriptUtxo: config.references.steps[4],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_06") {
      const carriage = await resolveField({
        config,
        planned: scriptFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      const shared = {
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId,
        signer: config.signer,
        threadOutRef: threadOutRef(),
        nativeTxCompactCbor: evidence.badTxInclusion.nativeTxCompactCbor,
        witnessSet: evidence.badTxWitnessSet,
        scriptTxWitsItems: evidence.badTxScriptWitnessItemCbors,
        publishedCarriageUtxos: carriage.publications,
        ...(carriage.certificate === undefined
          ? {}
          : { certificateUtxo: carriage.certificate }),
        referenceScriptUtxo: config.references.steps[5],
        awaitConfirmation: false,
      } as const;
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            if (direct(admitted)) {
              await submitMissingNativeScriptTxStep06({
                ...shared,
                witnessReferenceScripts: config.references.witnesses,
                preSubmitBoundary,
              });
            } else {
              await submitMissingNativeScriptTxStep06StartGrammar({
                ...shared,
                preSubmitBoundary,
              });
            }
          },
        ),
      });
    }
    if (input.stage === "step_07" || input.stage === "step_08") {
      const carriage = await resolveField({
        config,
        planned: scriptFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      const shared = {
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId,
        signer: config.signer,
        threadOutRef: threadOutRef(),
        nativeTxCompactCbor: evidence.badTxInclusion.nativeTxCompactCbor,
        witnessSet: evidence.badTxWitnessSet,
        scriptTxWitsItems: evidence.badTxScriptWitnessItemCbors,
        publishedCarriageUtxos: carriage.publications,
        ...(carriage.certificate === undefined
          ? {}
          : { certificateUtxo: carriage.certificate }),
        referenceScriptUtxo:
          input.stage === "step_07"
            ? config.references.steps[6]
            : config.references.steps[7],
        awaitConfirmation: false,
      } as const;
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            if (input.stage === "step_07") {
              await submitMissingNativeScriptTxStep07({
                ...shared,
                preSubmitBoundary,
              });
            } else {
              await submitMissingNativeScriptTxStep08({
                ...shared,
                witnessReferenceScripts: config.references.witnesses,
                preSubmitBoundary,
              });
            }
          },
        ),
      });
    }
    if (input.stage === "remove") {
      return await captureCursorRemoval({
        category: "missingNativeScriptTx",
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
      `missing-native-script-tx unsupported stage ${input.stage}`,
    );
  },
});
