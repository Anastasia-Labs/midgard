import {
  computeHash32,
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardSpendInputItem,
  decodeMidgardVersionedScript,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  deriveMidgardNativeTxWitnessSetCompact,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardReferenceScriptSourceLeaf,
} from "@al-ft/midgard-core";
import {
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core/codec/forced";
import { forcedVerdictSubject, Proof } from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import { buildExecutionSourceMachineAuthenticationFromRetainedDa } from "../execution-source-script-decoding/retained-witness.js";
import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { buildTrieView, requireProof } from "../prepare-double-spend.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../step-support.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import { requireHistoricalNativeScriptCorpus } from "../workflow/historical-native-script-corpus.js";
import { type FraudProofWorkflowAction } from "../workflow/orchestrator.js";
import {
  captureLocallyEvaluatedTransaction,
  type FraudProofPreSubmitBoundary,
} from "../workflow/transaction-boundary.js";
import type { AcceptedReconstructionState } from "./accepted-reconstruction-machine.js";
import { reconstructExecutionNativeScriptPurposes } from "./canonical-reconstruction.js";
import { prepareExecutionNativeScriptInvalidEvidence } from "./family.js";
import { ExecutionNativeScriptInvalidAcceptedDatumSchema } from "./schemas.js";
import {
  submitExecutionNativeScriptInvalidAcceptedFinishInline,
  submitExecutionNativeScriptInvalidAcceptedFinishPurpose,
  submitExecutionNativeScriptInvalidAcceptedFinishReceivePass,
  submitExecutionNativeScriptInvalidAcceptedFinishSpends,
  submitExecutionNativeScriptInvalidAcceptedInit,
  submitExecutionNativeScriptInvalidAcceptedInlineSource,
  submitExecutionNativeScriptInvalidAcceptedMint,
  submitExecutionNativeScriptInvalidAcceptedObserver,
  submitExecutionNativeScriptInvalidAcceptedReceive,
  submitExecutionNativeScriptInvalidAcceptedReferenceSource,
  submitExecutionNativeScriptInvalidAcceptedSpend,
} from "./submit-accepted-reconstruction.js";
import { submitExecutionNativeScriptInvalidInit } from "./submit-init.js";
import {
  submitExecutionNativeScriptInvalidStep01Accepted,
  submitExecutionNativeScriptInvalidStep01Forced,
} from "./submit-step-01.js";
import { submitExecutionNativeScriptInvalidStep02 } from "./submit-step-02.js";
import { submitExecutionNativeScriptInvalidStep03 } from "./submit-step-03.js";
import { submitExecutionNativeScriptInvalidStep04 } from "./submit-step-04-route.js";
import { submitExecutionNativeScriptInvalidStep05 } from "./submit-step-05.js";
import { submitExecutionNativeScriptInvalidStep06 } from "./submit-step-06.js";
import {
  type ManifestBoundExecutionNativeScriptInvalidWorkflow,
  type PreparedExecutionNativeScriptInvalid,
} from "./v1.prepare-manifest-bound-execution-native-script-invalid-replay.js";

/** Builds one exact action; the common adapter owns signing records and submission. */
export const captureExecutionNativeScriptInvalidAction = async ({
  workflow,
  prepared,
  action,
}: {
  workflow: ManifestBoundExecutionNativeScriptInvalidWorkflow;
  prepared: PreparedExecutionNativeScriptInvalid;
  action: FraudProofWorkflowAction;
}) => {
  const input = cursorFamilyActionInput({
    category: "executionNativeScriptInvalid",
    action,
  });
  if (input.stage === "remove")
    return captureCursorRemoval({
      category: "executionNativeScriptInvalid",
      lucid: workflow.lucid,
      blueprint: workflow.binding.blueprint,
      deploymentInfo: workflow.binding.deploymentInfo,
      network: workflow.binding.network,
      signer: workflow.signer,
      headerHash: prepared.block.headerHash,
      input,
      stateQueueMutationLeaseCoordinator:
        workflow.stateQueueMutationLeaseCoordinator,
      fraudProverRewardLovelace: BigInt(
        workflow.binding.releaseEconomics.policy.fraudProverRewardLovelace,
      ),
    });
  const transaction = await captureLocallyEvaluatedTransaction(
    async (preSubmitBoundary: FraudProofPreSubmitBoundary) => {
      if (input.stage === "init") {
        await submitExecutionNativeScriptInvalidInit({
          lucid: workflow.lucid,
          blueprint: workflow.binding.blueprint as Parameters<
            typeof submitExecutionNativeScriptInvalidInit
          >[0]["blueprint"],
          network: workflow.binding.network,
          contracts: workflow.contracts,
          category: workflow.binding.resolvedContracts.category,
          catalogue: workflow.binding.catalogue,
          signer: workflow.signer,
          fraudulentBlockOutRef: cursorStringField(
            input,
            "stateQueueBlockOutRef",
          ),
          fraudulentHeaderHash: prepared.block.headerHash,
          witnessReferenceScripts: workflow.references.witnesses,
          awaitConfirmation: false,
          preSubmitBoundary,
        });
        return;
      }
      const activeStage = {
        step: Number(input.ordinal),
        threadOutRef: cursorStringField(input, "threadOutRef"),
        stateQueueBlockOutRef: cursorStringField(
          input,
          "stateQueueBlockOutRef",
        ),
      };
      const transactionEntry =
        prepared.detection.source === "accepted"
          ? prepared.block.transactions.find(
              ({ nodeTxId }) => nodeTxId === prepared.detection.transactionId,
            )
          : undefined;
      const forcedEntry =
        prepared.detection.source === "forced"
          ? prepared.block.reconstruction.forcedTransactions[
              prepared.detection.forcedIndex!
            ]
          : undefined;
      const txCbor =
        transactionEntry === undefined
          ? forcedEntry?.fullTransactionCbor
          : Buffer.from(transactionEntry.txCbor, "hex");
      if (txCbor === undefined)
        throw new Error(
          "executionNativeScriptInvalid selected transaction disappeared",
        );
      const tx = (
        forcedEntry === undefined
          ? decodeMidgardNativeTxFullFromCanonicalCbor
          : decodeMidgardForcedTxFullFromCanonicalCbor
      )(txCbor);
      const compactCbor = (
        forcedEntry === undefined
          ? deriveMidgardNativeTxFaultEvidenceMaterial
          : deriveMidgardForcedTxFaultEvidenceMaterial
      )(txCbor).proofSource.compactCbor.toString("hex");
      const compactWitness = deriveMidgardNativeTxWitnessSetCompact(
        tx.witnessSet,
      );
      const witnessSet = {
        addr_tx_wits_hash: Buffer.from(compactWitness.addrTxWitsHash).toString(
          "hex",
        ),
        script_tx_wits_hash: Buffer.from(
          compactWitness.scriptTxWitsHash,
        ).toString("hex"),
        redeemer_tx_wits_hash: Buffer.from(
          compactWitness.redeemerTxWitsHash,
        ).toString("hex"),
      };
      const addressWitnessItems = decodeMidgardFieldPreimage(
        tx.witnessSet.addrTxWitsPreimageCbor,
      );
      const history = requireHistoricalNativeScriptCorpus(prepared.corpus);
      const predecessor = history.reconstructions.at(-2);
      const priorOutputs = new Map(
        (predecessor?.utxos ?? []).map(({ key, value }) => [
          Buffer.from(key).toString("hex"),
          Buffer.from(value),
        ]),
      );
      const reconstruction = reconstructExecutionNativeScriptPurposes({
        sourceKind: forcedEntry === undefined ? "normal" : "forced",
        canonicalTransactionCbor: txCbor,
        resolvedOutputsByOutRef: priorOutputs,
      });
      const purpose =
        reconstruction.purposes[prepared.detection.executionIndex];
      if (purpose === undefined)
        throw new Error(
          "executionNativeScriptInvalid execution coordinate disappeared",
        );
      const common = {
        lucid: workflow.lucid,
        contracts: workflow.contracts,
        categoryId: workflow.binding.definition.categoryId,
        signer: workflow.signer,
        threadOutRef: activeStage.threadOutRef,
        awaitConfirmation: false,
        preSubmitBoundary,
      } as const;
      if (activeStage.step === 1) {
        if (transactionEntry !== undefined) {
          const material = deriveMidgardNativeTxFaultEvidenceMaterial(txCbor);
          const trie = await buildTrieView(
            prepared.block.transactions.map((entry) => ({
              key: Buffer.from(entry.nodeTxId, "hex"),
              value: Buffer.from(entry.l2TransactionSourceCbor, "hex"),
            })),
          );
          await submitExecutionNativeScriptInvalidStep01Accepted({
            ...common,
            blueprint: workflow.binding.blueprint as Parameters<
              typeof submitExecutionNativeScriptInvalidStep01Accepted
            >[0]["blueprint"],
            network: workflow.binding.network,
            stateQueueBlockOutRef: activeStage.stateQueueBlockOutRef,
            txInclusion: parseSubmitStep01TxInclusion({
              nativeTxId: transactionEntry.nodeTxId,
              nativeTx: nativeTxFromCoreCompact(material.compact),
              nativeTxCompactCbor:
                material.proofSource.compactCbor.toString("hex"),
              l2TransactionSourceCbor: transactionEntry.l2TransactionSourceCbor,
              transactionsPhasRoot: trie.root,
              txMembershipProofCbor: requireProof(
                trie,
                Buffer.from(transactionEntry.nodeTxId, "hex"),
                "executionNativeScriptInvalid transaction",
              ),
            }),
            header: prepared.block.header,
            executionIndex: BigInt(prepared.detection.executionIndex),
            referenceScriptUtxo: workflow.references.steps[0],
            witnessReferenceScripts: workflow.references.witnesses,
          });
        } else {
          const eventKey = {
            ForcedTransactionEventKey: { tx_order_id: forcedEntry!.key },
          } as const;
          await submitExecutionNativeScriptInvalidStep01Forced({
            ...common,
            header: prepared.block.header,
            membership: await buildForcedTransactionLeafMembershipProof({
              reconstruction: prepared.block.reconstruction,
              eventKey,
            }),
            executionIndex: BigInt(prepared.detection.executionIndex),
            referenceScriptUtxo: workflow.references.steps[0],
          });
        }
      } else if (activeStage.step === 2) {
        if (
          forcedEntry === undefined ||
          forcedEntry.value.verdict === "ForcedTxValid"
        )
          throw new Error("executionNativeScriptInvalid forced source changed");
        const eventKey = {
          ForcedTransactionEventKey: { tx_order_id: forcedEntry.key },
        } as const;
        const authentication =
          await buildExecutionSourceMachineAuthenticationFromRetainedDa({
            eventKey,
            executionIndex: prepared.detection.executionIndex,
            authenticatedValidationTraceEntries:
              prepared.block.reconstruction.payload.block_body.validation_traces.map(
                ([key, value]) => ({
                  key: Buffer.from(key, "hex"),
                  value: Buffer.from(value, "hex"),
                }),
              ),
            retainedValidationWitnessEntries:
              prepared.block.reconstruction.payload.block_body.validation_trace_witnesses.map(
                ([key, value]) => ({
                  key: Buffer.from(key, "hex"),
                  value: Buffer.from(value, "hex"),
                }),
              ),
            expectedValidationTracesRoot:
              prepared.block.header.validationTracesRoot,
            expectedPurposeKind: purpose.purposeKindTag,
          });
        const evidence = prepareExecutionNativeScriptInvalidEvidence({
          finding: {
            subject: forcedVerdictSubject({
              transactionId: forcedEntry.value.tx_id,
              sourceKey: forcedEntry.key,
              rejectionReason: forcedEntry.value.verdict.ForcedTxInvalid.reason,
            }),
            executionIndex: prepared.detection.executionIndex,
          },
          transactionIdHex: forcedEntry.value.tx_id,
          sourceDescriptorHashHex: (purpose.source.originKind === 0
            ? hashMidgardInlineScriptSourceLeaf({
                sourceIndex: BigInt(purpose.source.sourceIndex),
                scriptLanguageTag: purpose.source.languageTag,
                scriptHash: Buffer.from(purpose.source.scriptHash, "hex"),
                scriptTotalLength: purpose.source.totalLength,
                itemCommitment: Buffer.from(
                  purpose.source.itemCommitment,
                  "hex",
                ),
              })
            : hashMidgardReferenceScriptSourceLeaf({
                sourceKey: Buffer.from(purpose.source.sourceKey, "hex"),
                scriptLanguageTag: purpose.source.languageTag,
                scriptHash: Buffer.from(purpose.source.scriptHash, "hex"),
                scriptTotalLength: purpose.source.totalLength,
                itemCommitment: Buffer.from(
                  purpose.source.itemCommitment,
                  "hex",
                ),
              })
          ).toString("hex"),
          scriptItemHashHex: computeHash32(
            decodeMidgardVersionedScript(
              Buffer.from(purpose.source.versionedItemCbor, "hex"),
            ).scriptBytes,
          ).toString("hex"),
          scriptBytes: decodeMidgardVersionedScript(
            Buffer.from(purpose.source.versionedItemCbor, "hex"),
          ).scriptBytes,
          addressWitnessItems,
          validityIntervalStart: tx.body.validityIntervalStart,
          validityIntervalEnd: tx.body.validityIntervalEnd,
        });
        await submitExecutionNativeScriptInvalidStep02({
          ...common,
          evidence,
          authentication: authentication.authentication,
          referenceScriptUtxo: workflow.references.steps[1],
        });
      } else if (activeStage.step === 3) {
        await submitExecutionNativeScriptInvalidStep03({
          ...common,
          scriptItemCbor: Buffer.from(purpose.source.versionedItemCbor, "hex"),
          referenceScriptUtxo: workflow.references.steps[2],
        });
      } else if (activeStage.step === 4) {
        await submitExecutionNativeScriptInvalidStep04({
          ...common,
          nativeTxCompactCbor: compactCbor,
          witnessSet,
          scriptItemCbor: Buffer.from(purpose.source.versionedItemCbor, "hex"),
          addressWitnessItems,
          referenceScriptUtxo: workflow.references.steps[3],
          witnessReferenceScripts: workflow.references.witnesses,
        });
      } else if (activeStage.step === 5) {
        await submitExecutionNativeScriptInvalidStep05({
          ...common,
          nativeTxCompactCbor: compactCbor,
          witnessSet,
          addressWitnessItems,
          referenceScriptUtxo: workflow.references.steps[4],
        });
      } else if (activeStage.step === 6) {
        await submitExecutionNativeScriptInvalidStep06({
          ...common,
          scriptItemCbor: Buffer.from(purpose.source.versionedItemCbor, "hex"),
          addressWitnessItems,
          referenceScriptUtxo: workflow.references.steps[5],
          witnessReferenceScripts: workflow.references.witnesses,
        });
      } else if (activeStage.step === 7) {
        await submitExecutionNativeScriptInvalidAcceptedInit({
          ...common,
          referenceScriptUtxo: workflow.references.steps[6],
        });
      } else {
        if (transactionEntry === undefined)
          throw new Error(
            "executionNativeScriptInvalid forced direction entered accepted reconstruction",
          );
        const accepted = workflow.contracts.acceptedPrelude;
        if (accepted === undefined || accepted.length !== 7)
          throw new Error(
            "executionNativeScriptInvalid accepted chain disappeared",
          );
        const acceptedStepIndex = activeStage.step - 7;
        const { threadUtxo } = await requireLinearFaultThreadUtxo({
          lucid: workflow.lucid,
          contracts: { ...workflow.contracts, steps: accepted },
          categoryId: workflow.binding.definition.categoryId,
          family: "execution-native-script-invalid",
          stepIndex: acceptedStepIndex,
          threadOutRef: activeStage.threadOutRef,
        });
        const state = requireLinearFaultStepState<AcceptedReconstructionState>({
          threadUtxo,
          signer: workflow.signer,
          schema: ExecutionNativeScriptInvalidAcceptedDatumSchema as never,
          family: "execution-native-script-invalid",
          stepIndex: acceptedStepIndex,
        });
        const membership = async (key: Buffer, output: Buffer) => {
          const entries = (predecessor?.utxos ?? []).map(
            ({ key: candidate, value }) => ({
              key: Buffer.from(candidate),
              value: buildCanonicalMidgardLedgerOutputMaterial({
                outputIndex: decodeMidgardSpendInputItem(candidate).outputIndex,
                outputCbor: value,
              }).descriptorCbor,
            }),
          );
          const trie = await buildTrieView(entries);
          const descriptor = buildCanonicalMidgardLedgerOutputMaterial({
            outputIndex: decodeMidgardSpendInputItem(key).outputIndex,
            outputCbor: output,
          }).descriptorCbor;
          const proofCbor = requireProof(
            trie,
            key,
            "executionNativeScriptInvalid prior ledger",
          );
          return {
            descriptorCbor: descriptor.toString("hex"),
            membershipProof: Data.from(proofCbor, Proof),
            membershipProofCbor: proofCbor,
          };
        };
        if (activeStage.step === 8) {
          const items = decodeMidgardFieldPreimage(
            tx.body.spendInputsPreimageCbor,
          );
          const item = items[Number(state.field_cursor)];
          if (item === undefined) {
            await submitExecutionNativeScriptInvalidAcceptedFinishSpends({
              ...common,
              nativeTxCompactCbor: compactCbor,
              spendInputsPreimageCbor:
                tx.body.spendInputsPreimageCbor.toString("hex"),
              referenceScriptUtxo: workflow.references.steps[7],
            });
          } else {
            const output = priorOutputs.get(item.toString("hex"));
            if (output === undefined)
              throw new Error(
                "executionNativeScriptInvalid spend output disappeared",
              );
            await submitExecutionNativeScriptInvalidAcceptedSpend({
              ...common,
              network: workflow.binding.network,
              nativeTxCompactCbor: compactCbor,
              spendInputsPreimageCbor:
                tx.body.spendInputsPreimageCbor.toString("hex"),
              ...(await membership(Buffer.from(item), output)),
              membershipReferenceScriptUtxo:
                workflow.references.witnesses.phasMembershipWithdraw,
              referenceScriptUtxo: workflow.references.steps[7],
            });
          }
        } else if (activeStage.step === 9) {
          const items = decodeMidgardFieldPreimage(tx.body.mintPreimageCbor);
          await (items[Number(state.field_cursor)] === undefined
            ? submitExecutionNativeScriptInvalidAcceptedFinishPurpose({
                ...common,
                phase: "mint",
                nativeTxCompactCbor: compactCbor,
                fieldPreimageCbor: tx.body.mintPreimageCbor.toString("hex"),
                referenceScriptUtxo: workflow.references.steps[8],
              })
            : submitExecutionNativeScriptInvalidAcceptedMint({
                ...common,
                nativeTxCompactCbor: compactCbor,
                mintPreimageCbor: tx.body.mintPreimageCbor.toString("hex"),
                referenceScriptUtxo: workflow.references.steps[8],
              }));
        } else if (activeStage.step === 10) {
          const items = decodeMidgardFieldPreimage(
            tx.body.requiredObserversPreimageCbor,
          );
          await (items[Number(state.field_cursor)] === undefined
            ? submitExecutionNativeScriptInvalidAcceptedFinishPurpose({
                ...common,
                phase: "observer",
                nativeTxCompactCbor: compactCbor,
                fieldPreimageCbor:
                  tx.body.requiredObserversPreimageCbor.toString("hex"),
                referenceScriptUtxo: workflow.references.steps[9],
              })
            : submitExecutionNativeScriptInvalidAcceptedObserver({
                ...common,
                nativeTxCompactCbor: compactCbor,
                observersPreimageCbor:
                  tx.body.requiredObserversPreimageCbor.toString("hex"),
                referenceScriptUtxo: workflow.references.steps[9],
              }));
        } else if (activeStage.step === 11) {
          const items = decodeMidgardFieldPreimage(tx.body.outputsPreimageCbor);
          await (items[Number(state.field_cursor)] === undefined
            ? submitExecutionNativeScriptInvalidAcceptedFinishReceivePass({
                ...common,
                nativeTxCompactCbor: compactCbor,
                outputsPreimageCbor:
                  tx.body.outputsPreimageCbor.toString("hex"),
                referenceScriptUtxo: workflow.references.steps[10],
              })
            : submitExecutionNativeScriptInvalidAcceptedReceive({
                ...common,
                nativeTxCompactCbor: compactCbor,
                outputsPreimageCbor:
                  tx.body.outputsPreimageCbor.toString("hex"),
                referenceScriptUtxo: workflow.references.steps[10],
              }));
        } else if (activeStage.step === 12) {
          const items = decodeMidgardFieldPreimage(
            tx.witnessSet.scriptTxWitsPreimageCbor,
          );
          await (items[Number(state.field_cursor)] === undefined
            ? submitExecutionNativeScriptInvalidAcceptedFinishInline({
                ...common,
                nativeTxCompactCbor: compactCbor,
                witnessSet,
                scriptsPreimageCbor:
                  tx.witnessSet.scriptTxWitsPreimageCbor.toString("hex"),
                referenceScriptUtxo: workflow.references.steps[11],
              })
            : submitExecutionNativeScriptInvalidAcceptedInlineSource({
                ...common,
                nativeTxCompactCbor: compactCbor,
                witnessSet,
                scriptsPreimageCbor:
                  tx.witnessSet.scriptTxWitsPreimageCbor.toString("hex"),
                referenceScriptUtxo: workflow.references.steps[11],
              }));
        } else if (activeStage.step === 13) {
          const items = decodeMidgardFieldPreimage(
            tx.body.referenceInputsPreimageCbor,
          );
          const item = items[Number(state.field_cursor)];
          if (item === undefined)
            throw new Error(
              "executionNativeScriptInvalid reference source exhausted",
            );
          const output = priorOutputs.get(item.toString("hex"));
          if (output === undefined)
            throw new Error(
              "executionNativeScriptInvalid reference output disappeared",
            );
          await submitExecutionNativeScriptInvalidAcceptedReferenceSource({
            ...common,
            network: workflow.binding.network,
            nativeTxCompactCbor: compactCbor,
            referenceInputsPreimageCbor:
              tx.body.referenceInputsPreimageCbor.toString("hex"),
            ...(await membership(Buffer.from(item), output)),
            membershipReferenceScriptUtxo:
              workflow.references.witnesses.phasMembershipWithdraw,
            referenceScriptUtxo: workflow.references.steps[12],
          });
        } else {
          throw new Error(
            "executionNativeScriptInvalid impossible accepted stage",
          );
        }
      }
    },
  );
  return { transaction };
};
