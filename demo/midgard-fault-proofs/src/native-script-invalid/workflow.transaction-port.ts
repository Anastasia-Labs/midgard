import { decodeMidgardAddressWitnessItem } from "@al-ft/midgard-core";
import { type LucidEvolution } from "@lucid-evolution/lucid";

import { resolvePublishedProofChunks } from "../publish-proof-chunks.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
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
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import {
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowTerminalVerifier,
} from "../workflow/orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import { submitNativeScriptInvalidInit } from "./submit-init.js";
import { submitNativeScriptInvalidStep01 } from "./submit-step-01.js";
import { submitNativeScriptInvalidStep01Forced } from "./submit-step-01-forced.js";
import { submitNativeScriptInvalidStep02 } from "./submit-step-02.js";
import { submitNativeScriptInvalidStep03 } from "./submit-step-03.js";
import { submitNativeScriptInvalidStep03StartSignerScan } from "./submit-step-03-staged.js";
import { submitNativeScriptInvalidStep04 } from "./submit-step-04.js";
import { submitNativeScriptInvalidStep05 } from "./submit-step-05.js";
import {
  type BoundConfig,
  buffers,
  isDirect,
  type NativeScriptInvalidWorkflowReferenceScripts,
  resolveField,
  scriptFieldPlan,
  signerFieldPlan,
  witnessSet,
} from "./workflow.resolve-field.js";
import {
  admitNativeScriptInvalidWorkflowArtifact,
  prepareNativeScriptInvalidWorkflowArtifact,
} from "./workflow-artifact.js";

export const transactionPort = (
  config: BoundConfig,
): CursorFamilyTransactionPort<"nativeScriptInvalid"> => ({
  portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
  category: "nativeScriptInvalid",
  prepare: async ({ evidence, classification }) =>
    await prepareNativeScriptInvalidWorkflowArtifact({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = await admitNativeScriptInvalidWorkflowArtifact(artifact);
    if (admitted.artifact.headerHash !== config.binding.definition.headerHash) {
      throw new Error(
        "native-script-invalid artifact changed the bound header",
      );
    }
    const input = cursorFamilyActionInput({
      category: "nativeScriptInvalid",
      action,
    });
    const stage = input.stage;
    const threadOutRef = () => cursorStringField(input, "threadOutRef");
    const categoryId = config.binding.resolvedContracts.category.categoryId;
    const common = {
      lucid: config.lucid,
      contracts: config.contracts,
      categoryId,
      signer: config.signer,
      witnessSet: witnessSet(admitted),
      nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
      awaitConfirmation: false,
    } as const;
    if (stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitNativeScriptInvalidInit({
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
    if (stage === "step_01" && admitted.forced !== undefined) {
      const forced = admitted.forced;
      return {
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitNativeScriptInvalidStep01Forced({
              ...common,
              threadOutRef: threadOutRef(),
              state: forced.evidence.state,
              forcedSource: forced.forcedSource,
              referenceScriptUtxo: config.references.steps[0],
              preSubmitBoundary,
            });
          },
        ),
      };
    }
    if (stage === "step_01") {
      const txInclusion = admitted.prepared.txInclusion;
      if (txInclusion === undefined)
        throw new Error("native-script-invalid: accepted inclusion is absent");
      const chunks = await resolvePublishedProofChunks({
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: txInclusion.txMembershipProofCbor,
      });
      if (chunks === undefined) {
        throw new Error("native-script-invalid transaction proof disappeared");
      }
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitNativeScriptInvalidStep01({
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
              txInclusion: parseSubmitStep01TxInclusion(txInclusion),
              referenceScriptUtxo: config.references.steps[0],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (stage === "step_02") {
      const carriage = await resolveField({
        config,
        plan: scriptFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitNativeScriptInvalidStep02({
              ...common,
              threadOutRef: threadOutRef(),
              scriptWitnessItems: buffers(
                admitted.prepared.scriptWitnessItemCbors,
              ),
              scriptIndex: admitted.prepared.scriptIndex,
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : { certificateUtxo: carriage.certificate }),
              referenceScriptUtxo: config.references.steps[1],
              preSubmitBoundary,
            });
          },
        ),
      });
    }
    if (stage === "step_03") {
      const carriage = await resolveField({
        config,
        plan: signerFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      const direct = isDirect(admitted);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            const args = {
              ...common,
              threadOutRef: threadOutRef(),
              scriptItemCbor: Buffer.from(
                admitted.prepared.scriptItemCbor,
                "hex",
              ),
              addressWitnessItems: buffers(
                admitted.prepared.addrWitnessItemCbors,
              ),
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : { certificateUtxo: carriage.certificate }),
              referenceScriptUtxo: config.references.steps[2],
              preSubmitBoundary,
            } as const;
            if (direct) {
              await submitNativeScriptInvalidStep03({
                ...args,
                addressWitnessVerificationKeys:
                  admitted.prepared.addrWitnessItemCbors.map(
                    (item) =>
                      decodeMidgardAddressWitnessItem(Buffer.from(item, "hex"))
                        .verificationKey,
                  ),
                witnessReferenceScripts: config.references.witnesses,
              });
            } else {
              await submitNativeScriptInvalidStep03StartSignerScan(args);
            }
          },
        ),
      });
    }
    if (stage === "step_04") {
      const carriage = await resolveField({
        config,
        plan: signerFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitNativeScriptInvalidStep04({
              ...common,
              threadOutRef: threadOutRef(),
              addressWitnessItems: buffers(
                admitted.prepared.addrWitnessItemCbors,
              ),
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : { certificateUtxo: carriage.certificate }),
              referenceScriptUtxo: config.references.steps[3],
              preSubmitBoundary,
            });
          },
        ),
      });
    }
    if (stage === "step_05") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitNativeScriptInvalidStep05({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              scriptItemCbor: Buffer.from(
                admitted.prepared.scriptItemCbor,
                "hex",
              ),
              addressWitnessItems: buffers(
                admitted.prepared.addrWitnessItemCbors,
              ),
              referenceScriptUtxo: config.references.steps[4],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (stage === "remove") {
      return await captureCursorRemoval({
        category: "nativeScriptInvalid",
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
    throw new Error(`native-script-invalid unsupported stage ${stage}`);
  },
});

export type ManifestBoundNativeScriptInvalidWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: NativeScriptInvalidWorkflowReferenceScripts;
  l1Source: FraudProofL1Source;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundNativeScriptInvalidWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"nativeScriptInvalid">;
  l1: FraudProofFamilyL1ObservationPort<"nativeScriptInvalid">;
  transactions: CursorFamilyTransactionPort<"nativeScriptInvalid">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;
