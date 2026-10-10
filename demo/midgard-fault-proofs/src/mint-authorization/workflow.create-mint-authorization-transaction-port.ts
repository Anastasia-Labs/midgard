import { encodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import { MintAuthorizationStep04Datum } from "@al-ft/midgard-sdk";
import { type MintAuthorizationStep04State, Proof } from "@al-ft/midgard-sdk";
import { type LucidEvolution } from "@lucid-evolution/lucid";
import { Data } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import { parseSubmitStep01TxInclusion } from "../step-support.js";
import { submitInit } from "../submit-init.js";
import { keyValuePhasMembershipProofs } from "../transition-trace/phas.js";
import type { CompleteCanonicalReplayContext } from "../workflow/complete-replay.js";
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
import { type FieldCarriagePrerequisitePort } from "../workflow/field-carriage-prerequisite.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import {
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowTerminalVerifier,
} from "../workflow/orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import {
  admitMintAuthorizationWorkflowArtifact,
  prepareMintAuthorizationWorkflowArtifact,
} from "./artifact.js";
import {
  requireMintAuthorizationStepState,
  requireMintAuthorizationThreadUtxo,
} from "./submit-common.js";
import { submitMintAuthorizationEvaluate } from "./submit-mint-authorization-evaluate.js";
import { submitMintAuthorizationStep01 } from "./submit-mint-authorization-step-01.js";
import { submitMintAuthorizationStep02 } from "./submit-mint-authorization-step-02.js";
import {
  submitMintAuthorizationStep03EvaluateUnsatisfied,
  submitMintAuthorizationStep03WitnessAbsence,
} from "./submit-mint-authorization-step-03.js";
import {
  submitMintAuthorizationStep04AdvanceComplete,
  submitMintAuthorizationStep04ResolveNext,
} from "./submit-mint-authorization-step-04.js";
import { submitMintAuthorizationStep05 } from "./submit-mint-authorization-step-05.js";
import { submitMintAuthorizationWitnessScan } from "./submit-mint-authorization-witness-scan.js";
import {
  type BoundConfig,
  mintAuthorizationWorkflowRawRequirement,
  type MintAuthorizationWorkflowReferenceScripts,
  planMintAuthorizationWorkflowField,
  resolveField,
  witnessSet,
} from "./workflow.mint-authorization-workflow-raw-requirement.js";

export const createMintAuthorizationTransactionPort = (
  config: BoundConfig,
  rawPrerequisite: FieldCarriagePrerequisitePort<"mintAuthorization">,
): CursorFamilyTransactionPort<"mintAuthorization"> => ({
  portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
  category: "mintAuthorization",
  prepare: async ({ evidence, classification }) =>
    await prepareMintAuthorizationWorkflowArtifact({
      evidence,
      classification,
      replayContext: config.replayContext,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = await admitMintAuthorizationWorkflowArtifact(artifact);
    if (admitted.current.headerHash !== config.binding.definition.headerHash)
      throw new Error("mint authorization: bound header changed");
    const input = cursorFamilyActionInput({
      category: "mintAuthorization",
      action,
    });
    const stage = input.stage;
    const categoryId = config.binding.resolvedContracts.category.categoryId;
    if (stage === "remove")
      return await captureCursorRemoval({
        category: "mintAuthorization",
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        deploymentInfo: config.binding.deploymentInfo,
        network: config.binding.network,
        signer: config.signer,
        headerHash: admitted.current.headerHash,
        input,
        stateQueueMutationLeaseCoordinator:
          config.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      });
    const planned = planMintAuthorizationWorkflowField(
      admitted,
      config.signer.paymentKeyHash,
      stage,
    );
    const carriage =
      planned === null ? null : await resolveField({ config, plan: planned });
    const rawPreimageUtxos =
      (await mintAuthorizationWorkflowRawRequirement(admitted, stage)) !== null
        ? (
            await rawPrerequisite.resolveAuthenticated({
              headerHash: admitted.current.headerHash,
              action,
              artifact,
            })
          ).publications
        : undefined;
    const common = {
      lucid: config.lucid,
      contracts: config.contracts,
      categoryId,
      signer: config.signer,
      network: config.binding.network,
      blueprint: config.binding.blueprint,
      nativeTxCompactCbor: admitted.nativeTxCompactCbor,
      witnessSet: witnessSet(admitted),
      publishedCarriageUtxos: carriage?.publications,
      certificateUtxo: carriage?.certificate,
      awaitConfirmation: false,
    } as const;
    return {
      transaction: await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          if (stage === "init") {
            await submitInit({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              fraudCategory: "mintAuthorization",
              fraudulentBlockOutRef: cursorStringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: admitted.current.headerHash,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
            return;
          }
          const threadOutRef = cursorStringField(input, "threadOutRef");
          if (stage === "step_01") {
            await submitMintAuthorizationStep01({
              ...common,
              threadOutRef,
              stateQueueBlockOutRef: cursorStringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: parseSubmitStep01TxInclusion(admitted.txInclusion),
              referenceScriptUtxo: config.references.steps[0],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
            });
          } else if (stage === "step_02") {
            await submitMintAuthorizationStep02({
              ...common,
              threadOutRef,
              reconstruction: admitted.current,
              evidenceReferences: rawPreimageUtxos,
              policyIndex: admitted.finding.policyIndex,
              direction: admitted.finding.direction,
              mintItemCbors: admitted.mintItemCbors,
              referenceScriptUtxo: config.references.steps[1],
              preSubmitBoundary,
            });
          } else if (stage === "step_03") {
            if (admitted.finding.direction === 0n)
              await submitMintAuthorizationStep03WitnessAbsence({
                ...common,
                threadOutRef,
                scriptTxWitsItemCbors: admitted.scriptWitnessItemCbors,
                rawPreimageUtxos,
                referenceScriptUtxo: config.references.steps[2],
                preSubmitBoundary,
              });
            else {
              if (admitted.finding.scriptBytesHex === null)
                throw new Error("mint authorization: native policy absent");
              await submitMintAuthorizationStep03EvaluateUnsatisfied({
                ...common,
                threadOutRef,
                scriptBytesHex: admitted.finding.scriptBytesHex,
                rawPreimageUtxos,
                addrTxWitsItemCbors: admitted.addrWitnessItemCbors,
                referenceScriptUtxo: config.references.steps[2],
                preSubmitBoundary,
              });
            }
          } else if (stage === "step_04") {
            const { threadUtxo } = await requireMintAuthorizationThreadUtxo({
              ...common,
              stepIndex: 3,
              threadOutRef,
            });
            const state =
              requireMintAuthorizationStepState<MintAuthorizationStep04State>({
                threadUtxo,
                signer: config.signer,
                schema: MintAuthorizationStep04Datum,
                stepIndex: 3,
              });
            const reference = admitted.references[Number(state.ref_cursor)];
            const args = {
              ...common,
              threadOutRef,
              referenceInputsItemCbors: admitted.referenceInputItemCbors,
              referenceScriptUtxo: config.references.steps[3],
              preSubmitBoundary,
            };
            if (reference === undefined)
              await submitMintAuthorizationStep04AdvanceComplete(args);
            else
              await submitMintAuthorizationStep04ResolveNext({
                ...args,
                descriptorCborHex: reference.descriptorCbor,
                trie: {
                  rootHex: state.prior_ledger_root,
                  prove: async (key) => {
                    if (key.toString("hex") !== reference.keyHex)
                      throw new Error(
                        "mint authorization: reference coordinate changed",
                      );
                    return Buffer.from(
                      Data.to(
                        (
                          await keyValuePhasMembershipProofs(
                            admitted.referenceLedger,
                            [
                              {
                                key,
                                value: Buffer.from(
                                  reference.descriptorCbor,
                                  "hex",
                                ),
                              },
                            ],
                          )
                        )[0]!,
                        Proof,
                      ),
                      "hex",
                    );
                  },
                },
              });
          } else if (stage === "step_05")
            await submitMintAuthorizationStep05({
              ...common,
              threadOutRef,
              referenceScriptUtxo: config.references.steps[4],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
            });
          else if (stage === "step_06") {
            if (
              admitted.finding.scriptBytesHex === null ||
              rawPreimageUtxos === undefined
            )
              throw new Error(
                "mint authorization native evaluator omitted retained policy",
              );
            await submitMintAuthorizationEvaluate({
              ...common,
              threadOutRef,
              scriptBytesHex: admitted.finding.scriptBytesHex,
              addrWitnessItemCbors: admitted.addrWitnessItemCbors,
              rawPreimageUtxos,
              referenceScriptUtxo: config.references.steps[5],
              preSubmitBoundary,
            });
          } else if (stage === "step_07") {
            if (rawPreimageUtxos === undefined)
              throw new Error("mint witness scan omitted retained field");
            await submitMintAuthorizationWitnessScan({
              ...common,
              threadOutRef,
              fieldBytesHex: encodeMidgardFieldPreimage(
                admitted.scriptWitnessItemCbors.map((item) =>
                  Buffer.from(item, "hex"),
                ),
              ).toString("hex"),
              rawPreimageUtxos,
              referenceScriptUtxo: config.references.steps[6],
              preSubmitBoundary,
            });
          } else
            throw new Error(`mint authorization: unsupported stage ${stage}`);
        },
      ),
    };
  },
});

export type ManifestBoundMintAuthorizationWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: MintAuthorizationWorkflowReferenceScripts;
  l1Source: FraudProofL1Source;
  replayContext?: CompleteCanonicalReplayContext;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundMintAuthorizationWorkflow = Readonly<{
  replayContext?: CompleteCanonicalReplayContext;
  binding: FraudProofWorkflowDeploymentBinding<"mintAuthorization">;
  l1: FraudProofFamilyL1ObservationPort<"mintAuthorization">;
  transactions: CursorFamilyTransactionPort<"mintAuthorization">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;
