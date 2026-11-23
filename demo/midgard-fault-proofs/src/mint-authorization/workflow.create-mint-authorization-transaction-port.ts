import { encodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import { Proof } from "@al-ft/midgard-sdk/core";
import {
  MIDGARD_FIELD_INDEX,
  MintAuthorizationStep04Datum,
  type MintAuthorizationStep04State,
} from "@al-ft/midgard-sdk/fraud-proof";
import { Data, type LucidEvolution } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import { parseSubmitStep01TxInclusion } from "../step-support.js";
import { buildInit } from "../submit-init.submit-init.js";
import type { CompleteCanonicalReplayContext } from "../workflow/complete-replay.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.admit-reference-scripts.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import type { ManifestBoundFamilyWorkflow } from "../workflow/family-definition.js";
import { type FieldCarriagePrerequisitePort } from "../workflow/field-carriage-prerequisite.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import { proofMaterialsAccess } from "../workflow/proof-materials-view.js";
import { workflowStepTransaction } from "../workflow/step-transaction.js";
import {
  admitMintAuthorizationWorkflowArtifact,
  prepareMintAuthorizationWorkflowArtifact,
} from "./artifact.js";
import { mintAuthorizationInputProof } from "./replay.js";
import {
  requireMintAuthorizationStepState,
  requireMintAuthorizationThreadUtxo,
} from "./submit-common.js";
import { buildMintAuthorizationEvaluate } from "./submit-mint-authorization-evaluate.js";
import { buildMintAuthorizationStep01 } from "./submit-mint-authorization-step-01.js";
import { buildMintAuthorizationStep02 } from "./submit-mint-authorization-step-02.submit-mint-authorization-step02.js";
import { buildMintAuthorizationStep03EvaluateUnsatisfied } from "./submit-mint-authorization-step-03.submit-mint-authorization-step03-evaluate-unsatisfied.js";
import { buildMintAuthorizationStep03WitnessAbsence } from "./submit-mint-authorization-step-03.submit-mint-authorization-step03-witness-absence.js";
import {
  buildMintAuthorizationStep04AdvanceComplete,
  buildMintAuthorizationStep04ResolveNext,
} from "./submit-mint-authorization-step-04.js";
import { buildMintAuthorizationStep05 } from "./submit-mint-authorization-step-05.js";
import { buildMintAuthorizationWitnessScan } from "./submit-mint-authorization-witness-scan.js";
import {
  type BoundConfig,
  mintAuthorizationStep04ScanField,
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
  capture: async ({ action, artifact, holder }) => {
    const materials = proofMaterialsAccess({
      signer: config.signer,
      lucid: config.lucid,
      holder,
    });
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
      input,
    );
    const carriage =
      planned === null
        ? null
        : await resolveField({ materials, config, plan: planned });
    const rawPreimageUtxos =
      (await mintAuthorizationWorkflowRawRequirement(admitted, stage)) !== null
        ? (
            await rawPrerequisite.resolveAuthenticated({
              headerHash: admitted.current.headerHash,
              holder,
              action,
              artifact,
            })
          ).publications
        : undefined;
    const common = {
      materials,
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
    } as const;
    return {
      transaction: (
        await workflowStepTransaction(
          await (async () => {
            if (stage === "init") {
              return await buildInit({
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
              });
            }
            const threadOutRef = cursorStringField(input, "threadOutRef");
            if (stage === "step_01") {
              return await buildMintAuthorizationStep01({
                ...common,
                threadOutRef,
                stateQueueBlockOutRef: cursorStringField(
                  input,
                  "stateQueueBlockOutRef",
                ),
                txInclusion: parseSubmitStep01TxInclusion(admitted.txInclusion),
                referenceScriptUtxo: config.references.steps[0],
                witnessReferenceScripts: config.references.witnesses,
              });
            } else if (stage === "step_02") {
              return await buildMintAuthorizationStep02({
                ...common,
                threadOutRef,
                reconstruction: admitted.current,
                evidenceReferences: rawPreimageUtxos,
                policyIndex: admitted.finding.policyIndex,
                direction: admitted.finding.direction,
                mintItemCbors: admitted.mintItemCbors,
                referenceScriptUtxo: config.references.steps[1],
              });
            } else if (stage === "step_03") {
              if (admitted.finding.direction === 0n)
                return await buildMintAuthorizationStep03WitnessAbsence({
                  ...common,
                  threadOutRef,
                  scriptTxWitsItemCbors: admitted.scriptWitnessItemCbors,
                  rawPreimageUtxos,
                  referenceScriptUtxo: config.references.steps[2],
                });
              else {
                if (admitted.finding.scriptBytesHex === null)
                  throw new Error("mint authorization: native policy absent");
                return await buildMintAuthorizationStep03EvaluateUnsatisfied({
                  ...common,
                  threadOutRef,
                  scriptBytesHex: admitted.finding.scriptBytesHex,
                  rawPreimageUtxos,
                  addrTxWitsItemCbors: admitted.addrWitnessItemCbors,
                  referenceScriptUtxo: config.references.steps[2],
                });
              }
            } else if (stage === "step_04") {
              const { threadUtxo } = await requireMintAuthorizationThreadUtxo({
                ...common,
                stepIndex: 3,
                threadOutRef,
              });
              const state =
                requireMintAuthorizationStepState<MintAuthorizationStep04State>(
                  {
                    threadUtxo,
                    signer: config.signer,
                    schema: MintAuthorizationStep04Datum,
                    stepIndex: 3,
                  },
                );
              if (
                mintAuthorizationStep04ScanField(input) !==
                Number(state.field_index)
              )
                throw new Error("mint authorization: step-04 field changed");
              const reference = (
                state.field_index ===
                BigInt(MIDGARD_FIELD_INDEX.referenceInputs)
                  ? admitted.references
                  : admitted.spends
              )[Number(state.cursor)];
              const args = {
                ...common,
                threadOutRef,
                referenceInputsItemCbors: admitted.referenceInputItemCbors,
                spendInputsItemCbors: admitted.spendInputItemCbors,
                referenceScriptUtxo: config.references.steps[3],
              };
              if (reference === undefined)
                return await buildMintAuthorizationStep04AdvanceComplete(args);
              else
                return await buildMintAuthorizationStep04ResolveNext({
                  ...args,
                  descriptorCborHex: reference.descriptorCbor,
                  trie: {
                    rootHex: state.prior_ledger_root,
                    prove: async (key) => {
                      if (key.toString("hex") !== reference.keyHex)
                        throw new Error(
                          "mint authorization: resolved input coordinate changed",
                        );
                      return Buffer.from(
                        Data.to(
                          await mintAuthorizationInputProof(
                            admitted,
                            reference,
                          ),
                          Proof,
                        ),
                        "hex",
                      );
                    },
                  },
                });
            } else if (stage === "step_05")
              return await buildMintAuthorizationStep05({
                ...common,
                threadOutRef,
                referenceScriptUtxo: config.references.steps[4],
                witnessReferenceScripts: config.references.witnesses,
              });
            else if (stage === "step_06") {
              if (
                admitted.finding.scriptBytesHex === null ||
                rawPreimageUtxos === undefined
              )
                throw new Error(
                  "mint authorization native evaluator omitted retained policy",
                );
              return await buildMintAuthorizationEvaluate({
                ...common,
                threadOutRef,
                scriptBytesHex: admitted.finding.scriptBytesHex,
                addrWitnessItemCbors: admitted.addrWitnessItemCbors,
                rawPreimageUtxos,
                referenceScriptUtxo: config.references.steps[5],
              });
            } else if (stage === "step_07") {
              if (rawPreimageUtxos === undefined)
                throw new Error("mint witness scan omitted retained field");
              return await buildMintAuthorizationWitnessScan({
                ...common,
                threadOutRef,
                fieldBytesHex: encodeMidgardFieldPreimage(
                  admitted.scriptWitnessItemCbors.map((item) =>
                    Buffer.from(item, "hex"),
                  ),
                ).toString("hex"),
                rawPreimageUtxos,
                referenceScriptUtxo: config.references.steps[6],
              });
            } else
              throw new Error(`mint authorization: unsupported stage ${stage}`);
          })(),
          `mint-authorization ${stage}`,
        )
      ).transaction,
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

export type ManifestBoundMintAuthorizationWorkflow =
  ManifestBoundFamilyWorkflow<"mintAuthorization", true, 7>;
