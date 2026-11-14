import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";

import { planFaultProofFieldOpening } from "../field-opening.js";
import { MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { CURSOR_FAMILY_TRANSACTION_PORT } from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import {
  CORE_WITNESS_ROLES,
  cursorStepContractNames,
  defineFamily,
} from "../workflow/family-definition.js";
import { workflowFieldCarriageOwnOutputs } from "../workflow/field-carriage-prerequisite.own-carriage-claims.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
} from "../workflow/transaction-boundary.js";
import {
  MintItemStep02DatumSchema,
  MintItemStep03DatumSchema,
  MintItemStep04DatumSchema,
} from "./schemas.js";
import { createManifestBoundMintItemNonCanonicalSubmission } from "./workflow.create-manifest-bound-mint-item-non-canonical-submission.js";
import {
  createMintItemNonCanonicalRawL1StageResolver,
  type MintItemNonCanonicalAssemblyRuntime,
  mintItemStageFromL1,
} from "./workflow.derive-mint-item-non-canonical-authenticated-source.js";
import { MINT_ITEM_NON_CANONICAL_CURSOR_SPEC } from "./workflow-spec.js";

/**
 * The step-02 envelope walk is the only step that reads field carriage; the
 * item scan reads the items its thread datum already holds.
 */
export const MINT_ITEM_NON_CANONICAL_FAMILY_DEFINITION = defineFamily<
  "mintItemNonCanonical",
  "computationThreadMint" | "fraudProofMint" | "phasMembershipWithdraw",
  true,
  4,
  MintItemNonCanonicalAssemblyRuntime
>({
  category: "mintItemNonCanonical",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    MintItemStep02DatumSchema,
    MintItemStep03DatumSchema,
    MintItemStep04DatumSchema,
  ],
  witnessRoles: CORE_WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: MINT_ITEM_NON_CANONICAL_CURSOR_SPEC,
    stepContractNames: cursorStepContractNames(
      MINT_ITEM_NON_CANONICAL_CURSOR_SPEC,
    ),
    transactionPort: (context) => {
      const category = "mintItemNonCanonical";
      const { config, material } = context.runtime;
      const { binding, l1, stateQueueMutationLeaseCoordinator } = context;
      return {
        portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
        category,
        prepare: material.prepare,
        validatePreparedArtifact: material.validatePreparedArtifact,
        capture: async ({ action, artifact, entries }) => {
          const input = cursorFamilyActionInput({ category, action });
          if (input.stage === "remove")
            return await captureCursorRemoval({
              category,
              lucid: config.lucid,
              blueprint: binding.blueprint,
              deploymentInfo: binding.deploymentInfo,
              network: binding.network,
              signer: config.signer,
              headerHash: binding.definition.headerHash,
              input,
              stateQueueMutationLeaseCoordinator,
              fraudProverRewardLovelace: BigInt(
                binding.releaseEconomics.policy.fraudProverRewardLovelace,
              ),
            });
          const admitted = material.require(artifact);
          const actions = {
            init: "submitInit",
            step_01: "submitStep01",
            step_02: "submitStep02",
            step_03: "submitStep03",
            step_04: "submitStep04",
          } as const;
          const familyAction = actions[input.stage as keyof typeof actions];
          if (familyAction === undefined)
            throw new Error(
              `${category} cursor action is outside its exact topology`,
            );
          const transaction = await captureLocallyEvaluatedTransaction(
            async (preSubmitBoundary) => {
              const submission =
                createManifestBoundMintItemNonCanonicalSubmission({
                  ownOutputs: workflowFieldCarriageOwnOutputs({
                    category,
                    entries,
                  }),
                  config,
                  preSubmitBoundary,
                  observe: async () =>
                    mintItemStageFromL1(
                      (
                        await l1.observe({
                          headerHash: binding.definition.headerHash,
                        })
                      ).stage,
                    ),
                  resolveStage: createMintItemNonCanonicalRawL1StageResolver({
                    config,
                    l1,
                    source: admitted.source,
                  }),
                });
              await submission.submit(familyAction, admitted.evidence);
            },
          );
          if (
            input.stage !== "init" &&
            !workflowTransactionInputOutRefs(transaction.signed).includes(
              cursorStringField(input, "threadOutRef"),
            )
          )
            throw new Error(
              `${category} captured transaction changed its authenticated thread input`,
            );
          return { transaction };
        },
      };
    },
  },
  fieldCarriage: [
    {
      requirementForAction: (context, { action, artifact }) => {
        if (action.input.stage !== "step_02") return null;
        const { config, material } = context.runtime;
        const { evidence, source } = material.require(artifact);
        return {
          planned: planFaultProofFieldOpening({
            anchorSourceKind: evidence.subject.source_kind === 1n ? 1n : 0n,
            fieldIndex: evidence.fieldIndex,
            anchorTxId: evidence.subject.transaction_id,
            nativeTxCompactCbor: source.nativeTxCompactCbor,
            itemCbors: decodeMidgardFieldPreimage(
              Buffer.from(evidence.fieldPreimageHex, "hex"),
            ),
            owner: config.signer.paymentKeyHash,
            publish: evidence.carriage === "RawUtxo",
            label: "mint-item-non-canonical field opening",
          }),
          compactCbor: source.nativeTxCompactCbor,
          witnessSetCompactCbor: source.witnessSetCompactCbor,
          certificate: {
            policyId: config.contracts.fieldPreimageCertificatePolicyId,
            mintingScript:
              config.contracts.fieldPreimageCertificateMintingScript,
            referenceScriptUtxo:
              config.referenceScripts.fieldPreimageCertificateMint,
          },
        };
      },
    },
  ],
});
