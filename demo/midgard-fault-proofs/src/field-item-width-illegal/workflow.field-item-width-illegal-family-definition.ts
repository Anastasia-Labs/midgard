import {
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxCompact } from "@al-ft/midgard-core/codec/forced";
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk/fraud-proof";

import { planFaultProofFieldOpening } from "../field-opening.plan-fault-proof-field-opening.js";
import { FIELD_ITEM_WIDTH_ILLEGAL_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.resolved-output-non-canonical-complete-canonical-replay.js";
import { CURSOR_FAMILY_TRANSACTION_PORT } from "../workflow/cursor-family-adapter.admit-reference-scripts.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import { assertManifestBoundWorkflowSigner } from "../workflow/deployment-manifest-binding.assert-deployment-info-matches-manifest.js";
import { bindFraudProofWorkflowDeployment } from "../workflow/deployment-manifest-binding.js";
import { CORE_WITNESS_ROLES } from "../workflow/family-definition.family-adapter-arm.js";
import {
  cursorStepContractNames,
  defineFamily,
} from "../workflow/family-definition.family-definition.js";
import { workflowFieldCarriageOwnOutputs } from "../workflow/field-carriage-prerequisite.own-carriage-claims.js";
import { proofMaterialsAccess } from "../workflow/proof-materials-view.js";
import { workflowStepTransaction } from "../workflow/step-transaction.js";
import { workflowTransactionInputOutRefs } from "../workflow/transaction-boundary.js";
import {
  FieldItemWidthStep02DatumSchema,
  FieldItemWidthStep03DatumSchema,
} from "./schemas.js";
import {
  createFieldItemWidthIllegalBoundConfig,
  createFieldItemWidthIllegalRawL1StageResolver,
  type LoadManifestBoundFieldItemWidthIllegalConfig,
  type ManifestBoundFieldItemWidthIllegalConfig,
} from "./workflow.create-field-item-width-illegal-raw-l1-stage-resolver.js";
import { createManifestBoundFieldItemWidthIllegalSubmission } from "./workflow.create-manifest-bound-field-item-width-illegal-submission.js";
import {
  type FieldItemWidthIllegalAssemblyRuntime,
  fieldItemWidthStageFromL1,
} from "./workflow.derive-field-item-width-illegal-authenticated-source.js";
import { FIELD_ITEM_WIDTH_ILLEGAL_CURSOR_SPEC } from "./workflow-spec.js";

export const loadManifestBoundFieldItemWidthIllegalConfig = async (
  input: LoadManifestBoundFieldItemWidthIllegalConfig,
): Promise<ManifestBoundFieldItemWidthIllegalConfig> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: input.manifest,
    blueprintJson: input.blueprintJson,
    deploymentInfo: input.deploymentInfo,
    category: "fieldItemWidthIllegal",
    headerHash: input.headerHash,
    proverCredential: input.signer.paymentKeyHash,
    stepDatumSchemas:
      FIELD_ITEM_WIDTH_ILLEGAL_FAMILY_DEFINITION.stepDatumSchemas,
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: input.signer.address,
    proofMaterialsAddress: input.signer.proofMaterialsAddress,
    paymentKeyHash: input.signer.paymentKeyHash,
  });

  return createFieldItemWidthIllegalBoundConfig(input, binding);
};

export const FIELD_ITEM_WIDTH_ILLEGAL_FAMILY_DEFINITION = defineFamily<
  "fieldItemWidthIllegal",
  "computationThreadMint" | "fraudProofMint" | "phasMembershipWithdraw",
  true,
  3,
  FieldItemWidthIllegalAssemblyRuntime
>({
  category: "fieldItemWidthIllegal",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    FieldItemWidthStep02DatumSchema,
    FieldItemWidthStep03DatumSchema,
  ],
  witnessRoles: CORE_WITNESS_ROLES,
  fieldPreimageCertificate: true,
  fieldGrammarCertificate: true,
  replayer: () => FIELD_ITEM_WIDTH_ILLEGAL_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: FIELD_ITEM_WIDTH_ILLEGAL_CURSOR_SPEC,
    stepContractNames: cursorStepContractNames(
      FIELD_ITEM_WIDTH_ILLEGAL_CURSOR_SPEC,
    ),
    transactionPort: (context) => {
      const category = "fieldItemWidthIllegal";
      const { config, material } = context.runtime;
      const { binding, l1, stateQueueMutationLeaseCoordinator } = context;
      return {
        portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
        category,
        prepare: material.prepare,
        validatePreparedArtifact: material.validatePreparedArtifact,
        capture: async ({ action, artifact, holder }) => {
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
          } as const;
          const familyAction = actions[input.stage as keyof typeof actions];
          if (familyAction === undefined)
            throw new Error(
              `${category} cursor action is outside its exact topology`,
            );
          const submission = createManifestBoundFieldItemWidthIllegalSubmission(
            {
              ownOutputs: workflowFieldCarriageOwnOutputs({
                category,
                materials: proofMaterialsAccess({
                  signer: config.signer,
                  lucid: config.lucid,
                  holder,
                }),
              }),
              config,
              observe: async () =>
                fieldItemWidthStageFromL1(
                  (
                    await l1.observe({
                      headerHash: binding.definition.headerHash,
                    })
                  ).stage,
                ),
              resolveStage: createFieldItemWidthIllegalRawL1StageResolver({
                config,
                l1,
                source: admitted.source,
              }),
            },
          );
          const { transaction } = await workflowStepTransaction(
            await submission.submit(familyAction, admitted.evidence),
            `${category} ${familyAction}`,
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
        const category = "fieldItemWidthIllegal";
        const { config, material } = context.runtime;
        const { binding } = context;
        if (action.input.stage !== "step_02") return null;
        const { evidence, source } = material.require(artifact);
        if (evidence.kind === "StaticDomain") return null;
        const witnessSet = decodeMidgardNativeTxWitnessSetCompact(
          Buffer.from(source.witnessSetCompactCbor, "hex"),
        );
        const compact = (
          evidence.subject.source_kind === 1n
            ? decodeMidgardForcedTxCompact
            : decodeMidgardNativeTxCompact
        )(Buffer.from(source.nativeTxCompactCbor, "hex"));
        const certificate = binding.fieldPreimageCertificate;
        if (certificate === null)
          throw new Error(`${category} omitted field certificate authority`);
        return {
          planned: planFaultProofFieldOpening({
            anchorSourceKind: evidence.subject.source_kind === 1n ? 1n : 0n,
            fieldIndex: Number(evidence.fieldIndex),
            anchorTxId: evidence.subject.transaction_id,
            nativeTxCompactCbor: source.nativeTxCompactCbor,
            fieldPreimage: Buffer.from(evidence.fieldPreimageHex, "hex"),
            witnessSet: {
              addr_tx_wits_hash: Buffer.from(
                witnessSet.addrTxWitsHash,
              ).toString("hex"),
              script_tx_wits_hash: Buffer.from(
                witnessSet.scriptTxWitsHash,
              ).toString("hex"),
              redeemer_tx_wits_hash: Buffer.from(
                witnessSet.redeemerTxWitsHash,
              ).toString("hex"),
            },
            ...(evidence.fieldIndex === 7n
              ? {
                  anchorWitnessSetHash: Buffer.from(
                    compact.transactionWitnessSetHash,
                  ).toString("hex"),
                }
              : {}),
            owner: config.signer.paymentKeyHash,
            publish: true,
            label: `${category} field opening`,
          }),
          compactCbor: source.nativeTxCompactCbor,
          witnessSetCompactCbor: source.witnessSetCompactCbor,
          certificate: {
            policyId: certificate.policyId,
            mintingScript: certificate.mintingScript,
            referenceScriptUtxo:
              config.referenceScripts.fieldPreimageCertificateMint,
          },
        };
      },
    },
  ],
});
