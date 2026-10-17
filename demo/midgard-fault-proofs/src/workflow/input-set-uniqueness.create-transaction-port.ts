import { InputSetUniquenessStep04DatumSchema } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { InputSetUniquenessContracts } from "../input-set-uniqueness/contracts.js";
import { submitInputSetUniquenessForcedStep01 } from "../input-set-uniqueness/submit-input-set-uniqueness-forced-step-01.js";
import { submitInputSetUniquenessInit } from "../input-set-uniqueness/submit-input-set-uniqueness-init.js";
import { submitInputSetUniquenessStep01 } from "../input-set-uniqueness/submit-input-set-uniqueness-step-01.js";
import { submitInputSetUniquenessStep02 } from "../input-set-uniqueness/submit-input-set-uniqueness-step-02.js";
import { submitInputSetUniquenessStep03 } from "../input-set-uniqueness/submit-input-set-uniqueness-step-03.js";
import {
  submitInputSetUniquenessStep04Advance,
  submitInputSetUniquenessStep04Finalize,
} from "../input-set-uniqueness/submit-input-set-uniqueness-step-04.js";
import { fetchUtxoByOutRef, parseOutRef } from "../runtime.js";
import {
  type ManifestBoundLinearFamilyWorkflow,
  type ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.js";
import { admitAnyInputSetUniquenessArtifact } from "./input-set-uniqueness.admit-input-set-uniqueness-forced-artifact.js";
import {
  actionInput,
  type AssemblyContext,
  type BoundConfig,
  captureRemoval,
  prepareInputSetUniquenessArtifact,
  resolveField,
  stringField,
  WITNESS_ROLES,
} from "./input-set-uniqueness.prepare-input-set-uniqueness-artifact.js";
import {
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import { resolveDirectFirstProofChunks } from "./proof-chunk-prerequisite.js";
import { captureLocallyEvaluatedTransaction } from "./transaction-boundary.js";

export const createTransactionPort = (
  config: BoundConfig,
): LinearFamilyTransactionPort<"inputSetUniqueness"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "inputSetUniqueness",
  prepare: async ({ evidence, classification }) =>
    await prepareInputSetUniquenessArtifact({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitAnyInputSetUniquenessArtifact(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error("input-set-uniqueness artifact changed header");
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitInputSetUniquenessInit({
              lucid: config.lucid,
              blueprint: config.blueprint,
              network: config.network,
              contracts: config.contracts,
              category: config.category,
              catalogue: config.catalogue,
              signer: config.signer,
              fraudulentBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: config.headerHash,
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_01") {
      if (admitted.sourceKind === "forced") {
        return Object.freeze({
          transaction: await captureLocallyEvaluatedTransaction(
            async (preSubmitBoundary) => {
              await submitInputSetUniquenessForcedStep01({
                lucid: config.lucid,
                contracts: config.contracts,
                categoryId: config.category.categoryId,
                signer: config.signer,
                threadOutRef: stringField(input, "threadOutRef"),
                header: admitted.forcedSource.header,
                membership: admitted.forcedSource.membership,
                referenceScriptUtxo: config.referenceScripts.steps[0],
                preSubmitBoundary,
                awaitConfirmation: false,
              });
            },
          ),
        });
      }
      const chunks = await resolveDirectFirstProofChunks({
        action,
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.artifact.tx.txMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitInputSetUniquenessStep01({
              lucid: config.lucid,
              blueprint: config.blueprint,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              network: config.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              stateQueueBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: admitted.inclusion,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.referenceScripts.steps[0],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_02") {
      if (admitted.sourceKind !== "accepted") {
        throw new Error(
          "input-set-uniqueness forced artifact cannot enter accepted step-02",
        );
      }
      const spend = await resolveField(config, admitted.spendPlan);
      const reference = await resolveField(config, admitted.referencePlan);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitInputSetUniquenessStep02({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              claim: admitted.claim,
              nativeTxCompactCbor: admitted.artifact.tx.nativeTxCompactCbor,
              spendInputItemCbors: admitted.artifact.spendInputItemCbors,
              referenceInputItemCbors:
                admitted.artifact.referenceInputItemCbors,
              publishedSpendCarriageUtxos: spend.publications,
              ...(spend.certificate === undefined
                ? {}
                : { spendCertificateUtxo: spend.certificate }),
              publishedReferenceCarriageUtxos: reference.publications,
              ...(reference.certificate === undefined
                ? {}
                : { referenceCertificateUtxo: reference.certificate }),
              publishMissingCarriage: false,
              referenceScriptUtxo: config.referenceScripts.steps[1],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_03") {
      if (admitted.sourceKind !== "forced") {
        throw new Error(
          "input-set-uniqueness accepted artifact cannot enter forced step-03",
        );
      }
      const spend = await resolveField(config, admitted.spendPlan);
      const reference = await resolveField(config, admitted.referencePlan);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitInputSetUniquenessStep03({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              nativeTxCompactCbor: admitted.artifact.nativeTxCompactCbor,
              spendInputItemCbors: admitted.artifact.spendInputItemCbors,
              referenceInputItemCbors:
                admitted.artifact.referenceInputItemCbors,
              publishedSpendCarriageUtxos: spend.publications,
              publishedReferenceCarriageUtxos: reference.publications,
              ...(spend.certificate === undefined
                ? {}
                : { spendCertificateUtxo: spend.certificate }),
              ...(reference.certificate === undefined
                ? {}
                : { referenceCertificateUtxo: reference.certificate }),
              referenceScriptUtxo: config.referenceScripts.steps[2],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_04") {
      if (admitted.sourceKind !== "forced") {
        throw new Error(
          "input-set-uniqueness accepted artifact cannot enter forced step-04",
        );
      }
      const threadOutRef = stringField(input, "threadOutRef");
      const thread = await fetchUtxoByOutRef({
        lucid: config.lucid,
        outRef: parseOutRef(threadOutRef, "input-set-uniqueness thread"),
        label: "input-set-uniqueness forced step-04 thread",
      });
      if (thread.datum == null) {
        throw new Error("input-set-uniqueness forced thread omitted datum");
      }
      const state = Data.from(
        thread.datum,
        InputSetUniquenessStep04DatumSchema as never,
      ) as {
        data: { cursor: bigint; spend_count: bigint; reference_count: bigint };
      };
      const total = state.data.spend_count + state.data.reference_count;
      if (state.data.cursor === total) {
        return Object.freeze({
          transaction: await captureLocallyEvaluatedTransaction(
            async (preSubmitBoundary) => {
              await submitInputSetUniquenessStep04Finalize({
                lucid: config.lucid,
                contracts: config.contracts,
                categoryId: config.category.categoryId,
                signer: config.signer,
                threadOutRef,
                spendInputItemCbors: admitted.artifact.spendInputItemCbors,
                referenceInputItemCbors:
                  admitted.artifact.referenceInputItemCbors,
                referenceScriptUtxo: config.referenceScripts.steps[3],
                witnessReferenceScripts: config.referenceScripts.witnesses,
                preSubmitBoundary,
                awaitConfirmation: false,
              });
            },
          ),
        });
      }
      const readingSpend = state.data.cursor < state.data.spend_count;
      const opening = await resolveField(
        config,
        readingSpend ? admitted.spendPlan : admitted.referencePlan,
      );
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitInputSetUniquenessStep04Advance({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef,
              nativeTxCompactCbor: admitted.artifact.nativeTxCompactCbor,
              spendInputItemCbors: admitted.artifact.spendInputItemCbors,
              referenceInputItemCbors:
                admitted.artifact.referenceInputItemCbors,
              publishedCarriageUtxos: opening.publications,
              ...(opening.certificate === undefined
                ? {}
                : { certificateUtxo: opening.certificate }),
              referenceScriptUtxo: config.referenceScripts.steps[3],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "remove") return await captureRemoval(config, input);
    throw new Error(
      `unsupported input-set-uniqueness stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundInputSetUniquenessWorkflowConfig =
  ManifestBoundLinearFamilyWorkflowConfig<
    "inputSetUniqueness",
    (typeof WITNESS_ROLES)[number],
    true
  >;

export type ManifestBoundInputSetUniquenessWorkflow =
  ManifestBoundLinearFamilyWorkflow<"inputSetUniqueness", true>;

export const contracts = (
  context: AssemblyContext,
): InputSetUniquenessContracts => {
  const resolved = context.binding.resolvedContracts;
  const chain = resolved.contracts.inputSetUniqueness;
  const stateQueuePolicyId = resolved.stateQueuePolicyId;
  if (
    stateQueuePolicyId === undefined ||
    chain === undefined ||
    chain.steps.length !== 4
  ) {
    throw new Error("input-set-uniqueness deployment chain is incomplete");
  }
  return {
    steps: [chain.steps[0]!, chain.steps[1]!, chain.steps[2]!, chain.steps[3]!],
    computationThread: resolved.contracts.computationThread,
    fraudProof: {
      policyId: resolved.contracts.fraudProof.policyId,
      mintingScript: resolved.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        resolved.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: context.certificate.policyId,
  };
};
