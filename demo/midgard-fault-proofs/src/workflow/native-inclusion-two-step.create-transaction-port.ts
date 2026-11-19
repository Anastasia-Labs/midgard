import {
  buildInvalidRangeStep01Forced,
  buildInvalidRangeStep02V1,
} from "../invalid-range/submit.js";
import { requireLinearFaultThreadUtxo } from "../linear-fault-family.js";
import { buildInit } from "../submit-init.js";
import { buildInvalidRangeStep01 } from "../submit-invalid-range-step-01.js";
import type { ZeroInputContracts } from "../zero-input/contracts.js";
import {
  buildZeroInputStep01Accepted,
  buildZeroInputStep01Forced,
} from "../zero-input/submit-step-01.js";
import { buildZeroInputStep02V1 } from "../zero-input/submit-step-02.js";
import {
  type ManifestBoundLinearFamilyWorkflow,
  type ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.js";
import { workflowFieldCarriageOwnOutputs } from "./field-carriage-prerequisite.own-carriage-claims.js";
import {
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import { admitNativeInclusionTwoStepArtifact } from "./native-inclusion-two-step.admit-native-inclusion-two-step-artifact.js";
import {
  actionInput,
  captureRemoval,
  stringField,
} from "./native-inclusion-two-step.capture-removal.js";
import { type NativeInclusionTwoStepCategory } from "./native-inclusion-two-step.parse-artifact.js";
import {
  type AssemblyContext,
  type BoundConfig,
  prepareNativeInclusionTwoStepArtifact,
  type WitnessRole,
} from "./native-inclusion-two-step.prepare-native-inclusion-two-step-artifact.js";
import { resolveDirectFirstProofChunks } from "./proof-chunk-prerequisite.js";
import { journaledStepTransaction } from "./step-transaction.js";

export const createTransactionPort = <
  Category extends NativeInclusionTwoStepCategory,
>(
  config: BoundConfig<Category>,
): LinearFamilyTransactionPort<Category> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: config.category,
  prepare: async ({ evidence, classification }) =>
    await prepareNativeInclusionTwoStepArtifact({
      category: config.category,
      evidence,
      classification,
    }),
  capture: async ({ action, artifact, entries }) => {
    const ownOutputs = workflowFieldCarriageOwnOutputs({
      category: config.category,
      entries,
    });
    const admitted = admitNativeInclusionTwoStepArtifact(artifact);
    if (
      admitted.artifact.category !== config.category ||
      admitted.artifact.headerHash !== config.headerHash
    ) {
      throw new Error(`${config.category} artifact changed workflow identity`);
    }
    const input = actionInput({ category: config.category, action });
    if (input.stage === "init") {
      return Object.freeze({
        transaction: (
          await journaledStepTransaction(
            await buildInit({
              lucid: config.lucid,
              blueprint: config.blueprint,
              deploymentInfo: config.deploymentInfo,
              network: config.network,
              signer: config.signer,
              fraudCategory: config.category,
              fraudulentBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: config.headerHash,
              witnessReferenceScripts: config.referenceScripts.witnesses,
            }),
            "native-inclusion-two-step buildInit",
          )
        ).transaction,
      });
    }
    if (input.stage === "step_01") {
      const chunks =
        admitted.artifact.sourceKind === "accepted"
          ? await resolveDirectFirstProofChunks({
              action,
              lucid: config.lucid,
              address: config.signer.proofMaterialsAddress,
              proofCbor: admitted.artifact.txMembershipProofCbor,
            })
          : undefined;
      return Object.freeze({
        transaction: (
          await journaledStepTransaction(
            await (async () => {
              const common = {
                lucid: config.lucid,
                blueprint: config.blueprint,
                deploymentInfo: config.deploymentInfo,
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
              } as const;
              void common;
              if (config.category === "invalidRange") {
                if (admitted.artifact.sourceKind === "forced") {
                  if (
                    config.invalidRangeContracts === null ||
                    admitted.invalidRangeEvidence === null ||
                    admitted.forcedSource === null
                  )
                    throw new Error(
                      "invalidRange forced authority disappeared",
                    );
                  return await buildInvalidRangeStep01Forced({
                    lucid: config.lucid,
                    contracts: config.invalidRangeContracts,
                    categoryId: config.categoryId,
                    signer: config.signer,
                    threadOutRef: stringField(input, "threadOutRef"),
                    evidence: admitted.invalidRangeEvidence,
                    forcedSource: admitted.forcedSource,
                    referenceScriptUtxo: config.referenceScripts.steps[0],
                  });
                } else {
                  if (admitted.inclusion === null)
                    throw new Error(
                      "invalidRange accepted inclusion disappeared",
                    );
                  return await buildInvalidRangeStep01({
                    ...common,
                    txInclusion: admitted.inclusion,
                  });
                }
              } else {
                const contracts = config.zeroInputContracts;
                const evidence = admitted.zeroInputEvidence;
                if (contracts === null || evidence === null)
                  throw new Error(
                    "zeroInput workflow omitted family authority",
                  );
                if (admitted.artifact.sourceKind === "forced") {
                  if (admitted.forcedSource === null)
                    throw new Error("zeroInput forced source disappeared");
                  return await buildZeroInputStep01Forced({
                    lucid: config.lucid,
                    contracts,
                    categoryId: config.categoryId,
                    signer: config.signer,
                    threadOutRef: stringField(input, "threadOutRef"),
                    finding: evidence,
                    forcedSource: admitted.forcedSource,
                    referenceScriptUtxo: config.referenceScripts.steps[0],
                  });
                } else {
                  if (admitted.inclusion === null)
                    throw new Error("zeroInput accepted inclusion disappeared");
                  const thread = await requireLinearFaultThreadUtxo({
                    lucid: config.lucid,
                    contracts,
                    categoryId: config.categoryId,
                    family: "zero-input",
                    stepIndex: 0,
                    threadOutRef: stringField(input, "threadOutRef"),
                  });
                  return await buildZeroInputStep01Accepted({
                    lucid: config.lucid,
                    blueprint: config.blueprint,
                    network: config.network,
                    contracts,
                    signer: config.signer,
                    finding: evidence,
                    threadUtxo: thread.threadUtxo,
                    threadToken: thread.threadToken,
                    stateQueueBlockOutRef: stringField(
                      input,
                      "stateQueueBlockOutRef",
                    ),
                    txInclusion: admitted.inclusion,
                    referenceScriptUtxo: config.referenceScripts.steps[0],
                    witnessReferenceScripts: config.referenceScripts.witnesses,
                  });
                }
              }
            })(),
            "native-inclusion-two-step step_01",
          )
        ).transaction,
      });
    }
    if (input.stage === "step_02") {
      return Object.freeze({
        transaction: (
          await journaledStepTransaction(
            await (async () => {
              const common = {
                lucid: config.lucid,
                blueprint: config.blueprint,
                deploymentInfo: config.deploymentInfo,
                network: config.network,
                signer: config.signer,
                threadOutRef: stringField(input, "threadOutRef"),
                referenceScriptUtxo: config.referenceScripts.steps[1],
                witnessReferenceScripts: config.referenceScripts.witnesses,
              } as const;
              void common;
              if (config.category === "invalidRange") {
                if (
                  config.invalidRangeContracts === null ||
                  admitted.invalidRangeEvidence === null
                )
                  throw new Error(
                    "invalidRange terminal authority disappeared",
                  );
                return await buildInvalidRangeStep02V1({
                  lucid: config.lucid,
                  contracts: config.invalidRangeContracts,
                  categoryId: config.categoryId,
                  signer: config.signer,
                  threadOutRef: stringField(input, "threadOutRef"),
                  evidence: admitted.invalidRangeEvidence,
                  referenceScriptUtxo: config.referenceScripts.steps[1],
                  witnessReferenceScripts: config.referenceScripts.witnesses,
                });
              } else {
                if (
                  config.zeroInputContracts === null ||
                  admitted.zeroInputEvidence === null
                )
                  throw new Error(
                    "zeroInput workflow omitted terminal authority",
                  );
                return await buildZeroInputStep02V1({
                  ownOutputs,
                  lucid: config.lucid,
                  contracts: config.zeroInputContracts,
                  categoryId: config.categoryId,
                  signer: config.signer,
                  threadOutRef: stringField(input, "threadOutRef"),
                  evidence: admitted.zeroInputEvidence,
                  nativeTxCompactCbor: admitted.artifact.nativeTxCompactCbor,
                  referenceScriptUtxo: config.referenceScripts.steps[1],
                  witnessReferenceScripts: config.referenceScripts.witnesses,
                });
              }
            })(),
            "native-inclusion-two-step step_02",
          )
        ).transaction,
      });
    }
    if (input.stage === "remove") {
      return await captureRemoval({ config, input });
    }
    throw new Error(
      `${config.category} workflow action has unsupported stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundInvalidRangeWorkflowConfig =
  ManifestBoundLinearFamilyWorkflowConfig<"invalidRange", WitnessRole, false>;

export type ManifestBoundZeroInputWorkflowConfig =
  ManifestBoundLinearFamilyWorkflowConfig<"zeroInput", WitnessRole, false>;

export type ManifestBoundNativeInclusionTwoStepWorkflow<
  Category extends NativeInclusionTwoStepCategory,
> = ManifestBoundLinearFamilyWorkflow<Category, false>;

export type ManifestBoundInvalidRangeWorkflow =
  ManifestBoundNativeInclusionTwoStepWorkflow<"invalidRange">;

export type ManifestBoundZeroInputWorkflow =
  ManifestBoundNativeInclusionTwoStepWorkflow<"zeroInput">;

export const stepReferenceOutRef = <
  Category extends NativeInclusionTwoStepCategory,
>(
  context: AssemblyContext<Category>,
  index: number,
): string => {
  const utxo = context.references.steps[index]!;
  return `${utxo.txHash}#${utxo.outputIndex.toString()}`;
};

export const zeroInputContracts = <
  Category extends NativeInclusionTwoStepCategory,
>(
  context: AssemblyContext<Category>,
): ZeroInputContracts => {
  const { binding } = context;
  const chain = binding.resolvedContracts.contracts.zeroInput;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  const certificatePolicyId =
    binding.fieldPreimageCertificate?.policyId ??
    binding.contractEntries.fieldPreimageCertificateMint?.scriptHash;
  if (
    chain === undefined ||
    stateQueuePolicyId === undefined ||
    certificatePolicyId === undefined
  ) {
    throw new Error("zeroInput deployment chain is incomplete");
  }
  return {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: [
        "fraud_proofs/zero_input/step_01.main.spend",
        "fraud_proofs/zero_input/step_02.main.spend",
      ][index]!,
      referenceOutRef: stepReferenceOutRef(context, index),
    })) as unknown as ZeroInputContracts["steps"],
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: {
      policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
      mintingScript:
        binding.resolvedContracts.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificatePolicyId,
  };
};
