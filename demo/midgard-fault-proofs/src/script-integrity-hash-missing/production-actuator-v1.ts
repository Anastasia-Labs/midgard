import { decodeMidgardNativeTxCompactV1 } from "@al-ft/midgard-core";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  type FaultProofFieldOpeningPlanV1,
  faultProofFieldOpeningV1,
  resolveFaultProofFieldCarriagePublicationsV1,
  resolveFaultProofFieldPreimageCertificateV1,
} from "../field-opening-v1.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofWorkflowDeploymentBindingV1 } from "../workflow/deployment-manifest-binding-v1.js";
import {
  PRODUCTION_CURSOR_FAMILY_TRANSACTION_PORT_V1,
  type ProductionCursorFamilyTransactionPortV1,
} from "../workflow/production-cursor-family-adapter-v1.js";
import {
  captureProductionCursorRemovalV1,
  productionCursorFamilyActionInputV1,
  productionCursorStringFieldV1,
} from "../workflow/production-cursor-family-runtime-v1.js";
import {
  captureLocallyEvaluatedTransactionV1,
  type FraudProofPreSubmitBoundaryV1,
} from "../workflow/transaction-boundary-v1.js";
import type { ScriptIntegrityHashMissingContractsV1 } from "./contracts-v1.js";
import { selectScriptIntegrityHashMissingCarriageV1 } from "./family-v1.js";
import {
  admitProductionScriptIntegrityHashMissingArtifactV1,
  prepareProductionScriptIntegrityHashMissingArtifactV1,
  scriptIntegrityHashMissingWitnessSetV1,
} from "./production-artifact-v1.js";
import { ScriptIntegrityStepDatumsV1 } from "./schemas-v1.js";
import {
  encodeScriptIntegrityField8CheckpointV1,
  encodeScriptIntegrityGrammarCheckpointV1,
  encodeScriptIntegritySemanticCheckpointV1,
  hashScriptIntegrityField8CheckpointV1,
  SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET_V1,
  scriptIntegrityGrammarHashV1,
  scriptIntegritySemanticHashV1,
} from "./staged-plan-v1.js";
import {
  submitScriptIntegrityHashMissingStep01AcceptedV1,
  submitScriptIntegrityHashMissingStep01ForcedV1,
  submitScriptIntegrityHashMissingStep02AcceptedV1,
  submitScriptIntegrityHashMissingStep02BindingV1,
  submitScriptIntegrityHashMissingStep03DirectV1,
} from "./submit-direct-v1.js";
import {
  submitScriptIntegrityHashMissingRedeemerGrammarV1,
  submitScriptIntegrityHashMissingScriptGrammarV1,
  submitScriptIntegrityHashMissingScriptScanV1,
  submitScriptIntegrityHashMissingStep03V1,
  submitScriptIntegrityHashMissingStep04V1,
} from "./submitters-v1.js";

export type ScriptIntegrityHashMissingWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScriptsV1>;
  fieldPreimageCertificateMint: UTxO;
}>;

export type BoundScriptIntegrityHashMissingActuatorConfigV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"scriptIntegrityHashMissing">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: ScriptIntegrityHashMissingContractsV1;
  references: ScriptIntegrityHashMissingWorkflowReferenceScriptsV1;
  lease: StateQueueMutationLeaseCoordinator;
}>;

const txWitnessSetHash = (compactCbor: string): string =>
  Buffer.from(
    decodeMidgardNativeTxCompactV1(Buffer.from(compactCbor, "hex"))
      .transactionWitnessSetHash,
  ).toString("hex");

const resolveField = async (
  config: BoundScriptIntegrityHashMissingActuatorConfigV1,
  planned: FaultProofFieldOpeningPlanV1,
) => {
  const publications = await resolveFaultProofFieldCarriagePublicationsV1({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned,
  });
  if (publications === undefined)
    throw new Error(
      "scriptIntegrityHashMissing field publications disappeared",
    );
  const certificate = await resolveFaultProofFieldPreimageCertificateV1({
    lucid: config.lucid,
    network: config.binding.network,
    planned,
    certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
  });
  if (planned.plan.tier === "Certified" && certificate === undefined)
    throw new Error("scriptIntegrityHashMissing certificate disappeared");
  return Object.freeze({
    carriage: Object.freeze([
      ...publications,
      ...(certificate === undefined ? [] : [certificate]),
    ]),
  });
};

const threadDatum = async (
  config: BoundScriptIntegrityHashMissingActuatorConfigV1,
  outRef: string,
  ordinal: 4 | 5 | 6,
) => {
  const [txHash, output] = outRef.split("#");
  const [utxo] = await config.lucid.utxosByOutRef([
    { txHash: txHash!, outputIndex: Number(output) },
  ]);
  if (utxo?.datum === undefined || utxo.datum === null)
    throw new Error("scriptIntegrityHashMissing cursor datum disappeared");
  return Data.from(
    utxo.datum,
    ScriptIntegrityStepDatumsV1[ordinal - 1] as never,
  ) as unknown as {
    fraud_prover: string;
    data: Record<string, unknown>;
  };
};

const phaseHash = (
  state: { data: Record<string, unknown> },
  phase: "ScriptGrammar" | "ScriptScan" | "RedeemerGrammar",
): string => {
  const selected = (state.data.phase as Record<string, unknown> | undefined)?.[
    phase
  ] as Record<string, unknown> | undefined;
  if (typeof selected?.checkpoint_hash !== "string")
    throw new Error(`scriptIntegrityHashMissing expected ${phase} cursor`);
  return selected.checkpoint_hash;
};

const captured = async (
  submit: (boundary: FraudProofPreSubmitBoundaryV1) => Promise<void>,
) =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransactionV1(submit),
  });

const direct = (scriptHex: string, redeemerHex: string): boolean =>
  selectScriptIntegrityHashMissingCarriageV1({
    membershipBytes: 0,
    fieldBytes:
      Buffer.from(scriptHex, "hex").length +
      Buffer.from(redeemerHex, "hex").length,
    directBudget: 15_148,
  }) === "direct";

export const createScriptIntegrityHashMissingTransactionPortV1 = (
  config: BoundScriptIntegrityHashMissingActuatorConfigV1,
): ProductionCursorFamilyTransactionPortV1<"scriptIntegrityHashMissing"> => ({
  portVersion: PRODUCTION_CURSOR_FAMILY_TRANSACTION_PORT_V1,
  category: "scriptIntegrityHashMissing",
  prepare: prepareProductionScriptIntegrityHashMissingArtifactV1,
  capture: async ({ action, artifact }) => {
    const admitted = admitProductionScriptIntegrityHashMissingArtifactV1(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.binding.definition.headerHash)
      throw new Error(
        "scriptIntegrityHashMissing artifact changed bound header",
      );
    const input = productionCursorFamilyActionInputV1({
      category: "scriptIntegrityHashMissing",
      action,
    });
    const categoryId = config.binding.resolvedContracts.category.categoryId;
    const threadOutRef = () =>
      productionCursorStringFieldV1(input, "threadOutRef");
    const datum = (index: number, data: unknown) =>
      Data.to(
        { fraud_prover: config.signer.paymentKeyHash, data } as never,
        ScriptIntegrityStepDatumsV1[index] as never,
      );
    const witnessHash = txWitnessSetHash(admitted.evidence.nativeTxCompactCbor);
    const witnessSet = scriptIntegrityHashMissingWitnessSetV1(
      admitted.evidence.witnessSetCompactCbor,
    );
    const containsNonNative = admitted.evidence.scriptLanguages.some(
      (language) => language !== 0,
    );
    const baseState = {
      subject: admitted.evidence.subject,
      witness_set_hash: witnessHash,
      script_integrity_hash: admitted.evidence.scriptIntegrityHash,
    };

    if (input.stage === "init")
      return await captured(async (preSubmitBoundary) => {
        await submitInit({
          lucid: config.lucid,
          blueprint: config.binding.blueprint,
          deploymentInfo: config.binding.deploymentInfo,
          network: config.binding.network,
          signer: config.signer,
          fraudCategory: "scriptIntegrityHashMissing",
          fraudulentBlockOutRef: productionCursorStringFieldV1(
            input,
            "stateQueueBlockOutRef",
          ),
          fraudulentHeaderHash: admitted.artifact.headerHash,
          witnessReferenceScripts: config.references.witnesses,
          preSubmitBoundary,
          awaitConfirmation: false,
        });
      });

    if (input.stage === "step_01")
      return await captured(async (preSubmitBoundary) => {
        const common = {
          lucid: config.lucid,
          contracts: config.contracts,
          categoryId,
          signer: config.signer,
          threadOutRef: threadOutRef(),
          referenceScriptUtxo: config.references.steps[0],
          preSubmitBoundary,
          awaitConfirmation: false,
        } as const;
        if (admitted.source.acceptedInclusion !== undefined)
          await submitScriptIntegrityHashMissingStep01AcceptedV1({
            ...common,
            blueprint: config.binding.blueprint,
            network: config.binding.network,
            stateQueueBlockOutRef: productionCursorStringFieldV1(
              input,
              "stateQueueBlockOutRef",
            ),
            txInclusion: admitted.source.acceptedInclusion,
            witnessReferenceScripts: config.references.witnesses,
          });
        else
          await submitScriptIntegrityHashMissingStep01ForcedV1({
            ...common,
            direction: admitted.source.forcedDirection!,
          });
      });

    if (input.stage === "step_02")
      return await captured(async (preSubmitBoundary) => {
        const common = {
          lucid: config.lucid,
          contracts: config.contracts,
          categoryId,
          signer: config.signer,
          threadOutRef: threadOutRef(),
          referenceScriptUtxo: config.references.steps[1],
          preSubmitBoundary,
          awaitConfirmation: false,
        } as const;
        if (admitted.source.acceptedInclusion !== undefined)
          await submitScriptIntegrityHashMissingStep02AcceptedV1({
            ...common,
            header: admitted.source.header,
            subject: admitted.evidence.subject as never,
            witnessSetHash: witnessHash,
          });
        else
          await submitScriptIntegrityHashMissingStep02BindingV1({
            ...common,
            header: admitted.source.forcedHeader!,
            forcedMembership: admitted.source.forcedMembership!,
            witnessSetHash: witnessHash,
          });
      });

    if (input.stage === "step_03") {
      if (
        direct(
          admitted.evidence.scriptWitnessesPreimageCbor,
          admitted.evidence.redeemersPreimageCbor,
        )
      )
        return await captured(async (preSubmitBoundary) => {
          await submitScriptIntegrityHashMissingStep03DirectV1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: threadOutRef(),
            evidence: admitted.evidence,
            nativeTxCompactCbor: admitted.evidence.nativeTxCompactCbor,
            witnessSet,
            referenceScriptUtxo: config.references.steps[2],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      const field = await resolveField(config, admitted.scriptPlan);
      const first = admitted.staged.grammar[0]!;
      return await captured(async (preSubmitBoundary) => {
        await submitScriptIntegrityHashMissingStep03V1({
          lucid: config.lucid,
          contracts: config.contracts,
          categoryId,
          signer: config.signer,
          threadOutRef: threadOutRef(),
          referenceScriptUtxo: config.references.steps[2],
          authenticatedCarriageUtxos: field.carriage,
          staged: true,
          nextDatum: datum(3, {
            ...baseState,
            phase: {
              ScriptGrammar: {
                checkpoint_hash: scriptIntegrityGrammarHashV1(first),
              },
            },
          }),
          buildArgs: ({ input_index, output_index }) => ({
            StartStaged: {
              input_index,
              output_index,
              script_witnesses_opening: faultProofFieldOpeningV1({
                planned: admitted.scriptPlan,
                referenceInputs: [
                  ...field.carriage,
                  config.references.steps[2],
                ],
                certificatePolicyId:
                  config.contracts.fieldPreimageCertificatePolicyId,
                label: "scriptIntegrityHashMissing script opening",
              }),
              item_budget: BigInt(SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET_V1),
            },
          }),
          preSubmitBoundary,
          awaitConfirmation: false,
        });
      });
    }

    if (input.stage === "step_04") {
      const field = await resolveField(config, admitted.scriptPlan);
      const currentHash = phaseHash(
        await threadDatum(config, threadOutRef(), 4),
        "ScriptGrammar",
      );
      const index = admitted.staged.grammar.findIndex(
        (value) => scriptIntegrityGrammarHashV1(value) === currentHash,
      );
      if (index < 0)
        throw new Error(
          "scriptIntegrityHashMissing grammar checkpoint substitution",
        );
      const current = admitted.staged.grammar[index]!;
      const next = admitted.staged.grammar[index + 1];
      const closes = next === undefined;
      const opening = faultProofFieldOpeningV1({
        planned: admitted.scriptPlan,
        referenceInputs: [...field.carriage, config.references.steps[3]],
        certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
        label: "scriptIntegrityHashMissing script opening",
      });
      return await captured(async (preSubmitBoundary) => {
        await submitScriptIntegrityHashMissingScriptGrammarV1({
          lucid: config.lucid,
          contracts: config.contracts,
          categoryId,
          signer: config.signer,
          threadOutRef: threadOutRef(),
          referenceScriptUtxo: config.references.steps[3],
          authenticatedCarriageUtxos: field.carriage,
          closes,
          nextDatum: datum(closes ? 4 : 3, {
            ...baseState,
            phase: closes
              ? {
                  ScriptScan: {
                    checkpoint_hash: scriptIntegritySemanticHashV1(
                      admitted.staged.semantic[0]!,
                    ),
                    contains_non_native_script: containsNonNative,
                  },
                }
              : {
                  ScriptGrammar: {
                    checkpoint_hash: scriptIntegrityGrammarHashV1(next!),
                  },
                },
          }),
          buildArgs: ({ input_index, output_index }) =>
            closes
              ? {
                  StartScan: {
                    input_index,
                    output_index,
                    opening,
                    checkpoint_bytes:
                      encodeScriptIntegrityGrammarCheckpointV1(
                        current,
                      ).toString("hex"),
                    item_budget: BigInt(
                      SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET_V1,
                    ),
                  },
                }
              : {
                  Resume: {
                    input_index,
                    output_index,
                    opening,
                    checkpoint_bytes:
                      encodeScriptIntegrityGrammarCheckpointV1(
                        current,
                      ).toString("hex"),
                    item_budget: BigInt(
                      SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET_V1,
                    ),
                  },
                },
          preSubmitBoundary,
          awaitConfirmation: false,
        });
      });
    }

    if (input.stage === "step_05") {
      const field = await resolveField(config, admitted.scriptPlan);
      const currentHash = phaseHash(
        await threadDatum(config, threadOutRef(), 5),
        "ScriptScan",
      );
      const index = admitted.staged.semantic.findIndex(
        (value) => scriptIntegritySemanticHashV1(value) === currentHash,
      );
      const current = admitted.staged.semantic[index]!;
      const next = admitted.staged.semantic[index + 1];
      if (index < 0 || next === undefined)
        throw new Error(
          "scriptIntegrityHashMissing semantic checkpoint substitution",
        );
      const closes = index + 1 === admitted.staged.semantic.length - 1;
      return await captured(async (preSubmitBoundary) => {
        await submitScriptIntegrityHashMissingScriptScanV1({
          lucid: config.lucid,
          contracts: config.contracts,
          categoryId,
          signer: config.signer,
          threadOutRef: threadOutRef(),
          referenceScriptUtxo: config.references.steps[4],
          authenticatedCarriageUtxos: field.carriage,
          closes,
          nextDatum: datum(closes ? 5 : 4, {
            ...baseState,
            phase: closes
              ? {
                  ScriptComplete: {
                    contains_non_native_script: containsNonNative,
                  },
                }
              : {
                  ScriptScan: {
                    checkpoint_hash: scriptIntegritySemanticHashV1(next),
                    contains_non_native_script: containsNonNative,
                  },
                },
          }),
          buildArgs: ({ input_index, output_index }) => ({
            input_index,
            output_index,
            opening: faultProofFieldOpeningV1({
              planned: admitted.scriptPlan,
              referenceInputs: [...field.carriage, config.references.steps[4]],
              certificatePolicyId:
                config.contracts.fieldPreimageCertificatePolicyId,
              label: "scriptIntegrityHashMissing script opening",
            }),
            checkpoint_bytes:
              encodeScriptIntegritySemanticCheckpointV1(current).toString(
                "hex",
              ),
            item_budget: BigInt(SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET_V1),
          }),
          preSubmitBoundary,
          awaitConfirmation: false,
        });
      });
    }

    if (input.stage === "step_06") {
      const field = await resolveField(config, admitted.redeemerPlan);
      const state = await threadDatum(config, threadOutRef(), 6);
      const phase = state.data.phase as Record<string, unknown>;
      const opening = faultProofFieldOpeningV1({
        planned: admitted.redeemerPlan,
        referenceInputs: [...field.carriage, config.references.steps[5]],
        certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
        label: "scriptIntegrityHashMissing redeemer opening",
      });
      if ("ScriptComplete" in phase) {
        const first = admitted.staged.redeemerGrammar[0]!;
        return await captured(async (preSubmitBoundary) => {
          await submitScriptIntegrityHashMissingRedeemerGrammarV1({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: threadOutRef(),
            referenceScriptUtxo: config.references.steps[5],
            authenticatedCarriageUtxos: field.carriage,
            closes: false,
            nextDatum: datum(5, {
              ...baseState,
              phase: {
                RedeemerGrammar: {
                  checkpoint_hash: hashScriptIntegrityField8CheckpointV1(first),
                  contains_non_native_script: containsNonNative,
                },
              },
            }),
            buildArgs: ({ input_index, output_index }) => ({
              Start: {
                input_index,
                output_index,
                opening,
                item_budget: BigInt(
                  SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET_V1,
                ),
              },
            }),
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      }
      const currentHash = phaseHash(state, "RedeemerGrammar");
      const index = admitted.staged.redeemerGrammar.findIndex(
        (value) => hashScriptIntegrityField8CheckpointV1(value) === currentHash,
      );
      if (index < 0)
        throw new Error(
          "scriptIntegrityHashMissing redeemer checkpoint substitution",
        );
      const current = admitted.staged.redeemerGrammar[index]!;
      const next = admitted.staged.redeemerGrammar[index + 1];
      const closes = next === undefined;
      return await captured(async (preSubmitBoundary) => {
        await submitScriptIntegrityHashMissingRedeemerGrammarV1({
          lucid: config.lucid,
          contracts: config.contracts,
          categoryId,
          signer: config.signer,
          threadOutRef: threadOutRef(),
          referenceScriptUtxo: config.references.steps[5],
          authenticatedCarriageUtxos: field.carriage,
          closes,
          nextDatum: datum(
            closes ? 6 : 5,
            closes
              ? {
                  subject: admitted.evidence.subject,
                  script_integrity_hash: admitted.evidence.scriptIntegrityHash,
                  contains_non_native_script: containsNonNative,
                  has_redeemers: admitted.evidence.redeemerCount > 0,
                }
              : {
                  ...baseState,
                  phase: {
                    RedeemerGrammar: {
                      checkpoint_hash: hashScriptIntegrityField8CheckpointV1(
                        next!,
                      ),
                      contains_non_native_script: containsNonNative,
                    },
                  },
                },
          ),
          buildArgs: ({ input_index, output_index }) =>
            closes
              ? {
                  Finish: {
                    input_index,
                    output_index,
                    opening,
                    checkpoint_bytes:
                      encodeScriptIntegrityField8CheckpointV1(current).toString(
                        "hex",
                      ),
                  },
                }
              : {
                  Resume: {
                    input_index,
                    output_index,
                    opening,
                    checkpoint_bytes:
                      encodeScriptIntegrityField8CheckpointV1(current).toString(
                        "hex",
                      ),
                    item_budget: BigInt(
                      SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET_V1,
                    ),
                  },
                },
          preSubmitBoundary,
          awaitConfirmation: false,
        });
      });
    }

    if (input.stage === "step_07")
      return await captured(async (preSubmitBoundary) => {
        await submitScriptIntegrityHashMissingStep04V1({
          lucid: config.lucid,
          contracts: config.contracts,
          categoryId,
          signer: config.signer,
          threadOutRef: threadOutRef(),
          referenceScriptUtxo: config.references.steps[6],
          witnessReferenceScripts: config.references.witnesses,
          preSubmitBoundary,
          awaitConfirmation: false,
        });
      });

    if (input.stage === "remove")
      return await captureProductionCursorRemovalV1({
        category: "scriptIntegrityHashMissing",
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        deploymentInfo: config.binding.deploymentInfo,
        network: config.binding.network,
        signer: config.signer,
        headerHash: admitted.artifact.headerHash,
        input,
        stateQueueMutationLeaseCoordinator: config.lease,
        fraudProverRewardLovelace: BigInt(
          config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      });

    throw new Error(
      `scriptIntegrityHashMissing unsupported production stage ${input.stage}`,
    );
  },
});

export const scriptIntegrityHashMissingFieldRequirementV1 = ({
  actionStage,
  artifact,
  owner,
}: {
  readonly actionStage: unknown;
  readonly artifact: unknown;
  readonly owner: string;
}): FaultProofFieldOpeningPlanV1 | null => {
  const admitted = admitProductionScriptIntegrityHashMissingArtifactV1(
    artifact,
    owner,
  );
  if (
    actionStage === "step_03" &&
    direct(
      admitted.evidence.scriptWitnessesPreimageCbor,
      admitted.evidence.redeemersPreimageCbor,
    )
  )
    return null;
  if (["step_03", "step_04", "step_05"].includes(String(actionStage)))
    return admitted.scriptPlan;
  return actionStage === "step_06" ? admitted.redeemerPlan : null;
};
