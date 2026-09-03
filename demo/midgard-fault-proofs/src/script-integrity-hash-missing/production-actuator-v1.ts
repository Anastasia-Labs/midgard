import { decodeMidgardNativeTxCompact } from "@al-ft/midgard-core";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  type FaultProofFieldOpeningPlan,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening-v1.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding-v1.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/production-cursor-family-adapter-v1.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/production-cursor-family-runtime-v1.js";
import {
  captureLocallyEvaluatedTransaction,
  type FraudProofPreSubmitBoundary,
} from "../workflow/transaction-boundary-v1.js";
import type { ScriptIntegrityHashMissingContracts } from "./contracts-v1.js";
import { selectScriptIntegrityHashMissingCarriage } from "./family-v1.js";
import {
  admitScriptIntegrityHashMissingArtifact,
  prepareScriptIntegrityHashMissingArtifact,
  scriptIntegrityHashMissingWitnessSet,
} from "./production-artifact-v1.js";
import { ScriptIntegrityStepDatums } from "./schemas-v1.js";
import {
  encodeScriptIntegrityField8Checkpoint,
  encodeScriptIntegrityGrammarCheckpoint,
  encodeScriptIntegritySemanticCheckpoint,
  hashScriptIntegrityField8Checkpoint,
  SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET,
  scriptIntegrityGrammarHash,
  scriptIntegritySemanticHash,
} from "./staged-plan-v1.js";
import {
  submitScriptIntegrityHashMissingStep01Accepted,
  submitScriptIntegrityHashMissingStep01Forced,
  submitScriptIntegrityHashMissingStep02Accepted,
  submitScriptIntegrityHashMissingStep02Binding,
  submitScriptIntegrityHashMissingStep03Direct,
} from "./submit-direct-v1.js";
import {
  submitScriptIntegrityHashMissingRedeemerGrammar,
  submitScriptIntegrityHashMissingScriptGrammar,
  submitScriptIntegrityHashMissingScriptScan,
  submitScriptIntegrityHashMissingStep03,
  submitScriptIntegrityHashMissingStep04,
} from "./submitters-v1.js";

export type ScriptIntegrityHashMissingWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
}>;

export type BoundScriptIntegrityHashMissingActuatorConfig = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"scriptIntegrityHashMissing">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: ScriptIntegrityHashMissingContracts;
  references: ScriptIntegrityHashMissingWorkflowReferenceScripts;
  lease: StateQueueMutationLeaseCoordinator;
}>;

const txWitnessSetHash = (compactCbor: string): string =>
  Buffer.from(
    decodeMidgardNativeTxCompact(Buffer.from(compactCbor, "hex"))
      .transactionWitnessSetHash,
  ).toString("hex");

const resolveField = async (
  config: BoundScriptIntegrityHashMissingActuatorConfig,
  planned: FaultProofFieldOpeningPlan,
) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned,
  });
  if (publications === undefined)
    throw new Error(
      "scriptIntegrityHashMissing field publications disappeared",
    );
  const certificate = await resolveFaultProofFieldPreimageCertificate({
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
  config: BoundScriptIntegrityHashMissingActuatorConfig,
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
    ScriptIntegrityStepDatums[ordinal - 1] as never,
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
  submit: (boundary: FraudProofPreSubmitBoundary) => Promise<void>,
) =>
  Object.freeze({
    transaction: await captureLocallyEvaluatedTransaction(submit),
  });

const direct = (scriptHex: string, redeemerHex: string): boolean =>
  selectScriptIntegrityHashMissingCarriage({
    membershipBytes: 0,
    fieldBytes:
      Buffer.from(scriptHex, "hex").length +
      Buffer.from(redeemerHex, "hex").length,
    directBudget: 15_148,
  }) === "direct";

export const createScriptIntegrityHashMissingTransactionPort = (
  config: BoundScriptIntegrityHashMissingActuatorConfig,
): CursorFamilyTransactionPort<"scriptIntegrityHashMissing"> => ({
  portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
  category: "scriptIntegrityHashMissing",
  prepare: prepareScriptIntegrityHashMissingArtifact,
  capture: async ({ action, artifact }) => {
    const admitted = admitScriptIntegrityHashMissingArtifact(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.binding.definition.headerHash)
      throw new Error(
        "scriptIntegrityHashMissing artifact changed bound header",
      );
    const input = cursorFamilyActionInput({
      category: "scriptIntegrityHashMissing",
      action,
    });
    const categoryId = config.binding.resolvedContracts.category.categoryId;
    const threadOutRef = () => cursorStringField(input, "threadOutRef");
    const datum = (index: number, data: unknown) =>
      Data.to(
        { fraud_prover: config.signer.paymentKeyHash, data } as never,
        ScriptIntegrityStepDatums[index] as never,
      );
    const witnessHash = txWitnessSetHash(admitted.evidence.nativeTxCompactCbor);
    const witnessSet = scriptIntegrityHashMissingWitnessSet(
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
          fraudulentBlockOutRef: cursorStringField(
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
          await submitScriptIntegrityHashMissingStep01Accepted({
            ...common,
            blueprint: config.binding.blueprint,
            network: config.binding.network,
            stateQueueBlockOutRef: cursorStringField(
              input,
              "stateQueueBlockOutRef",
            ),
            txInclusion: admitted.source.acceptedInclusion,
            witnessReferenceScripts: config.references.witnesses,
          });
        else
          await submitScriptIntegrityHashMissingStep01Forced({
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
          await submitScriptIntegrityHashMissingStep02Accepted({
            ...common,
            header: admitted.source.header,
            subject: admitted.evidence.subject as never,
            witnessSetHash: witnessHash,
          });
        else
          await submitScriptIntegrityHashMissingStep02Binding({
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
          await submitScriptIntegrityHashMissingStep03Direct({
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
        await submitScriptIntegrityHashMissingStep03({
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
                checkpoint_hash: scriptIntegrityGrammarHash(first),
              },
            },
          }),
          buildArgs: ({ input_index, output_index }) => ({
            StartStaged: {
              input_index,
              output_index,
              script_witnesses_opening: faultProofFieldOpening({
                planned: admitted.scriptPlan,
                referenceInputs: [
                  ...field.carriage,
                  config.references.steps[2],
                ],
                certificatePolicyId:
                  config.contracts.fieldPreimageCertificatePolicyId,
                label: "scriptIntegrityHashMissing script opening",
              }),
              item_budget: BigInt(SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET),
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
        (value) => scriptIntegrityGrammarHash(value) === currentHash,
      );
      if (index < 0)
        throw new Error(
          "scriptIntegrityHashMissing grammar checkpoint substitution",
        );
      const current = admitted.staged.grammar[index]!;
      const next = admitted.staged.grammar[index + 1];
      const closes = next === undefined;
      const opening = faultProofFieldOpening({
        planned: admitted.scriptPlan,
        referenceInputs: [...field.carriage, config.references.steps[3]],
        certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
        label: "scriptIntegrityHashMissing script opening",
      });
      return await captured(async (preSubmitBoundary) => {
        await submitScriptIntegrityHashMissingScriptGrammar({
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
                    checkpoint_hash: scriptIntegritySemanticHash(
                      admitted.staged.semantic[0]!,
                    ),
                    contains_non_native_script: containsNonNative,
                  },
                }
              : {
                  ScriptGrammar: {
                    checkpoint_hash: scriptIntegrityGrammarHash(next!),
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
                      encodeScriptIntegrityGrammarCheckpoint(current).toString(
                        "hex",
                      ),
                    item_budget: BigInt(
                      SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET,
                    ),
                  },
                }
              : {
                  Resume: {
                    input_index,
                    output_index,
                    opening,
                    checkpoint_bytes:
                      encodeScriptIntegrityGrammarCheckpoint(current).toString(
                        "hex",
                      ),
                    item_budget: BigInt(
                      SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET,
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
        (value) => scriptIntegritySemanticHash(value) === currentHash,
      );
      const current = admitted.staged.semantic[index]!;
      const next = admitted.staged.semantic[index + 1];
      if (index < 0 || next === undefined)
        throw new Error(
          "scriptIntegrityHashMissing semantic checkpoint substitution",
        );
      const closes = index + 1 === admitted.staged.semantic.length - 1;
      return await captured(async (preSubmitBoundary) => {
        await submitScriptIntegrityHashMissingScriptScan({
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
                    checkpoint_hash: scriptIntegritySemanticHash(next),
                    contains_non_native_script: containsNonNative,
                  },
                },
          }),
          buildArgs: ({ input_index, output_index }) => ({
            input_index,
            output_index,
            opening: faultProofFieldOpening({
              planned: admitted.scriptPlan,
              referenceInputs: [...field.carriage, config.references.steps[4]],
              certificatePolicyId:
                config.contracts.fieldPreimageCertificatePolicyId,
              label: "scriptIntegrityHashMissing script opening",
            }),
            checkpoint_bytes:
              encodeScriptIntegritySemanticCheckpoint(current).toString("hex"),
            item_budget: BigInt(SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET),
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
      const opening = faultProofFieldOpening({
        planned: admitted.redeemerPlan,
        referenceInputs: [...field.carriage, config.references.steps[5]],
        certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
        label: "scriptIntegrityHashMissing redeemer opening",
      });
      if ("ScriptComplete" in phase) {
        const first = admitted.staged.redeemerGrammar[0]!;
        return await captured(async (preSubmitBoundary) => {
          await submitScriptIntegrityHashMissingRedeemerGrammar({
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
                  checkpoint_hash: hashScriptIntegrityField8Checkpoint(first),
                  contains_non_native_script: containsNonNative,
                },
              },
            }),
            buildArgs: ({ input_index, output_index }) => ({
              Start: {
                input_index,
                output_index,
                opening,
                item_budget: BigInt(SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET),
              },
            }),
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        });
      }
      const currentHash = phaseHash(state, "RedeemerGrammar");
      const index = admitted.staged.redeemerGrammar.findIndex(
        (value) => hashScriptIntegrityField8Checkpoint(value) === currentHash,
      );
      if (index < 0)
        throw new Error(
          "scriptIntegrityHashMissing redeemer checkpoint substitution",
        );
      const current = admitted.staged.redeemerGrammar[index]!;
      const next = admitted.staged.redeemerGrammar[index + 1];
      const closes = next === undefined;
      return await captured(async (preSubmitBoundary) => {
        await submitScriptIntegrityHashMissingRedeemerGrammar({
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
                      checkpoint_hash: hashScriptIntegrityField8Checkpoint(
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
                      encodeScriptIntegrityField8Checkpoint(current).toString(
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
                      encodeScriptIntegrityField8Checkpoint(current).toString(
                        "hex",
                      ),
                    item_budget: BigInt(
                      SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET,
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
        await submitScriptIntegrityHashMissingStep04({
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
      return await captureCursorRemoval({
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

export const scriptIntegrityHashMissingFieldRequirement = ({
  actionStage,
  artifact,
  owner,
}: {
  readonly actionStage: unknown;
  readonly artifact: unknown;
  readonly owner: string;
}): FaultProofFieldOpeningPlan | null => {
  const admitted = admitScriptIntegrityHashMissingArtifact(artifact, owner);
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
