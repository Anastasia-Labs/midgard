import { Data } from "@lucid-evolution/lucid";

import { buildInit } from "../submit-init.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import { workflowFieldCarriageOwnOutputs } from "../workflow/field-carriage-prerequisite.own-carriage-claims.js";
import {
  type BoundScriptIntegrityHashMissingActuatorConfig,
  captured,
  direct,
  phaseHash,
  resolveField,
  threadDatum,
  txWitnessSetHash,
} from "./actuator.resolve-field.js";
import {
  admitScriptIntegrityHashMissingArtifact,
  prepareScriptIntegrityHashMissingArtifact,
  scriptIntegrityHashMissingWitnessSet,
} from "./artifact.js";
import { ScriptIntegrityStepDatums } from "./schemas.js";
import {
  encodeScriptIntegrityField8Checkpoint,
  encodeScriptIntegrityGrammarCheckpoint,
  encodeScriptIntegritySemanticCheckpoint,
  hashScriptIntegrityField8Checkpoint,
  SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET,
  scriptIntegrityGrammarHash,
  scriptIntegritySemanticHash,
} from "./staged-plan.js";
import {
  buildScriptIntegrityHashMissingStep01Accepted,
  buildScriptIntegrityHashMissingStep01Forced,
  buildScriptIntegrityHashMissingStep02Accepted,
  buildScriptIntegrityHashMissingStep02Binding,
  buildScriptIntegrityHashMissingStep03Direct,
} from "./submit-direct.js";
import {
  buildScriptIntegrityHashMissingRedeemerGrammar,
  buildScriptIntegrityHashMissingScriptGrammar,
  buildScriptIntegrityHashMissingScriptScan,
  buildScriptIntegrityHashMissingStep03,
  buildScriptIntegrityHashMissingStep04,
} from "./submitters.js";

export const createScriptIntegrityHashMissingTransactionPort = (
  config: BoundScriptIntegrityHashMissingActuatorConfig,
): CursorFamilyTransactionPort<"scriptIntegrityHashMissing"> => ({
  portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
  category: "scriptIntegrityHashMissing",
  prepare: prepareScriptIntegrityHashMissingArtifact,
  capture: async ({ action, artifact, entries }) => {
    const ownOutputs = workflowFieldCarriageOwnOutputs({
      category: "scriptIntegrityHashMissing",
      entries,
    });
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
    const witnessHash = txWitnessSetHash(
      admitted.evidence.nativeTxCompactCbor,
      admitted.evidence.finding.source === "forced" ? "forced" : "normal",
    );
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
      return await captured(
        await buildInit({
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
        }),
        "script-integrity-hash-missing init",
      );

    if (input.stage === "step_01") {
      const common = {
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId,
        signer: config.signer,
        threadOutRef: threadOutRef(),
        referenceScriptUtxo: config.references.steps[0],
      } as const;
      return await captured(
        admitted.source.acceptedInclusion !== undefined
          ? await buildScriptIntegrityHashMissingStep01Accepted({
              ...common,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              stateQueueBlockOutRef: cursorStringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: admitted.source.acceptedInclusion,
              witnessReferenceScripts: config.references.witnesses,
            })
          : await buildScriptIntegrityHashMissingStep01Forced({
              ...common,
              direction: admitted.source.forcedDirection!,
            }),
        "script-integrity-hash-missing step_01",
      );
    }

    if (input.stage === "step_02") {
      const common = {
        lucid: config.lucid,
        contracts: config.contracts,
        categoryId,
        signer: config.signer,
        threadOutRef: threadOutRef(),
        referenceScriptUtxo: config.references.steps[1],
      } as const;
      return await captured(
        admitted.source.acceptedInclusion !== undefined
          ? await buildScriptIntegrityHashMissingStep02Accepted({
              ...common,
              header: admitted.source.header,
              subject: admitted.evidence.subject as never,
              witnessSetHash: witnessHash,
            })
          : await buildScriptIntegrityHashMissingStep02Binding({
              ...common,
              header: admitted.source.forcedHeader!,
              forcedMembership: admitted.source.forcedMembership!,
              witnessSetHash: witnessHash,
            }),
        "script-integrity-hash-missing step_02",
      );
    }

    if (input.stage === "step_03") {
      if (direct(admitted))
        return await captured(
          await buildScriptIntegrityHashMissingStep03Direct({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId,
            signer: config.signer,
            threadOutRef: threadOutRef(),
            evidence: admitted.evidence,
            nativeTxCompactCbor: admitted.evidence.nativeTxCompactCbor,
            witnessSet,
            referenceScriptUtxo: config.references.steps[2],
          }),
          "script-integrity-hash-missing step_03",
        );
      const field = await resolveField(config, admitted.scriptPlan, ownOutputs);
      const first = admitted.staged.grammar[0]!;
      return await captured(
        await buildScriptIntegrityHashMissingStep03({
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
          buildArgs: ({ input_index, output_index }, ctx) => ({
            StartStaged: {
              input_index,
              output_index,
              script_witnesses_opening: field.opening(ctx),
              item_budget: BigInt(SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET),
            },
          }),
        }),
        "script-integrity-hash-missing buildScriptIntegrityHashMissingStep03",
      );
    }

    if (input.stage === "step_04") {
      const field = await resolveField(config, admitted.scriptPlan, ownOutputs);
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
      return await captured(
        await buildScriptIntegrityHashMissingScriptGrammar({
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
          buildArgs: ({ input_index, output_index }, ctx) =>
            closes
              ? {
                  StartScan: {
                    input_index,
                    output_index,
                    opening: field.opening(ctx),
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
                    opening: field.opening(ctx),
                    checkpoint_bytes:
                      encodeScriptIntegrityGrammarCheckpoint(current).toString(
                        "hex",
                      ),
                    item_budget: BigInt(
                      SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET,
                    ),
                  },
                },
        }),
        "script-integrity-hash-missing buildScriptIntegrityHashMissingScriptGrammar",
      );
    }

    if (input.stage === "step_05") {
      const field = await resolveField(config, admitted.scriptPlan, ownOutputs);
      const currentHash = phaseHash(
        await threadDatum(config, threadOutRef(), 5),
        "ScriptScan",
      );
      const index = admitted.staged.semantic.findIndex(
        (value) => scriptIntegritySemanticHash(value) === currentHash,
      );
      if (index < 0)
        throw new Error(
          "scriptIntegrityHashMissing semantic checkpoint substitution",
        );
      const current = admitted.staged.semantic[index]!;
      const next = admitted.staged.semantic[index + 1];
      // A walk that completed inside `StartScan` (few or no script witnesses)
      // committed its terminal; this transition resumes it, folds nothing,
      // and closes the field.
      const closes =
        next === undefined || index + 1 === admitted.staged.semantic.length - 1;
      return await captured(
        await buildScriptIntegrityHashMissingScriptScan({
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
                    checkpoint_hash: scriptIntegritySemanticHash(next!),
                    contains_non_native_script: containsNonNative,
                  },
                },
          }),
          buildArgs: ({ input_index, output_index }, ctx) => ({
            input_index,
            output_index,
            opening: field.opening(ctx),
            checkpoint_bytes:
              encodeScriptIntegritySemanticCheckpoint(current).toString("hex"),
            item_budget: BigInt(SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET),
          }),
        }),
        "script-integrity-hash-missing buildScriptIntegrityHashMissingScriptScan",
      );
    }

    if (input.stage === "step_06") {
      const field = await resolveField(
        config,
        admitted.redeemerPlan,
        ownOutputs,
      );
      const state = await threadDatum(config, threadOutRef(), 6);
      const phase = state.data.phase as Record<string, unknown>;
      if ("ScriptComplete" in phase) {
        const first = admitted.staged.redeemerGrammar[0]!;
        return await captured(
          await buildScriptIntegrityHashMissingRedeemerGrammar({
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
            buildArgs: ({ input_index, output_index }, ctx) => ({
              Start: {
                input_index,
                output_index,
                opening: field.opening(ctx),
                item_budget: BigInt(SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET),
              },
            }),
          }),
          "script-integrity-hash-missing step_06",
        );
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
      return await captured(
        await buildScriptIntegrityHashMissingRedeemerGrammar({
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
          buildArgs: ({ input_index, output_index }, ctx) =>
            closes
              ? {
                  Finish: {
                    input_index,
                    output_index,
                    opening: field.opening(ctx),
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
                    opening: field.opening(ctx),
                    checkpoint_bytes:
                      encodeScriptIntegrityField8Checkpoint(current).toString(
                        "hex",
                      ),
                    item_budget: BigInt(
                      SCRIPT_INTEGRITY_HASH_MISSING_ITEM_BUDGET,
                    ),
                  },
                },
        }),
        "script-integrity-hash-missing buildScriptIntegrityHashMissingRedeemerGrammar",
      );
    }

    if (input.stage === "step_07")
      return await captured(
        await buildScriptIntegrityHashMissingStep04({
          lucid: config.lucid,
          contracts: config.contracts,
          categoryId,
          signer: config.signer,
          threadOutRef: threadOutRef(),
          referenceScriptUtxo: config.references.steps[6],
          witnessReferenceScripts: config.references.witnesses,
        }),
        "script-integrity-hash-missing step_07",
      );

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
