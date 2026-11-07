import {
  computeScriptIntegrityHashForLanguages,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { Effect } from "effect";

import { findRedeemerByPointer } from "../midgard-redeemers.js";
import { PhaseBConfig, RejectCodes } from "../types.js";
import { type CandidateNode } from "./candidate-graph.js";
import {
  runScriptExecution,
  ScriptEvaluationIndeterminateError,
  scriptExecutionContextCbor,
} from "./cek-evaluation.js";
import {
  discoverLocalScriptExecutions,
  PURPOSE_KIND,
} from "./script-discovery.js";
import {
  collectScriptSources,
  type LocalScriptValidationResult,
  requiredScriptLanguages,
  type ResolvedReferenceInputs,
} from "./script-sources.js";

export const runLocalScriptEvaluation = (
  node: CandidateNode,
  stateValue: (outRefHex: string) => Buffer | undefined,
  resolvedReferenceInputs: ResolvedReferenceInputs,
  witnessKeyHashes: ReadonlySet<string>,
  config: PhaseBConfig,
): Effect.Effect<LocalScriptValidationResult, Error> =>
  Effect.gen(function* () {
    const candidate = node.candidate;
    const { ledgerTx } = candidate;
    const sources = collectScriptSources(
      node,
      stateValue,
      resolvedReferenceInputs,
    );
    const inlineSources = sources.filter(
      (source) => source.origin === "inline",
    );
    const discovered = discoverLocalScriptExecutions(
      node,
      stateValue,
      sources,
      resolvedReferenceInputs.inputs,
      witnessKeyHashes,
      config.l1SlotConfig,
    );
    if (discovered.kind === "rejected") {
      return discovered;
    }
    const usedInlineSourceIds = new Set(
      discovered.executions
        .filter((execution) => execution.resolved.source.origin === "inline")
        .map((execution) => execution.resolved.source.sourceId),
    );
    for (const [position, source] of inlineSources.entries()) {
      if (!usedInlineSourceIds.has(source.sourceId)) {
        const kind =
          source.nativeScript === undefined ? "non-native" : "native";
        return {
          kind: "rejected",
          code: RejectCodes.InvalidFieldType,
          detail: `extraneous ${kind} script witness ${source.sourceId}`,
          consensusPhase: "scriptSources",
          subject: {
            arm: "UnusedScriptWitness",
            index: BigInt(ledgerTx.scriptWitnesses[position]!.index),
          },
        };
      }
    }
    // Stage 12 follows stage 11: an unused redeemer is refused only once every
    // inline witness is used.
    if (discovered.unusedRedeemer !== undefined) {
      return discovered.unusedRedeemer;
    }

    // Execution ordinals are positions in the discovered execution list
    // (spend, mint, observe, receive), native and non-native alike.
    const executionIndexOf = new Map(
      discovered.executions.map((execution, index) => [
        execution,
        BigInt(index),
      ]),
    );
    // A rejected execution is cited by its purpose key: the purpose kind and
    // the redeemer pointer index of that purpose.
    const purposeKeyOf = (
      execution: (typeof discovered.executions)[number],
    ): { readonly purposeKind: bigint; readonly purposeIndex: bigint } => ({
      purposeKind: PURPOSE_KIND[execution.purpose.kind],
      purposeIndex: execution.pointer.index,
    });
    for (const execution of discovered.executions) {
      if (execution.resolved.version !== "NativeCardano") {
        continue;
      }
      const nativeScript = execution.resolved.source.nativeScript!;
      if (
        !verifyMidgardNativeScript(nativeScript, {
          validityIntervalStart: ledgerTx.validityIntervalStart,
          validityIntervalEnd: ledgerTx.validityIntervalEnd,
          witnessSigners: witnessKeyHashes,
        })
      ) {
        return {
          kind: "rejected",
          code: RejectCodes.NativeScriptInvalid,
          detail: `native script verification failed for ${execution.purpose.kind} ${execution.purpose.scriptHash}`,
          consensusPhase: "nativeScripts",
          subject: {
            arm: "ExecutionNativeScriptFalse",
            ...purposeKeyOf(execution),
          },
        };
      }
    }

    const nonNativeExecutions = discovered.executions.filter(
      (execution) => execution.resolved.version !== "NativeCardano",
    );

    const languages = requiredScriptLanguages(discovered.executions);
    const expectedScriptIntegrityHash = computeScriptIntegrityHashForLanguages(
      candidate.derived.redeemerWitnessHash,
      languages,
    );
    if (!ledgerTx.scriptIntegrityHash.equals(expectedScriptIntegrityHash)) {
      const expectedHex = expectedScriptIntegrityHash.toString("hex");
      const actualHex = ledgerTx.scriptIntegrityHash.toString("hex");
      return {
        kind: "rejected",
        code: RejectCodes.InvalidFieldType,
        detail: `script_integrity_hash mismatch: expected ${expectedHex} actual ${actualHex} required_languages=${languages.join(",")}`,
        consensusPhase: "scriptIntegrity",
        subject: { arm: "ScriptIntegrityHashMismatch" },
      };
    }

    for (const execution of nonNativeExecutions) {
      const redeemer = findRedeemerByPointer(
        discovered.contextView.redeemers.map((entry) => entry.redeemer),
        execution.pointer,
      )!;

      if (
        execution.resolved.version === "PlutusV3" &&
        execution.purpose.kind === "receive"
      ) {
        return {
          kind: "rejected",
          code: RejectCodes.PlutusScriptInvalid,
          detail: "ReceivingScript requires MidgardV1 context",
          consensusPhase: "cek",
          subject: {
            arm: "ReceivePurposePlutusV3Forbidden",
            ...purposeKeyOf(execution),
          },
        };
      }

      const executionBudget =
        config.enforceScriptBudget === false
          ? undefined
          : {
              cpu: redeemer.exUnits.steps,
              memory: redeemer.exUnits.memory,
            };
      const executionIndex = executionIndexOf.get(execution)!;
      const maxSteps =
        config.maxScriptExecutionSteps ??
        MIDGARD_CONSENSUS_LIMITS.maxValidationMachineStepCount;
      if (config.committedScriptRun !== undefined) {
        config.committedScriptRun({
          scriptBytes: execution.resolved.source.scriptBytes,
          scriptLanguage: execution.resolved.version as
            | "PlutusV3"
            | "MidgardV1",
          executionBudget,
          executionIndex,
          maxSteps,
        });
        continue;
      }
      const contextCbor = scriptExecutionContextCbor(
        discovered.contextView,
        execution,
        redeemer,
      );
      const {
        result,
        phase2Class,
        graph,
        execution: cek,
      } = yield* runScriptExecution(
        {
          scriptBytes: execution.resolved.source.scriptBytes,
          scriptLanguage: execution.resolved.version as
            | "PlutusV3"
            | "MidgardV1",
          contextCbor,
          executionBudget,
          executionIndex,
          maxSteps,
        },
        config,
      );
      config.onScriptEvaluated?.(candidate.ledgerTx.txId, {
        scriptBytes: Buffer.from(execution.resolved.source.scriptBytes),
        contextCbor: Buffer.from(contextCbor),
        executionIndex,
        executionBudget,
        result,
        ...(phase2Class === undefined ? {} : { phase2Class }),
        graph,
        execution: cek,
      });
      if (result.kind === "indeterminate") {
        return yield* Effect.fail(
          new ScriptEvaluationIndeterminateError(
            candidate.ledgerTx.txId,
            `${execution.purpose.kind} ${execution.purpose.scriptHash}: ${result.detail}`,
            { cause: result.cause },
          ),
        );
      }
      if (result.kind === "script_invalid") {
        return {
          kind: "rejected",
          code: RejectCodes.PlutusScriptInvalid,
          detail: `${execution.purpose.kind} ${execution.purpose.scriptHash}: ${result.detail}`,
          consensusPhase: "cek",
          subject: {
            arm: "PlutusExecutionFailed",
            ...purposeKeyOf(execution),
          },
        };
      }
      if (
        config.enforceScriptBudget !== false &&
        (result.budget.cpu > redeemer.exUnits.steps ||
          result.budget.memory > redeemer.exUnits.memory)
      ) {
        return {
          kind: "rejected",
          code: RejectCodes.PlutusScriptInvalid,
          detail: `${execution.purpose.kind} ${execution.purpose.scriptHash}: budget exceeded (spent mem=${result.budget.memory} cpu=${result.budget.cpu}, declared mem=${redeemer.exUnits.memory} cpu=${redeemer.exUnits.steps})`,
          consensusPhase: "cek",
          subject: {
            arm: "PlutusExecutionFailed",
            ...purposeKeyOf(execution),
          },
        };
      }
    }

    return { kind: "accepted" };
  });
