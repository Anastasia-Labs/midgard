import {
  decodeMidgardCekProgramEnvelope,
  decodeMidgardCekProgramMaterialSidecar,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeScriptIntegrityHashForLanguages,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { Effect } from "effect";

import {
  buildMidgardCekExecutionGraph,
  executeMidgardCekStructuralProgram,
} from "./cek-executor.js";
import {
  encodeScriptContextCbor,
  evaluateScriptWithHarmonic,
  type LocalScriptEvalResult,
} from "./local-script-eval.js";
import { findRedeemerByPointer } from "./midgard-redeemers.js";
import { discoverLocalScriptExecutions } from "./phase-b.discover-local-script-executions.js";
import {
  type CandidateNode,
  collectInlineScriptSources,
  type LocalScriptValidationResult,
  requiredScriptLanguages,
  type ResolvedReferenceInputs,
} from "./phase-b.resolve-reference-inputs.js";
import {
  buildMidgardScriptContext,
  buildPlutusV3ScriptContext,
} from "./script-context.js";
import { PhaseBConfig, RejectCodes } from "./types.js";

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
    const inlineSources = collectInlineScriptSources(ledgerTx);
    const sources = [
      ...inlineSources,
      ...resolvedReferenceInputs.scriptSources,
    ];
    const discovered = discoverLocalScriptExecutions(
      node,
      stateValue,
      sources,
      resolvedReferenceInputs.inputs,
      witnessKeyHashes,
    );
    if (discovered.kind === "rejected") {
      return discovered;
    }
    let proofProgramMaterial: ReturnType<
      typeof decodeMidgardCekProgramMaterialSidecar
    > | null = null;
    if (candidate.submission.programMaterialSidecarCbor !== null) {
      try {
        proofProgramMaterial = decodeMidgardCekProgramMaterialSidecar(
          candidate.submission.programMaterialSidecarCbor,
        );
      } catch (cause) {
        return {
          kind: "rejected",
          code: RejectCodes.CekProgramMaterial,
          detail: `invalid V1 CEK material during execution: ${String(cause)}`,
          consensusPhase: "scriptSources",
        };
      }
    }

    const usedInlineSourceIds = new Set(
      discovered.executions
        .filter((execution) => execution.resolved.source.origin === "inline")
        .map((execution) => execution.resolved.source.sourceId),
    );
    for (const source of inlineSources) {
      if (!usedInlineSourceIds.has(source.sourceId)) {
        const kind =
          source.nativeScript === undefined ? "non-native" : "native";
        return {
          kind: "rejected",
          code: RejectCodes.InvalidFieldType,
          detail: `extraneous ${kind} script witness ${source.sourceId}`,
          consensusPhase: "scriptSources",
        };
      }
    }

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
        };
      }

      const context =
        execution.resolved.version === "MidgardV1"
          ? buildMidgardScriptContext(
              discovered.contextView,
              execution.purpose,
              redeemer,
            )
          : buildPlutusV3ScriptContext(
              discovered.contextView,
              execution.purpose,
              redeemer,
            );
      const contextCbor = encodeScriptContextCbor(context);
      const executionBudget =
        config.enforceScriptBudget === false
          ? undefined
          : {
              cpu: redeemer.exUnits.steps,
              memory: redeemer.exUnits.memory,
            };
      let result: LocalScriptEvalResult;
      if (proofProgramMaterial !== null) {
        if (config.evaluateProofScript !== undefined) {
          result = yield* config.evaluateProofScript(
            execution.resolved.source.scriptBytes,
            contextCbor,
            executionBudget,
          );
        } else {
          try {
            const envelope = decodeMidgardCekProgramEnvelope(
              execution.resolved.source.scriptBytes,
            );
            const graph = buildMidgardCekExecutionGraph(
              envelope,
              proofProgramMaterial,
              contextCbor,
            );
            const cek = executeMidgardCekStructuralProgram({
              root: graph.root,
              material: graph.material.values(),
              constantWitnesses: graph.constantWitnesses,
              maxSteps: MIDGARD_CONSENSUS_LIMITS.maxValidationMachineStepCount,
              executionBudget,
            });
            result =
              cek.stopReason === "budgetExceeded" ||
              cek.terminalState.mode === "haltSuccess"
                ? {
                    kind: "accepted",
                    budget: {
                      cpu: cek.terminalState.cpu,
                      memory: cek.terminalState.memory,
                    },
                  }
                : {
                    kind: "script_invalid",
                    detail: `V1 CEK halted with error ${cek.terminalState.auxiliary.toString(10)}`,
                  };
          } catch (cause) {
            result = {
              kind: "script_invalid",
              detail: `V1 CEK execution failed closed: ${String(cause)}`,
            };
          }
        }
      } else {
        result =
          config.evaluateScript === undefined
            ? evaluateScriptWithHarmonic(
                execution.resolved.source.scriptBytes,
                context,
              )
            : yield* config.evaluateScript(
                execution.resolved.source.scriptBytes,
                contextCbor,
              );
      }
      if (result.kind === "script_invalid") {
        return {
          kind: "rejected",
          code: RejectCodes.PlutusScriptInvalid,
          detail: `${execution.purpose.kind} ${execution.purpose.scriptHash}: ${result.detail}`,
          consensusPhase: "cek",
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
        };
      }
    }

    return { kind: "accepted" };
  });

type ConflictNode = {
  readonly spentOutRefs: ReadonlySet<string>;
  readonly referenceOutRefs: ReadonlySet<string>;
};

/**
 * Builds transitive conflict components in near-linear time. Ref/ref overlap is
 * intentionally not a conflict; spend/spend and spend/ref overlap are.
 */
export const buildConflictComponents = <T extends ConflictNode>(
  readyNodes: readonly T[],
): T[][] => {
  const parent = readyNodes.map((_, index) => index);
  const rank = readyNodes.map(() => 0);
  const find = (index: number): number => {
    let root = index;
    while (parent[root] !== root) {
      root = parent[root];
    }
    while (parent[index] !== index) {
      const next = parent[index];
      parent[index] = root;
      index = next;
    }
    return root;
  };
  const union = (left: number, right: number): void => {
    let leftRoot = find(left);
    let rightRoot = find(right);
    if (leftRoot === rightRoot) return;
    if (rank[leftRoot] < rank[rightRoot]) {
      [leftRoot, rightRoot] = [rightRoot, leftRoot];
    }
    parent[rightRoot] = leftRoot;
    if (rank[leftRoot] === rank[rightRoot]) rank[leftRoot] += 1;
  };

  const spenderByOutRef = new Map<string, number>();
  const referencersByOutRef = new Map<string, number[]>();
  readyNodes.forEach((node, nodeIndex) => {
    for (const outRef of node.spentOutRefs) {
      const spender = spenderByOutRef.get(outRef);
      if (spender !== undefined) union(nodeIndex, spender);
      for (const referencer of referencersByOutRef.get(outRef) ?? []) {
        union(nodeIndex, referencer);
      }
      spenderByOutRef.set(outRef, nodeIndex);
    }
    for (const outRef of node.referenceOutRefs) {
      const spender = spenderByOutRef.get(outRef);
      if (spender !== undefined) union(nodeIndex, spender);
      const referencers = referencersByOutRef.get(outRef) ?? [];
      referencers.push(nodeIndex);
      referencersByOutRef.set(outRef, referencers);
    }
  });

  const componentByRoot = new Map<number, T[]>();
  readyNodes.forEach((node, index) => {
    const root = find(index);
    const component = componentByRoot.get(root) ?? [];
    component.push(node);
    componentByRoot.set(root, component);
  });
  return Array.from(componentByRoot.values());
};
