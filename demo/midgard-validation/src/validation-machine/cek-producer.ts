/**
 * The CEK producer: for each script execution, its descriptor, its context
 * built step by step (`cek-context-*.ak`), and its core steps.
 */
import {
  buildMidgardBoundedItemChunkProof,
  hashMidgardResolvedContextItemLeaf,
} from "@al-ft/midgard-core";

import {
  decodeMidgardCekContext,
  initialMidgardCekContextControl,
  summarizeMidgardCekContextParts,
} from "../cek-context.js";
import {
  buildMidgardCekExecutionGraph,
  startMidgardCekObservedRun,
} from "../cek-executor.js";
import { buildMidgardCanonicalCekProgram } from "../cek-program.js";
import { seedCekContext } from "./cek-producer.context-finalization.js";
import { observerContextItems } from "./cek-producer.context-observer.js";
import {
  redeemerContextItems,
  selectExecutionRedeemer,
} from "./cek-producer.context-redeemer.js";
import {
  type CekContextRun,
  type CekPrepared,
  type CekState,
  cekWitnessCbor,
  mintContextItems,
  outputContextItems,
  resolvedContextItems,
  signerContextItems,
} from "./cek-producer.context-step.js";
import { runCekExecution } from "./cek-producer.execution.js";
import {
  boundedItemForScriptSource,
  executionDescriptor,
} from "./native-scripts-producer.js";
import {
  phaseContinues,
  type PhaseOutcome,
  phaseStops,
  type TraceWalk,
} from "./trace-walk.js";

export const produceCek = (
  prepared: CekPrepared,
  state: CekState,
  nativeControlCbor: Buffer,
  { push, budget, mode }: Pick<TraceWalk, "push" | "budget" | "mode">,
): PhaseOutcome => {
  const { resolutionScheduleNodes, ledgerDescriptorState } = prepared;
  const resolvedLeaves = resolutionScheduleNodes.map((node, itemIndex) => {
    const descriptorCbor = ledgerDescriptorState.get(node.key.toString("hex"));
    if (descriptorCbor === undefined) {
      throw new Error(
        "CEK context construction lost an authenticated resolved-input descriptor",
      );
    }
    return hashMidgardResolvedContextItemLeaf({
      sourceKind: node.sourceKind,
      itemIndex,
      key: node.key,
      outputCbor: descriptorCbor,
    });
  });

  let evaluationIndex = 0;
  for (const [
    executionIndex,
    execution,
  ] of state.scriptExecutionEntries.entries()) {
    const { purpose, languageTag } = execution;
    const receive = languageTag === 3 && purpose.purposeKind === 3;
    const evaluation =
      languageTag === 0 || receive
        ? undefined
        : prepared.scriptEvaluations[evaluationIndex++];
    const at = {
      nativeControlCbor,
      executionCursor: executionIndex,
      completedCpu: budget.cpu,
      completedMemory: budget.memory,
    };
    const sourceItem = boundedItemForScriptSource(execution.source);
    push("cek", cekWitnessCbor(at), {
      kind: "nativeExecutionScan",
      ...executionDescriptor(state, executionIndex),
      firstChunkProof: buildMidgardBoundedItemChunkProof(sourceItem, 0),
      ...(evaluation?.phase2Class === undefined
        ? {}
        : { phase2Class: evaluation.phase2Class }),
    });

    if (receive) {
      return phaseStops(
        {
          ReceivePurposePlutusV3Forbidden: {
            purpose_kind: BigInt(purpose.purposeKind),
            purpose_index: BigInt(purpose.purposeIndex),
          },
        },
        "a PlutusV3 script runs under a receive purpose",
      );
    }
    if (languageTag === 0) continue;

    if (
      evaluation === undefined ||
      !evaluation.scriptBytes.equals(execution.source.script.scriptBytes) ||
      evaluation.phase2Class === undefined
    ) {
      throw new Error(
        "CEK execution is missing its exact script decode classification",
      );
    }
    if (evaluation.phase2Class !== 0) {
      if (
        evaluation.result.kind !== "script_invalid" ||
        evaluation.graph !== null ||
        evaluation.execution !== null
      )
        throw new Error(
          "Phase-two selection refusal disagrees with its evaluation",
        );
      return phaseStops(
        {
          PlutusExecutionFailed: {
            purpose_kind: BigInt(purpose.purposeKind),
            purpose_index: purpose.purposeIndex,
          },
        },
        "phase-two selection refuses the script",
      );
    }
    if (evaluation.graph === null && mode === "proof")
      throw new Error(
        "CEK execution is missing its authenticated program graph",
      );
    const selectedIndex = prepared.redeemerLeafHashes.findIndex((leaf) =>
      leaf.equals(execution.redeemerLeaf),
    );
    if (selectedIndex < 0) {
      throw new Error(
        "CEK execution does not select an authenticated redeemer",
      );
    }
    const exact = evaluation.execution;
    if (exact === null && mode === "proof") {
      throw new Error(
        "CEK trace is missing the evaluator's captured execution",
      );
    }
    const program = buildMidgardCanonicalCekProgram(
      execution.source.script.scriptBytes,
    );
    // A proof build carries the evaluator's graph and captured run; a
    // commit build derives the graph from the carried Flat program and
    // re-executes it, streaming its steps (unverified, kept by no sink).
    const graph =
      evaluation.graph ??
      buildMidgardCekExecutionGraph(
        program.termRoot,
        program.material.values(),
        evaluation.contextCbor,
      );
    const observed =
      exact === null
        ? startMidgardCekObservedRun({
            root: graph.root,
            material: graph.material.values(),
            constantWitnesses: graph.constantWitnesses,
            executionIndex: evaluation.executionIndex,
            maxSteps:
              prepared.input.consensusProfile.limits
                .maxValidationMachineStepCount,
            executionBudget: evaluation.executionBudget,
          })
        : null;
    const initialState = exact?.initialState ?? observed!.initialState;
    const initialControl = initialMidgardCekContextControl({
      languageTag,
      programTermRoot: program.termRoot,
      purposeKind: purpose.purposeKind,
      purposeIndex: purpose.purposeIndex,
      scriptHash: purpose.scriptHash,
      subject: purpose.subject,
      redeemerLeaf: execution.redeemerLeaf,
    });
    const run: CekContextRun = {
      prepared,
      state,
      execution,
      languageTag,
      parts: summarizeMidgardCekContextParts(
        decodeMidgardCekContext(evaluation.contextCbor),
        languageTag,
      ),
      resolvedLeaves,
      at: (control, auxiliary) =>
        push("cek", cekWitnessCbor(at, control), auxiliary),
    };

    // The context, stage by stage: the selected redeemer (0 to 1), the
    // resolved inputs (to 3), outputs (4), signers (5), observers (6), the
    // mint (9), the redeemer map (10), then its finalization and seed.
    let control = selectExecutionRedeemer(run, initialControl, selectedIndex);
    for (const step of [
      resolvedContextItems,
      outputContextItems,
      signerContextItems,
      observerContextItems,
      mintContextItems,
    ]) {
      control = step(run, control);
    }
    const redeemers = redeemerContextItems(run, control);
    control = seedCekContext(run, redeemers.control, redeemers.redeemerControl);

    const stop = runCekExecution({
      context: run.parts.context,
      at,
      budget,
      contextControl: control,
      graph,
      exact,
      observed,
      initialState,
      exUnits: prepared.decodedProofRedeemers[selectedIndex]!.exUnits,
      accepted: evaluation.result.kind === "accepted",
      push,
    });
    if (stop !== null) return stop;
  }
  if (evaluationIndex !== prepared.scriptEvaluations.length) {
    throw new Error("CEK trace did not consume every local script evaluation");
  }
  if (state.scriptExecutionEntries.length === 0) {
    push(
      "cek",
      cekWitnessCbor({
        nativeControlCbor,
        executionCursor: 0,
        completedCpu: 0n,
        completedMemory: 0n,
      }),
    );
  }
  return phaseContinues;
};
