/**
 * The CEK context's redeemers (`cek-context-redeemer.ak`, with the item
 * proof steps of `cek-context-item.ak`): the running execution's own
 * redeemer selection, then the redeemer map.
 */
import {
  buildMidgardRedeemerItemProofTrace,
  buildMidgardValidationMerkleMembership,
  finalizeMidgardRedeemerItemProof,
  hashMidgardRedeemerItemProofControl,
  MidgardRedeemerItemProofModes,
  MidgardRedeemerItemProofStages,
} from "@al-ft/midgard-core";

import {
  emptyMidgardCekDataSummary,
  hashMidgardCekRedeemerContextControl,
  initialMidgardCekRedeemerContextControl,
  type MidgardCekContextControl,
  type MidgardCekRedeemerContextControl,
  summarizeMidgardCekLucidData,
} from "../cek-context.js";
import {
  cardanoScriptPurposeData,
  type MidgardScriptPurpose,
  midgardScriptPurposeData,
} from "../midgard-redeemers.js";
import { prependMidgardCekDataPairSummary } from "../script-context-proof.js";
import { type CekContextRun } from "./cek-producer.context-step.js";
import { redeemerTagForPurposeKind } from "./redeemer-purpose.js";
import type { ScriptPurposeProofEntry } from "./trace-builder-types.js";
import { sameSequence, sameSummary } from "./trace-descriptor-summary.js";

/** The execution's own redeemer, item `index`, scanned for its budget:
 * stage 0 to 1. */
export const selectExecutionRedeemer = (
  run: CekContextRun,
  start: MidgardCekContextControl,
  index: number,
): MidgardCekContextControl => {
  const { decodedProofRedeemers, redeemerWitnessesCollection } = run.prepared;
  const { purpose } = run.execution;
  const item = redeemerWitnessesCollection.items[index]!;
  const trace = buildMidgardRedeemerItemProofTrace({
    itemIndex: index,
    itemCount: decodedProofRedeemers.length,
    itemBytes: item.bytes,
    mode: MidgardRedeemerItemProofModes.Descriptor,
    expectedPurposeTag: redeemerTagForPurposeKind(purpose.purposeKind),
    expectedPointerIndex: Number(purpose.purposeIndex),
  });
  run.at(start, {
    kind: "redeemerScanBegin",
    itemIndex: index,
    itemCount: decodedProofRedeemers.length,
    totalLength: item.bytes.length,
    itemCommitment: item.commitment,
    siblings: buildMidgardValidationMerkleMembership(
      run.prepared.redeemerLeafHashes,
      index,
    ).siblings,
  });
  let control: MidgardCekContextControl = {
    ...start,
    redeemerContextControlHash: hashMidgardRedeemerItemProofControl(
      trace.initial,
    ),
  };
  for (const itemStep of trace.steps) {
    run.at(control, {
      kind: "redeemerItemStep",
      redeemerControl: null,
      control: itemStep.control,
      witness: itemStep.witness,
    });
    control =
      itemStep.next.stage === MidgardRedeemerItemProofStages.Terminal
        ? {
            ...control,
            stage: 1,
            executionMemoryLimit: itemStep.next.executionMemory,
            executionCpuLimit: itemStep.next.executionSteps,
            redeemerContextControlHash: hashMidgardCekRedeemerContextControl(
              initialMidgardCekRedeemerContextControl(
                run.state.scriptPurposeEntries.length,
              ),
            ),
          }
        : {
            ...control,
            redeemerContextControlHash: hashMidgardRedeemerItemProofControl(
              itemStep.next,
            ),
          };
  }
  return control;
};

const purposeForProof = (
  purpose: ScriptPurposeProofEntry,
): MidgardScriptPurpose => {
  const scriptHash = purpose.scriptHash.toString("hex");
  if (purpose.purposeKind === 0) {
    return {
      kind: "spend",
      scriptHash,
      outRefHex: purpose.subject.toString("hex"),
    };
  }
  if (purpose.purposeKind === 1) {
    return { kind: "mint", scriptHash, policyId: scriptHash };
  }
  if (purpose.purposeKind === 2) return { kind: "observe", scriptHash };
  return { kind: "receive", scriptHash };
};

/**
 * The redeemer map, stage 9 to 10 (`context_successor`).
 *
 * The map is in ledger order, (tag, index) ascending, which is the purpose
 * frontier's order. The fold walks the execution frontier downwards from
 * the purpose bound and prepends, so the map ends ascending. The execution
 * leaf at `purposeBound - 1` admits one step: a select when it names a
 * redeemer, a skip when it is a native execution. The fold stops once every
 * redeemer is selected.
 */
export const redeemerContextItems = (
  run: CekContextRun,
  start: MidgardCekContextControl,
): {
  readonly control: MidgardCekContextControl;
  readonly redeemerControl: MidgardCekRedeemerContextControl;
} => {
  const {
    decodedProofRedeemers: { length: redeemerCount },
    redeemerLeafHashes: redeemerLeaves,
  } = run.prepared;
  const executions = run.state.scriptExecutionEntries;
  const executionLeaves = executions.map((entry) => entry.leaf);
  let control = start;
  let redeemerControl = initialMidgardCekRedeemerContextControl(
    run.state.scriptPurposeEntries.length,
  );
  const withRedeemerControl = (
    next: MidgardCekRedeemerContextControl,
    stage = control.stage,
  ) => {
    redeemerControl = next;
    control = {
      ...control,
      stage,
      redeemerContextControlHash:
        hashMidgardCekRedeemerContextControl(redeemerControl),
    };
  };
  for (
    let purposeFrontierIndex = executions.length - 1;
    purposeFrontierIndex >= 0 && redeemerControl.cursor < redeemerCount;
    purposeFrontierIndex -= 1
  ) {
    const execution = executions[purposeFrontierIndex]!;
    const { purpose } = execution;
    const executionSiblings = buildMidgardValidationMerkleMembership(
      executionLeaves,
      purposeFrontierIndex,
    ).siblings;
    if (execution.languageTag === 0) {
      run.at(control, {
        kind: "cekRedeemerContextSkip",
        control: redeemerControl,
        purposeLeaf: purpose.leaf,
        sourceLeaf: execution.source.leaf,
        executionSiblings,
      });
      withRedeemerControl({
        ...redeemerControl,
        purposeBound: purposeFrontierIndex,
      });
      continue;
    }
    const redeemerIndex = redeemerLeaves.findIndex((leaf) =>
      leaf.equals(execution.redeemerLeaf),
    );
    if (redeemerIndex < 0) {
      throw new Error(
        "CEK execution leaf names a redeemer outside the redeemer frontier",
      );
    }
    const item = run.prepared.redeemerWitnessesCollection.items[redeemerIndex]!;
    const descriptorOnly = run.languageTag === 3 && purpose.purposeKind === 3;
    const itemTrace = buildMidgardRedeemerItemProofTrace({
      itemIndex: redeemerIndex,
      itemCount: redeemerCount,
      itemBytes: item.bytes,
      mode: descriptorOnly
        ? MidgardRedeemerItemProofModes.Descriptor
        : MidgardRedeemerItemProofModes.Data,
      expectedPurposeTag: redeemerTagForPurposeKind(purpose.purposeKind),
      expectedPointerIndex: Number(purpose.purposeIndex),
    });
    run.at(control, {
      kind: "cekRedeemerContextSelect",
      control: redeemerControl,
      itemIndex: redeemerIndex,
      totalLength: item.bytes.length,
      itemCommitment: item.commitment,
      purpose: {
        purposeKind: purpose.purposeKind,
        purposeIndex: purpose.purposeIndex,
        scriptHash: purpose.scriptHash,
        subject: purpose.subject,
      },
      executionLanguageTag: execution.languageTag,
      sourceLeaf: execution.source.leaf,
      executionSiblings,
    });
    withRedeemerControl({
      ...redeemerControl,
      activeScanHash: hashMidgardRedeemerItemProofControl(itemTrace.initial),
      activeRedeemerLeaf: redeemerLeaves[redeemerIndex]!,
      activePurpose: descriptorOnly
        ? emptyMidgardCekDataSummary()
        : summarizeMidgardCekLucidData(
            (run.languageTag === 128
              ? midgardScriptPurposeData(purposeForProof(purpose))
              : cardanoScriptPurposeData(purposeForProof(purpose))) as never,
          ),
      purposeBound: purposeFrontierIndex,
    });
    for (const itemStep of itemTrace.steps) {
      run.at(control, {
        kind: "redeemerItemStep",
        redeemerControl,
        control: itemStep.control,
        witness: itemStep.witness,
      });
      let next: MidgardCekRedeemerContextControl;
      if (itemStep.next.stage !== MidgardRedeemerItemProofStages.Terminal) {
        next = {
          ...redeemerControl,
          activeScanHash: hashMidgardRedeemerItemProofControl(itemStep.next),
        };
      } else {
        const closed = {
          ...redeemerControl,
          cursor: redeemerControl.cursor + 1,
          activeScanHash: Buffer.alloc(0),
          activeRedeemerLeaf: Buffer.alloc(0),
          activePurpose: emptyMidgardCekDataSummary(),
        };
        const summary = descriptorOnly
          ? null
          : finalizeMidgardRedeemerItemProof(itemStep.next);
        if (!descriptorOnly && summary === null) {
          throw new Error("terminal redeemer item proof lacks a Data summary");
        }
        next =
          summary === null
            ? closed
            : {
                ...closed,
                mapItems: prependMidgardCekDataPairSummary(
                  redeemerControl.activePurpose,
                  summary,
                  redeemerControl.mapItems,
                ),
                currentRedeemer: redeemerLeaves[redeemerIndex]!.equals(
                  run.execution.redeemerLeaf,
                )
                  ? summary
                  : redeemerControl.currentRedeemer,
              };
      }
      withRedeemerControl(next, next.cursor === redeemerCount ? 10 : 9);
    }
  }
  if (redeemerControl.cursor !== redeemerCount) {
    throw new Error("CEK redeemer does not select an authenticated purpose");
  }
  if (
    control.stage !== 10 ||
    !sameSummary(redeemerControl.currentRedeemer, run.parts.redeemer) ||
    !sameSequence(redeemerControl.mapItems, run.parts.redeemerItems)
  ) {
    throw new Error("CEK redeemer context differs from the evaluated context");
  }
  return { control, redeemerControl };
};
