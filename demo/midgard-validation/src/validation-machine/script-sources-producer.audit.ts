/**
 * ScriptSources stages 11 and 12: every inline source and every redeemer
 * audited as used by some purpose, then the NativeScripts handoff built
 * from the folds the phase leaves.
 */
import {
  encodeCbor,
  hashMidgardRedeemerItemProofControl,
  MidgardRedeemerItemProofStages,
  type MidgardValidationMerkleFrontier,
} from "@al-ft/midgard-core";

import {
  encodeValidationFrontierPeaks as peaks,
  type ScriptDiscoveryTraceControl,
} from "./control-encoding.js";
import { proofBinding } from "./prepare-producers.shared.js";
import {
  type DiscoveryRun,
  resetCurrent,
} from "./script-sources-producer.discovery.js";
import type { ValidationTraceWalkState } from "./trace-builder-types.js";
import { type PhaseOutcome, phaseStops } from "./trace-walk.js";

/** The NativeScripts handoff ScriptSources leaves. */
export type NativeScriptsHandoff = Pick<
  ValidationTraceWalkState,
  | "authenticatedNativeScriptsWitnessCbor"
  | "authenticatedNativeScriptsBaseFields"
>;

/**
 * Stages 11 and 12, over stage 10's discovery control. It stops on the
 * first unused inline source or redeemer, or returns the handoff.
 */
export const produceScriptAudit = (
  run: DiscoveryRun,
  discoveredControl: ScriptDiscoveryTraceControl,
  descriptorFrontier: MidgardValidationMerkleFrontier,
): PhaseOutcome | ({ readonly kind: "audited" } & NativeScriptsHandoff) => {
  const { prepared, push, witness, sourceEntries, replay, purposes } = run;
  let discovery = resetCurrent({ ...discoveredControl, sourceCursor: 0 });
  for (
    let sourceIndex = 0;
    sourceIndex < sourceEntries.length;
    sourceIndex += 1
  ) {
    const source = sourceEntries[sourceIndex]!;
    push("scriptSources", witness(11, discovery), run.sourceScan(sourceIndex));
    if (
      source.originKind === "inline" &&
      (discovery.usedInlineBitmap & (1n << BigInt(sourceIndex))) === 0n
    ) {
      return phaseStops(
        { UnusedScriptWitness: { script_index: BigInt(sourceIndex) } },
        "V1 source audit found an extraneous inline script",
      );
    }
    discovery = { ...discovery, sourceCursor: sourceIndex + 1 };
  }

  push("scriptSources", witness(11, discovery));
  discovery = { ...discovery, redeemerCursor: 0 };
  for (
    let redeemerIndex = 0;
    redeemerIndex < prepared.decodedProofRedeemers.length;
    redeemerIndex += 1
  ) {
    const itemTrace = run.redeemerItemTrace(redeemerIndex);
    push(
      "scriptSources",
      witness(12, discovery),
      run.redeemerScanBegin(redeemerIndex),
    );
    discovery = {
      ...discovery,
      redeemerItemControlHash: hashMidgardRedeemerItemProofControl(
        itemTrace.initial,
      ),
    };
    for (const itemStep of itemTrace.steps) {
      push("scriptSources", witness(12, discovery), {
        kind: "redeemerItemStep",
        redeemerControl: null,
        control: itemStep.control,
        witness: itemStep.witness,
      });
      if (itemStep.next.stage !== MidgardRedeemerItemProofStages.Terminal) {
        discovery = {
          ...discovery,
          redeemerItemControlHash: hashMidgardRedeemerItemProofControl(
            itemStep.next,
          ),
        };
      }
    }
    if ((discovery.usedRedeemerBitmap & (1n << BigInt(redeemerIndex))) === 0n) {
      return phaseStops(
        { UnusedRedeemer: { redeemer_index: BigInt(redeemerIndex) } },
        "V1 redeemer audit found an extraneous redeemer",
      );
    }
    discovery = {
      ...discovery,
      redeemerCursor: redeemerIndex + 1,
      redeemerItemControlHash: Buffer.alloc(0),
    };
  }

  const { outputFrontier, redeemerFrontier } = prepared;
  const { assetFrontier } = purposes.mintFoldControl;
  const { executionFrontier } = discovery;
  const baseFields: unknown[] = [
    ...proofBinding(prepared),
    BigInt(prepared.resolutionScheduleNodes.length),
    BigInt(replay.spendIndex),
    peaks(replay.resolvedItemFrontier),
    BigInt(prepared.signerFrontier.count),
    prepared.signerFrontierCommitment,
    BigInt(replay.sourceFrontier.count),
    peaks(replay.sourceFrontier),
    BigInt(redeemerFrontier.count),
    peaks(redeemerFrontier),
    BigInt(purposes.purposeFrontier.count),
    peaks(purposes.purposeFrontier),
    BigInt(outputFrontier.count),
    peaks(outputFrontier),
    peaks(descriptorFrontier),
    BigInt(assetFrontier.count),
    peaks(assetFrontier),
    BigInt(executionFrontier.count),
    peaks(executionFrontier),
  ];
  const handoff: NativeScriptsHandoff = {
    authenticatedNativeScriptsBaseFields: baseFields,
    authenticatedNativeScriptsWitnessCbor: encodeCbor([
      ...baseFields,
      0n,
      0n,
      prepared.resolutionScheduleHash,
    ]),
  };
  push("scriptSources", witness(12, discovery));
  return { kind: "audited", ...handoff };
};
