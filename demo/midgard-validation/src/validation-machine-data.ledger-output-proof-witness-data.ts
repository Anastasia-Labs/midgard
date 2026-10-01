import type {
  MidgardBoundedItemChunkProof,
  MidgardCekDataFrame,
  MidgardLedgerOutputProofWitness,
} from "@al-ft/midgard-core";
import { selectMidgardFieldCarriageTier } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { Constr } from "@lucid-evolution/lucid";

import type {
  MidgardCekContextPartsControl,
  MidgardCekFinalContextControl,
  MidgardCekRedeemerContextControl,
  MidgardCekTxInfoAssemblyControl,
} from "./cek-context.js";
import type {
  MidgardCekDataSequenceSummary,
  MidgardCekDataSummary,
} from "./script-context-proof.js";
import {
  type ValidationMachineFieldCarriagePlanInput,
  type ValidationMachineSignerSetProof,
} from "./validation-machine/index.js";
import {
  byteList,
  bytes,
  type ConstructorData,
  fieldCarriageData,
  frontierPeaksData,
  int,
  option,
  type PlutusData,
  record,
  ValidationMachineCarriagePreimageSubstitutedError,
  ValidationMachineCarriageTierMismatchError,
  type ValidationMachineFieldCarriageResolver,
} from "./validation-machine-data.validation-machine-carriage-tier-mismatch-error.js";

/**
 * The seam itself: resolve one step's carriage, then check the answer against
 * §8.4's partition — and, at tier 1, against the step's own bytes — before it
 * can reach `evidence_hash`.
 *
 * Every field-reading auxiliary arm goes through here rather than calling the
 * resolver directly, so the invariant is structural — a misbehaving or merely
 * stale resolver cannot put an inadmissible tier on the wire at any one of the
 * arms while the others are checked.
 *
 * The byte comparison applies to tier 1 and to nothing else: tiers 2 and 3 carry
 * reference-input indices rather than bytes, so there is no preimage here to
 * disagree with. What is behind those indices is §8.7 content addressing, which
 * the door resolves and `assertMidgardFieldCarriageResolvesAtDoorV1` guards.
 */
export const resolvedFieldCarriageData = (
  resolveFieldCarriage: ValidationMachineFieldCarriageResolver,
  planInput: ValidationMachineFieldCarriagePlanInput,
): PlutusData => {
  const carriage = resolveFieldCarriage(planInput);
  const expectedTier = selectMidgardFieldCarriageTier(
    planInput.fieldPreimage.length,
  );
  if (carriage.carriage !== expectedTier) {
    throw new ValidationMachineCarriageTierMismatchError({
      fieldIndex: planInput.fieldIndex,
      preimageLength: planInput.fieldPreimage.length,
      expectedTier,
      returnedTier: carriage.carriage,
    });
  }
  if (
    carriage.carriage === "Inline" &&
    !carriage.preimage.equals(planInput.fieldPreimage)
  ) {
    throw new ValidationMachineCarriagePreimageSubstitutedError({
      fieldIndex: planInput.fieldIndex,
      preimageLength: planInput.fieldPreimage.length,
      returnedPreimageLength: carriage.preimage.length,
    });
  }
  return fieldCarriageData(carriage);
};

export const chunkProofData = (
  proof: MidgardBoundedItemChunkProof,
): ConstructorData =>
  record([
    int(proof.version),
    int(proof.fieldIndex),
    int(proof.itemIndex),
    int(proof.totalLength),
    int(proof.chunkIndex),
    bytes(proof.chunk),
    frontierPeaksData(proof.frontier),
    byteList(proof.siblings),
  ]);

export const signerProofData = (
  proof: ValidationMachineSignerSetProof,
): ConstructorData => {
  switch (proof.kind) {
    case "none":
      return new Constr(0, []);
    case "membership":
      return new Constr(1, [
        frontierPeaksData(proof.frontier),
        int(proof.signerIndex),
        byteList(proof.siblings),
      ]);
    case "empty":
      return new Constr(2, [frontierPeaksData(proof.frontier)]);
    case "belowFirst":
      return new Constr(3, [
        frontierPeaksData(proof.frontier),
        bytes(proof.firstSignerHash),
        byteList(proof.siblings),
      ]);
    case "aboveLast":
      return new Constr(4, [
        frontierPeaksData(proof.frontier),
        bytes(proof.lastSignerHash),
        byteList(proof.siblings),
      ]);
    case "between":
      return new Constr(5, [
        frontierPeaksData(proof.frontier),
        int(proof.lowerIndex),
        bytes(proof.lowerSignerHash),
        byteList(proof.lowerSiblings),
        bytes(proof.upperSignerHash),
        byteList(proof.upperSiblings),
      ]);
  }
};

export const summaryData = (summary: MidgardCekDataSummary): ConstructorData =>
  record([bytes(summary.root), summary.cborLength, summary.memory]);

export const sequenceSummaryData = (
  summary: MidgardCekDataSequenceSummary,
): ConstructorData =>
  record([
    bytes(summary.root),
    summary.length,
    summary.payloadCborLength,
    summary.memory,
  ]);

const dataTraverseFrameData = (frame: MidgardCekDataFrame): ConstructorData => {
  const kind =
    frame.kind === "constrSmall"
      ? 0
      : frame.kind === "constrLarge"
        ? 1
        : frame.kind === "list"
          ? 2
          : 3;
  const constructor = frame.kind === "constrSmall" ? frame.constructor : 0n;
  const constructorCborRoot =
    frame.kind === "constrLarge" ? frame.constructorCborRoot : Buffer.alloc(0);
  const constructorCborLength =
    frame.kind === "constrLarge" ? frame.constructorCborLength : 0n;
  const constructorMemory =
    frame.kind === "constrLarge" ? frame.constructorMemory : 0n;
  return record([
    int(kind),
    constructor,
    bytes(constructorCborRoot),
    constructorCborLength,
    constructorMemory,
    bytes(frame.tail),
    int(frame.expectedChildren),
    int(frame.childCount),
    frontierPeaksData(frame.childFrontier),
    int(frame.foldCursor),
    sequenceSummaryData(frame.sequence),
  ]);
};

export const dataTraverseActionData = (
  action: Extract<
    MidgardLedgerOutputProofWitness,
    { readonly kind: "datum" }
  >["action"],
): ConstructorData => {
  if (action === null) return new Constr(0, []);
  switch (action.kind) {
    case "headScalar":
      return new Constr(1, [int(action.itemLength)]);
    case "headSequence":
      return new Constr(2, [int(action.expectedChildren)]);
    case "headMap":
      return new Constr(3, []);
    case "headLargeConstructor":
      return new Constr(4, [
        int(action.constructorCborLength),
        int(action.expectedChildren),
      ]);
    case "attachScalar":
      return new Constr(5, [option(action.parent, dataTraverseFrameData)]);
    case "foldList":
      return new Constr(6, [
        dataTraverseFrameData(action.frame),
        int(action.childIndex),
        summaryData(action.child),
        byteList(action.siblings),
      ]);
    case "foldMap":
      return new Constr(7, [
        dataTraverseFrameData(action.frame),
        int(action.pairIndex),
        summaryData(action.key),
        summaryData(action.value),
        byteList(action.keySiblings),
        byteList(action.valueSiblings),
      ]);
    case "finalizeFrame":
      return new Constr(8, [
        dataTraverseFrameData(action.frame),
        option(action.parent, dataTraverseFrameData),
      ]);
  }
};

export const ledgerOutputProofWitnessData = (
  witness: MidgardLedgerOutputProofWitness,
): ConstructorData => {
  if (witness === null) return new Constr(0, []);
  switch (witness.kind) {
    case "chunks":
      return new Constr(1, [
        chunkProofData(witness.chunkProof),
        option(witness.nextChunkProof, chunkProofData),
      ]);
    case "value":
      return new Constr(2, [
        int(witness.assetIndex),
        bytes(witness.policyId),
        bytes(witness.assetName),
        witness.quantity,
        byteList(witness.siblings),
        option(witness.previous, (head) =>
          record([
            bytes(head.assetName),
            head.quantity,
            sequenceSummaryData(head.tail),
          ]),
        ),
      ]);
    case "datum":
      return new Constr(3, [
        dataTraverseActionData(witness.action),
        option(witness.window, bytes),
      ]);
    case "nativeFrame":
      return new Constr(4, [
        record([
          bytes(witness.frame.tail),
          int(witness.frame.kind),
          int(witness.frame.childCount),
          int(witness.frame.remaining),
          int(witness.frame.validCount),
          witness.frame.required,
        ]),
      ]);
    case "spanAttach":
      return new Constr(5, [
        chunkProofData(witness.chunkProof),
        option(witness.nextChunkProof, chunkProofData),
      ]);
    case "window":
      return new Constr(6, [bytes(witness.bytes)]);
  }
};

export const redeemerControlData = (
  control: MidgardCekRedeemerContextControl,
): ConstructorData =>
  record([
    int(control.cursor),
    sequenceSummaryData(control.mapItems),
    bytes(control.activeScanHash),
    bytes(control.activeRedeemerLeaf),
    summaryData(control.activePurpose),
    summaryData(control.currentRedeemer),
    int(control.purposeBound),
  ]);

export const finalContextControlData = (
  control: MidgardCekFinalContextControl,
): ConstructorData =>
  record([
    summaryData(control.txInfo),
    summaryData(control.redeemer),
    summaryData(control.scriptInfo),
  ]);

export const contextPartsControlData = (
  control: MidgardCekContextPartsControl,
): ConstructorData =>
  record([
    sequenceSummaryData(control.redeemerItems),
    summaryData(control.redeemer),
    summaryData(control.scriptInfo),
  ]);

export const txInfoAssemblyControlData = (
  control: MidgardCekTxInfoAssemblyControl,
): ConstructorData =>
  record([
    sequenceSummaryData(control.tailFields),
    summaryData(control.redeemer),
    summaryData(control.scriptInfo),
  ]);
