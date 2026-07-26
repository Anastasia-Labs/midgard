import type {
  MidgardBoundedCollectionItemProofV1,
  MidgardBoundedItemChunkProofV1,
  MidgardCekDataFrameV1,
  MidgardLedgerOutputProofWitnessV1,
  MidgardMpfProofDescriptorV1,
  MidgardMpfProofFrameV1,
  MidgardMpfProofStepV1,
  MidgardValidationMachineStateV1,
  MidgardValidationMerkleFrontierV1,
  MidgardValidationMerkleMembershipV1,
  MidgardValidationPhaseName,
} from "@al-ft/midgard-core";
import {
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  MIDGARD_CONSENSUS_LIMITS_V1,
} from "@al-ft/midgard-core";
import type { MidgardVersionedScript } from "@al-ft/midgard-core/codec";
import {
  asArray,
  asBigInt,
  asBytes,
  decodeSingleCbor,
  readCborArrayHeader,
  readCborBytes,
  readCborInteger,
  readCborUnsigned,
} from "@al-ft/midgard-core/codec/cbor";
import { Constr, Data } from "@lucid-evolution/lucid";

import type {
  MidgardCekContextPartsControlV1,
  MidgardCekFinalContextControlV1,
  MidgardCekRedeemerContextControlV1,
  MidgardCekTxInfoAssemblyControlV1,
} from "./cek-context.js";
import { midgardCekCoreStepDataV1 } from "./cek-data.js";
import type {
  MidgardCekDataScanControlV1,
  MidgardCekDataScanFrameV1,
  MidgardCekDataScanStepV1,
} from "./cek-data-scan.js";
import type { DecodedMidgardRedeemer } from "./midgard-redeemers.js";
import type {
  MidgardCekDataSequenceSummaryV1,
  MidgardCekDataSummaryV1,
} from "./script-context-proof.js";
import type {
  DeterministicValidationMachineTrace,
  ValidationMachineSignerSetProof,
  ValidationMachineWorkWitness,
} from "./validation-machine.js";

type PlutusData = unknown;
type ConstructorData = Constr<PlutusData>;

const bytes = (value: Uint8Array): string =>
  Buffer.from(value).toString("hex");
const int = (value: number | bigint): bigint => BigInt(value);
const record = (fields: readonly PlutusData[]): ConstructorData =>
  new Constr(0, [...fields]);
const bool = (value: boolean): ConstructorData =>
  new Constr(value ? 1 : 0, []);
const option = <T>(
  value: T | null,
  encode: (exact: T) => PlutusData,
): ConstructorData =>
  value === null ? new Constr(1, []) : new Constr(0, [encode(value)]);
const byteList = (values: readonly Uint8Array[]): readonly string[] =>
  values.map(bytes);

const proofData = (proofCbor: Uint8Array): PlutusData =>
  Data.from(bytes(proofCbor)) as PlutusData;

const frontierPeaksData = (
  frontier: MidgardValidationMerkleFrontierV1,
): readonly ConstructorData[] =>
  frontier.peaks.map((peak) => record([int(peak.height), bytes(peak.hash)]));

const mpfProofStepData = (
  step: MidgardMpfProofStepV1,
): ConstructorData => {
  if (step.kind === "branch") {
    return new Constr(0, [int(step.skip), bytes(step.neighbors)]);
  }
  if (step.kind === "fork") {
    return new Constr(1, [
      int(step.skip),
      record([
        int(step.neighbor.nibble),
        bytes(step.neighbor.prefix),
        bytes(step.neighbor.root),
      ]),
    ]);
  }
  return new Constr(2, [
    int(step.skip),
    bytes(step.key),
    bytes(step.value),
  ]);
};

const mpfProofFrameData = (
  frame: MidgardMpfProofFrameV1,
): ConstructorData =>
  record([
    int(frame.version),
    int(frame.frameIndex),
    int(frame.cursor),
    int(frame.nextCursor),
    mpfProofStepData(frame.step),
  ]);

const mpfProofDescriptorData = (
  descriptor: MidgardMpfProofDescriptorV1,
): ConstructorData =>
  record([
    int(descriptor.version),
    int(descriptor.frameCount),
    int(descriptor.terminalCursor),
    frontierPeaksData(descriptor.frontier),
  ]);

const ledgerDeltaOperationProofData = (
  descriptor: MidgardMpfProofDescriptorV1,
  membership: MidgardValidationMerkleMembershipV1,
): ConstructorData =>
  record([
    mpfProofDescriptorData(descriptor),
    int(membership.frontier.count),
    frontierPeaksData(membership.frontier),
    int(membership.leafIndex),
    byteList(membership.siblings),
  ]);

const collectionProofData = (
  proof: MidgardBoundedCollectionItemProofV1,
): ConstructorData =>
  record([
    int(proof.version),
    int(proof.fieldIndex),
    int(proof.itemCount),
    int(proof.itemIndex),
    int(proof.itemLength),
    bytes(proof.itemCommitment),
    frontierPeaksData(proof.frontier),
    byteList(proof.siblings),
  ]);

const chunkProofData = (
  proof: MidgardBoundedItemChunkProofV1,
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

const signerProofData = (
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

const scriptData = (script: MidgardVersionedScript): ConstructorData => {
  const language =
    script.language === "NativeCardano"
      ? 0
      : script.language === "PlutusV3"
        ? 1
        : 2;
  return record([
    new Constr(language, []),
    bytes(script.scriptBytes),
  ]);
};

const redeemerData = (
  redeemer: DecodedMidgardRedeemer,
): ConstructorData =>
  record([
    new Constr(redeemer.tag, []),
    redeemer.index,
    redeemer.dataCborHex,
    record([redeemer.exUnits.memory, redeemer.exUnits.steps]),
  ]);

const summaryData = (summary: MidgardCekDataSummaryV1): ConstructorData =>
  record([
    bytes(summary.root),
    summary.cborLength,
    summary.memory,
  ]);

const sequenceSummaryData = (
  summary: MidgardCekDataSequenceSummaryV1,
): ConstructorData =>
  record([
    bytes(summary.root),
    summary.length,
    summary.payloadCborLength,
    summary.memory,
  ]);

const dataTraverseFrameData = (
  frame: MidgardCekDataFrameV1,
): ConstructorData => {
  const kind =
    frame.kind === "constrSmall"
      ? 0
      : frame.kind === "constrLarge"
        ? 1
        : frame.kind === "list"
          ? 2
          : 3;
  const constructor =
    frame.kind === "constrSmall" ? frame.constructor : 0n;
  const constructorCborRoot =
    frame.kind === "constrLarge"
      ? frame.constructorCborRoot
      : Buffer.alloc(0);
  const constructorCborLength =
    frame.kind === "constrLarge"
      ? frame.constructorCborLength
      : 0n;
  const constructorMemory =
    frame.kind === "constrLarge"
      ? frame.constructorMemory
      : 0n;
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

const dataTraverseActionData = (
  action: Extract<
    MidgardLedgerOutputProofWitnessV1,
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
      return new Constr(5, [
        option(action.parent, dataTraverseFrameData),
      ]);
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

const ledgerOutputProofWitnessData = (
  witness: MidgardLedgerOutputProofWitnessV1,
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
        bytes(witness.policyId),
        bytes(witness.assetName),
        witness.quantity,
        byteList(witness.siblings),
      ]);
    case "datum":
      return new Constr(3, [
        dataTraverseActionData(witness.action),
        option(witness.chunkProof, chunkProofData),
        option(witness.nextChunkProof, chunkProofData),
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
  }
};

const redeemerControlData = (
  control: MidgardCekRedeemerContextControlV1,
): ConstructorData =>
  record([
    int(control.cursor),
    sequenceSummaryData(control.mapItems),
    bytes(control.activeScanHash),
    bytes(control.activeRedeemerLeaf),
    summaryData(control.activePurpose),
    summaryData(control.currentRedeemer),
  ]);

const finalContextControlData = (
  control: MidgardCekFinalContextControlV1,
): ConstructorData =>
  record([
    summaryData(control.txInfo),
    summaryData(control.redeemer),
    summaryData(control.scriptInfo),
  ]);

const contextPartsControlData = (
  control: MidgardCekContextPartsControlV1,
): ConstructorData =>
  record([
    sequenceSummaryData(control.redeemerItems),
    summaryData(control.redeemer),
    summaryData(control.scriptInfo),
  ]);

const txInfoAssemblyControlData = (
  control: MidgardCekTxInfoAssemblyControlV1,
): ConstructorData =>
  record([
    sequenceSummaryData(control.tailFields),
    summaryData(control.redeemer),
    summaryData(control.scriptInfo),
  ]);

const dataScanFrameData = (
  frame: MidgardCekDataScanFrameV1,
): ConstructorData =>
  record([
    int(frame.kind),
    frame.constructor,
    bytes(frame.tail),
    int(frame.expectedChildren),
    int(frame.childCount),
    frontierPeaksData(frame.childFrontier),
    int(frame.foldCursor),
    sequenceSummaryData(frame.sequence),
  ]);

const dataScanControlData = (
  control: MidgardCekDataScanControlV1,
): ConstructorData =>
  record([
    bytes(control.rawHash),
    int(control.rawLength),
    int(control.offset),
    bytes(control.frameRoot),
    bool(control.frameClosed),
    summaryData(
      control.result ?? {
        root: Buffer.alloc(0),
        cborLength: 0n,
        memory: 0n,
      },
    ),
  ]);

const dataScanStepData = (
  step: MidgardCekDataScanStepV1,
): ConstructorData => {
  const parent = (value: MidgardCekDataScanFrameV1 | null) =>
    option(value, dataScanFrameData);
  switch (step.kind) {
    case "openConstructor":
      return new Constr(0, [
        bytes(step.rawCbor),
        parent(step.parent),
        step.constructor,
        int(step.expectedChildren),
      ]);
    case "openList":
      return new Constr(1, [
        bytes(step.rawCbor),
        parent(step.parent),
        int(step.expectedChildren),
      ]);
    case "openMap":
      return new Constr(2, [
        bytes(step.rawCbor),
        parent(step.parent),
      ]);
    case "revealLeaf":
      return new Constr(3, [
        bytes(step.rawCbor),
        parent(step.parent),
        int(step.itemLength),
      ]);
    case "closeSequence":
      return new Constr(4, [
        bytes(step.rawCbor),
        dataScanFrameData(step.frame),
      ]);
    case "foldList":
      return new Constr(5, [
        dataScanFrameData(step.frame),
        int(step.childIndex),
        summaryData(step.child),
        byteList(step.siblings),
      ]);
    case "foldMap":
      return new Constr(6, [
        dataScanFrameData(step.frame),
        int(step.pairIndex),
        summaryData(step.key),
        summaryData(step.value),
        byteList(step.keySiblings),
        byteList(step.valueSiblings),
      ]);
    case "finalizeFrame":
      return new Constr(7, [
        dataScanFrameData(step.frame),
        parent(step.parent),
      ]);
  }
};

const valueMutationData = (
  mutation: Extract<
    NonNullable<ValidationMachineWorkWitness["auxiliary"]>,
    {
      readonly kind:
        | "valueInputAsset"
        | "valueOutputAsset"
        | "valueMintAsset";
    }
  >["mutationStep"],
): ConstructorData =>
  record([
    bool(mutation.oldDelta !== null),
    mutation.oldDelta ?? 0n,
    proofData(mutation.proofCbor),
  ]);

const sourceKind = (kind: "spend" | "reference"): bigint =>
  kind === "spend" ? 0n : 1n;
const originKind = (kind: "inline" | "reference"): bigint =>
  kind === "inline" ? 0n : 1n;

const resolverPhaseIndex = (
  phase: MidgardValidationPhaseName,
): number => {
  const index = {
    canonicalDecode: 0,
    compactBinding: 1,
    staticLedgerRules: 2,
    inputSets: 3,
    signatures: 4,
    phaseANativeScripts: 5,
    phaseAScriptPreconditions: 6,
    resolveInputs: 7,
    scriptSources: 8,
    nativeScripts: 9,
    scriptIntegrity: 10,
    cek: 11,
    valueAndMint: 12,
    ledgerDelta: 13,
    terminal: -1,
  }[phase];
  if (index < 0) {
    throw new Error(`validation phase ${phase} has no resolver`);
  }
  return index;
};

const scanStage = (
  witness: ValidationMachineWorkWitness,
  label: string,
): number => {
  const outer = readCborArrayHeader(witness.cbor, 0, label);
  if (outer.length < 6) {
    throw new Error(`${label} control has too few fields`);
  }
  let offset = outer.nextOffset;
  for (let index = 0; index < 5; index += 1) {
    offset = readCborBytes(
      witness.cbor,
      offset,
      `${label}.binding_${index.toString()}`,
    ).nextOffset;
  }
  const stage = readCborInteger(
    witness.cbor,
    offset,
    `${label}.stage`,
  ).value;
  const exact = Number(stage);
  if (!Number.isSafeInteger(exact) || exact < 0) {
    throw new Error(`${label} stage is invalid`);
  }
  return exact;
};

const scriptSourcesStage = (
  witness: ValidationMachineWorkWitness,
): number => {
  const control = asArray(
    decodeSingleCbor(witness.cbor),
    "script_sources_control",
  );
  if (control.length !== 28 && control.length !== 29) {
    throw new Error("script_sources_control has an invalid field count");
  }
  const stage = Number(
    asBigInt(control[9], "script_sources_control.stage"),
  );
  if (!Number.isSafeInteger(stage) || stage < 0) {
    throw new Error("script_sources_control stage is invalid");
  }
  return stage;
};

const resolveInputsCursor = (
  witness: ValidationMachineWorkWitness,
): number => {
  const control = asArray(
    decodeSingleCbor(witness.cbor),
    "resolve_inputs_control",
  );
  if (control.length !== 10) {
    throw new Error("resolve_inputs_control has an invalid field count");
  }
  const cursor = Number(
    asBigInt(control[4], "resolve_inputs_control.cursor"),
  );
  if (!Number.isSafeInteger(cursor) || cursor < 0) {
    throw new Error("resolve_inputs_control cursor is invalid");
  }
  return cursor;
};

const ledgerDeltaControlStatus = (
  witness: ValidationMachineWorkWitness,
): {
  readonly stage: number;
  readonly pendingStage: number | null;
} => {
  const control = asArray(
    decodeSingleCbor(witness.cbor),
    "ledger_delta_control",
  );
  if (control.length !== 14) {
    throw new Error("ledger_delta_control has an invalid field count");
  }
  const stage = Number(asBigInt(control[4], "ledger_delta_control.stage"));
  if (
    !Number.isSafeInteger(stage) ||
    stage < 0 ||
    stage > 2
  ) {
    throw new Error("ledger_delta_control stage is invalid");
  }
  const pendingCbor = asBytes(
    control[12],
    "ledger_delta_control.pending_mutation",
  );
  if (pendingCbor.length === 0) {
    return { stage, pendingStage: null };
  }
  const pending = asArray(
    decodeSingleCbor(pendingCbor),
    "ledger_delta_pending_mutation",
  );
  if (
    pending.length !== 10 ||
    asBigInt(pending[0], "ledger_delta_pending_mutation.version") !== 1n
  ) {
    throw new Error("ledger_delta pending mutation is invalid");
  }
  const pendingStage = Number(
    asBigInt(pending[1], "ledger_delta_pending_mutation.stage"),
  );
  if (pendingStage !== 0 && pendingStage !== 1) {
    throw new Error("ledger_delta pending mutation stage is invalid");
  }
  return { stage, pendingStage };
};

const nativeScanCursor = (
  witness: ValidationMachineWorkWitness,
): {
  readonly stage: number;
  readonly cursor: number;
} => {
  const outer = readCborArrayHeader(
    witness.cbor,
    0,
    "phase_a_native_control",
  );
  if (outer.length !== 18) {
    throw new Error("phase-A native control has an invalid field count");
  }
  let offset = outer.nextOffset;
  for (let index = 0; index < 5; index += 1) {
    offset = readCborBytes(
      witness.cbor,
      offset,
      `phase_a_native_control.binding_${index.toString()}`,
    ).nextOffset;
  }
  const stage = readCborInteger(
    witness.cbor,
    offset,
    "phase_a_native_control.stage",
  );
  offset = stage.nextOffset;
  for (let index = 0; index < 4; index += 1) {
    offset = readCborInteger(
      witness.cbor,
      offset,
      `phase_a_native_control.count_${index.toString()}`,
    ).nextOffset;
  }
  offset = readCborBytes(
    witness.cbor,
    offset,
    "phase_a_native_control.item_commitment",
  ).nextOffset;
  const cursor = readCborInteger(
    witness.cbor,
    offset,
    "phase_a_native_control.cursor",
  ).value;
  const exactStage = Number(stage.value);
  const exactCursor = Number(cursor);
  if (
    !Number.isSafeInteger(exactStage) ||
    exactStage < 0 ||
    !Number.isSafeInteger(exactCursor) ||
    exactCursor < 0
  ) {
    throw new Error("phase-A native control stage or cursor is invalid");
  }
  return { stage: exactStage, cursor: exactCursor };
};

const nativePayloadChildCount = ({
  witness,
  cursor,
  stage,
}: {
  readonly witness: Extract<
    NonNullable<ValidationMachineWorkWitness["auxiliary"]>,
    { readonly kind: "nativeScriptToken" }
  >;
  readonly cursor: number;
  readonly stage: number;
}): number => {
  const expectedChunkIndex = Math.floor(
    cursor / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  );
  if (witness.chunkProof.chunkIndex !== expectedChunkIndex) {
    throw new Error(
      "phase-A native token proof does not cover the committed cursor",
    );
  }
  const window = Buffer.concat([
    witness.chunkProof.chunk,
    witness.nextChunkProof?.chunk ?? Buffer.alloc(0),
  ]);
  let offset =
    cursor -
    expectedChunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1;
  if (stage === 6) {
    offset = readCborUnsigned(
      window,
      offset,
      "phase_a_native_payload.required",
    ).nextOffset;
  }
  const children = readCborArrayHeader(
    window,
    offset,
    "phase_a_native_payload.children",
  );
  return children.length;
};

export const validationSemanticResolverIndexV1 = (
  witness: ValidationMachineWorkWitness,
): number | null => {
  const auxiliary = witness.auxiliary;
  switch (witness.phase) {
    case "canonicalDecode":
      if (auxiliary === null) return 0;
      if (auxiliary.kind === "transactionFieldChunk") return 1;
      break;
    case "compactBinding":
    case "staticLedgerRules":
      if (auxiliary === null) return 0;
      break;
    case "inputSets":
      if (auxiliary === null) return 0;
      if (auxiliary.kind === "transactionFieldChunk") return 1;
      break;
    case "signatures":
      if (auxiliary === null) {
        return scanStage(witness, "signatures_control") === 2 ? 3 : 0;
      }
      if (auxiliary.kind === "transactionFieldChunk") return 1;
      if (auxiliary.kind === "requiredSignerItem") return 2;
      break;
    case "phaseANativeScripts": {
      if (auxiliary === null) return 0;
      if (auxiliary.kind === "transactionFieldChunk") return 1;
      if (auxiliary.kind === "nativeScriptFrame") return 13;
      if (auxiliary.kind !== "nativeScriptToken") break;
      const { stage, cursor } = nativeScanCursor(witness);
      if (stage === 1) return 2;
      if (stage === 3) {
        return {
          none: -1,
          membership: 8,
          empty: 9,
          belowFirst: 10,
          aboveLast: 11,
          between: 12,
        }[auxiliary.signerProof.kind];
      }
      if (stage === 4 || stage === 5) {
        return nativePayloadChildCount({
          witness: auxiliary,
          cursor,
          stage,
        }) > 0
          ? 3
          : 4;
      }
      if (stage === 6) {
        return nativePayloadChildCount({
          witness: auxiliary,
          cursor,
          stage,
        }) > 0
          ? 5
          : 6;
      }
      if (stage === 7 || stage === 8) return 7;
      break;
    }
    case "phaseAScriptPreconditions":
      if (auxiliary === null) return 0;
      if (auxiliary.kind === "transactionFieldChunk") return 1;
      break;
    case "resolveInputs":
      if (auxiliary === null) {
        return resolveInputsCursor(witness) === 0 ? 0 : 1;
      }
      if (
        auxiliary.kind === "scheduledLedgerLookup"
      ) {
        return auxiliary.value === null ? 5 : 2;
      }
      if (auxiliary.kind === "ledgerOutputProofStep") return 3;
      if (auxiliary.kind === "ledgerOutputProofFinalize") return 4;
      break;
    case "scriptSources": {
      if (scriptSourcesStage(witness) !== 5) return 0;
      if (auxiliary?.kind === "ledgerOutputProofBegin") return 1;
      if (auxiliary?.kind === "ledgerOutputProofStep") return 2;
      if (auxiliary?.kind === "ledgerOutputProofFinalize") return 3;
      if (auxiliary === null) return 4;
      break;
    }
    case "ledgerDelta": {
      const control = ledgerDeltaControlStatus(witness);
      if (auxiliary === null) {
        if (control.pendingStage === 1) return 6;
        if (control.pendingStage === 0) break;
        if (control.stage === 0) return 2;
        if (control.stage === 1) return 4;
        return 7;
      }
      if (auxiliary.kind === "ledgerDeltaOperation") return 0;
      if (auxiliary.kind === "ledgerDeltaReplay") return 1;
      if (auxiliary.kind === "ledgerDeltaOutput") return 3;
      if (auxiliary.kind === "ledgerDeltaProofFrame") return 5;
      break;
    }
    case "nativeScripts":
    case "scriptIntegrity":
    case "cek":
    case "valueAndMint":
      return null;
    case "terminal":
      break;
  }
  throw new Error(
    `validation evidence ${witness.phase}/${auxiliary?.kind ?? "none"} has no semantic resolver`,
  );
};

export type ValidationOneStepArgumentV1 = {
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number | null;
  readonly transitionCbor: Buffer;
  readonly auxiliaryCbor: Buffer;
  readonly evidenceCbor: Buffer;
};

export const validationMachineStateDataV1 = (
  state: MidgardValidationMachineStateV1,
): ConstructorData =>
  record([
    int(state.machineVersion),
    bytes(state.eventKeyHash),
    bytes(state.transactionId),
    bytes(state.transactionCommitment),
    bytes(state.validationContextHash),
    new Constr(state.sourceKind === "normal" ? 0 : 1, []),
    bytes(state.priorLedgerRoot),
    new Constr(
      {
        canonicalDecode: 0,
        compactBinding: 1,
        staticLedgerRules: 2,
        inputSets: 3,
        signatures: 4,
        phaseANativeScripts: 5,
        phaseAScriptPreconditions: 6,
        resolveInputs: 7,
        scriptSources: 8,
        nativeScripts: 9,
        scriptIntegrity: 10,
        cek: 11,
        valueAndMint: 12,
        ledgerDelta: 13,
        terminal: 14,
      }[state.phase],
      [],
    ),
    int(state.programCounter),
    bytes(state.workRoot),
    state.executionCpu,
    state.executionMemory,
    new Constr(
      state.verdict === "pending"
        ? 0
        : state.verdict === "accepted"
          ? 1
          : 2,
      [],
    ),
    bytes(state.rejectionCodeHash),
    bytes(state.ledgerDeltaRoot),
  ]);

export const validationOneStepWitnessDataV1 = ({
  witness,
  claimedSuccessor,
}: {
  readonly witness: ValidationMachineWorkWitness;
  readonly claimedSuccessor: MidgardValidationMachineStateV1;
}): ConstructorData =>
  record([
    bytes(witness.cbor),
    validationMachineStateDataV1(claimedSuccessor),
  ]);

export const validationAuxiliaryWitnessDataV1 = (
  auxiliary: ValidationMachineWorkWitness["auxiliary"],
): PlutusData => {
  if (auxiliary === null) return new Constr(0, []);
  switch (auxiliary.kind) {
    case "transactionFieldPreimage":
      return new Constr(1, [bytes(auxiliary.preimageCbor)]);
    case "transactionFieldChunk":
      return new Constr(2, [
        collectionProofData(auxiliary.collectionProof),
        chunkProofData(auxiliary.chunkProof),
      ]);
    case "requiredSignerItem":
      return new Constr(3, [
        collectionProofData(auxiliary.collectionProof),
        chunkProofData(auxiliary.chunkProof),
        signerProofData(auxiliary.signerProof),
      ]);
    case "nativeScriptToken":
      return new Constr(4, [
        chunkProofData(auxiliary.chunkProof),
        option(auxiliary.nextChunkProof, chunkProofData),
        signerProofData(auxiliary.signerProof),
      ]);
    case "nativeScriptFrame":
      return new Constr(5, [
        record([
          bytes(auxiliary.frame.tail),
          int(auxiliary.frame.kind),
          int(auxiliary.frame.childCount),
          int(auxiliary.frame.remaining),
          int(auxiliary.frame.validCount),
          auxiliary.frame.required,
        ]),
      ]);
    case "transactionFieldPairPreimage":
      return new Constr(6, [
        int(auxiliary.firstFieldIndex),
        bytes(auxiliary.firstPreimageCbor),
        int(auxiliary.secondFieldIndex),
        bytes(auxiliary.secondPreimageCbor),
      ]);
    case "scheduledLedgerLookup": {
      const fields = [
        sourceKind(auxiliary.sourceKind),
        bytes(auxiliary.key),
        bytes(auxiliary.nextScheduleHash),
      ];
      return auxiliary.value === null
        ? new Constr(10, [
            ...fields,
            proofData(auxiliary.proofCbor),
          ])
        : new Constr(9, [
            ...fields,
            bytes(auxiliary.value),
            proofData(auxiliary.proofCbor),
            signerProofData(auxiliary.signerProof),
          ]);
    }
    case "resolvedInputReplay":
      return new Constr(11, [
        sourceKind(auxiliary.sourceKind),
        bytes(auxiliary.key),
        bytes(auxiliary.nextScheduleHash),
        bytes(auxiliary.value),
      ]);
    case "outputReplay":
      return new Constr(12, [
        int(auxiliary.outputIndex),
        bytes(auxiliary.outputCbor),
        byteList(auxiliary.siblings),
        signerProofData(auxiliary.signerProof),
      ]);
    case "scriptPurposeScan":
      return new Constr(13, [
        int(auxiliary.purposeKind),
        auxiliary.purposeIndex,
        bytes(auxiliary.scriptHash),
        bytes(auxiliary.subject),
        byteList(auxiliary.siblings),
      ]);
    case "scriptSourceScan":
      return new Constr(14, [
        int(auxiliary.sourceIndex),
        originKind(auxiliary.originKind),
        bytes(auxiliary.sourceKey),
        scriptData(auxiliary.script),
        byteList(auxiliary.siblings),
      ]);
    case "redeemerScan":
      return new Constr(15, [
        int(auxiliary.redeemerIndex),
        redeemerData(auxiliary.redeemer),
        byteList(auxiliary.siblings),
      ]);
    case "nativeExecutionScan":
      return new Constr(16, [
        int(auxiliary.executionIndex),
        int(auxiliary.languageTag),
        int(auxiliary.purpose.purposeKind),
        auxiliary.purpose.purposeIndex,
        bytes(auxiliary.purpose.scriptHash),
        bytes(auxiliary.purpose.subject),
        byteList(auxiliary.purpose.siblings),
        int(auxiliary.source.sourceIndex),
        originKind(auxiliary.source.originKind),
        bytes(auxiliary.source.sourceKey),
        scriptData(auxiliary.source.script),
        byteList(auxiliary.source.siblings),
        bytes(auxiliary.redeemerLeaf),
        byteList(auxiliary.executionSiblings),
        byteList(auxiliary.signerHashes),
      ]);
    case "cekCoreStep":
      return new Constr(17, [midgardCekCoreStepDataV1(auxiliary.step)]);
    case "cekResolvedContextItem":
      return new Constr(18, [
        sourceKind(auxiliary.sourceKind),
        int(auxiliary.itemIndex),
        bytes(auxiliary.key),
        bytes(auxiliary.value),
        byteList(auxiliary.siblings),
      ]);
    case "cekOutputContextItem":
      return new Constr(19, [
        int(auxiliary.outputIndex),
        bytes(auxiliary.outputCbor),
        byteList(auxiliary.siblings),
      ]);
    case "cekSignerContextItem":
      return new Constr(20, [
        frontierPeaksData(auxiliary.frontier),
        int(auxiliary.signerIndex),
        bytes(auxiliary.signerHash),
        byteList(auxiliary.siblings),
      ]);
    case "cekMintContextItem":
      return new Constr(21, [
        int(auxiliary.mintIndex),
        bytes(auxiliary.policyId),
        bytes(auxiliary.assetName),
        auxiliary.quantity,
        byteList(auxiliary.siblings),
      ]);
    case "cekRedeemerContextSelect":
      return new Constr(22, [
        redeemerControlData(auxiliary.control),
        int(auxiliary.redeemerIndex),
        redeemerData(auxiliary.redeemer),
        byteList(auxiliary.redeemerSiblings),
        int(auxiliary.purposeFrontierIndex),
        int(auxiliary.purpose.purposeKind),
        auxiliary.purpose.purposeIndex,
        bytes(auxiliary.purpose.scriptHash),
        bytes(auxiliary.purpose.subject),
        byteList(auxiliary.purpose.siblings),
      ]);
    case "cekDataScanStep":
      return new Constr(23, [
        redeemerControlData(auxiliary.redeemerControl),
        dataScanControlData(auxiliary.control),
        dataScanStepData(auxiliary.step),
      ]);
    case "cekContextFinalize":
      return new Constr(24, [
        redeemerControlData(auxiliary.redeemerControl),
      ]);
    case "cekContextFinalizeSpend":
      return new Constr(25, [
        redeemerControlData(auxiliary.redeemerControl),
        int(auxiliary.itemIndex),
        bytes(auxiliary.key),
        bytes(auxiliary.value),
        byteList(auxiliary.siblings),
      ]);
    case "cekContextAssemble":
      return new Constr(26, [
        contextPartsControlData(auxiliary.control),
      ]);
    case "cekTxInfoFinalize":
      return new Constr(27, [
        txInfoAssemblyControlData(auxiliary.control),
      ]);
    case "cekContextSeed":
      return new Constr(28, [
        finalContextControlData(auxiliary.control),
      ]);
    case "valueInputAsset":
      return new Constr(29, [
        sourceKind(auxiliary.sourceKind),
        bytes(auxiliary.key),
        bytes(auxiliary.nextScheduleHash),
        bytes(auxiliary.value),
        int(auxiliary.assetIndex),
        valueMutationData(auxiliary.mutationStep),
      ]);
    case "valueOutputAsset":
      return new Constr(30, [
        int(auxiliary.outputIndex),
        bytes(auxiliary.outputCbor),
        byteList(auxiliary.siblings),
        int(auxiliary.assetIndex),
        valueMutationData(auxiliary.mutationStep),
      ]);
    case "valueMintAsset":
      return new Constr(31, [
        int(auxiliary.mintIndex),
        bytes(auxiliary.policyId),
        bytes(auxiliary.assetName),
        auxiliary.quantity,
        byteList(auxiliary.siblings),
        valueMutationData(auxiliary.mutationStep),
      ]);
    case "ledgerDeltaOperation":
      return new Constr(40, [
        int(auxiliary.operationKind === "delete" ? 0 : 1),
        bytes(auxiliary.key),
        bytes(auxiliary.value),
        ledgerDeltaOperationProofData(
          auxiliary.mutationStep.proofFoldTrace.descriptor,
          auxiliary.operationMembership,
        ),
      ]);
    case "ledgerDeltaReplay":
      return new Constr(32, [
        sourceKind(auxiliary.sourceKind),
        bytes(auxiliary.key),
        bytes(auxiliary.nextScheduleHash),
        bytes(auxiliary.value),
      ]);
    case "ledgerDeltaOutput":
      return new Constr(33, [
        int(auxiliary.outputIndex),
        bytes(auxiliary.descriptorCbor),
        byteList(auxiliary.siblings),
      ]);
    case "ledgerDeltaProofFrame":
      return new Constr(39, [
        mpfProofFrameData(auxiliary.frame),
        byteList(auxiliary.siblings),
      ]);
    case "transactionRedeemerItem":
      return new Constr(34, [
        collectionProofData(auxiliary.collectionProof),
        redeemerData(auxiliary.redeemer),
      ]);
    case "transactionFieldItem":
      return new Constr(35, [
        collectionProofData(auxiliary.collectionProof),
      ]);
    case "ledgerOutputProofBegin":
      return new Constr(36, [
        int(auxiliary.outputIndex),
        int(auxiliary.totalLength),
        bytes(auxiliary.itemCommitment),
        byteList(auxiliary.siblings),
      ]);
    case "ledgerOutputProofStep":
      return new Constr(37, [
        ledgerOutputProofWitnessData(auxiliary.witness),
      ]);
    case "ledgerOutputProofFinalize":
      return new Constr(38, [
        bytes(auxiliary.descriptorCbor),
        signerProofData(auxiliary.signerProof),
      ]);
  }
};

export const encodeValidationOneStepWitnessCborV1 = (input: {
  readonly witness: ValidationMachineWorkWitness;
  readonly claimedSuccessor: MidgardValidationMachineStateV1;
}): Buffer =>
  Buffer.from(
    Data.to(validationOneStepWitnessDataV1(input) as never),
    "hex",
  );

export const encodeValidationAuxiliaryWitnessCborV1 = (
  auxiliary: ValidationMachineWorkWitness["auxiliary"],
): Buffer =>
  Buffer.from(
    Data.to(validationAuxiliaryWitnessDataV1(auxiliary) as never),
    "hex",
  );

export const buildValidationOneStepArgumentV1 = ({
  trace,
  stateIndex,
}: {
  readonly trace: DeterministicValidationMachineTrace;
  readonly stateIndex: number;
}): ValidationOneStepArgumentV1 => {
  if (!Number.isSafeInteger(stateIndex) || stateIndex < 0) {
    throw new Error(
      "validation one-step state index must be a non-negative safe integer",
    );
  }
  const pre = trace.states[stateIndex];
  const claimedSuccessor = trace.states[stateIndex + 1];
  const witness = trace.witnesses[stateIndex];
  if (
    pre === undefined ||
    claimedSuccessor === undefined ||
    witness === undefined
  ) {
    throw new Error(
      `validation trace does not contain transition ${stateIndex.toString()}`,
    );
  }
  if (
    witness.phase !== pre.phase ||
    witness.programCounter !== pre.programCounter ||
    claimedSuccessor.programCounter !== pre.programCounter + 1
  ) {
    throw new Error(
      "validation one-step witness is not aligned with its trace states",
    );
  }
  const transitionData = validationOneStepWitnessDataV1({
    witness,
    claimedSuccessor,
  });
  const auxiliaryData = validationAuxiliaryWitnessDataV1(
    witness.auxiliary,
  );
  const transitionCbor = Buffer.from(
    Data.to(transitionData as never),
    "hex",
  );
  const auxiliaryCbor = Buffer.from(
    Data.to(auxiliaryData as never),
    "hex",
  );
  const evidenceCbor = Buffer.from(
    Data.to(record([transitionData, auxiliaryData]) as never),
    "hex",
  );
  const maximum =
    MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes;
  if (
    transitionCbor.length >= maximum ||
    auxiliaryCbor.length >= maximum ||
    evidenceCbor.length >= maximum
  ) {
    throw new Error(
      `validation transition ${stateIndex.toString()} exceeds the strict L1 preimage envelope`,
    );
  }
  return {
    resolverIndex: resolverPhaseIndex(pre.phase),
    semanticResolverIndex:
      validationSemanticResolverIndexV1(witness),
    transitionCbor,
    auxiliaryCbor,
    evidenceCbor,
  };
};
