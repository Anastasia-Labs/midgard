/**
 * The Plutus Data each script-decode state value serialises to in
 * `onchain/aiken/lib/midgard/script-decode-v1.ak`. Constructor indices follow
 * the Aiken declaration order; a change there is a change here.
 */
import {
  boolData,
  constrData,
  intData,
  optionData,
  type ScriptDecodeData,
} from "./script-decode.step-data.js";
import type {
  BlobSink,
  BytesAcc,
  BytesPurpose,
  ConstantType,
  ConstFoldNext,
  Control,
  DataMode,
  Fold,
  Frame,
  Machine,
  MagSink,
  NatPurpose,
  ScriptDecodeRecord,
  ScriptDecodeRun,
  SequenceSummary,
  Summary,
} from "./script-decode.step-types.js";

const CONSTANT_TYPE_INDEX = {
  integer: 0,
  bytestring: 1,
  string: 2,
  unit: 3,
  boolean: 4,
  list: 5,
  pair: 6,
  data: 7,
  g1: 8,
  g2: 9,
  ml: 10,
  array: 11,
  value: 12,
} as const;

const BYTES_PURPOSE_INDEX = {
  plain: 0,
  string: 1,
  currency: 2,
  token: 3,
  data: 4,
} as const;

const bytesOption = (value: Buffer | undefined): ScriptDecodeData =>
  optionData(value, (inner) => inner);

export const summaryData = (summary: Summary): ScriptDecodeData =>
  constrData(0, [
    Buffer.from(summary.root),
    summary.cborLength,
    summary.memory,
  ]);

export const sequenceData = (sequence: SequenceSummary): ScriptDecodeData =>
  constrData(0, [
    Buffer.from(sequence.root),
    sequence.length,
    sequence.payloadCborLength,
    sequence.memory,
  ]);

export const constantTypeData = (ty: ConstantType): ScriptDecodeData => {
  const index = CONSTANT_TYPE_INDEX[ty.k];
  if (ty.k === "list" || ty.k === "array") {
    return constrData(index, [constantTypeData(ty.element)]);
  }
  if (ty.k === "pair") {
    return constrData(index, [
      constantTypeData(ty.first),
      constantTypeData(ty.second),
    ]);
  }
  return constrData(index);
};

const sinkData = (sink: BlobSink): ScriptDecodeData =>
  constrData(0, [
    constrData(0, [
      intData(sink.frontier.count),
      sink.frontier.byteLength,
      sink.frontier.peaks.map((peak) =>
        constrData(0, [
          intData(peak.height),
          Buffer.from(peak.root),
          peak.byteLength,
        ]),
      ),
    ]),
    sink.pending,
  ]);

const magData = (mag: MagSink): ScriptDecodeData =>
  mag.k === "short"
    ? constrData(0, [boolData(mag.negative), mag.magnitude])
    : constrData(1, [
        boolData(mag.negative),
        sinkData(mag.sink),
        mag.part,
        intData(mag.total),
        intData(mag.first),
        boolData(mag.ones),
      ]);

export const frameData = (frame: Frame): ScriptDecodeData => {
  switch (frame.k) {
    case "delay":
      return constrData(0);
    case "lambda":
      return constrData(1);
    case "force":
      return constrData(2);
    case "applyFunction":
      return constrData(3);
    case "applyArgument":
      return constrData(4, [frame.function]);
    case "caseScrutinee":
      return constrData(5);
    case "constrFields":
      return constrData(6, [frame.tag, intData(frame.count)]);
    case "caseBranches":
      return constrData(7, [frame.scrutinee, intData(frame.count)]);
    case "termChild":
      return constrData(8, [frame.hash]);
    case "constant":
      return constrData(9, [frame.typeHash]);
    case "valueList":
      return constrData(10, [
        constantTypeData(frame.element),
        boolData(frame.array),
        intData(frame.count),
        frame.memory,
      ]);
    case "valueItem":
      return constrData(11, [summaryData(frame.summary)]);
    case "valuePairFirst":
      return constrData(12, [constantTypeData(frame.second)]);
    case "valuePairSecond":
      return constrData(13, [summaryData(frame.first), frame.memory]);
    case "assetOuter":
      return constrData(14, [
        intData(frame.count),
        bytesOption(frame.previous),
        boolData(frame.invalid),
        intData(frame.total),
      ]);
    case "assetInner":
      return constrData(15, [
        frame.currency,
        intData(frame.count),
        bytesOption(frame.previous),
        boolData(frame.invalid),
      ]);
    case "pairEntry":
      return constrData(16, [summaryData(frame.key), summaryData(frame.value)]);
    case "dataRoot":
      return constrData(17);
    case "dataList":
      return constrData(18, [
        frame.remaining,
        intData(frame.count),
        frame.constr,
        boolData(frame.breakAfter),
      ]);
    case "dataMap":
      return constrData(19, [
        frame.remaining,
        intData(frame.count),
        optionData(frame.key, summaryData),
      ]);
    case "valueRun":
      return constrData(20, [
        intData(frame.start),
        intData(frame.count),
        intData(frame.units),
      ]);
  }
};

const natPurposeData = (purpose: NatPurpose): ScriptDecodeData =>
  purpose.k === "version"
    ? constrData(0, [intData(purpose.index)])
    : purpose.k === "integer"
      ? constrData(1)
      : constrData(2, [purpose.token]);

const purposeData = (purpose: BytesPurpose): ScriptDecodeData =>
  constrData(BYTES_PURPOSE_INDEX[purpose]);

const accData = (acc: BytesAcc): ScriptDecodeData =>
  acc.k === "blob"
    ? constrData(0, [sinkData(acc.sink), intData(acc.utf8)])
    : constrData(1, [acc.key, intData(acc.length)]);

const foldData = (fold: Fold): ScriptDecodeData =>
  constrData(0, [
    boolData(fold.pairs),
    intData(fold.remaining),
    sequenceData(fold.sequence),
  ]);

const constFoldNextData = (next: ConstFoldNext): ScriptDecodeData => {
  switch (next.k) {
    case "list":
      return constrData(0, [
        constantTypeData(next.element),
        boolData(next.array),
        intData(next.count),
        next.memory,
      ]);
    case "assetInner":
      return constrData(1, [
        next.currency,
        intData(next.count),
        boolData(next.invalid),
      ]);
    case "assetOuter":
      return constrData(2, [boolData(next.invalid), intData(next.total)]);
  }
};

const dataModeData = (mode: DataMode): ScriptDecodeData => {
  switch (mode.k) {
    case "item":
      return constrData(0);
    case "bytes":
      return constrData(1, [
        sinkData(mode.sink),
        intData(mode.remaining),
        boolData(mode.indefinite),
      ]);
    case "bignum":
      return constrData(2, [
        magData(mode.magnitude),
        intData(mode.remaining),
        boolData(mode.indefinite),
      ]);
    case "fold":
      return constrData(3, [
        foldData(mode.fold),
        mode.next.k === "list"
          ? constrData(0, [mode.next.constr, boolData(mode.next.breakAfter)])
          : constrData(1),
      ]);
    case "skim":
      return constrData(4);
    case "trail":
      return constrData(5, [summaryData(mode.summary)]);
  }
};

export const controlData = (control: Control): ScriptDecodeData => {
  switch (control.k) {
    case "nat":
      return constrData(0, [
        natPurposeData(control.purpose),
        intData(control.start),
        intData(control.groups),
        control.acc,
        control.scale,
        boolData(control.big),
      ]);
    case "magnitude":
      return constrData(1, [
        intData(control.start),
        intData(control.groups),
        intData(control.emitted),
        magData(control.magnitude),
      ]);
    case "term":
      return constrData(2);
    case "return":
      return constrData(3, [control.hash]);
    case "consBit":
      return constrData(4);
    case "constrFold":
      return constrData(5, [
        control.tag,
        intData(control.remaining),
        control.root,
        intData(control.length),
      ]);
    case "caseFold":
      return constrData(6, [
        control.scrutinee,
        intData(control.remaining),
        control.root,
        intData(control.length),
      ]);
    case "types":
      return constrData(7, [
        intData(control.count),
        intData(control.pending),
        intData(control.top),
        boolData(control.bad),
        control.tags.map(intData),
      ]);
    case "value":
      return constrData(8, [constantTypeData(control.ty)]);
    case "valueReturn":
      return constrData(9, [summaryData(control.summary), control.memory]);
    case "listBit":
      return constrData(10, [constantTypeData(control.element)]);
    case "assetOuterBit":
      return constrData(11);
    case "assetInnerBit":
      return constrData(12);
    case "filler":
      return constrData(13, [purposeData(control.purpose)]);
    case "bytes":
      return constrData(14, [
        purposeData(control.purpose),
        accData(control.acc),
      ]);
    case "constFold":
      return constrData(15, [
        foldData(control.fold),
        constFoldNextData(control.next),
      ]);
    case "data":
      return constrData(16, [
        constrData(0, [
          intData(control.source.position),
          intData(control.source.left),
        ]),
        dataModeData(control.mode),
      ]);
    case "final":
      return constrData(17, [control.termRoot]);
    case "done":
      return constrData(18, [control.termRoot]);
    case "refused":
      return constrData(19, [intData(control.class), intData(control.offset)]);
  }
};

export const machineData = (machine: Machine): ScriptDecodeData =>
  constrData(0, [
    intData(machine.cursor),
    intData(machine.version),
    intData(machine.binders),
    intData(machine.constrCaseDepth),
    boolData(machine.openTerm),
    machine.stackRoot,
    controlData(machine.control),
  ]);

/** The run datum a script-decode run output carries. */
export const runData = (run: ScriptDecodeRun): ScriptDecodeData =>
  constrData(0, [
    run.scriptHash,
    intData(run.language),
    intData(run.flatStart),
    intData(run.flatLength),
    intData(run.byteLength),
    machineData(run.machine),
  ]);

/** The record datum a script-decode record output carries. */
export const recordData = (record: ScriptDecodeRecord): ScriptDecodeData =>
  record.k === "decoded"
    ? constrData(0, [
        record.scriptHash,
        intData(record.language),
        intData(record.programVersion),
        record.termRoot,
        intData(record.phase2Class),
      ])
    : constrData(1, [
        record.scriptHash,
        intData(record.language),
        intData(record.class),
        intData(record.bitOffset),
      ]);
