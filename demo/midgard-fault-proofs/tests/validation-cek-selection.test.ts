import {
  buildMidgardValidationTraceTree,
  encodeCbor,
  hashMidgardValidationMachineState,
  hashMidgardValidationRejectionReasonV1,
  hashMidgardValidationWorkWitness,
  MIDGARD_VALIDATION_MACHINE_VERSION,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardValidationMachineState,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CekDecodeRecordRequiredError,
  type DeterministicValidationMachineTrace,
  encodeMidgardCekValidationWitness,
  encodeValidationTerminalWitnessCbor,
  initialMidgardCekContextControl,
  type ValidationMachineWorkWitness,
} from "@al-ft/midgard-validation";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { buildValidationCekExecutionSelectionAction } from "../src/validation-cek-selection.js";

const hash = Buffer.alloc(28, 1);
const termRoot = Buffer.alloc(32, 2);
const policy = "ab".repeat(28);
const unit = policy + hash.toString("hex");
const utxo = (byte: string): UTxO => ({
  txHash: byte.repeat(32),
  outputIndex: 0,
  address: "unused in unit boundary",
  assets: { lovelace: 2_000_000n },
});

const fixture = (
  phase2Class: 0 | 5 | 11 = 0,
  languageTag: 0 | 3 | 128 = 3,
  purposeKind: 0 | 1 | 2 | 3 = 0,
) => {
  const nativeFields: unknown[] = Array.from({ length: 25 }, () => 0n);
  nativeFields[20] = 1n;
  const nativeControlCbor = encodeCbor(nativeFields);
  const selectionFields = {
    nativeControlCbor,
    executionCursor: 0,
    completedCpu: 7n,
    completedMemory: 9n,
    activeStateHash: null,
    executionCpuLimit: 0n,
    executionMemoryLimit: 0n,
    purposeKind: -1n,
    purposeIndex: -1n,
  };
  const sourceBytes = encodeCbor([BigInt(languageTag), Buffer.from([0])]);
  const witness: ValidationMachineWorkWitness = {
    phase: "cek",
    programCounter: 0,
    cbor: encodeMidgardCekValidationWitness({
      ...selectionFields,
      contextControl: null,
      programScriptHash: null,
    }),
    auxiliary: {
      kind: "nativeExecutionScan",
      phase2Class,
      executionIndex: 0,
      languageTag,
      purpose: {
        purposeKind,
        purposeIndex: 2n,
        scriptHash: hash,
        subject: Buffer.alloc(0),
        siblings: [],
      },
      source: {
        sourceIndex: 0,
        originKind: "inline",
        sourceKey: encodeCbor(0n),
        scriptTotalLength: sourceBytes.length,
        scriptItemCommitment: Buffer.alloc(32, 3),
        siblings: [],
      },
      redeemerLeaf: Buffer.alloc(32, 4),
      executionSiblings: [],
      firstChunkProof: {
        version: 1,
        fieldIndex: 6,
        itemIndex: 0,
        totalLength: sourceBytes.length,
        chunkIndex: 0,
        chunk: sourceBytes,
        frontier: { count: 0, peaks: [] },
        siblings: [],
      },
    },
  };
  const pre: MidgardValidationMachineState = {
    machineVersion: MIDGARD_VALIDATION_MACHINE_VERSION,
    eventKeyHash: Buffer.alloc(32, 5),
    transactionId: Buffer.alloc(32, 6),
    transactionCommitment: Buffer.alloc(32, 7),
    validationContextHash: Buffer.alloc(32, 8),
    sourceKind: "normal",
    priorLedgerRoot: Buffer.alloc(32, 9),
    phase: "cek",
    programCounter: 0,
    workRoot: hashMidgardValidationWorkWitness({
      phase: "cek",
      programCounter: 0,
      witnessCbor: witness.cbor,
    }),
    executionCpu: 7n,
    executionMemory: 9n,
    verdict: "pending",
    rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  };
  const reason = {
    PlutusExecutionFailed: {
      purpose_kind: BigInt(purposeKind),
      purpose_index: 2n,
    },
  };
  const nextCbor =
    phase2Class === 0
      ? encodeMidgardCekValidationWitness({
          ...selectionFields,
          programScriptHash: hash,
          contextControl: initialMidgardCekContextControl({
            languageTag: languageTag === 128 ? 128 : 3,
            programTermRoot: termRoot,
            purposeKind,
            purposeIndex: 2n,
            scriptHash: hash,
            subject: Buffer.alloc(0),
            redeemerLeaf: Buffer.alloc(32, 4),
          }),
        })
      : encodeValidationTerminalWitnessCbor({
          verdict: "rejected",
          reason,
          priorLedgerRoot: pre.priorLedgerRoot,
        });
  const post: MidgardValidationMachineState = {
    ...pre,
    phase: phase2Class === 0 ? "cek" : "terminal",
    programCounter: 1,
    workRoot: hashMidgardValidationWorkWitness({
      phase: phase2Class === 0 ? "cek" : "terminal",
      programCounter: 1,
      witnessCbor: nextCbor,
    }),
    verdict: phase2Class === 0 ? "pending" : "rejected",
    rejectionCodeHash:
      phase2Class === 0
        ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
        : hashMidgardValidationRejectionReasonV1(reason),
  };
  const states = [pre, post];
  const trace: DeterministicValidationMachineTrace = {
    validationContextCbor: Buffer.from([0]),
    states,
    witnesses: [
      witness,
      { phase: post.phase, programCounter: 1, cbor: nextCbor, auxiliary: null },
    ],
    tree: buildMidgardValidationTraceTree(
      states.map(hashMidgardValidationMachineState),
      phase2Class === 0 ? "accepted" : "rejected",
      post.rejectionCodeHash,
    ),
    verdict: phase2Class === 0 ? "accepted" : "rejected",
    rejectionCode: null,
    rejectionReason: phase2Class === 0 ? null : reason,
    ledgerOps: [],
  };
  const record: SDK.ScriptDecodeRecord = {
    Decoded: {
      script_hash: hash.toString("hex"),
      language: BigInt(languageTag),
      program_version: phase2Class === 5 ? -1n : 1n,
      term_root: termRoot.toString("hex"),
      phase2_class: BigInt(phase2Class),
    },
  };
  const recordReference: UTxO = {
    ...utxo("20"),
    assets: { [unit]: 1n },
    datum: Data.to(record, SDK.ScriptDecodeRecord),
  };
  const yields = [utxo("30"), utxo("10")] as const;
  const input = {
    context: { referenceInputs: [yields[0], recordReference, yields[1]] },
    trace,
    stateIndex: 0,
    inputIndex: 0n,
    outputIndex: 0n,
    yieldReferences: yields,
    decodeRecordReference: recordReference,
    scriptDecodePolicyId: policy,
  };
  return { input, record, recordReference, trace, yields };
};

const replaceRecord = (
  f: ReturnType<typeof fixture>,
  record: SDK.ScriptDecodeRecord,
) => ({
  ...f.input,
  context: {
    referenceInputs: f.input.context.referenceInputs.map((u) =>
      u === f.recordReference
        ? { ...u, datum: Data.to(record, SDK.ScriptDecodeRecord) }
        : u,
    ),
  },
});

describe("CEK selection final reference-input composition", () => {
  it.each([3, 128] as const)(
    "binds language %s, record root and final canonical reference indices",
    (language) => {
      const f = fixture(0, language);
      const action = buildValidationCekExecutionSelectionAction(f.input);
      expect(action.record_reference_input_index).toBe(1n);
      expect(action.yield_ref_input_indices).toEqual([2n, 0n]);
      const reordered = buildValidationCekExecutionSelectionAction({
        ...f.input,
        context: {
          referenceInputs: [
            utxo("00"),
            ...f.input.context.referenceInputs,
          ].reverse(),
        },
      });
      expect(reordered.record_reference_input_index).toBe(2n);
      expect(reordered.yield_ref_input_indices).toEqual([3n, 1n]);
      expect(
        Data.from(
          Data.to(action, SDK.ValidationCekExecutionSelectionAction),
          SDK.ValidationCekExecutionSelectionAction,
        ),
      ).toEqual(action);
    },
  );

  it("refuses missing, unminted, hash-datum and absent-final-layout records", () => {
    const f = fixture();
    const { decodeRecordReference: _, ...missing } = f.input;
    expect(() => buildValidationCekExecutionSelectionAction(missing)).toThrow(
      CekDecodeRecordRequiredError,
    );
    for (const record of [
      { ...f.recordReference, assets: {} },
      { ...f.recordReference, assets: { [unit]: 2n } },
      { ...f.recordReference, datumHash: "00".repeat(32) },
    ])
      expect(() =>
        buildValidationCekExecutionSelectionAction({
          ...f.input,
          context: { referenceInputs: [record, ...f.yields] },
        }),
      ).toThrow(/authentication token|inline datum/u);
    expect(() =>
      buildValidationCekExecutionSelectionAction({
        ...f.input,
        context: { referenceInputs: [...f.yields] },
      }),
    ).toThrow(/absent from the final transaction/u);
  });

  it("reads the final input's record and refuses substitutions in hash, language, root, class and version", () => {
    const f = fixture();
    if (!("Decoded" in f.record)) throw new Error("fixture is decoded");
    for (const mutation of [
      { script_hash: "fe".repeat(28) },
      { language: 128n },
      { term_root: "fe".repeat(32) },
      { phase2_class: 11n },
      { phase2_class: 12n },
      { program_version: -1n },
    ])
      expect(() =>
        buildValidationCekExecutionSelectionAction(
          replaceRecord(f, {
            Decoded: { ...f.record.Decoded, ...mutation },
          }),
        ),
      ).toThrow(
        /match the selected|selection successor|decode classification|inconsistent program/u,
      );
    expect(() =>
      buildValidationCekExecutionSelectionAction(
        replaceRecord(f, {
          Refused: {
            script_hash: hash.toString("hex"),
            language: 3n,
            class: 1n,
            bit_offset: 0n,
          },
        }),
      ),
    ).toThrow(/Refused has no execution successor/u);
  });

  it.each([5, 11] as const)(
    "class %s authorizes exactly the zero-budget selection terminal",
    (phase2Class) => {
      const f = fixture(phase2Class);
      const action = buildValidationCekExecutionSelectionAction(f.input);
      expect(action.record_reference_input_index).toBe(1n);
      for (const mutation of [
        { executionCpu: 8n },
        { executionMemory: 10n },
        { phase: "cek" as const },
        {
          rejectionCodeHash: hashMidgardValidationRejectionReasonV1({
            PlutusExecutionFailed: { purpose_kind: 1n, purpose_index: 2n },
          }),
        },
        { workRoot: Buffer.alloc(32, 0) },
      ])
        expect(() =>
          buildValidationCekExecutionSelectionAction({
            ...f.input,
            trace: {
              ...f.trace,
              states: [
                f.trace.states[0]!,
                { ...f.trace.states[1]!, ...mutation },
              ],
            },
          }),
        ).toThrow(/exact selection successor/u);
      if (!("Decoded" in f.record)) throw new Error("fixture is decoded");
      expect(() =>
        buildValidationCekExecutionSelectionAction(
          replaceRecord(f, {
            Decoded: {
              ...f.record.Decoded,
              phase2_class: phase2Class === 5 ? 11n : 5n,
              program_version: phase2Class === 5 ? 1n : -1n,
            },
          }),
        ),
      ).toThrow(/decode classification/u);
    },
  );

  it("native and forbidden PlutusV3 receive selection do not require a decode record", () => {
    for (const f of [fixture(0, 0), fixture(0, 3, 3)]) {
      const { decodeRecordReference: _, ...input } = f.input;
      expect(
        buildValidationCekExecutionSelectionAction(input)
          .record_reference_input_index,
      ).toBe(-1n);
    }
  });
});
