import {
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
  type MidgardValidationMachineState,
  parseMidgardVersionedScriptHeader,
} from "@al-ft/midgard-core";
import {
  asArray,
  asBigInt,
  asBytes,
  decodeSingleCbor,
} from "@al-ft/midgard-core/codec/cbor";
import { hashMidgardValidationRejectionReasonV1 } from "@al-ft/midgard-core/validation-trace";

import {
  encodeMidgardCekValidationWitness,
  initialMidgardCekContextControl,
} from "./cek-context.js";
import type { ScriptDecodePhase2Class } from "./script-decode.step-types.js";
import type { ValidationMachineWorkWitness } from "./validation-machine/index.js";
import { encodeValidationTerminalWitnessCbor } from "./validation-machine/terminal-witness.js";

export type CekSelectionDecodeRecord = {
  readonly scriptHash: Buffer;
  readonly languageTag: 3 | 128;
  readonly programVersion: -1 | 0 | 1;
  readonly termRoot: Buffer;
  readonly phase2Class: ScriptDecodePhase2Class;
};

/** Resolved from the final transaction's token-authenticated inline record. */
export type CekSelectionDecodeRecordResolver = (selection: {
  readonly scriptHash: Buffer;
  readonly languageTag: 3 | 128;
}) => CekSelectionDecodeRecord;

export class CekDecodeRecordRequiredError extends Error {
  constructor() {
    super(
      "CEK selection requires the selected script's authenticated decoded record",
    );
    this.name = "CekDecodeRecordRequiredError";
  }
}

/**
 * Checks the record's exact selection successor. Record authentication belongs
 * to the transaction composer; this routine neither looks up chain state nor
 * treats a locally derived graph as a record. Admission never calls it.
 */
export const validateCekSelectionDecodeRecord = ({
  pre,
  claimedSuccessor,
  witness,
  resolveCekDecodeRecord,
}: {
  readonly pre: MidgardValidationMachineState;
  readonly claimedSuccessor: MidgardValidationMachineState;
  readonly witness: ValidationMachineWorkWitness;
  readonly resolveCekDecodeRecord?: CekSelectionDecodeRecordResolver;
}): void => {
  const auxiliary = witness.auxiliary;
  if (
    witness.phase !== "cek" ||
    auxiliary?.kind !== "nativeExecutionScan" ||
    auxiliary.languageTag === 0 ||
    (auxiliary.languageTag === 3 && auxiliary.purpose.purposeKind === 3)
  )
    return;
  if (resolveCekDecodeRecord === undefined)
    throw new CekDecodeRecordRequiredError();
  const header = parseMidgardVersionedScriptHeader(
    auxiliary.firstChunkProof.chunk,
    auxiliary.source.scriptTotalLength,
  );
  if (header?.languageTag !== auxiliary.languageTag)
    throw new Error("CEK selection language does not match its source header");
  const record = resolveCekDecodeRecord({
    scriptHash: auxiliary.purpose.scriptHash,
    languageTag: auxiliary.languageTag,
  });
  if (
    !record.scriptHash.equals(auxiliary.purpose.scriptHash) ||
    record.languageTag !== auxiliary.languageTag ||
    record.termRoot.length !== 32 ||
    ![0, 5, 11].includes(record.phase2Class) ||
    ![-1, 0, 1].includes(record.programVersion) ||
    (record.programVersion === -1) !== (record.phase2Class === 5) ||
    record.phase2Class !== auxiliary.phase2Class
  )
    throw new Error(
      "CEK decoded record disagrees with the selected script and decode classification",
    );

  let expected: MidgardValidationMachineState;
  if (record.phase2Class !== 0) {
    const reason = {
      PlutusExecutionFailed: {
        purpose_kind: BigInt(auxiliary.purpose.purposeKind),
        purpose_index: auxiliary.purpose.purposeIndex,
      },
    };
    const terminalCbor = encodeValidationTerminalWitnessCbor({
      verdict: "rejected",
      reason,
      priorLedgerRoot: pre.priorLedgerRoot,
    });
    expected = {
      ...pre,
      phase: "terminal",
      programCounter: pre.programCounter + 1,
      workRoot: hashMidgardValidationWorkWitness({
        phase: "terminal",
        programCounter: pre.programCounter + 1,
        witnessCbor: terminalCbor,
      }),
      verdict: "rejected",
      rejectionCodeHash: hashMidgardValidationRejectionReasonV1(reason),
    };
  } else {
    const control = asArray(decodeSingleCbor(witness.cbor), "cek_selection");
    if (control.length !== 11)
      throw new Error("CEK selection has an invalid control field count");
    const executionCursor = Number(
      asBigInt(control[2], "cek_selection.execution_cursor"),
    );
    if (!Number.isSafeInteger(executionCursor) || executionCursor < 0)
      throw new Error("CEK selection execution cursor is invalid");
    const contextCbor = encodeMidgardCekValidationWitness({
      nativeControlCbor: asBytes(control[0], "cek_selection.native_control"),
      contextControl: initialMidgardCekContextControl({
        languageTag: record.languageTag,
        programTermRoot: record.termRoot,
        purposeKind: auxiliary.purpose.purposeKind,
        purposeIndex: auxiliary.purpose.purposeIndex,
        scriptHash: record.scriptHash,
        subject: auxiliary.purpose.subject,
        redeemerLeaf: auxiliary.redeemerLeaf,
      }),
      executionCursor,
      completedCpu: asBigInt(control[3], "cek_selection.completed_cpu"),
      completedMemory: asBigInt(control[4], "cek_selection.completed_memory"),
      activeStateHash: null,
      programScriptHash: record.scriptHash,
      executionCpuLimit: 0n,
      executionMemoryLimit: 0n,
      purposeKind: -1n,
      purposeIndex: -1n,
    });
    expected = {
      ...pre,
      programCounter: pre.programCounter + 1,
      workRoot: hashMidgardValidationWorkWitness({
        phase: "cek",
        programCounter: pre.programCounter + 1,
        witnessCbor: contextCbor,
      }),
    };
  }
  if (
    !hashMidgardValidationMachineState(expected).equals(
      hashMidgardValidationMachineState(claimedSuccessor),
    )
  )
    throw new Error(
      "CEK decoded record does not authorize the exact selection successor",
    );
};
