import { computeHash32 } from "@al-ft/midgard-core";
import {
  encodeVerdictSubject,
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk";

import {
  type ExecutionNativeScriptInvalidPushdownStep,
  executionNativeScriptInvalidPushdownStep,
  executionNativeScriptInvalidSignerSet,
} from "./evidence-machine.js";

export const EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY =
  "executionNativeScriptInvalid" as const;
export const EXECUTION_NATIVE_SCRIPT_INVALID_ID = "00000032" as const;

const fail = (message: string): never => {
  throw new Error(`${EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY}: ${message}`);
};

const canonicalHex = (value: string, bytes: number, label: string): Buffer => {
  if (!new RegExp(`^[0-9a-f]{${String(bytes * 2)}}$`, "u").test(value)) {
    return fail(`${label} must be canonical ${String(bytes)}-byte hex`);
  }
  return Buffer.from(value, "hex");
};

export type ExecutionNativeScriptInvalidFinding = Readonly<{
  subject: VerdictSubject;
  executionIndex: number;
}>;

export const classifyExecutionNativeScriptInvalidFinding = (
  finding: ExecutionNativeScriptInvalidFinding,
): ExecutionNativeScriptInvalidFinding => {
  if (!verdictSubjectIsCanonical(finding.subject))
    return fail("verdict subject is not canonical");
  if (
    !Number.isSafeInteger(finding.executionIndex) ||
    finding.executionIndex < 0
  )
    return fail("execution index must be a non-negative safe integer");
  if (
    finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE
  ) {
    if (finding.subject.rejection_reason !== null)
      return fail("wrongful acceptance must not carry a rejection reason");
  } else if (
    finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION
  ) {
    const reason = finding.subject.rejection_reason;
    if (
      reason === null ||
      typeof reason === "string" ||
      !("ExecutionNativeScriptFalse" in reason) ||
      reason.ExecutionNativeScriptFalse.execution_index !==
        BigInt(finding.executionIndex)
    )
      return fail(
        "typed reason is not ExecutionNativeScriptFalse at the execution coordinate",
      );
  } else return fail("unsupported direction");
  return Object.freeze({ ...finding });
};

export type ExecutionNativeScriptInvalidAuthenticatedInput = Readonly<{
  finding: ExecutionNativeScriptInvalidFinding;
  transactionIdHex: string;
  sourceDescriptorHashHex: string;
  scriptItemHashHex: string;
  scriptBytes: Uint8Array;
  addressWitnessItems: readonly Uint8Array[];
  validityIntervalStart: bigint;
  validityIntervalEnd: bigint;
}>;

export type ExecutionNativeScriptInvalidEvidence = Readonly<{
  finding: ExecutionNativeScriptInvalidFinding;
  authenticated: ExecutionNativeScriptInvalidAuthenticatedInput;
  bindingHash: string;
  terminal: ExecutionNativeScriptInvalidPushdownStep;
  contradiction: boolean;
}>;

const i64 = (value: bigint): Buffer => {
  const result = Buffer.alloc(8);
  result.writeBigInt64BE(value);
  return result;
};

export const prepareExecutionNativeScriptInvalidEvidence = (
  input: ExecutionNativeScriptInvalidAuthenticatedInput,
): ExecutionNativeScriptInvalidEvidence => {
  const finding = classifyExecutionNativeScriptInvalidFinding(input.finding);
  const transactionId = canonicalHex(
    input.transactionIdHex,
    32,
    "transaction id",
  );
  if (input.transactionIdHex !== finding.subject.transaction_id)
    return fail("transaction identity differs from the authenticated subject");
  const sourceDescriptorHash = canonicalHex(
    input.sourceDescriptorHashHex,
    32,
    "source descriptor hash",
  );
  const scriptItemHash = canonicalHex(
    input.scriptItemHashHex,
    32,
    "script item hash",
  );
  if (input.validityIntervalEnd < input.validityIntervalStart)
    return fail("validity interval is malformed");
  const actualScriptHash = computeHash32(input.scriptBytes);
  if (!actualScriptHash.equals(scriptItemHash))
    return fail("script bytes differ from the authenticated item hash");
  const signerSet = executionNativeScriptInvalidSignerSet(
    input.addressWitnessItems,
  );
  let terminal = executionNativeScriptInvalidPushdownStep({
    scriptBytes: input.scriptBytes,
    validityIntervalStart: input.validityIntervalStart,
    validityIntervalEnd: input.validityIntervalEnd,
    signerSet,
  });
  while (!terminal.complete) {
    terminal = executionNativeScriptInvalidPushdownStep({
      scriptBytes: input.scriptBytes,
      validityIntervalStart: input.validityIntervalStart,
      validityIntervalEnd: input.validityIntervalEnd,
      signerSet,
      committedCursorHash: terminal.nextCursorHash,
      cursorBytes: Buffer.from(terminal.nextCursorBytes, "hex"),
      frames: terminal.nextFrames,
    });
  }
  const accepted =
    finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE;
  const contradiction = accepted
    ? terminal.satisfied === false
    : terminal.satisfied === true;
  if (!contradiction)
    return fail(
      "authenticated native evaluation does not contradict the verdict",
    );
  const bindingHash = computeHash32(
    Buffer.concat([
      Buffer.from("MidgardExecutionNativeScriptInvalidV1", "ascii"),
      Buffer.from(encodeVerdictSubject(finding.subject)),
      transactionId,
      sourceDescriptorHash,
      scriptItemHash,
      i64(input.validityIntervalStart),
      i64(input.validityIntervalEnd),
      Buffer.from(terminal.nextCursorHash, "hex"),
    ]),
  ).toString("hex");
  return Object.freeze({
    finding,
    authenticated: Object.freeze({ ...input, finding }),
    bindingHash,
    terminal,
    contradiction,
  });
};
