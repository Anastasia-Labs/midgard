import { computeHash32 } from "@al-ft/midgard-core";
import {
  encodeVerdictSubjectV1,
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1,
  verdictSubjectIsCanonicalV1,
  type VerdictSubjectV1,
} from "@al-ft/midgard-sdk";

import {
  type ExecutionNativeScriptInvalidPushdownStepV1,
  executionNativeScriptInvalidPushdownStepV1,
  executionNativeScriptInvalidSignerSetV1,
} from "./evidence-machine-v1.js";

export const EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_V1 =
  "executionNativeScriptInvalid" as const;
export const EXECUTION_NATIVE_SCRIPT_INVALID_ID_V1 = "00000032" as const;

const fail = (message: string): never => {
  throw new Error(`${EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_V1}: ${message}`);
};

const canonicalHex = (value: string, bytes: number, label: string): Buffer => {
  if (!new RegExp(`^[0-9a-f]{${String(bytes * 2)}}$`, "u").test(value)) {
    return fail(`${label} must be canonical ${String(bytes)}-byte hex`);
  }
  return Buffer.from(value, "hex");
};

export type ExecutionNativeScriptInvalidFindingV1 = Readonly<{
  subject: VerdictSubjectV1;
  executionIndex: number;
}>;

export const classifyExecutionNativeScriptInvalidFindingV1 = (
  finding: ExecutionNativeScriptInvalidFindingV1,
): ExecutionNativeScriptInvalidFindingV1 => {
  if (!verdictSubjectIsCanonicalV1(finding.subject))
    return fail("verdict subject is not canonical");
  if (
    !Number.isSafeInteger(finding.executionIndex) ||
    finding.executionIndex < 0
  )
    return fail("execution index must be a non-negative safe integer");
  if (
    finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1
  ) {
    if (finding.subject.rejection_reason !== null)
      return fail("wrongful acceptance must not carry a rejection reason");
  } else if (
    finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1
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

export type ExecutionNativeScriptInvalidAuthenticatedInputV1 = Readonly<{
  finding: ExecutionNativeScriptInvalidFindingV1;
  transactionIdHex: string;
  sourceDescriptorHashHex: string;
  scriptItemHashHex: string;
  scriptBytes: Uint8Array;
  addressWitnessItems: readonly Uint8Array[];
  validityIntervalStart: bigint;
  validityIntervalEnd: bigint;
}>;

export type ExecutionNativeScriptInvalidEvidenceV1 = Readonly<{
  finding: ExecutionNativeScriptInvalidFindingV1;
  authenticated: ExecutionNativeScriptInvalidAuthenticatedInputV1;
  bindingHash: string;
  terminal: ExecutionNativeScriptInvalidPushdownStepV1;
  contradiction: boolean;
}>;

const i64 = (value: bigint): Buffer => {
  const result = Buffer.alloc(8);
  result.writeBigInt64BE(value);
  return result;
};

export const prepareExecutionNativeScriptInvalidEvidenceV1 = (
  input: ExecutionNativeScriptInvalidAuthenticatedInputV1,
): ExecutionNativeScriptInvalidEvidenceV1 => {
  const finding = classifyExecutionNativeScriptInvalidFindingV1(input.finding);
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
  const signerSet = executionNativeScriptInvalidSignerSetV1(
    input.addressWitnessItems,
  );
  let terminal = executionNativeScriptInvalidPushdownStepV1({
    scriptBytes: input.scriptBytes,
    validityIntervalStart: input.validityIntervalStart,
    validityIntervalEnd: input.validityIntervalEnd,
    signerSet,
  });
  while (!terminal.complete) {
    terminal = executionNativeScriptInvalidPushdownStepV1({
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
    finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1;
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
      Buffer.from(encodeVerdictSubjectV1(finding.subject)),
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
