import { computeHash32, encodeMidgardNativeScript } from "@al-ft/midgard-core";
import {
  forcedVerdictSubjectV1,
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  type RejectionReasonV1,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyExecutionNativeScriptInvalidFindingV1,
  prepareExecutionNativeScriptInvalidEvidenceV1,
} from "../src/execution-native-script-invalid/family-v1.js";

const txId = "11".repeat(32);
const base = {
  version: 1n,
  source_kind: 0n,
  transaction_id: txId,
  source_key: "",
} as const;
const script = (keyHash: number) =>
  encodeMidgardNativeScript({
    type: "sig",
    keyHash: Buffer.alloc(28, keyHash),
  });
const input = (
  direction: bigint,
  rejection_reason: RejectionReasonV1 | null,
  bytes: Buffer,
) => ({
  finding: {
    subject: { ...base, direction, rejection_reason },
    executionIndex: 3,
  },
  transactionIdHex: txId,
  sourceDescriptorHashHex: "22".repeat(32),
  scriptItemHashHex: computeHash32(bytes).toString("hex"),
  scriptBytes: bytes,
  addressWitnessItems: [],
  validityIntervalStart: 4n,
  validityIntervalEnd: 9n,
});

describe("executionNativeScriptInvalid authenticated family", () => {
  it("convicts accepted false and binds a deterministic terminal", () => {
    const evidence = prepareExecutionNativeScriptInvalidEvidenceV1(
      input(PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1, null, script(7)),
    );
    expect(evidence.terminal.satisfied).toBe(false);
    expect(evidence.bindingHash).toMatch(/^[0-9a-f]{64}$/u);
  });

  it("refuses another typed reason and a changed execution coordinate", () => {
    expect(() =>
      classifyExecutionNativeScriptInvalidFindingV1({
        subject: forcedVerdictSubjectV1({
          transactionId: txId,
          sourceKey: { transactionId: "33".repeat(32), outputIndex: 0n },
          rejectionReason: {
            ExecutionNativeScriptFalse: { execution_index: 2n },
          },
        }),
        executionIndex: 3,
      }),
    ).toThrow(/execution coordinate/u);
  });

  it("refuses substituted transaction, source item, and malformed interval", () => {
    const bytes = script(7);
    expect(() =>
      prepareExecutionNativeScriptInvalidEvidenceV1({
        ...input(0n, null, bytes),
        transactionIdHex: "33".repeat(32),
      }),
    ).toThrow(/transaction identity/u);
    expect(() =>
      prepareExecutionNativeScriptInvalidEvidenceV1({
        ...input(0n, null, bytes),
        scriptItemHashHex: "44".repeat(32),
      }),
    ).toThrow(/script bytes/u);
    expect(() =>
      prepareExecutionNativeScriptInvalidEvidenceV1({
        ...input(0n, null, bytes),
        validityIntervalStart: 10n,
      }),
    ).toThrow(/interval/u);
  });
});
