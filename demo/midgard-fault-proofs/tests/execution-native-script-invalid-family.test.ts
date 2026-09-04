import { computeHash32, encodeMidgardNativeScript } from "@al-ft/midgard-core";
import {
  forcedVerdictSubject,
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  type RejectionReason,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyExecutionNativeScriptInvalidFinding,
  prepareExecutionNativeScriptInvalidEvidence,
} from "../src/execution-native-script-invalid/family.js";

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
  rejection_reason: RejectionReason | null,
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
    const evidence = prepareExecutionNativeScriptInvalidEvidence(
      input(PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE, null, script(7)),
    );
    expect(evidence.terminal.satisfied).toBe(false);
    expect(evidence.bindingHash).toMatch(/^[0-9a-f]{64}$/u);
  });

  it("refuses another typed reason and a changed execution coordinate", () => {
    expect(() =>
      classifyExecutionNativeScriptInvalidFinding({
        subject: forcedVerdictSubject({
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
      prepareExecutionNativeScriptInvalidEvidence({
        ...input(0n, null, bytes),
        transactionIdHex: "33".repeat(32),
      }),
    ).toThrow(/transaction identity/u);
    expect(() =>
      prepareExecutionNativeScriptInvalidEvidence({
        ...input(0n, null, bytes),
        scriptItemHashHex: "44".repeat(32),
      }),
    ).toThrow(/script bytes/u);
    expect(() =>
      prepareExecutionNativeScriptInvalidEvidence({
        ...input(0n, null, bytes),
        validityIntervalStart: 10n,
      }),
    ).toThrow(/interval/u);
  });
});
