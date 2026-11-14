import { deploymentFamilyStepNames } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import {
  checkExecutionSourceScriptDecodingRoleOrder,
  EXECUTION_SOURCE_SCRIPT_DECODING_ROLE_ORDER,
} from "../src/fraud-proof/contracts/families/execution-source-script-decoding.js";
import { executionSourceScriptDecodingSubjectConvicts } from "../src/fraud-proof/execution-source-script-decoding.js";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  type VerdictSubject,
} from "../src/fraud-proof/proof-thread-substrate.js";
import type { RejectionReason } from "../src/rejection-reason.js";

const manifestSteps = () => [
  ...deploymentFamilyStepNames("executionSourceScriptDecoding"),
];

describe("execution-source-script-decoding role order", () => {
  it("names every manifest step in the manifest's order", () => {
    expect(manifestSteps()).toEqual([
      "fraudProofExecutionSourceScriptDecoding",
      "fraudProofExecutionSourceScriptDecodingBindEvent",
      "fraudProofExecutionSourceScriptDecodingTerminal",
    ]);
    expect(EXECUTION_SOURCE_SCRIPT_DECODING_ROLE_ORDER).toEqual([
      "step01",
      "bindEvent",
      "terminal",
    ]);
    expect(() =>
      checkExecutionSourceScriptDecodingRoleOrder(manifestSteps()),
    ).not.toThrow();
  });

  it("refuses a manifest that swaps the event selection and the terminal", () => {
    const steps = manifestSteps();
    [steps[1], steps[2]] = [steps[2]!, steps[1]!];
    expect(() => checkExecutionSourceScriptDecodingRoleOrder(steps)).toThrow(
      "execution-source-script-decoding role bindEvent is fraudProofExecutionSourceScriptDecodingBindEvent, but manifest step 1 is fraudProofExecutionSourceScriptDecodingTerminal",
    );
  });

  it("refuses a manifest with a step more or less than the roles", () => {
    const steps = manifestSteps();
    for (const changed of [steps.slice(1), [...steps, steps[0]!]])
      expect(() =>
        checkExecutionSourceScriptDecodingRoleOrder(changed),
      ).toThrow(
        `execution-source-script-decoding has 3 roles; the manifest has ${changed.length} steps`,
      );
  });
});

const forced = (reason: RejectionReason | null): VerdictSubject =>
  forcedVerdictSubject({
    transactionId: "ab".repeat(32),
    sourceKey: { transactionId: "cd".repeat(32), outputIndex: 0n },
    rejectionReason: reason,
  });

// The same rows are pinned on-chain in
// execution-source-script-decoding/terminal.test.ak; change both together.
describe("execution-source-script-decoding verdict twin", () => {
  it("convicts every recorded decoding arm and nothing else", () => {
    const at = (purpose_kind: bigint, purpose_index: bigint) => ({
      purpose_kind,
      purpose_index,
    });
    const rows: readonly [VerdictSubject, boolean][] = [
      [forced({ ExecutionNativeScriptMalformed: at(0n, 0n) }), true],
      [forced({ ExecutionNativeScriptNodeLimit: at(1n, 7n) }), true],
      [forced({ ExecutionNativeScriptDepthLimit: at(3n, 255n) }), true],
      [forced({ ExecutionNativeScriptMalformed: at(2n, 0n) }), true],
      [forced({ ExecutionNativeScriptNodeLimit: at(0n, 1024n) }), true],
      [forced({ ExecutionNativeScriptDepthLimit: at(1n, 0n) }), true],
      [forced({ ExecutionNativeScriptFalse: at(0n, 0n) }), false],
      [forced({ UnusedRedeemer: { redeemer_index: 0n } }), false],
      [forced("ScriptIntegrityHashMismatch"), false],
      [acceptedVerdictSubject("ab".repeat(32)), false],
      [forced(null), false],
    ];
    for (const [subject, expected] of rows)
      expect(executionSourceScriptDecodingSubjectConvicts(subject)).toBe(
        expected,
      );
  });

  it("refuses an acceptance carrying a recorded arm", () => {
    const subject = {
      ...forced({
        ExecutionNativeScriptMalformed: { purpose_kind: 0n, purpose_index: 0n },
      }),
      direction: acceptedVerdictSubject("ab".repeat(32)).direction,
    };
    expect(() =>
      executionSourceScriptDecodingSubjectConvicts(subject),
    ).toThrow();
  });
});
