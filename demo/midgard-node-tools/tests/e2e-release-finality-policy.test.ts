import { describe, expect, it } from "vitest";

import {
  parseReleaseL1FinalityPolicy,
  RELEASE_L1_FINALITY_POLICY_DEEP_ROLLBACK_POLICY,
} from "../src/commands/e2e-release-finality-policy.js";

/**
 * The F04 release-bound policy, written out rather than imported.
 *
 * The whole point of this parser is that the release fixes these three values
 * and a caller cannot choose its own: taking the expected value from the
 * production constant would make the accept case unable to notice a change to
 * the very thing it is pinning.
 */
const RELEASE_POLICY = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;

const policy = (): Record<string, unknown> => ({ ...RELEASE_POLICY });

describe("release-bound L1 finality policy V1", () => {
  it("pins the deep-rollback policy identifier the release ships", () => {
    expect(RELEASE_L1_FINALITY_POLICY_DEEP_ROLLBACK_POLICY).toBe(
      RELEASE_POLICY.deepRollbackPolicy,
    );
  });

  it("accepts the exact source-neutral F04 policy and returns it canonically", () => {
    const parsed = parseReleaseL1FinalityPolicy(policy());

    expect(parsed).toEqual(RELEASE_POLICY);
    // The parser returns its own frozen record rather than the caller's
    // object, so a later mutation of the input cannot reach the policy the
    // release runs on.
    expect(Object.isFrozen(parsed)).toBe(true);
    const input = policy();
    const fromInput = parseReleaseL1FinalityPolicy(input);
    input.confirmationDepth = 9;
    expect(fromInput.confirmationDepth).toBe(30);
  });

  /**
   * Each row keeps every other requirement satisfied and violates exactly one,
   * and names the refusal that violation must produce — a bare `toThrow()`
   * was satisfied by any failure at all, including a null dereference.
   */
  const rejections: readonly {
    readonly name: string;
    readonly candidate: unknown;
    readonly message: RegExp;
  }[] = [
    {
      name: "confirmation depth one below the release depth",
      candidate: { ...policy(), confirmationDepth: 29 },
      message: /l1Finality\.confirmationDepth must be exactly 30$/,
    },
    {
      name: "confirmation depth one above the release depth",
      candidate: { ...policy(), confirmationDepth: 31 },
      message: /l1Finality\.confirmationDepth must be exactly 30$/,
    },
    {
      name: "confirmation depth as a numeric string",
      candidate: { ...policy(), confirmationDepth: "30" },
      message: /l1Finality\.confirmationDepth must be exactly 30$/,
    },
    {
      name: "recovery depth one below the release depth",
      candidate: { ...policy(), automaticRecoveryMaxDepth: 2159 },
      message: /l1Finality\.automaticRecoveryMaxDepth must be exactly 2160$/,
    },
    {
      name: "recovery depth one above the release depth",
      candidate: { ...policy(), automaticRecoveryMaxDepth: 2161 },
      message: /l1Finality\.automaticRecoveryMaxDepth must be exactly 2160$/,
    },
    {
      name: "recovery depth collapsed onto the confirmation depth",
      candidate: { ...policy(), automaticRecoveryMaxDepth: 30 },
      message: /l1Finality\.automaticRecoveryMaxDepth must be exactly 2160$/,
    },
    {
      name: "a caller-selected deep-rollback policy",
      candidate: { ...policy(), deepRollbackPolicy: "manual-repair" },
      message: /l1Finality\.deepRollbackPolicy is not canonical V1$/,
    },
    {
      name: "a later deep-rollback policy version",
      candidate: {
        ...policy(),
        deepRollbackPolicy: "automated_rewind_replay_incident-v2",
      },
      message: /l1Finality\.deepRollbackPolicy is not canonical V1$/,
    },
    {
      name: "an extra source-selection field",
      candidate: { ...policy(), sourceMode: "local_node" },
      message: /must contain the exact release-finality fields$/,
    },
    {
      name: "an aliased field name",
      candidate: {
        confirmationDepth: 30,
        automaticRecoveryMaxDepth: 2160,
        deep_rollback_policy: RELEASE_POLICY.deepRollbackPolicy,
      },
      message: /must contain the exact release-finality fields$/,
    },
    {
      name: "a missing field",
      candidate: {
        confirmationDepth: 30,
        automaticRecoveryMaxDepth: 2160,
      },
      message: /must contain the exact release-finality fields$/,
    },
    {
      name: "null",
      candidate: null,
      message: /l1Finality must be a plain object$/,
    },
    {
      name: "an array",
      candidate: [30, 2160, RELEASE_POLICY.deepRollbackPolicy],
      message: /l1Finality must be a plain object$/,
    },
    {
      name: "a JSON string",
      candidate: JSON.stringify(RELEASE_POLICY),
      message: /l1Finality must be a plain object$/,
    },
  ];

  it.each(rejections)("rejects $name", ({ candidate, message }) => {
    expect(() => parseReleaseL1FinalityPolicy(candidate)).toThrow(message);
  });

  it("names the caller's field in the refusal", () => {
    expect(() =>
      parseReleaseL1FinalityPolicy(
        { ...policy(), confirmationDepth: 6 },
        "release identity l1Finality",
      ),
    ).toThrow(
      "release identity l1Finality.confirmationDepth must be exactly 30",
    );
  });
});
