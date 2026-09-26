import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
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
/**
 * Confirmation depth per deployment profile, also written out. The compiled
 * profile selects the row: testing profiles run at 3, public profiles at 30.
 */
const RELEASE_DEPTH_BY_PROFILE: Readonly<Record<string, number>> = {
  mainnet: 30,
  "preprod-public": 30,
  "preprod-testing": 3,
  "local-devnet-testing": 3,
};
const RELEASE_DEPTH =
  RELEASE_DEPTH_BY_PROFILE[SELECTED_DEPLOYMENT_PROFILE.name]!;
const OTHER_PROFILE_DEPTH = RELEASE_DEPTH === 3 ? 30 : 3;
const DEPTH_REFUSAL = new RegExp(
  `l1Finality\\.confirmationDepth must equal the deployment profile value ${RELEASE_DEPTH.toString()}$`,
);

const RELEASE_POLICY = {
  confirmationDepth: RELEASE_DEPTH,
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
    expect(fromInput.confirmationDepth).toBe(RELEASE_DEPTH);
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
      candidate: { ...policy(), confirmationDepth: RELEASE_DEPTH - 1 },
      message: DEPTH_REFUSAL,
    },
    {
      name: "confirmation depth one above the release depth",
      candidate: { ...policy(), confirmationDepth: RELEASE_DEPTH + 1 },
      message: DEPTH_REFUSAL,
    },
    {
      name: "the other profile family's confirmation depth",
      candidate: { ...policy(), confirmationDepth: OTHER_PROFILE_DEPTH },
      message: DEPTH_REFUSAL,
    },
    {
      name: "confirmation depth as a numeric string",
      candidate: { ...policy(), confirmationDepth: RELEASE_DEPTH.toString() },
      message: DEPTH_REFUSAL,
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
      candidate: { ...policy(), automaticRecoveryMaxDepth: RELEASE_DEPTH },
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
        confirmationDepth: RELEASE_DEPTH,
        automaticRecoveryMaxDepth: 2160,
        deep_rollback_policy: RELEASE_POLICY.deepRollbackPolicy,
      },
      message: /must contain the exact release-finality fields$/,
    },
    {
      name: "a missing field",
      candidate: {
        confirmationDepth: RELEASE_DEPTH,
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
      candidate: [RELEASE_DEPTH, 2160, RELEASE_POLICY.deepRollbackPolicy],
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
      `release identity l1Finality.confirmationDepth must equal the deployment profile value ${RELEASE_DEPTH.toString()}`,
    );
  });
});
