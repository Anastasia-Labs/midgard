export const RELEASE_L1_FINALITY_POLICY_V1_DEEP_ROLLBACK_POLICY =
  "automated_rewind_replay_incident-v1" as const;

export type ReleaseL1FinalityPolicyV1 = Readonly<{
  confirmationDepth: 30;
  automaticRecoveryMaxDepth: 2160;
  deepRollbackPolicy: typeof RELEASE_L1_FINALITY_POLICY_V1_DEEP_ROLLBACK_POLICY;
}>;

const exactRecord = (
  value: unknown,
  field: string,
): Record<string, unknown> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    throw new Error(`${field} must be a plain object`);
  }
  const candidate = value as Record<string, unknown>;
  const expected = [
    "automaticRecoveryMaxDepth",
    "confirmationDepth",
    "deepRollbackPolicy",
  ];
  const actual = Object.keys(candidate).sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${field} must contain the exact release-finality fields`);
  }
  return candidate;
};

/** Parse the source-neutral finality policy carried by release identity. */
export const parseReleaseL1FinalityPolicyV1 = (
  value: unknown,
  field = "deployment manifest l1Finality",
): ReleaseL1FinalityPolicyV1 => {
  const candidate = exactRecord(value, field);
  if (candidate.confirmationDepth !== 30) {
    throw new Error(`${field}.confirmationDepth must be exactly 30`);
  }
  if (candidate.automaticRecoveryMaxDepth !== 2160) {
    throw new Error(`${field}.automaticRecoveryMaxDepth must be exactly 2160`);
  }
  if (
    candidate.deepRollbackPolicy !==
    RELEASE_L1_FINALITY_POLICY_V1_DEEP_ROLLBACK_POLICY
  ) {
    throw new Error(`${field}.deepRollbackPolicy is not canonical V1`);
  }
  return Object.freeze({
    confirmationDepth: 30,
    automaticRecoveryMaxDepth: 2160,
    deepRollbackPolicy: RELEASE_L1_FINALITY_POLICY_V1_DEEP_ROLLBACK_POLICY,
  });
};
