export type CommitteePromiseAdoptionConfig = Readonly<{
  policyArtifactPath: string;
  trustedPolicyDigest: string;
  resourceProfilePath: string;
  trustedResourceProfileDigest: string;
  calibrationEvidencePath: string;
  trustedCalibrationEvidenceDigest: string;
  faultModelPath: string;
  trustedFaultModelDigest: string;
}>;
/** Explicit trusted owner adoption; partial configuration never activates. */
export const committeePromiseAdoptionConfig = (
  env: Readonly<Record<string, string | undefined>>,
): CommitteePromiseAdoptionConfig | undefined => {
  const fields = {
    policyArtifactPath: "DA_PROMISE_POLICY_ARTIFACT_PATH",
    trustedPolicyDigest: "DA_PROMISE_POLICY_TRUSTED_SHA256",
    resourceProfilePath: "DA_PROMISE_RESOURCE_PROFILE_PATH",
    trustedResourceProfileDigest: "DA_PROMISE_RESOURCE_PROFILE_TRUSTED_SHA256",
    calibrationEvidencePath: "DA_PROMISE_CALIBRATION_EVIDENCE_PATH",
    trustedCalibrationEvidenceDigest: "DA_PROMISE_CALIBRATION_TRUSTED_SHA256",
    faultModelPath: "DA_PROMISE_FAULT_MODEL_PATH",
    trustedFaultModelDigest: "DA_PROMISE_FAULT_MODEL_TRUSTED_SHA256",
  } as const;
  if (Object.values(fields).every((key) => !env[key]?.trim())) return undefined;
  const values = Object.fromEntries(
    Object.entries(fields).map(([field, key]) => {
      const value = env[key]?.trim();
      if (
        !value ||
        (field.startsWith("trusted") && !/^[0-9a-f]{64}$/u.test(value))
      )
        throw new Error(
          `Explicit promise policy adoption requires valid ${key}`,
        );
      return [field, value];
    }),
  );
  return values as CommitteePromiseAdoptionConfig;
};
