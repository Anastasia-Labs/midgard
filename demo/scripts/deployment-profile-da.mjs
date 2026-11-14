/** The public deployment's retained input history policy. */
export const validatePublicDaTransport = (profile) => {
  if (!Object.hasOwn(profile, "da_transport")) return;
  const policy = profile.da_transport;
  if (
    policy === null ||
    typeof policy !== "object" ||
    Array.isArray(policy) ||
    Object.keys(policy).length !== 1 ||
    !Object.hasOwn(policy, "retention_days")
  )
    throw new Error("da_transport must contain exactly retention_days");
  const days = policy.retention_days;
  if (
    !Number.isSafeInteger(days) ||
    !Number.isSafeInteger(days * 86_400_000) ||
    days < 120
  )
    throw new Error(
      "da_transport.retention_days must be at least 120 for public Preprod bootstrap",
    );
};
