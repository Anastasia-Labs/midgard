const DAY_IN_MILLIS = 24 * 60 * 60 * 1000;

export const MIN_DA_PAYLOAD_RETENTION_DAYS = 8;

export const validateRetentionDays = (retentionDays: number): number => {
  if (!Number.isSafeInteger(retentionDays) || retentionDays < 0) {
    throw new Error("RETENTION_DAYS must be a non-negative safe integer.");
  }
  if (retentionDays > 0 && retentionDays < MIN_DA_PAYLOAD_RETENTION_DAYS) {
    throw new Error(
      `RETENTION_DAYS must be 0 or at least ${MIN_DA_PAYLOAD_RETENTION_DAYS} days so DA payloads remain available through the challenge window.`,
    );
  }
  return retentionDays;
};

export const shouldPruneRetention = (retentionDays: number): boolean =>
  validateRetentionDays(retentionDays) > 0;

export const computeRetentionCutoff = (
  now: Date,
  retentionDays: number,
): Date => {
  const days = validateRetentionDays(retentionDays);
  return new Date(now.getTime() - days * DAY_IN_MILLIS);
};
