const DAY_IN_MILLIS = 24 * 60 * 60 * 1000;

export const shouldPruneRetention = (retentionDays: number): boolean =>
  Number.isFinite(retentionDays) && retentionDays > 0;

export const computeRetentionCutoff = (
  now: Date,
  retentionDays: number,
): Date => {
  const days = Math.max(0, Math.floor(retentionDays));
  return new Date(now.getTime() - days * DAY_IN_MILLIS);
};
