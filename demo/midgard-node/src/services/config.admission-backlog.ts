/** Share of `MAX_DURABLE_ADMISSION_BACKLOG` above which the node is not ready. */
export const READINESS_DURABLE_ADMISSION_BACKLOG_FRACTION = 0.95;

/**
 * The readiness backlog limit follows the admission bound, so the node turns
 * not-ready shortly before `/submit` starts answering 503 for a full backlog,
 * and never while the backlog is at or below this share of the bound.
 */
export const readinessDurableAdmissionBacklogLimit = (
  maxDurableAdmissionBacklog: number,
): number =>
  Math.floor(
    maxDurableAdmissionBacklog * READINESS_DURABLE_ADMISSION_BACKLOG_FRACTION,
  );
