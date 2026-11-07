/**
 * Default `MAX_DURABLE_ADMISSION_BACKLOG`: 60 s at the 5,000 TPS design rate
 * (design D5). Provisional until the M1 measurement (#877) sets it.
 */
export const DEFAULT_MAX_DURABLE_ADMISSION_BACKLOG = 300_000;

/**
 * Default `MAX_DURABLE_ADMISSION_BACKLOG_BYTES`: the default row bound at
 * 1 KiB per tx, so txs averaging up to 1 KiB reach the row bound first.
 * Provisional with the row bound.
 */
export const DEFAULT_MAX_DURABLE_ADMISSION_BACKLOG_BYTES =
  DEFAULT_MAX_DURABLE_ADMISSION_BACKLOG * 1024;

/**
 * Percent of both backlog bounds that anonymous submissions (`/submit`) may
 * fill. Peer-forwarded txs may fill the rest (design D5), so a public burst
 * cannot stop operators sharing their users' txs.
 */
export const ANONYMOUS_ADMISSION_SHED_PERCENT = 80;

/** Share of `MAX_DURABLE_ADMISSION_BACKLOG` above which the node is not ready. */
export const READINESS_DURABLE_ADMISSION_BACKLOG_FRACTION = 0.95;

/**
 * The backlog bounds a submission may reserve against: anonymous ones shed
 * at `ANONYMOUS_ADMISSION_SHED_PERCENT` of each bound (rounded down), peer
 * ones at the bound itself.
 */
export const admissionBacklogLimitsFor = (
  submitSource: "native" | "peer",
  maxRows: number,
  maxBytes: number,
): { readonly maxRows: number; readonly maxBytes: number } =>
  submitSource === "peer"
    ? { maxRows, maxBytes }
    : {
        maxRows: Math.floor((maxRows * ANONYMOUS_ADMISSION_SHED_PERCENT) / 100),
        maxBytes: Math.floor(
          (maxBytes * ANONYMOUS_ADMISSION_SHED_PERCENT) / 100,
        ),
      };

/**
 * The readiness backlog limit follows the admission bound, above the
 * anonymous shed share: anonymous load is shed first, and the node turns
 * not-ready only once peer traffic has filled the backlog past this share.
 */
export const readinessDurableAdmissionBacklogLimit = (
  maxDurableAdmissionBacklog: number,
): number =>
  Math.floor(
    maxDurableAdmissionBacklog * READINESS_DURABLE_ADMISSION_BACKLOG_FRACTION,
  );
