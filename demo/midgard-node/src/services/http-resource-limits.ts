export const HTTP_RESPONSE_MAX_BYTES = 1_048_576;
/** Reserve 64 KiB for JSON keys and up to 100 outrefs; hex doubles stored bytes. */
export const HTTP_PUBLIC_READ_DATA_MAX_BYTES =
  (HTTP_RESPONSE_MAX_BYTES - 65_536) / 2;

/**
 * Fail-fast SQL permits per HTTP read lane. Readiness and admin reads take the
 * operator lane, so public reads holding every public permit cannot refuse
 * them, and they cannot drain public reads. The http-read pool has one
 * connection per permit, four in all.
 */
export const HTTP_READ_LANE_PERMITS = { public: 3, operator: 1 } as const;
export type HttpReadLane = keyof typeof HTTP_READ_LANE_PERMITS;
