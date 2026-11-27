export const DA_PAYLOAD_ENVELOPE_VERSION = 1 as const;
export const DA_PAYLOAD_INNER_SCHEMA_VERSION = 1 as const;

export const DaPayloadContentEncoding = {
  identity: 0,
  zstd: 1,
} as const;

export type DaPayloadContentEncoding =
  (typeof DaPayloadContentEncoding)[keyof typeof DaPayloadContentEncoding];
