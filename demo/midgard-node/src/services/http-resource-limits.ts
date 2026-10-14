export const HTTP_RESPONSE_MAX_BYTES = 1_048_576;
/** Reserve 64 KiB for JSON keys and up to 100 outrefs; hex doubles stored bytes. */
export const HTTP_PUBLIC_READ_DATA_MAX_BYTES =
  (HTTP_RESPONSE_MAX_BYTES - 65_536) / 2;
