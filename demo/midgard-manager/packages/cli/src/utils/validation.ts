import * as S from '@effect/schema/Schema';

/**
 * Schema for endpoints that must include an HTTP or HTTPS scheme.
 */
export const URLSchema = S.String.pipe(
  S.pattern(/^https?:\/\/.+/),
  S.description('Must be a valid HTTP/HTTPS URL')
);

/**
 * Schema for TCP ports accepted by the CLI configuration.
 */
export const PortSchema = S.Number.pipe(
  S.between(1, 65535),
  S.description('Must be a valid port number between 1 and 65535')
);

/**
 * Schema for full node/service endpoints, including an optional explicit port.
 */
export const EndpointSchema = S.String.pipe(
  S.pattern(/^https?:\/\/.+(:\d+)?$/),
  S.description('Must be a valid HTTP/HTTPS URL with optional port')
);
