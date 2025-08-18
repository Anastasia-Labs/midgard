import * as S from '@effect/schema/Schema';

// URL validation schema
export const URLSchema = S.String.pipe(
  S.pattern(/^https?:\/\/.+/),
  S.description('Must be a valid HTTP/HTTPS URL')
);

// Port validation schema
export const PortSchema = S.Number.pipe(
  S.between(1, 65535),
  S.description('Must be a valid port number between 1 and 65535')
);

// Endpoint validation (combines URL and optional port)
export const EndpointSchema = S.String.pipe(
  S.pattern(/^https?:\/\/.+(:\d+)?$/),
  S.description('Must be a valid HTTP/HTTPS URL with optional port')
);
