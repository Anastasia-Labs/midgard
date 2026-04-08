import * as S from '@effect/schema/Schema';

/**
 * Configuration schema shared across CLI entrypoints.
 *
 * Keeping the schema close to the exported runtime type makes it easier to
 * reason about what can be loaded from disk, validated at startup, and passed
 * through interactive flows.
 */

/**
 * Lightweight URL validation helper reserved for config-related parsing flows.
 */
const isValidUrl = (url: string) => {
  try {
    new URL(url);
    return true;
  } catch {
    return false;
  }
};

/**
 * Runtime schema for the persisted manager configuration file.
 *
 * The shape intentionally stays compact: node connectivity, generator runtime
 * controls, and logging are the only mutable knobs the current CLI persists.
 */
export const MidgardConfig = S.Struct({
  // Node configuration
  node: S.Struct({
    endpoint: S.String.pipe(S.pattern(/^https?:\/\/.+/)),
  }),

  // Transaction generator configuration
  generator: S.Struct({
    enabled: S.Boolean,
    maxConcurrent: S.Number.pipe(S.positive(), S.int()),
    batchSize: S.Number.pipe(S.positive(), S.int()),
    intervalMs: S.Number.pipe(S.positive(), S.int()),
  }),

  // Logging configuration
  logging: S.Struct({
    level: S.Literal('debug', 'info', 'warn', 'error'),
    format: S.Literal('json', 'pretty'),
  }),
});

void isValidUrl;

/**
 * Static TypeScript view of {@link MidgardConfig}.
 */
export type MidgardConfig = S.Schema.To<typeof MidgardConfig>;
