import * as S from '@effect/schema/Schema';

/**
 * Runtime schema for the manager CLI configuration file.
 */
export const configSchema = S.Struct({
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

/**
 * TypeScript view of the validated CLI configuration.
 */
export type MidgardConfig = S.Schema.Type<typeof configSchema>;

/**
 * Default configuration written on first startup.
 */
export const defaultConfig: MidgardConfig = {
  node: {
    endpoint: 'http://localhost:3000',
  },
  generator: {
    enabled: false,
    maxConcurrent: 10,
    batchSize: 100,
    intervalMs: 1000,
  },
  logging: {
    level: 'info',
    format: 'pretty',
  },
};

/**
 * Simple configuration error used by non-Effect call sites.
 */
export class ConfigError {
  readonly _tag = 'ConfigError';
  constructor(
    readonly message: string,
    readonly cause?: unknown
  ) {}
}
