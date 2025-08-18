import * as S from '@effect/schema/Schema';

// Schema for our configuration
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

// Export type
export type MidgardConfig = S.Schema.Type<typeof configSchema>;

// Default configuration
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

// Configuration errors
export class ConfigError {
  readonly _tag = 'ConfigError';
  constructor(
    readonly message: string,
    readonly cause?: unknown
  ) {}
}
