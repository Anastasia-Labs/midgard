import { createScheduledGenerator, MidgardNodeClient } from '@midgard-manager/tx-generator';
import { Effect, pipe } from 'effect';

import type { Config, GeneratorConfig } from '../config/schema.js';

// Error types
export class GeneratorError {
  readonly _tag = 'GeneratorError';
  constructor(readonly message: string) {}
}

// Service interface
export interface GeneratorService {
  generateBatch: (config: GeneratorConfig) => Effect.Effect<never, GeneratorError, void>;
  startScheduled: (config: GeneratorConfig) => Effect.Effect<never, GeneratorError, void>;
}

// Service implementation
export const makeGeneratorService = (
  config: Config
): Effect.Effect<never, never, GeneratorService> =>
  Effect.succeed({
    generateBatch: (genConfig) =>
      pipe(
        Effect.tryPromise({
          try: async () => {
            const nodeClient = new MidgardNodeClient(config.nodeUrl);

            const generator = await createScheduledGenerator({
              nodeClient,
              network: config.network,
              walletSeedOrPrivateKey: config.walletKey,
              intervalMs: 0, // Single batch
              transactionConfig: {
                oneToOne: {
                  transactionsPerBatch: genConfig.batchSize ?? 10,
                },
                multiOutput: {
                  transactionsPerBatch: genConfig.batchSize ?? 10,
                },
              },
              outputDir: config.outputDir,
            });

            await generator.start();
          },
          catch: (error) => new GeneratorError(String(error)),
        })
      ),

    startScheduled: (genConfig) =>
      pipe(
        Effect.tryPromise({
          try: async () => {
            const nodeClient = new MidgardNodeClient(config.nodeUrl);

            const generator = await createScheduledGenerator({
              nodeClient,
              network: config.network,
              walletSeedOrPrivateKey: config.walletKey,
              intervalMs: (genConfig.interval ?? 5) * 1000,
              transactionConfig: {
                oneToOne: {
                  transactionsPerBatch: genConfig.batchSize ?? 10,
                },
                multiOutput: {
                  transactionsPerBatch: genConfig.batchSize ?? 10,
                },
              },
              outputDir: config.outputDir,
            });

            await generator.start();
          },
          catch: (error) => new GeneratorError(String(error)),
        })
      ),
  });
