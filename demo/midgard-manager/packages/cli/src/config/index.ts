import { existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';

import * as S from '@effect/schema/Schema';
import { Effect } from 'effect';
import { dirname } from 'path';
import { fileURLToPath } from 'url';

import { MidgardError } from '../utils/errors.js';
import { configSchema, defaultConfig, type MidgardConfig } from './schema.js';

/**
 * Monorepo-relative configuration paths for the manager CLI.
 */
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const MONOREPO_ROOT = join(__dirname, '../../../../../..');
const PROJECT_ROOT = join(MONOREPO_ROOT, 'demo/midgard-manager');

const CONFIG_DIR = join(PROJECT_ROOT, 'config');
const CONFIG_PATH = join(CONFIG_DIR, 'settings.json');

/**
 * Ensures the configuration directory exists before any read or write.
 */
const ensureConfigDir = Effect.try({
  try: () => {
    if (!existsSync(CONFIG_DIR)) {
      mkdirSync(CONFIG_DIR, { recursive: true });
    }
  },
  catch: (error) => {
    throw MidgardError.config(`Failed to create config directory: ${error}`);
  },
});

/**
 * Loads the CLI configuration from disk, creating the default file when
 * missing.
 */
export const loadConfig = Effect.gen(function* (_) {
  yield* _(ensureConfigDir);

  return yield* _(
    Effect.try({
      try: () => {
        if (!existsSync(CONFIG_PATH)) {
          writeFileSync(CONFIG_PATH, JSON.stringify(defaultConfig, null, 2));
          return defaultConfig;
        }

        const configFile = readFileSync(CONFIG_PATH, 'utf-8');
        const parsedConfig = JSON.parse(configFile);
        return S.decodeSync(configSchema)(parsedConfig);
      },
      catch: (error) => {
        throw MidgardError.config(`Failed to load config: ${error}`);
      },
    })
  );
});

/**
 * Persists the full CLI configuration to disk.
 */
export const saveConfig = (config: MidgardConfig) =>
  Effect.gen(function* (_) {
    yield* _(ensureConfigDir);

    return yield* _(
      Effect.try({
        try: () => {
          writeFileSync(CONFIG_PATH, JSON.stringify(config, null, 2));
          return config;
        },
        catch: (error) => {
          throw MidgardError.config(`Failed to save config: ${error}`);
        },
      })
    );
  });
