import { Effect, Logger, LogLevel } from 'effect';

import type { MidgardConfig } from './config/schema.js';

/**
 * Converts the CLI's config-level log setting into Effect's log-level enum.
 */
const toEffectLogLevel = (level: MidgardConfig['logging']['level']): LogLevel => {
  switch (level) {
    case 'debug':
      return LogLevel.Debug;
    case 'info':
      return LogLevel.Info;
    case 'warn':
      return LogLevel.Warning;
    case 'error':
      return LogLevel.Error;
  }
};

/**
 * Creates a logger instance honoring the configured level and output format.
 */
export const createLogger = (config: MidgardConfig) => {
  const logLevel = toEffectLogLevel(config.logging.level);

  const logger = Logger.make(({ logLevel: messageLevel, message }) => {
    if (messageLevel >= logLevel) {
      const timestamp = new Date().toISOString();

      if (config.logging.format === 'json') {
        console.log(
          JSON.stringify({
            timestamp,
            level: LogLevel.toString(messageLevel),
            message,
          })
        );
      } else {
        console.log(`[${timestamp}] ${LogLevel.toString(messageLevel).padEnd(5)} ${message}`);
      }
    }

    return Effect.unit;
  });

  return logger;
};

/**
 * Convenience wrappers around Effect's global logging helpers.
 */
export const log = {
  debug: (message: string) => Logger.debug(message),
  info: (message: string) => Logger.info(message),
  warn: (message: string) => Logger.warn(message),
  error: (message: string) => Logger.error(message),
};
