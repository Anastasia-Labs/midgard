import { Effect, Logger, LogLevel } from 'effect';

import type { MidgardConfig } from './config/schema.js';

// Convert log level to Effect's LogLevel
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

// Initialize logger with given configuration
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

// Logging utility functions
export const log = {
  debug: (message: string) => Logger.debug(message),
  info: (message: string) => Logger.info(message),
  warn: (message: string) => Logger.warn(message),
  error: (message: string) => Logger.error(message),
};
