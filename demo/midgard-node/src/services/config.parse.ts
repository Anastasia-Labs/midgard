import { Config } from "effect";

import {
  positiveFiniteNumber,
  positiveSafeInteger,
} from "../artifact-schema.js";

/**
 * The value shapes the node settings tables share. Each reads one setting and
 * takes `defaultValue` when it is unset. `integerConfig` and `stringConfig`
 * check only the value's type; the others also refuse config load, naming the
 * setting, when the value is out of range.
 */

export const integerConfig = (name: string, defaultValue: number) =>
  Config.integer(name).pipe(Config.withDefault(defaultValue));

export const stringConfig = (name: string, defaultValue: string) =>
  Config.string(name).pipe(Config.withDefault(defaultValue));

export const positiveSafeIntegerConfig = (name: string, defaultValue: number) =>
  integerConfig(name, defaultValue).pipe(
    Config.mapAttempt((value) => positiveSafeInteger(value, name)),
  );

export const nonNegativeSafeIntegerConfig = (
  name: string,
  defaultValue: number,
) =>
  integerConfig(name, defaultValue).pipe(
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value < 0) {
        throw new Error(`${name} must be a non-negative safe integer`);
      }
      return value;
    }),
  );

export const positiveFiniteNumberConfig = (
  name: string,
  defaultValue: number,
) =>
  Config.number(name).pipe(
    Config.withDefault(defaultValue),
    Config.mapAttempt((value) => positiveFiniteNumber(value, name)),
  );

/** An integer setting refused above `max`. */
export const cappedPositiveSafeIntegerConfig = (
  name: string,
  defaultValue: number,
  max: number,
) =>
  positiveSafeIntegerConfig(name, defaultValue).pipe(
    Config.mapAttempt((value) => {
      if (value > max) {
        throw new Error(`${name} must be <= ${max.toString()}`);
      }
      return value;
    }),
  );
