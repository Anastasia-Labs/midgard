import { type Env } from "./config.committee-config.js";

export const requireEnv = (env: Env, name: string): string => {
  const value = env[name];
  if (value === undefined || value.trim() === "") {
    throw new Error(`${name} is required`);
  }
  return value.trim();
};

export const optionalNonEmpty = (
  value: string | undefined,
): string | undefined => {
  const trimmed = value?.trim();
  return trimmed === undefined || trimmed === "" ? undefined : trimmed;
};

export const optionalKeySource = (
  value: string | undefined,
  name: string,
): string | undefined => {
  const source = optionalNonEmpty(value);
  if (source === undefined) {
    return undefined;
  }
  validateKeySourceSyntax(source, name);
  return source;
};

const validateKeySourceSyntax = (source: string, name: string): void => {
  const prefixedSources = [
    "file:",
    "seed:",
    "mnemonic:",
    "private-key:",
    "privateKey:",
  ];
  for (const prefix of prefixedSources) {
    if (source === prefix) {
      throw new Error(`${name} must include a value after ${prefix}`);
    }
  }
};

export const splitList = (value: string): readonly string[] => {
  const values = value
    .split(",")
    .map((part) => part.trim())
    .filter((part) => part.length > 0);
  if (values.length === 0) {
    throw new Error("expected a non-empty comma-separated list");
  }
  return values;
};

export const optionalSplitList = (
  value: string | undefined,
): readonly string[] => {
  const trimmed = optionalNonEmpty(value);
  return trimmed === undefined ? [] : splitList(trimmed);
};

export const booleanEnv = (
  value: string | undefined,
  defaultValue: boolean,
): boolean => {
  const normalized = value?.trim().toLowerCase();
  if (normalized === undefined || normalized === "") {
    return defaultValue;
  }
  if (["1", "true", "yes", "on"].includes(normalized)) {
    return true;
  }
  if (["0", "false", "no", "off"].includes(normalized)) {
    return false;
  }
  throw new Error("boolean environment values must be true or false");
};
