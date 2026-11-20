import { readFile } from "node:fs/promises";
import { isAbsolute, resolve } from "node:path";

import dotenv from "dotenv";

export type E2EEnvInheritance = "process" | "none";

export type E2EEnvFileProvenance = {
  readonly path: string;
  readonly keys: readonly string[];
};

export type E2EEnvProvenance = {
  readonly inheritance: E2EEnvInheritance;
  readonly envFiles: readonly E2EEnvFileProvenance[];
  readonly overrideKeys: readonly string[];
  readonly explicitEnvKeys: readonly string[];
};

export type BuildE2EProcessEnvOptions = {
  readonly cwd: string;
  readonly envFiles?: readonly string[];
  readonly overrides?: Readonly<Record<string, string | undefined>>;
  readonly inherit?: E2EEnvInheritance;
  readonly baseEnv?: NodeJS.ProcessEnv;
};

export type BuiltE2EProcessEnv = {
  readonly env: NodeJS.ProcessEnv;
  readonly provenance: E2EEnvProvenance;
};

const SECRET_KEY_PATTERN =
  /(seed|secret|private|password|passphrase|api[_-]?key|blockfrost|admin[_-]?key|token)/i;

export const parseEnvOverride = (entry: string): readonly [string, string] => {
  const separator = entry.indexOf("=");
  if (separator <= 0) {
    throw new Error("--env entries must use KEY=VALUE");
  }
  const key = entry.slice(0, separator);
  if (!/^[A-Za-z_][A-Za-z0-9_]*$/.test(key)) {
    throw new Error(`invalid environment variable name: ${key}`);
  }
  return [key, entry.slice(separator + 1)];
};

export const parseEnvOverrides = (
  entries: readonly string[] = [],
): Record<string, string> =>
  Object.fromEntries(entries.map((entry) => parseEnvOverride(entry)));

export const redactEnvKeys = (
  env: Readonly<Record<string, unknown>> = {},
): readonly string[] =>
  Object.keys(env)
    .sort((left, right) => left.localeCompare(right))
    .map((key) => (SECRET_KEY_PATTERN.test(key) ? `${key}=<redacted>` : key));

export const loadDotenvFile = async (
  path: string,
): Promise<Readonly<Record<string, string>>> =>
  dotenv.parse(await readFile(path, "utf8"));

export const buildE2EProcessEnv = async ({
  cwd,
  envFiles = [],
  overrides = {},
  inherit = "process",
  baseEnv = process.env,
}: BuildE2EProcessEnvOptions): Promise<BuiltE2EProcessEnv> => {
  const env: NodeJS.ProcessEnv = inherit === "process" ? { ...baseEnv } : {};
  const explicitKeys = new Set<string>();
  const envFileProvenance: E2EEnvFileProvenance[] = [];

  for (const envFile of envFiles) {
    const path = isAbsolute(envFile) ? envFile : resolve(cwd, envFile);
    const parsed = await loadDotenvFile(path);
    for (const [key, value] of Object.entries(parsed)) {
      env[key] = value;
      explicitKeys.add(key);
    }
    envFileProvenance.push({
      path,
      keys: redactEnvKeys(parsed),
    });
  }

  for (const [key, value] of Object.entries(overrides)) {
    explicitKeys.add(key);
    if (value === undefined) {
      delete env[key];
    } else {
      env[key] = value;
    }
  }

  return {
    env,
    provenance: {
      inheritance: inherit,
      envFiles: envFileProvenance,
      overrideKeys: redactEnvKeys(overrides),
      explicitEnvKeys: redactEnvKeys(
        Object.fromEntries([...explicitKeys].map((key) => [key, env[key]])),
      ),
    },
  };
};
