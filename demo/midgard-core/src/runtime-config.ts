import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import { parseDocument } from "yaml";

/** Load component-local settings without exposing YAML source in diagnostics. */
export const loadRuntimeConfig = ({
  env = process.env,
  cwd = process.cwd(),
}: {
  readonly env?: NodeJS.ProcessEnv;
  readonly cwd?: string;
} = {}): void => {
  const mode = env.MIDGARD_CONFIG_MODE ?? "enabled";
  if (mode === "disabled") return;
  if (mode !== "enabled") {
    throw new Error("MIDGARD_CONFIG_MODE must be enabled or disabled");
  }
  const configuredPath = env.MIDGARD_CONFIG_FILE;
  if (configuredPath === undefined && env.MIDGARD_DOTENV_MODE === "disabled") {
    return;
  }
  if (configuredPath !== undefined && configuredPath.trim().length === 0) {
    throw new Error("MIDGARD_CONFIG_FILE must name a file");
  }
  let source: string;
  try {
    source = readFileSync(
      resolve(cwd, configuredPath ?? "config.yaml"),
      "utf8",
    );
  } catch (error) {
    if (
      configuredPath === undefined &&
      (error as NodeJS.ErrnoException).code === "ENOENT"
    ) {
      return;
    }
    throw new Error(
      "Cannot read component config.yaml; check MIDGARD_CONFIG_FILE",
    );
  }

  let value: unknown;
  try {
    const document = parseDocument(source, { prettyErrors: false });
    if (document.errors.length > 0 || document.warnings.length > 0) {
      throw new Error();
    }
    value = document.toJS({ maxAliasCount: 0 });
  } catch {
    // YAML diagnostics can quote the offending line, including wallet seeds.
    throw new Error("Invalid component config.yaml syntax (contents redacted)");
  }
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(
      "Component config.yaml must be a mapping of setting names to strings",
    );
  }
  const entries = Object.entries(value);
  for (const [key, entry] of entries) {
    if (
      !/^[A-Z][A-Z0-9_]*$/u.test(key) ||
      [
        "MIDGARD_CONFIG_FILE",
        "MIDGARD_CONFIG_MODE",
        "MIDGARD_DOTENV_MODE",
      ].includes(key) ||
      typeof entry !== "string" ||
      entry.trim().length === 0
    ) {
      throw new Error(
        "Component config.yaml requires uppercase setting names and nonempty string values; loader controls belong in the environment",
      );
    }
  }
  // Validate the entire document before changing the environment. Explicit
  // process settings take precedence over file settings, including empty ones.
  for (const [key, entry] of entries) {
    if (env[key] === undefined) env[key] = entry as string;
  }
};
