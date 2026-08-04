import { resolve } from "node:path";

import dotenv from "dotenv";

export const MIDGARD_DOTENV_MODE_ENV = "MIDGARD_DOTENV_MODE";
export const MIDGARD_DOTENV_DISABLED = "disabled";
const MIDGARD_DOTENV_ENABLED = "enabled";

/**
 * Loads the checkout .env for ordinary interactive commands. Isolated process
 * harnesses set MIDGARD_DOTENV_MODE=disabled so a missing child variable can
 * never be silently backfilled from the developer's checkout.
 */
export const loadRuntimeDotenv = ({
  env = process.env,
  cwd = process.cwd(),
}: {
  readonly env?: NodeJS.ProcessEnv;
  readonly cwd?: string;
} = {}): void => {
  const mode = env[MIDGARD_DOTENV_MODE_ENV] ?? MIDGARD_DOTENV_ENABLED;
  if (mode === MIDGARD_DOTENV_DISABLED) return;
  if (mode !== MIDGARD_DOTENV_ENABLED) {
    throw new Error(
      `${MIDGARD_DOTENV_MODE_ENV} must be ${MIDGARD_DOTENV_ENABLED} or ${MIDGARD_DOTENV_DISABLED}`,
    );
  }
  dotenv.config({
    path: resolve(cwd, ".env"),
    processEnv: env as Record<string, string>,
  });
};
