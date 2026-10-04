import { spawnSync } from "node:child_process";
import { delimiter } from "node:path";
import { fileURLToPath } from "node:url";

export const pinnedPnpmEnvironment = (env) => ({
  ...env,
  PATH: `${fileURLToPath(new URL("../bin", import.meta.url))}${delimiter}${env.PATH ?? ""}`,
});

// Corepack selects the immutable packageManager pin from this working
// directory. A pnpm 10 docs process must not run the pnpm 9 demo recipes.
export const pinnedPnpm = (cwd, args) => ({
  argv: ["corepack", "pnpm", ...args],
  cwd,
});
export const pinnedPnpmVersion = (cwd) => {
  const step = pinnedPnpm(cwd, ["--version"]);
  const result = spawnSync(step.argv[0], step.argv.slice(1), {
    cwd: step.cwd,
    encoding: "utf8",
    timeout: 10_000,
  });
  if (result.error)
    return {
      error: result.error.message,
      missing: result.error.code === "ENOENT",
    };
  if (result.status !== 0)
    return {
      error: `corepack pnpm --version exited ${result.status}: ${result.stderr.trim()}`,
    };
  return { version: result.stdout.trim().split("\n").at(-1) };
};
