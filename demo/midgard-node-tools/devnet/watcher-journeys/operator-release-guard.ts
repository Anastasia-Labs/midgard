import { spawn } from "node:child_process";

import { projectOnlyEnvironment } from "./watcher-evidence-reader.js";

/**
 * Starts operator-2's node from outside this process when this process dies
 * still holding it: a signal, a test-timeout kill or a crash runs no
 * `afterAll` and no session close, and would leave operator-2's node stopped
 * after the run. The guard is its own process, in its own process group, so
 * the signal that ends the run does not reach it. It checks `holderPid`
 * every `pollMs`; once that process is gone it runs `startCommand` from `/`
 * with no `COMPOSE_*` variables, as the hold's own start does, and exits.
 * `disarm` ends it without starting anything, for a run that starts the
 * node itself.
 */
export const armOperatorReleaseGuard = ({
  startCommand,
  holderPid = process.pid,
  pollMs = 1000,
}: Readonly<{
  startCommand: readonly string[];
  holderPid?: number;
  pollMs?: number;
}>): Readonly<{ disarm(): void }> => {
  if (startCommand.length === 0)
    throw new Error("Operator-2's release guard needs a start command");
  const guard = spawn(
    process.execPath,
    [
      "-e",
      GUARD_SOURCE,
      String(holderPid),
      JSON.stringify(startCommand),
      String(pollMs),
    ],
    {
      cwd: "/",
      env: projectOnlyEnvironment(),
      detached: true,
      stdio: "ignore",
    },
  );
  guard.unref();
  let disarmed = false;
  return Object.freeze({
    disarm: () => {
      if (disarmed) return;
      disarmed = true;
      guard.kill("SIGTERM");
    },
  });
};

/** The guard process: plain Node, no imports from this package. */
const GUARD_SOURCE = `
const { spawnSync } = require("node:child_process");
const holder = Number(process.argv[1]);
const command = JSON.parse(process.argv[2]);
process.on("SIGTERM", () => process.exit(0));
const alive = () => {
  try {
    process.kill(holder, 0);
    return true;
  } catch (error) {
    return error.code === "EPERM";
  }
};
const timer = setInterval(() => {
  if (alive()) return;
  clearInterval(timer);
  spawnSync(command[0], command.slice(1), { cwd: "/", stdio: "ignore", timeout: 120000 });
  process.exit(0);
}, Number(process.argv[3]));
`;
