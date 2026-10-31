import { spawn } from "node:child_process";
import { readdir, readlink } from "node:fs/promises";
import { join, resolve } from "node:path";

/** flock exits with this code only when another process holds the lock. */
export const LOCK_CONFLICT_EXIT_CODE = 75;
export const CONTROLLER_LOCK_ENV = "MIDGARD_STACK_CONTROLLER_LOCK";

/** Re-runs `argv` under `flock --no-fork`, so the controller itself holds the lock until it exits. */
export async function runUnderControllerLock(
  lock: string,
  argv: readonly string[],
  env: NodeJS.ProcessEnv,
): Promise<void> {
  const exitCode = await new Promise<number>((resolvePromise, reject) => {
    const child = spawn(
      "flock",
      [
        "--nonblock",
        "--no-fork",
        "--conflict-exit-code",
        String(LOCK_CONFLICT_EXIT_CODE),
        lock,
        process.execPath,
        ...argv,
      ],
      { stdio: "inherit", env: { ...env, [CONTROLLER_LOCK_ENV]: lock } },
    );
    child.once("error", reject);
    child.once("exit", (code, signal) =>
      signal
        ? reject(new Error(`Stack controller stopped: ${signal}`))
        : resolvePromise(code ?? 1),
    );
  });
  if (exitCode === LOCK_CONFLICT_EXIT_CODE)
    throw new Error(`Another stack controller holds ${lock}`);
  if (exitCode !== 0)
    throw new Error(
      `Stack controller failed (exit ${exitCode}); see the error above`,
    );
}

/** The inherited marker alone proves nothing: require an open descriptor on the lock file. */
export async function assertHoldsControllerLock(
  lock: string,
  pid: number = process.pid,
) {
  const directory = `/proc/${pid}/fd`;
  const targets = await Promise.all(
    (await readdir(directory)).map((fd) =>
      readlink(join(directory, fd)).catch(() => ""),
    ),
  );
  if (!targets.includes(resolve(lock)))
    throw new Error(
      `${CONTROLLER_LOCK_ENV} is set, but this process does not hold ${lock}`,
    );
}
