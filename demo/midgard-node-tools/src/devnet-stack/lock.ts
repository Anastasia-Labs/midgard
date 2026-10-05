import {
  closeSync,
  existsSync,
  openSync,
  readFileSync,
  unlinkSync,
  writeSync,
} from "node:fs";

import {
  isSqliteMutexBusy,
  SqliteProcessMutex,
} from "@al-ft/midgard-core/sqlite-process-mutex";

export class ControllerLockBusy extends Error {}

const alive = (pid: number) => {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
};

/** The kernel's start time of `pid` (field 22 of /proc/<pid>/stat), if readable. */
export const processStartTime = (pid: number): string | undefined => {
  try {
    const stat = readFileSync(`/proc/${pid}/stat`, "utf8");
    // The command name (field 2) may hold spaces and parentheses.
    return stat.slice(stat.lastIndexOf(")") + 2).split(" ")[19];
  } catch {
    return undefined;
  }
};

const ownerRecord = (pid: number) =>
  `${pid} ${processStartTime(pid) ?? ""}`.trim();

/**
 * The live process a lock file names, or undefined. A lock records its
 * owner's PID and start time, so a PID the kernel has since given to another
 * process (after a SIGKILL or a host restart) never counts as the owner. A
 * lock written with a PID only is judged by the PID alone.
 */
export const lockOwner = (path: string): number | undefined => {
  if (!existsSync(path)) return undefined;
  const [pidText, startTime] = readFileSync(path, "utf8").trim().split(" ");
  const pid = Number(pidText);
  if (!Number.isSafeInteger(pid) || pid <= 0 || !alive(pid)) return undefined;
  return startTime === undefined || startTime === processStartTime(pid)
    ? pid
    : undefined;
};

/**
 * One controller per run directory. The lock file holds the owner's PID and
 * start time; a lock whose owner has exited is stale and is taken over.
 * The persistent SQLite sidecar owns the entire operation, including stale
 * takeover. Never unlink/replace that sidecar while controllers can run.
 */
export const acquireLock = (path: string): (() => void) => {
  const mutexPath = `${path}.mutex.sqlite`;
  // Preserve filesystem permission/missing-directory errors, and create the
  // sidecar with private permissions. Do not open/close an existing SQLite
  // file outside its VFS: that could release this process's POSIX locks.
  try {
    closeSync(openSync(mutexPath, "wx", 0o600));
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "EEXIST") throw error;
  }
  let mutex: SqliteProcessMutex;
  try {
    mutex = SqliteProcessMutex.acquire(mutexPath);
  } catch (error) {
    if (isSqliteMutexBusy(error))
      throw new ControllerLockBusy(`another controller holds ${path}`);
    throw error;
  }
  try {
    return acquireOwnedLock(path, mutex);
  } catch (error) {
    mutex.close();
    throw error;
  }
};

const acquireOwnedLock = (
  path: string,
  mutex: SqliteProcessMutex,
): (() => void) => {
  const record = ownerRecord(process.pid);
  for (let attempt = 0; attempt < 2; attempt += 1) {
    try {
      const fd = openSync(path, "wx", 0o600);
      try {
        writeSync(fd, record);
      } finally {
        closeSync(fd);
      }
      let released = false;
      const release = () => {
        if (released) return;
        released = true;
        process.removeListener("exit", release);
        try {
          if (existsSync(path) && readFileSync(path, "utf8") === record)
            unlinkSync(path);
        } finally {
          mutex.close();
        }
      };
      process.once("exit", release);
      return release;
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code !== "EEXIST") throw error;
      try {
        const owner = lockOwner(path);
        if (owner !== undefined)
          throw new ControllerLockBusy(
            `another controller (pid ${owner}) holds ${path}`,
          );
        unlinkSync(path);
      } catch (error) {
        // A live legacy owner can release metadata without our new mutex.
        // Only that exact metadata disappearance is a retryable transition.
        if ((error as NodeJS.ErrnoException).code === "ENOENT")
          throw new ControllerLockBusy(
            `lock owner changed while acquiring ${path}`,
          );
        throw error;
      }
    }
  }
  throw new Error(`could not acquire ${path}`);
};
