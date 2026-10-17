import { randomUUID } from "node:crypto";
import {
  mkdirSync,
  readFileSync,
  renameSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { dirname, resolve } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

/** Publish a lock directory holding this process's PID, or return false. */
const tryLock = (lock) => {
  const staged = `${lock}.${process.pid}.${randomUUID()}`;
  mkdirSync(staged);
  writeFileSync(resolve(staged, "pid"), `${process.pid}\n`);
  try {
    // rename(2) refuses a non-empty target, so exactly one contender wins,
    // and the lock never exists without its holder's PID.
    renameSync(staged, lock);
    return true;
  } catch (error) {
    rmSync(staged, { recursive: true, force: true });
    if (error.code === "ENOTEMPTY" || error.code === "EEXIST") return false;
    throw error;
  }
};

/** Move the lock aside before deleting it, so no contender sees it half-removed. */
const discardLock = (lock) => {
  const discarded = `${lock}.discarded.${randomUUID()}`;
  try {
    renameSync(lock, discarded);
  } catch (error) {
    if (error.code === "ENOENT") return;
    throw error;
  }
  rmSync(discarded, { recursive: true, force: true });
};

const lockHolder = (lock) => {
  try {
    return Number.parseInt(readFileSync(resolve(lock, "pid"), "utf8"), 10);
  } catch (error) {
    if (error.code === "ENOENT" || error.code === "ENOTDIR") return undefined;
    throw error;
  }
};

const running = (pid) => {
  try {
    process.kill(pid, 0);
    return true;
  } catch (error) {
    return error.code === "EPERM";
  }
};

/**
 * A process killed mid-build leaves its lock behind. Contenders break it one
 * at a time, re-reading the holder under the guard so a lock another
 * contender has just re-taken is never broken.
 */
const breakStaleLock = (lock) => {
  const guard = `${lock}.break`;
  try {
    mkdirSync(guard);
  } catch (error) {
    if (error.code === "EEXIST") return;
    throw error;
  }
  try {
    const holder = lockHolder(lock);
    if (holder !== undefined && !running(holder)) discardLock(lock);
  } finally {
    rmSync(guard, { recursive: true, force: true });
  }
};

/**
 * Runs `build` while holding the lock directory `lock`, unless `ready()`
 * holds first: every process that needs the same build output waits for the
 * one building it and then finds it ready, instead of building it again or
 * reading it half-written. A lock whose holder has exited is broken; a live
 * holder is waited on for at most `timeoutMs`. Resolves to whether this
 * process built.
 */
export const withBuildLock = async (
  { lock, ready, timeoutMs, pollMs = 200 },
  build,
) => {
  mkdirSync(dirname(lock), { recursive: true });
  const deadline = Date.now() + timeoutMs;
  for (;;) {
    if (ready()) return false;
    if (tryLock(lock)) break;
    breakStaleLock(lock);
    if (Date.now() >= deadline)
      throw new Error(
        `Timed out after ${String(timeoutMs / 1000)} s waiting for build lock ${lock} held by PID ${String(lockHolder(lock))}`,
      );
    await pause(pollMs);
  }
  try {
    if (ready()) return false;
    await build();
    return true;
  } finally {
    discardLock(lock);
  }
};
