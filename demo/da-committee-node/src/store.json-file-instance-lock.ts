import { randomUUID } from "node:crypto";
import { readFile, rename, rm, writeFile } from "node:fs/promises";
import { hostname } from "node:os";

import {
  isSqliteMutexBusy,
  JsonStoreProcessMutex,
} from "./store.json-file-process-mutex.js";
import { isNodeError } from "./store.parse-stored-record-map.js";

/** How often a holder checks its stamp, so an idle holder still fails fast. */
export const JSON_STORE_INSTANCE_LOCK_CHECK_MS = 10_000;

export type JsonStoreInstanceLockOptions = {
  readonly checkMs?: number;
  /**
   * Called once when the lock file no longer holds this holder's stamp.
   * Every later write is refused, and the process must stop.
   */
  readonly onLost?: (error: Error) => void;
  /**
   * Where a failed idle check is reported, as one JSON line per distinct
   * error until a check succeeds again. Defaults to stderr.
   */
  readonly log?: (line: string) => void;
};

/** The store's process mutex is held by another live process. */
export class JsonStoreLeaseHeldError extends Error {
  override readonly name = "JsonStoreLeaseHeldError";
}

/**
 * Exclusive ownership of a JSON committee store. The SQLite process mutex,
 * held for the store's lifetime, is the only authority: holding it proves no
 * other cooperating writer is alive on this filesystem, so whatever an
 * earlier holder left in the lock file is overwritten unjudged. The lock file
 * is a write-once stamp naming the current holder, which the holder only ever
 * reads afterwards: it finds a successor's stamp there only if storage
 * locking failed and that successor got the mutex too, and it then refuses
 * every write.
 */
export class JsonStoreInstanceLock {
  private timer: ReturnType<typeof setInterval> | undefined;
  private lost: Error | undefined;
  private released: Promise<void> | undefined;
  private readonly checks = new Set<Promise<void>>();
  private reportedCheckFailure: string | undefined;
  private idleCheckRunning = false;

  private constructor(
    readonly lockPath: string,
    readonly owner: string,
    private readonly stamp: string,
    private readonly mutex: JsonStoreProcessMutex,
    private readonly onLost: ((error: Error) => void) | undefined,
    private readonly log: (line: string) => void,
  ) {}

  /** Takes `<storePath>.lock`, under the mutex `<storePath>.lock.mutex.sqlite`. */
  static async acquire(
    storePath: string,
    options: JsonStoreInstanceLockOptions = {},
  ): Promise<JsonStoreInstanceLock> {
    const lockPath = `${storePath}.lock`;
    let mutex: JsonStoreProcessMutex;
    try {
      mutex = JsonStoreProcessMutex.acquire(`${lockPath}.mutex.sqlite`);
    } catch (error) {
      if (isSqliteMutexBusy(error))
        throw new JsonStoreLeaseHeldError(
          `committee node file store is already exclusively leased: ${lockPath}; its process mutex is held`,
        );
      throw error;
    }
    try {
      const owner = `${process.pid.toString()}:${randomUUID()}`;
      const stamp = `${JSON.stringify({
        owner,
        pid: process.pid,
        hostname: hostname(),
        acquiredAt: new Date().toISOString(),
      })}\n`;
      // Published by one rename, which replaces whatever is there even when
      // that file is read-only, and never leaves a torn stamp behind.
      const tmpPath = `${lockPath}.${owner.replace(":", "-")}.tmp`;
      try {
        await writeFile(tmpPath, stamp, { mode: 0o600 });
        await rename(tmpPath, lockPath);
      } catch (error) {
        await rm(tmpPath, { force: true }).catch(() => undefined);
        throw error;
      }
      const lock = new JsonStoreInstanceLock(
        lockPath,
        owner,
        stamp,
        mutex,
        options.onLost,
        options.log ?? ((line) => process.stderr.write(line)),
      );
      lock.timer = setInterval(() => {
        // A read still under way, as on a hung filesystem, skips this tick,
        // so reads never pile up. A failed read is retried on the next
        // interval; writes are refused until one succeeds.
        if (lock.idleCheckRunning) return;
        lock.idleCheckRunning = true;
        lock
          .assertHeld()
          .then(
            () => {
              lock.reportedCheckFailure = undefined;
            },
            (error: unknown) => lock.reportCheckFailure(error),
          )
          .finally(() => {
            lock.idleCheckRunning = false;
          });
      }, options.checkMs ?? JSON_STORE_INSTANCE_LOCK_CHECK_MS);
      lock.timer.unref?.();
      return lock;
    } catch (error) {
      mutex.close();
      throw error;
    }
  }

  /** Throws unless the lock file still holds exactly this holder's stamp. */
  async assertHeld(): Promise<void> {
    const check = this.check();
    const settled = check.then(
      () => undefined,
      () => undefined,
    );
    this.checks.add(settled);
    try {
      await check;
    } finally {
      this.checks.delete(settled);
    }
  }

  /**
   * Stops checking and drops the mutex once every check already under way
   * has finished; the stamp stays for the next holder.
   */
  release(): Promise<void> {
    if (this.timer !== undefined) clearInterval(this.timer);
    this.timer = undefined;
    this.released ??= (async () => {
      await Promise.all(this.checks);
      this.mutex.close();
    })();
    return this.released;
  }

  private async check(): Promise<void> {
    this.assertNotEnded();
    const current = await readFile(this.lockPath, "utf8").catch(
      (error: unknown) => {
        if (isNodeError(error) && error.code === "ENOENT") return undefined;
        throw error;
      },
    );
    // A successor may stamp the file as soon as this holder lets go, so a
    // check that read it after release proves nothing about a takeover.
    this.assertNotEnded();
    if (current === undefined)
      throw this.markLost(
        `committee node file store instance lock stamp was removed: ${this.lockPath} no longer exists. ` +
          "The holder never removes it, so something outside this process did. " +
          'Every write is refused; see "JSON store ownership" in the da-committee-node README.',
      );
    if (current !== this.stamp)
      throw this.markLost(
        `committee node file store instance lock was taken over by another process: ${this.lockPath} no longer holds this process's stamp. ` +
          `The store's process mutex (${this.lockPath}.mutex.sqlite) admits one holder at a time, so its locking failed: ` +
          "the sidecar was removed or replaced, the store is on a network filesystem or shared across kernels or virtual machines, " +
          "or this process opened and closed the sidecar by other means. " +
          'Every write is refused; see "JSON store ownership" in the da-committee-node README.',
      );
  }

  private assertNotEnded(): void {
    if (this.lost !== undefined) throw this.lost;
    if (this.released !== undefined)
      throw new Error(
        `committee node file store instance lock was released: ${this.lockPath}`,
      );
  }

  /** Logs a failed idle check once per distinct error; a loss has onLost. */
  private reportCheckFailure(error: unknown): void {
    if (this.lost !== undefined || this.released !== undefined) return;
    const message = error instanceof Error ? error.message : String(error);
    if (message === this.reportedCheckFailure) return;
    this.reportedCheckFailure = message;
    this.log(
      `${JSON.stringify({ event: "committee_store_instance_lock_check_failed", lockPath: this.lockPath, error: message })}\n`,
    );
  }

  /** Records the loss, reporting it once, and returns the error writes get. */
  private markLost(message: string): Error {
    if (this.lost !== undefined) return this.lost;
    this.lost = new Error(message);
    if (this.timer !== undefined) clearInterval(this.timer);
    this.timer = undefined;
    this.onLost?.(this.lost);
    return this.lost;
  }
}
