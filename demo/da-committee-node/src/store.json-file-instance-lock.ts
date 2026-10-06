import { randomUUID } from "node:crypto";
import { readFile, writeFile } from "node:fs/promises";
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
  private released = false;

  private constructor(
    readonly lockPath: string,
    readonly owner: string,
    private readonly stamp: string,
    private readonly mutex: JsonStoreProcessMutex,
    private readonly onLost: ((error: Error) => void) | undefined,
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
      await writeFile(lockPath, stamp, { mode: 0o600 });
      const lock = new JsonStoreInstanceLock(
        lockPath,
        owner,
        stamp,
        mutex,
        options.onLost,
      );
      lock.timer = setInterval(() => {
        // A failed read is retried on the next interval; a write reports it.
        lock.assertHeld().catch(() => undefined);
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
    if (this.lost !== undefined) throw this.lost;
    if (this.released)
      throw new Error(
        `committee node file store instance lock was released: ${this.lockPath}`,
      );
    const current = await readFile(this.lockPath, "utf8").catch(
      (error: unknown) => {
        if (isNodeError(error) && error.code === "ENOENT") return undefined;
        throw error;
      },
    );
    if (current !== this.stamp) throw this.markLost();
  }

  /** Stops checking and drops the mutex; the stamp stays for the next holder. */
  release(): Promise<void> {
    if (this.timer !== undefined) clearInterval(this.timer);
    this.timer = undefined;
    if (!this.released) {
      this.released = true;
      this.mutex.close();
    }
    return Promise.resolve();
  }

  /** Records the loss, reporting it once, and returns the error writes get. */
  private markLost(): Error {
    if (this.lost !== undefined) return this.lost;
    this.lost = new Error(
      `committee node file store instance lock was taken over by another process: ${this.lockPath} no longer holds this process's stamp. ` +
        `The store's process mutex (${this.lockPath}.mutex.sqlite) admits one holder at a time, so its locking failed: ` +
        "the sidecar was removed or replaced, the store is on a network filesystem or shared across kernels or virtual machines, " +
        "or this process opened and closed the sidecar by other means. " +
        'Every write is refused; see "JSON store ownership" in the da-committee-node README.',
    );
    if (this.timer !== undefined) clearInterval(this.timer);
    this.timer = undefined;
    this.onLost?.(this.lost);
    return this.lost;
  }
}
