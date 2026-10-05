import { randomUUID } from "node:crypto";
import { readFileSync, readlinkSync } from "node:fs";
import {
  link,
  open as openFile,
  readFile,
  rename,
  stat,
  unlink,
} from "node:fs/promises";
import { hostname as osHostname } from "node:os";
import { dirname } from "node:path";

import {
  isSqliteMutexBusy,
  JsonStoreProcessMutex,
} from "./store.json-file-process-mutex.js";
import { isNodeError } from "./store.parse-stored-record-map.js";

/** How often a live holder renews its lease. */
export const JSON_STORE_LEASE_RENEW_MS = 10_000;
/**
 * How long a lease may go unrenewed before another process may take it over.
 * Several renewals long, so a holder that is merely slow is never displaced.
 * Takeover is examined only while the successor holds the process mutex, so
 * a live cooperating holder, which keeps the mutex, is never displaced.
 */
export const JSON_STORE_LEASE_STALE_MS = 60_000;

/** The lease a JSON committee store's lock file holds. */
export type JsonStoreLeaseRecord = {
  readonly schemaVersion: 2;
  readonly owner: string;
  readonly pid: number;
  readonly bootId?: string;
  /**
   * The holder's pid namespace (`/proc/self/ns/pid`). Its pid says whether it
   * is alive only to a process in the same namespace: containers sharing a
   * hostname and a boot id each number their own processes from 1.
   */
  readonly pidNamespace?: string;
  readonly hostname: string;
  readonly acquiredAt: string;
  readonly renewedAt: string;
};

export type JsonStoreLeaseOptions = {
  readonly renewMs?: number;
  readonly staleMs?: number;
  readonly now?: () => number;
  readonly isProcessAlive?: (pid: number) => boolean;
  readonly bootId?: () => string | undefined;
  readonly pidNamespace?: () => string | undefined;
  readonly hostname?: () => string;
  /**
   * Called once when the lease is found taken over by another process. Every
   * later write is refused, and the process must stop.
   */
  readonly onLost?: (error: Error) => void;
  /** Called when a lease left by a dead or stale holder is taken over. */
  readonly onTakeover?: (reason: string) => void;
};

/** The lock is held by a live, renewing holder, or one not provably gone. */
export class JsonStoreLeaseHeldError extends Error {
  override readonly name = "JsonStoreLeaseHeldError";
}

/** Leases held by this process, by owner, so a second open here is refused. */
const heldHere = new Set<string>();

const readBootId = (): string | undefined => {
  try {
    return readFileSync("/proc/sys/kernel/random/boot_id", "utf8").trim();
  } catch {
    return undefined;
  }
};

const readPidNamespace = (): string | undefined => {
  try {
    return readlinkSync("/proc/self/ns/pid");
  } catch {
    return undefined;
  }
};

const processAlive = (pid: number): boolean => {
  try {
    process.kill(pid, 0);
    return true;
  } catch (error) {
    return !(isNodeError(error) && error.code === "ESRCH");
  }
};

const parseLease = (raw: string): JsonStoreLeaseRecord | undefined => {
  try {
    const value = JSON.parse(raw) as Partial<JsonStoreLeaseRecord>;
    return value.schemaVersion === 2 &&
      typeof value.owner === "string" &&
      Number.isSafeInteger(value.pid) &&
      (value.bootId === undefined || typeof value.bootId === "string") &&
      (value.pidNamespace === undefined ||
        typeof value.pidNamespace === "string") &&
      typeof value.hostname === "string" &&
      typeof value.acquiredAt === "string" &&
      typeof value.renewedAt === "string"
      ? (value as JsonStoreLeaseRecord)
      : undefined;
  } catch {
    return undefined;
  }
};

/**
 * Why the holder of `raw` may be displaced, or undefined while it must not
 * be. A holder is displaced only when it is provably gone (same host: its
 * boot ended, or, in the same pid namespace, its pid is this process or is
 * not running) or its lease is stale. A holder whose pid namespace is
 * unknown or another one is judged by its lease alone. A lock file this
 * process cannot read (a torn write, a legacy lock) is judged by its
 * modification time alone. Callers examine it only while holding the
 * process mutex: holding it proves no cooperating writer is alive on this
 * filesystem, and it serializes concurrent successors.
 */
export const leaseTakeoverReason = (args: {
  readonly raw: string;
  readonly mtimeMs: number;
  readonly nowMs: number;
  readonly staleMs: number;
  readonly hostname: string;
  readonly bootId: string | undefined;
  readonly pidNamespace: string | undefined;
  readonly isProcessAlive: (pid: number) => boolean;
}): string | undefined => {
  const lease = parseLease(args.raw);
  const renewedAtMs = Math.max(
    args.mtimeMs,
    lease === undefined ? 0 : Date.parse(lease.renewedAt) || 0,
  );
  const stale = args.nowMs - renewedAtMs > args.staleMs;
  if (lease === undefined) {
    return stale ? "unreadable_lease_stale" : undefined;
  }
  if (heldHere.has(lease.owner)) {
    return undefined;
  }
  if (lease.hostname === args.hostname) {
    if (
      lease.bootId !== undefined &&
      args.bootId !== undefined &&
      lease.bootId !== args.bootId
    ) {
      return "holder_boot_ended";
    }
    const samePidNamespace =
      lease.pidNamespace !== undefined &&
      lease.pidNamespace === args.pidNamespace;
    if (samePidNamespace && lease.pid === process.pid) {
      return "holder_was_an_earlier_run_of_this_process";
    }
    if (
      samePidNamespace &&
      (lease.bootId === undefined || lease.bootId === args.bootId) &&
      !args.isProcessAlive(lease.pid)
    ) {
      return "holder_process_gone";
    }
  }
  return stale ? "lease_stale" : undefined;
};

/**
 * An exclusive, renewed lease on a JSON committee store, published atomically
 * under a SQLite process mutex held for the store lifetime.
 * Local filesystems with reliable SQLite/POSIX locking are required. Never
 * delete/replace the persistent mutex file while any holder may be alive.
 */
export class JsonStoreLease {
  private timer: ReturnType<typeof setInterval> | undefined;
  /**
   * Renewals and ownership checks run one at a time; metadata publishes
   * atomically and a close waits for any queued renewal before releasing.
   */
  private serial: Promise<void> = Promise.resolve();
  /** A renewal is waiting its turn; a slow one never piles up more. */
  private renewalQueued = false;
  private lost: Error | undefined;
  private releasePromise: Promise<void> | undefined;

  private constructor(
    readonly lockPath: string,
    readonly owner: string,
    private record: JsonStoreLeaseRecord,
    private readonly options: JsonStoreLeaseOptions,
    private readonly mutex: JsonStoreProcessMutex,
  ) {}

  static async acquire(
    lockPath: string,
    options: JsonStoreLeaseOptions = {},
  ): Promise<JsonStoreLease> {
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
      return await JsonStoreLease.acquireWithMutex(lockPath, options, mutex);
    } catch (error) {
      mutex.close();
      throw error;
    }
  }

  private static async acquireWithMutex(
    lockPath: string,
    options: JsonStoreLeaseOptions,
    mutex: JsonStoreProcessMutex,
  ): Promise<JsonStoreLease> {
    const now = options.now ?? Date.now;
    const host = (options.hostname ?? osHostname)();
    const bootId = (options.bootId ?? readBootId)();
    const pidNamespace = (options.pidNamespace ?? readPidNamespace)();
    for (let attempt = 0; attempt < 3; attempt += 1) {
      const present = await readFile(lockPath, "utf8").then(
        () => true,
        (error: unknown) => {
          if (isNodeError(error) && error.code === "ENOENT") return false;
          throw error;
        },
      );
      if (present) {
        await JsonStoreLease.displace(lockPath, {
          nowMs: now(),
          staleMs: options.staleMs ?? JSON_STORE_LEASE_STALE_MS,
          hostname: host,
          bootId,
          pidNamespace,
          isProcessAlive: options.isProcessAlive ?? processAlive,
          ...(options.onTakeover === undefined
            ? {}
            : { onTakeover: options.onTakeover }),
        });
        continue;
      }
      const at = new Date(now()).toISOString();
      const record: JsonStoreLeaseRecord = {
        schemaVersion: 2,
        owner: `${process.pid.toString()}:${randomUUID()}`,
        pid: process.pid,
        ...(bootId === undefined ? {} : { bootId }),
        ...(pidNamespace === undefined ? {} : { pidNamespace }),
        hostname: host,
        acquiredAt: at,
        renewedAt: at,
      };
      const lease = new JsonStoreLease(
        lockPath,
        record.owner,
        record,
        options,
        mutex,
      );
      try {
        await lease.writeRecord();
      } catch (error) {
        // The mutex still excludes all cooperating successors during cleanup.
        if (
          parseLease(await readFile(lockPath, "utf8").catch(() => ""))
            ?.owner === record.owner
        )
          await unlink(lockPath).catch(() => undefined);
        throw error;
      }
      heldHere.add(record.owner);
      lease.timer = setInterval(() => {
        if (lease.renewalQueued) return;
        lease.renewalQueued = true;
        void lease.exclusive(() => {
          lease.renewalQueued = false;
          return lease.renew();
        });
      }, options.renewMs ?? JSON_STORE_LEASE_RENEW_MS);
      lease.timer.unref?.();
      return lease;
    }
    throw new JsonStoreLeaseHeldError(
      `committee node file store is already exclusively leased: ${lockPath}; the lease kept changing hands while it was taken`,
    );
  }

  /** Moves a displaceable holder's lock aside, or throws while it is live. */
  private static async displace(
    lockPath: string,
    args: Omit<Parameters<typeof leaseTakeoverReason>[0], "raw" | "mtimeMs"> & {
      readonly onTakeover?: (reason: string) => void;
    },
  ): Promise<void> {
    let raw: string;
    let mtimeMs: number;
    try {
      [raw, mtimeMs] = await Promise.all([
        readFile(lockPath, "utf8"),
        stat(lockPath).then(({ mtimeMs: ms }) => ms),
      ]);
    } catch (error) {
      if (isNodeError(error) && error.code === "ENOENT") return;
      throw error;
    }
    const reason = leaseTakeoverReason({ ...args, raw, mtimeMs });
    if (reason === undefined) {
      throw new JsonStoreLeaseHeldError(
        `committee node file store is already exclusively leased: ${lockPath}; its holder is live and renewing (it is taken over once the holder is gone or its lease goes ${(args.staleMs / 1000).toString()} s unrenewed)`,
      );
    }
    const aside = `${lockPath}.displaced-${randomUUID()}`;
    try {
      await rename(lockPath, aside);
    } catch (error) {
      if (isNodeError(error) && error.code === "ENOENT") return;
      throw error;
    }
    // Another process may have taken the lock over between the read and the
    // rename; then the file moved aside is its live lease, and goes back.
    if ((await readFile(aside, "utf8")) !== raw) {
      await link(aside, lockPath).catch(() => undefined);
      await unlink(aside).catch(() => undefined);
      throw new JsonStoreLeaseHeldError(
        `committee node file store is already exclusively leased: ${lockPath}; another process took it over first`,
      );
    }
    await unlink(aside).catch(() => undefined);
    args.onTakeover?.(reason);
  }

  /** Throws unless the lock file still names this lease. */
  assertHeld(): Promise<void> {
    return this.exclusive(() => this.checkHeld());
  }

  private exclusive<T>(run: () => Promise<T>): Promise<T> {
    const result = this.serial.then(run);
    this.serial = result.then(
      () => undefined,
      () => undefined,
    );
    return result;
  }

  private async checkHeld(): Promise<void> {
    if (this.lost !== undefined) throw this.lost;
    const current = await readFile(this.lockPath, "utf8").catch(
      (error: unknown) => {
        if (isNodeError(error) && error.code === "ENOENT") return undefined;
        throw error;
      },
    );
    if (current === undefined || parseLease(current)?.owner !== this.owner) {
      this.markLost();
      throw this.lost!;
    }
  }

  async release(): Promise<void> {
    this.releasePromise ??= this.releaseOnce();
    await this.releasePromise;
  }

  private async releaseOnce(): Promise<void> {
    if (this.timer !== undefined) clearInterval(this.timer);
    this.timer = undefined;
    try {
      await this.serial;
      if (this.lost === undefined) {
        try {
          await this.checkHeld();
        } catch (error) {
          if (this.lost === undefined) throw error;
        }
        if (this.lost === undefined) await unlink(this.lockPath);
      }
    } finally {
      heldHere.delete(this.owner);
      this.mutex.close();
    }
  }

  private async renew(): Promise<void> {
    if (this.lost !== undefined || this.timer === undefined) return;
    try {
      await this.checkHeld();
      this.record = {
        ...this.record,
        renewedAt: new Date((this.options.now ?? Date.now)()).toISOString(),
      };
      await this.writeRecord();
    } catch {
      // A failed renewal write is retried on the next interval; only a lock
      // file naming another holder loses the lease, and checkHeld reported
      // it.
    }
  }

  private markLost(): void {
    if (this.lost !== undefined) return;
    this.lost = new Error(
      `committee node file store lease was taken over by another process: ${this.lockPath}`,
    );
    if (this.timer !== undefined) clearInterval(this.timer);
    this.timer = undefined;
    this.options.onLost?.(this.lost);
  }

  private async writeRecord(): Promise<void> {
    const temporary = `${this.lockPath}.${this.owner}.metadata.tmp`;
    const handle = await openFile(temporary, "wx", 0o600);
    try {
      try {
        await handle.writeFile(`${JSON.stringify(this.record)}\n`, "utf8");
        await handle.sync();
      } finally {
        await handle.close();
      }
      await rename(temporary, this.lockPath);
      const directory = await openFile(dirname(this.lockPath), "r");
      try {
        await directory.sync();
      } finally {
        await directory.close();
      }
    } finally {
      await unlink(temporary).catch((error: unknown) => {
        if (!(isNodeError(error) && error.code === "ENOENT")) throw error;
      });
    }
  }
}
