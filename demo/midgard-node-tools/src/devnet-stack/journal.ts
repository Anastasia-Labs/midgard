import {
  linkSync,
  mkdirSync,
  readFileSync,
  unlinkSync,
  writeFileSync,
} from "node:fs";
import { dirname } from "node:path";

import { readJsonIfPresent, writeDurableJson } from "./durable.js";
import { lockOwner, processStartTime } from "./lock.js";

type JournalFile = {
  readonly schemaVersion: "midgard-devnet-journal-v1";
  readonly entries: Record<string, unknown>;
};

/** How long a write waits for another process's write to the same journal.
 * A write holds the lock for one read and one durable replace, so a holder
 * this slow is stopped, and the refusal is left to the caller's retry. */
export const JOURNAL_LOCK_WAIT_MS = 10_000;
const LOCK_POLL_MS = 5;

const pause = (ms: number) =>
  Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, ms);

const missing = (error: unknown) =>
  (error as NodeJS.ErrnoException).code === "ENOENT";

const readIfPresent = (path: string) => {
  try {
    return readFileSync(path, "utf8");
  } catch (error) {
    if (missing(error)) return undefined;
    throw error;
  }
};

const unlinkIfPresent = (path: string) => {
  try {
    unlinkSync(path);
  } catch (error) {
    if (!missing(error)) throw error;
  }
};

/** The lock's live holder; null once the lock is gone. */
const liveHolder = (lock: string) => {
  try {
    return lockOwner(lock) ?? undefined;
  } catch (error) {
    if (missing(error)) return null;
    throw error;
  }
};

/**
 * Runs `body` holding `<path>.lock`, which names this process by PID and
 * start time. The lock is linked into place from a file already holding
 * that record, so another process never reads a half-written lock as a
 * dead holder's. A lock whose holder has exited is taken over; a live
 * holder is waited for, up to `waitMs`.
 */
const withJournalLock = <T>(path: string, waitMs: number, body: () => T): T => {
  const lock = `${path}.lock`;
  const record =
    `${process.pid.toString()} ${processStartTime(process.pid) ?? ""}`.trim();
  const staging = `${lock}.${process.pid.toString()}`;
  mkdirSync(dirname(path), { recursive: true, mode: 0o700 });
  writeFileSync(staging, record, { mode: 0o600 });
  const deadline = Date.now() + waitMs;
  try {
    for (;;) {
      try {
        linkSync(staging, lock);
        break;
      } catch (error) {
        if ((error as NodeJS.ErrnoException).code !== "EEXIST") throw error;
      }
      const held = readIfPresent(lock);
      const holder = held === undefined ? null : liveHolder(lock);
      if (holder === null) continue;
      if (holder === undefined) {
        if (readIfPresent(lock) === held) unlinkIfPresent(lock);
        continue;
      }
      if (Date.now() > deadline)
        throw new Error(
          `${lock} is held by live pid ${holder.toString()} for over ${waitMs.toString()} ms; the journal is unchanged`,
        );
      pause(LOCK_POLL_MS);
    }
  } finally {
    unlinkIfPresent(staging);
  }
  try {
    return body();
  } finally {
    if (readIfPresent(lock) === record) unlinkIfPresent(lock);
  }
};

/**
 * Durable step records. A step writes its intent (for example a signed
 * transaction id) before acting on it, so a rerun reconciles that exact
 * intent against the chain instead of building a second one.
 */
export class Journal {
  private readonly file: JournalFile;
  private readonly lockWaitMs: number;

  constructor(
    private readonly path: string,
    options: { readonly lockWaitMs?: number } = {},
  ) {
    this.lockWaitMs = options.lockWaitMs ?? JOURNAL_LOCK_WAIT_MS;
    this.file = readJsonIfPresent<JournalFile>(path) ?? {
      schemaVersion: "midgard-devnet-journal-v1",
      entries: {},
    };
    if (this.file.schemaVersion !== "midgard-devnet-journal-v1")
      throw new Error(`${path} has an unknown schema; preserve it`);
  }

  get<T>(key: string): T | undefined {
    return this.file.entries[key] as T | undefined;
  }

  /** Every value whose key starts with `prefix`, in insertion order. */
  withPrefix<T>(prefix: string): T[] {
    return Object.entries(this.file.entries)
      .filter(([key]) => key.startsWith(prefix))
      .map(([, value]) => value as T);
  }

  /** Writes over the file as it is now, under the journal's lock, so no
   * entry another instance or process recorded since is ever dropped: the
   * up or journey controller and the endurance maintainer share it. */
  set(key: string, value: unknown): void {
    withJournalLock(this.path, this.lockWaitMs, () => {
      const current = readJsonIfPresent<JournalFile>(this.path);
      Object.assign(this.file.entries, current?.entries, { [key]: value });
      writeDurableJson(this.path, this.file);
    });
  }
}
