/**
 * The watcher journals' timer retries, by failure class (owner ruling
 * 2026-10-09: retry only what is transient). The background reopen of an
 * open that could not complete runs only while its cause is transient
 * (SQLite busy or locked); a file SQLite cannot open is opened again only by
 * the next use. The `journal_busy` requeue runs again on its backoff only
 * when it met a busy database again; any other failure keeps the hold,
 * named, with no timer.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { createWatcherJournalBusyHold } from "../../src/fault-proofs/watcher-journal-busy.js";
import {
  isTransientJournalFailure,
  watcherJournalOpener,
} from "../../src/fault-proofs/watcher-journal-database.opener.js";
import { WatcherJournalUnavailableError } from "../../src/fault-proofs/watcher-journal-database.types.js";

const sqlite = (errcode: number, message: string) =>
  Object.assign(new Error(message), { code: "ERR_SQLITE_ERROR", errcode });
const busy = () => sqlite(5, "database is locked");
const cannotOpen = () => sqlite(14, "unable to open database file");

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

describe("isTransientJournalFailure", () => {
  it.each([
    ["busy", busy(), true],
    ["busy, wrapped", new WatcherJournalUnavailableError(busy()), true],
    ["locked", sqlite(6, "database table is locked"), true],
    ["cannot open", new WatcherJournalUnavailableError(cannotOpen()), false],
    ["unclassified", new Error("boom"), false],
  ])("%s", (_name, error, transient) => {
    expect(isTransientJournalFailure(error)).toBe(transient);
  });
});

/** An opener whose first `failures` opens fail with `cause`. */
const opener = (cause: () => Error, failures: number) => {
  let opens = 0;
  const handle = watcherJournalOpener(
    async () => {
      opens += 1;
      if (opens > 50) throw new Error("retried without bound");
      if (opens <= failures) throw new WatcherJournalUnavailableError(cause());
      return "journals";
    },
    { retryInBackground: true },
  );
  return { handle, opens: () => opens };
};

describe("the journals' background reopen, by failure class", () => {
  it("reopens a transient failure on its timer, with no use", async () => {
    const { handle, opens } = opener(busy, 2);
    await expect(handle.open()).rejects.toThrow("unavailable");
    await vi.advanceTimersByTimeAsync(5_000);
    expect(opens()).toBe(3);
    await expect(handle.open()).resolves.toBe("journals");
    expect(opens()).toBe(3);
    handle.close();
  });

  it("leaves a failure no wait fixes to the next use", async () => {
    const { handle, opens } = opener(cannotOpen, 1);
    await expect(handle.open()).rejects.toThrow("unable to open");
    await vi.advanceTimersByTimeAsync(120_000);
    expect(opens()).toBe(1);
    await expect(handle.open()).resolves.toBe("journals");
    expect(opens()).toBe(2);
    handle.close();
  });
});

describe("the journal_busy requeue, by failure class", () => {
  const hold = (fail: () => Error, failures: number) => {
    let requeues = 0;
    let resumed = 0;
    const held = createWatcherJournalBusyHold({
      requeue: () => {
        requeues += 1;
        if (requeues > 50) throw new Error("retried without bound");
        return requeues <= failures
          ? Promise.reject(fail())
          : Promise.resolve();
      },
      resumed: () => {
        resumed += 1;
      },
    });
    return {
      held,
      requeues: () => requeues,
      resumed: () => resumed,
    };
  };

  it("runs the requeue again on its backoff while the database stays busy", async () => {
    const { held, requeues, resumed } = hold(busy, 1);
    held.hold(busy());
    await vi.advanceTimersByTimeAsync(10_000);
    expect(requeues()).toBe(2);
    expect(resumed()).toBe(1);
    expect(held.reason()).toBeNull();
    held.close();
  });

  it("keeps the hold, named, with no timer, on a requeue failure that is not transient", async () => {
    const { held, requeues, resumed } = hold(() => new Error("broken"), 1);
    held.hold(busy());
    await vi.advanceTimersByTimeAsync(120_000);
    expect(requeues()).toBe(1);
    expect(resumed()).toBe(0);
    expect(held.reason()).toBe("broken");
    held.close();
  });
});
