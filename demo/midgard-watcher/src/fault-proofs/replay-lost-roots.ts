import { createHash } from "node:crypto";

import type { WatcherOperationsSink } from "../runtime/operations-observability.js";
import { WATCHER_PACKAGE_NAME } from "../runtime/package-name.js";

/** The error-level log reason of a ledger root the store stopped holding. */
export const WATCHER_REPLAYED_LEDGER_ROOT_LOST = "replayed_ledger_root_lost";

/** The one `replayed_ledger_root_lost` alert subject of a watcher. */
export const WATCHER_REPLAYED_LEDGER_ROOT_LOST_SUBJECT = createHash("sha256")
  .update("midgard-watcher-replayed-ledger-root-lost-alert-v1")
  .digest("hex");

/**
 * The watcher's record of ledgers its store lost: roots the retention window
 * keeps that this process had seen held and that the store no longer holds
 * (`ReplayedLedger.retain`). Each is logged once at error level, and the
 * `replayed_ledger_root_lost` alert stands while one of them is still kept
 * and not held again. The keeper derives a lost ledger again on its next
 * lookup, so the alert tells the operator only; it never holds readiness.
 */
export const createWatcherReplayLostRoots = (
  input: Readonly<{
    sink?: Pick<WatcherOperationsSink, "setAlert">;
    nowMs?: () => number;
    log?: (line: string) => void;
  }> = {},
) => {
  const nowMs = input.nowMs ?? Date.now;
  const log =
    input.log ?? ((line: string) => process.stderr.write(`${line}\n`));
  const lost = new Set<string>();
  let active = false;
  const settle = () => {
    if (active === lost.size > 0) return;
    active = lost.size > 0;
    input.sink?.setAlert({
      code: "replayed_ledger_root_lost",
      subjectDigest: WATCHER_REPLAYED_LEDGER_ROOT_LOST_SUBJECT,
      active,
      observedAtMs: Math.floor(nowMs()).toString(),
    });
  };
  return Object.freeze({
    /** A retention pass kept `kept` and found `notHeld` of it lost. */
    retained: (
      kept: ReadonlySet<string>,
      notHeld: ReadonlySet<string>,
    ): void => {
      for (const root of lost) if (!kept.has(root)) lost.delete(root);
      for (const root of notHeld) {
        if (lost.has(root)) continue;
        lost.add(root);
        log(
          JSON.stringify({
            packageName: WATCHER_PACKAGE_NAME,
            level: "error",
            reason: WATCHER_REPLAYED_LEDGER_ROOT_LOST,
            root,
          }),
        );
      }
      settle();
    },
    /** The store holds `root` again. */
    held: (root: string): void => {
      if (lost.delete(root)) settle();
    },
  });
};

export type WatcherReplayLostRoots = ReturnType<
  typeof createWatcherReplayLostRoots
>;
