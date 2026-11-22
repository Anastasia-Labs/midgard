import type { ReplayedLedgerUnverified } from "@al-ft/midgard-fault-proofs/replayed-ledger";

import { WATCHER_PACKAGE_NAME } from "../runtime/package-name.js";

/** The warn-level log reason of a confirmed block passed without a replay. */
export const WATCHER_REPLAYED_LEDGER_CONFIRMED_UNVERIFIED =
  "replayed_ledger_confirmed_unverified";

/**
 * Records a confirmed block this watcher's verified head moved past without
 * replaying it (`ReplayedLedger.ensure`): one warn-level line per block,
 * with why it could not be replayed. The block is merged and nothing about
 * it can be challenged any more, so the line tells the operator only; it
 * never holds readiness.
 */
export const logWatcherReplayUnverified =
  (log: (line: string) => void = (line) => process.stderr.write(`${line}\n`)) =>
  ({ headerHash, cause }: ReplayedLedgerUnverified): void =>
    log(
      JSON.stringify({
        packageName: WATCHER_PACKAGE_NAME,
        level: "warn",
        reason: WATCHER_REPLAYED_LEDGER_CONFIRMED_UNVERIFIED,
        headerHash,
        cause,
      }),
    );
