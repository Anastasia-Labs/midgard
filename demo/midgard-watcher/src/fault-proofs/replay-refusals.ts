import { createHash } from "node:crypto";

import type { ReplayedLedgerRefusal } from "@al-ft/midgard-fault-proofs";

import type { WatcherOperationsSink } from "../runtime/operations-observability.js";
import { WATCHER_PACKAGE_NAME } from "../runtime/scaffold.js";

/** What a replay refusal says went wrong (`ReplayedLedgerRefusal.kind`). */
export const WATCHER_REPLAY_REFUSAL_KINDS = Object.freeze([
  "replay-refused",
  "root-mismatch",
  "parent-mismatch",
] as const satisfies readonly ReplayedLedgerRefusal["kind"][]);

/**
 * Where the refusal was met: moving the replayed ledger's checkpoint to the
 * L1 ConfirmedState head (`confirmed`), or replaying a header for its
 * classification (`classification`). A classified header whose replay does
 * not open its root is what the fault-proof path exists for; the confirmed
 * head not opening its root means the watcher's replay and the confirmed
 * state disagree.
 */
export const WATCHER_REPLAY_REFUSAL_PHASES = Object.freeze([
  "confirmed",
  "classification",
] as const);

export type WatcherReplayRefusalKind =
  (typeof WATCHER_REPLAY_REFUSAL_KINDS)[number];
export type WatcherReplayRefusalPhase =
  (typeof WATCHER_REPLAY_REFUSAL_PHASES)[number];

/** The error-level log reason of a confirmed head that does not open its root. */
export const WATCHER_REPLAYED_LEDGER_ROOT_MISMATCH =
  "replayed_ledger_root_mismatch";
/** The warn-level log reason of every other newly observed refusal. */
export const WATCHER_REPLAYED_LEDGER_REFUSED = "replayed_ledger_refused";

/** How many distinct refusals `recent` keeps. */
export const WATCHER_REPLAY_REFUSALS_REPORTED = 16;

const DETAIL_BYTES = 512;

export type WatcherReplayRefusalObservation = Readonly<{
  phase: WatcherReplayRefusalPhase;
  kind: WatcherReplayRefusalKind;
  headerHash: string;
  detail: string;
  observedAtMs: string;
}>;

export type WatcherReplayRefusalsStatus = Readonly<{
  /**
   * The confirmed head whose replayed ledger does not open its root (or
   * does not start from its parent's), while that lasts: the `root_mismatch`
   * alert is active for it, and clears once the ledger advances.
   */
  rootMismatch: WatcherReplayRefusalObservation | null;
  /** Newly observed refusals since the process started, per phase and kind. */
  totals: Readonly<
    Record<
      WatcherReplayRefusalPhase,
      Readonly<Record<WatcherReplayRefusalKind, string>>
    >
  >;
  /**
   * The latest distinct refusals observed, oldest first: recent
   * observations, not an active set. One leaving the list is not resolved.
   */
  recent: readonly WatcherReplayRefusalObservation[];
}>;

const zeroTotals = () =>
  Object.fromEntries(
    WATCHER_REPLAY_REFUSAL_PHASES.map((phase) => [
      phase,
      Object.fromEntries(
        WATCHER_REPLAY_REFUSAL_KINDS.map((kind) => [kind, 0n]),
      ),
    ]),
  ) as Record<
    WatcherReplayRefusalPhase,
    Record<WatcherReplayRefusalKind, bigint>
  >;

/** One `root_mismatch` alert subject per confirmed head. */
export const watcherRootMismatchAlertSubject = (headerHash: string): string =>
  createHash("sha256")
    .update("midgard-watcher-replayed-ledger-root-mismatch-alert-v1\u0000")
    .update(headerHash.toLowerCase())
    .digest("hex");

const isRootMismatch = (
  phase: WatcherReplayRefusalPhase,
  kind: WatcherReplayRefusalKind,
) => phase === "confirmed" && kind !== "replay-refused";

/**
 * The watcher's record of its replayed ledger's refusals
 * (`createReplayedLedger`'s `onRefusal`). Each newly observed refusal is
 * counted, kept in `recent` and logged once: at error level under
 * `replayed_ledger_root_mismatch` when the confirmed head does not open its
 * root, else at warn level under `replayed_ledger_refused`. The confirmed
 * head's mismatch also holds the `root_mismatch` alert on the attached
 * operations sink until `progressed` reports the ledger advanced.
 */
export const createWatcherReplayRefusals = (
  input: Readonly<{
    nowMs?: () => number;
    log?: (line: string) => void;
  }> = {},
) => {
  const nowMs = input.nowMs ?? Date.now;
  const log =
    input.log ?? ((line: string) => process.stderr.write(`${line}\n`));
  const totals = zeroTotals();
  const recent: WatcherReplayRefusalObservation[] = [];
  let rootMismatch: WatcherReplayRefusalObservation | null = null;
  let sink: Pick<WatcherOperationsSink, "setAlert"> | undefined;

  const alert = (headerHash: string, active: boolean) =>
    sink?.setAlert({
      code: "root_mismatch",
      subjectDigest: watcherRootMismatchAlertSubject(headerHash),
      active,
      observedAtMs: Math.floor(nowMs()).toString(),
    });

  const record = (
    refusal: ReplayedLedgerRefusal,
    phase: WatcherReplayRefusalPhase,
  ): void => {
    const known = recent.some(
      (entry) =>
        entry.phase === phase &&
        entry.kind === refusal.kind &&
        entry.headerHash === refusal.headerHash,
    );
    const mismatch = isRootMismatch(phase, refusal.kind);
    if (known && (!mismatch || rootMismatch?.headerHash === refusal.headerHash))
      return;
    const observation: WatcherReplayRefusalObservation = Object.freeze({
      phase,
      kind: refusal.kind,
      headerHash: refusal.headerHash,
      detail: refusal.detail.slice(0, DETAIL_BYTES),
      observedAtMs: Math.floor(nowMs()).toString(),
    });
    if (!known) {
      totals[phase][refusal.kind] += 1n;
      recent.push(observation);
      if (recent.length > WATCHER_REPLAY_REFUSALS_REPORTED) recent.shift();
    }
    if (mismatch) {
      if (rootMismatch !== null) alert(rootMismatch.headerHash, false);
      rootMismatch = observation;
      alert(observation.headerHash, true);
    }
    log(
      JSON.stringify({
        packageName: WATCHER_PACKAGE_NAME,
        level: mismatch ? "error" : "warn",
        reason: mismatch
          ? WATCHER_REPLAYED_LEDGER_ROOT_MISMATCH
          : WATCHER_REPLAYED_LEDGER_REFUSED,
        phase,
        kind: observation.kind,
        headerHash: observation.headerHash,
        detail: observation.detail,
      }),
    );
  };

  return Object.freeze({
    record,
    /** The replayed ledger reached the confirmed head: its mismatch, if any, is over. */
    progressed: (): void => {
      if (rootMismatch === null) return;
      const { headerHash } = rootMismatch;
      rootMismatch = null;
      alert(headerHash, false);
    },
    /** Raises and clears `root_mismatch` on `target` from now on. */
    attach: (target: Pick<WatcherOperationsSink, "setAlert">): void => {
      sink = target;
      if (rootMismatch !== null) alert(rootMismatch.headerHash, true);
    },
    status: (): WatcherReplayRefusalsStatus =>
      Object.freeze({
        rootMismatch,
        totals: Object.freeze(
          Object.fromEntries(
            WATCHER_REPLAY_REFUSAL_PHASES.map((phase) => [
              phase,
              Object.freeze(
                Object.fromEntries(
                  WATCHER_REPLAY_REFUSAL_KINDS.map((kind) => [
                    kind,
                    totals[phase][kind].toString(),
                  ]),
                ),
              ),
            ]),
          ),
        ) as WatcherReplayRefusalsStatus["totals"],
        recent: Object.freeze([...recent]),
      }),
  });
};

export type WatcherReplayRefusals = ReturnType<
  typeof createWatcherReplayRefusals
>;

/** The status of a watcher that has observed no refusal. */
export const NO_WATCHER_REPLAY_REFUSALS: WatcherReplayRefusalsStatus =
  createWatcherReplayRefusals({ log: () => undefined }).status();
