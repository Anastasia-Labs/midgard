import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type CommitteeL1Clock,
  committeeL1Time,
} from "../l1/follower/l1-clock.js";
import {
  availabilityResponderCollateral,
  type AvailabilityResponderSkippedRecord,
  DACH_SUFFIX_HEX_LENGTH,
} from "./factory.availability-responder-operations.js";
import {
  type AvailabilityResponderAction,
  type AvailabilityResponderChallenge,
} from "./responder.js";
import { AvailabilityResponderValidityDeferredError } from "./validity-deferred-error.js";

/**
 * Every live availability challenge, read from its challenge record
 * (`ChallengeRecordV1`). A record is the UTxO at the availability script that
 * holds a 32-byte DACH token under the availability policy; its datum is parsed
 * canonically against the deployment's parameters, and the SDK snapshot of its
 * header then authenticates the record against the `Challenged` state-queue
 * node, the terminal accumulator and every unsettled tranche. Ordered by
 * response deadline, then header hash.
 *
 * A record is judged on its own: one that cannot be answered is reported to
 * `onSkipped` and left out, and never stops discovery of the others. A record
 * whose state-queue node is gone, or is not `Challenged` by it, is stranded
 * (a timeout or fraud removal pruned its node while it was challenged; nothing
 * can spend it again), so there is nothing to answer.
 */
export const discoverAvailabilityResponderChallenges = async (
  lucid: LucidEvolution,
  deployment: SDK.DaAvailabilityDeployment,
  onSkipped: (skipped: AvailabilityResponderSkippedRecord) => void = () => {},
  reads?: Readonly<{
    scope: SDK.DaAvailabilityReadScope;
    onSnapshot?: (snapshot: SDK.DaAvailabilitySnapshotUtxos) => void;
  }>,
): Promise<readonly AvailabilityResponderChallenge[]> => {
  const utxosAt = (address: string) =>
    reads
      ? reads.scope.read(() => lucid.utxosAt(address))
      : lucid.utxosAt(address);
  const [availabilityUtxos, stateQueueUtxos, correctionLockUtxos] =
    await Promise.all([
      utxosAt(deployment.contracts.availabilityChallenge.spendingScriptAddress),
      utxosAt(deployment.contracts.stateQueue.spendingScriptAddress),
      utxosAt(deployment.contracts.correctionLock.spendingScriptAddress),
    ]);
  reads?.scope.assertCurrent();
  reads?.onSnapshot?.({
    availabilityUtxos,
    stateQueueUtxos,
    correctionLockUtxos,
  });
  const recordUnitPrefix =
    deployment.contracts.availabilityChallenge.policyId +
    SDK.DA_AVAILABILITY_CHALLENGE_ASSET_NAME_PREFIX;
  const result: AvailabilityResponderChallenge[] = [];
  for (const utxo of availabilityUtxos) {
    reads?.scope.assertCurrent();
    const recordUnit = Object.keys(utxo.assets).find(
      (unit) =>
        unit.length === recordUnitPrefix.length + DACH_SUFFIX_HEX_LENGTH &&
        unit.startsWith(recordUnitPrefix),
    );
    if (recordUnit === undefined) continue;
    const skip = (stranded: boolean, reason: string) =>
      onSkipped({
        outRef: `${utxo.txHash}#${utxo.outputIndex}`,
        stranded,
        reason,
      });
    try {
      if (typeof utxo.datum !== "string")
        throw new Error(
          "Authenticated availability challenge record has no inline datum",
        );
      const record = SDK.parseDaAvailabilityChallengeRecordCbor(
        utxo.datum,
        deployment.parameters,
      );
      const challengeAssetName = recordUnit.slice(
        deployment.contracts.availabilityChallenge.policyId.length,
      );
      if (record.challenge_asset_name !== challengeAssetName)
        throw new Error(
          "Availability challenge record datum names a different challenge than its token",
        );
      const snapshot = await SDK.daAvailabilityChallengeSnapshotFromUtxos(
        deployment,
        record.commitment.header_hash,
        {
          availabilityUtxos,
          stateQueueUtxos,
          correctionLockUtxos,
        },
      );
      if (
        !snapshot.queue ||
        !snapshot.record ||
        !snapshot.recordDatum ||
        snapshot.record.txHash !== utxo.txHash ||
        snapshot.record.outputIndex !== utxo.outputIndex ||
        snapshot.recordDatum.challenge_asset_name !== challengeAssetName
      ) {
        skip(
          true,
          "Availability challenge record is not the one its state-queue node is challenged by",
        );
        continue;
      }
      if (!snapshot.terminal || !snapshot.terminalDatum) {
        throw new Error(
          "Authenticated availability challenge has incomplete live state",
        );
      }
      result.push({
        record: { utxo: snapshot.record, datum: snapshot.recordDatum },
        terminal: { utxo: snapshot.terminal, datum: snapshot.terminalDatum },
        queue: snapshot.queue.utxo,
        tranches: snapshot.tranches,
      });
    } catch (error) {
      skip(false, error instanceof Error ? error.message : String(error));
    }
  }
  reads?.scope.assertCurrent();
  return result.sort((a, b) => {
    const left = a.record.datum,
      right = b.record.datum;
    return left.response_deadline === right.response_deadline
      ? left.commitment.header_hash.localeCompare(right.commitment.header_hash)
      : left.response_deadline < right.response_deadline
        ? -1
        : 1;
  });
};

/**
 * The limits the action's validator puts on its validity interval:
 *
 * - Publish: the inclusive upper bound is at or before the response deadline
 *   (`validate_advance_tranche`), so `validTo <= response_deadline + 1`.
 * - Settle of an Active tranche (an expired one): the inclusive lower bound
 *   is at or after the response deadline (`validate_settlement_status`).
 * - Settle of a published tranche (a Receipt), and Close: none. They are
 *   completion work, which the response deadline cannot bound.
 */
export const availabilityResponderValidityLimits = (
  action: AvailabilityResponderAction,
): SDK.DaAvailabilityValidityLimits | undefined => {
  const deadline = action.challenge.record.datum.response_deadline;
  if (action.kind === "publish") return { upperLimitMs: deadline + 1n };
  if (action.kind === "settle" && "Active" in action.tranche.datum)
    return { lowerLimitMs: deadline };
  return undefined;
};

/**
 * The times one responder build is taken at, kept apart so that only the
 * validity upper bound may come from anything but the committee's L1 clock
 * (plan §3.6):
 *
 * - `tipSlot`: the observed follower tip; the validity lower bound is
 *   anchored to it.
 * - `validToNowMs`: the "now" the validity upper bound is picked from (the
 *   SDK window's `nowMs`). The plain responder uses the L1 clock; promise
 *   mode uses its adopted, drift-bounded promise network clock, whose upper
 *   time its TTL rule is measured against.
 * - `deadlineNowMs`: the committee's L1 clock. Whether the response deadline
 *   has passed is decided on it alone, in both modes.
 * - `minimumRoomSlots`: how many slots past `validToNowMs` an interval
 *   anchored on a lagging tip must still run (the SDK window's
 *   `minimumRoomSlots`). Promise mode sets it from its TTL rule; the plain
 *   responder keeps the window's default.
 */
export type AvailabilityResponderBuildTime = Readonly<{
  tipSlot: number;
  validToNowMs: number;
  deadlineNowMs: number;
  minimumRoomSlots?: number;
}>;

/** The plain responder's build time: every "now" is the L1 clock's. */
export const availabilityResponderL1BuildTime = (
  l1Clock: CommitteeL1Clock,
): AvailabilityResponderBuildTime => {
  const { tipSlot, nowMs } = committeeL1Time(l1Clock);
  return { tipSlot, validToNowMs: nowMs, deadlineNowMs: nowMs };
};

const publicationDeadlinePassed = (
  action: AvailabilityResponderAction,
  time: AvailabilityResponderBuildTime,
): boolean =>
  action.kind === "publish" &&
  BigInt(time.deadlineNowMs) >= action.challenge.record.datum.response_deadline;

/**
 * The action's validity interval (`SDK.daAvailabilityValidityWindow`): the
 * lower bound is measured from the observed follower tip and the upper bound
 * from `validToNowMs`, never from the committee's wall clock. A window that is
 * not admissible defers the action before anything is built, signed or
 * journaled (`AvailabilityResponderValidityDeferredError`); a closed
 * publication window is a missed deadline only once the L1 clock says so.
 *
 * A signed transaction that has not landed keeps its exact bytes until
 * canonical reconciliation authorizes release. With every input canonically
 * unspent, that is once its validity has passed, and
 * tests/availability-response-budget.test.ts counts that wait against the
 * response window. Validity expiry alone does not authorize replacement while
 * an input's fate is unresolved.
 */
const availabilityResponderValidity = (
  lucid: LucidEvolution,
  action: AvailabilityResponderAction,
  time: AvailabilityResponderBuildTime,
): Readonly<{ validFrom: bigint; validTo: bigint }> => {
  const limits = availabilityResponderValidityLimits(action);
  const window = SDK.daAvailabilityValidityWindow({
    lucid,
    tip: { currentSlot: time.tipSlot, ledgerTipSlot: time.tipSlot },
    nowMs: time.validToNowMs,
    ...(limits === undefined ? {} : { limits }),
    ...(time.minimumRoomSlots === undefined
      ? {}
      : { minimumRoomSlots: time.minimumRoomSlots }),
  });
  switch (window.status) {
    case "ready":
      return { validFrom: window.validFrom, validTo: window.validTo };
    case "not_due":
      throw new AvailabilityResponderValidityDeferredError(
        "ledger_tip_behind",
        `the ledger tip (slot ${window.tipSlot.toString()}) has not reached the interval's lower bound (due at slot ${window.dueSlot.toString()})`,
      );
    case "refused":
      throw new AvailabilityResponderValidityDeferredError(
        window.reason === "window_closed" &&
        publicationDeadlinePassed(action, time)
          ? "deadline_passed"
          : "no_validity_interval",
        window.detail,
      );
  }
};

/** Publication has the authenticated response upper bound. Settle and Close
 * are completion work; this response deadline cannot bound their recovery. */
export const availabilityResponderPublicationDeadlineMs = (
  action: AvailabilityResponderAction,
): number | undefined =>
  action.kind === "publish"
    ? Number(action.challenge.record.datum.response_deadline)
    : undefined;

/**
 * The operation the SDK runner signs. It carries no `unsignedDeadlineMs`:
 * the runner measures that on its context's wall clock, which would refuse a
 * publication the L1 clock still admits whenever the wall runs ahead. The
 * deadline is the committee's, on its L1 clock: the responder's next-action
 * choice, the validity window's classification, and `buildTime` read again
 * just before the build returns, so a publication whose deadline passed on
 * the L1 clock meanwhile is never handed to the runner to sign. The caller's
 * read scope ends build reads at the deadline on the same clock. On chain,
 * the window's upper limit (`response_deadline + 1`) bounds the rest.
 */
export const availabilityResponderTransactionOperation = (
  lucid: LucidEvolution,
  deployment: SDK.DaAvailabilityDeployment,
  action: AvailabilityResponderAction,
  buildTime: () => AvailabilityResponderBuildTime,
): Parameters<typeof SDK.runDaAvailabilityOperation>[1] => ({
  action: action.kind,
  headerHash: action.challenge.record.datum.commitment.header_hash,
  build: async (signal) => {
    signal.throwIfAborted();
    const { tx } = await buildAvailabilityResponderTransaction(
      lucid,
      deployment,
      action,
      buildTime(),
      signal,
    );
    if (publicationDeadlinePassed(action, buildTime()))
      throw new AvailabilityResponderValidityDeferredError(
        "deadline_passed",
        "the response deadline passed on the committee's L1 clock while the publication was built",
      );
    return tx;
  },
});

export const buildAvailabilityResponderTransaction = async (
  lucid: LucidEvolution,
  deployment: SDK.DaAvailabilityDeployment,
  action: AvailabilityResponderAction,
  time: AvailabilityResponderBuildTime,
  signal?: AbortSignal,
): Promise<SDK.BuiltDaAvailabilityTransaction> => {
  signal?.throwIfAborted();
  const p = deployment.parameters;
  const feeLovelace =
    action.kind === "publish"
      ? p.max_publication_fee_lovelace
      : action.kind === "settle"
        ? p.max_settlement_fee_lovelace
        : p.max_close_fee_lovelace;
  const { validFrom, validTo } = availabilityResponderValidity(
    lucid,
    action,
    time,
  );
  const resources = {
    feeLovelace,
    validFrom,
    validTo,
    collateralInputs: await availabilityResponderCollateral(lucid, feeLovelace),
  };
  signal?.throwIfAborted();
  switch (action.kind) {
    case "publish":
      return Effect.runPromise(
        SDK.buildPublishDaAvailabilityChunkTxProgram(lucid, deployment, {
          ...resources,
          thread: action.tranche.utxo,
          previousCarrier: action.tranche.carrier,
          publication: action.publication,
        }),
        { signal },
      );
    case "settle":
      return Effect.runPromise(
        SDK.buildSettleDaAvailabilityTrancheTxProgram(lucid, deployment, {
          ...resources,
          record: action.challenge.record.utxo,
          terminal: action.challenge.terminal.utxo,
          thread: action.tranche.utxo,
          carrier: action.tranche.carrier,
        }),
        { signal },
      );
    case "close":
      return Effect.runPromise(
        SDK.buildCloseDaAvailabilityChallengeTxProgram(lucid, deployment, {
          ...resources,
          record: action.challenge.record.utxo,
          terminal: action.challenge.terminal.utxo,
          queue: action.challenge.queue,
        }),
        { signal },
      );
  }
};
