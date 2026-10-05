import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  availabilityResponderCollateral,
  type AvailabilityResponderSkippedRecord,
  DACH_SUFFIX_HEX_LENGTH,
} from "./factory.availability-responder-operations.js";
import {
  type AvailabilityResponderAction,
  type AvailabilityResponderChallenge,
} from "./responder.js";

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
    readUtxos?: (
      address: string,
      scope: SDK.DaAvailabilityReadScope,
    ) => Promise<UTxO[]>;
    onSnapshot?: (snapshot: SDK.DaAvailabilitySnapshotUtxos) => void;
  }>,
): Promise<readonly AvailabilityResponderChallenge[]> => {
  const utxosAt = (address: string) =>
    reads
      ? reads.scope.read(() =>
          reads.readUtxos
            ? reads.readUtxos(address, reads.scope)
            : lucid.utxosAt(address),
        )
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
 * A responder transaction is valid from this long before it is built (clock
 * skew) until AVAILABILITY_RESPONDER_VALIDITY_SPAN_MS after that lower bound,
 * so it can land for at most span minus backdate after it is built. A signed
 * transaction that has not landed keeps its exact bytes until canonical
 * reconciliation authorizes release. With every input canonically unspent,
 * that is once its validity has passed, and
 * tests/availability-response-budget.test.ts counts that wait against the
 * response window. Validity expiry alone does not authorize replacement while
 * an input's fate is unresolved.
 */
export const AVAILABILITY_RESPONDER_VALIDITY_BACKDATE_MS = 60_000;
export const AVAILABILITY_RESPONDER_VALIDITY_SPAN_MS = 120_000;

/** Publication has the authenticated response upper bound. Settle and Close
 * are completion work; this response deadline cannot bound their recovery. */
export const availabilityResponderTransactionOperation = (
  lucid: LucidEvolution,
  deployment: SDK.DaAvailabilityDeployment,
  action: AvailabilityResponderAction,
): Parameters<typeof SDK.runDaAvailabilityOperation>[1] => ({
  action: action.kind,
  headerHash: action.challenge.record.datum.commitment.header_hash,
  unsignedDeadlineMs:
    action.kind === "publish"
      ? Number(action.challenge.record.datum.response_deadline)
      : undefined,
  build: async (signal) =>
    (
      await buildAvailabilityResponderTransaction(
        lucid,
        deployment,
        action,
        Date.now(),
        signal,
      )
    ).tx,
});

export const buildAvailabilityResponderTransaction = async (
  lucid: LucidEvolution,
  deployment: SDK.DaAvailabilityDeployment,
  action: AvailabilityResponderAction,
  nowMs = Date.now(),
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
  const validFrom = BigInt(
    Math.max(0, nowMs - AVAILABILITY_RESPONDER_VALIDITY_BACKDATE_MS),
  );
  const unconstrainedUpper =
    validFrom + BigInt(AVAILABILITY_RESPONDER_VALIDITY_SPAN_MS);
  const deadlineUpper = action.challenge.record.datum.response_deadline + 1n;
  const validTo =
    action.kind === "publish" && deadlineUpper < unconstrainedUpper
      ? deadlineUpper
      : unconstrainedUpper;
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
