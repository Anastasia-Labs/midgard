import * as SDK from "@al-ft/midgard-sdk";
import {
  Data as LucidData,
  type LucidEvolution,
  paymentCredentialOf,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { compareOutRefs, outRefLabel } from "../../tx-context.js";
import { type CurrentOperatorSchedulerWindow } from "./commit-block-planner.js";
import {
  activeSchedulerState,
  latestSchedulerShiftHeaderEndTime,
  SCHEDULER_REFRESH_MAX_POLLS,
  SCHEDULER_REFRESH_POLL_INTERVAL,
  SCHEDULER_SHIFT_DURATION_MS,
} from "./scheduler-refresh.scheduler-refresh-due-work-from-no-inline-submit-defer.js";

export const getOperatorKeyHash = (
  lucid: LucidEvolution,
): Effect.Effect<string, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const operatorAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to resolve operator wallet address",
          cause,
        }),
    });
    const paymentCredential = paymentCredentialOf(operatorAddress);
    if (paymentCredential?.type !== "Key") {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Operator wallet does not have a key payment credential",
          cause: operatorAddress,
        }),
      );
    }
    return paymentCredential.hash;
  });

const selectActiveOperatorInput = (
  activeOperatorUtxos: readonly UTxO[],
  operatorKeyHash: string,
): Effect.Effect<UTxO, SDK.StateQueueError> =>
  Effect.gen(function* () {
    for (const utxo of activeOperatorUtxos) {
      const nodeDatumEither = yield* Effect.either(
        SDK.getLinkedListNodeViewFromUTxO(utxo),
      );
      if (nodeDatumEither._tag === "Left") {
        continue;
      }
      if (
        nodeDatumEither.right.key !== "Empty" &&
        nodeDatumEither.right.key.Key.key === operatorKeyHash
      ) {
        return utxo;
      }
    }
    return yield* Effect.fail(
      new SDK.StateQueueError({
        message:
          "No active-operators node for current operator key hash; cannot build real state_queue commit witness",
        cause: operatorKeyHash,
      }),
    );
  });

export const filterLocallyConsumedUtxos = (
  utxos: readonly UTxO[],
  consumedOutRefs: ReadonlySet<string>,
): readonly UTxO[] =>
  utxos.filter((utxo) => !consumedOutRefs.has(outRefLabel(utxo)));

export const fetchActiveOperatorUtxos = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  message: string,
): Effect.Effect<readonly UTxO[], SDK.StateQueueError> =>
  SDK.utxosAtByNFTPolicyId(
    lucid,
    contracts.activeOperators.spendingScriptAddress,
    contracts.activeOperators.policyId,
  ).pipe(
    Effect.map((beacons) => beacons.map((beacon) => beacon.utxo)),
    Effect.mapError(
      (cause) =>
        new SDK.StateQueueError({
          message,
          cause,
        }),
    ),
  );

const requireInlineActiveOperatorDatum = (
  activeOperatorInput: UTxO,
): Effect.Effect<UTxO & { datum: string }, SDK.StateQueueError> =>
  Effect.gen(function* () {
    if (activeOperatorInput.datum == null) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Active-operators UTxO must include inline datum for real state_queue commit",
          cause: `${activeOperatorInput.txHash}#${activeOperatorInput.outputIndex}`,
        }),
      );
    }
    return activeOperatorInput as UTxO & { datum: string };
  });

export const fetchFreshActiveOperatorInputForCommit = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  operatorKeyHash: string,
  /** Out-refs live own intents spend (`NodeWalletView.held`). */
  heldOutRefs: ReadonlySet<string>,
): Effect.Effect<UTxO & { datum: string }, SDK.StateQueueError> =>
  Effect.gen(function* () {
    let lastCandidateLabels: readonly string[] = [];

    for (
      let pollCount = 0;
      pollCount < SCHEDULER_REFRESH_MAX_POLLS;
      pollCount += 1
    ) {
      const activeOperatorUtxos = yield* fetchActiveOperatorUtxos(
        lucid,
        contracts,
        "Failed to refresh active-operators UTxOs for state_queue commit",
      );
      lastCandidateLabels = activeOperatorUtxos.map(outRefLabel);
      const freshActiveOperatorUtxos = filterLocallyConsumedUtxos(
        activeOperatorUtxos,
        heldOutRefs,
      );
      const activeOperatorInput = yield* Effect.either(
        selectActiveOperatorInput(freshActiveOperatorUtxos, operatorKeyHash),
      );
      if (activeOperatorInput._tag === "Right") {
        return yield* requireInlineActiveOperatorDatum(
          activeOperatorInput.right,
        );
      }

      const staleCandidateLabels = activeOperatorUtxos
        .filter((utxo) => heldOutRefs.has(outRefLabel(utxo)))
        .map(outRefLabel);
      if (staleCandidateLabels.length === 0) {
        return yield* Effect.fail(activeOperatorInput.left);
      }
      if (pollCount === 0) {
        yield* Effect.logWarning(
          `Active-operators provider view still includes locally consumed outref(s) ${staleCandidateLabels.join(
            ",",
          )}; waiting for refreshed commit witness input.`,
        );
      }
      yield* Effect.sleep(SCHEDULER_REFRESH_POLL_INTERVAL);
    }

    return yield* Effect.fail(
      new SDK.StateQueueError({
        message:
          "Timed out waiting for refreshed active-operators UTxO after scheduler refresh",
        cause: `operator=${operatorKeyHash},held_outrefs=${[
          ...heldOutRefs,
        ].join(",")},last_candidates=${lastCandidateLabels.join(",")}`,
      }),
    );
  });

export const getSchedulerDatumFromUTxO = (
  schedulerUtxo: UTxO,
): Effect.Effect<SDK.SchedulerDatum, SDK.StateQueueError> =>
  Effect.gen(function* () {
    if (schedulerUtxo.datum == null) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Scheduler UTxO must include inline datum",
          cause: `${schedulerUtxo.txHash}#${schedulerUtxo.outputIndex}`,
        }),
      );
    }
    const schedulerDatum = schedulerUtxo.datum;
    return yield* Effect.try({
      try: () =>
        LucidData.from(
          schedulerDatum,
          SDK.SchedulerDatum as never,
        ) as SDK.SchedulerDatum,
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to decode scheduler datum",
          cause,
        }),
    });
  });

export const requireExistingSchedulerWitnessUtxo = (
  schedulerUtxos: readonly UTxO[],
  schedulerWitnessUnit: string,
): Effect.Effect<UTxO, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const existingWitness = [...schedulerUtxos]
      .filter((utxo) => (utxo.assets[schedulerWitnessUnit] ?? 0n) > 0n)
      .sort(compareOutRefs)[0];
    if (existingWitness !== undefined) {
      return existingWitness;
    }

    return yield* Effect.fail(
      new SDK.StateQueueError({
        message:
          "Incomplete protocol deployment: scheduler root UTxO is missing; refusing commit-time scheduler minting",
        cause: `unit=${schedulerWitnessUnit}`,
      }),
    );
  });

export const resolveCurrentOperatorSchedulerWindow = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Effect.Effect<
  CurrentOperatorSchedulerWindow | undefined,
  SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const operatorKeyHash = yield* getOperatorKeyHash(lucid);
    const schedulerWitnessUnit = toUnit(
      contracts.scheduler.policyId,
      SDK.SCHEDULER_ASSET_NAME,
    );
    const schedulerUtxos = (yield* SDK.utxosAtByNFTPolicyId(
      lucid,
      contracts.scheduler.spendingScriptAddress,
      contracts.scheduler.policyId,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message:
              "Failed to fetch scheduler UTxOs for scheduler-aware commit planning",
            cause,
          }),
      ),
    )).map((beacon) => beacon.utxo);
    const schedulerRefInput = yield* requireExistingSchedulerWitnessUtxo(
      schedulerUtxos,
      schedulerWitnessUnit,
    );
    const schedulerDatum = yield* getSchedulerDatumFromUTxO(schedulerRefInput);
    const active = activeSchedulerState(schedulerDatum);
    if (active?.operator !== operatorKeyHash) {
      return undefined;
    }
    const startTimeMs = Number(active.startTime);
    const endTimeMs = Number(latestSchedulerShiftHeaderEndTime(active));
    if (
      !Number.isSafeInteger(startTimeMs) ||
      !Number.isSafeInteger(endTimeMs)
    ) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Current scheduler window is outside the safe JavaScript time range",
          cause: `start=${active.startTime.toString()},duration=${SCHEDULER_SHIFT_DURATION_MS.toString()}`,
        }),
      );
    }
    return {
      schedulerOutRef: outRefLabel(schedulerRefInput),
      operatorKeyHash,
      startTimeMs,
      endTimeMs,
    } satisfies CurrentOperatorSchedulerWindow;
  });
