import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  alignedUnixTimeStrictlyAfter,
  ledgerTipBackdatedValidFromMs,
} from "../../workers/utils/commit-end-time.js";
import {
  awaitLedgerTipAtLowerBound,
  observedLedgerTip,
} from "../ledger-tip-lower-bound.js";
import { ACTIVATION_VALIDITY_WINDOW_MS } from "../register-active-operator.fetch-hub-oracle-ref-input.js";
import {
  alignUnixTimeMsToSlotBoundary,
  currentTimeMsForLucidOrEmulatorFallback,
} from "./clock.js";

/** How far before the observed tip an immediate activation opens. */
const ACTIVATION_IMMEDIATE_BACKDATE_MS = 60_000;

/**
 * Existing registered-operator liveness exception: only the earliest registration
 * can restore an empty active set. The activation transaction consumes this root,
 * so another activation invalidates the witness rather than weakening the check.
 * The caller authenticates the registered node against the registered policy.
 */
export const canActivateRegisteredOperatorImmediately = (
  activeRoot: Pick<SDK.NodeWithDatum, "utxo" | "datum">,
  registeredNode: SDK.LinkedListNodeView,
  activeOperators: Pick<
    SDK.MidgardValidators["activeOperators"],
    "policyId" | "spendingScriptAddress"
  >,
): boolean =>
  activeRoot.utxo.address === activeOperators.spendingScriptAddress &&
  activeRoot.utxo.assets[
    toUnit(activeOperators.policyId, SDK.ACTIVE_OPERATORS_ROOT_ASSET_NAME)
  ] === 1n &&
  activeRoot.datum.key === "Empty" &&
  activeRoot.datum.next === "Empty" &&
  registeredNode.key !== "Empty" &&
  registeredNode.next === "Empty";

/**
 * The activation's validity window, chosen at build time once the observed
 * L1 ledger tip admits it.
 *
 * An ordinary activation opens at the activation time. It is built only once
 * the observed tip has reached that time by the submit margin: the tip moves
 * block by block, so the wait re-reads it rather than sleeping out a slot
 * difference. The earliest registration may restore an authenticated empty
 * active set at once; its lower bound is backdated from the observed tip so
 * it is already open there. On custom networks (emulator) the window has no
 * upper bound: wall-clock slot estimation can drift from the emulator ledger
 * tip during long candidate retries and cause false "slot range" failures.
 */
export const activationValidityWindowAtTip = (
  lucid: LucidEvolution,
  {
    activationTime,
    immediateActivation,
  }: { readonly activationTime: bigint; readonly immediateActivation: boolean },
): Effect.Effect<
  () => { readonly validFrom: bigint; readonly validTo?: bigint },
  SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const usesWallClockTime = lucid.config().network === "Custom";
    const alignedActivationTime = alignUnixTimeMsToSlotBoundary(
      lucid,
      activationTime,
    );
    const activationLowerBoundMs =
      alignedActivationTime >= activationTime
        ? alignedActivationTime
        : BigInt(alignedUnixTimeStrictlyAfter(lucid, Number(activationTime)));
    const tip = yield* Effect.mapError(
      immediateActivation
        ? observedLedgerTip(lucid)
        : awaitLedgerTipAtLowerBound(
            lucid,
            activationLowerBoundMs,
            "operator activation",
          ),
      (cause) =>
        new SDK.StateQueueError({
          message: "Operator activation needs the observed L1 ledger tip",
          cause,
        }),
    );
    return () => {
      const upperBoundBase =
        currentTimeMsForLucidOrEmulatorFallback(lucid) +
        ACTIVATION_VALIDITY_WINDOW_MS;
      const validFrom = immediateActivation
        ? BigInt(
            ledgerTipBackdatedValidFromMs({
              lucid,
              submitSlotSnapshot: tip,
              backdateMs: ACTIVATION_IMMEDIATE_BACKDATE_MS,
              ...(usesWallClockTime
                ? {}
                : {
                    rangeFloorMs: Number(
                      alignUnixTimeMsToSlotBoundary(lucid, upperBoundBase) -
                        SDK.MAX_VALIDITY_RANGE_LENGTH_MS,
                    ),
                  }),
            }),
          )
        : activationLowerBoundMs;
      if (usesWallClockTime) return { validFrom };
      const validTo = alignUnixTimeMsToSlotBoundary(
        lucid,
        upperBoundBase > validFrom
          ? upperBoundBase
          : validFrom + ACTIVATION_VALIDITY_WINDOW_MS,
      );
      return { validFrom, validTo };
    };
  });
