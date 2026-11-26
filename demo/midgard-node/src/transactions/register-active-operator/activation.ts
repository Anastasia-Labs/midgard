import {
  alignedUnixTimeStrictlyAfter,
  ledgerTipBackdatedValidFromMs,
} from "@al-ft/midgard-core/ogmios-slot";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKOperators from "@al-ft/midgard-sdk/operators";
import * as SDKStateQueue from "@al-ft/midgard-sdk/state-queue";
import { type LucidEvolution, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  awaitLedgerTipAtLowerBound,
  observedLedgerTip,
} from "../ledger-tip-lower-bound.js";
import {
  ACTIVATION_VALIDITY_WINDOW_MS,
  fetchNodeSet,
  linkPointsToKey,
  registeredNodeMatchesOperator,
} from "../register-active-operator.fetch-hub-oracle-ref-input.js";
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
  activeRoot: Pick<SDKOperators.NodeWithDatum, "utxo" | "datum">,
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
  SDKStateQueue.StateQueueError
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
        new SDKStateQueue.StateQueueError({
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

/**
 * The list nodes an activation spends or cites, from one read of the three
 * operator lists. Other operators' transactions re-create these outputs: a
 * block commitment spends its operator's active node, which is the anchor a
 * later key is inserted behind. A read from before a wait is not one to
 * build from.
 */
export const readActivationNodes = (
  lucid: LucidEvolution,
  contracts: Pick<
    SDK.MidgardValidators,
    "registeredOperators" | "activeOperators" | "retiredOperators"
  >,
  operatorKeyHash: string,
) =>
  Effect.gen(function* () {
    const [registered, active, retired] = yield* Effect.all(
      [
        contracts.registeredOperators,
        contracts.activeOperators,
        contracts.retiredOperators,
      ].map(({ spendingScriptAddress, policyId }) =>
        fetchNodeSet(lucid, spendingScriptAddress, policyId),
      ),
    );
    const missing = (message: string, cause: string = operatorKeyHash) =>
      Effect.fail(new SDKStateQueue.StateQueueError({ message, cause }));
    const registeredNode = registered!.find(({ datum }) =>
      registeredNodeMatchesOperator(datum, operatorKeyHash),
    );
    if (registeredNode === undefined)
      return yield* missing(
        "Operator registration node was not found after registration step",
      );
    const registeredAnchor = registered!.find(({ datum }) =>
      linkPointsToKey(datum, registeredNode.datum.key),
    );
    if (registeredAnchor === undefined)
      return yield* missing(
        "Registered-operators anchor node for activation was not found",
      );
    const activeInsertionAnchor = active!.find(({ datum }) =>
      SDKOperators.orderedNotMemberWitness(datum, operatorKeyHash),
    );
    if (activeInsertionAnchor === undefined)
      return yield* missing(
        "Failed to find active-operators ordered insertion anchor for activation",
        "No active-operators node proves strict ordered non-membership for the operator key",
      );
    const retiredNotMemberWitness = retired!.find(({ datum }) =>
      SDKOperators.orderedNotMemberWitness(datum, operatorKeyHash),
    );
    if (retiredNotMemberWitness === undefined)
      return yield* missing(
        "Failed to find retired-operators witness node proving non-membership for activation",
      );
    return {
      registeredNode,
      registeredAnchor,
      activeInsertionAnchor,
      retiredNotMemberWitness,
    };
  });
