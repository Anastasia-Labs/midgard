import type { View } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { type CommitteeConfig } from "../config.js";
import {
  type CommitteeAvailabilityReads,
  type FollowerBoundary,
  sameBoundary,
} from "../l1/follower/availability-reads.js";
import { AvailabilityResponderAwaitingScanError } from "./responder.js";

/**
 * The responder's canonical boundary, operation context and reconcile step,
 * on the committee follower's facts.
 *
 * The boundary is the follower's view (plan §8.1): while the follower holds
 * the committee unready, or once a rollback undid the view this pass
 * reconciled at, every boundary read throws
 * `AvailabilityResponderAwaitingScanError`. Transaction status, inputs and
 * the verified foreign-spend check all read the same facts, so a responder
 * whose Publish, Settle or Close lost its race to another transaction (every
 * one of them spends only protocol UTxOs) expires that intent once the rival
 * spend is final, instead of waiting on it forever.
 */
export const availabilityResponderOperations = (input: {
  readonly lucid: LucidEvolution;
  readonly reads: CommitteeAvailabilityReads;
  /** Throws unless the committee store is bound to the configured L1 source. */
  readonly assertSourceHealthy: () => Promise<void>;
  readonly context: Omit<
    SDK.DaAvailabilityOperationContext,
    "observe" | "assertActuationCurrent" | "readBoundary"
  >;
}) => {
  const { reads } = input;
  /** The view the last reconciliation ran at; new actions need it current. */
  let reconciledView: View | undefined;
  const readBoundary = async (
    scope?: SDK.DaAvailabilityReadScope,
  ): Promise<FollowerBoundary> => {
    scope?.assertCurrent();
    const read = <T>(run: () => Promise<T>) =>
      scope === undefined ? run() : scope.read(run);
    const boundary = await read(() => reads.readBoundary());
    // Work reconciled at a view a rollback since undid is stale (§8.1).
    if (
      reconciledView !== undefined &&
      !(await read(() => reads.viewValid(reconciledView!)))
    )
      throw new AvailabilityResponderAwaitingScanError(
        "its view rolled back since this pass reconciled; the next pass reconciles again",
      );
    scope?.assertCurrent();
    return boundary;
  };
  const assertActuationCurrent = async (
    scope?: SDK.DaAvailabilityReadScope,
  ): Promise<void> => {
    if (scope) await scope.read(() => input.assertSourceHealthy());
    else await input.assertSourceHealthy();
    await readBoundary(scope);
  };
  const context: SDK.DaAvailabilityOperationContext = {
    ...input.context,
    assertActuationCurrent,
    readBoundary,
    observe: SDK.createDaAvailabilityOperationObserver({
      lucid: input.lucid,
      readBoundary,
      resolveForeignSpend: async (outRef, scope) => {
        const before = await readBoundary(scope);
        try {
          return await SDK.resolveDaAvailabilityForeignSpend({
            ...reads.foreignSpend,
            outRef,
            readBoundary,
            scope,
          });
        } catch (error) {
          // A spend read the follower's view moved under is the wait for the
          // next pass, which reads it again at one view.
          if (
            !(error instanceof AvailabilityResponderAwaitingScanError) &&
            !sameBoundary(before, await readBoundary(scope))
          )
            throw new AvailabilityResponderAwaitingScanError(
              "its view advanced during a canonical spend read; the next pass reads again",
            );
          throw error;
        }
      },
    }),
  };
  const reconcile = async (
    scope?: SDK.DaAvailabilityReadScope,
  ): Promise<"ready" | "pending" | Readonly<{ held: string }>> => {
    // A new pass adopts the follower's current view only for
    // reconciliation; no new action is selected until every durable intent
    // is checked against it.
    reconciledView = undefined;
    reconciledView = (await readBoundary(scope)).view;
    scope?.assertCurrent();
    const results = await SDK.reconcileDaAvailabilityOperations(
      scope === undefined
        ? context
        : {
            ...committeeBoundReadContext(context, scope),
            observationSignal: scope.signal,
            observationTimeoutMs: Math.max(
              1,
              Math.ceil(
                Math.min(
                  context.observationTimeoutMs ?? scope.remainingMs(),
                  scope.remainingMs(),
                ),
              ),
            ),
          },
    );
    scope?.assertCurrent();
    // A held or conflicting intent stops new signing with its reason; it is
    // read afresh on every pass, so evidence that clears it clears the hold.
    const held = results.find(
      (result) => result.status === "held" || result.status === "conflict",
    );
    if (held !== undefined)
      return {
        held: `${held.txHash}: ${held.detail ?? "A conflicting transaction spends this intent's inputs"}`,
      };
    return results.some(
      (result) =>
        result.status !== "confirmed" &&
        result.status !== "included" &&
        result.status !== "expired",
    )
      ? "pending"
      : "ready";
  };
  return { readBoundary, assertActuationCurrent, context, reconcile };
};

/**
 * The operation context with every evidence read bound to `parent` when the
 * SDK passes no narrower scope of its own.
 */
export const committeeBoundReadContext = (
  context: SDK.DaAvailabilityOperationContext,
  parent: SDK.DaAvailabilityReadScope,
): SDK.DaAvailabilityOperationContext => ({
  ...context,
  assertActuationCurrent: (child) =>
    context.assertActuationCurrent(child ?? parent),
  ...(context.readBoundary
    ? {
        readBoundary: (child?: SDK.DaAvailabilityReadScope) =>
          context.readBoundary!(child ?? parent),
      }
    : {}),
  observe: (intent, child) => context.observe(intent, child ?? parent),
});

/**
 * The deployment's `ParametersV1` from the manifest-pinned configuration: the
 * value compiled into the availability and DA attestation validators, shared
 * by the responder and by the attestation apply's pool check.
 */
export const availabilityParametersFromConfig = (
  config: Pick<CommitteeConfig, "availabilityChallenge">,
): SDK.DaAvailabilityParameters => {
  const p = config.availabilityChallenge;
  return SDK.daAvailabilityParameters({
    responseGeometry: SDK.availabilityResponseGeometry(p.responseGeometry),
    daBondLovelace: BigInt(p.daBondLovelace),
    daSlashPenaltyLovelace: BigInt(p.daSlashPenaltyLovelace),
    daBondMinTopUpLovelace: BigInt(p.daBondMinTopUpLovelace),
    daBondPoolFloorLovelace: BigInt(p.daBondPoolFloorLovelace),
    challengeRecordLovelace: BigInt(p.challengeRecordLovelace),
    challengerBondLovelace: BigInt(p.challengerBondLovelace),
    maxOpenFeeLovelace: BigInt(p.maxOpenFeeLovelace),
    maxPublicationFeeLovelace: BigInt(p.maxPublicationFeeLovelace),
    maxSettlementFeeLovelace: BigInt(p.maxSettlementFeeLovelace),
    maxCloseFeeLovelace: BigInt(p.maxCloseFeeLovelace),
    maxTimeoutFeeLovelace: BigInt(p.maxTimeoutFeeLovelace),
  });
};

export const availabilityResponderCollateral = async (
  lucid: Pick<LucidEvolution, "config" | "utxosAt"> & {
    readonly wallet: () => { readonly address: () => Promise<string> };
  },
  fee: bigint,
): Promise<readonly UTxO[]> => {
  const protocol = lucid.config().protocolParameters;
  if (protocol === undefined)
    throw new Error("Availability responder requires live protocol parameters");
  const required = (fee * BigInt(protocol.collateralPercentage) + 99n) / 100n;
  const address = await lucid.wallet().address();
  const candidates = (await lucid.utxosAt(address))
    .filter(
      (utxo) =>
        !utxo.datum &&
        !utxo.datumHash &&
        !utxo.scriptRef &&
        Object.keys(utxo.assets).length === 1 &&
        (utxo.assets.lovelace ?? 0n) >= required,
    )
    .sort((a, b) =>
      `${a.txHash}#${a.outputIndex}`.localeCompare(
        `${b.txHash}#${b.outputIndex}`,
      ),
    );
  if (candidates[0] === undefined)
    throw new Error(
      `Availability responder wallet lacks separate plain-ADA collateral of at least ${required} lovelace`,
    );
  return [candidates[0]];
};

/** A challenge record discovery left out, and why. */
export type AvailabilityResponderSkippedRecord = Readonly<{
  outRef: string;
  /** True when its state-queue node is gone or challenged by another record. */
  stranded: boolean;
  reason: string;
}>;

/** A DACH asset name is 32 bytes: the 4-byte prefix and a 28-byte identity. */
export const DACH_SUFFIX_HEX_LENGTH = 56;
