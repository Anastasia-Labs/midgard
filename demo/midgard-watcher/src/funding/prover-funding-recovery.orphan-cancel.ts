import {
  parseWorkflowFundingAbandonmentHandoff,
  parseWorkflowFundingSubmissionHandoff,
  type SignedWorkflowTransactionRetirement,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";

import {
  refreshWatcherProtocolParameterRuntimeAuthority,
  type WatcherProtocolParameterRuntimeAuthority,
} from "./prover-funding.js";
import {
  signWatcherOrphanCancel,
  WATCHER_ORPHAN_CANCEL_AFTER_READS,
  type WatcherOrphanCancelFees,
  type WatcherOrphanCancelWallet,
} from "./prover-funding-orphan-cancel.transaction.js";
import type {
  WatcherProverFundingReservationRecord,
  WatcherProverFundingReservationStore,
} from "./prover-funding-reservation.js";
import { transactionInputOutRefs } from "./sqlite-prover-funding-reservation-store.derive-signed-transition.js";

/** Whether a fault-proof target can no longer run again, so no fresh
 * execution can ever take an orphan of it over. */
export type WatcherOrphanTargetGone = (
  target: Readonly<{ category: string; headerHash: string }>,
) => boolean | Promise<boolean>;

/** What a cancel needs: the wallet, the target predicate and live fees.
 * Without any of them an orphan is never cancelled. */
export type WatcherOrphanCancelConfig = Readonly<{
  wallet: WatcherOrphanCancelWallet;
  targetGone: WatcherOrphanTargetGone;
  fees: () => Promise<WatcherOrphanCancelFees>;
}>;

/** One attempt as this pass left it. */
export type OrphanAttemptState =
  | Readonly<{ final: SignedWorkflowTransactionRetirement }>
  | Readonly<{
      final?: undefined;
      handOver: boolean;
      /** It landed within release finality. */
      landed: boolean;
      /** Neither landed nor in the mempool, with its inputs unspent or
       * outside the follower's history: it may never retire by itself. */
      stranded: boolean;
    }>;

/** The prover wallet and the target predicate a cancel needs. */
export type WatcherOrphanCancelInput = Readonly<{
  wallet: WatcherOrphanCancelWallet;
  targetGone: WatcherOrphanTargetGone;
}>;

/** The cancel configuration, with fees from the live protocol parameters. */
export const watcherOrphanCancelConfig = (
  input: Readonly<{
    orphanCancel?: WatcherOrphanCancelInput;
    protocolParameters: WatcherProtocolParameterRuntimeAuthority;
  }>,
): WatcherOrphanCancelConfig | undefined =>
  input.orphanCancel === undefined
    ? undefined
    : {
        ...input.orphanCancel,
        fees: async () => {
          const { snapshot } =
            await refreshWatcherProtocolParameterRuntimeAuthority(
              input.protocolParameters,
            );
          return {
            minFeeA: BigInt(snapshot.minFeeA),
            minFeeB: BigInt(snapshot.minFeeB),
            coinsPerUtxoByte: BigInt(snapshot.coinsPerUtxoByte),
          };
        },
      };

export const ORPHAN_CANCEL_IN_FLIGHT = "a cancel of its funding is in flight";
/** A cancel that retired unlanded because its validity passed. */
export const ORPHAN_CANCEL_EXPIRED = "its cancel expired unlanded";
/** A cancel that retired unlanded because another transaction spent its
 * funding first. */
export const ORPHAN_CANCEL_INVALIDATED =
  "its cancel was invalidated: another transaction spent its funding";

/** The target an orphan's recorded handoffs name, or null. */
const targetOf = async (
  store: WatcherProverFundingReservationStore,
  reservationId: string,
) => {
  for (const saved of (await store.readRecordedSubmissions?.({
    reservationId,
  })) ?? []) {
    const { identity } = parseWorkflowFundingSubmissionHandoff(
      (saved as { handoff: unknown }).handoff,
    );
    if (identity.target.kind === "state_queue_header")
      return {
        category: identity.category,
        headerHash: identity.target.headerHash,
      };
  }
  for (const saved of (await store.readLegacyAbandonedTransactions?.({
    reservationId,
  })) ?? []) {
    const { identity } = parseWorkflowFundingAbandonmentHandoff(
      (saved as { handoff: unknown }).handoff,
    );
    if (identity.target.kind === "state_queue_header")
      return {
        category: identity.category,
        headerHash: identity.target.headerHash,
      };
  }
  return null;
};

/**
 * Cancels the funding of an orphan whose attempts cannot retire by
 * themselves: an attempt without an expiry that left the mempool unspent,
 * of a target no fresh execution can take over. The cancel spends the
 * funding back to the wallet, invalidating those attempts. At most one
 * cancel is live per orphan; a new one is signed only after the last
 * retired past release finality without landing.
 */
export const createWatcherOrphanCancels = (
  input: Readonly<{
    store: WatcherProverFundingReservationStore;
    config: WatcherOrphanCancelConfig;
    now: () => number;
  }>,
) => {
  const { store, config } = input;
  /** Consecutive complete reads that found the orphan cancellable. */
  const rounds = new Map<string, number>();
  /** The cancel this process submitted for each orphan. */
  const submitted = new Map<string, string>();
  const submit = async (
    reservationId: string,
    cancel: {
      transactionHash: string;
      signedTransactionCborHex: string;
    },
  ) => {
    submitted.set(reservationId, cancel.transactionHash);
    try {
      await config.wallet.submit(cancel.signedTransactionCborHex);
    } catch {
      // The recorded bytes are submitted again on the next read.
    }
  };
  /** The funding inputs a cancel spends, or null when it is not due. */
  const cancellable = async (
    record: WatcherProverFundingReservationRecord,
    attempts: readonly Readonly<{
      transactionHash: string;
      signedTransactionCborHex: string;
    }>[],
    states: ReadonlyMap<string, OrphanAttemptState>,
  ) => {
    const open = attempts.filter(
      ({ transactionHash }) => states.get(transactionHash)?.final === undefined,
    );
    if (
      open.some(({ transactionHash }) => {
        const state = states.get(transactionHash);
        return (
          state === undefined ||
          state.final !== undefined ||
          !state.handOver ||
          state.landed
        );
      })
    )
      return null;
    const funding = new Map(
      record.activeInputs
        .filter(({ role }) => role === "funding")
        .map((leased) => [leased.outRef, leased]),
    );
    const spent = new Set<string>();
    for (const { transactionHash, signedTransactionCborHex } of open) {
      const body = CML.Transaction.from_cbor_hex(
        signedTransactionCborHex,
      ).body();
      // An attempt with an expiry retires by itself; one whose input is
      // spent is invalidated by itself.
      if (body.ttl() !== undefined) continue;
      if (!(states.get(transactionHash) as { stranded: boolean }).stranded)
        return null;
      const own = transactionInputOutRefs(body.inputs()).filter((outRef) =>
        funding.has(outRef),
      );
      for (const outRef of own) spent.add(outRef);
    }
    if (spent.size === 0) return null;
    const completed =
      (await store.readCompletionHandoff({
        reservationId: record.reservationId,
      })) !== null;
    const target = completed
      ? null
      : await targetOf(store, record.reservationId);
    if (!completed && (target === null || !(await config.targetGone(target))))
      return null;
    return [...spent].map((outRef) => funding.get(outRef)!);
  };
  return Object.freeze({
    /**
     * One reading of an orphan that did not settle; an attempt it did not
     * read has no state, and no cancel is signed then. Returns why it is
     * held when a cancel is in flight, or null.
     */
    step: async (
      record: WatcherProverFundingReservationRecord,
      attempts: readonly Readonly<{
        transactionHash: string;
        signedTransactionCborHex: string;
      }>[],
      states: ReadonlyMap<string, OrphanAttemptState>,
    ): Promise<string | null> => {
      const { reservationId } = record;
      const cancel = await store.readOrphanCancel?.({ reservationId });
      if (cancel != null) {
        rounds.delete(reservationId);
        const state = states.get(cancel.transactionHash);
        if (state?.final !== undefined) {
          if (state.final.reason === "included")
            return "its cancel landed; it settles once its attempts are release-final";
          await store.dropRetiredOrphanCancel?.({
            orphan: record,
            retirement: state.final,
          });
          return state.final.reason === "expired"
            ? ORPHAN_CANCEL_EXPIRED
            : ORPHAN_CANCEL_INVALIDATED;
        }
        if (
          state !== undefined &&
          !state.landed &&
          (state.stranded ||
            submitted.get(reservationId) !== cancel.transactionHash)
        )
          await submit(reservationId, cancel);
        return ORPHAN_CANCEL_IN_FLIGHT;
      }
      const inputs = await cancellable(record, attempts, states);
      if (inputs === null) {
        rounds.delete(reservationId);
        return null;
      }
      const agreed = (rounds.get(reservationId) ?? 0) + 1;
      rounds.set(reservationId, agreed);
      if (agreed < WATCHER_ORPHAN_CANCEL_AFTER_READS) return null;
      rounds.delete(reservationId);
      const signedTransactionCborHex = await signWatcherOrphanCancel({
        inputs,
        wallet: config.wallet,
        fees: await config.fees(),
        now: input.now(),
      });
      await store.recordOrphanCancel?.({
        orphan: record,
        signedTransactionCborHex,
      });
      const recorded = await store.readOrphanCancel?.({ reservationId });
      if (recorded == null) return null;
      await submit(reservationId, recorded);
      return ORPHAN_CANCEL_IN_FLIGHT;
    },
    /** Forgets every orphan but `held`: one that settled or left. */
    retain: (held: ReadonlySet<string>) => {
      for (const tracked of [rounds, submitted])
        for (const reservationId of [...tracked.keys()])
          if (!held.has(reservationId)) tracked.delete(reservationId);
    },
  });
};

export type WatcherOrphanCancels = ReturnType<
  typeof createWatcherOrphanCancels
>;
