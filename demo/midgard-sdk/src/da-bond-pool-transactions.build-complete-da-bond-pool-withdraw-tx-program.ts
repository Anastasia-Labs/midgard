import { type LucidEvolution, type TxBuilder } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { daBondPoolBacking, daBondPoolUnlockAt } from "./da-bond-pool.js";
import {
  check,
  DaBondPoolBuildError,
  type DaBondPoolValidity,
  fail,
  refusal,
} from "./da-bond-pool-transactions.append-da-bond-pool-initialization.js";
import {
  type AlignedValidity,
  quorumPoolSpend,
  type QuorumSpendConfig,
  resolvePool,
} from "./da-bond-pool-transactions.build-top-up-da-bond-pool-tx-program.js";
import { MAX_VALIDITY_RANGE_LENGTH_MS } from "./protocol-parameters.js";
import {
  slotAlignedLowerBoundAtOrAfter,
  slotAlignedUpperBoundAtOrBefore,
  type SlotClock,
} from "./validity-range.js";

/**
 * The validity bounds the chain will see, from the bounds the caller asked
 * for: the lower bound moves up to a slot boundary (so the validator's
 * inclusive lower bound is never earlier than asked) and the upper bound down
 * to one (Lucid floors `validTo` to its slot). The builders set exactly these.
 */
const slotAlignedValidity = (
  slotClock: SlotClock,
  validity: AlignedValidity,
): Effect.Effect<AlignedValidity, DaBondPoolBuildError> =>
  check(() => {
    const validFrom =
      validity.validFrom === undefined
        ? undefined
        : slotAlignedLowerBoundAtOrAfter(slotClock, validity.validFrom);
    const validTo =
      validity.validTo === undefined
        ? undefined
        : slotAlignedUpperBoundAtOrBefore(slotClock, validity.validTo);
    if (
      validFrom !== undefined &&
      validTo !== undefined &&
      validFrom >= validTo - 1n
    ) {
      throw refusal(
        "invalid_validity_range",
        "DA bond pool validity range is empty once slot-aligned",
        `validFrom=${validFrom.toString()},validTo=${validTo.toString()}`,
      );
    }
    return { validFrom, validTo };
  }, "invalid_validity_range");

const applyValidity = (tx: TxBuilder, validity: AlignedValidity): TxBuilder => {
  let next = tx;
  if (validity.validFrom !== undefined) {
    next = next.validFrom(Number(validity.validFrom));
  }
  if (validity.validTo !== undefined) {
    next = next.validTo(Number(validity.validTo));
  }
  return next;
};

/**
 * `BeginWithdraw`: the owner quorum moves a `Bonded` pool to
 * `Withdrawing { unlock_at }`, value unchanged. The validator needs a closed,
 * short validity range and recomputes `unlock_at = inclusive upper bound +
 * da_bond_withdraw_delay_ms_v1`. The builder slot-aligns `validTo`, sets that
 * value, and writes `unlock_at` from it (`daBondPoolUnlockAt`), so the datum
 * equals what the validator recomputes. `withdrawDelayMs` must be the
 * deployment profile's `timing.da_bond_withdraw_delay_ms`.
 */
export const buildBeginDaBondPoolWithdrawTxProgram = (
  lucid: LucidEvolution,
  config: QuorumSpendConfig & {
    readonly withdrawDelayMs: bigint;
    readonly validity: DaBondPoolValidity;
  },
): Effect.Effect<TxBuilder, DaBondPoolBuildError> =>
  Effect.gen(function* () {
    const pool = yield* check(
      () => resolvePool(config.poolValidator, config.pool),
      "invalid_pool",
    );
    if (pool.datum !== "Bonded") {
      return yield* fail(
        "pool_not_bonded",
        "DA bond pool BeginWithdraw needs a Bonded pool",
        pool.datumCbor,
      );
    }
    if (config.withdrawDelayMs <= 0n) {
      return yield* fail(
        "invalid_validity_range",
        "DA bond pool withdraw delay must be positive",
        config.withdrawDelayMs.toString(),
      );
    }
    const validity = yield* slotAlignedValidity(lucid, config.validity);
    const validFrom = validity.validFrom!;
    const validTo = validity.validTo!;
    if (validTo - 1n - validFrom > MAX_VALIDITY_RANGE_LENGTH_MS) {
      return yield* fail(
        "invalid_validity_range",
        "DA bond pool BeginWithdraw validity range exceeds the maximum length",
        `validFrom=${validFrom.toString()},validTo=${validTo.toString()},max=${MAX_VALIDITY_RANGE_LENGTH_MS.toString()}`,
      );
    }
    const unlockAt = daBondPoolUnlockAt({
      validToMs: validTo,
      withdrawDelayMs: config.withdrawDelayMs,
    });
    const tx = yield* quorumPoolSpend(
      lucid,
      config,
      pool,
      "BeginWithdraw",
      {},
      { Withdrawing: { unlock_at: unlockAt } },
      pool.lovelace,
    );
    return applyValidity(tx, validity);
  });

/**
 * `CancelWithdraw`: the owner quorum returns a `Withdrawing` pool to `Bonded`,
 * value unchanged. The validator reads no bound, so `validity` is optional.
 */
export const buildCancelDaBondPoolWithdrawTxProgram = (
  lucid: LucidEvolution,
  config: QuorumSpendConfig & {
    readonly validity?: Partial<DaBondPoolValidity>;
  },
): Effect.Effect<TxBuilder, DaBondPoolBuildError> =>
  Effect.gen(function* () {
    const pool = yield* check(
      () => resolvePool(config.poolValidator, config.pool),
      "invalid_pool",
    );
    if (pool.datum === "Bonded") {
      return yield* fail(
        "pool_not_withdrawing",
        "DA bond pool CancelWithdraw needs a Withdrawing pool",
        pool.datumCbor,
      );
    }
    const validity = yield* slotAlignedValidity(lucid, config.validity ?? {});
    const tx = yield* quorumPoolSpend(
      lucid,
      config,
      pool,
      "CancelWithdraw",
      {},
      "Bonded",
      pool.lovelace,
    );
    return applyValidity(tx, validity);
  });

/**
 * `CompleteWithdraw { amount }`: at or after `unlock_at` (the validator reads
 * the inclusive lower bound), the owner quorum takes `0 < amount <= backing`
 * and the pool continues `Bonded` with `input - amount`. The caller chooses
 * `destination` (a bech32 address) for the withdrawn lovelace.
 * `skipUnlockPrecheck` lets an emulator negative build an early completion
 * for the validator to refuse.
 */
export const buildCompleteDaBondPoolWithdrawTxProgram = (
  lucid: LucidEvolution,
  config: QuorumSpendConfig & {
    readonly amount: bigint;
    readonly destination: string;
    readonly validity: Pick<DaBondPoolValidity, "validFrom"> &
      Partial<Pick<DaBondPoolValidity, "validTo">>;
    readonly skipUnlockPrecheck?: true;
  },
): Effect.Effect<TxBuilder, DaBondPoolBuildError> =>
  Effect.gen(function* () {
    const pool = yield* check(
      () => resolvePool(config.poolValidator, config.pool),
      "invalid_pool",
    );
    if (pool.datum === "Bonded") {
      return yield* fail(
        "pool_not_withdrawing",
        "DA bond pool CompleteWithdraw needs a Withdrawing pool",
        pool.datumCbor,
      );
    }
    const unlockAt = pool.datum.Withdrawing.unlock_at;
    const validity = yield* slotAlignedValidity(lucid, config.validity);
    const validFrom = validity.validFrom!;
    if (config.skipUnlockPrecheck !== true && validFrom < unlockAt) {
      return yield* fail(
        "before_unlock",
        "DA bond pool CompleteWithdraw lower bound is before unlock_at",
        `validFrom=${validFrom.toString()},unlock_at=${unlockAt.toString()}`,
      );
    }
    if (config.amount <= 0n) {
      return yield* fail(
        "invalid_amount",
        "DA bond pool withdrawal amount must be positive",
        config.amount.toString(),
      );
    }
    const backing = daBondPoolBacking({
      lovelace: pool.lovelace,
      parameters: config.parameters,
    });
    if (config.amount > backing) {
      return yield* fail(
        "amount_exceeds_backing",
        "DA bond pool withdrawal amount exceeds the pool's backing",
        `amount=${config.amount.toString()},backing=${backing.toString()}`,
      );
    }
    const tx = yield* quorumPoolSpend(
      lucid,
      config,
      pool,
      "CompleteWithdraw",
      { amount: config.amount },
      "Bonded",
      pool.lovelace - config.amount,
    );
    return applyValidity(
      tx.pay.ToAddress(config.destination, { lovelace: config.amount }),
      validity,
    );
  });
