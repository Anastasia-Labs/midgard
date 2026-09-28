import {
  type BuildTxWithRedeemer,
  Data,
  getAddressDetails,
  type LucidEvolution,
  type RedeemerContext,
  type TxBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import type { DaAvailabilityParameters } from "./availability-challenge.js";
import type { AuthenticatedValidator } from "./common.js";
import { DaParamsDatum } from "./da-attestation.js";
import {
  daBondPoolBacking,
  type DaBondPoolDatum,
  DaBondPoolMintRedeemer,
  DaBondPoolSpendRedeemer,
  daBondPoolUnit,
  daBondPoolUnlockAt,
  decodeDaBondPoolDatum,
  encodeDaBondPoolDatum,
} from "./da-bond-pool.js";
import type { GenericErrorFields } from "./errors.js";
import { MAX_VALIDITY_RANGE_LENGTH_MS } from "./protocol-parameters.js";
import {
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "./tx-context-redeemer.js";
import {
  slotAlignedLowerBoundAtOrAfter,
  slotAlignedUpperBoundAtOrBefore,
  type SlotClock,
} from "./validity-range.js";

/**
 * Transaction builders for the pooled DA committee bond (Aiken
 * `validators/da-bond-pool.ak`): the one-shot `InitPool` mint, the open
 * `TopUp`, and the owner-quorum `BeginWithdraw` / `CancelWithdraw` /
 * `CompleteWithdraw` steps. Every builder returns an incomplete `TxBuilder`;
 * the caller funds, completes, signs and submits it. Without reference-script
 * UTxOs a builder attaches the pool script inline.
 *
 * Timing is never a constant here: the withdraw delay a `BeginWithdraw`
 * writes is the deployment profile's `timing.da_bond_withdraw_delay_ms`, which
 * the validator compiles in, and callers pass it.
 *
 * Redeemer indices are positional and read off the final transaction:
 * `da_params_ref_input_index` counts the sorted reference inputs and
 * `output_index` is the continuing pool output's position.
 */

/** Why a pool build refused, so a caller (and a test) can tell which. */
export type DaBondPoolBuildFailureReason =
  | "amount_exceeds_backing"
  | "before_unlock"
  | "below_floor"
  | "below_min_top_up"
  | "duplicate_signer"
  | "insufficient_signers"
  | "invalid_amount"
  | "invalid_da_params"
  | "invalid_pool"
  | "invalid_pool_address"
  | "invalid_validity_range"
  | "invalid_witness"
  | "missing_witness"
  | "pool_not_bonded"
  | "pool_not_withdrawing"
  | "reference_script_mismatch"
  | "signer_not_owner";

export class DaBondPoolBuildError extends EffectData.TaggedError(
  "DaBondPoolBuildError",
)<GenericErrorFields & { readonly reason: DaBondPoolBuildFailureReason }> {}

/**
 * Published pool scripts. The pool is one multivalidator, so both roles carry
 * the same script.
 */
export type DaBondPoolReferenceScripts = {
  readonly daBondPoolMinting?: UTxO;
  readonly daBondPoolSpending?: UTxO;
};

/** The pool UTxO to spend, as `fetchDaBondPool` returns it. */
export type DaBondPoolSpendInput = {
  readonly utxo: UTxO;
};

/** Validity bounds, in POSIX milliseconds, as passed to Lucid. */
export type DaBondPoolValidity = {
  readonly validFrom: bigint;
  readonly validTo: bigint;
};

const refusal = (
  reason: DaBondPoolBuildFailureReason,
  message: string,
  cause: unknown = undefined,
): DaBondPoolBuildError => new DaBondPoolBuildError({ reason, message, cause });

const fail = (
  reason: DaBondPoolBuildFailureReason,
  message: string,
  cause: unknown = undefined,
): Effect.Effect<never, DaBondPoolBuildError> =>
  Effect.fail(refusal(reason, message, cause));

/** Runs a synchronous check that throws `DaBondPoolBuildError`. */
const check = <A>(
  run: () => A,
  otherwise: DaBondPoolBuildFailureReason,
): Effect.Effect<A, DaBondPoolBuildError> =>
  Effect.try({
    try: run,
    catch: (error) =>
      error instanceof DaBondPoolBuildError
        ? error
        : refusal(
            otherwise,
            error instanceof Error ? error.message : String(error),
            error,
          ),
  });

const outRefLabel = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/**
 * The pool lives at `Address { Script(own policy), None }` (InitPool pins the
 * stake part to None), so a validator object whose spending address has any
 * other shape does not describe the pool.
 */
const requirePoolAddress = (poolValidator: AuthenticatedValidator): string => {
  const address = poolValidator.spendingScriptAddress;
  let details: ReturnType<typeof getAddressDetails>;
  try {
    details = getAddressDetails(address);
  } catch (error) {
    throw refusal(
      "invalid_pool_address",
      "DA bond pool spending address does not decode",
      error,
    );
  }
  if (
    details.paymentCredential?.type !== "Script" ||
    details.paymentCredential.hash !== poolValidator.policyId ||
    poolValidator.spendingScriptHash !== poolValidator.policyId ||
    details.stakeCredential !== undefined
  ) {
    throw refusal(
      "invalid_pool_address",
      "DA bond pool address must be Script(pool policy) with no stake credential",
      address,
    );
  }
  return address;
};

const requireReferenceScript = (
  reference: UTxO | undefined,
  scriptHash: string,
  label: string,
): UTxO | undefined => {
  if (reference === undefined) {
    return undefined;
  }
  if (
    reference.scriptRef == null ||
    validatorToScriptHash(reference.scriptRef) !== scriptHash
  ) {
    throw refusal(
      "reference_script_mismatch",
      `${label} reference script does not carry the DA bond pool script`,
      outRefLabel(reference),
    );
  }
  return reference;
};

/** The one output at the pool address holding the pool NFT. */
const poolOutputIndex = (
  ctx: RedeemerContext,
  poolAddress: string,
  unit: string,
  label: string,
): bigint =>
  requireUniqueOutputIndex(
    ctx.outputs,
    (output) =>
      output.address === poolAddress && (output.assets[unit] ?? 0n) === 1n,
    label,
  );

/**
 * Appends the pool's one-shot `InitPool` to `tx`: mints the one pool NFT and
 * pays it, with `lovelace` and an inline `Bonded` datum, to
 * `Script(pool policy)`. The caller spends the validator's `init_ref` in the
 * same transaction (the atomic protocol init already spends the hub one-shot,
 * which is the pool's `init_ref`).
 *
 * The redeemer's `output_index` is the pool output's position in the final
 * transaction. When `outputIndex` is given, the build also refuses unless the
 * pool lands exactly there.
 *
 * Throws `DaBondPoolBuildError`: this fragment composes into other builders,
 * like `appendEventHistoryInitialization`.
 */
export const appendDaBondPoolInitialization = (
  tx: TxBuilder,
  params: {
    readonly poolValidator: AuthenticatedValidator;
    /** `ParametersV1.da_bond_pool_floor_lovelace`. */
    readonly floorLovelace: bigint;
    readonly lovelace: bigint;
    readonly outputIndex?: bigint;
    readonly referenceScript?: UTxO;
  },
): TxBuilder => {
  const { poolValidator } = params;
  const poolAddress = requirePoolAddress(poolValidator);
  if (params.lovelace < params.floorLovelace) {
    throw refusal(
      "below_floor",
      "DA bond pool initial lovelace is below the pool floor",
      `lovelace=${params.lovelace.toString()},floor=${params.floorLovelace.toString()}`,
    );
  }
  if (params.outputIndex !== undefined && params.outputIndex < 0n) {
    throw refusal(
      "invalid_pool",
      "DA bond pool output index must be non-negative",
      params.outputIndex.toString(),
    );
  }
  const reference = requireReferenceScript(
    params.referenceScript,
    poolValidator.policyId,
    "DA bond pool minting",
  );
  const unit = daBondPoolUnit(poolValidator.policyId);
  const initRedeemer = ((ctx) => {
    const outputIndex = poolOutputIndex(
      ctx,
      poolAddress,
      unit,
      "DA bond pool init",
    );
    if (
      params.outputIndex !== undefined &&
      outputIndex !== params.outputIndex
    ) {
      throw new Error(
        `DA bond pool init output landed at ${outputIndex.toString()}, expected ${params.outputIndex.toString()}`,
      );
    }
    return Data.to(
      { output_index: outputIndex } satisfies DaBondPoolMintRedeemer as never,
      DaBondPoolMintRedeemer as never,
    );
  }) satisfies BuildTxWithRedeemer;

  tx.mintAssets({ [unit]: 1n }, initRedeemer).pay.ToContract(
    poolAddress,
    { kind: "inline", value: encodeDaBondPoolDatum("Bonded") },
    { lovelace: params.lovelace, [unit]: 1n },
  );
  if (reference === undefined) {
    tx.attach.Script(poolValidator.mintingScript);
  } else {
    tx.readFrom([reference]);
  }
  return tx;
};

/**
 * A standalone `InitPool`: spends `initUtxo` (the validator's `init_ref`) and
 * mints the pool with `lovelace >= da_bond_pool_floor_lovelace`.
 */
export const buildInitDaBondPoolTxProgram = (
  lucid: LucidEvolution,
  config: {
    readonly poolValidator: AuthenticatedValidator;
    readonly parameters: DaAvailabilityParameters;
    readonly initUtxo: UTxO;
    readonly lovelace: bigint;
    readonly outputIndex?: bigint;
    readonly referenceScripts?: Pick<
      DaBondPoolReferenceScripts,
      "daBondPoolMinting"
    >;
  },
): Effect.Effect<TxBuilder, DaBondPoolBuildError> =>
  check(
    () =>
      appendDaBondPoolInitialization(
        lucid.newTx().collectFrom([config.initUtxo]),
        {
          poolValidator: config.poolValidator,
          floorLovelace: config.parameters.da_bond_pool_floor_lovelace,
          lovelace: config.lovelace,
          outputIndex: config.outputIndex,
          referenceScript: config.referenceScripts?.daBondPoolMinting,
        },
      ),
    "invalid_pool",
  );

type ResolvedPool = {
  readonly utxo: UTxO;
  readonly datum: DaBondPoolDatum;
  readonly datumCbor: string;
  readonly lovelace: bigint;
  readonly address: string;
  readonly unit: string;
};

/**
 * The pool input, re-read from the UTxO itself: it holds the NFT once, sits
 * at `Script(pool policy)` and carries a canonical inline pool datum.
 */
const resolvePool = (
  poolValidator: AuthenticatedValidator,
  pool: DaBondPoolSpendInput,
): ResolvedPool => {
  const unit = daBondPoolUnit(poolValidator.policyId);
  const { utxo } = pool;
  if ((utxo.assets[unit] ?? 0n) !== 1n) {
    throw refusal(
      "invalid_pool",
      "DA bond pool UTxO must hold the pool NFT exactly once",
      outRefLabel(utxo),
    );
  }
  let payment: ReturnType<typeof getAddressDetails>["paymentCredential"];
  try {
    payment = getAddressDetails(utxo.address).paymentCredential;
  } catch (error) {
    throw refusal(
      "invalid_pool",
      "DA bond pool UTxO address does not decode",
      error,
    );
  }
  if (payment?.type !== "Script" || payment.hash !== poolValidator.policyId) {
    throw refusal(
      "invalid_pool",
      "DA bond pool UTxO must sit at Script(pool policy)",
      utxo.address,
    );
  }
  if (typeof utxo.datum !== "string" || utxo.datumHash != null) {
    throw refusal(
      "invalid_pool",
      "DA bond pool UTxO must carry an inline datum",
      outRefLabel(utxo),
    );
  }
  let datum: DaBondPoolDatum;
  try {
    datum = decodeDaBondPoolDatum(utxo.datum);
  } catch (error) {
    throw refusal(
      "invalid_pool",
      "DA bond pool datum is not a pool datum",
      error,
    );
  }
  return {
    utxo,
    datum,
    datumCbor: utxo.datum,
    lovelace: utxo.assets.lovelace ?? 0n,
    address: utxo.address,
    unit,
  };
};

const attachSpend = (
  tx: TxBuilder,
  poolValidator: AuthenticatedValidator,
  reference: UTxO | undefined,
): TxBuilder =>
  reference === undefined
    ? tx.attach.Script(poolValidator.spendingScript)
    : tx.readFrom([reference]);

/**
 * `TopUp`: anyone adds `amount` lovelace to the pool, in either state; the
 * datum and the NFT carry over unchanged. `amount` must reach
 * `da_bond_min_top_up_lovelace`; `skipMinimumPrecheck` lets an emulator
 * negative build a below-minimum top-up for the validator to refuse.
 */
export const buildTopUpDaBondPoolTxProgram = (
  lucid: LucidEvolution,
  config: {
    readonly poolValidator: AuthenticatedValidator;
    readonly parameters: DaAvailabilityParameters;
    readonly pool: DaBondPoolSpendInput;
    readonly amount: bigint;
    readonly referenceScripts?: Pick<
      DaBondPoolReferenceScripts,
      "daBondPoolSpending"
    >;
    readonly skipMinimumPrecheck?: true;
  },
): Effect.Effect<TxBuilder, DaBondPoolBuildError> =>
  Effect.gen(function* () {
    const pool = yield* check(
      () => resolvePool(config.poolValidator, config.pool),
      "invalid_pool",
    );
    if (config.amount < 0n) {
      return yield* fail(
        "invalid_amount",
        "DA bond pool top-up amount must be non-negative",
        config.amount.toString(),
      );
    }
    if (
      config.skipMinimumPrecheck !== true &&
      config.amount < config.parameters.da_bond_min_top_up_lovelace
    ) {
      return yield* fail(
        "below_min_top_up",
        "DA bond pool top-up is below da_bond_min_top_up_lovelace",
        `amount=${config.amount.toString()},minimum=${config.parameters.da_bond_min_top_up_lovelace.toString()}`,
      );
    }
    const reference = yield* check(
      () =>
        requireReferenceScript(
          config.referenceScripts?.daBondPoolSpending,
          config.poolValidator.spendingScriptHash,
          "DA bond pool spending",
        ),
      "reference_script_mismatch",
    );
    const topUpRedeemer = ((ctx) =>
      Data.to(
        {
          TopUp: {
            output_index: poolOutputIndex(
              ctx,
              pool.address,
              pool.unit,
              "DA bond pool top-up",
            ),
          },
        } satisfies DaBondPoolSpendRedeemer as never,
        DaBondPoolSpendRedeemer as never,
      )) satisfies BuildTxWithRedeemer;
    const tx = lucid
      .newTx()
      .collectFrom([pool.utxo], topUpRedeemer)
      .pay.ToContract(
        pool.address,
        { kind: "inline", value: pool.datumCbor },
        { lovelace: pool.lovelace + config.amount, [pool.unit]: 1n },
      );
    return attachSpend(tx, config.poolValidator, reference);
  });

type QuorumSpendConfig = {
  readonly poolValidator: AuthenticatedValidator;
  readonly parameters: DaAvailabilityParameters;
  readonly pool: DaBondPoolSpendInput;
  /** The DA params UTxO (governor NFT, inline `DaParamsDatum`). */
  readonly daParamsUtxo: UTxO;
  /** The signing owners: distinct, all in `owners`, at least `update_threshold`. */
  readonly signerKeyHashes: readonly string[];
  readonly referenceScripts?: Pick<
    DaBondPoolReferenceScripts,
    "daBondPoolSpending"
  >;
  /**
   * Lets an emulator negative build a spend whose signers miss the owner
   * quorum, for the validator to refuse.
   */
  readonly skipQuorumPrecheck?: true;
};

const KEY_HASH = /^[0-9a-f]{56}$/u;

/**
 * The owner quorum the validator counts (`da_params.owner_quorum_met` walks
 * the owners and counts each signer once): every chosen signer is a distinct
 * owner, and there are at least `update_threshold` of them. Throws
 * `DaBondPoolBuildError`.
 */
export const assertDaBondPoolOwnerQuorum = (
  daParams: Pick<DaParamsDatum, "owners" | "update_threshold">,
  signerKeyHashes: readonly string[],
): void => {
  const seen = new Set<string>();
  for (const signer of signerKeyHashes) {
    if (!KEY_HASH.test(signer) || !daParams.owners.includes(signer)) {
      throw refusal(
        "signer_not_owner",
        "DA bond pool signer is not a DA params owner",
        signer,
      );
    }
    if (seen.has(signer)) {
      throw refusal(
        "duplicate_signer",
        "DA bond pool signer is listed twice and would count once",
        signer,
      );
    }
    seen.add(signer);
  }
  if (BigInt(seen.size) < daParams.update_threshold) {
    throw refusal(
      "insufficient_signers",
      "DA bond pool withdrawal needs at least update_threshold owner signatures",
      `signers=${seen.size.toString()},update_threshold=${daParams.update_threshold.toString()}`,
    );
  }
};

const readDaParams = (utxo: UTxO): DaParamsDatum => {
  if (typeof utxo.datum !== "string" || utxo.datumHash != null) {
    throw refusal(
      "invalid_da_params",
      "DA params UTxO must carry an inline datum",
      outRefLabel(utxo),
    );
  }
  try {
    return Data.from(utxo.datum, DaParamsDatum);
  } catch (error) {
    throw refusal(
      "invalid_da_params",
      "DA params UTxO datum is not a DaParamsDatum",
      error,
    );
  }
};

type WithdrawArm = "BeginWithdraw" | "CancelWithdraw" | "CompleteWithdraw";

/**
 * The shared quorum spend: checks the owner quorum against the DA params
 * datum, references the DA params UTxO, adds every chosen owner as a required
 * signer, spends the pool and continues it at the pool address with
 * `nextDatum` and `outputLovelace` plus the NFT.
 */
const quorumPoolSpend = (
  lucid: LucidEvolution,
  config: QuorumSpendConfig,
  pool: ResolvedPool,
  arm: WithdrawArm,
  leadingFields: Readonly<Record<string, bigint>>,
  nextDatum: DaBondPoolDatum,
  outputLovelace: bigint,
): Effect.Effect<TxBuilder, DaBondPoolBuildError> =>
  Effect.gen(function* () {
    const daParams = yield* check(
      () => readDaParams(config.daParamsUtxo),
      "invalid_da_params",
    );
    if (config.skipQuorumPrecheck !== true) {
      yield* check(
        () => assertDaBondPoolOwnerQuorum(daParams, config.signerKeyHashes),
        "insufficient_signers",
      );
    }
    const reference = yield* check(
      () =>
        requireReferenceScript(
          config.referenceScripts?.daBondPoolSpending,
          config.poolValidator.spendingScriptHash,
          "DA bond pool spending",
        ),
      "reference_script_mismatch",
    );
    const label = `DA bond pool ${arm}`;
    const spendRedeemer = ((ctx) =>
      Data.to(
        {
          [arm]: {
            ...leadingFields,
            da_params_ref_input_index: requireReferenceInputIndex(
              ctx,
              config.daParamsUtxo,
              `${label} DA params`,
            ),
            output_index: poolOutputIndex(ctx, pool.address, pool.unit, label),
          },
        } as never,
        DaBondPoolSpendRedeemer as never,
      )) satisfies BuildTxWithRedeemer;
    let tx = lucid
      .newTx()
      .readFrom([config.daParamsUtxo])
      .collectFrom([pool.utxo], spendRedeemer)
      .pay.ToContract(
        pool.address,
        { kind: "inline", value: encodeDaBondPoolDatum(nextDatum) },
        { lovelace: outputLovelace, [pool.unit]: 1n },
      );
    for (const signer of config.signerKeyHashes) {
      tx = tx.addSignerKey(signer);
    }
    return attachSpend(tx, config.poolValidator, reference);
  });

type AlignedValidity = {
  readonly validFrom?: bigint;
  readonly validTo?: bigint;
};

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
