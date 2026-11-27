import {
  type BuildTxWithRedeemer,
  Data,
  getAddressDetails,
  type LucidEvolution,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { DaAvailabilityParameters } from "./availability-challenge.js";
import type { AuthenticatedValidator } from "./common.js";
import { DaParamsDatum } from "./da-attestation.js";
import {
  type DaBondPoolDatum,
  DaBondPoolSpendRedeemer,
  daBondPoolUnit,
  decodeDaBondPoolDatum,
  encodeDaBondPoolDatum,
} from "./da-bond-pool.js";
import {
  check,
  DaBondPoolBuildError,
  type DaBondPoolReferenceScripts,
  type DaBondPoolSpendInput,
  fail,
  outRefLabel,
  poolOutputIndex,
  refusal,
  requireReferenceScript,
  type ResolvedPool,
} from "./da-bond-pool-transactions.append-da-bond-pool-initialization.js";
import { requireReferenceInputIndex } from "./tx-context-redeemer.js";

/**
 * The pool input, re-read from the UTxO itself: it holds the NFT once, sits
 * at `Script(pool policy)` and carries a canonical inline pool datum.
 */
export const resolvePool = (
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

export type QuorumSpendConfig = {
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
export const quorumPoolSpend = (
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

export type AlignedValidity = {
  readonly validFrom?: bigint;
  readonly validTo?: bigint;
};
