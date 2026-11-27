import { calculateMinLovelaceFromUTxO } from "@lucid-evolution/lucid";

import { type DaAvailabilityParameters } from "./availability-challenge.js";
import { type DaAvailabilityOpeningInputRequirement } from "./availability-challenge-transactions.at.js";

/** The ledger's collateral input cap; the SDK builders select within it. */
const MAX_COLLATERAL_COINS = 3;

/** The widest coin a plain-ADA output can hold, so its min-ADA is the largest. */
const WIDEST_COIN = 2n ** 63n;

const ceilPercent = (lovelace: bigint, percentage: number): bigint =>
  (lovelace * BigInt(percentage) + 99n) / 100n;

/** The live min-ADA of a plain-ADA coin of `lovelace` at `address`. */
export const daAvailabilityPlainCoinMinAda = (
  coinsPerUtxoByte: bigint,
  address: string,
  lovelace: bigint,
): bigint =>
  calculateMinLovelaceFromUTxO(coinsPerUtxoByte, {
    address,
    assets: { lovelace },
    txHash: "00".repeat(32),
    outputIndex: 0,
  });

/**
 * The opening input an Open with fee `feeLovelace` spends, and the collateral
 * the same Open posts. The collateral is the ledger percentage of the fee; a
 * larger collateral set must leave a valid plain-ADA collateral return.
 */
export const daAvailabilityOpeningInputRequirement = (input: {
  readonly parameters: Pick<
    DaAvailabilityParameters,
    "challenger_bond_lovelace" | "challenge_record_lovelace"
  >;
  readonly challengerAddress: string;
  readonly feeLovelace: bigint;
  readonly collateralPercentage: number;
  readonly coinsPerUtxoByte: bigint;
}): DaAvailabilityOpeningInputRequirement => {
  const collateralLovelace = ceilPercent(
    input.feeLovelace,
    input.collateralPercentage,
  );
  return {
    address: input.challengerAddress,
    lovelace:
      input.parameters.challenger_bond_lovelace +
      input.parameters.challenge_record_lovelace +
      input.feeLovelace,
    bondLovelace: input.parameters.challenger_bond_lovelace,
    recordLovelace: input.parameters.challenge_record_lovelace,
    feeLovelace: input.feeLovelace,
    collateralLovelace,
    collateralReturnLovelace: daAvailabilityPlainCoinMinAda(
      input.coinsPerUtxoByte,
      input.challengerAddress,
      collateralLovelace,
    ),
  };
};

/** The requirement in words, amounts in lovelace. */
export const describeDaAvailabilityOpeningInput = (
  r: DaAvailabilityOpeningInputRequirement,
): string =>
  `one ADA-only coin with no datum and no reference script of exactly ${r.lovelace.toString()} lovelace (challenger_bond_lovelace ${r.bondLovelace.toString()} + challenge_record_lovelace ${r.recordLovelace.toString()} + fee ${r.feeLovelace.toString()}) at ${r.address}, plus separate plain-ADA collateral of exactly ${r.collateralLovelace.toString()} or at least ${(r.collateralLovelace + r.collateralReturnLovelace).toString()} lovelace in at most ${MAX_COLLATERAL_COINS.toString()} coins`;

/**
 * The largest fee any step of one challenge workflow can carry: an Open,
 * publication, settlement or close at its cap, a removal continuation at
 * `max_timeout_fee`, or a Timeout at the slashed penalty plus the
 * challenger's `max_timeout_fee` share.
 */
export const daAvailabilityLargestWorkflowFeeLovelace = (
  p: Pick<
    DaAvailabilityParameters,
    | "max_open_fee_lovelace"
    | "max_publication_fee_lovelace"
    | "max_settlement_fee_lovelace"
    | "max_close_fee_lovelace"
    | "max_timeout_fee_lovelace"
    | "da_slash_penalty_lovelace"
  >,
): bigint =>
  [
    p.max_open_fee_lovelace,
    p.max_publication_fee_lovelace,
    p.max_settlement_fee_lovelace,
    p.max_close_fee_lovelace,
    p.da_slash_penalty_lovelace + p.max_timeout_fee_lovelace,
  ].reduce((max, fee) => (fee > max ? fee : max));

/**
 * One plain-ADA collateral coin every step of a challenge workflow accepts:
 * the ledger percentage of the largest workflow fee plus the min-ADA of the
 * widest plain-ADA collateral return, so a step with a smaller fee still
 * leaves a valid return. Collateral is only taken from a failing script, so
 * the one coin serves the Open, the Timeout and every step between.
 */
export const daAvailabilityWorkflowCollateralLovelace = (input: {
  readonly parameters: Parameters<
    typeof daAvailabilityLargestWorkflowFeeLovelace
  >[0];
  readonly challengerAddress: string;
  readonly collateralPercentage: number;
  readonly coinsPerUtxoByte: bigint;
}): bigint =>
  ceilPercent(
    daAvailabilityLargestWorkflowFeeLovelace(input.parameters),
    input.collateralPercentage,
  ) +
  daAvailabilityPlainCoinMinAda(
    input.coinsPerUtxoByte,
    input.challengerAddress,
    WIDEST_COIN,
  );
