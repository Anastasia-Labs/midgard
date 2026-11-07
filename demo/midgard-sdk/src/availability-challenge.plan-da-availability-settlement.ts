import { Data } from "@lucid-evolution/lucid";

import {
  assertCanonicalDaAvailabilityCommitment,
  assertCanonicalDaAvailabilityTerminalAccumulatorDatum,
  assertCanonicalDaAvailabilityTrancheDatum,
} from "./availability-challenge.assert-canonical-da-availability-commitment.js";
import { assertCanonicalDaAvailabilityParameters } from "./availability-challenge.assert-canonical-da-availability-parameters.js";
import { type DaAvailabilitySettlementPlan } from "./availability-challenge.build-da-availability-challenge-datum-plan.js";
import { foldDaAvailabilityTerminalAccumulator } from "./availability-challenge.build-da-availability-commitment.js";
import {
  DaAvailabilityCommitmentError,
  DaAvailabilityTerminalAccumulatorDatum,
} from "./availability-challenge.da-availability-mint-redeemer-schema.js";
import {
  DaAvailabilityCommitment,
  DaAvailabilityParameters,
  DaAvailabilityTrancheDatum,
  DaAvailabilityTrancheDescriptor,
  DaAvailabilityTrancheDescriptorSchema,
  DaAvailabilityTrancheTerminalStatus,
} from "./availability-challenge.da-availability-tranche-datum-schema.js";

/**
 * Pure mirror of one bounded `SettleTranche` transition. Production builders
 * feed it decoded, script-authenticated UTxO data and then emit the exact datum
 * and value it returns.
 */
export const planDaAvailabilitySettlement = (input: {
  readonly commitment: DaAvailabilityCommitment;
  readonly terminalAccumulator: DaAvailabilityTerminalAccumulatorDatum;
  readonly tranche: DaAvailabilityTrancheDatum;
  readonly threadLovelace: bigint;
  readonly carrierLovelace: bigint;
  readonly transactionFeeLovelace: bigint;
  readonly inclusiveValidityLower: bigint;
  readonly parameters: DaAvailabilityParameters;
}): DaAvailabilitySettlementPlan => {
  assertCanonicalDaAvailabilityParameters(input.parameters);
  assertCanonicalDaAvailabilityCommitment(
    input.commitment,
    input.parameters.response_geometry,
  );
  assertCanonicalDaAvailabilityTerminalAccumulatorDatum(
    input.terminalAccumulator,
  );
  assertCanonicalDaAvailabilityTrancheDatum(input.tranche);
  const terminal = input.terminalAccumulator;
  const descriptorIndex = Number(terminal.next_tranche_index);
  if (
    !Number.isSafeInteger(descriptorIndex) ||
    descriptorIndex < 0 ||
    input.commitment.tranche_descriptors[descriptorIndex] === undefined ||
    input.threadLovelace <= 0n ||
    input.carrierLovelace < 0n ||
    input.transactionFeeLovelace <= 0n ||
    input.transactionFeeLovelace > input.parameters.max_settlement_fee_lovelace
  ) {
    throw new DaAvailabilityCommitmentError(
      "settlement indices, values, or fee are not canonical",
    );
  }
  const descriptor = input.commitment.tranche_descriptors[descriptorIndex]!;
  let status: DaAvailabilityTrancheTerminalStatus;
  let trancheIdentity: {
    readonly deployment_identity: string;
    readonly header_hash: string;
    readonly challenge_asset_name: string;
    readonly descriptor: DaAvailabilityTrancheDescriptor;
    readonly challenger: string;
  };
  if ("Receipt" in input.tranche) {
    trancheIdentity = input.tranche.Receipt;
    if (
      input.tranche.Receipt.terminal_accumulator !==
      descriptor.terminal_accumulator
    ) {
      throw new DaAvailabilityCommitmentError(
        "published settlement receipt does not equal its signed terminal accumulator",
      );
    }
    status = {
      PublishedTranche: {
        terminal_accumulator: descriptor.terminal_accumulator,
      },
    };
  } else {
    trancheIdentity = input.tranche.Active;
    if (input.inclusiveValidityLower < terminal.response_deadline) {
      throw new DaAvailabilityCommitmentError(
        "an active tranche may settle only at or after the authenticated deadline",
      );
    }
    status = {
      TimedOutTranche: {
        next_offset: input.tranche.Active.next_offset,
        partial_accumulator: input.tranche.Active.accumulator,
      },
    };
  }
  if (
    trancheIdentity.deployment_identity !==
      input.commitment.deployment_identity ||
    trancheIdentity.header_hash !== input.commitment.header_hash ||
    trancheIdentity.challenge_asset_name !== terminal.challenge_asset_name ||
    Data.to(
      trancheIdentity.descriptor as never,
      DaAvailabilityTrancheDescriptorSchema as never,
    ) !==
      Data.to(
        descriptor as never,
        DaAvailabilityTrancheDescriptorSchema as never,
      ) ||
    trancheIdentity.challenger !== terminal.challenger ||
    terminal.deployment_identity !== input.commitment.deployment_identity ||
    terminal.header_hash !== input.commitment.header_hash ||
    terminal.next_tranche_index !== descriptor.tranche_index
  ) {
    throw new DaAvailabilityCommitmentError(
      "settlement tranche, terminal accumulator, and signed commitment identities differ",
    );
  }
  const nextTerminalLovelace =
    terminal.remaining_challenger_lovelace +
    input.threadLovelace +
    input.carrierLovelace -
    input.transactionFeeLovelace;
  if (nextTerminalLovelace <= 0n) {
    throw new DaAvailabilityCommitmentError(
      "settlement consumes the protected challenger value",
    );
  }
  const nextTerminalAccumulator: DaAvailabilityTerminalAccumulatorDatum = {
    ...terminal,
    next_tranche_index: terminal.next_tranche_index + 1n,
    folded_terminal_accumulator: foldDaAvailabilityTerminalAccumulator({
      previousAccumulator: terminal.folded_terminal_accumulator,
      trancheIndex: descriptorIndex,
      status,
    }),
    has_timed_out_tranche:
      terminal.has_timed_out_tranche || "TimedOutTranche" in status,
    remaining_challenger_lovelace: nextTerminalLovelace,
  };
  assertCanonicalDaAvailabilityTerminalAccumulatorDatum(
    nextTerminalAccumulator,
  );
  return { status, nextTerminalAccumulator, nextTerminalLovelace };
};

export const assertDaAvailabilityChallengerBondConservation = (input: {
  readonly initialChallengerBondLovelace: bigint;
  readonly currentThreadLovelace: readonly bigint[];
  readonly currentCarrierLovelace: readonly bigint[];
  readonly paidTransactionFeesLovelace: readonly bigint[];
}): void => {
  const allValues = [
    ...input.currentThreadLovelace,
    ...input.currentCarrierLovelace,
  ];
  const allFees = input.paidTransactionFeesLovelace;
  if (
    input.initialChallengerBondLovelace <= 0n ||
    allValues.some((value) => value <= 0n) ||
    allFees.some((fee) => fee <= 0n) ||
    allValues.reduce((total, value) => total + value, 0n) +
      allFees.reduce((total, value) => total + value, 0n) !==
      input.initialChallengerBondLovelace
  ) {
    throw new DaAvailabilityCommitmentError(
      "challenger bond is not isolated and exactly conserved by live threads, carriers, and paid fees",
    );
  }
};

export type DaAvailabilityTrancheProtectedValue = Readonly<{
  trancheIndex: number;
  threadLovelace: bigint;
  carrierLovelace: bigint;
}>;

export type DaAvailabilityTrancheRefund = Readonly<{
  trancheIndex: number;
  refundLovelace: bigint;
  attributedTransactionFeeLovelace: bigint;
}>;

export const planDaAvailabilityTerminalRefund = (input: {
  readonly kind: "close" | "timeout";
  readonly tranches: readonly DaAvailabilityTrancheProtectedValue[];
  readonly transactionFeeLovelace: bigint;
  readonly parameters: DaAvailabilityParameters;
}): readonly DaAvailabilityTrancheRefund[] => {
  assertCanonicalDaAvailabilityParameters(input.parameters);
  const feeCeiling =
    input.kind === "close"
      ? input.parameters.max_close_fee_lovelace
      : input.parameters.max_timeout_fee_lovelace;
  if (
    input.tranches.length === 0 ||
    input.tranches.some(
      (value, index) =>
        value.trancheIndex !== index ||
        value.threadLovelace <= 0n ||
        value.carrierLovelace < 0n,
    ) ||
    input.transactionFeeLovelace <= 0n ||
    input.transactionFeeLovelace > feeCeiling
  ) {
    throw new DaAvailabilityCommitmentError(
      "terminal availability transition has a noncanonical protected value or fee above its authenticated ceiling",
    );
  }
  const refunds = input.tranches.map(
    (value, index): DaAvailabilityTrancheRefund => {
      const attributedTransactionFeeLovelace =
        index === 0 ? input.transactionFeeLovelace : 0n;
      return {
        trancheIndex: value.trancheIndex,
        attributedTransactionFeeLovelace,
        refundLovelace:
          value.threadLovelace +
          value.carrierLovelace -
          attributedTransactionFeeLovelace,
      };
    },
  );
  if (refunds.some((refund) => refund.refundLovelace <= 0n)) {
    throw new DaAvailabilityCommitmentError(
      "terminal availability transition leaves no challenger refund",
    );
  }
  return refunds;
};
