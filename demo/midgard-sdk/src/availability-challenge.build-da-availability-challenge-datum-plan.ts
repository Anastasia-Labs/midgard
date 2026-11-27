import { Data } from "@lucid-evolution/lucid";

import {
  assertCanonicalDaAvailabilityChallengeRecord,
  assertCanonicalDaAvailabilityCommitment,
  assertCanonicalDaAvailabilityTerminalAccumulatorDatum,
  assertCanonicalDaAvailabilityTrancheDatum,
} from "./availability-challenge.assert-canonical-da-availability-commitment.js";
import {
  assertCanonicalDaAvailabilityParameters,
  daAvailabilityTerminalAccumulatorStart,
  daAvailabilityTrancheStartAccumulator,
} from "./availability-challenge.assert-canonical-da-availability-parameters.js";
import {
  daAvailabilityChallengeAssetName,
  DaAvailabilityCommitmentError,
  daAvailabilityResponseDeadline,
  DaAvailabilityTerminalAccumulatorDatum,
  DaAvailabilityTerminalAccumulatorDatumSchema,
  parseCanonicalDataCbor,
  requireHash,
  requireSafePositiveInteger,
} from "./availability-challenge.da-availability-mint-redeemer-schema.js";
import {
  DaAvailabilityChallengeRecord,
  DaAvailabilityCommitment,
  DaAvailabilityParameters,
  DaAvailabilityTrancheDatum,
  DaAvailabilityTrancheTerminalStatus,
} from "./availability-challenge.da-availability-tranche-datum-schema.js";
import { type OutputReference } from "./common.js";

export const encodeDaAvailabilityTerminalAccumulatorDatum = (
  datum: DaAvailabilityTerminalAccumulatorDatum,
): string => {
  assertCanonicalDaAvailabilityTerminalAccumulatorDatum(datum);
  return Data.to(
    datum as never,
    DaAvailabilityTerminalAccumulatorDatumSchema as never,
  );
};

export const parseDaAvailabilityTerminalAccumulatorDatumCbor = (
  cborHex: string,
): DaAvailabilityTerminalAccumulatorDatum => {
  const datum = parseCanonicalDataCbor<
    typeof DaAvailabilityTerminalAccumulatorDatumSchema,
    DaAvailabilityTerminalAccumulatorDatum
  >({
    cborHex,
    schema: DaAvailabilityTerminalAccumulatorDatumSchema,
    name: "availability terminal accumulator datum",
  });
  assertCanonicalDaAvailabilityTerminalAccumulatorDatum(datum);
  return datum;
};

export type DaAvailabilityChallengeDatumPlan = Readonly<{
  challengeAssetName: string;
  responseDeadline: bigint;
  /** The challenge record datum `OpenChallenge` mints alongside the DACH token. */
  record: DaAvailabilityChallengeRecord;
  /** Exactly `parameters.challenge_record_lovelace`; the record holds no more. */
  recordLovelace: bigint;
  trancheThreads: readonly DaAvailabilityTrancheDatum[];
  trancheFunding: readonly DaAvailabilityTrancheFunding[];
  terminalAccumulator: DaAvailabilityTerminalAccumulatorDatum;
  terminalAccumulatorFundingLovelace: bigint;
}>;

export type DaAvailabilityTrancheFunding = Readonly<{
  trancheIndex: number;
  initialLovelace: bigint;
  maximumPublicationFeeReserveLovelace: bigint;
  maximumSettlementFeeReserveLovelace: bigint;
}>;

/**
 * Deterministic exact split of the approved challenger fee bond. Each tranche
 * first receives enough value for every publication and its one settlement at
 * their authenticated fee ceilings; the terminal accumulator separately holds
 * the larger close/timeout ceiling. All remaining working/refund value is
 * split with a one-lovelace remainder assigned to the earliest descriptors.
 */
export const planDaAvailabilityTrancheFunding = (input: {
  readonly commitment: DaAvailabilityCommitment;
  readonly parameters: DaAvailabilityParameters;
}): readonly DaAvailabilityTrancheFunding[] => {
  assertCanonicalDaAvailabilityParameters(input.parameters);
  assertCanonicalDaAvailabilityCommitment(
    input.commitment,
    input.parameters.response_geometry,
  );
  const trancheCount = input.commitment.tranche_descriptors.length;
  requireSafePositiveInteger(trancheCount, "trancheCount");
  const chunkByteLength = Number(
    input.parameters.response_geometry.chunk_byte_length,
  );
  const publicationFeeReserves = input.commitment.tranche_descriptors.map(
    (descriptor) =>
      BigInt(Math.ceil(Number(descriptor.byte_length) / chunkByteLength)) *
      input.parameters.max_publication_fee_lovelace,
  );
  const totalPublicationFeeReserve = publicationFeeReserves.reduce(
    (total, reserve) => total + reserve,
    0n,
  );
  const totalSettlementFeeReserve =
    BigInt(trancheCount) * input.parameters.max_settlement_fee_lovelace;
  const terminalFeeCeiling =
    input.parameters.max_close_fee_lovelace >
    input.parameters.max_timeout_fee_lovelace
      ? input.parameters.max_close_fee_lovelace
      : input.parameters.max_timeout_fee_lovelace;
  const distributable =
    input.parameters.challenger_bond_lovelace -
    totalPublicationFeeReserve -
    totalSettlementFeeReserve -
    terminalFeeCeiling;
  if (distributable <= 0n) {
    throw new DaAvailabilityCommitmentError(
      "challenger bond does not leave working/refund value after publication, settlement, and terminal fee reserves",
    );
  }
  const count = BigInt(trancheCount);
  const base = distributable / count;
  const remainder = distributable % count;
  return publicationFeeReserves.map(
    (maximumPublicationFeeReserveLovelace, trancheIndex) => ({
      trancheIndex,
      maximumPublicationFeeReserveLovelace,
      maximumSettlementFeeReserveLovelace:
        input.parameters.max_settlement_fee_lovelace,
      initialLovelace:
        maximumPublicationFeeReserveLovelace +
        input.parameters.max_settlement_fee_lovelace +
        base +
        (BigInt(trancheIndex) < remainder ? 1n : 0n),
    }),
  );
};

/**
 * Datum/value-topology plan for the approved split challenger fee bond. It
 * fixes identity, deadline, the challenge record and the initial per-tranche
 * shares, while the measured fee ceiling and each exact transaction fee remain
 * separate. `commitment` is the full signed commitment whose
 * `daAvailabilityCommitmentHash` the Attested state-queue node carries.
 * `challengerFundingOutRef` is the challenger input `OpenChallenge` consumes;
 * it derives the DACH identity. `openedAt` must be the open transaction's
 * inclusive upper validity bound (`validTo - 1`); the validator refuses any
 * other anchor.
 */
export const buildDaAvailabilityChallengeDatumPlan = (input: {
  readonly commitment: DaAvailabilityCommitment;
  readonly challengerFundingOutRef: OutputReference;
  readonly challenger: string;
  readonly openedAt: bigint;
  readonly parameters: DaAvailabilityParameters;
}): DaAvailabilityChallengeDatumPlan => {
  assertCanonicalDaAvailabilityParameters(input.parameters);
  assertCanonicalDaAvailabilityCommitment(
    input.commitment,
    input.parameters.response_geometry,
  );
  requireHash(input.challenger, 28, "challenger");
  const commitment = input.commitment;
  const challengeAssetName = daAvailabilityChallengeAssetName(
    input.challengerFundingOutRef,
  );
  const responseDeadline = daAvailabilityResponseDeadline({
    payloadByteLength: Number(commitment.payload_byte_length),
    openedAt: input.openedAt,
  });
  const record: DaAvailabilityChallengeRecord = {
    commitment,
    challenge_asset_name: challengeAssetName,
    challenger: input.challenger,
    opened_at: input.openedAt,
    response_deadline: responseDeadline,
  };
  const trancheThreads = commitment.tranche_descriptors.map(
    (descriptor): DaAvailabilityTrancheDatum => ({
      Active: {
        deployment_identity: commitment.deployment_identity,
        header_hash: commitment.header_hash,
        challenge_asset_name: challengeAssetName,
        descriptor,
        next_offset: descriptor.start_offset,
        accumulator: daAvailabilityTrancheStartAccumulator({
          deploymentIdentity: commitment.deployment_identity,
          headerHash: commitment.header_hash,
          trancheIndex: Number(descriptor.tranche_index),
          startOffset: Number(descriptor.start_offset),
          byteLength: Number(descriptor.byte_length),
        }),
        latest_carrier_output_index: null,
        response_deadline: responseDeadline,
        challenger: input.challenger,
      },
    }),
  );
  assertCanonicalDaAvailabilityChallengeRecord(record, input.parameters);
  trancheThreads.forEach(assertCanonicalDaAvailabilityTrancheDatum);
  const trancheFunding = planDaAvailabilityTrancheFunding({
    commitment,
    parameters: input.parameters,
  });
  const terminalAccumulatorFundingLovelace =
    input.parameters.max_close_fee_lovelace >
    input.parameters.max_timeout_fee_lovelace
      ? input.parameters.max_close_fee_lovelace
      : input.parameters.max_timeout_fee_lovelace;
  const terminalAccumulator: DaAvailabilityTerminalAccumulatorDatum = {
    deployment_identity: commitment.deployment_identity,
    header_hash: commitment.header_hash,
    challenge_asset_name: challengeAssetName,
    next_tranche_index: 0n,
    folded_terminal_accumulator: daAvailabilityTerminalAccumulatorStart({
      deploymentIdentity: commitment.deployment_identity,
      headerHash: commitment.header_hash,
      challengeAssetName,
    }),
    has_timed_out_tranche: false,
    response_deadline: responseDeadline,
    challenger: input.challenger,
    remaining_challenger_lovelace: terminalAccumulatorFundingLovelace,
  };
  assertCanonicalDaAvailabilityTerminalAccumulatorDatum(terminalAccumulator);
  return {
    challengeAssetName,
    responseDeadline,
    record,
    recordLovelace: input.parameters.challenge_record_lovelace,
    trancheThreads,
    trancheFunding,
    terminalAccumulator,
    terminalAccumulatorFundingLovelace,
  };
};

export const planDaAvailabilityPublicationValueTransition = (input: {
  readonly threadInputLovelace: bigint;
  readonly previousCarrierInputLovelace: bigint;
  readonly nextCarrierOutputLovelace: bigint;
  readonly transactionFeeLovelace: bigint;
  readonly minimumThreadOutputLovelace: bigint;
  readonly isFirstPublication: boolean;
  readonly parameters: DaAvailabilityParameters;
}): bigint => {
  assertCanonicalDaAvailabilityParameters(input.parameters);
  if (
    input.threadInputLovelace <= 0n ||
    input.previousCarrierInputLovelace < 0n ||
    input.nextCarrierOutputLovelace <= 0n ||
    input.minimumThreadOutputLovelace <= 0n ||
    input.transactionFeeLovelace <= 0n ||
    input.transactionFeeLovelace >
      input.parameters.max_publication_fee_lovelace ||
    (input.isFirstPublication
      ? input.previousCarrierInputLovelace !== 0n
      : input.previousCarrierInputLovelace <= 0n)
  ) {
    throw new DaAvailabilityCommitmentError(
      "publication value transition has a noncanonical carrier, thread floor, or fee above its authenticated ceiling",
    );
  }
  const threadOutputLovelace =
    input.threadInputLovelace +
    input.previousCarrierInputLovelace -
    input.nextCarrierOutputLovelace -
    input.transactionFeeLovelace;
  if (threadOutputLovelace < input.minimumThreadOutputLovelace) {
    throw new DaAvailabilityCommitmentError(
      "publication fee/carrier would consume the protected tranche working floor",
    );
  }
  return threadOutputLovelace;
};

export type DaAvailabilitySettlementPlan = Readonly<{
  status: DaAvailabilityTrancheTerminalStatus;
  nextTerminalAccumulator: DaAvailabilityTerminalAccumulatorDatum;
  nextTerminalLovelace: bigint;
}>;
