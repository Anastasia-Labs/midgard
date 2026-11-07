import { Data } from "@lucid-evolution/lucid";

import {
  DaAvailabilityCommitmentError,
  daAvailabilityResponseWindowMs,
  type DaAvailabilityTrancheLayout,
  hashDomainAndData,
  parseCanonicalDataCbor,
  requireHash,
  requireSafePositiveInteger,
} from "./availability-challenge.da-availability-mint-redeemer-schema.js";
import {
  CHALLENGE_ASSET_NAME,
  DA_AVAILABILITY_COMMITMENT_VERSION,
  DA_AVAILABILITY_FULL_PAYLOAD_MAX_BYTES,
  DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES,
  DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY,
  DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  DA_AVAILABILITY_SMALL_PAYLOAD_MAX_BYTES,
  DaAvailabilityParameters,
  DaAvailabilityParametersSchema,
  DaAvailabilityResponseGeometry,
  DaAvailabilityTerminalAccumulatorStartSchema,
  DaAvailabilityTrancheStartSchema,
  TERMINAL_ACCUMULATOR_START_DOMAIN,
  TRANCHE_START_DOMAIN,
} from "./availability-challenge.da-availability-tranche-datum-schema.js";

export const assertCanonicalDaAvailabilityResponseGeometry = (
  geometry: DaAvailabilityResponseGeometry,
): void => {
  const chunkByteLength = Number(geometry.chunk_byte_length);
  const trancheByteLength = Number(geometry.tranche_byte_length);
  const maxTrancheCount = Number(geometry.max_tranche_count);
  requireSafePositiveInteger(
    chunkByteLength,
    "response_geometry.chunk_byte_length",
  );
  requireSafePositiveInteger(
    trancheByteLength,
    "response_geometry.tranche_byte_length",
  );
  requireSafePositiveInteger(
    maxTrancheCount,
    "response_geometry.max_tranche_count",
  );
  if (
    BigInt(chunkByteLength) !== geometry.chunk_byte_length ||
    BigInt(trancheByteLength) !== geometry.tranche_byte_length ||
    BigInt(maxTrancheCount) !== geometry.max_tranche_count
  ) {
    throw new DaAvailabilityCommitmentError(
      "response geometry must fit canonical safe integers",
    );
  }
  if (chunkByteLength > DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES) {
    throw new DaAvailabilityCommitmentError(
      "response chunk exceeds the L1 reliable-publication safety ceiling",
    );
  }
  if (
    trancheByteLength < DA_AVAILABILITY_SMALL_PAYLOAD_MAX_BYTES ||
    trancheByteLength > DA_AVAILABILITY_FULL_PAYLOAD_MAX_BYTES
  ) {
    throw new DaAvailabilityCommitmentError(
      "response tranche must cover the complete small class and stay within the 64 MiB payload ceiling",
    );
  }
  if (
    maxTrancheCount > DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY ||
    Math.ceil(DA_AVAILABILITY_FULL_PAYLOAD_MAX_BYTES / trancheByteLength) >
      maxTrancheCount
  ) {
    throw new DaAvailabilityCommitmentError(
      "response geometry cannot cover the 64 MiB class within its authenticated tranche-count bound",
    );
  }
};

export const availabilityResponseGeometry = (input: {
  readonly chunkByteLength: number;
  readonly trancheByteLength: number;
  readonly maxTrancheCount: number;
}): DaAvailabilityResponseGeometry => {
  const geometry = {
    chunk_byte_length: BigInt(input.chunkByteLength),
    tranche_byte_length: BigInt(input.trancheByteLength),
    max_tranche_count: BigInt(input.maxTrancheCount),
  };
  assertCanonicalDaAvailabilityResponseGeometry(geometry);
  return geometry;
};

export const assertCanonicalDaAvailabilityParameters = (
  parameters: DaAvailabilityParameters,
): void => {
  assertCanonicalDaAvailabilityResponseGeometry(parameters.response_geometry);
  if (
    parameters.da_bond_lovelace <= 0n ||
    parameters.da_slash_penalty_lovelace <= 0n ||
    parameters.da_slash_penalty_lovelace >= parameters.da_bond_lovelace
  ) {
    throw new DaAvailabilityCommitmentError(
      "availability release parameters require a positive DA bond and a slash penalty strictly between zero and it",
    );
  }
  if (
    parameters.da_bond_min_top_up_lovelace <= 0n ||
    parameters.da_bond_pool_floor_lovelace <= 0n ||
    parameters.challenge_record_lovelace <= 0n
  ) {
    throw new DaAvailabilityCommitmentError(
      "availability release parameters require a positive DA bond minimum top-up, pool floor and challenge-record lovelace",
    );
  }
  const profileAmounts = {
    daBondLovelace: parameters.da_bond_lovelace,
    daSlashPenaltyLovelace: parameters.da_slash_penalty_lovelace,
    daBondMinTopUpLovelace: parameters.da_bond_min_top_up_lovelace,
    daBondPoolFloorLovelace: parameters.da_bond_pool_floor_lovelace,
    challengeRecordLovelace: parameters.challenge_record_lovelace,
  };
  for (const [key, expected] of Object.entries(
    DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  )) {
    if (profileAmounts[key as keyof typeof profileAmounts] !== expected) {
      throw new DaAvailabilityCommitmentError(
        `availability release parameters ${key} must equal the selected deployment profile's value ${expected.toString()}`,
      );
    }
  }
  if (
    parameters.max_open_fee_lovelace <= 0n ||
    parameters.max_publication_fee_lovelace <= 0n ||
    parameters.max_settlement_fee_lovelace <= 0n ||
    parameters.max_close_fee_lovelace <= 0n ||
    parameters.max_timeout_fee_lovelace <= 0n
  ) {
    throw new DaAvailabilityCommitmentError(
      "availability release fee ceilings must be positive measured values",
    );
  }
  const maximumPublicationCount = BigInt(
    maximumDaAvailabilityPublicationCount(parameters.response_geometry),
  );
  const terminalFeeCeiling =
    parameters.max_close_fee_lovelace > parameters.max_timeout_fee_lovelace
      ? parameters.max_close_fee_lovelace
      : parameters.max_timeout_fee_lovelace;
  if (
    maximumPublicationCount * parameters.max_publication_fee_lovelace +
      parameters.response_geometry.max_tranche_count *
        parameters.max_settlement_fee_lovelace +
      terminalFeeCeiling >=
    parameters.challenger_bond_lovelace
  ) {
    throw new DaAvailabilityCommitmentError(
      "challenger bond must cover every maximum-size publication fee plus the larger terminal fee ceiling",
    );
  }
};

export const daAvailabilityParameters = (input: {
  readonly responseGeometry: DaAvailabilityResponseGeometry;
  readonly daBondLovelace: bigint;
  readonly challengerBondLovelace: bigint;
  readonly maxOpenFeeLovelace: bigint;
  readonly maxPublicationFeeLovelace: bigint;
  readonly maxSettlementFeeLovelace: bigint;
  readonly maxCloseFeeLovelace: bigint;
  readonly maxTimeoutFeeLovelace: bigint;
  readonly daSlashPenaltyLovelace: bigint;
  readonly daBondMinTopUpLovelace: bigint;
  readonly daBondPoolFloorLovelace: bigint;
  readonly challengeRecordLovelace: bigint;
}): DaAvailabilityParameters => {
  const parameters = {
    response_geometry: input.responseGeometry,
    da_bond_lovelace: input.daBondLovelace,
    challenger_bond_lovelace: input.challengerBondLovelace,
    max_open_fee_lovelace: input.maxOpenFeeLovelace,
    max_publication_fee_lovelace: input.maxPublicationFeeLovelace,
    max_settlement_fee_lovelace: input.maxSettlementFeeLovelace,
    max_close_fee_lovelace: input.maxCloseFeeLovelace,
    max_timeout_fee_lovelace: input.maxTimeoutFeeLovelace,
    da_slash_penalty_lovelace: input.daSlashPenaltyLovelace,
    da_bond_min_top_up_lovelace: input.daBondMinTopUpLovelace,
    da_bond_pool_floor_lovelace: input.daBondPoolFloorLovelace,
    challenge_record_lovelace: input.challengeRecordLovelace,
  };
  assertCanonicalDaAvailabilityParameters(parameters);
  return parameters;
};

export const encodeDaAvailabilityParameters = (
  parameters: DaAvailabilityParameters,
): string => {
  assertCanonicalDaAvailabilityParameters(parameters);
  return Data.to(parameters as never, DaAvailabilityParametersSchema as never);
};

/**
 * Strict durable/configuration codec. Shape-compatible or non-canonical CBOR
 * is never accepted as authenticated release parameters.
 */
export const parseDaAvailabilityParametersCbor = (
  cborHex: string,
): DaAvailabilityParameters => {
  const parameters = parseCanonicalDataCbor<
    typeof DaAvailabilityParametersSchema,
    DaAvailabilityParameters
  >({
    cborHex,
    schema: DaAvailabilityParametersSchema,
    name: "availability parameters",
  });
  assertCanonicalDaAvailabilityParameters(parameters);
  return parameters;
};

/**
 * Deterministic minimal tranche partition under the authenticated measured
 * geometry. Its tranche width/count are release data, while the exact 64 KiB
 * and 64 MiB response classes stay protocol-fixed.
 */
export const deriveDaAvailabilityTrancheLayout = (
  payloadByteLength: number,
  responseGeometry: DaAvailabilityResponseGeometry,
): readonly DaAvailabilityTrancheLayout[] => {
  daAvailabilityResponseWindowMs(payloadByteLength);
  assertCanonicalDaAvailabilityResponseGeometry(responseGeometry);
  const trancheByteLength = Number(responseGeometry.tranche_byte_length);
  const trancheCount = Math.ceil(payloadByteLength / trancheByteLength);
  if (trancheCount > Number(responseGeometry.max_tranche_count)) {
    throw new DaAvailabilityCommitmentError(
      "payload requires more than the authenticated response geometry's tranche bound",
    );
  }
  const result: DaAvailabilityTrancheLayout[] = [];
  let startOffset = 0;
  for (let trancheIndex = 0; trancheIndex < trancheCount; trancheIndex += 1) {
    const byteLength = Math.min(
      trancheByteLength,
      payloadByteLength - startOffset,
    );
    result.push({ trancheIndex, startOffset, byteLength });
    startOffset += byteLength;
  }
  return result;
};

export const maximumDaAvailabilityPublicationCount = (
  responseGeometry: DaAvailabilityResponseGeometry,
): number => {
  assertCanonicalDaAvailabilityResponseGeometry(responseGeometry);
  const chunkByteLength = Number(responseGeometry.chunk_byte_length);
  return deriveDaAvailabilityTrancheLayout(
    DA_AVAILABILITY_FULL_PAYLOAD_MAX_BYTES,
    responseGeometry,
  ).reduce(
    (total, tranche) => total + Math.ceil(tranche.byteLength / chunkByteLength),
    0,
  );
};

export const daAvailabilityTrancheStartAccumulator = (input: {
  readonly deploymentIdentity: string;
  readonly headerHash: string;
  readonly trancheIndex: number;
  readonly startOffset: number;
  readonly byteLength: number;
}): string => {
  requireHash(input.deploymentIdentity, 28, "deploymentIdentity");
  requireHash(input.headerHash, 28, "headerHash");
  requireSafePositiveInteger(input.byteLength, "byteLength");
  if (
    !Number.isSafeInteger(input.trancheIndex) ||
    input.trancheIndex < 0 ||
    input.trancheIndex >= DA_AVAILABILITY_MAX_TRANCHE_COUNT_SAFETY ||
    !Number.isSafeInteger(input.startOffset) ||
    input.startOffset < 0
  ) {
    throw new DaAvailabilityCommitmentError(
      "tranche index and start offset must be canonical non-negative integers",
    );
  }
  const cbor = Data.to(
    {
      version: DA_AVAILABILITY_COMMITMENT_VERSION,
      deployment_identity: input.deploymentIdentity,
      header_hash: input.headerHash,
      tranche_index: BigInt(input.trancheIndex),
      start_offset: BigInt(input.startOffset),
      byte_length: BigInt(input.byteLength),
    } as never,
    DaAvailabilityTrancheStartSchema as never,
  );
  return hashDomainAndData(TRANCHE_START_DOMAIN, cbor);
};

export const daAvailabilityTerminalAccumulatorStart = (input: {
  readonly deploymentIdentity: string;
  readonly headerHash: string;
  readonly challengeAssetName: string;
}): string => {
  requireHash(input.deploymentIdentity, 28, "deploymentIdentity");
  requireHash(input.headerHash, 28, "headerHash");
  if (!CHALLENGE_ASSET_NAME.test(input.challengeAssetName)) {
    throw new DaAvailabilityCommitmentError(
      "challengeAssetName must be the canonical 32-byte DACH identity",
    );
  }
  const cbor = Data.to(
    {
      version: DA_AVAILABILITY_COMMITMENT_VERSION,
      deployment_identity: input.deploymentIdentity,
      header_hash: input.headerHash,
      challenge_asset_name: input.challengeAssetName,
    } as never,
    DaAvailabilityTerminalAccumulatorStartSchema as never,
  );
  return hashDomainAndData(TERMINAL_ACCUMULATOR_START_DOMAIN, cbor);
};
