import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  advanceDaAvailabilityTranche,
  assertCanonicalDaAvailabilityCommitment,
  assertDaAvailabilityChallengerBondConservation,
  assertDaAvailabilityTerminalReceipts,
  availabilityResponseGeometry,
  buildDaAvailabilityChallengeDatumPlan,
  buildDaAvailabilityCommitment,
  DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE,
  DA_AVAILABILITY_FULL_RESPONSE_WINDOW_MS,
  DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
  DA_AVAILABILITY_SMALL_RESPONSE_WINDOW_MS,
  daAvailabilityAttestationMessage,
  daAvailabilityChallengeAssetName,
  DaAvailabilityChallengeRecord,
  daAvailabilityChunkLeafHash,
  DaAvailabilityCommitment,
  daAvailabilityCommitmentHash,
  DaAvailabilityParameters,
  daAvailabilityParameters,
  DaAvailabilityPublicationDatum,
  daAvailabilityPublicationTier,
  daAvailabilityPublishedTerminalCommitment,
  daAvailabilityResponseDeadline,
  daAvailabilityResponseWindowMs,
  DaAvailabilityStateQueueStatus,
  daAvailabilityStateQueueStatusPermitsMerge,
  daAvailabilityTerminalAccumulatorStart,
  daAvailabilityTrancheAssetName,
  deriveDaAvailabilityTrancheLayout,
  encodeDaAvailabilityChallengeRecord,
  encodeDaAvailabilityCommitment,
  encodeDaAvailabilityParameters,
  encodeDaAvailabilityPublicationDatum,
  encodeDaAvailabilityTerminalAccumulatorDatum,
  encodeDaAvailabilityTrancheDatum,
  maximumDaAvailabilityPublicationCount,
  parseDaAvailabilityChallengeRecordCbor,
  parseDaAvailabilityCommitmentCbor,
  parseDaAvailabilityParametersCbor,
  parseDaAvailabilityPublicationDatumCbor,
  parseDaAvailabilityTerminalAccumulatorDatumCbor,
  parseDaAvailabilityTrancheDatumCbor,
  planDaAvailabilityPublications,
  planDaAvailabilityPublicationsFromChallengeRecord,
  planDaAvailabilityPublicationValueTransition,
  planDaAvailabilitySettlement,
  planDaAvailabilityTerminalRefund,
  reconstructDaAvailabilityPayload,
  verifyDaAvailabilityPayloadCommitment,
} from "../src/availability-challenge.js";
import { daAvailabilityStateQueueStatusIdentity } from "../src/da-availability-state.js";
import {
  build,
  CANDIDATE_GEOMETRY,
  CHALLENGE_ASSET,
  COMMITMENT_GOLDENS,
  COMMITMENT_HASH,
  commitmentFromGolden,
  DEPLOYMENT,
  HEADER,
  MAX_CLOSE_FEE,
  MAX_OPEN_FEE,
  MAX_PUBLICATION_FEE,
  MAX_SETTLEMENT_FEE,
  MAX_TIMEOUT_FEE,
  OUT_REF,
  OWNER,
  parameterInput,
  payload,
  publicationTransactionBytes,
} from "./availability-challenge.publication-transaction-bytes.js";

describe("Q58 canonical DA availability commitment V1", () => {
  it("fixes the approved deadlines and binds the DA amounts to the profile", () => {
    expect(daAvailabilityResponseWindowMs(64 * 1024)).toBe(
      DA_AVAILABILITY_SMALL_RESPONSE_WINDOW_MS,
    );
    expect(daAvailabilityResponseWindowMs(64 * 1024 + 1)).toBe(
      DA_AVAILABILITY_FULL_RESPONSE_WINDOW_MS,
    );
    expect(DA_AVAILABILITY_PROFILE_BOND_AMOUNTS).toEqual({
      daBondLovelace: BigInt(
        SELECTED_DEPLOYMENT_PROFILE.da_bond.da_bond_lovelace,
      ),
      daSlashPenaltyLovelace: BigInt(
        SELECTED_DEPLOYMENT_PROFILE.da_bond.da_slash_penalty_lovelace,
      ),
      daBondMinTopUpLovelace: BigInt(
        SELECTED_DEPLOYMENT_PROFILE.da_bond.da_bond_min_top_up_lovelace,
      ),
      daBondPoolFloorLovelace: BigInt(
        SELECTED_DEPLOYMENT_PROFILE.da_bond.da_bond_pool_floor_lovelace,
      ),
      challengeRecordLovelace: BigInt(
        SELECTED_DEPLOYMENT_PROFILE.da_bond.challenge_record_lovelace,
      ),
    });
    expect(DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE).toBe(
      10_000_000_000n,
    );
    expect(() =>
      daAvailabilityParameters(
        parameterInput({ maxPublicationFeeLovelace: 1_000_000_000n }),
      ),
    ).toThrow("must cover every maximum-size publication fee");
  });

  it("unequal bonds accepted", () => {
    for (const challengerBondLovelace of [
      2_409_200_001n,
      9_999_999_999n,
      12_000_000_000n,
    ]) {
      const parameters = daAvailabilityParameters(
        parameterInput({ challengerBondLovelace }),
      );
      expect(parameters.challenger_bond_lovelace).toBe(challengerBondLovelace);
      expect(parameters.da_bond_lovelace).toBe(
        DA_AVAILABILITY_PROFILE_BOND_AMOUNTS.daBondLovelace,
      );
      expect(parameters.da_bond_lovelace).not.toBe(
        parameters.challenger_bond_lovelace,
      );
      expect(
        parseDaAvailabilityParametersCbor(
          encodeDaAvailabilityParameters(parameters),
        ),
      ).toEqual(parameters);
    }
  });

  it("coverage floor binds the challenger bond only", () => {
    // Every maximum-size publication fee, one settlement fee per tranche and
    // the larger terminal fee ceiling.
    const floor =
      BigInt(maximumDaAvailabilityPublicationCount(CANDIDATE_GEOMETRY)) *
        MAX_PUBLICATION_FEE +
      CANDIDATE_GEOMETRY.max_tranche_count * MAX_SETTLEMENT_FEE +
      MAX_TIMEOUT_FEE;
    expect(
      daAvailabilityParameters(
        parameterInput({ challengerBondLovelace: floor + 1n }),
      ).challenger_bond_lovelace,
    ).toBe(floor + 1n);
    expect(() =>
      daAvailabilityParameters(
        parameterInput({ challengerBondLovelace: floor }),
      ),
    ).toThrow("challenger bond must cover every maximum-size publication fee");
    // The DA bond is never compared to the fee floor: the preprod-testing
    // 500 tADA DA bond sits far below it and is accepted.
    expect(DA_AVAILABILITY_PROFILE_BOND_AMOUNTS.daBondLovelace).toBeLessThan(
      floor,
    );
    // With one-lovelace fee ceilings the floor drops below the DA bond. A
    // challenger bond equal to that floor is still refused: a DA bond above
    // the floor never stands in for the challenger bond.
    const oneLovelaceFees = {
      maxOpenFeeLovelace: 1n,
      maxPublicationFeeLovelace: 1n,
      maxSettlementFeeLovelace: 1n,
      maxCloseFeeLovelace: 1n,
      maxTimeoutFeeLovelace: 1n,
    };
    const smallFloor =
      BigInt(maximumDaAvailabilityPublicationCount(CANDIDATE_GEOMETRY)) +
      CANDIDATE_GEOMETRY.max_tranche_count +
      1n;
    expect(DA_AVAILABILITY_PROFILE_BOND_AMOUNTS.daBondLovelace).toBeGreaterThan(
      smallFloor,
    );
    expect(
      daAvailabilityParameters(
        parameterInput({
          ...oneLovelaceFees,
          challengerBondLovelace: smallFloor + 1n,
        }),
      ).challenger_bond_lovelace,
    ).toBe(smallFloor + 1n);
    expect(() =>
      daAvailabilityParameters(
        parameterInput({
          ...oneLovelaceFees,
          challengerBondLovelace: smallFloor,
        }),
      ),
    ).toThrow("challenger bond must cover every maximum-size publication fee");
  });

  it("500 tADA DA bond + 10k challenger bond passes on preprod-testing", () => {
    expect(SELECTED_DEPLOYMENT_PROFILE.name).toBe("preprod-testing");
    expect(DA_AVAILABILITY_PROFILE_BOND_AMOUNTS.daBondLovelace).toBe(
      500_000_000n,
    );
    const parameters = daAvailabilityParameters(
      parameterInput({ challengerBondLovelace: 10_000_000_000n }),
    );
    expect(parameters.da_bond_lovelace).toBe(500_000_000n);
    expect(parameters.challenger_bond_lovelace).toBe(10_000_000_000n);
    expect(parameters.da_slash_penalty_lovelace).toBe(100_000_000n);
    expect(parameters.da_bond_min_top_up_lovelace).toBe(5_000_000n);
    expect(parameters.da_bond_pool_floor_lovelace).toBe(5_000_000n);
    expect(parameters.challenge_record_lovelace).toBe(27_000_000n);
  });

  it("DA amounts must equal the selected profile", () => {
    for (const [key, expected] of Object.entries(
      DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
    )) {
      for (const drifted of [expected - 1n, expected + 1n]) {
        expect(() =>
          daAvailabilityParameters(parameterInput({ [key]: drifted })),
        ).toThrow(
          `availability release parameters ${key} must equal the selected deployment profile's value ${expected.toString()}`,
        );
      }
    }
    const parameters = daAvailabilityParameters(parameterInput({}));
    const driftedCbor = Data.to(
      { ...parameters, da_bond_lovelace: parameters.da_bond_lovelace + 1n },
      DaAvailabilityParameters,
    );
    expect(() => parseDaAvailabilityParametersCbor(driftedCbor)).toThrow(
      "daBondLovelace must equal the selected deployment profile's value",
    );
  });

  it("refuses a slash penalty outside the DA bond and non-positive pool amounts", () => {
    const bond = DA_AVAILABILITY_PROFILE_BOND_AMOUNTS.daBondLovelace;
    for (const daSlashPenaltyLovelace of [0n, -1n, bond, bond + 1n]) {
      expect(() =>
        daAvailabilityParameters(parameterInput({ daSlashPenaltyLovelace })),
      ).toThrow(
        "require a positive DA bond and a slash penalty strictly between zero and it",
      );
    }
    for (const key of [
      "daBondMinTopUpLovelace",
      "daBondPoolFloorLovelace",
      "challengeRecordLovelace",
    ] as const) {
      expect(() =>
        daAvailabilityParameters(parameterInput({ [key]: 0n })),
      ).toThrow(
        "require a positive DA bond minimum top-up, pool floor and challenge-record lovelace",
      );
    }
  });

  it("uses the authenticated measured geometry without freezing its starting probe", () => {
    expect(
      deriveDaAvailabilityTrancheLayout(64 * 1024, CANDIDATE_GEOMETRY),
    ).toEqual([{ trancheIndex: 0, startOffset: 0, byteLength: 64 * 1024 }]);
    expect(
      deriveDaAvailabilityTrancheLayout(
        4 * 1024 * 1024 + 1,
        CANDIDATE_GEOMETRY,
      ),
    ).toEqual([
      {
        trancheIndex: 0,
        startOffset: 0,
        byteLength: 4 * 1024 * 1024,
      },
      {
        trancheIndex: 1,
        startOffset: 4 * 1024 * 1024,
        byteLength: 1,
      },
    ]);
    const max = deriveDaAvailabilityTrancheLayout(
      64 * 1024 * 1024,
      CANDIDATE_GEOMETRY,
    );
    expect(max).toHaveLength(16);
    expect(max.at(-1)).toEqual({
      trancheIndex: 15,
      startOffset: 15 * 4 * 1024 * 1024,
      byteLength: 4 * 1024 * 1024,
    });

    const alternateGeometry = availabilityResponseGeometry({
      chunkByteLength: 8_000,
      trancheByteLength: 8 * 1024 * 1024,
      maxTrancheCount: 8,
    });
    expect(
      deriveDaAvailabilityTrancheLayout(64 * 1024 * 1024, alternateGeometry),
    ).toHaveLength(8);
  });

  it("measures the signed reference-script publication body with the membership proof", () => {
    const reserveTargetBytes = 16_384 - 512;
    let lower = 1;
    let upper = 15_148;
    while (lower < upper) {
      const candidate = Math.ceil((lower + upper) / 2);
      if (publicationTransactionBytes(candidate) <= reserveTargetBytes) {
        lower = candidate;
      } else {
        upper = candidate - 1;
      }
    }
    expect({
      chunkByteLength: lower,
      signedBytes: publicationTransactionBytes(lower),
      adjacentSignedBytes: publicationTransactionBytes(lower + 1),
    }).toEqual({
      chunkByteLength: 14_020,
      signedBytes: 15_872,
      adjacentSignedBytes: 15_873,
    });
    const activatedGeometry = availabilityResponseGeometry({
      chunkByteLength: 14_020,
      trancheByteLength: 4 * 1024 * 1024,
      maxTrancheCount: 16,
    });
    const maxPayload = new Uint8Array(4 * 1024 * 1024).fill(42);
    const commitment = buildDaAvailabilityCommitment({
      deploymentIdentity: DEPLOYMENT,
      headerHash: HEADER,
      payload: maxPayload,
      responseGeometry: activatedGeometry,
    });
    const [tranche] = planDaAvailabilityPublications({
      commitment,
      payload: maxPayload,
      challengeAssetName: daAvailabilityChallengeAssetName(OUT_REF),
    });
    const first = tranche!.publications[0]!;
    expect({
      chunkCount: tranche!.descriptor.chunk_count,
      chunkCommitment: tranche!.descriptor.chunk_commitment,
      frontier: first.chunk_frontier,
      siblings: first.chunk_siblings,
      leafHash: daAvailabilityChunkLeafHash({
        trancheIndex: 0,
        chunkIndex: 0,
        chunkOffset: 0,
        chunkByteLength: Number(first.chunk_byte_length),
        chunkHash: first.chunk_hash,
      }),
      chunkHash: first.chunk_hash,
      previousAccumulator: first.previous_accumulator,
      nextAccumulator: first.next_accumulator,
    }).toEqual({
      chunkCount: 300n,
      chunkCommitment:
        "406476e3fcfcc50f07f11bb09b9d59bf4101c258327cb8b7ab116757338edba5",
      frontier: [
        {
          height: 2n,
          hash: "1a916a0649ad791a11e9e723fea7da14b128fee860cd129d772f09272465e9ad",
        },
        {
          height: 3n,
          hash: "02cf6052bcd6b453f2b27afa65bd4545cc81fb73d3b452ef4c21fa3400fa58e5",
        },
        {
          height: 5n,
          hash: "e0c5aceb218e4482322bc4e2d2164261295f65cfd31d61469cf1f08fc3ae66b7",
        },
        {
          height: 8n,
          hash: "90019f795c3b49fb01f60fd89a3b82c54aa1f939d442bbd3c3ea8b425019fefa",
        },
      ],
      siblings: [
        "cd89e524959690a742777d6fcfee00b67f49680d7db88f11f0b50d544685e8c3",
        "b13af5471d8dd476547a5db0d45e60e9d7adfb8e3d92129b3b7173173480f5a2",
        "356b52f863d490ab79e3f11c9c09f2945dc137b25ee3b1098c78ebd48181753d",
        "6561a840018eec9cfb52705a20982d5f501ef5531c883ba21c9ba1f625a7ed81",
        "0337d34252a47edf2ff04623473692a90255a79662d460de9f8be66524249450",
        "b08429687e352fa2c76576b98d1a27fe8c4e6e28ed8eec75fe284dc5e843a054",
        "88a59d200f5edb186dd53d8d60cdbcb170f8a99774dfb2a198f48d5ede33dd35",
        "a6adec45f394267b46affeb0180a47a33ca2b7618304d98d61c7d71eaaf0b732",
      ],
      leafHash:
        "2b89672abc40b0ba8c2d4db8cd236b2ca6bda31ab8db93517f88e88dff114675",
      chunkHash:
        "ce90e7bd02c999f77ac8e586b0213dec629ebd0306ec4b73bc9481d0ad180de3",
      previousAccumulator:
        "2b65a8131ee963df765ab5b9e16cccc64a18933fad7d054ead3ef10608bb9483",
      nextAccumulator:
        "2938a180043b88852b2563a047d994a1edea85d8a18cb6833975094c16816cf8",
    });
  });

  it("binds deployment, header, length, owner, order, bytes, and every terminal accumulator", () => {
    const bytes = payload(80_000);
    const commitment = buildDaAvailabilityCommitment({
      deploymentIdentity: DEPLOYMENT,
      headerHash: HEADER,
      payload: bytes,
      responseGeometry: CANDIDATE_GEOMETRY,
    });
    assertCanonicalDaAvailabilityCommitment(commitment);
    expect(
      Data.from(
        Data.to(commitment, DaAvailabilityCommitment),
        DaAvailabilityCommitment,
      ),
    ).toEqual(commitment);
    expect(
      verifyDaAvailabilityPayloadCommitment({ commitment, payload: bytes }),
    ).toBe(true);

    for (const mutation of [
      { ...bytes, 0: bytes[0]! ^ 1 },
      bytes.subarray(1),
      Uint8Array.from([...bytes, 0]),
    ]) {
      expect(
        verifyDaAvailabilityPayloadCommitment({
          commitment,
          payload: Uint8Array.from(mutation),
        }),
      ).toBe(false);
    }

    const differentDeployment = {
      ...commitment,
      deployment_identity: "66".repeat(28),
    };
    expect(
      Buffer.from(
        daAvailabilityAttestationMessage(differentDeployment),
      ).toString("hex"),
    ).not.toBe(
      Buffer.from(daAvailabilityAttestationMessage(commitment)).toString("hex"),
    );
  });

  it("rejects reordered, gapped, overlong, or wrong-chunk commitments", () => {
    const commitment = build(4 * 1024 * 1024 + 1);
    const [first, second] = commitment.tranche_descriptors;
    expect(first).toBeDefined();
    expect(second).toBeDefined();

    for (const malformed of [
      { ...commitment, tranche_descriptors: [second!, first!] },
      {
        ...commitment,
        tranche_descriptors: [
          first!,
          { ...second!, start_offset: second!.start_offset + 1n },
        ],
      },
      {
        ...commitment,
        tranche_descriptors: [
          { ...first!, byte_length: first!.byte_length + 1n },
          second!,
        ],
      },
      {
        ...commitment,
        response_geometry: {
          ...commitment.response_geometry,
          tranche_byte_length:
            commitment.response_geometry.tranche_byte_length + 1n,
        },
      },
    ]) {
      expect(() =>
        assertCanonicalDaAvailabilityCommitment(malformed),
      ).toThrow();
    }
  });

  it("round-trips the challenge record and state-queue availability states", () => {
    const commitment = build(1024);
    const parameters = daAvailabilityParameters(parameterInput({}));
    const record: DaAvailabilityChallengeRecord = {
      commitment,
      challenge_asset_name: CHALLENGE_ASSET,
      challenger: OWNER,
      opened_at: 1_000n,
      response_deadline: daAvailabilityResponseDeadline({
        payloadByteLength: 1024,
        openedAt: 1_000n,
      }),
    };
    const recordCbor = encodeDaAvailabilityChallengeRecord(record, parameters);
    expect(recordCbor).toBe(Data.to(record, DaAvailabilityChallengeRecord));
    expect(
      parseDaAvailabilityChallengeRecordCbor(recordCbor, parameters),
    ).toEqual(record);
    expect(parseDaAvailabilityChallengeRecordCbor(recordCbor)).toEqual(record);

    for (const [malformed, reason] of [
      [
        { ...record, response_deadline: record.response_deadline + 1n },
        "exact canonical response deadline",
      ],
      [
        { ...record, response_deadline: record.response_deadline - 1n },
        "exact canonical response deadline",
      ],
      [
        { ...record, challenge_asset_name: "00".repeat(32) },
        "canonical 32-byte DACH identity",
      ],
      [
        { ...record, commitment: { ...commitment, version: 2n } },
        "version must be exactly V1",
      ],
    ] as const) {
      expect(() =>
        encodeDaAvailabilityChallengeRecord(malformed, parameters),
      ).toThrow(reason);
      expect(() =>
        parseDaAvailabilityChallengeRecordCbor(
          Data.to(malformed, DaAvailabilityChallengeRecord),
          parameters,
        ),
      ).toThrow(reason);
    }
    expect(() =>
      parseDaAvailabilityChallengeRecordCbor(recordCbor.toUpperCase()),
    ).toThrow("lowercase CBOR hex");
    expect(() =>
      parseDaAvailabilityChallengeRecordCbor(`${recordCbor}00`),
    ).toThrow();
    // A record whose commitment carries another response geometry than the
    // authenticated parameters is refused even though it is self-consistent.
    const otherGeometry = availabilityResponseGeometry({
      chunkByteLength: 8_000,
      trancheByteLength: 8 * 1024 * 1024,
      maxTrancheCount: 8,
    });
    const otherRecord = {
      ...record,
      commitment: buildDaAvailabilityCommitment({
        deploymentIdentity: DEPLOYMENT,
        headerHash: HEADER,
        payload: payload(1024),
        responseGeometry: otherGeometry,
      }),
    };
    expect(() =>
      parseDaAvailabilityChallengeRecordCbor(
        Data.to(otherRecord, DaAvailabilityChallengeRecord),
        parameters,
      ),
    ).toThrow("response geometry does not equal the authenticated");

    for (const status of [
      "Unattested",
      { Attested: { commitment_hash: COMMITMENT_HASH } },
      {
        Challenged: {
          commitment_hash: COMMITMENT_HASH,
          challenge_asset_name: CHALLENGE_ASSET,
        },
      },
      { Published: { terminal_commitment: "88".repeat(32) } },
    ] as const) {
      expect(
        Data.from(
          Data.to(status, DaAvailabilityStateQueueStatus),
          DaAvailabilityStateQueueStatus,
        ),
      ).toEqual(status);
    }
    // Aiken StateQueueStatusV1: Attested is constructor 1 with the one
    // commitment hash; Challenged is constructor 2, hash then challenge name.
    expect(
      Data.to(
        { Attested: { commitment_hash: COMMITMENT_HASH } },
        DaAvailabilityStateQueueStatus,
      ),
    ).toBe(Data.to(new Constr(1, [COMMITMENT_HASH])));
    expect(
      Data.to(
        {
          Challenged: {
            commitment_hash: COMMITMENT_HASH,
            challenge_asset_name: CHALLENGE_ASSET,
          },
        },
        DaAvailabilityStateQueueStatus,
      ),
    ).toBe(Data.to(new Constr(2, [COMMITMENT_HASH, CHALLENGE_ASSET])));
  });

  it("names each availability state by its commitment hash and challenge", () => {
    expect(daAvailabilityStateQueueStatusIdentity("Unattested")).toBe(
      "Unattested",
    );
    expect(
      daAvailabilityStateQueueStatusIdentity({
        Attested: { commitment_hash: COMMITMENT_HASH },
      }),
    ).toBe(`Attested:${COMMITMENT_HASH}`);
    expect(
      daAvailabilityStateQueueStatusIdentity({
        Challenged: {
          commitment_hash: COMMITMENT_HASH,
          challenge_asset_name: CHALLENGE_ASSET,
        },
      }),
    ).toBe(`Challenged:${COMMITMENT_HASH}:${CHALLENGE_ASSET}`);
    expect(
      daAvailabilityStateQueueStatusIdentity({
        Published: { terminal_commitment: "88".repeat(32) },
      }),
    ).toBe(`Published:${"88".repeat(32)}`);
  });

  it("matches the on-chain merge gate for every availability state", () => {
    expect(daAvailabilityStateQueueStatusPermitsMerge("Unattested")).toBe(
      false,
    );
    expect(
      daAvailabilityStateQueueStatusPermitsMerge({
        Attested: { commitment_hash: COMMITMENT_HASH },
      }),
    ).toBe(true);
    expect(
      daAvailabilityStateQueueStatusPermitsMerge({
        Challenged: {
          commitment_hash: COMMITMENT_HASH,
          challenge_asset_name: CHALLENGE_ASSET,
        },
      }),
    ).toBe(false);
    expect(
      daAvailabilityStateQueueStatusPermitsMerge({
        Published: { terminal_commitment: "88".repeat(32) },
      }),
    ).toBe(true);
  });

  it("derives unique bounded challenge, tranche, and published identities", () => {
    const challengeAsset = daAvailabilityChallengeAssetName(OUT_REF);
    const tranche0 = daAvailabilityTrancheAssetName({
      challengeAssetName: challengeAsset,
      trancheIndex: 0,
    });
    const tranche15 = daAvailabilityTrancheAssetName({
      challengeAssetName: challengeAsset,
      trancheIndex: 15,
    });
    expect(challengeAsset).toHaveLength(64);
    expect(tranche0).toHaveLength(64);
    expect(tranche15).toHaveLength(64);
    expect(new Set([challengeAsset, tranche0, tranche15]).size).toBe(3);
    expect(
      daAvailabilityChallengeAssetName({ ...OUT_REF, outputIndex: 8n }),
    ).not.toBe(challengeAsset);
    expect(
      daAvailabilityTrancheAssetName({
        challengeAssetName: challengeAsset,
        trancheIndex: 0,
      }),
    ).not.toBe(tranche15);
    expect(daAvailabilityPublishedTerminalCommitment(build(1024))).toMatch(
      /^[0-9a-f]{64}$/u,
    );
    expect(() =>
      daAvailabilityTrancheAssetName({
        challengeAssetName: "00".repeat(32),
        trancheIndex: 0,
      }),
    ).toThrow();
  });

  it.each([
    [0, "0000"],
    [1, "0100"],
    [15, "0f00"],
    [63, "3f00"],
  ] as const)(
    "matches the Aiken little-endian tranche asset vector for index %i",
    (trancheIndex, encodedIndex) => {
      const suffix = "22".repeat(28);
      expect(
        daAvailabilityTrancheAssetName({
          challengeAssetName: `44414348${suffix}`,
          trancheIndex,
        }),
      ).toBe(`4454${suffix}${encodedIndex}`);
    },
  );

  it("plans exact ordered publications and advances only through the deadline", () => {
    const geometry = availabilityResponseGeometry({
      chunkByteLength: 3,
      trancheByteLength: 64 * 1024,
      maxTrancheCount: 1024,
    });
    const bytes = payload(5);
    const commitment = buildDaAvailabilityCommitment({
      deploymentIdentity: DEPLOYMENT,
      headerHash: HEADER,
      payload: bytes,
      responseGeometry: geometry,
    });
    const challengeAssetName = daAvailabilityChallengeAssetName(OUT_REF);
    const [tranche] = planDaAvailabilityPublications({
      commitment,
      payload: bytes,
      challengeAssetName,
    });
    expect(tranche?.publications.map((item) => item.chunk_offset)).toEqual([
      0n,
      3n,
    ]);
    expect(tranche?.publications.map((item) => item.chunk_byte_length)).toEqual(
      [3n, 2n],
    );
    const deadline = daAvailabilityResponseDeadline({
      payloadByteLength: bytes.length,
      openedAt: 1_000n,
    });
    const active = {
      Active: {
        deployment_identity: DEPLOYMENT,
        header_hash: HEADER,
        challenge_asset_name: challengeAssetName,
        descriptor: commitment.tranche_descriptors[0]!,
        next_offset: 0n,
        accumulator: tranche!.initialAccumulator,
        latest_carrier_output_index: null,
        response_deadline: deadline,
        challenger: OWNER,
      },
    } as const;
    const first = advanceDaAvailabilityTranche({
      active,
      publication: tranche!.publications[0]!,
      responseGeometry: geometry,
      inclusiveValidityUpper: deadline,
      carrierOutputIndex: 1n,
    });
    expect(first).toHaveProperty("Active.next_offset", 3n);
    const receipt = advanceDaAvailabilityTranche({
      active: first,
      publication: tranche!.publications[1]!,
      responseGeometry: geometry,
      inclusiveValidityUpper: deadline,
      carrierOutputIndex: 1n,
    });
    expect(receipt).toHaveProperty(
      "Receipt.terminal_accumulator",
      commitment.tranche_descriptors[0]!.terminal_accumulator,
    );

    expect(() =>
      advanceDaAvailabilityTranche({
        active,
        publication: tranche!.publications[0]!,
        responseGeometry: geometry,
        inclusiveValidityUpper: deadline + 1n,
        carrierOutputIndex: 1n,
      }),
    ).toThrow("exceeds the response deadline");
    expect(() =>
      advanceDaAvailabilityTranche({
        active,
        publication: {
          ...tranche!.publications[0]!,
          chunk_offset: 1n,
        },
        responseGeometry: geometry,
        inclusiveValidityUpper: deadline,
        carrierOutputIndex: 1n,
      }),
    ).toThrow("not an index-bound member");
    expect(() =>
      advanceDaAvailabilityTranche({
        active: receipt,
        publication: tranche!.publications[1]!,
        responseGeometry: geometry,
        inclusiveValidityUpper: deadline,
        carrierOutputIndex: 1n,
      }),
    ).toThrow("terminal receipt");
  });

  it("uses one complete inline item through the measured fit boundary and chunks only above it", () => {
    const geometry = availabilityResponseGeometry({
      chunkByteLength: 4095,
      trancheByteLength: 64 * 1024,
      maxTrancheCount: 1024,
    });
    const challengeAssetName = daAvailabilityChallengeAssetName(OUT_REF);
    for (const [length, tier, publicationCount] of [
      [4095, "complete_item_inline", 1],
      [4096, "ordered_chunks", 2],
      [64 * 1024 + 1, "parallel_tranches", 18],
    ] as const) {
      const bytes = payload(length);
      const commitment = buildDaAvailabilityCommitment({
        deploymentIdentity: DEPLOYMENT,
        headerHash: HEADER,
        payload: bytes,
        responseGeometry: geometry,
      });
      const plan = planDaAvailabilityPublications({
        commitment,
        payload: bytes,
        challengeAssetName,
      });
      expect(
        daAvailabilityPublicationTier({
          payloadByteLength: length,
          responseGeometry: geometry,
        }),
      ).toBe(tier);
      expect(
        plan.reduce((count, tranche) => count + tranche.publications.length, 0),
      ).toBe(publicationCount);
      if (tier === "complete_item_inline") {
        expect(plan).toHaveLength(1);
        expect(plan[0]!.publications).toHaveLength(1);
        expect(plan[0]!.publications[0]!.chunk).toBe(
          Buffer.from(bytes).toString("hex"),
        );
      }
    }
  });

  it("closes only an exact ordered receipt set", () => {
    const geometry = availabilityResponseGeometry({
      chunkByteLength: 4095,
      trancheByteLength: 64 * 1024,
      maxTrancheCount: 1024,
    });
    const commitment = buildDaAvailabilityCommitment({
      deploymentIdentity: DEPLOYMENT,
      headerHash: HEADER,
      payload: payload(70 * 1024),
      responseGeometry: geometry,
    });
    const challengeAssetName = daAvailabilityChallengeAssetName(OUT_REF);
    const receipts = commitment.tranche_descriptors.map((descriptor) => ({
      Receipt: {
        deployment_identity: DEPLOYMENT,
        header_hash: HEADER,
        challenge_asset_name: challengeAssetName,
        descriptor,
        terminal_accumulator: descriptor.terminal_accumulator,
        terminal_carrier_output_index: 1n,
        challenger: OWNER,
      },
    }));
    expect(
      assertDaAvailabilityTerminalReceipts({
        commitment,
        challengeAssetName,
        challenger: OWNER,
        receipts,
      }),
    ).toBe(daAvailabilityPublishedTerminalCommitment(commitment));
    expect(() =>
      assertDaAvailabilityTerminalReceipts({
        commitment,
        challengeAssetName,
        challenger: OWNER,
        receipts: [receipts[1]!, receipts[0]!],
      }),
    ).toThrow("does not equal its signed descriptor");
    expect(() =>
      assertDaAvailabilityTerminalReceipts({
        commitment,
        challengeAssetName,
        challenger: OWNER,
        receipts: [receipts[0]!, receipts[0]!],
      }),
    ).toThrow("does not equal its signed descriptor");
    expect(() =>
      assertDaAvailabilityTerminalReceipts({
        commitment,
        challengeAssetName,
        challenger: OWNER,
        receipts: [
          {
            Receipt: {
              ...receipts[0]!.Receipt,
              terminal_accumulator: "00".repeat(32),
            },
          },
          receipts[1]!,
        ],
      }),
    ).toThrow("does not equal its signed descriptor");
  });

  it("reconstructs exact public L1 history and rejects missing or reordered chunks", () => {
    const geometry = availabilityResponseGeometry({
      chunkByteLength: 4095,
      trancheByteLength: 64 * 1024,
      maxTrancheCount: 1024,
    });
    const bytes = payload(70 * 1024);
    const commitment = buildDaAvailabilityCommitment({
      deploymentIdentity: DEPLOYMENT,
      headerHash: HEADER,
      payload: bytes,
      responseGeometry: geometry,
    });
    const challengeAssetName = daAvailabilityChallengeAssetName(OUT_REF);
    const openedAt = 1_000n;
    const responseDeadline = daAvailabilityResponseDeadline({
      payloadByteLength: bytes.length,
      openedAt,
    });
    const parameters = daAvailabilityParameters({
      responseGeometry: geometry,
      ...DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
      challengerBondLovelace:
        DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE,
      maxOpenFeeLovelace: MAX_OPEN_FEE,
      maxPublicationFeeLovelace: MAX_PUBLICATION_FEE,
      maxSettlementFeeLovelace: MAX_SETTLEMENT_FEE,
      maxCloseFeeLovelace: MAX_CLOSE_FEE,
      maxTimeoutFeeLovelace: MAX_TIMEOUT_FEE,
    });
    const record = {
      commitment,
      challenge_asset_name: challengeAssetName,
      challenger: OWNER,
      opened_at: openedAt,
      response_deadline: responseDeadline,
    } as const;
    const recordEvidence = {
      datumCborHex: encodeDaAvailabilityChallengeRecord(record, parameters),
      challengerFundingOutRef: OUT_REF,
      recordOutputOutRef: {
        transactionId: "98".repeat(32),
        outputIndex: 1n,
      },
    } as const;
    const plan = planDaAvailabilityPublications({
      commitment,
      payload: bytes,
      challengeAssetName,
    });
    const tranches = plan.map((item) => ({
      descriptor: item.descriptor,
      publications: item.publications.map((publication, publicationIndex) => ({
        publication,
        inclusiveValidityUpper: responseDeadline,
        carrierOutputIndex: BigInt(publicationIndex + 1),
      })),
    }));
    expect(
      reconstructDaAvailabilityPayload({
        challengeRecord: recordEvidence,
        parameters,
        tranches,
      }),
    ).toEqual(bytes);

    const missing = tranches.map((item, index) =>
      index === 0
        ? { ...item, publications: item.publications.slice(1) }
        : item,
    );
    expect(() =>
      reconstructDaAvailabilityPayload({
        challengeRecord: recordEvidence,
        parameters,
        tranches: missing,
      }),
    ).toThrow();

    const reordered = tranches.map((item, index) =>
      index === 0
        ? {
            ...item,
            publications: [
              item.publications[1]!,
              item.publications[0]!,
              ...item.publications.slice(2),
            ],
          }
        : item,
    );
    expect(() =>
      reconstructDaAvailabilityPayload({
        challengeRecord: recordEvidence,
        parameters,
        tranches: reordered,
      }),
    ).toThrow();

    const forgedLaterDeadlineCbor = Data.to(
      { ...record, response_deadline: responseDeadline + 1n },
      DaAvailabilityChallengeRecord,
    );
    expect(() =>
      reconstructDaAvailabilityPayload({
        challengeRecord: {
          ...recordEvidence,
          datumCborHex: forgedLaterDeadlineCbor,
        },
        parameters,
        tranches,
      }),
    ).toThrow("exact canonical response deadline");
    expect(() =>
      reconstructDaAvailabilityPayload({
        challengeRecord: {
          ...recordEvidence,
          challengerFundingOutRef: {
            ...OUT_REF,
            outputIndex: OUT_REF.outputIndex + 1n,
          },
        },
        parameters,
        tranches,
      }),
    ).toThrow(
      "DACH identity derived from its consumed challenger funding input",
    );
    expect(() =>
      reconstructDaAvailabilityPayload({
        challengeRecord: {
          ...recordEvidence,
          recordOutputOutRef: OUT_REF,
        },
        parameters,
        tranches,
      }),
    ).toThrow("cannot equal its consumed challenger funding input");
    expect(() =>
      reconstructDaAvailabilityPayload({
        challengeRecord: {
          ...recordEvidence,
          datumCborHex: Data.to(
            { Attested: { commitment_hash: COMMITMENT_HASH } },
            DaAvailabilityStateQueueStatus,
          ),
        },
        parameters,
        tranches,
      }),
    ).toThrow("not valid V1 Plutus Data");
    const evidenceWithExtraField = {
      ...recordEvidence,
      responseDeadline,
    };
    expect(() =>
      reconstructDaAvailabilityPayload({
        challengeRecord: evidenceWithExtraField,
        parameters,
        tranches,
      }),
    ).toThrow("must contain exactly datum and input/output identities");

    // The response planner reads identity and commitment from the same
    // authenticated record and yields exactly the direct plan.
    expect(
      planDaAvailabilityPublicationsFromChallengeRecord({
        challengeRecord: recordEvidence,
        parameters,
        payload: bytes,
      }),
    ).toEqual(plan);
    expect(() =>
      planDaAvailabilityPublicationsFromChallengeRecord({
        challengeRecord: {
          ...recordEvidence,
          challengerFundingOutRef: {
            ...OUT_REF,
            outputIndex: OUT_REF.outputIndex + 1n,
          },
        },
        parameters,
        payload: bytes,
      }),
    ).toThrow(
      "DACH identity derived from its consumed challenger funding input",
    );
  });

  it("fails closed outside canonical payload and chunk bounds", () => {
    expect(() =>
      deriveDaAvailabilityTrancheLayout(0, CANDIDATE_GEOMETRY),
    ).toThrow();
    expect(() =>
      deriveDaAvailabilityTrancheLayout(
        64 * 1024 * 1024 + 1,
        CANDIDATE_GEOMETRY,
      ),
    ).toThrow();
    expect(() =>
      availabilityResponseGeometry({
        ...DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
        chunkByteLength: 16_000,
      }),
    ).toThrow();
  });

  it("pins the cross-language commitment and asset-identity vectors", () => {
    // Expected values come from the #688 golden generator's own Plutus Data
    // encoder (scripts/da-vector-support.mjs), not from the SDK codec.
    const commitment = build(1024);
    expect({
      attestationMessage: Buffer.from(
        daAvailabilityAttestationMessage(commitment),
      ).toString("hex"),
      commitmentHash: daAvailabilityCommitmentHash(commitment),
      terminalAccumulator:
        commitment.tranche_descriptors[0]?.terminal_accumulator,
      publishedTerminal: daAvailabilityPublishedTerminalCommitment(commitment),
      challengeAsset: daAvailabilityChallengeAssetName(OUT_REF),
    }).toEqual({
      attestationMessage:
        "e112ac3bc387661c6e1f71714cc985628b3cf209e07b9fe157e2b5d4c2eeb04f",
      commitmentHash:
        "8fc6cf29efeaea044def3eb5ba3781e15b4b242917001148a3b3cc544f643574",
      terminalAccumulator:
        "966596e9655de8409b0ae66e3565b4e0720249359fc047d5acce2e79d3f21b58",
      publishedTerminal:
        "c1409a562423644cafaf3fdc58e3112e37105b2643f72079ca2102f90364632f",
      challengeAsset:
        "444143484acf0e543a1ed6e9d85df692e5b23859fa3ba1d807b1f44361f5fe9f",
    });
  });

  it("pins the ParametersV1 field order against the on-chain vector", () => {
    // onchain/aiken/lib/midgard/availability-challenge.test.ak serialises the
    // same record to these bytes. Every field carries a different value, so a
    // reordered schema field changes the bytes.
    const cbor = "d8799fd8799f010203ff0405060708090a0b0c0d0eff";
    const parameters: DaAvailabilityParameters = {
      response_geometry: {
        chunk_byte_length: 1n,
        tranche_byte_length: 2n,
        max_tranche_count: 3n,
      },
      da_bond_lovelace: 4n,
      challenger_bond_lovelace: 5n,
      max_open_fee_lovelace: 6n,
      max_publication_fee_lovelace: 7n,
      max_settlement_fee_lovelace: 8n,
      max_close_fee_lovelace: 9n,
      max_timeout_fee_lovelace: 10n,
      da_slash_penalty_lovelace: 11n,
      da_bond_min_top_up_lovelace: 12n,
      da_bond_pool_floor_lovelace: 13n,
      challenge_record_lovelace: 14n,
    };
    expect(Data.to(parameters, DaAvailabilityParameters)).toBe(cbor);
    expect(Data.from(cbor, DaAvailabilityParameters)).toEqual(parameters);
  });

  it.each(COMMITMENT_GOLDENS.vectors.map((vector) => [vector.label, vector]))(
    "reproduces the #688 CommitmentV1 / ChallengeRecordV1 golden %s",
    (_label, vector) => {
      const commitment = commitmentFromGolden(vector.commitment);
      expect(encodeDaAvailabilityCommitment(commitment)).toBe(
        vector.commitmentCborHex,
      );
      expect(
        parseDaAvailabilityCommitmentCbor(vector.commitmentCborHex),
      ).toEqual(commitment);
      expect(daAvailabilityCommitmentHash(commitment)).toBe(
        vector.commitmentHashHex,
      );
      expect(
        Buffer.from(daAvailabilityAttestationMessage(commitment)).toString(
          "hex",
        ),
      ).toBe(vector.attestationMessageHex);
      const record: DaAvailabilityChallengeRecord = {
        commitment,
        challenge_asset_name: vector.challengeRecord.challengeAssetName,
        challenger: vector.challengeRecord.challenger,
        opened_at: BigInt(vector.challengeRecord.openedAt),
        response_deadline: BigInt(vector.challengeRecord.responseDeadline),
      };
      expect(Data.to(record, DaAvailabilityChallengeRecord)).toBe(
        vector.challengeRecordCborHex,
      );
      expect(
        Data.from(vector.challengeRecordCborHex, DaAvailabilityChallengeRecord),
      ).toEqual(record);
      expect(
        daAvailabilityChallengeAssetName({
          transactionId: vector.outputReference.transactionId,
          outputIndex: BigInt(vector.outputReference.outputIndex),
        }),
      ).toBe(vector.challengeAssetNameHex);
    },
  );

  it("strictly decodes release parameters and signed commitments for durable handoff", () => {
    const parameters = daAvailabilityParameters({
      responseGeometry: CANDIDATE_GEOMETRY,
      ...DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
      challengerBondLovelace: 12_000_000_000n,
      maxOpenFeeLovelace: MAX_OPEN_FEE,
      maxPublicationFeeLovelace: MAX_PUBLICATION_FEE,
      maxSettlementFeeLovelace: MAX_SETTLEMENT_FEE,
      maxCloseFeeLovelace: MAX_CLOSE_FEE,
      maxTimeoutFeeLovelace: MAX_TIMEOUT_FEE,
    });
    const parametersCbor = encodeDaAvailabilityParameters(parameters);
    expect(parseDaAvailabilityParametersCbor(parametersCbor)).toEqual(
      parameters,
    );
    expect(() =>
      parseDaAvailabilityParametersCbor(parametersCbor.toUpperCase()),
    ).toThrow("lowercase CBOR hex");

    const unequalBonds = {
      ...parameters,
      challenger_bond_lovelace: parameters.challenger_bond_lovelace - 1n,
    };
    expect(
      parseDaAvailabilityParametersCbor(
        Data.to(unequalBonds, DaAvailabilityParameters),
      ),
    ).toEqual(unequalBonds);
    const penaltyAtBondCbor = Data.to(
      {
        ...parameters,
        da_slash_penalty_lovelace: parameters.da_bond_lovelace,
      },
      DaAvailabilityParameters,
    );
    expect(() => parseDaAvailabilityParametersCbor(penaltyAtBondCbor)).toThrow(
      "a slash penalty strictly between zero and it",
    );

    const commitment = build(70 * 1024);
    const commitmentCbor = encodeDaAvailabilityCommitment(commitment);
    expect(
      parseDaAvailabilityCommitmentCbor(commitmentCbor, CANDIDATE_GEOMETRY),
    ).toEqual(commitment);
    const alternateGeometry = availabilityResponseGeometry({
      chunkByteLength: 8_000,
      trancheByteLength: 8 * 1024 * 1024,
      maxTrancheCount: 8,
    });
    expect(() =>
      parseDaAvailabilityCommitmentCbor(commitmentCbor, alternateGeometry),
    ).toThrow("does not equal the authenticated deployment/DA parameters");
  });

  it("strictly binds inline publication datums to the signed tranche descriptor", () => {
    const geometry = availabilityResponseGeometry({
      chunkByteLength: 3,
      trancheByteLength: 64 * 1024,
      maxTrancheCount: 1024,
    });
    const bytes = payload(5);
    const commitment = buildDaAvailabilityCommitment({
      deploymentIdentity: DEPLOYMENT,
      headerHash: HEADER,
      payload: bytes,
      responseGeometry: geometry,
    });
    const [tranche] = planDaAvailabilityPublications({
      commitment,
      payload: bytes,
      challengeAssetName: daAvailabilityChallengeAssetName(OUT_REF),
    });
    const publication = tranche!.publications[0]!;
    const publicationCbor = encodeDaAvailabilityPublicationDatum(
      publication,
      geometry,
      tranche!.descriptor,
    );
    expect(
      parseDaAvailabilityPublicationDatumCbor(
        publicationCbor,
        geometry,
        tranche!.descriptor,
      ),
    ).toEqual(publication);

    const oversizedGeometry = availabilityResponseGeometry({
      chunkByteLength: 2,
      trancheByteLength: 64 * 1024,
      maxTrancheCount: 1024,
    });
    expect(() =>
      parseDaAvailabilityPublicationDatumCbor(
        publicationCbor,
        oversizedGeometry,
        tranche!.descriptor,
      ),
    ).toThrow("exceeds the authenticated response geometry");

    for (const malformed of [
      { ...publication, chunk_hash: "00".repeat(32) },
      { ...publication, chunk_byte_length: publication.chunk_byte_length + 1n },
      { ...publication, next_accumulator: "00".repeat(32) },
      { ...publication, challenge_asset_name: "00".repeat(32) },
    ]) {
      expect(() =>
        parseDaAvailabilityPublicationDatumCbor(
          Data.to(malformed, DaAvailabilityPublicationDatum),
          geometry,
          tranche!.descriptor,
        ),
      ).toThrow();
    }

    const foreignPayload = Uint8Array.from(bytes, (value) => value ^ 0xff);
    const foreignCommitment = buildDaAvailabilityCommitment({
      deploymentIdentity: DEPLOYMENT,
      headerHash: HEADER,
      payload: foreignPayload,
      responseGeometry: geometry,
    });
    const [foreignTranche] = planDaAvailabilityPublications({
      commitment: foreignCommitment,
      payload: foreignPayload,
      challengeAssetName: daAvailabilityChallengeAssetName(OUT_REF),
    });
    const foreignPublication = foreignTranche!.publications[0]!;
    const foreignCbor = Data.to(
      foreignPublication,
      DaAvailabilityPublicationDatum,
    );
    expect(() =>
      parseDaAvailabilityPublicationDatumCbor(
        foreignCbor,
        geometry,
        tranche!.descriptor,
      ),
    ).toThrow("does not equal the signed tranche descriptor");
  });

  it("derives exact challenge datums and deterministic tranche funding", () => {
    const geometry = availabilityResponseGeometry({
      chunkByteLength: 4095,
      trancheByteLength: 64 * 1024,
      maxTrancheCount: 1024,
    });
    const parameters = daAvailabilityParameters({
      responseGeometry: geometry,
      ...DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
      challengerBondLovelace: 10_000_000_000n,
      maxOpenFeeLovelace: MAX_OPEN_FEE,
      maxPublicationFeeLovelace: MAX_PUBLICATION_FEE,
      maxSettlementFeeLovelace: MAX_SETTLEMENT_FEE,
      maxCloseFeeLovelace: MAX_CLOSE_FEE,
      maxTimeoutFeeLovelace: MAX_TIMEOUT_FEE,
    });
    const commitment = buildDaAvailabilityCommitment({
      deploymentIdentity: DEPLOYMENT,
      headerHash: HEADER,
      payload: payload(70 * 1024),
      responseGeometry: geometry,
    });
    const challengerFundingOutRef = { ...OUT_REF, outputIndex: 8n };
    const plan = buildDaAvailabilityChallengeDatumPlan({
      commitment,
      challengerFundingOutRef,
      challenger: OWNER,
      openedAt: 1_000n,
      parameters,
    });
    expect(plan.challengeAssetName).toBe(
      daAvailabilityChallengeAssetName(challengerFundingOutRef),
    );
    expect(plan.responseDeadline).toBe(
      1_000n + BigInt(DA_AVAILABILITY_FULL_RESPONSE_WINDOW_MS),
    );
    expect(plan.trancheThreads).toHaveLength(2);
    expect(plan.trancheFunding).toEqual([
      {
        trancheIndex: 0,
        initialLovelace: 5_003_150_000n,
        maximumPublicationFeeReserveLovelace: 8_500_000n,
        maximumSettlementFeeReserveLovelace: 500_000n,
      },
      {
        trancheIndex: 1,
        initialLovelace: 4_995_650_000n,
        maximumPublicationFeeReserveLovelace: 1_000_000n,
        maximumSettlementFeeReserveLovelace: 500_000n,
      },
    ]);
    expect(plan.terminalAccumulatorFundingLovelace).toBe(1_200_000n);
    expect(plan.terminalAccumulator).toEqual({
      deployment_identity: DEPLOYMENT,
      header_hash: HEADER,
      challenge_asset_name: plan.challengeAssetName,
      next_tranche_index: 0n,
      folded_terminal_accumulator: daAvailabilityTerminalAccumulatorStart({
        deploymentIdentity: DEPLOYMENT,
        headerHash: HEADER,
        challengeAssetName: plan.challengeAssetName,
      }),
      has_timed_out_tranche: false,
      response_deadline: plan.responseDeadline,
      challenger: OWNER,
      remaining_challenger_lovelace: 1_200_000n,
    });
    expect(
      plan.trancheThreads.map((thread) =>
        "Active" in thread ? thread.Active.next_offset : null,
      ),
    ).toEqual([0n, 64n * 1024n]);
    expect(plan.record).toEqual({
      commitment,
      challenge_asset_name: plan.challengeAssetName,
      challenger: OWNER,
      opened_at: 1_000n,
      response_deadline: plan.responseDeadline,
    });
    expect(plan.recordLovelace).toBe(parameters.challenge_record_lovelace);
    expect(
      parseDaAvailabilityChallengeRecordCbor(
        encodeDaAvailabilityChallengeRecord(plan.record, parameters),
        parameters,
      ),
    ).toEqual(plan.record);
    expect(
      plan.trancheThreads.map((thread) =>
        parseDaAvailabilityTrancheDatumCbor(
          encodeDaAvailabilityTrancheDatum(thread),
        ),
      ),
    ).toEqual(plan.trancheThreads);
    expect(
      parseDaAvailabilityTerminalAccumulatorDatumCbor(
        encodeDaAvailabilityTerminalAccumulatorDatum(plan.terminalAccumulator),
      ),
    ).toEqual(plan.terminalAccumulator);

    // Only a canonical commitment under the authenticated geometry opens.
    expect(() =>
      buildDaAvailabilityChallengeDatumPlan({
        commitment: build(1024),
        challengerFundingOutRef,
        challenger: OWNER,
        openedAt: 1_000n,
        parameters,
      }),
    ).toThrow("response geometry does not equal the authenticated");
    expect(() =>
      buildDaAvailabilityChallengeDatumPlan({
        commitment,
        challengerFundingOutRef,
        challenger: OWNER,
        openedAt: -1n,
        parameters,
      }),
    ).toThrow("openedAt must be non-negative");
    expect(() =>
      buildDaAvailabilityChallengeDatumPlan({
        commitment,
        challengerFundingOutRef,
        challenger: "33".repeat(27),
        openedAt: 1_000n,
        parameters,
      }),
    ).toThrow("challenger must be exactly 28");

    expect(() =>
      parseDaAvailabilityChallengeRecordCbor(
        Data.to(
          { ...plan.record, response_deadline: plan.responseDeadline + 1n },
          DaAvailabilityChallengeRecord,
        ),
        parameters,
      ),
    ).toThrow("exact canonical response deadline");
  });

  it("conserves isolated tranche/carrier value and attributes each fee exactly once", () => {
    const geometry = availabilityResponseGeometry({
      chunkByteLength: 4095,
      trancheByteLength: 64 * 1024,
      maxTrancheCount: 1024,
    });
    const parameters = daAvailabilityParameters({
      responseGeometry: geometry,
      ...DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
      challengerBondLovelace: 10_000_000_000n,
      maxOpenFeeLovelace: MAX_OPEN_FEE,
      maxPublicationFeeLovelace: MAX_PUBLICATION_FEE,
      maxSettlementFeeLovelace: MAX_SETTLEMENT_FEE,
      maxCloseFeeLovelace: MAX_CLOSE_FEE,
      maxTimeoutFeeLovelace: MAX_TIMEOUT_FEE,
    });
    const firstThread = planDaAvailabilityPublicationValueTransition({
      threadInputLovelace: 5_004_350_000n,
      previousCarrierInputLovelace: 0n,
      nextCarrierOutputLovelace: 2_000_000n,
      transactionFeeLovelace: 400_000n,
      minimumThreadOutputLovelace: 1_000_000n,
      isFirstPublication: true,
      parameters,
    });
    expect(firstThread).toBe(5_001_950_000n);
    assertDaAvailabilityChallengerBondConservation({
      initialChallengerBondLovelace: 10_000_000_000n,
      currentThreadLovelace: [firstThread, 4_995_650_000n],
      currentCarrierLovelace: [2_000_000n],
      paidTransactionFeesLovelace: [400_000n],
    });

    const secondThread = planDaAvailabilityPublicationValueTransition({
      threadInputLovelace: firstThread,
      previousCarrierInputLovelace: 2_000_000n,
      nextCarrierOutputLovelace: 1_800_000n,
      transactionFeeLovelace: 450_000n,
      minimumThreadOutputLovelace: 1_000_000n,
      isFirstPublication: false,
      parameters,
    });
    expect(secondThread).toBe(5_001_700_000n);
    assertDaAvailabilityChallengerBondConservation({
      initialChallengerBondLovelace: 10_000_000_000n,
      currentThreadLovelace: [secondThread, 4_995_650_000n],
      currentCarrierLovelace: [1_800_000n],
      paidTransactionFeesLovelace: [400_000n, 450_000n],
    });

    const refunds = planDaAvailabilityTerminalRefund({
      kind: "close",
      tranches: [
        {
          trancheIndex: 0,
          threadLovelace: secondThread,
          carrierLovelace: 1_800_000n,
        },
        {
          trancheIndex: 1,
          threadLovelace: 4_995_650_000n,
          carrierLovelace: 0n,
        },
      ],
      transactionFeeLovelace: 900_000n,
      parameters,
    });
    expect(refunds).toEqual([
      {
        trancheIndex: 0,
        refundLovelace: 5_002_600_000n,
        attributedTransactionFeeLovelace: 900_000n,
      },
      {
        trancheIndex: 1,
        refundLovelace: 4_995_650_000n,
        attributedTransactionFeeLovelace: 0n,
      },
    ]);
    expect(
      refunds.reduce((total, refund) => total + refund.refundLovelace, 0n),
    ).toBe(10_000_000_000n - 400_000n - 450_000n - 900_000n);

    expect(() =>
      planDaAvailabilityPublicationValueTransition({
        threadInputLovelace: 5_004_350_000n,
        previousCarrierInputLovelace: 0n,
        nextCarrierOutputLovelace: 2_000_000n,
        transactionFeeLovelace: MAX_PUBLICATION_FEE + 1n,
        minimumThreadOutputLovelace: 1_000_000n,
        isFirstPublication: true,
        parameters,
      }),
    ).toThrow("fee above its authenticated ceiling");
    expect(() =>
      planDaAvailabilityPublicationValueTransition({
        threadInputLovelace: 5_004_350_000n,
        previousCarrierInputLovelace: 0n,
        nextCarrierOutputLovelace: 5_003_000_000n,
        transactionFeeLovelace: 400_000n,
        minimumThreadOutputLovelace: 1_000_000n,
        isFirstPublication: true,
        parameters,
      }),
    ).toThrow("consume the protected tranche working floor");
    expect(() =>
      assertDaAvailabilityChallengerBondConservation({
        initialChallengerBondLovelace: 10_000_000_000n,
        currentThreadLovelace: [firstThread, 4_995_650_000n],
        currentCarrierLovelace: [2_000_000n],
        paidTransactionFeesLovelace: [400_000n, 400_000n],
      }),
    ).toThrow("not isolated and exactly conserved");
    expect(() =>
      planDaAvailabilityTerminalRefund({
        kind: "close",
        tranches: [
          {
            trancheIndex: 1,
            threadLovelace: secondThread,
            carrierLovelace: 1_800_000n,
          },
          {
            trancheIndex: 0,
            threadLovelace: 4_995_650_000n,
            carrierLovelace: 0n,
          },
        ],
        transactionFeeLovelace: 900_000n,
        parameters,
      }),
    ).toThrow("noncanonical protected value");
    expect(() =>
      planDaAvailabilityTerminalRefund({
        kind: "timeout",
        tranches: [
          {
            trancheIndex: 0,
            threadLovelace: secondThread,
            carrierLovelace: 1_800_000n,
          },
          {
            trancheIndex: 1,
            threadLovelace: 4_995_650_000n,
            carrierLovelace: 0n,
          },
        ],
        transactionFeeLovelace: MAX_TIMEOUT_FEE + 1n,
        parameters,
      }),
    ).toThrow("fee above its authenticated ceiling");
  });

  it("folds published and timed-out tranches into the one canonical terminal accumulator", () => {
    const parameters = daAvailabilityParameters({
      responseGeometry: CANDIDATE_GEOMETRY,
      ...DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
      challengerBondLovelace: 10_000_000_000n,
      maxOpenFeeLovelace: MAX_OPEN_FEE,
      maxPublicationFeeLovelace: MAX_PUBLICATION_FEE,
      maxSettlementFeeLovelace: MAX_SETTLEMENT_FEE,
      maxCloseFeeLovelace: MAX_CLOSE_FEE,
      maxTimeoutFeeLovelace: MAX_TIMEOUT_FEE,
    });
    const bytes = payload(16_000);
    const commitment = buildDaAvailabilityCommitment({
      deploymentIdentity: DEPLOYMENT,
      headerHash: HEADER,
      payload: bytes,
      responseGeometry: CANDIDATE_GEOMETRY,
    });
    const challenge = buildDaAvailabilityChallengeDatumPlan({
      commitment,
      challengerFundingOutRef: OUT_REF,
      challenger: OWNER,
      openedAt: 1_000n,
      parameters,
    });
    const [tranchePlan] = planDaAvailabilityPublications({
      commitment,
      payload: bytes,
      challengeAssetName: challenge.challengeAssetName,
    });
    if (
      tranchePlan === undefined ||
      challenge.trancheThreads[0] === undefined
    ) {
      throw new Error("missing settlement fixture tranche");
    }
    let receipt = challenge.trancheThreads[0];
    for (const publication of tranchePlan.publications) {
      receipt = advanceDaAvailabilityTranche({
        active: receipt,
        publication,
        responseGeometry: CANDIDATE_GEOMETRY,
        inclusiveValidityUpper: challenge.responseDeadline,
        carrierOutputIndex: 1n,
      });
    }
    const published = planDaAvailabilitySettlement({
      commitment,
      terminalAccumulator: challenge.terminalAccumulator,
      tranche: receipt,
      threadLovelace: challenge.trancheFunding[0]!.initialLovelace - 500_000n,
      carrierLovelace: 2_000_000n,
      transactionFeeLovelace: 400_000n,
      inclusiveValidityLower: 1_001n,
      parameters,
    });
    expect(published.status).toEqual({
      PublishedTranche: {
        terminal_accumulator: tranchePlan.descriptor.terminal_accumulator,
      },
    });
    expect(published.nextTerminalAccumulator).toMatchObject({
      next_tranche_index: 1n,
      has_timed_out_tranche: false,
      remaining_challenger_lovelace: published.nextTerminalLovelace,
    });
    expect(
      published.nextTerminalAccumulator.folded_terminal_accumulator,
    ).toMatch(/^[0-9a-f]{64}$/u);

    const timedOut = planDaAvailabilitySettlement({
      commitment,
      terminalAccumulator: challenge.terminalAccumulator,
      tranche: challenge.trancheThreads[0]!,
      threadLovelace: challenge.trancheFunding[0]!.initialLovelace,
      carrierLovelace: 0n,
      transactionFeeLovelace: 400_000n,
      inclusiveValidityLower: challenge.responseDeadline,
      parameters,
    });
    expect(timedOut.status).toMatchObject({
      TimedOutTranche: {
        next_offset: tranchePlan.descriptor.start_offset,
      },
    });
    expect(timedOut.nextTerminalAccumulator.has_timed_out_tranche).toBe(true);
    expect(() =>
      planDaAvailabilitySettlement({
        commitment,
        terminalAccumulator: challenge.terminalAccumulator,
        tranche: challenge.trancheThreads[0]!,
        threadLovelace: challenge.trancheFunding[0]!.initialLovelace,
        carrierLovelace: 0n,
        transactionFeeLovelace: 400_000n,
        inclusiveValidityLower: challenge.responseDeadline - 1n,
        parameters,
      }),
    ).toThrow("authenticated deadline");
  });
});
