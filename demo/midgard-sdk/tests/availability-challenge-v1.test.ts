import { CML, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  advanceDaAvailabilityTrancheV1,
  assertCanonicalDaAvailabilityCommitmentV1,
  assertDaAvailabilityChallengerBondConservationV1,
  assertDaAvailabilityTerminalReceiptsV1,
  availabilityResponseGeometryV1,
  buildDaAvailabilityChallengeDatumPlanV1,
  buildDaAvailabilityCommitmentV1,
  DA_AVAILABILITY_BOND_LOVELACE_MEASUREMENT_CANDIDATE_V1,
  DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE_V1,
  DA_AVAILABILITY_FULL_RESPONSE_WINDOW_MS_V1,
  DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE_V1,
  DA_AVAILABILITY_SMALL_RESPONSE_WINDOW_MS_V1,
  daAvailabilityAttestationMessageV1,
  daAvailabilityBondAssetNameV1,
  DaAvailabilityBondDatumV1,
  daAvailabilityChallengeAssetNameV1,
  daAvailabilityChunkLeafHashV1,
  DaAvailabilityCommitmentV1,
  DaAvailabilityParametersV1,
  daAvailabilityParametersV1,
  DaAvailabilityPublicationDatumV1,
  daAvailabilityPublicationTierV1,
  daAvailabilityPublishedTerminalCommitmentV1,
  daAvailabilityResponseDeadlineV1,
  daAvailabilityResponseWindowMsV1,
  DaAvailabilitySpendRedeemerV1,
  daAvailabilityStateQueueStatusPermitsMergeV1,
  DaAvailabilityStateQueueStatusV1,
  daAvailabilityTerminalAccumulatorStartV1,
  daAvailabilityTrancheAssetNameV1,
  deriveDaAvailabilityTrancheLayoutV1,
  encodeDaAvailabilityBondDatumV1,
  encodeDaAvailabilityCommitmentV1,
  encodeDaAvailabilityParametersV1,
  encodeDaAvailabilityPublicationDatumV1,
  encodeDaAvailabilityTerminalAccumulatorDatumV1,
  encodeDaAvailabilityTrancheDatumV1,
  parseDaAvailabilityBondDatumV1Cbor,
  parseDaAvailabilityCommitmentV1Cbor,
  parseDaAvailabilityParametersV1Cbor,
  parseDaAvailabilityPublicationDatumV1Cbor,
  parseDaAvailabilityTerminalAccumulatorDatumV1Cbor,
  parseDaAvailabilityTrancheDatumV1Cbor,
  planDaAvailabilityPublicationsV1,
  planDaAvailabilityPublicationValueTransitionV1,
  planDaAvailabilitySettlementV1,
  planDaAvailabilityTerminalRefundV1,
  reconstructDaAvailabilityPayloadV1,
  verifyDaAvailabilityPayloadCommitmentV1,
} from "../src/availability-challenge-v1.js";

const DEPLOYMENT = "11".repeat(28);
const HEADER = "22".repeat(28);
const OWNER = "33".repeat(28);
const COMMITTEE_HASH = "44".repeat(32);
const SIGNERS = "80" + "00".repeat(31);
const OUT_REF = { transactionId: "99".repeat(32), outputIndex: 7n };
const BOND_ASSET = daAvailabilityBondAssetNameV1(OUT_REF);
const MAX_OPEN_FEE = 500_000n;
const MAX_PUBLICATION_FEE = 500_000n;
const MAX_SETTLEMENT_FEE = 500_000n;
const MAX_CLOSE_FEE = 1_000_000n;
const MAX_TIMEOUT_FEE = 1_200_000n;

const CANDIDATE_GEOMETRY = availabilityResponseGeometryV1(
  DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE_V1,
);

const payload = (length: number): Uint8Array =>
  Uint8Array.from({ length }, (_, index) => (index * 17 + 3) % 256);

const publicationTransactionBytes = (chunkByteLength: number): number => {
  const geometry = availabilityResponseGeometryV1({
    chunkByteLength,
    trancheByteLength: 4 * 1024 * 1024,
    maxTrancheCount: 16,
  });
  const bytes = payload(4 * 1024 * 1024);
  const commitment = buildDaAvailabilityCommitmentV1({
    deploymentIdentity: DEPLOYMENT,
    headerHash: HEADER,
    payload: bytes,
    bondOwner: OWNER,
    responseGeometry: geometry,
  });
  const challengeAssetName = daAvailabilityChallengeAssetNameV1(OUT_REF);
  const [tranche] = planDaAvailabilityPublicationsV1({
    commitment,
    payload: bytes,
    challengeAssetName,
  });
  if (tranche === undefined) throw new Error("missing measured tranche");
  const publication = tranche.publications[0];
  if (publication === undefined)
    throw new Error("missing measured publication");
  const initial = {
    Active: {
      deployment_identity: DEPLOYMENT,
      header_hash: HEADER,
      challenge_asset_name: challengeAssetName,
      descriptor: tranche.descriptor,
      next_offset: tranche.descriptor.start_offset,
      accumulator: tranche.initialAccumulator,
      latest_carrier_output_index: null,
      response_deadline: BigInt(DA_AVAILABILITY_FULL_RESPONSE_WINDOW_MS_V1),
      challenger: OWNER,
    },
  } as const;
  const continued = advanceDaAvailabilityTrancheV1({
    active: initial,
    publication,
    responseGeometry: geometry,
    inclusiveValidityUpper: 1_000n,
    carrierOutputIndex: 1n,
  });
  const scriptAddress = CML.Address.from_raw_bytes(
    Buffer.concat([Buffer.from([0x70]), Buffer.alloc(28, 0xaa)]),
  );
  const keyAddress = CML.Address.from_raw_bytes(
    Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0xbb)]),
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(Buffer.alloc(32, 0x01)),
      0n,
    ),
  );
  const policyId = CML.ScriptHash.from_raw_bytes(Buffer.alloc(28, 0xcc));
  const trancheAssets = CML.MapAssetNameToCoin.new();
  trancheAssets.insert(
    CML.AssetName.from_raw_bytes(
      Buffer.from(
        daAvailabilityTrancheAssetNameV1({
          challengeAssetName,
          trancheIndex: 0,
        }),
        "hex",
      ),
    ),
    1n,
  );
  const multiAsset = CML.MultiAsset.new();
  multiAsset.insert_assets(policyId, trancheAssets);
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      scriptAddress,
      CML.Value.new(5_000_000_000n, multiAsset),
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_hex(
          encodeDaAvailabilityTrancheDatumV1(continued),
        ),
      ),
      undefined,
    ),
  );
  outputs.add(
    CML.TransactionOutput.new(
      scriptAddress,
      CML.Value.from_coin(5_000_000n),
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_hex(
          encodeDaAvailabilityPublicationDatumV1(
            publication,
            geometry,
            tranche.descriptor,
          ),
        ),
      ),
      undefined,
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 500_000n);
  body.set_ttl(1_000n);
  const referenceInputs = CML.TransactionInputList.new();
  referenceInputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(Buffer.alloc(32, 0x02)),
      0n,
    ),
  );
  body.set_reference_inputs(referenceInputs);
  const collateralInputs = CML.TransactionInputList.new();
  collateralInputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(Buffer.alloc(32, 0x03)),
      0n,
    ),
  );
  body.set_collateral_inputs(collateralInputs);
  body.set_total_collateral(1_000_000n);
  body.set_collateral_return(
    CML.TransactionOutput.new(
      keyAddress,
      CML.Value.from_coin(4_000_000n),
      undefined,
      undefined,
    ),
  );
  body.set_script_data_hash(
    CML.ScriptDataHash.from_raw_bytes(Buffer.alloc(32, 0xdd)),
  );
  const spendRedeemerCbor = Data.to(
    {
      AdvanceTranche: {
        thread_output_index: 0n,
        carrier_output_index: 1n,
        m_previous_carrier_input_index: null,
      },
    },
    DaAvailabilitySpendRedeemerV1,
  );
  const redeemers = CML.LegacyRedeemerList.new();
  redeemers.add(
    CML.LegacyRedeemer.new(
      CML.RedeemerTag.Spend,
      0n,
      CML.PlutusData.from_cbor_hex(spendRedeemerCbor),
      CML.ExUnits.new(1_311_209n, 583_403_149n),
    ),
  );
  const witnessSet = CML.TransactionWitnessSet.new();
  witnessSet.set_redeemers(CML.Redeemers.new_arr_legacy_redeemer(redeemers));
  const signingKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0xee));
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(CML.make_vkey_witness(CML.hash_transaction(body), signingKey));
  witnessSet.set_vkeywitnesses(vkeys);
  return CML.Transaction.new(body, witnessSet, true, undefined).to_cbor_bytes()
    .length;
};

const build = (length: number) =>
  buildDaAvailabilityCommitmentV1({
    deploymentIdentity: DEPLOYMENT,
    headerHash: HEADER,
    payload: payload(length),
    bondOwner: OWNER,
    responseGeometry: CANDIDATE_GEOMETRY,
  });

describe("Q58 canonical DA availability commitment V1", () => {
  it("fixes the approved deadlines while keeping matching bonds release-bound", () => {
    expect(daAvailabilityResponseWindowMsV1(64 * 1024)).toBe(
      DA_AVAILABILITY_SMALL_RESPONSE_WINDOW_MS_V1,
    );
    expect(daAvailabilityResponseWindowMsV1(64 * 1024 + 1)).toBe(
      DA_AVAILABILITY_FULL_RESPONSE_WINDOW_MS_V1,
    );
    expect(DA_AVAILABILITY_BOND_LOVELACE_MEASUREMENT_CANDIDATE_V1).toBe(
      10_000_000_000n,
    );
    expect(
      DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE_V1,
    ).toBe(DA_AVAILABILITY_BOND_LOVELACE_MEASUREMENT_CANDIDATE_V1);
    expect(
      daAvailabilityParametersV1({
        responseGeometry: CANDIDATE_GEOMETRY,
        daBondLovelace: 12_000_000_000n,
        challengerBondLovelace: 12_000_000_000n,
        maxOpenFeeLovelace: MAX_OPEN_FEE,
        maxPublicationFeeLovelace: MAX_PUBLICATION_FEE,
        maxSettlementFeeLovelace: MAX_SETTLEMENT_FEE,
        maxCloseFeeLovelace: MAX_CLOSE_FEE,
        maxTimeoutFeeLovelace: MAX_TIMEOUT_FEE,
      }).da_bond_lovelace,
    ).toBe(12_000_000_000n);
    expect(() =>
      daAvailabilityParametersV1({
        responseGeometry: CANDIDATE_GEOMETRY,
        daBondLovelace: 12_000_000_000n,
        challengerBondLovelace: 11_999_999_999n,
        maxOpenFeeLovelace: MAX_OPEN_FEE,
        maxPublicationFeeLovelace: MAX_PUBLICATION_FEE,
        maxSettlementFeeLovelace: MAX_SETTLEMENT_FEE,
        maxCloseFeeLovelace: MAX_CLOSE_FEE,
        maxTimeoutFeeLovelace: MAX_TIMEOUT_FEE,
      }),
    ).toThrow("exactly matching DA and challenger bonds");
    expect(() =>
      daAvailabilityParametersV1({
        responseGeometry: CANDIDATE_GEOMETRY,
        daBondLovelace: 12_000_000_000n,
        challengerBondLovelace: 12_000_000_000n,
        maxOpenFeeLovelace: MAX_OPEN_FEE,
        maxPublicationFeeLovelace: 1_000_000_000n,
        maxSettlementFeeLovelace: MAX_SETTLEMENT_FEE,
        maxCloseFeeLovelace: MAX_CLOSE_FEE,
        maxTimeoutFeeLovelace: MAX_TIMEOUT_FEE,
      }),
    ).toThrow("must cover every maximum-size publication fee");
  });

  it("uses the authenticated measured geometry without freezing its starting probe", () => {
    expect(
      deriveDaAvailabilityTrancheLayoutV1(64 * 1024, CANDIDATE_GEOMETRY),
    ).toEqual([{ trancheIndex: 0, startOffset: 0, byteLength: 64 * 1024 }]);
    expect(
      deriveDaAvailabilityTrancheLayoutV1(
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
    const max = deriveDaAvailabilityTrancheLayoutV1(
      64 * 1024 * 1024,
      CANDIDATE_GEOMETRY,
    );
    expect(max).toHaveLength(16);
    expect(max.at(-1)).toEqual({
      trancheIndex: 15,
      startOffset: 15 * 4 * 1024 * 1024,
      byteLength: 4 * 1024 * 1024,
    });

    const alternateGeometry = availabilityResponseGeometryV1({
      chunkByteLength: 8_000,
      trancheByteLength: 8 * 1024 * 1024,
      maxTrancheCount: 8,
    });
    expect(
      deriveDaAvailabilityTrancheLayoutV1(64 * 1024 * 1024, alternateGeometry),
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
    const activatedGeometry = availabilityResponseGeometryV1({
      chunkByteLength: 14_020,
      trancheByteLength: 4 * 1024 * 1024,
      maxTrancheCount: 16,
    });
    const maxPayload = new Uint8Array(4 * 1024 * 1024).fill(42);
    const commitment = buildDaAvailabilityCommitmentV1({
      deploymentIdentity: DEPLOYMENT,
      headerHash: HEADER,
      payload: maxPayload,
      bondOwner: OWNER,
      responseGeometry: activatedGeometry,
    });
    const [tranche] = planDaAvailabilityPublicationsV1({
      commitment,
      payload: maxPayload,
      challengeAssetName: daAvailabilityChallengeAssetNameV1(OUT_REF),
    });
    const first = tranche!.publications[0]!;
    expect({
      chunkCount: tranche!.descriptor.chunk_count,
      chunkCommitment: tranche!.descriptor.chunk_commitment,
      frontier: first.chunk_frontier,
      siblings: first.chunk_siblings,
      leafHash: daAvailabilityChunkLeafHashV1({
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
    const commitment = buildDaAvailabilityCommitmentV1({
      deploymentIdentity: DEPLOYMENT,
      headerHash: HEADER,
      payload: bytes,
      bondOwner: OWNER,
      responseGeometry: CANDIDATE_GEOMETRY,
    });
    assertCanonicalDaAvailabilityCommitmentV1(commitment);
    expect(
      Data.from(
        Data.to(commitment, DaAvailabilityCommitmentV1),
        DaAvailabilityCommitmentV1,
      ),
    ).toEqual(commitment);
    expect(
      verifyDaAvailabilityPayloadCommitmentV1({ commitment, payload: bytes }),
    ).toBe(true);

    for (const mutation of [
      { ...bytes, 0: bytes[0]! ^ 1 },
      bytes.subarray(1),
      Uint8Array.from([...bytes, 0]),
    ]) {
      expect(
        verifyDaAvailabilityPayloadCommitmentV1({
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
        daAvailabilityAttestationMessageV1(differentDeployment),
      ).toString("hex"),
    ).not.toBe(
      Buffer.from(daAvailabilityAttestationMessageV1(commitment)).toString(
        "hex",
      ),
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
        assertCanonicalDaAvailabilityCommitmentV1(malformed),
      ).toThrow();
    }
  });

  it("round-trips retained bond and state-queue availability states", () => {
    const commitment = build(1024);
    const available = {
      Available: {
        commitment,
        da_bond_asset_name: BOND_ASSET,
        committee_signers_hash: COMMITTEE_HASH,
        attested_signers: SIGNERS,
      },
    } as const;
    expect(
      Data.from(
        Data.to(available, DaAvailabilityBondDatumV1),
        DaAvailabilityBondDatumV1,
      ),
    ).toEqual(available);

    for (const status of [
      "Unattested",
      { Attested: { da_bond_asset_name: BOND_ASSET } },
      {
        Challenged: {
          da_bond_asset_name: BOND_ASSET,
          challenge_asset_name: "77".repeat(32),
        },
      },
      { Published: { terminal_commitment: "88".repeat(32) } },
    ] as const) {
      expect(
        Data.from(
          Data.to(status, DaAvailabilityStateQueueStatusV1),
          DaAvailabilityStateQueueStatusV1,
        ),
      ).toEqual(status);
    }
  });

  it("matches the on-chain merge gate for every availability state", () => {
    expect(daAvailabilityStateQueueStatusPermitsMergeV1("Unattested")).toBe(
      false,
    );
    expect(
      daAvailabilityStateQueueStatusPermitsMergeV1({
        Attested: { da_bond_asset_name: BOND_ASSET },
      }),
    ).toBe(true);
    expect(
      daAvailabilityStateQueueStatusPermitsMergeV1({
        Challenged: {
          da_bond_asset_name: BOND_ASSET,
          challenge_asset_name: "77".repeat(32),
        },
      }),
    ).toBe(false);
    expect(
      daAvailabilityStateQueueStatusPermitsMergeV1({
        Published: { terminal_commitment: "88".repeat(32) },
      }),
    ).toBe(true);
  });

  it("derives unique bounded bond, challenge, tranche, and published identities", () => {
    const bondAsset = daAvailabilityBondAssetNameV1(OUT_REF);
    const challengeAsset = daAvailabilityChallengeAssetNameV1(OUT_REF);
    const tranche0 = daAvailabilityTrancheAssetNameV1({
      challengeAssetName: challengeAsset,
      trancheIndex: 0,
    });
    const tranche15 = daAvailabilityTrancheAssetNameV1({
      challengeAssetName: challengeAsset,
      trancheIndex: 15,
    });
    expect(bondAsset).toHaveLength(64);
    expect(challengeAsset).toHaveLength(64);
    expect(tranche0).toHaveLength(64);
    expect(tranche15).toHaveLength(64);
    expect(new Set([bondAsset, challengeAsset, tranche0, tranche15]).size).toBe(
      4,
    );
    expect(
      daAvailabilityBondAssetNameV1({ ...OUT_REF, outputIndex: 8n }),
    ).not.toBe(bondAsset);
    expect(
      daAvailabilityTrancheAssetNameV1({
        challengeAssetName: challengeAsset,
        trancheIndex: 0,
      }),
    ).not.toBe(tranche15);
    expect(daAvailabilityPublishedTerminalCommitmentV1(build(1024))).toMatch(
      /^[0-9a-f]{64}$/u,
    );
    expect(() =>
      daAvailabilityTrancheAssetNameV1({
        challengeAssetName: "00".repeat(32),
        trancheIndex: 0,
      }),
    ).toThrow();
  });

  it("plans exact ordered publications and advances only through the deadline", () => {
    const geometry = availabilityResponseGeometryV1({
      chunkByteLength: 3,
      trancheByteLength: 64 * 1024,
      maxTrancheCount: 1024,
    });
    const bytes = payload(5);
    const commitment = buildDaAvailabilityCommitmentV1({
      deploymentIdentity: DEPLOYMENT,
      headerHash: HEADER,
      payload: bytes,
      bondOwner: OWNER,
      responseGeometry: geometry,
    });
    const challengeAssetName = daAvailabilityChallengeAssetNameV1(OUT_REF);
    const [tranche] = planDaAvailabilityPublicationsV1({
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
    const deadline = daAvailabilityResponseDeadlineV1({
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
    const first = advanceDaAvailabilityTrancheV1({
      active,
      publication: tranche!.publications[0]!,
      responseGeometry: geometry,
      inclusiveValidityUpper: deadline,
      carrierOutputIndex: 1n,
    });
    expect(first).toHaveProperty("Active.next_offset", 3n);
    const receipt = advanceDaAvailabilityTrancheV1({
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
      advanceDaAvailabilityTrancheV1({
        active,
        publication: tranche!.publications[0]!,
        responseGeometry: geometry,
        inclusiveValidityUpper: deadline + 1n,
        carrierOutputIndex: 1n,
      }),
    ).toThrow("exceeds the response deadline");
    expect(() =>
      advanceDaAvailabilityTrancheV1({
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
      advanceDaAvailabilityTrancheV1({
        active: receipt,
        publication: tranche!.publications[1]!,
        responseGeometry: geometry,
        inclusiveValidityUpper: deadline,
        carrierOutputIndex: 1n,
      }),
    ).toThrow("terminal receipt");
  });

  it("uses one complete inline item through the measured fit boundary and chunks only above it", () => {
    const geometry = availabilityResponseGeometryV1({
      chunkByteLength: 4095,
      trancheByteLength: 64 * 1024,
      maxTrancheCount: 1024,
    });
    const challengeAssetName = daAvailabilityChallengeAssetNameV1(OUT_REF);
    for (const [length, tier, publicationCount] of [
      [4095, "complete_item_inline", 1],
      [4096, "ordered_chunks", 2],
      [64 * 1024 + 1, "parallel_tranches", 18],
    ] as const) {
      const bytes = payload(length);
      const commitment = buildDaAvailabilityCommitmentV1({
        deploymentIdentity: DEPLOYMENT,
        headerHash: HEADER,
        payload: bytes,
        bondOwner: OWNER,
        responseGeometry: geometry,
      });
      const plan = planDaAvailabilityPublicationsV1({
        commitment,
        payload: bytes,
        challengeAssetName,
      });
      expect(
        daAvailabilityPublicationTierV1({
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
    const geometry = availabilityResponseGeometryV1({
      chunkByteLength: 4095,
      trancheByteLength: 64 * 1024,
      maxTrancheCount: 1024,
    });
    const commitment = buildDaAvailabilityCommitmentV1({
      deploymentIdentity: DEPLOYMENT,
      headerHash: HEADER,
      payload: payload(70 * 1024),
      bondOwner: OWNER,
      responseGeometry: geometry,
    });
    const challengeAssetName = daAvailabilityChallengeAssetNameV1(OUT_REF);
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
      assertDaAvailabilityTerminalReceiptsV1({
        commitment,
        challengeAssetName,
        challenger: OWNER,
        receipts,
      }),
    ).toBe(daAvailabilityPublishedTerminalCommitmentV1(commitment));
    expect(() =>
      assertDaAvailabilityTerminalReceiptsV1({
        commitment,
        challengeAssetName,
        challenger: OWNER,
        receipts: [receipts[1]!, receipts[0]!],
      }),
    ).toThrow("does not equal its signed descriptor");
    expect(() =>
      assertDaAvailabilityTerminalReceiptsV1({
        commitment,
        challengeAssetName,
        challenger: OWNER,
        receipts: [receipts[0]!, receipts[0]!],
      }),
    ).toThrow("does not equal its signed descriptor");
    expect(() =>
      assertDaAvailabilityTerminalReceiptsV1({
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
    const geometry = availabilityResponseGeometryV1({
      chunkByteLength: 4095,
      trancheByteLength: 64 * 1024,
      maxTrancheCount: 1024,
    });
    const bytes = payload(70 * 1024);
    const commitment = buildDaAvailabilityCommitmentV1({
      deploymentIdentity: DEPLOYMENT,
      headerHash: HEADER,
      payload: bytes,
      bondOwner: OWNER,
      responseGeometry: geometry,
    });
    const challengeAssetName = daAvailabilityChallengeAssetNameV1(OUT_REF);
    const openedAt = 1_000n;
    const responseDeadline = daAvailabilityResponseDeadlineV1({
      payloadByteLength: bytes.length,
      openedAt,
    });
    const parameters = daAvailabilityParametersV1({
      responseGeometry: geometry,
      daBondLovelace: DA_AVAILABILITY_BOND_LOVELACE_MEASUREMENT_CANDIDATE_V1,
      challengerBondLovelace:
        DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE_V1,
      maxOpenFeeLovelace: MAX_OPEN_FEE,
      maxPublicationFeeLovelace: MAX_PUBLICATION_FEE,
      maxSettlementFeeLovelace: MAX_SETTLEMENT_FEE,
      maxCloseFeeLovelace: MAX_CLOSE_FEE,
      maxTimeoutFeeLovelace: MAX_TIMEOUT_FEE,
    });
    const challengedBond = {
      ChallengedBond: {
        commitment,
        da_bond_asset_name: daAvailabilityBondAssetNameV1(OUT_REF),
        committee_signers_hash: COMMITTEE_HASH,
        attested_signers: SIGNERS,
        challenge_asset_name: challengeAssetName,
        challenger: OWNER,
        opened_at: openedAt,
        response_deadline: responseDeadline,
      },
    } as const;
    const challengedBondEvidence = {
      datumCborHex: encodeDaAvailabilityBondDatumV1(challengedBond, parameters),
      bondInputOutRef: OUT_REF,
      challengedBondOutputOutRef: {
        transactionId: "98".repeat(32),
        outputIndex: 1n,
      },
    } as const;
    const plan = planDaAvailabilityPublicationsV1({
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
      reconstructDaAvailabilityPayloadV1({
        challengedBond: challengedBondEvidence,
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
      reconstructDaAvailabilityPayloadV1({
        challengedBond: challengedBondEvidence,
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
      reconstructDaAvailabilityPayloadV1({
        challengedBond: challengedBondEvidence,
        parameters,
        tranches: reordered,
      }),
    ).toThrow();

    const forgedLaterDeadlineCbor = Data.to(
      {
        ChallengedBond: {
          ...challengedBond.ChallengedBond,
          response_deadline: responseDeadline + 1n,
        },
      },
      DaAvailabilityBondDatumV1,
    );
    expect(() =>
      reconstructDaAvailabilityPayloadV1({
        challengedBond: {
          ...challengedBondEvidence,
          datumCborHex: forgedLaterDeadlineCbor,
        },
        parameters,
        tranches,
      }),
    ).toThrow("exact canonical response deadline");
    expect(() =>
      reconstructDaAvailabilityPayloadV1({
        challengedBond: {
          ...challengedBondEvidence,
          bondInputOutRef: {
            ...OUT_REF,
            outputIndex: OUT_REF.outputIndex + 1n,
          },
        },
        parameters,
        tranches,
      }),
    ).toThrow("DACH identity derived from its consumed bond input");
    expect(() =>
      reconstructDaAvailabilityPayloadV1({
        challengedBond: {
          ...challengedBondEvidence,
          datumCborHex: encodeDaAvailabilityBondDatumV1(
            {
              Available: {
                commitment,
                da_bond_asset_name: daAvailabilityBondAssetNameV1(OUT_REF),
                committee_signers_hash: COMMITTEE_HASH,
                attested_signers: SIGNERS,
              },
            },
            parameters,
          ),
        },
        parameters,
        tranches,
      }),
    ).toThrow("requires the authenticated challenged-bond datum");
    const evidenceWithExtraField = {
      ...challengedBondEvidence,
      responseDeadline,
    };
    expect(() =>
      reconstructDaAvailabilityPayloadV1({
        challengedBond: evidenceWithExtraField,
        parameters,
        tranches,
      }),
    ).toThrow("must contain exactly datum and input/output identities");
  });

  it("fails closed outside canonical payload and chunk bounds", () => {
    expect(() =>
      deriveDaAvailabilityTrancheLayoutV1(0, CANDIDATE_GEOMETRY),
    ).toThrow();
    expect(() =>
      deriveDaAvailabilityTrancheLayoutV1(
        64 * 1024 * 1024 + 1,
        CANDIDATE_GEOMETRY,
      ),
    ).toThrow();
    expect(() =>
      availabilityResponseGeometryV1({
        ...DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE_V1,
        chunkByteLength: 16_000,
      }),
    ).toThrow();
  });

  it("pins the cross-language commitment and asset-identity vectors", () => {
    const commitment = build(1024);
    expect({
      attestationMessage: Buffer.from(
        daAvailabilityAttestationMessageV1(commitment),
      ).toString("hex"),
      terminalAccumulator:
        commitment.tranche_descriptors[0]?.terminal_accumulator,
      publishedTerminal:
        daAvailabilityPublishedTerminalCommitmentV1(commitment),
      bondAsset: daAvailabilityBondAssetNameV1(OUT_REF),
      challengeAsset: daAvailabilityChallengeAssetNameV1(OUT_REF),
    }).toEqual({
      attestationMessage:
        "3e6afd561e46492afcd026e421e69071e5eaecd5d1624beb98118f7db5e17bc7",
      terminalAccumulator:
        "966596e9655de8409b0ae66e3565b4e0720249359fc047d5acce2e79d3f21b58",
      publishedTerminal:
        "fa14ab0d721e643d5b6b969c947cfe6feb622cc12678ff12da69ec9cb4d1634a",
      bondAsset:
        "4441424e4acf0e543a1ed6e9d85df692e5b23859fa3ba1d807b1f44361f5fe9f",
      challengeAsset:
        "444143484acf0e543a1ed6e9d85df692e5b23859fa3ba1d807b1f44361f5fe9f",
    });
  });

  it("strictly decodes release parameters and signed commitments for durable handoff", () => {
    const parameters = daAvailabilityParametersV1({
      responseGeometry: CANDIDATE_GEOMETRY,
      daBondLovelace: 12_000_000_000n,
      challengerBondLovelace: 12_000_000_000n,
      maxOpenFeeLovelace: MAX_OPEN_FEE,
      maxPublicationFeeLovelace: MAX_PUBLICATION_FEE,
      maxSettlementFeeLovelace: MAX_SETTLEMENT_FEE,
      maxCloseFeeLovelace: MAX_CLOSE_FEE,
      maxTimeoutFeeLovelace: MAX_TIMEOUT_FEE,
    });
    const parametersCbor = encodeDaAvailabilityParametersV1(parameters);
    expect(parseDaAvailabilityParametersV1Cbor(parametersCbor)).toEqual(
      parameters,
    );
    expect(() =>
      parseDaAvailabilityParametersV1Cbor(parametersCbor.toUpperCase()),
    ).toThrow("lowercase CBOR hex");

    const mismatchedBondsCbor = Data.to(
      {
        ...parameters,
        challenger_bond_lovelace: parameters.challenger_bond_lovelace - 1n,
      },
      DaAvailabilityParametersV1,
    );
    expect(() =>
      parseDaAvailabilityParametersV1Cbor(mismatchedBondsCbor),
    ).toThrow("exactly matching DA and challenger bonds");

    const commitment = build(70 * 1024);
    const commitmentCbor = encodeDaAvailabilityCommitmentV1(commitment);
    expect(
      parseDaAvailabilityCommitmentV1Cbor(commitmentCbor, CANDIDATE_GEOMETRY),
    ).toEqual(commitment);
    const alternateGeometry = availabilityResponseGeometryV1({
      chunkByteLength: 8_000,
      trancheByteLength: 8 * 1024 * 1024,
      maxTrancheCount: 8,
    });
    expect(() =>
      parseDaAvailabilityCommitmentV1Cbor(commitmentCbor, alternateGeometry),
    ).toThrow("does not equal the authenticated deployment/DA parameters");
  });

  it("strictly binds inline publication datums to the signed tranche descriptor", () => {
    const geometry = availabilityResponseGeometryV1({
      chunkByteLength: 3,
      trancheByteLength: 64 * 1024,
      maxTrancheCount: 1024,
    });
    const bytes = payload(5);
    const commitment = buildDaAvailabilityCommitmentV1({
      deploymentIdentity: DEPLOYMENT,
      headerHash: HEADER,
      payload: bytes,
      bondOwner: OWNER,
      responseGeometry: geometry,
    });
    const [tranche] = planDaAvailabilityPublicationsV1({
      commitment,
      payload: bytes,
      challengeAssetName: daAvailabilityChallengeAssetNameV1(OUT_REF),
    });
    const publication = tranche!.publications[0]!;
    const publicationCbor = encodeDaAvailabilityPublicationDatumV1(
      publication,
      geometry,
      tranche!.descriptor,
    );
    expect(
      parseDaAvailabilityPublicationDatumV1Cbor(
        publicationCbor,
        geometry,
        tranche!.descriptor,
      ),
    ).toEqual(publication);

    const oversizedGeometry = availabilityResponseGeometryV1({
      chunkByteLength: 2,
      trancheByteLength: 64 * 1024,
      maxTrancheCount: 1024,
    });
    expect(() =>
      parseDaAvailabilityPublicationDatumV1Cbor(
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
        parseDaAvailabilityPublicationDatumV1Cbor(
          Data.to(malformed, DaAvailabilityPublicationDatumV1),
          geometry,
          tranche!.descriptor,
        ),
      ).toThrow();
    }

    const foreignPayload = Uint8Array.from(bytes, (value) => value ^ 0xff);
    const foreignCommitment = buildDaAvailabilityCommitmentV1({
      deploymentIdentity: DEPLOYMENT,
      headerHash: HEADER,
      payload: foreignPayload,
      bondOwner: OWNER,
      responseGeometry: geometry,
    });
    const [foreignTranche] = planDaAvailabilityPublicationsV1({
      commitment: foreignCommitment,
      payload: foreignPayload,
      challengeAssetName: daAvailabilityChallengeAssetNameV1(OUT_REF),
    });
    const foreignPublication = foreignTranche!.publications[0]!;
    const foreignCbor = Data.to(
      foreignPublication,
      DaAvailabilityPublicationDatumV1,
    );
    expect(() =>
      parseDaAvailabilityPublicationDatumV1Cbor(
        foreignCbor,
        geometry,
        tranche!.descriptor,
      ),
    ).toThrow("does not equal the signed tranche descriptor");
  });

  it("derives exact challenge datums and deterministic tranche funding", () => {
    const geometry = availabilityResponseGeometryV1({
      chunkByteLength: 4095,
      trancheByteLength: 64 * 1024,
      maxTrancheCount: 1024,
    });
    const parameters = daAvailabilityParametersV1({
      responseGeometry: geometry,
      daBondLovelace: 10_000_000_000n,
      challengerBondLovelace: 10_000_000_000n,
      maxOpenFeeLovelace: MAX_OPEN_FEE,
      maxPublicationFeeLovelace: MAX_PUBLICATION_FEE,
      maxSettlementFeeLovelace: MAX_SETTLEMENT_FEE,
      maxCloseFeeLovelace: MAX_CLOSE_FEE,
      maxTimeoutFeeLovelace: MAX_TIMEOUT_FEE,
    });
    const commitment = buildDaAvailabilityCommitmentV1({
      deploymentIdentity: DEPLOYMENT,
      headerHash: HEADER,
      payload: payload(70 * 1024),
      bondOwner: OWNER,
      responseGeometry: geometry,
    });
    const availableBond = {
      Available: {
        commitment,
        da_bond_asset_name: daAvailabilityBondAssetNameV1(OUT_REF),
        committee_signers_hash: COMMITTEE_HASH,
        attested_signers: SIGNERS,
      },
    } as const;
    const bondInputOutRef = { ...OUT_REF, outputIndex: 8n };
    const plan = buildDaAvailabilityChallengeDatumPlanV1({
      availableBond,
      bondInputOutRef,
      challenger: OWNER,
      openedAt: 1_000n,
      parameters,
    });
    expect(plan.challengeAssetName).toBe(
      daAvailabilityChallengeAssetNameV1(bondInputOutRef),
    );
    expect(plan.responseDeadline).toBe(
      1_000n + BigInt(DA_AVAILABILITY_FULL_RESPONSE_WINDOW_MS_V1),
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
      folded_terminal_accumulator: daAvailabilityTerminalAccumulatorStartV1({
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
    expect(
      parseDaAvailabilityBondDatumV1Cbor(
        encodeDaAvailabilityBondDatumV1(plan.challengedBond, parameters),
        parameters,
      ),
    ).toEqual(plan.challengedBond);
    expect(
      plan.trancheThreads.map((thread) =>
        parseDaAvailabilityTrancheDatumV1Cbor(
          encodeDaAvailabilityTrancheDatumV1(thread),
        ),
      ),
    ).toEqual(plan.trancheThreads);
    expect(
      parseDaAvailabilityTerminalAccumulatorDatumV1Cbor(
        encodeDaAvailabilityTerminalAccumulatorDatumV1(
          plan.terminalAccumulator,
        ),
      ),
    ).toEqual(plan.terminalAccumulator);

    expect(() =>
      buildDaAvailabilityChallengeDatumPlanV1({
        availableBond: plan.challengedBond,
        bondInputOutRef,
        challenger: OWNER,
        openedAt: 1_000n,
        parameters,
      }),
    ).toThrow("only an available retained DA bond");

    if (!("ChallengedBond" in plan.challengedBond)) {
      throw new Error("challenge planner returned an available bond");
    }
    const malformedDeadline = {
      ChallengedBond: {
        ...plan.challengedBond.ChallengedBond,
        response_deadline: plan.responseDeadline + 1n,
      },
    };
    expect(() =>
      parseDaAvailabilityBondDatumV1Cbor(
        Data.to(malformedDeadline, DaAvailabilityBondDatumV1),
        parameters,
      ),
    ).toThrow("exact canonical response deadline");
  });

  it("conserves isolated tranche/carrier value and attributes each fee exactly once", () => {
    const geometry = availabilityResponseGeometryV1({
      chunkByteLength: 4095,
      trancheByteLength: 64 * 1024,
      maxTrancheCount: 1024,
    });
    const parameters = daAvailabilityParametersV1({
      responseGeometry: geometry,
      daBondLovelace: 10_000_000_000n,
      challengerBondLovelace: 10_000_000_000n,
      maxOpenFeeLovelace: MAX_OPEN_FEE,
      maxPublicationFeeLovelace: MAX_PUBLICATION_FEE,
      maxSettlementFeeLovelace: MAX_SETTLEMENT_FEE,
      maxCloseFeeLovelace: MAX_CLOSE_FEE,
      maxTimeoutFeeLovelace: MAX_TIMEOUT_FEE,
    });
    const firstThread = planDaAvailabilityPublicationValueTransitionV1({
      threadInputLovelace: 5_004_350_000n,
      previousCarrierInputLovelace: 0n,
      nextCarrierOutputLovelace: 2_000_000n,
      transactionFeeLovelace: 400_000n,
      minimumThreadOutputLovelace: 1_000_000n,
      isFirstPublication: true,
      parameters,
    });
    expect(firstThread).toBe(5_001_950_000n);
    assertDaAvailabilityChallengerBondConservationV1({
      initialChallengerBondLovelace: 10_000_000_000n,
      currentThreadLovelace: [firstThread, 4_995_650_000n],
      currentCarrierLovelace: [2_000_000n],
      paidTransactionFeesLovelace: [400_000n],
    });

    const secondThread = planDaAvailabilityPublicationValueTransitionV1({
      threadInputLovelace: firstThread,
      previousCarrierInputLovelace: 2_000_000n,
      nextCarrierOutputLovelace: 1_800_000n,
      transactionFeeLovelace: 450_000n,
      minimumThreadOutputLovelace: 1_000_000n,
      isFirstPublication: false,
      parameters,
    });
    expect(secondThread).toBe(5_001_700_000n);
    assertDaAvailabilityChallengerBondConservationV1({
      initialChallengerBondLovelace: 10_000_000_000n,
      currentThreadLovelace: [secondThread, 4_995_650_000n],
      currentCarrierLovelace: [1_800_000n],
      paidTransactionFeesLovelace: [400_000n, 450_000n],
    });

    const refunds = planDaAvailabilityTerminalRefundV1({
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
      planDaAvailabilityPublicationValueTransitionV1({
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
      planDaAvailabilityPublicationValueTransitionV1({
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
      assertDaAvailabilityChallengerBondConservationV1({
        initialChallengerBondLovelace: 10_000_000_000n,
        currentThreadLovelace: [firstThread, 4_995_650_000n],
        currentCarrierLovelace: [2_000_000n],
        paidTransactionFeesLovelace: [400_000n, 400_000n],
      }),
    ).toThrow("not isolated and exactly conserved");
    expect(() =>
      planDaAvailabilityTerminalRefundV1({
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
      planDaAvailabilityTerminalRefundV1({
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
    const parameters = daAvailabilityParametersV1({
      responseGeometry: CANDIDATE_GEOMETRY,
      daBondLovelace: 10_000_000_000n,
      challengerBondLovelace: 10_000_000_000n,
      maxOpenFeeLovelace: MAX_OPEN_FEE,
      maxPublicationFeeLovelace: MAX_PUBLICATION_FEE,
      maxSettlementFeeLovelace: MAX_SETTLEMENT_FEE,
      maxCloseFeeLovelace: MAX_CLOSE_FEE,
      maxTimeoutFeeLovelace: MAX_TIMEOUT_FEE,
    });
    const bytes = payload(16_000);
    const commitment = buildDaAvailabilityCommitmentV1({
      deploymentIdentity: DEPLOYMENT,
      headerHash: HEADER,
      payload: bytes,
      bondOwner: OWNER,
      responseGeometry: CANDIDATE_GEOMETRY,
    });
    const available = {
      Available: {
        commitment,
        da_bond_asset_name: BOND_ASSET,
        committee_signers_hash: COMMITTEE_HASH,
        attested_signers: SIGNERS,
      },
    } as const;
    const challenge = buildDaAvailabilityChallengeDatumPlanV1({
      availableBond: available,
      bondInputOutRef: OUT_REF,
      challenger: OWNER,
      openedAt: 1_000n,
      parameters,
    });
    const [tranchePlan] = planDaAvailabilityPublicationsV1({
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
      receipt = advanceDaAvailabilityTrancheV1({
        active: receipt,
        publication,
        responseGeometry: CANDIDATE_GEOMETRY,
        inclusiveValidityUpper: challenge.responseDeadline,
        carrierOutputIndex: 1n,
      });
    }
    const published = planDaAvailabilitySettlementV1({
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

    const timedOut = planDaAvailabilitySettlementV1({
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
      planDaAvailabilitySettlementV1({
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
