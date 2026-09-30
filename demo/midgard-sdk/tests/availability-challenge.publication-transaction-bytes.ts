import { readFileSync } from "node:fs";

import { CML, Data } from "@lucid-evolution/lucid";

import {
  advanceDaAvailabilityTranche,
  availabilityResponseGeometry,
  buildDaAvailabilityCommitment,
  DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE,
  DA_AVAILABILITY_FULL_RESPONSE_WINDOW_MS,
  DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
  daAvailabilityChallengeAssetName,
  DaAvailabilityCommitment,
  daAvailabilityParameters,
  DaAvailabilitySpendRedeemer,
  daAvailabilityTrancheAssetName,
  encodeDaAvailabilityPublicationDatum,
  encodeDaAvailabilityTrancheDatum,
  planDaAvailabilityPublications,
} from "../src/availability-challenge.js";

export const DEPLOYMENT = "11".repeat(28);

export const HEADER = "22".repeat(28);

export const OWNER = "33".repeat(28);

export const OUT_REF = { transactionId: "99".repeat(32), outputIndex: 7n };

export const CHALLENGE_ASSET = daAvailabilityChallengeAssetName(OUT_REF);

export const COMMITMENT_HASH = "44".repeat(32);

export const MAX_OPEN_FEE = 500_000n;

export const MAX_PUBLICATION_FEE = 500_000n;

export const MAX_SETTLEMENT_FEE = 500_000n;

export const MAX_CLOSE_FEE = 1_000_000n;

export const MAX_TIMEOUT_FEE = 1_200_000n;

export const CANDIDATE_GEOMETRY = availabilityResponseGeometry(
  DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
);

type DaAvailabilityParametersInput = Parameters<
  typeof daAvailabilityParameters
>[0];

// The selected profile's DA amounts, a 10k tADA challenger bond and the test
// fee ceilings, with any field overridden.
export const parameterInput = (
  overrides: Partial<DaAvailabilityParametersInput>,
): DaAvailabilityParametersInput => ({
  responseGeometry: CANDIDATE_GEOMETRY,
  ...DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  challengerBondLovelace:
    DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE,
  maxOpenFeeLovelace: MAX_OPEN_FEE,
  maxPublicationFeeLovelace: MAX_PUBLICATION_FEE,
  maxSettlementFeeLovelace: MAX_SETTLEMENT_FEE,
  maxCloseFeeLovelace: MAX_CLOSE_FEE,
  maxTimeoutFeeLovelace: MAX_TIMEOUT_FEE,
  ...overrides,
});

export const payload = (length: number): Uint8Array =>
  Uint8Array.from({ length }, (_, index) => (index * 17 + 3) % 256);

export const publicationTransactionBytes = (
  chunkByteLength: number,
): number => {
  const geometry = availabilityResponseGeometry({
    chunkByteLength,
    trancheByteLength: 4 * 1024 * 1024,
    maxTrancheCount: 16,
  });
  const bytes = payload(4 * 1024 * 1024);
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
      response_deadline: BigInt(DA_AVAILABILITY_FULL_RESPONSE_WINDOW_MS),
      challenger: OWNER,
    },
  } as const;
  const continued = advanceDaAvailabilityTranche({
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
        daAvailabilityTrancheAssetName({
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
          encodeDaAvailabilityTrancheDatum(continued),
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
          encodeDaAvailabilityPublicationDatum(
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
    DaAvailabilitySpendRedeemer,
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

export const build = (length: number) =>
  buildDaAvailabilityCommitment({
    deploymentIdentity: DEPLOYMENT,
    headerHash: HEADER,
    payload: payload(length),
    responseGeometry: CANDIDATE_GEOMETRY,
  });

type GoldenCommitment = {
  readonly version: number;
  readonly deploymentIdentity: string;
  readonly headerHash: string;
  readonly payloadByteLength: number;
  readonly responseGeometry: {
    readonly chunkByteLength: number;
    readonly trancheByteLength: number;
    readonly maxTrancheCount: number;
  };
  readonly trancheDescriptors: readonly {
    readonly trancheIndex: number;
    readonly startOffset: number;
    readonly byteLength: number;
    readonly chunkCount: number;
    readonly chunkCommitment: string;
    readonly terminalAccumulator: string;
  }[];
};

type GoldenVector = {
  readonly label: string;
  readonly commitment: GoldenCommitment;
  readonly commitmentCborHex: string;
  readonly commitmentHashHex: string;
  readonly attestationMessageHex: string;
  readonly challengeRecord: {
    readonly challengeAssetName: string;
    readonly challenger: string;
    readonly openedAt: number;
    readonly responseDeadline: number;
  };
  readonly challengeRecordCborHex: string;
  readonly outputReference: {
    readonly transactionId: string;
    readonly outputIndex: number;
  };
  readonly challengeAssetNameHex: string;
};

// The #688 cross-language goldens, generated by
// scripts/generate-da-commitment-v1-goldens.mjs and asserted byte for byte by
// the Aiken golden module. Never edited by hand.
export const COMMITMENT_GOLDENS = JSON.parse(
  readFileSync(
    new URL("./fixtures/da-commitment-v1.generated.json", import.meta.url),
    "utf8",
  ),
) as { readonly vectors: readonly GoldenVector[] };

export const commitmentFromGolden = (
  golden: GoldenCommitment,
): DaAvailabilityCommitment => ({
  version: BigInt(golden.version),
  deployment_identity: golden.deploymentIdentity,
  header_hash: golden.headerHash,
  payload_byte_length: BigInt(golden.payloadByteLength),
  response_geometry: {
    chunk_byte_length: BigInt(golden.responseGeometry.chunkByteLength),
    tranche_byte_length: BigInt(golden.responseGeometry.trancheByteLength),
    max_tranche_count: BigInt(golden.responseGeometry.maxTrancheCount),
  },
  tranche_descriptors: golden.trancheDescriptors.map((descriptor) => ({
    tranche_index: BigInt(descriptor.trancheIndex),
    start_offset: BigInt(descriptor.startOffset),
    byte_length: BigInt(descriptor.byteLength),
    chunk_count: BigInt(descriptor.chunkCount),
    chunk_commitment: descriptor.chunkCommitment,
    terminal_accumulator: descriptor.terminalAccumulator,
  })),
});
