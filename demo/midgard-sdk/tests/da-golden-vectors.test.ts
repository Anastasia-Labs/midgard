/**
 * The TypeScript consumer of the two DA golden channels:
 *
 *   * `da-commitment-v1` (`fixtures/da-commitment-v1.generated.json`): the
 *     `CommitmentV1` bytes, `commitment_hash_v1`, the attestation message, the
 *     `ChallengeRecordV1` bytes and the DACH name at 1, 16 and 64 tranches;
 *   * `da-bond-pool-v1` (`fixtures/da-bond-pool-v1.generated.json`): the pool
 *     datum and redeemers, every `StateQueueStatusV1` arm and `ParametersV1` at
 *     max_tranche_count 1, 16 and 64.
 *
 * Both fixtures are written by generators that encode with their own
 * `scripts/da-vector-support.mjs`, never with the SDK codec, and the Aiken
 * golden modules pin the same hex against `builtin.serialise_data`. So a pass
 * here means the SDK codecs agree with the on-chain encoding byte for byte.
 * When one disagrees, fix the codec, never the fixture: the fixtures are
 * regenerated only when the Aiken types change.
 */
import { readFileSync } from "node:fs";

import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  daAvailabilityAttestationMessage,
  daAvailabilityChallengeAssetName,
  type DaAvailabilityChallengeRecord,
  DaAvailabilityChallengeRecord as DaAvailabilityChallengeRecordData,
  type DaAvailabilityCommitment,
  daAvailabilityCommitmentHash,
  type DaAvailabilityParameters,
  encodeDaAvailabilityChallengeRecord,
  encodeDaAvailabilityCommitment,
  encodeDaAvailabilityParameters,
  parseDaAvailabilityCommitmentCbor,
  parseDaAvailabilityParametersCbor,
} from "../src/availability-challenge.js";
import { DaAvailabilityStateQueueStatus } from "../src/da-availability-state.js";
import {
  type DaBondPoolDatum,
  DaBondPoolMintRedeemer,
  DaBondPoolSpendRedeemer,
  encodeDaBondPoolDatum,
  parseDaBondPoolDatumCbor,
} from "../src/da-bond-pool.js";

const readFixture = <T>(name: string): T =>
  JSON.parse(
    readFileSync(new URL(`./fixtures/${name}`, import.meta.url), "utf8"),
  ) as T;

const snake = (name: string): string =>
  name.replace(/[A-Z]/gu, (letter) => `_${letter.toLowerCase()}`);

// ---------------------------------------------------------------------------
// da-commitment-v1
// ---------------------------------------------------------------------------

type CommitmentGolden = {
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

type CommitmentVector = {
  readonly label: string;
  readonly trancheCount: number;
  readonly commitment: CommitmentGolden;
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

const COMMITMENT_FIXTURE = readFixture<{
  readonly vectors: readonly CommitmentVector[];
}>("da-commitment-v1.generated.json");

const commitmentOf = (golden: CommitmentGolden): DaAvailabilityCommitment => ({
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

const recordOf = (vector: CommitmentVector): DaAvailabilityChallengeRecord => ({
  commitment: commitmentOf(vector.commitment),
  challenge_asset_name: vector.challengeRecord.challengeAssetName,
  challenger: vector.challengeRecord.challenger,
  opened_at: BigInt(vector.challengeRecord.openedAt),
  response_deadline: BigInt(vector.challengeRecord.responseDeadline),
});

// `tranches_64` measures the widest record and uses a 259,200,000 ms response
// window, which is no profile's window, so the strict record encoder (which
// requires the selected profile's exact deadline) refuses it. Its bytes are
// still checked through the record schema.
const PROFILE_DEADLINE_RECORD_LABELS = ["tranches_1", "tranches_16"];

describe("da-commitment-v1 goldens through the SDK codecs", () => {
  it("covers the 1, 16 and 64 tranche classes", () => {
    expect(
      COMMITMENT_FIXTURE.vectors.map((vector) => [
        vector.label,
        vector.commitment.trancheDescriptors.length,
      ]),
    ).toEqual([
      ["tranches_1", 1],
      ["tranches_16", 16],
      ["tranches_64", 64],
    ]);
  });

  describe.each(COMMITMENT_FIXTURE.vectors)("$label", (vector) => {
    const commitment = commitmentOf(vector.commitment);

    it("commitmentCborHex: encodeDaAvailabilityCommitment", () => {
      expect(encodeDaAvailabilityCommitment(commitment)).toBe(
        vector.commitmentCborHex,
      );
      expect(
        parseDaAvailabilityCommitmentCbor(vector.commitmentCborHex),
      ).toEqual(commitment);
    });

    it("commitmentHashHex: daAvailabilityCommitmentHash", () => {
      expect(daAvailabilityCommitmentHash(commitment)).toBe(
        vector.commitmentHashHex,
      );
    });

    it("attestationMessageHex: daAvailabilityAttestationMessage", () => {
      expect(
        Buffer.from(daAvailabilityAttestationMessage(commitment)).toString(
          "hex",
        ),
      ).toBe(vector.attestationMessageHex);
    });

    it("challengeRecordCborHex: the ChallengeRecordV1 codec", () => {
      const record = recordOf(vector);
      expect(Data.to(record, DaAvailabilityChallengeRecordData)).toBe(
        vector.challengeRecordCborHex,
      );
      expect(
        Data.from(
          vector.challengeRecordCborHex,
          DaAvailabilityChallengeRecordData,
        ),
      ).toEqual(record);
      if (PROFILE_DEADLINE_RECORD_LABELS.includes(vector.label)) {
        expect(encodeDaAvailabilityChallengeRecord(record)).toBe(
          vector.challengeRecordCborHex,
        );
      } else {
        expect(() => encodeDaAvailabilityChallengeRecord(record)).toThrow(
          "exact canonical response deadline",
        );
      }
    });

    it("challengeAssetNameHex: daAvailabilityChallengeAssetName", () => {
      expect(
        daAvailabilityChallengeAssetName({
          transactionId: vector.outputReference.transactionId,
          outputIndex: BigInt(vector.outputReference.outputIndex),
        }),
      ).toBe(vector.challengeAssetNameHex);
      expect(vector.challengeRecord.challengeAssetName).toBe(
        vector.challengeAssetNameHex,
      );
    });
  });
});

// ---------------------------------------------------------------------------
// da-bond-pool-v1
// ---------------------------------------------------------------------------

type Tagged = { readonly kind: string } & Readonly<Record<string, string>>;

type PoolVector<K extends string, V> = {
  readonly label: string;
  readonly cborHex: string;
} & Readonly<Record<K, V>>;

type ParametersGolden = {
  readonly responseGeometry: Readonly<Record<string, string>>;
} & Readonly<Record<string, string>>;

const POOL_FIXTURE = readFixture<{
  readonly parametersProfile: string;
  readonly poolDatums: readonly PoolVector<"datum", Tagged>[];
  readonly poolMintRedeemers: readonly PoolVector<"redeemer", Tagged>[];
  readonly poolSpendRedeemers: readonly PoolVector<"redeemer", Tagged>[];
  readonly statuses: readonly PoolVector<"status", Tagged>[];
  readonly parameters: readonly PoolVector<"parameters", ParametersGolden>[];
}>("da-bond-pool-v1.generated.json");

/** `{ kind, ...fields }` as a Lucid enum value: a literal or `{ Kind: {...} }`. */
const enumValueOf = (
  tagged: Tagged,
  field: (name: string, value: string) => bigint | string,
): unknown => {
  const { kind, ...fields } = tagged;
  const entries = Object.entries(fields);
  return entries.length === 0
    ? kind
    : {
        [kind]: Object.fromEntries(
          entries.map(([name, value]) => [snake(name), field(name, value)]),
        ),
      };
};

const integerField = (_name: string, value: string): bigint => BigInt(value);

const statusField = (_name: string, value: string): string => value;

const parametersOf = (golden: ParametersGolden): DaAvailabilityParameters => {
  const { responseGeometry, ...amounts } = golden;
  return {
    response_geometry: {
      chunk_byte_length: BigInt(responseGeometry.chunkByteLength),
      tranche_byte_length: BigInt(responseGeometry.trancheByteLength),
      max_tranche_count: BigInt(responseGeometry.maxTrancheCount),
    },
    ...Object.fromEntries(
      Object.entries(amounts).map(([name, value]) => [
        snake(name),
        BigInt(value as string),
      ]),
    ),
  } as DaAvailabilityParameters;
};

describe("da-bond-pool-v1 goldens through the SDK codecs", () => {
  it("covers every pool arm, every status and the 1, 16 and 64 classes", () => {
    expect(
      POOL_FIXTURE.poolDatums.map((vector) => vector.datum.kind),
    ).toContain("Bonded");
    expect(
      POOL_FIXTURE.poolDatums.filter(
        (vector) => vector.datum.kind === "Withdrawing",
      ).length,
    ).toBeGreaterThanOrEqual(3);
    expect(
      POOL_FIXTURE.poolMintRedeemers.map((vector) => vector.redeemer.kind),
    ).toEqual(["InitPool"]);
    expect(
      POOL_FIXTURE.poolSpendRedeemers.map((vector) => vector.redeemer.kind),
    ).toEqual([
      "TopUp",
      "Slash",
      "BeginWithdraw",
      "CancelWithdraw",
      "CompleteWithdraw",
    ]);
    expect(POOL_FIXTURE.statuses.map((vector) => vector.status.kind)).toEqual([
      "Unattested",
      "Attested",
      "Challenged",
      "Published",
    ]);
    expect(
      POOL_FIXTURE.parameters.map(
        (vector) => vector.parameters.responseGeometry.maxTrancheCount,
      ),
    ).toEqual(["1", "16", "64"]);
  });

  describe.each(POOL_FIXTURE.poolDatums)("pool datum $label", (vector) => {
    const datum = enumValueOf(vector.datum, integerField) as DaBondPoolDatum;
    it("encodeDaBondPoolDatum reproduces the hex and parses it back", () => {
      expect(encodeDaBondPoolDatum(datum)).toBe(vector.cborHex);
      expect(parseDaBondPoolDatumCbor(vector.cborHex)).toEqual(datum);
    });
  });

  describe.each(POOL_FIXTURE.poolMintRedeemers)(
    "pool mint redeemer $label",
    (vector) => {
      // `InitPool` is the policy's only constructor, which the SDK schema
      // writes as a plain object (see `DaBondPoolMintRedeemerSchema`).
      const redeemer: DaBondPoolMintRedeemer = {
        output_index: BigInt(vector.redeemer.outputIndex!),
      };
      it("DaBondPoolMintRedeemer reproduces the hex and decodes it", () => {
        expect(vector.redeemer.kind).toBe("InitPool");
        expect(Data.to(redeemer, DaBondPoolMintRedeemer)).toBe(vector.cborHex);
        expect(Data.from(vector.cborHex, DaBondPoolMintRedeemer)).toEqual(
          redeemer,
        );
      });
    },
  );

  describe.each(POOL_FIXTURE.poolSpendRedeemers)(
    "pool spend redeemer $label",
    (vector) => {
      const redeemer = enumValueOf(
        vector.redeemer,
        integerField,
      ) as DaBondPoolSpendRedeemer;
      it("DaBondPoolSpendRedeemer reproduces the hex and decodes it", () => {
        expect(Data.to(redeemer, DaBondPoolSpendRedeemer)).toBe(vector.cborHex);
        expect(Data.from(vector.cborHex, DaBondPoolSpendRedeemer)).toEqual(
          redeemer,
        );
      });
    },
  );

  describe.each(POOL_FIXTURE.statuses)("status $label", (vector) => {
    const status = enumValueOf(
      vector.status,
      statusField,
    ) as DaAvailabilityStateQueueStatus;
    it("DaAvailabilityStateQueueStatus reproduces the hex and decodes it", () => {
      expect(Data.to(status, DaAvailabilityStateQueueStatus)).toBe(
        vector.cborHex,
      );
      expect(Data.from(vector.cborHex, DaAvailabilityStateQueueStatus)).toEqual(
        status,
      );
    });
  });

  it("parameters carry the named profile's da_bond amounts", () => {
    const profile =
      DEPLOYMENT_PROFILES[
        POOL_FIXTURE.parametersProfile as keyof typeof DEPLOYMENT_PROFILES
      ];
    expect(profile).toBeDefined();
    for (const vector of POOL_FIXTURE.parameters) {
      const parameters = parametersOf(vector.parameters);
      expect({
        da_bond_lovelace: parameters.da_bond_lovelace,
        da_slash_penalty_lovelace: parameters.da_slash_penalty_lovelace,
        da_bond_min_top_up_lovelace: parameters.da_bond_min_top_up_lovelace,
        da_bond_pool_floor_lovelace: parameters.da_bond_pool_floor_lovelace,
        challenge_record_lovelace: parameters.challenge_record_lovelace,
      }).toEqual(
        Object.fromEntries(
          Object.entries(profile.da_bond).map(([name, value]) => [
            name,
            BigInt(value),
          ]),
        ),
      );
    }
  });

  describe.each(POOL_FIXTURE.parameters)("parameters $label", (vector) => {
    const parameters = parametersOf(vector.parameters);
    it("encodeDaAvailabilityParameters reproduces the hex and parses it back", () => {
      expect(encodeDaAvailabilityParameters(parameters)).toBe(vector.cborHex);
      expect(parseDaAvailabilityParametersCbor(vector.cborHex)).toEqual(
        parameters,
      );
    });
  });
});
