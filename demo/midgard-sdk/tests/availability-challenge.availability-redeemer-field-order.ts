import "./availability-challenge.q58-canonical-da-availability-commitment-v1.js";

import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";
import { CML, Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
  DA_AVAILABILITY_SMALL_PAYLOAD_MAX_BYTES,
  DaAvailabilityChallengeRecord,
  DaAvailabilityMintRedeemer,
  DaAvailabilitySpendRedeemer,
} from "../src/availability-challenge.js";
import {
  COMMITMENT_GOLDENS,
  OWNER,
} from "./availability-challenge.publication-transaction-bytes.js";

describe("availability mint delegation ABI", () => {
  it.each([
    [0, "OpenChallenge", [1n, 2n, 3n, 4n, 5n, 6n, 7n, OWNER]],
    [1, "SettleTranche", [1n, 2n, 3n, 4n, new Constr(1, [])]],
    [2, "CloseChallenge", [1n, 2n, 3n, 4n, 5n, 6n]],
    [3, "TimeoutChallenge", [1n, 2n, 3n, 4n, 5n, 6n, 7n]],
  ] as const)(
    "keeps constructor %s (%s) and requires its leading yield reference",
    (tag, arm, fields) => {
      const encoded = Data.to(new Constr(tag, [42n, ...fields]));
      const decoded = Data.from(encoded, DaAvailabilityMintRedeemer);
      expect(decoded).toMatchObject({
        [arm]: { yield_to_ref_input_index: 42n },
      });
      expect(Data.to(decoded, DaAvailabilityMintRedeemer)).toBe(encoded);
      expect(() =>
        Data.from(
          Data.to(new Constr(tag, [...fields])),
          DaAvailabilityMintRedeemer,
        ),
      ).toThrow();
    },
  );
});

// The fixpoint every publication builder in this package uses (see
// stabilizedMinimumLovelace in src/fraud-proof/field-preimage-carriage.ts):
// min-Ada depends on the output's own size, which depends on its coin field.
const stabilizedRecordMinimumLovelace = (input: {
  readonly address: CML.Address;
  readonly assets: CML.MultiAsset;
  readonly datumCbor: string;
  readonly coinsPerUtxoByte: bigint;
}): bigint => {
  const datum = CML.DatumOption.new_datum(
    CML.PlutusData.from_cbor_hex(input.datumCbor),
  );
  let lovelace = 0n;
  for (let attempt = 0; attempt < 8; attempt += 1) {
    const required = CML.min_ada_required(
      CML.TransactionOutput.new(
        input.address,
        CML.Value.new(lovelace, input.assets),
        datum,
        undefined,
      ),
      input.coinsPerUtxoByte,
    );
    if (required <= lovelace) return lovelace;
    lovelace = required;
  }
  throw new Error("challenge record min-Ada did not stabilise");
};

describe("challenge record min-UTxO (A9)", () => {
  const widest = COMMITMENT_GOLDENS.vectors.find(
    (vector) => vector.label === "tranches_64",
  );
  // The availability policy is also the record's payment script; the record
  // holds exactly one 32-byte DACH token under it.
  const scriptHash = CML.ScriptHash.from_raw_bytes(Buffer.alloc(28, 0xab));
  const recordAssets = (): CML.MultiAsset => {
    const tokens = CML.MapAssetNameToCoin.new();
    tokens.insert(CML.AssetName.from_raw_bytes(Buffer.alloc(32, 0xff)), 1n);
    const assets = CML.MultiAsset.new();
    assets.insert_assets(scriptHash, tokens);
    return assets;
  };
  const recordAddresses = (networkId: number) => {
    const payment = CML.Credential.new_script(scriptHash);
    const stake = CML.Credential.new_pub_key(
      CML.Ed25519KeyHash.from_raw_bytes(Buffer.alloc(28, 0xcd)),
    );
    return [
      [
        "enterprise",
        CML.EnterpriseAddress.new(networkId, payment).to_address(),
      ],
      ["base", CML.BaseAddress.new(networkId, payment, stake).to_address()],
    ] as const;
  };

  it("measures the widest canonical record: 64 tranches, 9-byte times", () => {
    // 64 is Aiken max_tranche_count_safety_v1, the most descriptors a
    // canonical on-chain commitment admits.
    expect(widest).toBeDefined();
    expect(widest!.commitment.trancheDescriptors).toHaveLength(64);
    const record = Data.from(
      widest!.challengeRecordCborHex,
      DaAvailabilityChallengeRecord,
    );
    expect(record.commitment.tranche_descriptors).toHaveLength(64);
    // Both times take the 9-byte CBOR integer form, the widest a timestamp
    // in the 64-bit range can use.
    expect(record.opened_at).toBeGreaterThanOrEqual(2n ** 32n);
    expect(record.response_deadline).toBeGreaterThanOrEqual(2n ** 32n);
  });

  it.each(
    Object.values(DEPLOYMENT_PROFILES).map((profile) => [
      profile.name,
      profile,
    ]),
  )(
    "%s: challenge_record_lovelace covers the widest record at both address shapes",
    (_name, profile) => {
      const recordLovelace = BigInt(profile.da_bond.challenge_record_lovelace);
      const coinsPerUtxoByte = BigInt(profile.limits.coins_per_utxo_byte);
      expect(coinsPerUtxoByte).toBe(4_310n);
      const networkId = profile.network === "Mainnet" ? 1 : 0;
      for (const [, address] of recordAddresses(networkId)) {
        const minimum = stabilizedRecordMinimumLovelace({
          address,
          assets: recordAssets(),
          datumCbor: widest!.challengeRecordCborHex,
          coinsPerUtxoByte,
        });
        expect(minimum).toBeLessThanOrEqual(recordLovelace);
        // The output as OpenChallenge writes it, coin = challenge_record_lovelace.
        expect(
          CML.min_ada_required(
            CML.TransactionOutput.new(
              address,
              CML.Value.new(recordLovelace, recordAssets()),
              CML.DatumOption.new_datum(
                CML.PlutusData.from_cbor_hex(widest!.challengeRecordCborHex),
              ),
              undefined,
            ),
            coinsPerUtxoByte,
          ),
        ).toBeLessThanOrEqual(recordLovelace);
      }
    },
  );
});

describe("availability redeemer field order", () => {
  // Field names and order of Aiken MintRedeemerV1 / SpendRedeemerV1
  // (lib/midgard/availability-challenge.ak); each field i decodes from
  // Constr field i.
  const redeemerArms = [
    [
      DaAvailabilityMintRedeemer,
      0,
      "OpenChallenge",
      [
        "yield_to_ref_input_index",
        "hub_oracle_ref_input_index",
        "record_output_index",
        "challenger_input_index",
        "state_queue_input_index",
        "state_queue_output_index",
        "first_tranche_output_index",
        "terminal_accumulator_output_index",
      ],
      [OWNER],
    ],
    [
      DaAvailabilityMintRedeemer,
      1,
      "SettleTranche",
      [
        "yield_to_ref_input_index",
        "record_ref_input_index",
        "terminal_accumulator_input_index",
        "terminal_accumulator_output_index",
        "tranche_input_index",
      ],
      [new Constr(1, [])],
    ],
    [
      DaAvailabilityMintRedeemer,
      2,
      "CloseChallenge",
      [
        "yield_to_ref_input_index",
        "hub_oracle_ref_input_index",
        "record_input_index",
        "terminal_accumulator_input_index",
        "state_queue_input_index",
        "state_queue_output_index",
        "challenger_refund_output_index",
      ],
      [],
    ],
    [
      DaAvailabilityMintRedeemer,
      3,
      "TimeoutChallenge",
      [
        "yield_to_ref_input_index",
        "hub_oracle_ref_input_index",
        "record_input_index",
        "terminal_accumulator_input_index",
        "state_queue_mint_redeemer_index",
        "pool_input_index",
        "pool_output_index",
        "challenger_refund_output_index",
      ],
      [],
    ],
    [
      DaAvailabilitySpendRedeemer,
      0,
      "AdvanceTranche",
      ["thread_output_index", "carrier_output_index"],
      [new Constr(1, [])],
    ],
    [
      DaAvailabilitySpendRedeemer,
      1,
      "ConsumeCarrier",
      ["thread_input_index", "thread_spend_redeemer_index"],
      [],
    ],
    [DaAvailabilitySpendRedeemer, 2, "Coordinate", ["mint_redeemer_index"], []],
  ] as const;
  it.each(
    redeemerArms.map(([schema, tag, arm, integerFields, trailing]) => ({
      schema,
      tag,
      arm,
      integerFields,
      trailing,
    })),
  )(
    "constructor $tag $arm decodes each integer field from its Aiken position",
    ({ schema, tag, arm, integerFields, trailing }) => {
      const values = integerFields.map((_, index) => BigInt(100 + index));
      const decoded = Data.from(
        Data.to(new Constr(tag, [...values, ...trailing])),
        schema as never,
      ) as Record<string, Record<string, unknown>>;
      const fields = decoded[arm];
      expect(fields).toBeDefined();
      expect(Object.keys(fields!).slice(0, integerFields.length)).toEqual(
        integerFields,
      );
      integerFields.forEach((name, index) => {
        expect(fields![name]).toBe(BigInt(100 + index));
      });
    },
  );
});

describe("deployment:check DA response budget inputs", () => {
  // demo/scripts/deployment-profiles.mjs is plain Node and runs before any
  // package builds, so it restates these two protocol sizes. This pins the
  // copies to their sources: a smaller chunk or a larger small-payload class
  // raises the chained-publication count the budget must cover.
  it("uses the SDK small-payload class and response chunk size", async () => {
    const budget = (await import(
      new URL("../../scripts/deployment-profiles.mjs", import.meta.url).href
    )) as {
      DA_SMALL_PAYLOAD_MAX_BYTES: number;
      DA_RESPONSE_CHUNK_BYTES: number;
      DA_SMALL_PAYLOAD_CHAINED_PUBLICATIONS: number;
    };
    expect(budget.DA_SMALL_PAYLOAD_MAX_BYTES).toBe(
      DA_AVAILABILITY_SMALL_PAYLOAD_MAX_BYTES,
    );
    expect(budget.DA_RESPONSE_CHUNK_BYTES).toBe(
      DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE.chunkByteLength,
    );
    expect(budget.DA_SMALL_PAYLOAD_CHAINED_PUBLICATIONS).toBe(
      Math.ceil(
        DA_AVAILABILITY_SMALL_PAYLOAD_MAX_BYTES /
          DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE.chunkByteLength,
      ),
    );
  });
});
