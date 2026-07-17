import { Kupmios } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

const baseProtocolParameters = {
  minFeeCoefficient: 44,
  minFeeReferenceScripts: {
    base: 15,
    range: 25_600,
    multiplier: 1.2,
  },
  stakePoolVotingThresholds: {
    noConfidence: "51/100",
    constitutionalCommittee: {
      default: "51/100",
      stateOfNoConfidence: "51/100",
    },
    hardForkInitiation: "51/100",
    protocolParametersUpdate: { security: "51/100" },
  },
  delegateRepresentativeVotingThresholds: {
    noConfidence: "2/3",
    constitutionalCommittee: {
      default: "2/3",
      stateOfNoConfidence: "2/3",
    },
    constitution: "2/3",
    hardForkInitiation: "2/3",
    protocolParametersUpdate: {
      network: "2/3",
      economic: "2/3",
      technical: "2/3",
      governance: "2/3",
    },
    treasuryWithdrawals: "2/3",
  },
  constitutionalCommitteeMinSize: 0,
  constitutionalCommitteeMaxTermLength: 146,
  governanceActionLifetime: 6,
  governanceActionDeposit: { ada: { lovelace: 100_000_000_000 } },
  delegateRepresentativeDeposit: { ada: { lovelace: 500_000_000 } },
  delegateRepresentativeMaxIdleTime: 20,
  minFeeConstant: { ada: { lovelace: 155_381 } },
  maxBlockBodySize: { bytes: 90_112 },
  maxBlockHeaderSize: { bytes: 1_100 },
  maxTransactionSize: { bytes: 16_384 },
  stakeCredentialDeposit: { ada: { lovelace: 2_000_000 } },
  stakePoolDeposit: { ada: { lovelace: 500_000_000 } },
  stakePoolRetirementEpochBound: 18,
  desiredNumberOfStakePools: 500,
  stakePoolPledgeInfluence: "3/10",
  monetaryExpansion: "3/1000",
  treasuryExpansion: "1/5",
  minStakePoolCost: { ada: { lovelace: 340_000_000 } },
  minUtxoDepositConstant: { ada: { lovelace: 0 } },
  minUtxoDepositCoefficient: 4_310,
  plutusCostModels: {
    "plutus:v1": [1],
    "plutus:v2": [2],
    "plutus:v3": [3],
  },
  scriptExecutionPrices: { memory: "577/10000", cpu: "721/10000000" },
  maxExecutionUnitsPerTransaction: { memory: 14_000_000, cpu: 10_000_000_000 },
  maxExecutionUnitsPerBlock: { memory: 62_000_000, cpu: 20_000_000_000 },
  maxValueSize: { bytes: 5_000 },
  collateralPercentage: 150,
  maxCollateralInputs: 3,
  version: { major: 10, minor: 0 },
} as const;

const responseWith = (
  referenceScriptSize: Readonly<Record<string, unknown>>,
) => ({
  jsonrpc: "2.0",
  method: "queryLedgerState/protocolParameters",
  id: null,
  result: { ...baseProtocolParameters, ...referenceScriptSize },
});

const protocolParametersFrom = async (payload: unknown) => {
  vi.stubGlobal(
    "fetch",
    vi.fn(async () =>
      Response.json(payload, {
        status: 200,
        headers: { "content-type": "application/json" },
      }),
    ),
  );
  return new Kupmios(
    "http://127.0.0.1:1442",
    "http://127.0.0.1:1337",
  ).getProtocolParameters();
};

describe("Kupmios canonical Ogmios v7 protocol parameters", () => {
  afterEach(() => {
    vi.unstubAllGlobals();
  });

  it("accepts the canonical maxReferenceScriptsSizePerTransaction field", async () => {
    await expect(
      protocolParametersFrom(
        responseWith({
          maxReferenceScriptsSizePerTransaction: { bytes: 204_800 },
        }),
      ),
    ).resolves.toMatchObject({
      minFeeA: 44,
      minFeeB: 155_381,
      maxTxSize: 16_384,
      minFeeRefScriptCostPerByte: 15,
    });
  });

  it("normalizes the legacy Ogmios v6 field", async () => {
    await expect(
      protocolParametersFrom(
        responseWith({ maxReferenceScriptsSize: { bytes: 204_800 } }),
      ),
    ).resolves.toMatchObject({
      minFeeA: 44,
      minFeeB: 155_381,
      maxTxSize: 16_384,
      minFeeRefScriptCostPerByte: 15,
    });
  });

  it("fails closed when legacy and canonical fields conflict", async () => {
    await expect(
      protocolParametersFrom(
        responseWith({
          maxReferenceScriptsSize: { bytes: 102_400 },
          maxReferenceScriptsSizePerTransaction: { bytes: 204_800 },
        }),
      ),
    ).rejects.toThrow(/Conflicting Ogmios protocol parameters/);
  });

  it("fails closed when the canonical field is missing", async () => {
    await expect(protocolParametersFrom(responseWith({}))).rejects.toThrow(
      /maxReferenceScriptsSize/,
    );
  });

  it("retains numeric validation for the canonical field", async () => {
    await expect(
      protocolParametersFrom(
        responseWith({
          maxReferenceScriptsSizePerTransaction: { bytes: "204800" },
        }),
      ),
    ).rejects.toThrow(/Expected number/);
  });
});
