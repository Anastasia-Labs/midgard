import { credentialToAddress } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  availabilityResponseGeometry,
  buildDaAvailabilityChallengeDatumPlan,
  buildDaAvailabilityCommitment,
  DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE,
  DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
  daAvailabilityCommitmentHash,
  daAvailabilityParameters,
  encodeDaAvailabilityChallengeRecord,
} from "../src/availability-challenge.js";
import {
  assertDaAvailabilityChallengeRecordMinAda,
  assertDaAvailabilityOpenCommitment,
  assertDaAvailabilityOpenWithinChallengeWindow,
  daAvailabilityTimeoutChallengerFee,
  DaAvailabilityTransactionError,
  type DaAvailabilityTransactionErrorReason,
  planDaAvailabilityTimeout,
} from "../src/availability-challenge-transactions.js";
import { STATE_QUEUE_NODE_ASSET_NAME_PREFIX } from "../src/linked-list.js";

const DEPLOYMENT = "11".repeat(28);

export const HEADER = "22".repeat(28);

export const CHALLENGER = "33".repeat(28);

const OUT_REF = { transactionId: "99".repeat(32), outputIndex: 7n };

const MAX_TIMEOUT_FEE = 1_200_000n;

const GEOMETRY = availabilityResponseGeometry(
  DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
);

export const PARAMETERS = daAvailabilityParameters({
  responseGeometry: GEOMETRY,
  ...DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  challengerBondLovelace:
    DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE,
  maxOpenFeeLovelace: 500_000n,
  maxPublicationFeeLovelace: 500_000n,
  maxSettlementFeeLovelace: 500_000n,
  maxCloseFeeLovelace: 1_000_000n,
  maxTimeoutFeeLovelace: MAX_TIMEOUT_FEE,
});

export const BOND = PARAMETERS.da_bond_lovelace;

export const PENALTY = PARAMETERS.da_slash_penalty_lovelace;

export const FLOOR = PARAMETERS.da_bond_pool_floor_lovelace;

export const RECORD = PARAMETERS.challenge_record_lovelace;

const COINS_PER_UTXO_BYTE = 4_310n;

export const SCRIPT_ADDRESS = credentialToAddress("Preprod", {
  type: "Script",
  hash: "55".repeat(28),
});

export const KEY_ADDRESS = credentialToAddress("Preprod", {
  type: "Key",
  hash: CHALLENGER,
});

export const DAAT_POLICY = "66".repeat(28);

export const commitment = buildDaAvailabilityCommitment({
  deploymentIdentity: DEPLOYMENT,
  headerHash: HEADER,
  payload: Uint8Array.from({ length: 4_000 }, (_, i) => (i * 17 + 3) % 256),
  responseGeometry: GEOMETRY,
});

export const commitmentHash = daAvailabilityCommitmentHash(commitment);

export const refusal = (
  run: () => unknown,
  reason: DaAvailabilityTransactionErrorReason,
) => {
  let caught: unknown;
  try {
    run();
  } catch (cause) {
    caught = cause;
  }
  expect(caught).toBeInstanceOf(DaAvailabilityTransactionError);
  expect((caught as DaAvailabilityTransactionError).reason).toBe(reason);
};

describe("Open commitment binding", () => {
  const open = (
    status: Parameters<typeof assertDaAvailabilityOpenCommitment>[0]["status"],
  ) =>
    assertDaAvailabilityOpenCommitment({
      commitment,
      deploymentIdentity: DEPLOYMENT,
      queueAssetName: STATE_QUEUE_NODE_ASSET_NAME_PREFIX + HEADER,
      status,
      parameters: PARAMETERS,
    });

  it("accepts the commitment that hashes to Attested{commitment_hash}", () => {
    expect(open({ Attested: { commitment_hash: commitmentHash } })).toBe(
      commitmentHash,
    );
  });

  it("refuses a commitment whose hash differs from the attested one", () => {
    refusal(
      () => open({ Attested: { commitment_hash: "44".repeat(32) } }),
      "commitment-hash-mismatch",
    );
  });

  it("refuses a node that is not Attested, another deployment or another block", () => {
    expect(() => open("Unattested")).toThrow(/Attested queue node/);
    expect(() =>
      assertDaAvailabilityOpenCommitment({
        commitment,
        deploymentIdentity: "12".repeat(28),
        queueAssetName: STATE_QUEUE_NODE_ASSET_NAME_PREFIX + HEADER,
        status: { Attested: { commitment_hash: commitmentHash } },
        parameters: PARAMETERS,
      }),
    ).toThrow(/another deployment/);
    expect(() =>
      assertDaAvailabilityOpenCommitment({
        commitment,
        deploymentIdentity: DEPLOYMENT,
        queueAssetName: STATE_QUEUE_NODE_ASSET_NAME_PREFIX + "23".repeat(28),
        status: { Attested: { commitment_hash: commitmentHash } },
        parameters: PARAMETERS,
      }),
    ).toThrow(/commitment's block/);
  });
});

describe("Open challenge window", () => {
  it("admits an inclusive upper bound strictly before end_time + window", () => {
    expect(() =>
      assertDaAvailabilityOpenWithinChallengeWindow({
        validTo: 1_000n + 600n,
        nodeEndTime: 1_000n,
        daChallengeWindowMs: 600n,
      }),
    ).not.toThrow();
  });

  it("refuses an inclusive upper bound at end_time + window", () => {
    refusal(
      () =>
        assertDaAvailabilityOpenWithinChallengeWindow({
          validTo: 1_000n + 600n + 1n,
          nodeEndTime: 1_000n,
          daChallengeWindowMs: 600n,
        }),
      "challenge-window-closed",
    );
  });
});

describe("challenge record min-ADA", () => {
  const plan = buildDaAvailabilityChallengeDatumPlan({
    commitment,
    challengerFundingOutRef: OUT_REF,
    challenger: CHALLENGER,
    openedAt: 1_000_000n,
    parameters: PARAMETERS,
  });
  const record = {
    address: SCRIPT_ADDRESS,
    assets: {
      lovelace: plan.recordLovelace,
      ["77".repeat(28) + plan.challengeAssetName]: 1n,
    },
    datum: encodeDaAvailabilityChallengeRecord(plan.record, PARAMETERS),
  };

  it("holds exactly challenge_record_lovelace, above the live floor", () => {
    expect(plan.recordLovelace).toBe(RECORD);
    expect(() =>
      assertDaAvailabilityChallengeRecordMinAda({
        coinsPerUtxoByte: COINS_PER_UTXO_BYTE,
        record,
        challengeRecordLovelace: RECORD,
      }),
    ).not.toThrow();
  });

  it("refuses when the live floor exceeds challenge_record_lovelace", () => {
    refusal(
      () =>
        assertDaAvailabilityChallengeRecordMinAda({
          coinsPerUtxoByte: COINS_PER_UTXO_BYTE * 100n,
          record,
          challengeRecordLovelace: RECORD,
        }),
      "record-min-ada",
    );
  });

  it("refuses a record that is not exactly challenge_record_lovelace", () => {
    expect(() =>
      assertDaAvailabilityChallengeRecordMinAda({
        coinsPerUtxoByte: COINS_PER_UTXO_BYTE,
        record: {
          ...record,
          assets: { ...record.assets, lovelace: RECORD + 1n },
        },
        challengeRecordLovelace: RECORD,
      }),
    ).toThrow(/exactly challenge_record_lovelace/);
  });
});

describe("timeout challenger fee cap", () => {
  it("needs no contribution when the slashed part covers the ledger fee", () => {
    expect(
      daAvailabilityTimeoutChallengerFee({
        feePartLovelace: PENALTY,
        requiredFeeLovelace: 2_000_000n,
        parameters: PARAMETERS,
      }),
    ).toBe(0n);
  });

  it("returns the shortfall up to and including the cap", () => {
    expect(
      daAvailabilityTimeoutChallengerFee({
        feePartLovelace: 300_000n,
        requiredFeeLovelace: 300_000n + MAX_TIMEOUT_FEE,
        parameters: PARAMETERS,
      }),
    ).toBe(MAX_TIMEOUT_FEE);
  });

  it("refuses a contribution above max_timeout_fee", () => {
    refusal(
      () =>
        daAvailabilityTimeoutChallengerFee({
          feePartLovelace: 0n,
          requiredFeeLovelace: MAX_TIMEOUT_FEE + 1n,
          parameters: PARAMETERS,
        }),
      "timeout-challenger-fee-cap",
    );
    refusal(
      () =>
        planDaAvailabilityTimeout({
          poolLovelace: FLOOR + 3n * BOND,
          remainingChallengerLovelace: 10_000_000n,
          challengerFeeLovelace: MAX_TIMEOUT_FEE + 1n,
          parameters: PARAMETERS,
        }),
      "timeout-challenger-fee-cap",
    );
    refusal(
      () =>
        planDaAvailabilityTimeout({
          poolLovelace: FLOOR + 3n * BOND,
          remainingChallengerLovelace: 10_000_000n,
          challengerFeeLovelace: -1n,
          parameters: PARAMETERS,
        }),
      "timeout-challenger-fee-cap",
    );
  });
});
