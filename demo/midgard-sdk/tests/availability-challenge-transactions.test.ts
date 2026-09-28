import {
  CML,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
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
  recoverDaAvailabilityCommitmentFromApplyTx,
  selectDaAvailabilityCollateral,
} from "../src/availability-challenge-transactions.js";
import {
  daAttestationAssetName,
  DaAttestationDatum,
} from "../src/da-attestation.js";
import { STATE_QUEUE_NODE_ASSET_NAME_PREFIX } from "../src/linked-list.js";

const DEPLOYMENT = "11".repeat(28);
const HEADER = "22".repeat(28);
const CHALLENGER = "33".repeat(28);
const OUT_REF = { transactionId: "99".repeat(32), outputIndex: 7n };
const MAX_TIMEOUT_FEE = 1_200_000n;
const GEOMETRY = availabilityResponseGeometry(
  DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
);
const PARAMETERS = daAvailabilityParameters({
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
const BOND = PARAMETERS.da_bond_lovelace;
const PENALTY = PARAMETERS.da_slash_penalty_lovelace;
const FLOOR = PARAMETERS.da_bond_pool_floor_lovelace;
const RECORD = PARAMETERS.challenge_record_lovelace;
const COINS_PER_UTXO_BYTE = 4_310n;
const SCRIPT_ADDRESS = credentialToAddress("Preprod", {
  type: "Script",
  hash: "55".repeat(28),
});
const KEY_ADDRESS = credentialToAddress("Preprod", {
  type: "Key",
  hash: CHALLENGER,
});
const DAAT_POLICY = "66".repeat(28);

const commitment = buildDaAvailabilityCommitment({
  deploymentIdentity: DEPLOYMENT,
  headerHash: HEADER,
  payload: Uint8Array.from({ length: 4_000 }, (_, i) => (i * 17 + 3) % 256),
  responseGeometry: GEOMETRY,
});
const commitmentHash = daAvailabilityCommitmentHash(commitment);

const refusal = (
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

describe("timeout output arithmetic", () => {
  const REMAINING = 9_000_000n;
  const C = 400_000n;
  const conserved = (pool: bigint, c: bigint) => {
    const plan = planDaAvailabilityTimeout({
      poolLovelace: pool,
      remainingChallengerLovelace: REMAINING,
      challengerFeeLovelace: c,
      parameters: PARAMETERS,
    });
    // Record + terminal + pool in == challenger + pool out + fee.
    expect(RECORD + REMAINING + pool).toBe(
      plan.challengerOutputLovelace +
        plan.poolOutputLovelace +
        plan.feeLovelace,
    );
    expect(plan.feeLovelace).toBe(plan.feePart + c);
    expect(plan.challengerOutputLovelace).toBe(
      REMAINING - c + RECORD + plan.payout,
    );
    return plan;
  };

  it("full pool: takes one da_bond, burns the penalty, pays the rest", () => {
    const pool = FLOOR + 3n * BOND;
    const plan = conserved(pool, C);
    expect(plan.taken).toBe(BOND);
    expect(plan.feePart).toBe(PENALTY);
    expect(plan.payout).toBe(BOND - PENALTY);
    expect(plan.poolOutputLovelace).toBe(pool - BOND);
    expect(conserved(pool, 0n).feeLovelace).toBe(PENALTY);
  });

  it("partial pool above the penalty: takes the backing, pays taken - penalty", () => {
    const pool = FLOOR + PENALTY + 7n;
    const plan = conserved(pool, C);
    expect(plan.taken).toBe(PENALTY + 7n);
    expect(plan.feePart).toBe(PENALTY);
    // A dust payout merges into the challenger output; it is not refused.
    expect(plan.payout).toBe(7n);
    expect(plan.poolOutputLovelace).toBe(FLOOR);
  });

  it("partial pool below the penalty: all backing is fee, no payout", () => {
    const pool = FLOOR + PENALTY / 2n;
    const plan = conserved(pool, C);
    expect(plan.taken).toBe(PENALTY / 2n);
    expect(plan.feePart).toBe(PENALTY / 2n);
    expect(plan.payout).toBe(0n);
    expect(plan.poolOutputLovelace).toBe(FLOOR);
  });

  it("empty pool: nothing taken, the challenger pays the whole fee", () => {
    for (const pool of [FLOOR, FLOOR - 1n, 0n]) {
      const plan = conserved(pool, C);
      expect(plan.taken).toBe(0n);
      expect(plan.feePart).toBe(0n);
      expect(plan.payout).toBe(0n);
      expect(plan.poolOutputLovelace).toBe(pool);
      expect(plan.feeLovelace).toBe(C);
    }
    expect(() =>
      planDaAvailabilityTimeout({
        poolLovelace: FLOOR,
        remainingChallengerLovelace: REMAINING,
        challengerFeeLovelace: 0n,
        parameters: PARAMETERS,
      }),
    ).toThrow(/fee must be positive/);
  });

  it("refuses a contribution above the remaining challenger reserve", () => {
    expect(() =>
      planDaAvailabilityTimeout({
        poolLovelace: FLOOR,
        remainingChallengerLovelace: C - 1n,
        challengerFeeLovelace: C,
        parameters: PARAMETERS,
      }),
    ).toThrow(/remaining challenger reserve/);
  });
});

describe("collateral selection", () => {
  const coin = (index: number, lovelace: bigint): UTxO => ({
    txHash: "aa".repeat(32),
    outputIndex: index,
    address: KEY_ADDRESS,
    assets: { lovelace },
  });

  it("takes the largest coins first, at most three", () => {
    const picked = selectDaAvailabilityCollateral({
      candidates: [
        coin(0, 2_000_000n),
        coin(1, 9_000_000n),
        coin(2, 3_000_000n),
      ],
      requiredLovelace: 10_000_000n,
      minimumReturnLovelace: 1_000_000n,
    });
    expect(picked.map((u) => u.outputIndex)).toEqual([1, 2]);
  });

  it("accepts an exact cover and refuses a sub-minimum return", () => {
    expect(
      selectDaAvailabilityCollateral({
        candidates: [coin(0, 5_000_000n)],
        requiredLovelace: 5_000_000n,
        minimumReturnLovelace: 1_000_000n,
      }),
    ).toHaveLength(1);
    refusal(
      () =>
        selectDaAvailabilityCollateral({
          candidates: [coin(0, 5_500_000n)],
          requiredLovelace: 5_000_000n,
          minimumReturnLovelace: 1_000_000n,
        }),
      "collateral-insufficient",
    );
  });

  it("refuses when three coins cannot cover the requirement", () => {
    refusal(
      () =>
        selectDaAvailabilityCollateral({
          candidates: [1, 2, 3, 4].map((i) => coin(i, 2_000_000n)),
          requiredLovelace: 7_000_000n,
          minimumReturnLovelace: 1_000_000n,
        }),
      "collateral-insufficient",
    );
  });
});

describe("recoverDaAvailabilityCommitmentFromApplyTx", () => {
  const attestation = Data.to(
    {
      header_hash: HEADER,
      availability_commitment: commitment,
      da_threshold: 1n,
      committee_signers_hash: "88".repeat(32),
      rescue_beneficiary: {
        paymentCredential: { PublicKeyCredential: [CHALLENGER] },
        stakeCredential: null,
      },
      attested_signers: "00".repeat(32),
      attestation_count: 1n,
    },
    DaAttestationDatum,
  );
  const unit = { policy: DAAT_POLICY, name: daAttestationAssetName(HEADER) };
  const address = CML.Address.from_bech32(SCRIPT_ADDRESS);
  const input = (txHash: string, index: bigint) =>
    CML.TransactionInput.new(CML.TransactionHash.from_hex(txHash), index);
  const tx = (
    inputs: readonly CML.TransactionInput[],
    outputs: readonly CML.TransactionOutput[],
    mint?: bigint,
    witnessDatum?: string,
  ) => {
    const inputList = CML.TransactionInputList.new();
    for (const i of inputs) inputList.add(i);
    const outputList = CML.TransactionOutputList.new();
    for (const o of outputs) outputList.add(o);
    const body = CML.TransactionBody.new(inputList, outputList, 200_000n);
    if (mint !== undefined) {
      const m = CML.Mint.new();
      m.set(
        CML.ScriptHash.from_hex(unit.policy),
        CML.AssetName.from_hex(unit.name),
        mint,
      );
      body.set_mint(m);
    }
    const witnesses = CML.TransactionWitnessSet.new();
    if (witnessDatum !== undefined) {
      const datums = CML.PlutusDataList.new();
      datums.add(CML.PlutusData.from_cbor_hex(witnessDatum));
      witnesses.set_plutus_datums(datums);
    }
    const t = CML.Transaction.new(body, witnesses, true);
    return {
      cbor: t.to_cbor_hex(),
      hash: CML.hash_transaction(t.body()).to_hex(),
    };
  };
  const daatOutput = (hashed: boolean) => {
    const assets = CML.MultiAsset.new();
    assets.set(
      CML.ScriptHash.from_hex(unit.policy),
      CML.AssetName.from_hex(unit.name),
      1n,
    );
    const data = CML.PlutusData.from_cbor_hex(attestation);
    return CML.TransactionOutput.new(
      address,
      CML.Value.new(5_000_000n, assets),
      hashed
        ? CML.DatumOption.new_hash(CML.hash_plutus_data(data))
        : CML.DatumOption.new_datum(data),
    );
  };
  const plain = () =>
    CML.TransactionOutput.new(address, CML.Value.from_coin(2_000_000n));
  const scenario = (options: {
    hashed?: boolean;
    witnessDatum?: boolean;
    burn?: bigint;
  }) => {
    // The DAAT sits at output 1 of its producing transaction.
    const producer = tx(
      [input("ab".repeat(32), 0n)],
      [plain(), daatOutput(options.hashed ?? false)],
      1n,
    );
    const apply = tx(
      [input("cd".repeat(32), 3n), input(producer.hash, 1n)],
      [plain()],
      options.burn === 0n ? undefined : (options.burn ?? -1n),
      options.witnessDatum ? attestation : undefined,
    );
    const store = new Map([
      [producer.hash, producer.cbor],
      [apply.hash, apply.cbor],
    ]);
    return { producer, apply, store };
  };
  const lucid = { config: () => ({}) } as unknown as Pick<
    LucidEvolution,
    "config"
  >;
  const recover = (
    s: ReturnType<typeof scenario>,
    overrides: Partial<
      Parameters<typeof recoverDaAvailabilityCommitmentFromApplyTx>[1]
    > = {},
  ) =>
    recoverDaAvailabilityCommitmentFromApplyTx(lucid, {
      applyTxHash: s.apply.hash,
      daAttestationPolicyId: DAAT_POLICY,
      fetchTransactionCbor: async (hash) => s.store.get(hash),
      ...overrides,
    });
  const rejects = async (
    promise: Promise<unknown>,
    reason: DaAvailabilityTransactionErrorReason,
  ) => {
    const caught = await promise.then(
      () => undefined,
      (cause: unknown) => cause,
    );
    expect(caught).toBeInstanceOf(DaAvailabilityTransactionError);
    expect((caught as DaAvailabilityTransactionError).reason).toBe(reason);
  };

  it("reads the spent DAAT output's inline datum from its producing transaction", async () => {
    const s = scenario({});
    const recovered = await recover(s, {
      expectedCommitmentHash: commitmentHash,
    });
    expect(recovered.source).toBe("inline-datum");
    expect(recovered.commitmentHash).toBe(commitmentHash);
    expect(recovered.headerHash).toBe(HEADER);
    expect(recovered.attestationOutRef).toEqual({
      txHash: s.producer.hash,
      outputIndex: 1,
    });
    expect(daAvailabilityCommitmentHash(recovered.commitment)).toBe(
      commitmentHash,
    );
  });

  it("falls back to the Apply witness datums for a hashed DAAT datum", async () => {
    const recovered = await recover(
      scenario({ hashed: true, witnessDatum: true }),
    );
    expect(recovered.source).toBe("witness-datum");
    expect(recovered.commitmentHash).toBe(commitmentHash);
  });

  it("refuses a hashed DAAT datum nothing resolves", async () => {
    await rejects(
      recover(scenario({ hashed: true })),
      "apply-commitment-unrecoverable",
    );
  });

  it("refuses a commitment other than the expected one", async () => {
    await rejects(
      recover(scenario({}), { expectedCommitmentHash: "45".repeat(32) }),
      "commitment-hash-mismatch",
    );
  });

  it("refuses an Apply transaction that burns no DAAT", async () => {
    await rejects(
      recover(scenario({ burn: 0n })),
      "apply-commitment-unrecoverable",
    );
  });

  it("refuses fetched bytes that do not hash to the requested transaction", async () => {
    const s = scenario({});
    const other = scenario({ hashed: true });
    await rejects(
      recover(s, { fetchTransactionCbor: async () => other.apply.cbor }),
      "apply-commitment-unrecoverable",
    );
  });

  it("refuses when no producing transaction is available", async () => {
    const s = scenario({});
    await rejects(
      recover(s, {
        fetchTransactionCbor: async (hash) =>
          hash === s.apply.hash ? s.apply.cbor : undefined,
      }),
      "apply-commitment-unrecoverable",
    );
  });
});
