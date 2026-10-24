import "./availability-challenge-transactions.timeout-output-arithmetic.js";

import { CML, Data, type LucidEvolution } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { daAvailabilityCommitmentHash } from "../src/availability-challenge.js";
import {
  DaAvailabilityTransactionError,
  type DaAvailabilityTransactionErrorReason,
  recoverDaAvailabilityCommitmentFromApplyTx,
} from "../src/availability-challenge-transactions.js";
import {
  daAttestationAssetName,
  DaAttestationDatum,
} from "../src/da-attestation.js";
import {
  CHALLENGER,
  commitment,
  commitmentHash,
  DAAT_POLICY,
  HEADER,
  SCRIPT_ADDRESS,
} from "./availability-challenge-transactions.challenge-record-min-ada.js";

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
