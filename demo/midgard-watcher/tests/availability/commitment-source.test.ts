import type { FraudProofRawL1Transaction } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { recoverWatcherAttestedCommitment } from "../../src/availability/commitment-source.js";
import { refusalError } from "../../src/l1-follower/fault-proof-l1-source.chain.js";

const HEADER = "22".repeat(28);
const QUEUE_POLICY = "55".repeat(28);
const DAAT_POLICY = "66".repeat(28);
const GEOMETRY = SDK.availabilityResponseGeometry(
  SDK.DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
);
const commitment = SDK.buildDaAvailabilityCommitment({
  deploymentIdentity: "11".repeat(28),
  headerHash: HEADER,
  payload: Uint8Array.from({ length: 4_000 }, (_, i) => (i * 17 + 3) % 256),
  responseGeometry: GEOMETRY,
});
const commitmentHash = SDK.daAvailabilityCommitmentHash(commitment);
const address = CML.Address.from_bech32(
  credentialToAddress("Preprod", { type: "Script", hash: "77".repeat(28) }),
);
const attestation = Data.to(
  {
    header_hash: HEADER,
    availability_commitment: commitment,
    da_threshold: 1n,
    committee_signers_hash: "88".repeat(32),
    rescue_beneficiary: {
      paymentCredential: { PublicKeyCredential: ["33".repeat(28)] },
      stakeCredential: null,
    },
    attested_signers: "00".repeat(32),
    attestation_count: 1n,
  },
  SDK.DaAttestationDatum,
);
const DAAT_NAME = SDK.daAttestationAssetName(HEADER);

const rawTransaction = (
  inputs: readonly (readonly [string, bigint])[],
  outputs: readonly CML.TransactionOutput[],
  daatMint?: bigint,
): FraudProofRawL1Transaction => {
  const inputList = CML.TransactionInputList.new();
  for (const [txHash, index] of inputs)
    inputList.add(
      CML.TransactionInput.new(CML.TransactionHash.from_hex(txHash), index),
    );
  const outputList = CML.TransactionOutputList.new();
  for (const output of outputs) outputList.add(output);
  const body = CML.TransactionBody.new(inputList, outputList, 200_000n);
  if (daatMint !== undefined) {
    const mint = CML.Mint.new();
    mint.set(
      CML.ScriptHash.from_hex(DAAT_POLICY),
      CML.AssetName.from_hex(DAAT_NAME),
      daatMint,
    );
    body.set_mint(mint);
  }
  return {
    txHash: CML.hash_transaction(body).to_hex(),
    bodyCbor: body.to_cbor_hex(),
    witnessSetCbor: CML.TransactionWitnessSet.new().to_cbor_hex(),
    redeemersCbor: null,
    isValid: true,
    inclusionPoint: {} as FraudProofRawL1Transaction["inclusionPoint"],
    confirmationDepth: 30,
    resolvedInputs: [],
    resolvedReferenceInputs: [],
  };
};
const plain = () =>
  CML.TransactionOutput.new(address, CML.Value.from_coin(2_000_000n));
const daatOutput = () => {
  const assets = CML.MultiAsset.new();
  assets.set(
    CML.ScriptHash.from_hex(DAAT_POLICY),
    CML.AssetName.from_hex(DAAT_NAME),
    1n,
  );
  return CML.TransactionOutput.new(
    address,
    CML.Value.new(5_000_000n, assets),
    CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(attestation)),
  );
};

/**
 * Init produces the DAAT output (outside the queue node's unit history); the
 * queue node's history holds its Append and the Apply that burns the DAAT.
 */
const scenario = (applies = 1) => {
  const init = rawTransaction(
    [["ab".repeat(32), 0n]],
    [plain(), daatOutput()],
    1n,
  );
  const append = rawTransaction([["cd".repeat(32), 0n]], [plain()]);
  const applyTransactions = Array.from({ length: applies }, (_, index) =>
    rawTransaction(
      [
        [append.txHash, 0n],
        [init.txHash, 1n],
        ["ef".repeat(32), BigInt(index)],
      ],
      [plain()],
      -1n,
    ),
  );
  const readHistory = vi.fn(async (_unit: string) => [
    append,
    ...applyTransactions,
  ]);
  const readTransaction = vi.fn(async (txHash: string) =>
    txHash === init.txHash ? init : undefined,
  );
  return { init, readHistory, readTransaction };
};
const recover = (
  s: ReturnType<typeof scenario>,
  expectedCommitmentHash: string,
) =>
  recoverWatcherAttestedCommitment({
    headerHash: HEADER,
    expectedCommitmentHash,
    stateQueuePolicyId: QUEUE_POLICY,
    daAttestationPolicyId: DAAT_POLICY,
    lucid: { config: () => ({}) } as unknown as Pick<LucidEvolution, "config">,
    readHistory: s.readHistory,
    readTransaction: s.readTransaction,
  });
const refusal = async (promise: Promise<unknown>) => {
  const caught = await promise.then(
    () => undefined,
    (cause: unknown) => cause,
  );
  expect(caught).toBeInstanceOf(SDK.DaAvailabilityTransactionError);
  return (caught as SDK.DaAvailabilityTransactionError).reason;
};

describe("watcher Attested commitment recovery (spec #685 E1)", () => {
  it("recovers the attested commitment from the node's canonical Apply", async () => {
    const s = scenario();
    const recovered = await recover(s, commitmentHash);
    expect(s.readHistory).toHaveBeenCalledWith(
      QUEUE_POLICY + SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + HEADER,
    );
    expect(recovered.headerHash).toBe(HEADER);
    expect(recovered.commitmentHash).toBe(commitmentHash);
    expect(recovered.attestationOutRef).toEqual({
      txHash: s.init.txHash,
      outputIndex: 1,
    });
    expect(SDK.daAvailabilityCommitmentHash(recovered.commitment)).toBe(
      commitmentHash,
    );
  });

  // Apply's fee input comes from the submitter's untracked wallet: once the
  // follower has pruned, it cannot say whether that input's producer landed
  // inside the pruned window, so reading the producer is refused. Apply's
  // own resolved inputs already show the output holds no DAAT token.
  it("recovers when a fee input before the DAAT input has an unreadable producer", async () => {
    const init = rawTransaction(
      [["ab".repeat(32), 0n]],
      [plain(), daatOutput()],
      1n,
    );
    const append = rawTransaction([["cd".repeat(32), 0n]], [plain()]);
    const fee = "01".repeat(32);
    const resolved = (outRef: string, output: CML.TransactionOutput) => ({
      outRef,
      outputCbor: output.to_cbor_hex(),
      datumCbor: null,
      referenceScriptCbor: null,
    });
    const apply = {
      ...rawTransaction(
        [
          [fee, 0n],
          [append.txHash, 0n],
          [init.txHash, 1n],
        ],
        [plain()],
        -1n,
      ),
      resolvedInputs: [
        resolved(`${fee}#0`, plain()),
        resolved(`${append.txHash}#0`, plain()),
        resolved(`${init.txHash}#1`, daatOutput()),
      ],
    };
    const recovered = await recover(
      {
        init,
        readHistory: vi.fn(async () => [append, apply]),
        readTransaction: vi.fn(async (txHash: string) => {
          if (txHash === init.txHash) return init;
          throw refusalError(
            "beyond_retention",
            `${txHash} is not stored and may have landed at or below slot 1`,
          );
        }),
      },
      commitmentHash,
    );
    expect(recovered.commitmentHash).toBe(commitmentHash);
    expect(recovered.attestationOutRef).toEqual({
      txHash: init.txHash,
      outputIndex: 1,
    });
  });

  // One producer made both the DAAT output Apply spends and a plain input
  // Apply spends after it: it must still be read for the DAAT output.
  it("recovers when the DAAT input's producer also made a plain Apply input", async () => {
    const init = rawTransaction(
      [["ab".repeat(32), 0n]],
      [daatOutput(), plain()],
      1n,
    );
    const append = rawTransaction([["cd".repeat(32), 0n]], [plain()]);
    const resolved = (outRef: string, output: CML.TransactionOutput) => ({
      outRef,
      outputCbor: output.to_cbor_hex(),
      datumCbor: null,
      referenceScriptCbor: null,
    });
    const apply = {
      ...rawTransaction(
        [
          [append.txHash, 0n],
          [init.txHash, 0n],
          [init.txHash, 1n],
        ],
        [plain()],
        -1n,
      ),
      resolvedInputs: [
        resolved(`${append.txHash}#0`, plain()),
        resolved(`${init.txHash}#0`, daatOutput()),
        resolved(`${init.txHash}#1`, plain()),
      ],
    };
    const recovered = await recover(
      {
        init,
        readHistory: vi.fn(async () => [append, apply]),
        readTransaction: vi.fn(async (txHash: string) =>
          txHash === init.txHash ? init : undefined,
        ),
      },
      commitmentHash,
    );
    expect(recovered.commitmentHash).toBe(commitmentHash);
    expect(recovered.attestationOutRef).toEqual({
      txHash: init.txHash,
      outputIndex: 0,
    });
  });

  it("refuses a commitment that does not hash to the node's commitment_hash", async () => {
    await expect(refusal(recover(scenario(), "99".repeat(32)))).resolves.toBe(
      "commitment-hash-mismatch",
    );
  });

  it.each([0, 2])(
    "refuses a history with %i canonical Apply transactions",
    async (applies) => {
      await expect(
        refusal(recover(scenario(applies), commitmentHash)),
      ).resolves.toBe("apply-commitment-unrecoverable");
    },
  );
});
