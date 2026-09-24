import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  Data,
  datumToHash,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  mintingPolicyToId,
  scriptFromNative,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { Columns } from "../src/database/depositSubmissionAttempts.js";
import {
  depositSubmissionAttemptFromCompletedTx,
  matchesDepositSubmissionIntent,
} from "../src/transactions/submit-deposit.js";

/** Signed-body journal parsing only; the native NFT is not history policy acceptance evidence. */
const fixture = async () => {
  const wallet = generateEmulatorAccount({ lovelace: 100_000_000n });
  const lucid = await Lucid(new Emulator([wallet]), "Custom");
  lucid.selectWallet.fromSeed(wallet.seedPhrase);
  const owner = getAddressDetails(wallet.address).paymentCredential!.hash;
  const policy = scriptFromNative({ type: "sig", keyHash: owner });
  const policyId = mintingPolicyToId(policy);
  const nonce = (await lucid.wallet().getUtxos())[0]!;
  const id = {
    transactionId: nonce.txHash,
    outputIndex: BigInt(nonce.outputIndex),
  };
  const eventId = Data.to(id, SDK.OutputReference);
  const key = datumToHash(eventId);
  const unit = policyId + key;
  const node: SDK.EventHistoryNode = {
    position: { Key: [key] },
    next: null,
    protected_until: 2000n,
    payload: {
      Order: {
        facts: {
          event_id: id,
          inclusion_time: 1000n,
          structural_lovelace: 2_000_000n,
          structural_refund_key: owner,
          location: {
            Inline: {
              payload: {
                DepositPayload: {
                  event: {
                    id,
                    info: {
                      l2_address: Effect.runSync(
                        SDK.addressDataFromBech32(wallet.address),
                      ),
                      l2_network_id: 0n,
                      l2_datum: null,
                    },
                  },
                },
              },
            },
          },
        },
      },
    },
  };
  const signed = await (
    await lucid
      .newTx()
      .collectFrom([nonce])
      .mintAssets({ [unit]: 1n })
      .attach.MintingPolicy(policy)
      .pay.ToAddressWithData(
        wallet.address,
        { kind: "inline", value: Data.to(node, SDK.EventHistoryNode) },
        { lovelace: 7_000_000n, [unit]: 1n },
      )
      .complete({ localUPLCEval: true })
  ).sign
    .withWallet()
    .complete();
  const order = {
    ...coreToTxOutput(
      CML.Transaction.from_cbor_hex(signed.toCBOR()).body().outputs().get(0),
    ),
    txHash: signed.toHash(),
    outputIndex: 0,
  };
  // Reader fixture only: this synthetic root is not policy acceptance evidence.
  const [deposit] = await Effect.runPromise(
    SDK.utxosToDepositUTxOs(
      [
        {
          txHash: "ee".repeat(32),
          outputIndex: 0,
          address: wallet.address,
          assets: { lovelace: 3_000_000n, [policyId]: 1n },
          datum: Data.to(
            {
              position: "Root",
              next: key,
              protected_until: 0n,
              payload: "RootContent",
            },
            SDK.EventHistoryNode,
          ),
        },
        order,
      ],
      [],
      {
        address: wallet.address,
        policyId,
        retentionAddress: wallet.address,
        inlineLimitBytes: 512n,
      },
    ),
  );
  if (deposit === undefined) throw new Error("Missing reader fixture");
  return {
    deposit,
    txHash: signed.toHash(),
    transactionCbor: signed.toCBOR(),
    metadata: {
      depositAddress: wallet.address,
      depositEventId: eventId,
      depositAssetName: key,
      depositAuthUnit: unit,
      nonceInput: nonce,
      validTo: 1,
      inclusionTime: 1000,
      structuralLovelace: 2_000_000n,
      orderOutputIndex: 0,
    },
    config: {
      l2Address: wallet.address,
      l2Datum: null,
      lovelace: 5_000_000n,
      additionalAssets: {},
    },
  };
};

describe("history deposit submission journal", () => {
  it("matches submission intent after a pointer continuation, independently of original transaction hash", async () => {
    const input = await fixture();
    const attempt = depositSubmissionAttemptFromCompletedTx(input);
    expect(attempt[Columns.METADATA].l2DatumCbor).toBeNull();
    expect(
      matchesDepositSubmissionIntent(input.deposit, attempt, "Custom"),
    ).toBe(true);
    const moved = {
      ...input.deposit,
      utxo: { ...input.deposit.utxo, txHash: "99".repeat(32), outputIndex: 9 },
    };
    expect(matchesDepositSubmissionIntent(moved, attempt, "Custom")).toBe(true);
  });
  it("refuses event content, timing, network and original Value drift", async () => {
    const input = await fixture();
    const attempt = depositSubmissionAttemptFromCompletedTx(input);
    const deposit = input.deposit;
    for (const changed of [
      {
        ...deposit,
        facts: {
          ...deposit.facts,
          inclusion_time: deposit.facts.inclusion_time + 1n,
        },
      },
      { ...deposit, facts: { ...deposit.facts, structural_lovelace: 0n } },
      { ...deposit, originalAssets: { lovelace: 7_000_000n } },
      {
        ...deposit,
        event: {
          ...deposit.event,
          info: { ...deposit.event.info, l2_datum: 0n },
        },
      },
      {
        ...deposit,
        event: {
          ...deposit.event,
          info: { ...deposit.event.info, l2_network_id: 1n },
        },
      },
    ])
      expect(matchesDepositSubmissionIntent(changed, attempt, "Custom")).toBe(
        false,
      );
    const otherAddress = generateEmulatorAccount({
      lovelace: 1_000_000n,
    }).address;
    expect(
      matchesDepositSubmissionIntent(
        deposit,
        { ...attempt, [Columns.EXPECTED_L2_ADDRESS]: otherAddress },
        "Custom",
      ),
    ).toBe(false);
  });

  it("records only original Value while preserving structural funding metadata", async () => {
    const input = await fixture();
    const attempt = depositSubmissionAttemptFromCompletedTx(input);
    expect(attempt[Columns.EXPECTED_ASSETS]).toEqual({ lovelace: "5000000" });
    expect(attempt[Columns.METADATA].structuralLovelace).toBe("2000000");
    expect(attempt[Columns.METADATA].transactionCbor).toBe(
      input.transactionCbor,
    );
    expect(attempt[Columns.EXPECTED_DEPOSIT_OUT_REF]).toBe(`${input.txHash}#0`);
    expect(attempt[Columns.DEPOSIT_EVENT_ID].toString("hex")).toBe(
      input.metadata.depositEventId,
    );
  });

  it("captures the exact completed transaction before any wallet signature", async () => {
    const input = await fixture();
    const signed = CML.Transaction.from_cbor_hex(input.transactionCbor);
    const witnesses = signed.witness_set();
    witnesses.set_vkeywitnesses(CML.VkeywitnessList.new());
    const unsigned = CML.Transaction.new(
      signed.body(),
      witnesses,
      signed.is_valid(),
      signed.auxiliary_data(),
    );
    const transactionCbor = unsigned.to_cbor_hex();
    const attempt = depositSubmissionAttemptFromCompletedTx({
      ...input,
      transactionCbor,
    });
    expect(attempt[Columns.TX_HASH].toString("hex")).toBe(input.txHash);
    expect(attempt[Columns.METADATA].transactionCbor).toBe(transactionCbor);
    expect(
      CML.Transaction.from_cbor_hex(attempt[Columns.METADATA].transactionCbor)
        .witness_set()
        .vkeywitnesses()
        ?.len(),
    ).toBe(0);
    expect(() =>
      depositSubmissionAttemptFromCompletedTx({
        ...input,
        txHash: "00".repeat(32),
      }),
    ).toThrow(/hash does not match/);
  });

  it.each([
    "structuralLovelace",
    "depositAssetName",
    "orderOutputIndex",
  ] as const)("rejects inconsistent %s metadata", async (field) => {
    const input = await fixture();
    const metadata = { ...input.metadata };
    if (field === "structuralLovelace") metadata.structuralLovelace = 0n;
    else if (field === "depositAssetName")
      metadata.depositAssetName = "ff".repeat(32);
    else metadata.orderOutputIndex = 1;
    expect(() =>
      depositSubmissionAttemptFromCompletedTx({ ...input, metadata }),
    ).toThrow("does not match");
  });

  it("does not accept locked structural funding as requested deposit Value", async () => {
    const input = await fixture();
    expect(() =>
      depositSubmissionAttemptFromCompletedTx({
        ...input,
        config: { ...input.config, lovelace: 7_000_000n },
      }),
    ).toThrow("does not match requested projected assets");
  });
});
