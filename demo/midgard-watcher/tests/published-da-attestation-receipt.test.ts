import { MIDGARD_PROTOCOL_VERSION } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  toUnit,
  utxoToCore,
} from "@lucid-evolution/lucid";
import { expect, it, vi } from "vitest";

import { verifyPublishedDaAttestationReceipt } from "./support/published-da-attestation-receipt.js";

const headerHash = "11".repeat(28);
const bondAssetName = "22".repeat(32);
const stateQueueAddress = credentialToAddress("Preprod", {
  type: "Script",
  hash: "33".repeat(28),
});
const stateQueueUnit = toUnit(
  "44".repeat(28),
  SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
);
const header: SDK.Header = {
  prevUtxosRoot: "55".repeat(32),
  utxosRoot: "55".repeat(32),
  withdrawalsRoot: "55".repeat(32),
  forcedTransactionsRoot: "55".repeat(32),
  transactionsRoot: "55".repeat(32),
  depositsRoot: "55".repeat(32),
  transitionTraceRoot: "55".repeat(32),
  eventToStepRoot: "55".repeat(32),
  validationTracesRoot: "55".repeat(32),
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 0n,
  depositCount: 0n,
  totalEventCount: 0n,
  transitionStepCount: 0n,
  validationTraceCount: 0n,
  startTime: 0n,
  endTime: 1n,
  blockSlot: 1n,
  expectedNetworkId: 0n,
  minFeeA: 44n,
  minFeeB: 155381n,
  prevHeaderHash: "66".repeat(28),
  operatorVkey: "77".repeat(28),
  protocolVersion: BigInt(MIDGARD_PROTOCOL_VERSION),
};
const transaction = (
  attestation: SDK.StateQueueNode["da_attestation"] = {
    Attested: { da_bond_asset_name: bondAssetName },
  },
  copies = 1,
) => {
  const outputs = CML.TransactionOutputList.new();
  for (let i = 0; i < copies; i++)
    outputs.add(
      utxoToCore({
        txHash: "00".repeat(32),
        outputIndex: i,
        address: stateQueueAddress,
        assets: { lovelace: 5_000_000n, [stateQueueUnit]: 1n },
        datum: SDK.encodeLinkedListNodeView({
          key: { Key: { key: headerHash } },
          next: "Empty",
          data: SDK.castStateQueueNodeToData({
            proven_fraud: null,
            header,
            da_attestation: attestation,
          }) as SDK.LinkedListNodeView["data"],
        }),
      }).output(),
    );
  return CML.Transaction.new(
    CML.TransactionBody.new(CML.TransactionInputList.new(), outputs, 200_000n),
    CML.TransactionWitnessSet.new(),
    true,
  );
};
const fixture = (included = transaction()) => ({
  txHash: CML.hash_transaction(included.body()).to_hex(),
  signedCbor: included.to_cbor_hex(),
  readConfirmedTransaction: vi.fn(async () => ({
    cbor: included.to_cbor_hex(),
  })),
  readLiveAttestation: vi.fn(
    async (): Promise<SDK.StateQueueNode["da_attestation"]> => {
      throw new Error("Proof already consumed the header");
    },
  ),
  stateQueueAddress,
  stateQueueUnit,
  headerHash,
  bondAssetName,
});

it("accepts the exact included DA apply after a proof consumes its live header", async () => {
  const input = fixture();
  await expect(
    verifyPublishedDaAttestationReceipt(input),
  ).resolves.toBeUndefined();
  expect(input.readConfirmedTransaction).toHaveBeenCalledWith(input.txHash);
  expect(input.readLiveAttestation).not.toHaveBeenCalled();
});

it("preserves strict live-output verification without a native receipt reader", async () => {
  const input = { ...fixture(), readConfirmedTransaction: undefined };
  await expect(verifyPublishedDaAttestationReceipt(input)).rejects.toThrow(
    "Proof already consumed",
  );
  input.readLiveAttestation.mockResolvedValue(SDK.NO_DA_ATTESTATION);
  await expect(verifyPublishedDaAttestationReceipt(input)).rejects.toThrow(
    "did not attach",
  );
  input.readLiveAttestation.mockResolvedValue({
    Attested: { da_bond_asset_name: bondAssetName },
  });
  await expect(
    verifyPublishedDaAttestationReceipt(input),
  ).resolves.toBeUndefined();
});

it("rejects collateral-only inclusion even with the exact signed body hash", async () => {
  const valid = transaction();
  const invalid = CML.Transaction.new(valid.body(), valid.witness_set(), false);
  const input = fixture(valid);
  input.readConfirmedTransaction.mockResolvedValue({
    cbor: invalid.to_cbor_hex(),
  });
  await expect(verifyPublishedDaAttestationReceipt(input)).rejects.toThrow(
    "exact valid signed body",
  );
});

it("rejects a different included body or a substituted signed body", async () => {
  const input = fixture();
  const other = transaction(SDK.NO_DA_ATTESTATION).to_cbor_hex();
  input.readConfirmedTransaction.mockResolvedValue({ cbor: other });
  await expect(verifyPublishedDaAttestationReceipt(input)).rejects.toThrow(
    "exact valid signed body",
  );
  await expect(
    verifyPublishedDaAttestationReceipt({ ...fixture(), signedCbor: other }),
  ).rejects.toThrow("exact valid signed body");
});

it.each([0, 2])("rejects %s matching state queue outputs", async (copies) => {
  await expect(
    verifyPublishedDaAttestationReceipt(
      fixture(transaction(undefined, copies)),
    ),
  ).rejects.toThrow("unique state queue output");
});

it("rejects an unattested output, changed bond, or unrelated header identity", async () => {
  await expect(
    verifyPublishedDaAttestationReceipt(
      fixture(transaction(SDK.NO_DA_ATTESTATION)),
    ),
  ).rejects.toThrow("expected attestation");
  await expect(
    verifyPublishedDaAttestationReceipt({
      ...fixture(),
      bondAssetName: "99".repeat(32),
    }),
  ).rejects.toThrow("expected attestation");
  await expect(
    verifyPublishedDaAttestationReceipt({
      ...fixture(),
      headerHash: "99".repeat(28),
    }),
  ).rejects.toThrow("expected attestation");
});

it("does not fall back to live state when native receipt lookup fails", async () => {
  const input = fixture();
  input.readConfirmedTransaction.mockRejectedValue(
    new Error("Native recorder failed"),
  );
  await expect(verifyPublishedDaAttestationReceipt(input)).rejects.toThrow(
    "Native recorder failed",
  );
  expect(input.readLiveAttestation).not.toHaveBeenCalled();
});
