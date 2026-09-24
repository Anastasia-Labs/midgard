import { decodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, datumToHash, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it } from "vitest";

import * as DepositsDB from "../src/database/deposits.js";
import * as WithdrawalsDB from "../src/database/withdrawals.js";
import { depositUTxOToEntry } from "../src/fibers/fetch-and-insert-deposit-utxos.js";
import { withdrawalUTxOToEntry } from "../src/fibers/fetch-and-insert-withdrawal-utxos.js";
import { historyIncarnationEntry } from "../src/l1-event-history-entries.js";
import {
  type HistoryIncarnation,
  historyIncarnationId,
} from "../src/l1-event-history-provenance.js";

const rawDatum = "a3020001010202";
const owner = "aa".repeat(28);
const id = { transactionId: "bb".repeat(32), outputIndex: 1n };
const address: SDK.AddressData = {
  paymentCredential: { PublicKeyCredential: [owner] },
  stakeCredential: null,
};
const deployment: SDK.EventHistoryDeployment = {
  policyId: "cc".repeat(28),
  address: "history",
  retentionAddress: "retention",
  inlineLimitBytes: 1024n,
};
const fixture = (kind: "Deposit" | "Withdrawal") => {
  const payload: SDK.EventHistoryPayload =
    kind === "Deposit"
      ? {
          DepositPayload: {
            event: {
              id,
              info: { l2_address: address, l2_network_id: 0n, l2_datum: 0n },
            },
          },
        }
      : {
          WithdrawalPayload: {
            event: {
              id,
              info: {
                body: {
                  l2_outref: id,
                  l2_owner: owner,
                  l2_value: new Map([["", new Map([["", 5_000_000n]])]]),
                  l1_address: address,
                  l1_datum: { InlineDatum: { data: 0n } },
                },
                signature: ["dd".repeat(32), "ee".repeat(64)],
                validity: "WithdrawalIsValid",
              },
            },
            refund_address: address,
            refund_datum: { InlineDatum: { data: 0n } },
          },
        };
  let payloadCbor = replacePlutusConstrFieldCbor(
    Data.to(payload, SDK.EventHistoryPayload),
    kind === "Deposit" ? [0, 1, 2, 0] : [0, 1, 0, 4, 0],
    rawDatum,
  );
  if (kind === "Withdrawal")
    payloadCbor = replacePlutusConstrFieldCbor(payloadCbor, [2, 0], rawDatum);
  const key = datumToHash(Data.to(id, SDK.OutputReference));
  const node: SDK.EventHistoryNode = {
    position: { Key: [key] },
    next: null,
    protected_until: 0n,
    payload: {
      Order: {
        facts: {
          event_id: id,
          inclusion_time: 1000n,
          location: { Inline: { payload } },
          structural_lovelace: 3_000_000n,
          structural_refund_key: owner,
        },
      },
    },
  };
  const order: UTxO = {
    txHash: "11".repeat(32),
    outputIndex: 1,
    address: deployment.address,
    assets: { lovelace: 8_000_000n, [deployment.policyId + key]: 1n },
    datum: replacePlutusConstrFieldCbor(
      Data.to(node, SDK.EventHistoryNode),
      [3, 0, 2, 0],
      payloadCbor,
    ),
  };
  const root: UTxO = {
    ...order,
    outputIndex: 0,
    assets: { lovelace: 3_000_000n, [deployment.policyId]: 1n },
    datum: Data.to(
      {
        position: "Root",
        next: key,
        protected_until: 0n,
        payload: "RootContent",
      },
      SDK.EventHistoryNode,
    ),
  };
  return {
    utxos: [root, order],
    payloadCbor: aikenSerialisedPlutusDataCborPreservingMapOrder(payloadCbor),
  };
};

it("projects the original deposit datum and funds despite a stale typed event view", async () => {
  const f = fixture("Deposit");
  const [event] = await Effect.runPromise(
    SDK.utxosToDepositUTxOs(f.utxos, [], deployment),
  );
  event!.event.info.l2_datum = null;
  event!.event.info.l2_network_id = 1n;
  event!.event.id.outputIndex = 9n;
  const row = await Effect.runPromise(depositUTxOToEntry(event!, "Preprod"));
  const output = decodeMidgardTxOutput(row[DepositsDB.Columns.LEDGER_OUTPUT]);
  expect(output.datum?.cbor.toString("hex")).toBe(rawDatum);
  expect(row[DepositsDB.Columns.ID].toString("hex")).toBe(
    Data.to(id, SDK.OutputReference),
  );
  expect(row[DepositsDB.Columns.INFO].toString("hex")).toBe(
    plutusConstrFieldCbor(f.payloadCbor, [0, 1]),
  );
});

it("stores exact withdrawal body and refund datum while leaving validity unclassified", async () => {
  const f = fixture("Withdrawal");
  const [event] = await Effect.runPromise(
    SDK.utxosToWithdrawalUTxOs(f.utxos, [], deployment),
  );
  const row = await Effect.runPromise(withdrawalUTxOToEntry(event!));
  expect(row[WithdrawalsDB.Columns.RAW_EVENT_INFO].toString("hex")).toBe(
    plutusConstrFieldCbor(f.payloadCbor, [0, 1]),
  );
  expect(row[WithdrawalsDB.Columns.L1_DATUM].toString("hex")).toBe(
    plutusConstrFieldCbor(f.payloadCbor, [0, 1, 0, 4]),
  );
  expect(row[WithdrawalsDB.Columns.REFUND_DATUM].toString("hex")).toBe(
    plutusConstrFieldCbor(f.payloadCbor, [2]),
  );
  expect(row[WithdrawalsDB.Columns.L2_VALUE].toString("hex")).toBe(
    plutusConstrFieldCbor(f.payloadCbor, [0, 1, 0, 2]),
  );
  expect(row[WithdrawalsDB.Columns.VALIDITY]).toBeNull();
  expect(row[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]).toBeNull();
});

it.each(["Deposit", "Withdrawal"] as const)(
  "derives %s rows from retired journal facts without a live Order",
  async (kind) => {
    const f = fixture(kind);
    const [visible] =
      kind === "Deposit"
        ? await Effect.runPromise(
            SDK.utxosToDepositUTxOs(f.utxos, [], deployment),
          )
        : await Effect.runPromise(
            SDK.utxosToWithdrawalUTxOs(f.utxos, [], deployment),
          );
    const v = visible!;
    const event = {
      key: v.assetName,
      idCbor: v.idCbor.toString("hex"),
      inclusionTime: BigInt(v.inclusionTime.getTime()),
      factsCbor: Data.to(v.facts, SDK.EventHistoryFacts),
      payloadCbor: f.payloadCbor,
      originalAssetsCbor: Data.to(
        SDK.assetsToValue(v.originalAssets),
        SDK.Value,
      ),
      outRef: { txHash: v.utxo.txHash, outputIndex: v.utxo.outputIndex },
    };
    const bindingDigest = "42".repeat(32);
    const historyKind = kind === "Deposit" ? "deposit" : "withdrawal";
    const at = {
      blockHash: "43".repeat(32),
      slot: 1,
      height: 1,
      transactionHash: v.utxo.txHash,
      transactionIndex: 0,
    };
    const retiredOutRef = { txHash: "44".repeat(32), outputIndex: 2 };
    const incarnation: HistoryIncarnation = {
      id: historyIncarnationId(bindingDigest, historyKind, event),
      bindingDigest,
      kind: historyKind,
      event,
      placement: {
        admission: at,
        current: null,
        retirement: {
          at: { ...at, slot: 3, height: 3 },
          outRef: retiredOutRef,
          reason: kind === "Deposit" ? "absorbed" : "payout_initialized",
          observerRedeemerIndex: 0,
          witnessCbor: "d87980",
        },
      },
    };
    const result = await Effect.runPromise(
      historyIncarnationEntry(incarnation, "Preprod"),
    );
    expect(result.kind).toBe(historyKind);
    if (result.kind === "deposit") {
      const output = decodeMidgardTxOutput(
        result.entry[DepositsDB.Columns.LEDGER_OUTPUT],
      );
      expect(output.datum?.cbor.toString("hex")).toBe(rawDatum);
      expect(output.value.lovelace).toBe(5_000_000n);
      expect(
        result.entry[DepositsDB.Columns.DEPOSIT_L1_TX_HASH].toString("hex"),
      ).toBe(retiredOutRef.txHash);
    } else {
      expect(
        result.entry[WithdrawalsDB.Columns.RAW_EVENT_INFO].toString("hex"),
      ).toBe(plutusConstrFieldCbor(f.payloadCbor, [0, 1]));
      expect(
        result.entry[WithdrawalsDB.Columns.REFUND_DATUM].toString("hex"),
      ).toBe(plutusConstrFieldCbor(f.payloadCbor, [2]));
      expect(
        result.entry[WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH].toString(
          "hex",
        ),
      ).toBe(retiredOutRef.txHash);
      expect(
        result.entry[WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX],
      ).toBe(retiredOutRef.outputIndex);
      expect(result.entry[WithdrawalsDB.Columns.VALIDITY]).toBeNull();
    }
    const bad = {
      ...incarnation,
      event: {
        ...event,
        idCbor: Data.to({ ...id, outputIndex: 99n }, SDK.OutputReference),
      },
    };
    await expect(
      Effect.runPromise(historyIncarnationEntry(bad, "Preprod")),
    ).rejects.toThrow("Failed to read history incarnation entry");
  },
);
