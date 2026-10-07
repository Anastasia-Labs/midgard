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
import { historyIncarnationEntry } from "../src/l1-event-history-entries.js";
import {
  type HistoryIncarnation,
  historyIncarnationId,
} from "../src/l1-event-history-provenance.js";
import { userEventEntry } from "../src/l1-events/index.js";
import { projectOrderAsFollower } from "./helpers/emulator-l1-follower.js";

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

/** The fixture's Order as the follower's derivation opens and projects it. */
const projectedOrder = (
  kind: "Deposit" | "Withdrawal",
  f: ReturnType<typeof fixture>,
) =>
  projectOrderAsFollower(
    f.utxos[1]!,
    kind === "Deposit" ? "deposit" : "withdrawal",
    deployment.policyId,
  );

it("projects the original deposit datum and funds from the Order's own bytes", () => {
  const f = fixture("Deposit");
  const decoded = userEventEntry(projectedOrder("Deposit", f), "Preprod");
  if (decoded.kind !== "deposit") throw new Error("expected a deposit");
  const output = decodeMidgardTxOutput(
    Buffer.from(decoded.entry.ledgerOutput, "hex"),
  );
  expect(output.datum?.cbor.toString("hex")).toBe(rawDatum);
  expect(decoded.entry.idCbor).toBe(Data.to(id, SDK.OutputReference));
  expect(decoded.entry.infoCbor).toBe(
    plutusConstrFieldCbor(f.payloadCbor, [0, 1]),
  );
});

it("stores the exact withdrawal body and refund datum", () => {
  const f = fixture("Withdrawal");
  const decoded = userEventEntry(projectedOrder("Withdrawal", f), "Preprod");
  if (decoded.kind !== "withdrawal") throw new Error("expected a withdrawal");
  expect(decoded.entry.rawEventInfo).toBe(
    plutusConstrFieldCbor(f.payloadCbor, [0, 1]),
  );
  expect(decoded.entry.l1Datum).toBe(
    plutusConstrFieldCbor(f.payloadCbor, [0, 1, 0, 4]),
  );
  expect(decoded.entry.refundDatum).toBe(
    plutusConstrFieldCbor(f.payloadCbor, [2]),
  );
  expect(decoded.entry.l2Value).toBe(
    plutusConstrFieldCbor(f.payloadCbor, [0, 1, 0, 2]),
  );
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
