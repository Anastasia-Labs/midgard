import {
  aikenSerialisedPlutusConstrFieldCbor,
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { Data, datumToHash, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { OutputReference } from "../src/common.js";
import { DepositEvent, WithdrawalEvent } from "../src/ledger-state.js";
import { historyEventFromPresence } from "../src/user-events/history-events.js";
import {
  captureEventHistoryWitness,
  type EventHistoryDeployment,
  EventHistoryNode,
  EventHistoryPayload,
  fetchDepositUTxOsProgram,
  fetchWithdrawalUTxOsProgram,
  opensEventHistoryCommitmentCbor,
  prepareEventHistoryPayload,
  readEventHistoryOrders,
  utxosToDepositUTxOs,
  utxosToWithdrawalUTxOs,
} from "../src/user-events/index.js";

const owner = "aa".repeat(28);
const id = { transactionId: "bb".repeat(32), outputIndex: 0n };
const auth = { PublicKeyCredential: [owner] as [string] };
const address = { paymentCredential: auth, stakeCredential: null };
const token = "cc".repeat(28) + "abcd";
const deployment: EventHistoryDeployment = {
  policyId: "dd".repeat(28),
  address: "history-reader-fixture",
  retentionAddress: "history-retention-fixture",
  inlineLimitBytes: 512n,
};
const key = datumToHash(Data.to(id, OutputReference));
const root = (next: string | null): UTxO => ({
  txHash: "11".repeat(32),
  outputIndex: 0,
  address: deployment.address,
  assets: { lovelace: 3_000_000n, [deployment.policyId]: 1n },
  datum: Data.to(
    { position: "Root", next, protected_until: 0n, payload: "RootContent" },
    EventHistoryNode,
  ),
});

const fixture = (kind: "Deposit" | "Withdrawal", external = false) => {
  const arbitraryData = new Map<Data, Data>([
    ["bbaa", external ? "ab".repeat(600) : [1n, "abcd"]],
    [2n, 3n],
  ]);
  const payload: EventHistoryPayload =
    kind === "Deposit"
      ? {
          DepositPayload: {
            event: {
              id,
              info: {
                l2_address: address,
                l2_network_id: 0n,
                l2_datum: arbitraryData,
              },
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
                  l2_value: new Map([["", new Map([["", 9_000_000n]])]]),
                  l1_address: address,
                  l1_datum: "NoDatum",
                },
                signature: ["44".repeat(32), "55".repeat(64)],
                validity: "WithdrawalIsValid",
              },
            },
            refund_address: address,
            refund_datum: { InlineDatum: { data: arbitraryData } },
          },
        };
  const plan = prepareEventHistoryPayload(payload, auth, {
    inlineLimitBytes: deployment.inlineLimitBytes,
    maxPayloadBytes: 15000n,
    maxPayloadNodes: 1024n,
  });
  expect(plan.kind).toBe(external ? "External" : "Inline");
  const node: EventHistoryNode = {
    position: { Key: [key] },
    next: null,
    protected_until: 0n,
    payload: {
      Order: {
        facts: {
          event_id: id,
          inclusion_time: 1_000n,
          location: plan.location,
          structural_lovelace: 2_000_000n,
          structural_refund_key: owner,
        },
      },
    },
  };
  const order: UTxO = {
    txHash: "22".repeat(32),
    outputIndex: 1,
    address: deployment.address,
    assets: {
      lovelace: 7_000_000n,
      [token]: 2n,
      [deployment.policyId + key]: 1n,
    },
    datum: Data.to(node, EventHistoryNode),
  };
  const retained: UTxO[] =
    plan.kind === "External"
      ? [
          {
            txHash: "33".repeat(32),
            outputIndex: 0,
            address: deployment.retentionAddress,
            assets: { lovelace: 3_000_000n },
            datum: plan.datumCbor,
          },
        ]
      : [];
  return { payload, node, order, retained, utxos: [root(key), order] };
};

const changedNode = (order: UTxO, node: EventHistoryNode): UTxO => ({
  ...order,
  datum: Data.to(node, EventHistoryNode),
});

describe("authenticated history event readers", () => {
  it.each([
    ["Deposit", false],
    ["Deposit", true],
    ["Withdrawal", false],
    ["Withdrawal", true],
  ] as const)(
    "opens %s external=%s with exact event CBOR and original funds",
    async (kind, external) => {
      const f = fixture(kind, external);
      const provider = {
        utxosAt: vi.fn(async (address: string) =>
          address === deployment.address ? f.utxos : f.retained,
        ),
      };
      const events =
        kind === "Deposit"
          ? await Effect.runPromise(
              fetchDepositUTxOsProgram(provider, deployment),
            )
          : await Effect.runPromise(
              fetchWithdrawalUTxOsProgram(provider, deployment),
            );
      expect(provider.utxosAt).toHaveBeenCalledTimes(external ? 2 : 1);
      expect(events).toHaveLength(1);
      const event = events[0]!;
      expect(event.kind).toBe(kind);
      expect(event.originalAssets).toEqual({
        lovelace: 5_000_000n,
        [token]: 2n,
      });
      expect(event.utxo.assets.lovelace).toBe(7_000_000n);
      expect(event.inclusionTime).toEqual(new Date(1000));
      const cbor =
        "DepositPayload" in f.payload
          ? Data.to(f.payload.DepositPayload.event, DepositEvent)
          : Data.to(f.payload.WithdrawalPayload.event, WithdrawalEvent);
      expect(event.idCbor.toString("hex")).toBe(
        aikenSerialisedPlutusConstrFieldCbor(cbor, [0]),
      );
      expect(event.infoCbor.toString("hex")).toBe(
        aikenSerialisedPlutusConstrFieldCbor(cbor, [1]),
      );
      expect(event.history.retainedDataUtxo).toEqual(f.retained[0]);
      if (event.kind === "Withdrawal" && "WithdrawalPayload" in f.payload) {
        expect(event.refundAddress).toEqual(
          f.payload.WithdrawalPayload.refund_address,
        );
        expect(event.refundDatum).toEqual(
          f.payload.WithdrawalPayload.refund_datum,
        );
      }
    },
  );

  it.each([
    ["Deposit", false],
    ["Deposit", true],
    ["Withdrawal", false],
    ["Withdrawal", true],
  ] as const)(
    "retains raw map order and repeated pairs for %s external=%s",
    (kind, external) => {
      for (const rawDatum of [
        "a202000101",
        "a3020001010202",
        "a20281c2490100000000000000000181a202000101",
      ]) {
        const f = fixture(kind, external);
        let payloadCbor = Data.to(f.payload, EventHistoryPayload);
        payloadCbor = replacePlutusConstrFieldCbor(
          payloadCbor,
          kind === "Deposit" ? [0, 1, 2, 0] : [2, 0],
          rawDatum,
        );
        if (kind === "Withdrawal") {
          payloadCbor = replacePlutusConstrFieldCbor(
            payloadCbor,
            [0, 1, 0, 4],
            `d87b9f${rawDatum}ff`,
          );
        }
        payloadCbor =
          aikenSerialisedPlutusDataCborPreservingMapOrder(payloadCbor);
        if (external) {
          f.retained[0]!.datum = replacePlutusConstrFieldCbor(
            f.retained[0]!.datum!,
            [1],
            payloadCbor,
          );
          const storageHash = datumToHash(
            aikenSerialisedPlutusDataCborPreservingMapOrder(
              f.retained[0]!.datum!,
            ),
          );
          f.order.datum = replacePlutusConstrFieldCbor(
            f.order.datum!,
            [3, 0, 2, 0],
            Data.to(storageHash),
          );
        } else {
          f.order.datum = replacePlutusConstrFieldCbor(
            f.order.datum!,
            [3, 0, 2, 0],
            payloadCbor,
          );
        }
        const witness = readEventHistoryOrders(
          f.utxos,
          f.retained,
          deployment,
        )[0]!;
        const captured = captureEventHistoryWitness(
          witness,
          deployment.policyId,
          kind,
        );
        expect(witness.payloadCbor).toBe(payloadCbor);
        expect(captured.payloadCbor).toBe(payloadCbor);
        expect(plutusConstrFieldCbor(captured.openingCbor, [0])).toBe(
          payloadCbor,
        );
        expect(captured.commitment.payload_hash).toBe(datumToHash(payloadCbor));
        const assetsCbor = plutusConstrFieldCbor(captured.openingCbor, [1]);
        expect(
          opensEventHistoryCommitmentCbor(
            captured.commitment,
            payloadCbor,
            assetsCbor,
          ),
        ).toBe(true);
        const normalized = Data.to(
          Data.from(payloadCbor, EventHistoryPayload),
          EventHistoryPayload,
        );
        expect(
          opensEventHistoryCommitmentCbor(
            captured.commitment,
            normalized,
            assetsCbor,
          ),
        ).toBe(false);
        expect(() =>
          captureEventHistoryWitness(
            { ...witness, payloadCbor: normalized },
            deployment.policyId,
            kind,
          ),
        ).toThrow(/differs/);
        const event = historyEventFromPresence(witness, deployment);
        expect(event.infoCbor.toString("hex")).toBe(
          plutusConstrFieldCbor(payloadCbor, [0, 1]),
        );
        expect(event.originalAssets).toEqual({
          lovelace: 5_000_000n,
          [token]: 2n,
        });
      }
    },
  );

  it("derives the returned event and time from raw evidence despite stale typed views", () => {
    const f = fixture("Deposit");
    const witness = readEventHistoryOrders(f.utxos, [], deployment)[0]!;
    const expected = historyEventFromPresence(witness, deployment);
    if (
      !("DepositPayload" in witness.payload) ||
      witness.anchor.node.payload === "RootContent" ||
      !("Order" in witness.anchor.node.payload)
    )
      throw new Error("Expected deposit Order");
    witness.payload.DepositPayload.event.info.l2_datum = null;
    witness.payload.DepositPayload.event.info.l2_network_id = 1n;
    witness.anchor.node.payload.Order.facts.inclusion_time = 999n;
    const actual = historyEventFromPresence(witness, deployment);
    expect(actual.event).toEqual(expected.event);
    expect(actual.facts).toEqual(expected.facts);
    expect(actual.inclusionTime).toEqual(new Date(1000));
    expect(actual.infoCbor).toEqual(expected.infoCbor);
  });

  it("excludes roots, fillers and unauthenticated donations", async () => {
    const f = fixture("Deposit");
    const fillerKey = "ff".repeat(32);
    const filler = changedNode(
      {
        ...f.order,
        txHash: "66".repeat(32),
        assets: {
          lovelace: 10_000_000n,
          [deployment.policyId + fillerKey]: 1n,
        },
      },
      {
        position: { Key: [fillerKey] },
        next: null,
        protected_until: 0n,
        payload: { Filler: { refund_key: owner } },
      },
    );
    const donation = {
      ...f.order,
      txHash: "77".repeat(32),
      assets: { lovelace: 50_000_000n },
    };
    const result = await Effect.runPromise(
      utxosToDepositUTxOs(
        [
          filler,
          donation,
          root(key),
          changedNode(f.order, { ...f.node, next: fillerKey }),
        ],
        [],
        deployment,
      ),
    );
    expect(result).toHaveLength(1);
    expect(result[0]!.originalAssets.lovelace).toBe(5_000_000n);
    expect(readEventHistoryOrders([root(null)], [], deployment)).toEqual([]);
  });

  it("refuses missing root, missing successor, disconnected node and duplicate key snapshots", () => {
    const f = fixture("Deposit");
    for (const nodes of [
      [f.order],
      [root(key)],
      [root(null), f.order],
      [...f.utxos, f.order],
    ]) {
      expect(() => readEventHistoryOrders(nodes, [], deployment)).toThrow(
        /Root|missing|disconnected|Duplicate/,
      );
    }
  });

  it("rejects wrong token, address, event identity, kind and inline bound", async () => {
    const f = fixture("Deposit");
    for (const order of [
      {
        ...f.order,
        assets: { lovelace: 7_000_000n, [deployment.policyId + key]: 2n },
      },
      { ...f.order, address: "wrong-script" },
    ]) {
      expect(() =>
        readEventHistoryOrders([root(key), order], [], deployment),
      ).toThrow(/authenticate|shape/);
    }
    if (f.node.payload === "RootContent" || !("Order" in f.node.payload))
      throw new Error("Expected Order");
    const facts = f.node.payload.Order.facts;
    expect(() =>
      readEventHistoryOrders(
        [
          root(key),
          changedNode(f.order, {
            ...f.node,
            payload: {
              Order: {
                facts: { ...facts, event_id: { ...id, outputIndex: 1n } },
              },
            },
          }),
        ],
        [],
        deployment,
      ),
    ).toThrow(/identity/);
    expect(() =>
      readEventHistoryOrders(f.utxos, [], {
        ...deployment,
        inlineLimitBytes: 1n,
      }),
    ).toThrow(/bound/);
    await expect(
      Effect.runPromise(utxosToWithdrawalUTxOs(f.utxos, [], deployment)),
    ).rejects.toThrow(/different event kind/);
    const w = fixture("Withdrawal");
    await expect(
      Effect.runPromise(utxosToDepositUTxOs(w.utxos, [], deployment)),
    ).rejects.toThrow(/different event kind/);
  });

  it("requires the actual retained datum at the configured script with its complete hash", () => {
    const f = fixture("Deposit", true);
    const retained = f.retained[0]!;
    for (const candidates of [
      [],
      [{ ...retained, address: "archive" }],
      [{ ...retained, datum: Data.to(0n) }],
      [{ ...retained, scriptRef: { type: "PlutusV3" as const, script: "00" } }],
    ]) {
      expect(() =>
        readEventHistoryOrders(f.utxos, candidates, deployment),
      ).toThrow(/unavailable on L1/);
    }
  });

  it("filters the immutable inclusion time with inclusive lower and exclusive upper bounds", async () => {
    const f = fixture("Deposit");
    const provider = { utxosAt: async () => f.utxos };
    for (const [lower, upper, count] of [
      [1000n, 1001n, 1],
      [999n, 1000n, 0],
      [1001n, 2000n, 0],
    ] as const) {
      expect(
        await Effect.runPromise(
          fetchDepositUTxOsProgram(provider, {
            ...deployment,
            inclusionTimeLowerBound: lower,
            inclusionTimeUpperBound: upper,
          }),
        ),
      ).toHaveLength(count);
    }
  });

  it("propagates unavailable providers and rejects structural funding exceeding the output", async () => {
    await expect(
      Effect.runPromise(
        fetchDepositUTxOsProgram(
          {
            utxosAt: async () => {
              throw new Error("provider unavailable");
            },
          },
          deployment,
        ),
      ),
    ).rejects.toThrow(/provider unavailable/);
    const f = fixture("Deposit");
    await expect(
      Effect.runPromise(
        utxosToDepositUTxOs(
          [
            root(key),
            { ...f.order, assets: { ...f.order.assets, lovelace: 1_000_000n } },
          ],
          [],
          deployment,
        ),
      ),
    ).rejects.toThrow(/structural ADA/);
  });
});
