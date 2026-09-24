import { decodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import { aikenSerialisedPlutusDataCborPreservingMapOrder as canonical } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { deriveCanonicalOriginalDepositTransitionEffect } from "@al-ft/midgard-validation";
import { Data, datumToHash, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  capturePublishedDepositHistory,
  readPublishedDepositHistory,
} from "./published-deposit-history.js";

const owner = "aa".repeat(28);
const id = { transactionId: "bb".repeat(32), outputIndex: 0n };
const policy = "dd".repeat(28);
const token = "cc".repeat(28) + "abcd";
const key = datumToHash(canonical(Data.to(id, SDK.OutputReference)));
const metadata = {
  depositAuthUnit: policy + key,
  depositAssetName: key,
  inclusionTime: 1_000,
};

const readFixture = async (external: boolean) => {
  const deployment: SDK.EventHistoryDeployment = {
    policyId: policy,
    address: "history-reader-fixture",
    retentionAddress: "history-retention-fixture",
    inlineLimitBytes: 512n,
  };
  const auth = { PublicKeyCredential: [owner] as [string] };
  const payload: SDK.EventHistoryPayload = {
    DepositPayload: {
      event: {
        id,
        info: {
          l2_address: { paymentCredential: auth, stakeCredential: null },
          l2_network_id: 0n,
          l2_datum: new Map<Data, Data>([
            ["bbaa", external ? "ab".repeat(600) : [1n, "abcd"]],
            [
              2n,
              new Map<Data, Data>([
                [3n, "ffaa"],
                ["00", 4n],
              ]),
            ],
          ]),
        },
      },
    },
  };
  const plan = SDK.prepareEventHistoryPayload(payload, auth, {
    inlineLimitBytes: 512n,
    maxPayloadBytes: 5000n,
    maxPayloadNodes: 512n,
  });
  expect(plan.kind).toBe(external ? "External" : "Inline");
  const root: UTxO = {
    txHash: "11".repeat(32),
    outputIndex: 0,
    address: deployment.address,
    assets: { lovelace: 3_000_000n, [policy]: 1n },
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
  const node: SDK.EventHistoryNode = {
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
  const utxo: UTxO = {
    txHash: "22".repeat(32),
    outputIndex: 1,
    address: deployment.address,
    assets: { lovelace: 7_000_000n, [token]: 2n, [policy + key]: 1n },
    datum: Data.to(node, SDK.EventHistoryNode),
  };
  const retention: UTxO[] =
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
  const provider = {
    utxosAt: async (address: string) =>
      address === deployment.address ? [root, utxo] : retention,
  };
  const orders = await Effect.runPromise(
    SDK.fetchDepositUTxOsProgram(provider, deployment),
  );
  expect(orders).toHaveLength(1);
  return orders[0]!;
};

describe("published deposit history capture", () => {
  it.each([false, true])(
    "captures authenticated reader external=%s and preserves nested Data maps through JSON checkpoints",
    async (external) => {
      const order = await readFixture(external);
      const history = capturePublishedDepositHistory(order, policy);
      const restored = readPublishedDepositHistory(
        JSON.parse(JSON.stringify(history)),
        metadata,
      );
      expect(canonical(Data.to(restored.event, SDK.DepositEvent))).toBe(
        canonical(Data.to(order.event, SDK.DepositEvent)),
      );
      expect(restored.event.info.l2_datum).toBeInstanceOf(Map);
      expect(restored.originalAssets).toEqual({
        lovelace: 5_000_000n,
        [token]: 2n,
      });
      const effect = deriveCanonicalOriginalDepositTransitionEffect({
        configuredNetwork: "Custom",
        eventId: restored.event.id,
        l2Address: restored.event.info.l2_address,
        l2NetworkId: restored.event.info.l2_network_id,
        l2DatumCbor: Buffer.from(
          canonical(Data.to(restored.event.info.l2_datum!)),
          "hex",
        ),
        originalAssets: restored.originalAssets,
      });
      const insert = effect.operations[0];
      if (insert?.type !== "insert")
        throw new Error("Deposit did not project an insertion");
      const output = decodeMidgardTxOutput(insert.outputCbor);
      expect(output.value.lovelace).toBe(5_000_000n);
      expect(output.value.assets.has(policy)).toBe(false);
      expect(output.value.assets.get("cc".repeat(28))?.get("abcd")).toBe(2n);
      // Navigation changes cannot change the saved immutable event opening.
      order.utxo.assets.lovelace = 99_000_000n;
      order.utxo.datum = Data.to(0n);
      expect(
        readPublishedDepositHistory(history, metadata).originalAssets.lovelace,
      ).toBe(5_000_000n);
    },
  );

  it("rejects substituted opening, commitment, metadata and obsolete checkpoints", async () => {
    const history = capturePublishedDepositHistory(
      await readFixture(true),
      policy,
    );
    const opening = Data.from(history.openingCbor, SDK.EventHistoryOpening);
    const altered = {
      ...opening,
      original_assets: SDK.assetsToValue({ lovelace: 7_000_000n }),
    };
    expect(() =>
      readPublishedDepositHistory(
        {
          ...history,
          openingCbor: canonical(Data.to(altered, SDK.EventHistoryOpening)),
        },
        metadata,
      ),
    ).toThrow("differs");
    const commitment = Data.from(
      history.commitmentCbor,
      SDK.EventHistoryCommitment,
    );
    expect(() =>
      readPublishedDepositHistory(
        {
          ...history,
          commitmentCbor: canonical(
            Data.to(
              { ...commitment, payload_hash: "ff".repeat(32) },
              SDK.EventHistoryCommitment,
            ),
          ),
        },
        metadata,
      ),
    ).toThrow("differs");
    expect(() =>
      readPublishedDepositHistory(history, {
        ...metadata,
        inclusionTime: 1001,
      }),
    ).toThrow("differs");
    expect(() =>
      readPublishedDepositHistory(history, {
        ...metadata,
        depositAuthUnit: "ee".repeat(28) + key,
      }),
    ).toThrow("differs");
    expect(() =>
      readPublishedDepositHistory(undefined as never, metadata),
    ).toThrow("capture is required");
    expect(() => readPublishedDepositHistory({} as never, metadata)).toThrow(
      "capture is required",
    );
  });

  it("refuses capture when reader event or original funds disagree with its history witness", async () => {
    const order = await readFixture(false);
    expect(() =>
      capturePublishedDepositHistory(
        {
          ...order,
          originalAssets: { ...order.originalAssets, lovelace: 7_000_000n },
        },
        policy,
      ),
    ).toThrow("disagrees");
    expect(() =>
      capturePublishedDepositHistory(
        {
          ...order,
          event: {
            ...order.event,
            info: { ...order.event.info, l2_datum: 99n },
          },
        },
        policy,
      ),
    ).toThrow("disagrees");
  });
});
