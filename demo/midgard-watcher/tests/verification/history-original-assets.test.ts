import { EventHistoryNode, OutputReference } from "@al-ft/midgard-sdk";
import {
  deriveCanonicalDepositTransitionEffect,
  deriveCanonicalOriginalDepositTransitionEffect,
} from "@al-ft/midgard-validation";
import { CML, Data, datumToHash } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { watcherOriginalDepositAssets } from "../../src/verification/history-original-assets.js";

const policy = "11".repeat(28);
const owner = "22".repeat(28);
const id = { transactionId: "33".repeat(32), outputIndex: 0n };
const key = datumToHash(Data.to(id, OutputReference));
const tokenPolicy = "44".repeat(28);
const address = {
  paymentCredential: { PublicKeyCredential: [owner] as [string] },
  stakeCredential: null,
};
const node = (): EventHistoryNode => ({
  position: { Key: [key] },
  next: null,
  protected_until: 1000n,
  payload: {
    Order: {
      facts: {
        event_id: id,
        inclusion_time: 100n,
        location: {
          Inline: {
            payload: {
              DepositPayload: {
                event: {
                  id,
                  info: {
                    l2_address: address,
                    l2_network_id: 0n,
                    l2_datum: null,
                  },
                },
              },
            },
          },
        },
        structural_lovelace: 2_000_000n,
        structural_refund_key: owner,
      },
    },
  },
});

// Serialization/projection fixtures only; the caller must own an admitted L1 capability.
const event = (datum = node(), authKey = key) => {
  const datumCborHex = Data.to(datum, EventHistoryNode);
  const multiasset = CML.MultiAsset.new();
  multiasset.set(
    CML.ScriptHash.from_hex(policy),
    CML.AssetName.from_hex(authKey),
    1n,
  );
  multiasset.set(
    CML.ScriptHash.from_hex(tokenPolicy),
    CML.AssetName.from_hex("aabb"),
    3n,
  );
  const output = CML.TransactionOutput.new(
    CML.Address.from_hex("60" + owner),
    CML.Value.new(7_000_000n, multiasset),
    CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datumCborHex)),
  );
  return {
    kind: "deposit" as const,
    policyId: policy,
    eventId: Data.to(id, OutputReference),
    assetNameHex: authKey,
    datumCborHex,
    outputCborHex: output.to_canonical_cbor_hex(),
  };
};

describe("watcher original deposit funds", () => {
  it("agrees with node original-Value projection and excludes structural ADA", () => {
    const originalAssets = watcherOriginalDepositAssets(event());
    expect(originalAssets).toEqual({
      lovelace: 5_000_000n,
      [tokenPolicy + "aabb"]: 3n,
    });
    const common = {
      configuredNetwork: "Custom" as const,
      eventId: id,
      l2NetworkId: 0n,
      l2Address: address,
      l2DatumCbor: null,
    };
    const correct = deriveCanonicalOriginalDepositTransitionEffect({
      ...common,
      originalAssets,
    });
    const inflated = deriveCanonicalDepositTransitionEffect({
      ...common,
      l1Assets: { ...originalAssets, lovelace: 7_000_000n, [policy + key]: 1n },
      depositPolicyId: policy,
      depositAssetNameHex: key,
    });
    expect(correct.digest).not.toBe(inflated.digest);
  });

  it("preserves funds across pointer/protection changes and external payload storage", () => {
    const moved = node();
    moved.next = "ff".repeat(32);
    moved.protected_until = 2000n;
    if (moved.payload === "RootContent" || !("Order" in moved.payload))
      throw new Error("fixture");
    moved.payload.Order.facts.location = {
      External: { storage_datum_hash: "aa".repeat(32) },
    };
    expect(watcherOriginalDepositAssets(event(moved))).toEqual(
      watcherOriginalDepositAssets(event()),
    );
  });

  it("refuses roots and fillers even when they carry funds", () => {
    expect(() =>
      watcherOriginalDepositAssets(
        event(
          {
            position: "Root",
            next: key,
            protected_until: 0n,
            payload: "RootContent",
          },
          "",
        ),
      ),
    ).toThrow(/selected deposit/);
    const filler = node();
    filler.payload = { Filler: { refund_key: owner } };
    expect(() => watcherOriginalDepositAssets(event(filler))).toThrow(
      /selected deposit/,
    );
  });

  it("refuses cached datum, identity and authentication mismatches", () => {
    const altered = node();
    if (altered.payload === "RootContent" || !("Order" in altered.payload))
      throw new Error("fixture");
    altered.payload.Order.facts.structural_lovelace = 1n;
    expect(() =>
      watcherOriginalDepositAssets({
        ...event(),
        datumCborHex: Data.to(altered, EventHistoryNode),
      }),
    ).toThrow(/differs/);
    expect(() =>
      watcherOriginalDepositAssets({
        ...event(),
        eventId: Data.to({ ...id, outputIndex: 1n }, OutputReference),
      }),
    ).toThrow(/selected deposit/);
    expect(() =>
      watcherOriginalDepositAssets(event(node(), "ee".repeat(32))),
    ).toThrow(/selected deposit/);
  });

  it("refuses structural funds exceeding the actual output", () => {
    const altered = node();
    if (altered.payload === "RootContent" || !("Order" in altered.payload))
      throw new Error("fixture");
    altered.payload.Order.facts.structural_lovelace = 7_000_001n;
    expect(() => watcherOriginalDepositAssets(event(altered))).toThrow(
      /structural ADA/,
    );
  });
});
