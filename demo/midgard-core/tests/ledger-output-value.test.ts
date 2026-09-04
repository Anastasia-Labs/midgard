import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  advanceMidgardLedgerOutputValue,
  buildMidgardCekDataTraverseTrace,
  buildMidgardLedgerOutputValueTrace,
  buildMidgardValidationMerkleMembership,
  emptyMidgardCekDataPairSummary,
  encodeMidgardLedgerOutputValueControl,
  finalizeMidgardCekDataTraverse,
  finalizeMidgardLedgerOutputValue,
  hashMidgardLedgerOutputAssetLeaf,
  type MidgardCekDataSummary,
  type MidgardLedgerOutputAsset,
  prependMidgardCekDataPairSummary,
  summarizeMidgardCekMapData,
} from "../src/index.js";

const assets: readonly MidgardLedgerOutputAsset[] = [
  {
    policyId: Buffer.alloc(28, 0x11),
    assetName: Buffer.alloc(0),
    quantity: 1n,
  },
  {
    policyId: Buffer.alloc(28, 0x11),
    assetName: Buffer.from("aa", "hex"),
    quantity: 42n,
  },
  {
    policyId: Buffer.alloc(28, 0x22),
    assetName: Buffer.from("bbcc", "hex"),
    quantity: 0xffff_ffff_ffff_ffffn,
  },
];

const expectedValueSummary = ({
  lovelace,
  entries,
}: {
  readonly lovelace: bigint;
  readonly entries: readonly MidgardLedgerOutputAsset[];
}): MidgardCekDataSummary => {
  const summarizeScalar = (value: string | bigint): MidgardCekDataSummary => {
    const cbor = Buffer.from(Data.to(value as never), "hex");
    return finalizeMidgardCekDataTraverse(
      buildMidgardCekDataTraverseTrace({
        sourceStart: 0,
        source: cbor,
      }).terminal,
    )!;
  };
  const policies = new Map<string, MidgardLedgerOutputAsset[]>();
  for (const { policyId, assetName, quantity } of entries) {
    const policy = policyId.toString("hex");
    const policyAssets = policies.get(policy) ?? [];
    policyAssets.push({ policyId, assetName, quantity });
    policies.set(policy, policyAssets);
  }
  let valueEntries = emptyMidgardCekDataPairSummary();
  const policyEntries = [...policies.entries()];
  for (
    let policyIndex = policyEntries.length - 1;
    policyIndex >= 0;
    policyIndex -= 1
  ) {
    const [policy, policyAssets] = policyEntries[policyIndex]!;
    let assetEntries = emptyMidgardCekDataPairSummary();
    for (
      let assetIndex = policyAssets.length - 1;
      assetIndex >= 0;
      assetIndex -= 1
    ) {
      const asset = policyAssets[assetIndex]!;
      assetEntries = prependMidgardCekDataPairSummary(
        summarizeScalar(asset.assetName.toString("hex")),
        summarizeScalar(asset.quantity),
        assetEntries,
      );
    }
    valueEntries = prependMidgardCekDataPairSummary(
      summarizeScalar(policy),
      summarizeMidgardCekMapData(assetEntries),
      valueEntries,
    );
  }
  if (lovelace !== 0n) {
    const emptyBytes = summarizeScalar("");
    valueEntries = prependMidgardCekDataPairSummary(
      emptyBytes,
      summarizeMidgardCekMapData(
        prependMidgardCekDataPairSummary(
          emptyBytes,
          summarizeScalar(lovelace),
          emptyMidgardCekDataPairSummary(),
        ),
      ),
      valueEntries,
    );
  }
  return summarizeMidgardCekMapData(valueEntries);
};

describe("streamed ledger output Value V1", () => {
  it("folds coin and multi-policy assets into the exact Plutus Value", () => {
    const trace = buildMidgardLedgerOutputValueTrace({
      assets,
      lovelace: 8_000_000n,
    });

    expect(finalizeMidgardLedgerOutputValue(trace.terminal)).toStrictEqual(
      expectedValueSummary({
        lovelace: 8_000_000n,
        entries: assets,
      }),
    );
    expect(trace.steps).toHaveLength(assets.length + 2);
    expect(trace.steps[0]!.witness).toMatchObject({
      policyId: assets[2]!.policyId,
      assetName: assets[2]!.assetName,
      quantity: assets[2]!.quantity,
    });
  });

  it("supports the empty Value map when coin and assets are absent", () => {
    const trace = buildMidgardLedgerOutputValueTrace({
      assets: [],
      lovelace: 0n,
    });

    expect(finalizeMidgardLedgerOutputValue(trace.terminal)).toStrictEqual(
      expectedValueSummary({ lovelace: 0n, entries: [] }),
    );
  });

  it("fails closed for substituted facts and membership paths", () => {
    const trace = buildMidgardLedgerOutputValueTrace({
      assets,
      lovelace: 8_000_000n,
    });
    const first = trace.steps[0]!;
    const second = trace.steps[1]!;
    expect(first.witness).not.toBeNull();
    expect(second.witness).not.toBeNull();

    expect(
      advanceMidgardLedgerOutputValue({
        control: first.control,
        assetFrontier: trace.frontier,
        lovelace: 8_000_000n,
        witness: {
          ...first.witness!,
          quantity: first.witness!.quantity - 1n,
        },
      }),
    ).toBeNull();
    expect(
      advanceMidgardLedgerOutputValue({
        control: second.control,
        assetFrontier: trace.frontier,
        lovelace: 8_000_000n,
        witness: {
          ...second.witness!,
          siblings: second.witness!.siblings.map((sibling, index) =>
            index === 0 ? Buffer.alloc(32, 0x99) : sibling,
          ),
        },
      }),
    ).toBeNull();
  });

  it("uses one bounded membership reveal per asset without a Value cap", () => {
    const manyAssets = Array.from(
      { length: 300 },
      (_, index): MidgardLedgerOutputAsset => {
        const name = Buffer.alloc(2);
        name.writeUInt16BE(index);
        return {
          policyId: Buffer.alloc(28, 0x44),
          assetName: name,
          quantity: BigInt(index + 1),
        };
      },
    );
    const trace = buildMidgardLedgerOutputValueTrace({
      assets: manyAssets,
      lovelace: 1n,
    });

    expect(trace.steps).toHaveLength(302);
    expect(finalizeMidgardLedgerOutputValue(trace.terminal)).toStrictEqual(
      expectedValueSummary({
        lovelace: 1n,
        entries: manyAssets,
      }),
    );
    expect(
      Math.max(
        ...trace.steps.map(({ witness }) =>
          witness === null
            ? 0
            : 28 + witness.assetName.length + 9 + witness.siblings.length * 32,
        ),
      ),
    ).toBeLessThan(16_384);
  });

  it("pins the terminal control and frontier inputs", () => {
    const trace = buildMidgardLedgerOutputValueTrace({
      assets,
      lovelace: 8_000_000n,
    });
    const leaves = assets.map(hashMidgardLedgerOutputAssetLeaf);
    const membership = buildMidgardValidationMerkleMembership(leaves, 2);

    expect(membership.frontier).toStrictEqual(trace.frontier);
    expect(
      encodeMidgardLedgerOutputValueControl(trace.terminal).toString("hex"),
    ).toBe(
      "8701020040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000d8799f835820f6d68f04ebaaf198c28e605965e60a233a8a46428e2284e19f344f6290e7464d18591888ff",
    );
  });
});
