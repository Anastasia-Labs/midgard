import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  advanceMidgardLedgerOutputValueV1,
  buildMidgardCekDataTraverseTraceV1,
  buildMidgardLedgerOutputValueTraceV1,
  buildMidgardValidationMerkleMembershipV1,
  emptyMidgardCekDataPairSummaryV1,
  encodeMidgardLedgerOutputValueControlV1,
  finalizeMidgardCekDataTraverseV1,
  finalizeMidgardLedgerOutputValueV1,
  hashMidgardLedgerOutputAssetLeafV1,
  type MidgardCekDataSummaryV1,
  type MidgardLedgerOutputAssetV1,
  prependMidgardCekDataPairSummaryV1,
  summarizeMidgardCekMapDataV1,
} from "../src/index.js";

const assets: readonly MidgardLedgerOutputAssetV1[] = [
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
  readonly entries: readonly MidgardLedgerOutputAssetV1[];
}): MidgardCekDataSummaryV1 => {
  const summarizeScalar = (value: string | bigint): MidgardCekDataSummaryV1 => {
    const cbor = Buffer.from(Data.to(value as never), "hex");
    return finalizeMidgardCekDataTraverseV1(
      buildMidgardCekDataTraverseTraceV1({
        sourceStart: 0,
        source: cbor,
      }).terminal,
    )!;
  };
  const policies = new Map<string, MidgardLedgerOutputAssetV1[]>();
  for (const { policyId, assetName, quantity } of entries) {
    const policy = policyId.toString("hex");
    const policyAssets = policies.get(policy) ?? [];
    policyAssets.push({ policyId, assetName, quantity });
    policies.set(policy, policyAssets);
  }
  let valueEntries = emptyMidgardCekDataPairSummaryV1();
  const policyEntries = [...policies.entries()];
  for (
    let policyIndex = policyEntries.length - 1;
    policyIndex >= 0;
    policyIndex -= 1
  ) {
    const [policy, policyAssets] = policyEntries[policyIndex]!;
    let assetEntries = emptyMidgardCekDataPairSummaryV1();
    for (
      let assetIndex = policyAssets.length - 1;
      assetIndex >= 0;
      assetIndex -= 1
    ) {
      const asset = policyAssets[assetIndex]!;
      assetEntries = prependMidgardCekDataPairSummaryV1(
        summarizeScalar(asset.assetName.toString("hex")),
        summarizeScalar(asset.quantity),
        assetEntries,
      );
    }
    valueEntries = prependMidgardCekDataPairSummaryV1(
      summarizeScalar(policy),
      summarizeMidgardCekMapDataV1(assetEntries),
      valueEntries,
    );
  }
  if (lovelace !== 0n) {
    const emptyBytes = summarizeScalar("");
    valueEntries = prependMidgardCekDataPairSummaryV1(
      emptyBytes,
      summarizeMidgardCekMapDataV1(
        prependMidgardCekDataPairSummaryV1(
          emptyBytes,
          summarizeScalar(lovelace),
          emptyMidgardCekDataPairSummaryV1(),
        ),
      ),
      valueEntries,
    );
  }
  return summarizeMidgardCekMapDataV1(valueEntries);
};

describe("streamed ledger output Value V1", () => {
  it("folds coin and multi-policy assets into the exact Plutus Value", () => {
    const trace = buildMidgardLedgerOutputValueTraceV1({
      assets,
      lovelace: 8_000_000n,
    });

    expect(finalizeMidgardLedgerOutputValueV1(trace.terminal)).toStrictEqual(
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
    const trace = buildMidgardLedgerOutputValueTraceV1({
      assets: [],
      lovelace: 0n,
    });

    expect(finalizeMidgardLedgerOutputValueV1(trace.terminal)).toStrictEqual(
      expectedValueSummary({ lovelace: 0n, entries: [] }),
    );
  });

  it("fails closed for substituted facts and membership paths", () => {
    const trace = buildMidgardLedgerOutputValueTraceV1({
      assets,
      lovelace: 8_000_000n,
    });
    const first = trace.steps[0]!;
    const second = trace.steps[1]!;
    expect(first.witness).not.toBeNull();
    expect(second.witness).not.toBeNull();

    expect(
      advanceMidgardLedgerOutputValueV1({
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
      advanceMidgardLedgerOutputValueV1({
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
      (_, index): MidgardLedgerOutputAssetV1 => {
        const name = Buffer.alloc(2);
        name.writeUInt16BE(index);
        return {
          policyId: Buffer.alloc(28, 0x44),
          assetName: name,
          quantity: BigInt(index + 1),
        };
      },
    );
    const trace = buildMidgardLedgerOutputValueTraceV1({
      assets: manyAssets,
      lovelace: 1n,
    });

    expect(trace.steps).toHaveLength(302);
    expect(finalizeMidgardLedgerOutputValueV1(trace.terminal)).toStrictEqual(
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
    const trace = buildMidgardLedgerOutputValueTraceV1({
      assets,
      lovelace: 8_000_000n,
    });
    const leaves = assets.map(hashMidgardLedgerOutputAssetLeafV1);
    const membership = buildMidgardValidationMerkleMembershipV1(leaves, 2);

    expect(membership.frontier).toStrictEqual(trace.frontier);
    expect(
      encodeMidgardLedgerOutputValueControlV1(trace.terminal).toString("hex"),
    ).toBe(
      "8701020040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000d8799f835820f6d68f04ebaaf198c28e605965e60a233a8a46428e2284e19f344f6290e7464d18591888ff",
    );
  });
});
