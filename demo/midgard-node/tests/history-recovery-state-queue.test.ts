import * as SDK from "@al-ft/midgard-sdk";
import { toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import type { LedgerSnapshotOutput } from "../src/l1-ledger-snapshot.js";
import { validateRecoveryStateQueue } from "../src/services/history-recovery-state-queue.js";

const policyId = "aa".repeat(28);
const address =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";
const contracts: Pick<SDK.MidgardValidators, "stateQueue"> = {
  stateQueue: {
    policyId,
    spendingScriptAddress: address,
  } as SDK.MidgardValidators["stateQueue"],
};
const rootUnit = toUnit(policyId, SDK.STATE_QUEUE_ROOT_ASSET_NAME);
const utxosRoot = "cc".repeat(32);
const nodeDatum = (overrides: Partial<SDK.LinkedListNodeView> = {}): string =>
  SDK.encodeLinkedListNodeView({
    key: "Empty",
    next: "Empty",
    data: SDK.castConfirmedStateToData({
      headerHash: "11".repeat(28),
      prevHeaderHash: "22".repeat(28),
      utxoRoot: utxosRoot,
      startTime: 0n,
      endTime: 1_000n,
      protocolVersion: 1n,
    }) as SDK.LinkedListNodeView["data"],
    ...overrides,
  });
const root: LedgerSnapshotOutput = {
  txHash: "bb".repeat(32),
  outputIndex: 2,
  address,
  assets: { lovelace: 3_000_000n, [rootUnit]: 1n },
  datum: nodeDatum(),
  hasReferenceScript: false,
};
const expectedBase = {
  outRef: `${root.txHash}#2`,
  datumCbor: nodeDatum(),
  utxosRoot,
};
const unrelated: LedgerSnapshotOutput = {
  ...root,
  txHash: "dd".repeat(32),
  assets: { lovelace: 2_000_000n },
};
const validate = (
  outputs: readonly LedgerSnapshotOutput[],
  base = expectedBase,
) => validateRecoveryStateQueue({ outputs, contracts, expectedBase: base });

describe("recovery state-queue snapshot adapter", () => {
  it("returns the exact confirmed queue UTxO from a complete transport capture", async () => {
    const result = await Effect.runPromise(validate([unrelated, root]));
    const { hasReferenceScript: _hasReferenceScript, ...exactUTxO } = root;
    expect(result.utxosRoot).toBe(utxosRoot);
    expect(result.queueUTxO.utxo).toEqual(exactUTxO);
    expect(result.queueUTxO.assetName).toBe(SDK.STATE_QUEUE_ROOT_ASSET_NAME);
    expect(result.queueUTxO.datum.key).toBe("Empty");
    expect(result.queueUTxO.datum.next).toBe("Empty");
    expect(result.queueUTxO.utxo.assets).not.toBe(root.assets);
  });

  it.each([
    ["missing queue", [unrelated]],
    ["duplicate policy outref", [root, root]],
    ["duplicate unrelated outref", [root, unrelated, unrelated]],
    ["second policy output", [root, { ...root, outputIndex: 3 }]],
    ["foreign address", [{ ...root, address: "foreign-address" }]],
    [
      "foreign policy",
      [
        {
          ...root,
          assets: {
            lovelace: 3_000_000n,
            [toUnit("ee".repeat(28), SDK.STATE_QUEUE_ROOT_ASSET_NAME)]: 1n,
          },
        },
      ],
    ],
    ["reference script", [{ ...root, hasReferenceScript: true }]],
    [
      "hash-only datum",
      [{ ...root, datum: undefined, datumHash: "ff".repeat(32) }],
    ],
    ["inline and hash datum", [{ ...root, datumHash: "ff".repeat(32) }]],
    ["missing inline datum", [{ ...root, datum: undefined }]],
    [
      "wrong NFT quantity",
      [{ ...root, assets: { ...root.assets, [rootUnit]: 2n } }],
    ],
    [
      "zero NFT quantity",
      [{ ...root, assets: { ...root.assets, [rootUnit]: 0n } }],
    ],
    [
      "extra asset",
      [
        {
          ...root,
          assets: { ...root.assets, [toUnit("ee".repeat(28), "01")]: 1n },
        },
      ],
    ],
    [
      "wrong root asset name",
      [
        {
          ...root,
          assets: {
            lovelace: 3_000_000n,
            [toUnit(
              policyId,
              SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + "11".repeat(28),
            )]: 1n,
          },
        },
      ],
    ],
  ] satisfies ReadonlyArray<
    readonly [string, readonly LedgerSnapshotOutput[]]
  >)("rejects %s", async (_label, outputs) => {
    const result = await Effect.runPromise(Effect.either(validate(outputs)));
    expect(result).toMatchObject({
      _tag: "Left",
      left: { _tag: "StateQueueError" },
    });
  });

  it.each([
    [
      "unmerged successor",
      nodeDatum({ next: { Key: { key: "11".repeat(28) } } }),
    ],
    ["nonroot key", nodeDatum({ key: { Key: { key: "11".repeat(28) } } })],
    [
      "malformed confirmed data",
      nodeDatum({ data: "00" as SDK.LinkedListNodeView["data"] }),
    ],
    ["malformed CBOR", "ff"],
  ])(
    "rejects %s even when the expected raw datum matches",
    async (_label, datum) => {
      const result = await Effect.runPromise(
        Effect.either(
          validate([{ ...root, datum }], { ...expectedBase, datumCbor: datum }),
        ),
      );
      expect(result).toMatchObject({
        _tag: "Left",
        left: { _tag: "StateQueueError" },
      });
    },
  );

  it.each([
    ["outref", { ...expectedBase, outRef: `${root.txHash}#3` }],
    [
      "datum bytes",
      { ...expectedBase, datumCbor: `${expectedBase.datumCbor}00` },
    ],
    ["UTxO root", { ...expectedBase, utxosRoot: "ee".repeat(32) }],
  ])("rejects a mismatched expected %s", async (_label, base) => {
    const result = await Effect.runPromise(
      Effect.either(validate([root], base)),
    );
    expect(result).toMatchObject({
      _tag: "Left",
      left: { _tag: "StateQueueError" },
    });
  });
});
