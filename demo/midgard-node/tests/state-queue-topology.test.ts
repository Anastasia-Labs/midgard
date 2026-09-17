import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  fetchCanonicalStateQueueNodesProgram,
  fetchStateQueueSnapshotProgram,
  summarizeStateQueueTopology,
} from "../src/services/state-queue-topology.js";

const policyId = "aa".repeat(28);
const stateQueueAddress =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";
const validator = {
  policyId,
  spendingScriptAddress: stateQueueAddress,
} as SDK.AuthenticatedValidator;

const linkedUtxo = ({
  assetName,
  key,
  next,
  txByte,
  data = "00" as SDK.LinkedListNodeView["data"],
}: {
  readonly assetName: string;
  readonly key: SDK.LinkedListNodeView["key"];
  readonly next: SDK.LinkedListNodeView["next"];
  readonly txByte: string;
  readonly data?: SDK.LinkedListNodeView["data"];
}): UTxO => {
  const datum: SDK.LinkedListNodeView = {
    key,
    next,
    data,
  };
  return {
    txHash: txByte.repeat(32),
    outputIndex: 0,
    address: stateQueueAddress,
    assets: {
      lovelace: 3_000_000n,
      [toUnit(policyId, assetName)]: 1n,
    },
    datum: SDK.encodeLinkedListNodeView(datum),
  };
};

const exactLucid = (byUnit: ReadonlyMap<string, readonly UTxO[]>) => {
  const utxosAt = vi.fn(() =>
    Promise.reject(new Error("address-wide lookup must not be called")),
  );
  const utxosAtWithUnit = vi.fn((_address: string, unit: string) =>
    Promise.resolve([...(byUnit.get(unit) ?? [])]),
  );
  return {
    api: { utxosAt, utxosAtWithUnit } as unknown as LucidEvolution,
    utxosAt,
    utxosAtWithUnit,
  };
};

const mkNode = (
  key: SDK.LinkedListNodeView["key"],
  next: SDK.LinkedListNodeView["next"],
): SDK.StateQueueUTxO =>
  ({
    utxo: {
      txHash: "00".repeat(32),
      outputIndex: 0,
      address: "addr_test1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq",
      assets: {},
    },
    datum: {
      key,
      next,
      data: "00",
    } as SDK.LinkedListNodeView,
    assetName: "4e6f6465",
  }) as SDK.StateQueueUTxO;

describe("state queue topology", () => {
  it("reports uninitialized topology when no policy UTxOs are present", () => {
    const topology = summarizeStateQueueTopology(0, []);
    expect(topology.initialized).toBe(false);
    expect(topology.healthy).toBe(false);
    expect(topology.reason).toBeUndefined();
  });

  it("accepts a single-node queue as healthy", () => {
    const topology = summarizeStateQueueTopology(1, [mkNode("Empty", "Empty")]);
    expect(topology.initialized).toBe(true);
    expect(topology.healthy).toBe(true);
    expect(topology.rootCount).toBe(1);
    expect(topology.tailCount).toBe(1);
    expect(topology.reason).toBeUndefined();
  });

  it("flags duplicate roots/tails as unhealthy", () => {
    const topology = summarizeStateQueueTopology(2, [
      mkNode("Empty", "Empty"),
      mkNode("Empty", "Empty"),
    ]);
    expect(topology.initialized).toBe(true);
    expect(topology.healthy).toBe(false);
    expect(topology.reason).toContain("root");
  });

  it("flags non-decodable policy UTxOs", () => {
    const topology = summarizeStateQueueTopology(3, [
      mkNode("Empty", "Empty"),
      mkNode({ Key: { key: "11".repeat(28) } }, "Empty"),
    ]);
    expect(topology.invalidNodeCount).toBe(1);
    expect(topology.healthy).toBe(false);
    expect(topology.reason).toContain("non-decodable");
  });

  it("walks the canonical queue by exact linked NFT units for commit preflight", async () => {
    const headerHash = "11".repeat(28);
    const tailAssetName = SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash;
    const header: SDK.Header = {
      prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      utxosRoot: "77".repeat(32),
      withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      withdrawalCount: 0n,
      forcedTransactionCount: 0n,
      l2TransactionCount: 0n,
      depositCount: 0n,
      totalEventCount: 0n,
      transitionStepCount: 0n,
      validationTraceCount: 0n,
      startTime: 1_000n,
      endTime: 2_000n,
      blockSlot: 0n,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      prevHeaderHash: "88".repeat(28),
      operatorVkey: "99".repeat(28),
      protocolVersion: 1n,
    };
    const root = linkedUtxo({
      assetName: SDK.STATE_QUEUE_ROOT_ASSET_NAME,
      key: "Empty",
      next: { Key: { key: headerHash } },
      txByte: "22",
      data: SDK.castConfirmedStateToData({
        headerHash: "aa".repeat(28),
        prevHeaderHash: "bb".repeat(28),
        utxoRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        startTime: 0n,
        endTime: 0n,
        protocolVersion: 1n,
      }) as SDK.LinkedListNodeView["data"],
    });
    const tail = linkedUtxo({
      assetName: tailAssetName,
      key: { Key: { key: headerHash } },
      next: "Empty",
      txByte: "33",
      data: SDK.castStateQueueNodeToData({
        header,
        da_attestation: SDK.NO_DA_ATTESTATION,
      }) as SDK.LinkedListNodeView["data"],
    });
    const lucid = exactLucid(
      new Map([
        [toUnit(policyId, SDK.STATE_QUEUE_ROOT_ASSET_NAME), [root]],
        [toUnit(policyId, tailAssetName), [tail]],
      ]),
    );

    const nodes = await Effect.runPromise(
      fetchCanonicalStateQueueNodesProgram(lucid.api, validator),
    );
    expect(nodes.map((node) => node.assetName)).toEqual([
      SDK.STATE_QUEUE_ROOT_ASSET_NAME,
      tailAssetName,
    ]);
    expect(lucid.utxosAtWithUnit).toHaveBeenCalledTimes(2);
    expect(lucid.utxosAt).not.toHaveBeenCalled();

    const snapshot = await Effect.runPromise(
      fetchStateQueueSnapshotProgram(lucid.api, validator, "commit_preflight"),
    );
    expect(snapshot.root.outRef).toBe(`${root.txHash}#0`);
    expect(snapshot.tailCommitBase.outRef).toBe(`${tail.txHash}#0`);
    expect(lucid.utxosAt).not.toHaveBeenCalled();
  });

  it("fails closed when a linked unit is missing, duplicated, or cyclic", async () => {
    const headerHash = "44".repeat(28);
    const tailAssetName = SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash;
    const root = linkedUtxo({
      assetName: SDK.STATE_QUEUE_ROOT_ASSET_NAME,
      key: "Empty",
      next: { Key: { key: headerHash } },
      txByte: "55",
    });
    const cyclicTail = linkedUtxo({
      assetName: tailAssetName,
      key: { Key: { key: headerHash } },
      next: { Key: { key: headerHash } },
      txByte: "66",
    });
    const rootUnit = toUnit(policyId, SDK.STATE_QUEUE_ROOT_ASSET_NAME);
    const tailUnit = toUnit(policyId, tailAssetName);

    const missing = exactLucid(new Map([[rootUnit, [root]]]));
    const missingOutcome = await Effect.runPromise(
      Effect.either(
        fetchCanonicalStateQueueNodesProgram(missing.api, validator),
      ),
    );
    expect(missingOutcome).toMatchObject({
      _tag: "Left",
      left: {
        _tag: "StateQueueError",
        message: "State-queue linked-list unit is missing or not unique",
      },
    });

    const duplicate = exactLucid(
      new Map([
        [rootUnit, [root]],
        [tailUnit, [cyclicTail, cyclicTail]],
      ]),
    );
    const duplicateOutcome = await Effect.runPromise(
      Effect.either(
        fetchCanonicalStateQueueNodesProgram(duplicate.api, validator),
      ),
    );
    expect(duplicateOutcome).toMatchObject({
      _tag: "Left",
      left: {
        _tag: "StateQueueError",
        message: "State-queue linked-list unit is missing or not unique",
      },
    });

    const cyclic = exactLucid(
      new Map([
        [rootUnit, [root]],
        [tailUnit, [cyclicTail]],
      ]),
    );
    const cyclicOutcome = await Effect.runPromise(
      Effect.either(
        fetchCanonicalStateQueueNodesProgram(cyclic.api, validator),
      ),
    );
    expect(cyclicOutcome).toMatchObject({
      _tag: "Left",
      left: {
        _tag: "StateQueueError",
        message: "Cannot derive state-queue snapshot from cyclic topology",
      },
    });
  });
});
