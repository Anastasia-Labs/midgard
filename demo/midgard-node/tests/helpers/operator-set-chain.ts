/**
 * The operator directory on a simulated chain (NC14 tests): the three
 * operator lists, the scheduler and the hub oracle of a real deployment as
 * `SimOutput`s with their real datums, and the list transactions the
 * operator lifecycle submits (insert, remove, update in place), each as
 * parts one `SimTx` joins.
 *
 * Every output this module builds is remembered (by identity, as `SimChain`
 * keeps it), so a list read over `chain.live()` needs no datum decoding.
 */
import type { OutRef } from "@al-ft/midgard-l1-follower";
import type {
  SimOutput,
  SimTx,
  SimUtxo,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type Data as PlutusData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type OperatorListContract,
  type OperatorSetConfig,
  operatorSetConfig,
  type OperatorSetContract,
} from "../../src/l1-operator-set/index.js";
import { loadRealMidgardContractsForTest } from "./real-midgard-contracts.js";

export type OperatorList = "registered" | "active" | "retired";

export const OPERATOR_LISTS: readonly OperatorList[] = [
  "registered",
  "active",
  "retired",
];

export type OperatorSetChainFixture = Readonly<{
  config: OperatorSetConfig;
  /** The hub oracle's datum (Plutus data CBOR). */
  hubOracleDatum: Buffer;
}>;

/** The operator set's config and hub oracle datum of a real deployment. */
export const loadOperatorSetChainFixture =
  async (): Promise<OperatorSetChainFixture> => {
    const contracts = await loadRealMidgardContractsForTest({
      txHash: "5e".repeat(32),
      outputIndex: 0,
    });
    const hub = await Effect.runPromise(SDK.makeHubOracleDatum(contracts));
    return {
      config: operatorSetConfig(contracts),
      hubOracleDatum: Buffer.from(
        Data.to<SDK.HubOracleDatum>(hub, SDK.HubOracleDatum),
        "hex",
      ),
    };
  };

/** A 28-byte key, every byte `byte`. */
export const operatorKey = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(28);

type NodeInfo = Readonly<{
  list: OperatorList;
  key: string | null;
  next: string | null;
  /** The node's data (Plutus data); for an active node, its strikes. */
  strikes: bigint;
}>;

const nodeInfo = new WeakMap<SimOutput, NodeInfo>();

export type ListNode = Readonly<{ utxo: SimUtxo; info: NodeInfo }>;

/** One transaction's inputs, outputs and mint (`txOf` joins them). */
export type TxParts = Readonly<{
  inputs: readonly OutRef[];
  outputs: readonly SimOutput[];
  mint: readonly (readonly [string, string, bigint])[];
}>;

/** One `SimTx` of `parts`. */
export const txOf = (parts: readonly TxParts[], nonce: number): SimTx => {
  const mint = new Map<string, Map<string, bigint>>();
  for (const [policy, name, quantity] of parts.flatMap((part) => part.mint)) {
    const names = mint.get(policy) ?? new Map<string, bigint>();
    names.set(name, (names.get(name) ?? 0n) + quantity);
    mint.set(policy, names);
  }
  return {
    inputs: parts.flatMap((part) => part.inputs),
    outputs: parts.flatMap((part) => part.outputs),
    ...(mint.size === 0 ? {} : { mint }),
    nonce,
  };
};

const LOVELACE = 5_000_000n;

const beaconed = (
  contract: OperatorSetContract,
  assetName: string,
  datum: string,
): SimOutput => ({
  address: Buffer.from(contract.address, "hex"),
  lovelace: LOVELACE,
  assets: new Map([[contract.policyId, new Map([[assetName, 1n]])]]),
  datum: Buffer.from(datum, "hex"),
});

const nodeData = (
  list: OperatorList,
  key: string,
  strikes: bigint,
): PlutusData => {
  switch (list) {
    case "registered":
      return Data.from(
        Data.to<SDK.RegisteredOperatorDatum>(
          { operator: key },
          SDK.RegisteredOperatorDatum,
        ),
      );
    case "active":
      return Data.from(
        Data.to<SDK.ActiveOperatorDatum>(
          { bond_unlock_time: null, inactivity_strikes: strikes },
          SDK.ActiveOperatorDatum,
        ),
      );
    case "retired":
      return Data.from(
        Data.to<SDK.RetiredOperatorDatum>(
          { bond_unlock_time: null },
          SDK.RetiredOperatorDatum,
        ),
      );
  }
};

export class OperatorSetChain {
  constructor(readonly fixture: OperatorSetChainFixture) {}

  contract(list: OperatorList): OperatorListContract {
    return this.fixture.config[list];
  }

  assetName(list: OperatorList, key: string | null): string {
    const contract = this.contract(list);
    return key === null
      ? contract.rootAssetName
      : `${contract.nodePrefix}${key}`;
  }

  /** A list node output (the root for a null key). */
  node(
    list: OperatorList,
    key: string | null,
    next: string | null,
    strikes = 0n,
  ): SimOutput {
    const output = beaconed(
      this.contract(list),
      this.assetName(list, key),
      SDK.encodeLinkedListNodeView({
        key: key === null ? "Empty" : { Key: { key } },
        next: next === null ? "Empty" : { Key: { key: next } },
        data: key === null ? "" : nodeData(list, key, strikes),
      }),
    );
    nodeInfo.set(output, { list, key, next, strikes });
    return output;
  }

  scheduler(operator: string | null, startTime = 0n): SimOutput {
    return beaconed(
      this.fixture.config.scheduler,
      SDK.SCHEDULER_ASSET_NAME,
      Data.to<SDK.SchedulerDatum>(
        operator === null
          ? "NoActiveOperators"
          : { ActiveOperator: { operator, start_time: startTime } },
        SDK.SchedulerDatum,
      ),
    );
  }

  hubOracle(): SimOutput {
    return beaconed(
      this.fixture.config.hubOracle,
      SDK.HUB_ORACLE_ASSET_NAME,
      this.fixture.hubOracleDatum.toString("hex"),
    );
  }

  /** The deployment: three empty lists, the scheduler and the hub oracle. */
  genesis(funding: OutRef): TxParts {
    return {
      inputs: [funding],
      outputs: [
        ...OPERATOR_LISTS.map((list) => this.node(list, null, null)),
        this.scheduler(null),
        this.hubOracle(),
      ],
      mint: [],
    };
  }

  /** The live nodes of `list`, root first then by key. */
  nodes(live: readonly SimUtxo[], list: OperatorList): ListNode[] {
    return live
      .flatMap((utxo) => {
        const info = nodeInfo.get(utxo.output);
        return info?.list === list ? [{ utxo, info }] : [];
      })
      .sort((a, b) =>
        a.info.key === null
          ? -1
          : b.info.key === null
            ? 1
            : a.info.key < b.info.key
              ? -1
              : 1,
      );
  }

  find(
    live: readonly SimUtxo[],
    list: OperatorList,
    key: string,
  ): ListNode | undefined {
    return this.nodes(live, list).find((node) => node.info.key === key);
  }

  liveScheduler(live: readonly SimUtxo[]): SimUtxo | undefined {
    const policy = this.fixture.config.scheduler.policyId;
    return live.find((utxo) => utxo.output.assets?.has(policy) === true);
  }

  /** Inserts `key` after its predecessor (the root, or the greatest lower key). */
  insert(
    live: readonly SimUtxo[],
    list: OperatorList,
    key: string,
    strikes = 0n,
  ): TxParts {
    const before = this.nodes(live, list).filter(
      (node) => node.info.key === null || node.info.key < key,
    );
    const anchor = before[before.length - 1];
    if (anchor === undefined) throw new Error(`${list}: no root`);
    return {
      inputs: [anchor.utxo.outRef],
      outputs: [
        this.node(list, anchor.info.key, key, anchor.info.strikes),
        this.node(list, key, anchor.info.next, strikes),
      ],
      mint: [[this.contract(list).policyId, this.assetName(list, key), 1n]],
    };
  }

  /** Removes `key`, relinking its predecessor; its token is burned. */
  remove(live: readonly SimUtxo[], list: OperatorList, key: string): TxParts {
    const nodes = this.nodes(live, list);
    const target = nodes.find((node) => node.info.key === key);
    const anchor = nodes.find((node) => node.info.next === key);
    if (target === undefined || anchor === undefined)
      throw new Error(`${list}: ${key} is not linked`);
    return {
      inputs: [anchor.utxo.outRef, target.utxo.outRef],
      outputs: [
        this.node(list, anchor.info.key, target.info.next, anchor.info.strikes),
      ],
      mint: [[this.contract(list).policyId, this.assetName(list, key), -1n]],
    };
  }

  /** Spends an active node and recreates it with one more strike. */
  strike(live: readonly SimUtxo[], key: string): TxParts {
    const target = this.find(live, "active", key);
    if (target === undefined) throw new Error(`active: no ${key}`);
    return {
      inputs: [target.utxo.outRef],
      outputs: [
        this.node("active", key, target.info.next, target.info.strikes + 1n),
      ],
      mint: [],
    };
  }

  /** Spends the scheduler and hands the shift to `operator`. */
  shift(
    live: readonly SimUtxo[],
    operator: string | null,
    startTime: bigint,
  ): TxParts {
    const current = this.liveScheduler(live);
    if (current === undefined) throw new Error("no live scheduler");
    return {
      inputs: [current.outRef],
      outputs: [this.scheduler(operator, startTime)],
      mint: [],
    };
  }
}
