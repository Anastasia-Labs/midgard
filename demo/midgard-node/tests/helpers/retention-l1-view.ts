import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { Lucid, MidgardContracts } from "../../src/services/index.js";

const policyId = "aa".repeat(28);
const stateQueueAddress =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";

const header = (utxosRoot: string): SDK.Header => ({
  prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  utxosRoot,
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
});

const linkedUtxo = (
  assetName: string,
  datum: SDK.LinkedListNodeView,
  txByte: string,
): UTxO => ({
  txHash: txByte.repeat(32),
  outputIndex: 0,
  address: stateQueueAddress,
  assets: { lovelace: 3_000_000n, [toUnit(policyId, assetName)]: 1n },
  datum: SDK.encodeLinkedListNodeView(datum),
});

/**
 * An L1 state queue served by exact-unit lookups: a `ConfirmedState` root whose
 * datum names `confirmedHeadHash`, followed by one header node per
 * `liveUtxosRoots` entry (the last one carrying an attestation marker, so no
 * status filter can pass unnoticed). Returns the Lucid and contract services
 * the retention L1 view reads, and the live header hashes it must report.
 */
export const makeRetentionL1Queue = async ({
  confirmedHeadHash,
  liveUtxosRoots,
}: {
  readonly confirmedHeadHash: string;
  readonly liveUtxosRoots: readonly string[];
}) => {
  const headers = liveUtxosRoots.map(header);
  const liveHeaderHashes = await Promise.all(
    headers.map((value) => Effect.runPromise(SDK.hashBlockHeader(value))),
  );
  const byUnit = new Map<string, UTxO[]>();
  const put = (assetName: string, datum: SDK.LinkedListNodeView, i: number) =>
    byUnit.set(toUnit(policyId, assetName), [
      linkedUtxo(assetName, datum, (i + 16).toString(16)),
    ]);
  const keyOf = (index: number): SDK.LinkedListNodeView["next"] =>
    index < liveHeaderHashes.length
      ? { Key: { key: liveHeaderHashes[index]! } }
      : "Empty";
  put(
    SDK.STATE_QUEUE_ROOT_ASSET_NAME,
    {
      key: "Empty",
      next: keyOf(0),
      data: SDK.castConfirmedStateToData({
        headerHash: confirmedHeadHash,
        prevHeaderHash: "bb".repeat(28),
        utxoRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        startTime: 0n,
        endTime: 0n,
        protocolVersion: 1n,
      }) as SDK.LinkedListNodeView["data"],
    },
    0,
  );
  headers.forEach((value, index) => {
    const headerHash = liveHeaderHashes[index]!;
    put(
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
      {
        key: { Key: { key: headerHash } },
        next: keyOf(index + 1),
        data: SDK.castStateQueueNodeToData({
          proven_fraud: null,
          header: value,
          da_attestation:
            index === headers.length - 1
              ? { Attested: { da_bond_asset_name: "33".repeat(32) } }
              : SDK.NO_DA_ATTESTATION,
        }) as SDK.LinkedListNodeView["data"],
      },
      index + 1,
    );
  });
  const api = {
    utxosAt: () =>
      Promise.reject(new Error("address-wide lookup must not be called")),
    utxosAtWithUnit: (_address: string, unit: string) =>
      Promise.resolve([...(byUnit.get(unit) ?? [])]),
  } as unknown as LucidEvolution;
  const provide = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
    effect.pipe(
      Effect.provideService(Lucid, { api } as never),
      Effect.provideService(MidgardContracts, {
        stateQueue: { policyId, spendingScriptAddress: stateQueueAddress },
      } as never),
    ) as Effect.Effect<A, E, Exclude<R, Lucid | MidgardContracts>>;
  return { liveHeaderHashes, provide };
};
