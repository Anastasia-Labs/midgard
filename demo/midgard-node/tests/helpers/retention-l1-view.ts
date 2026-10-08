import * as SDK from "@al-ft/midgard-sdk";
import { toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { MidgardContracts } from "../../src/services/index.js";
import { seedLandedStateQueue } from "./landed-state-queue.js";

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
 * A landed state queue (P1 facts): a `ConfirmedState` root whose datum names
 * `confirmedHeadHash`, followed by one header node per `liveUtxosRoots` entry
 * (the last one carrying an attestation marker, so no status filter can pass
 * unnoticed). `provide` seeds those facts in the node database and provides
 * the contracts the retention L1 view reads; it returns the live header
 * hashes the view must report.
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
  const utxos: UTxO[] = [];
  const put = (assetName: string, datum: SDK.LinkedListNodeView, i: number) =>
    utxos.push(linkedUtxo(assetName, datum, (i + 16).toString(16)));
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
              ? { Attested: { commitment_hash: "33".repeat(32) } }
              : SDK.NO_DA_ATTESTATION,
        }) as SDK.LinkedListNodeView["data"],
      },
      index + 1,
    );
  });
  const stateQueue = { policyId, spendingScriptAddress: stateQueueAddress };
  const provide = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
    Effect.zipRight(seedLandedStateQueue(stateQueue, utxos), effect).pipe(
      Effect.provideService(MidgardContracts, { stateQueue } as never),
    );
  return { liveHeaderHashes, provide };
};
