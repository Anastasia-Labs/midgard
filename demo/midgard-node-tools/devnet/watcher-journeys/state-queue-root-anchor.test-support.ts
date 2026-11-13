/** L1 fixtures for the state queue root anchor tests. */
import * as SDK from "@al-ft/midgard-sdk";
import { toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  header,
  stateQueueAddress,
  stateQueuePolicyId,
  transaction,
} from "midgard-watcher/tests/published-da-target-consumption.scenario";

import type { QueueRootState } from "./state-queue-root-anchor.js";

export const stateQueue = {
  spendingScriptAddress: stateQueueAddress,
  policyId: stateQueuePolicyId,
};
export const headerHash = Effect.runSync(SDK.hashBlockHeader(header));
export const otherHash = "ab".repeat(28);
export const olderHash = "e5".repeat(28);
const rootUnit = toUnit(stateQueuePolicyId, SDK.STATE_QUEUE_ROOT_ASSET_NAME);
export const nodeUnit = (hash: string) =>
  toUnit(stateQueuePolicyId, SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + hash);
const link = (hash: string | null): SDK.LinkedListNodeView["next"] =>
  hash === null ? "Empty" : { Key: { key: hash } };

let nonce = 0;
const nextTxHash = () => (++nonce).toString(16).padStart(64, "0");

/** A node output holding `body` under the key `hash`. */
export const nodeOutput = (
  hash: string,
  next: string | null,
  body: SDK.Header = header,
  attestation: SDK.StateQueueNode["da_attestation"] = SDK.NO_DA_ATTESTATION,
): UTxO => ({
  txHash: nextTxHash(),
  outputIndex: 0,
  address: stateQueueAddress,
  assets: { lovelace: 5_000_000n, [nodeUnit(hash)]: 1n },
  datum: SDK.encodeLinkedListNodeView({
    key: link(hash),
    next: link(next),
    data: SDK.castStateQueueNodeToData({
      proven_fraud: null,
      header: body,
      block_time: body.endTime,
      da_attestation: attestation,
    }) as SDK.LinkedListNodeView["data"],
  }),
});

/** The root holding the confirmed state of `hash`, linked to `next`. */
export const rootOutput = (hash: string, next: string | null): UTxO => ({
  txHash: nextTxHash(),
  outputIndex: 0,
  address: stateQueueAddress,
  assets: { lovelace: 5_000_000n, [rootUnit]: 1n },
  datum: SDK.encodeLinkedListNodeView({
    key: "Empty",
    next: link(next),
    data: SDK.castConfirmedStateToData({
      headerHash: hash,
      prevHeaderHash: header.prevHeaderHash,
      utxoRoot: header.utxosRoot,
      startTime: header.startTime,
      endTime: header.endTime,
      protocolVersion: header.protocolVersion,
    }) as SDK.LinkedListNodeView["data"],
  }),
});

/** The root after `header`'s block merged. */
export const mergedRoot: QueueRootState = {
  headerHash,
  prevHeaderHash: header.prevHeaderHash,
  utxoRoot: header.utxosRoot,
  endTime: header.endTime,
};

export const ATTESTED: SDK.StateQueueNode["da_attestation"] = {
  Attested: { commitment_hash: "ee".repeat(32) },
};

export const kupoUrl = "http://kupo.test";

/**
 * A block's life on a fake L1: its commit's unattested node, the
 * attestation that replaced it with `attestedNode`, and the merge that spent
 * that and left `mergeOutputs`, by default the root holding the block. Kupo
 * lists the node's spends most recent first, as it does: in slots 9 and 3,
 * or both in slot 9.
 */
export const mergedHistory = (
  options: {
    hash?: string;
    attestedNode?: UTxO;
    attestValid?: boolean;
    mergeValid?: boolean;
    mergeSkipsNode?: boolean;
    mergeOutputs?: UTxO[];
    sameSlot?: boolean;
  } = {},
) => {
  const hash = options.hash ?? headerHash;
  const commit = transaction({
    outputs: [rootOutput(olderHash, hash), nodeOutput(hash, null)],
  });
  const attest = transaction({
    inputs: [{ txHash: commit.txHash, outputIndex: 1 }],
    outputs: [options.attestedNode ?? nodeOutput(hash, null, header, ATTESTED)],
    valid: options.attestValid ?? true,
  });
  const merge = transaction({
    inputs: [
      options.mergeSkipsNode === true
        ? { txHash: "c2".repeat(32), outputIndex: 0 }
        : { txHash: attest.txHash, outputIndex: 0 },
    ],
    outputs: options.mergeOutputs ?? [rootOutput(hash, null)],
    valid: options.mergeValid ?? true,
  });
  const spends = [
    [attest.txHash, 0, merge.txHash, 9],
    [commit.txHash, 1, attest.txHash, options.sameSlot === true ? 9 : 3],
  ] as const;
  const bodies = new Map(
    [commit, attest, merge].map(({ txHash, tx }) => [txHash, tx.to_cbor_hex()]),
  );
  const nodeSpends = `${kupoUrl}/matches/${stateQueuePolicyId}.${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${hash}?spent`;
  return {
    commitTxHash: commit.txHash,
    attestTxHash: attest.txHash,
    lookup: {
      kupoUrl,
      stateQueue,
      readConfirmedTransaction: async (txHash: string) => {
        const cbor = bodies.get(txHash);
        if (cbor === undefined) throw new Error(`No transaction ${txHash}`);
        return { cbor };
      },
      // Kupo's reader asks by URL string.
      fetchImpl: (async (url: Parameters<typeof fetch>[0]) =>
        Response.json(
          url !== nodeSpends
            ? []
            : spends.map(([txHash, index, spender, slot]) => ({
                transaction_id: txHash,
                output_index: index,
                spent_at: { transaction_id: spender, slot_no: slot },
              })),
        )) as typeof fetch,
    },
  };
};
