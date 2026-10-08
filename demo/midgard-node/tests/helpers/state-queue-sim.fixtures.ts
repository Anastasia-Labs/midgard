/**
 * State-queue fixtures for the node's P1 tests (N2): the simulator's queue
 * address and policy, V1 headers, root and node datums, and queue outputs.
 */
import {
  type SimOutput,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { StateQueueProjectionConfig } from "../../src/l1-state-queue/index.js";

export const QUEUE_ADDRESS = simUniverse().trackedAddress;
export const QUEUE_POLICY = "71".repeat(28);
export const OTHER_POLICY = "72".repeat(28);
export const ROOT_ASSET = SDK.STATE_QUEUE_ROOT_ASSET_NAME;
export const NODE_PREFIX = SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX;
export const GENESIS_HASH = "00".repeat(28);

export const SIM_QUEUE_CONFIG: StateQueueProjectionConfig = {
  address: QUEUE_ADDRESS.toString("hex"),
  policyId: QUEUE_POLICY,
  maxNodes: 10_000,
};

export const hex32 = (n: number): string => n.toString(16).padStart(64, "0");

export const simHeader = (
  nonce: number,
  prevHeaderHash: string,
): SDK.Header => ({
  prevUtxosRoot: hex32(nonce),
  utxosRoot: hex32(nonce),
  withdrawalsRoot: hex32(nonce),
  forcedTransactionsRoot: hex32(nonce),
  transactionsRoot: hex32(nonce),
  depositsRoot: hex32(nonce),
  transitionTraceRoot: hex32(nonce),
  eventToStepRoot: hex32(nonce),
  validationTracesRoot: hex32(nonce),
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 0n,
  depositCount: 0n,
  totalEventCount: 0n,
  transitionStepCount: 0n,
  validationTraceCount: 0n,
  startTime: BigInt(nonce * 1_000),
  endTime: BigInt(nonce * 1_000 + 1_000),
  blockSlot: BigInt(nonce),
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash,
  operatorVkey: "ab".repeat(28),
  protocolVersion: 1n,
});

const datumOf = (datum: SDK.LinkedListDatum): Buffer =>
  Buffer.from(Data.to(datum, SDK.LinkedListDatum), "hex");

export const rootDatum = (headerHash: string, link: string | null): Buffer =>
  datumOf({
    data: {
      Root: {
        data: Data.castTo(
          {
            headerHash,
            prevHeaderHash: GENESIS_HASH,
            utxoRoot: hex32(0),
            startTime: 0n,
            endTime: 0n,
            protocolVersion: 1n,
          },
          SDK.ConfirmedState,
        ),
      },
    },
    link,
  });

export const nodeDatum = (
  header: SDK.Header,
  status: SDK.DaAvailabilityStateQueueStatus,
  link: string | null,
): Buffer =>
  datumOf({
    data: {
      Node: {
        data: Data.castTo(
          { header, da_attestation: status, proven_fraud: null },
          SDK.StateQueueNode,
        ),
      },
    },
    link,
  });

export const queueOutput = (assetName: string, datum: Buffer): SimOutput => ({
  address: QUEUE_ADDRESS,
  lovelace: 5_000_000n,
  assets: new Map([[QUEUE_POLICY, new Map([[assetName, 1n]])]]),
  datum,
});
