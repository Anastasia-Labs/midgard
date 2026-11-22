import { MIDGARD_PROTOCOL_VERSION } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKStateQueue from "@al-ft/midgard-sdk/state-queue";
import { credentialToAddress } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  planNextTimeoutCorrection,
  selectTimeoutCorrectionTarget,
} from "../src/remove-unattested-block.reconcile-last-timeout-correction-step.js";
const h = (byte: string) => byte.repeat(28);
const tx = (byte: string) => byte.repeat(32);
const address = credentialToAddress("Preprod", { type: "Key", hash: h("dd") });
const header: SDK.Header = {
  prevUtxosRoot: "55".repeat(32),
  utxosRoot: "55".repeat(32),
  withdrawalsRoot: "55".repeat(32),
  forcedTransactionsRoot: "55".repeat(32),
  transactionsRoot: "55".repeat(32),
  depositsRoot: "55".repeat(32),
  transitionTraceRoot: "55".repeat(32),
  eventToStepRoot: "55".repeat(32),
  validationTracesRoot: "55".repeat(32),
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 0n,
  depositCount: 0n,
  totalEventCount: 0n,
  transitionStepCount: 0n,
  validationTraceCount: 0n,
  startTime: 0n,
  endTime: 1n,
  blockSlot: 1n,
  expectedNetworkId: 0n,
  minFeeA: 44n,
  minFeeB: 155381n,
  prevHeaderHash: "66".repeat(28),
  operatorVkey: "77".repeat(28),
  protocolVersion: BigInt(MIDGARD_PROTOCOL_VERSION),
};
const block = (
  byte: string,
  endTime = 1n,
  attested = false,
): SDKStateQueue.StateQueueUTxO => ({
  assetName: SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + h(byte),
  datum: {
    key: { Key: { key: h(byte) } },
    next: "Empty",
    data: SDK.castStateQueueNodeToData({
      proven_fraud: null,
      header: { ...header, endTime },
      block_time: endTime,
      da_attestation: attested
        ? { Attested: { commitment_hash: tx("ab") } }
        : SDK.NO_DA_ATTESTATION,
    }) as SDK.LinkedListNodeView["data"],
  },
  utxo: {
    txHash: tx(byte),
    outputIndex: 0,
    address,
    assets: { lovelace: 5_000_000n },
  },
});
const queue = (...nodes: SDKStateQueue.StateQueueUTxO[]) => {
  const root: SDKStateQueue.StateQueueUTxO = {
    ...block("aa"),
    assetName: SDKStateQueue.STATE_QUEUE_ROOT_ASSET_NAME,
    datum: { key: "Empty", next: "Empty", data: "d87980" },
  };
  const all = [root, ...nodes].map((node) => ({
    ...node,
    datum: { ...node.datum },
  }));
  for (let i = 0; i < all.length - 1; i++)
    all[i]!.datum.next = all[i + 1]!.datum.key;
  return all;
};
describe("generalized attestation-timeout recovery", () => {
  it("selects and prunes an expired interior target while preserving its attested prefix", async () => {
    const nodes = queue(block("01", 1n, true), block("11"), block("22"));
    expect(
      (await selectTimeoutCorrectionTarget(nodes, 4_000_000n, "Idle"))?.target,
    ).toBe(nodes[2]);
    const plan = planNextTimeoutCorrection(nodes, h("11"));
    expect(plan).toMatchObject({
      kind: "prune-descendant",
      predecessor: nodes[1],
      target: nodes[2],
      removed: nodes[3],
    });
    expect(nodes[0]!.utxo.txHash).toBe(tx("aa"));
    expect(nodes[1]!.utxo.txHash).toBe(tx("01"));
  });
  it("removes a tail through its immediate predecessor without consuming the root", () => {
    const nodes = queue(block("01", 1n, true), block("11"));
    const plan = planNextTimeoutCorrection(nodes, h("11"));
    expect(plan).toMatchObject({
      kind: "remove-block",
      predecessor: nodes[1],
      target: nodes[2],
      removed: nodes[2],
    });
  });
  it("resumes a locked later target ahead of another eligible timeout and rejects foreign locks", async () => {
    const nodes = queue(block("01"), block("11"));
    expect(
      (
        await selectTimeoutCorrectionTarget(nodes, 4_000_000n, {
          Locked: {
            target_header_hash: h("11"),
            correction_identity: "AttestationTimeout",
          },
        })
      )?.target,
    ).toBe(nodes[2]);
    await expect(
      selectTimeoutCorrectionTarget(nodes, 4_000_000n, {
        Locked: {
          target_header_hash: h("11"),
          correction_identity: {
            FraudProof: { fraud_proof_asset_name: tx("bb") },
          },
        },
      }),
    ).rejects.toThrow("another correction kind");
  });
  it("keeps attested blocks out of selection and uses the exact deadline boundary", async () => {
    const nodes = queue(block("01", 1n, true), block("11"));
    expect(
      (
        await selectTimeoutCorrectionTarget(
          nodes,
          SDKStateQueue.DA_ATTESTATION_TIMEOUT_MS,
          "Idle",
        )
      )?.deadline,
    ).toBe(SDKStateQueue.DA_ATTESTATION_TIMEOUT_MS + 1n);
    expect(
      (
        await selectTimeoutCorrectionTarget(
          nodes,
          SDKStateQueue.DA_ATTESTATION_TIMEOUT_MS + 1n,
          "Idle",
        )
      )?.target,
    ).toBe(nodes[2]);
    expect(
      await selectTimeoutCorrectionTarget(
        queue(block("01", 1n, true)),
        4_000_000n,
        "Idle",
      ),
    ).toBeUndefined();
  });
  it("rejects disconnected predecessor and descendant plans", () => {
    const nodes = queue(block("01"), block("11"), block("22"));
    nodes[1]!.datum.next = "Empty";
    expect(() => planNextTimeoutCorrection(nodes, h("11"))).toThrow(
      "predecessor",
    );
    nodes[1]!.datum.next = nodes[2]!.datum.key;
    nodes[2]!.datum.next = "Empty";
    expect(() => planNextTimeoutCorrection(nodes, h("11"))).toThrow(
      "descendant",
    );
  });
});
