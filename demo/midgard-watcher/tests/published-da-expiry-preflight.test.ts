import { MIDGARD_PROTOCOL_VERSION } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect, it, vi } from "vitest";

import {
  createPublishedWatcherBlockActor,
  type PublishedWatcherDeployment,
} from "./support/published-block-actor.js";

const headerHash = "11".repeat(28);
const stateQueuePolicyId = "44".repeat(28);
const address = credentialToAddress("Preprod", {
  type: "Key",
  hash: "dd".repeat(28),
});
const stateQueueUnit = toUnit(
  stateQueuePolicyId,
  SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
);
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
const fixture = async () => {
  let attested = false;
  const newTx = vi.fn(() => {
    throw new Error("unexpected transaction construction");
  });
  const submitTx = vi.fn();
  const currentTime = Number(
    header.endTime + SDK.DA_ATTESTATION_TIMEOUT_MS + 1n,
  );
  const target = (): UTxO => ({
    txHash: "aa".repeat(32),
    outputIndex: 0,
    address,
    assets: { lovelace: 5_000_000n, [stateQueueUnit]: 1n },
    datum: SDK.encodeLinkedListNodeView({
      key: { Key: { key: headerHash } },
      next: "Empty",
      data: SDK.castStateQueueNodeToData({
        proven_fraud: null,
        header,
        da_attestation: attested
          ? { Attested: { da_bond_asset_name: "ee".repeat(32) } }
          : SDK.NO_DA_ATTESTATION,
      }) as SDK.LinkedListNodeView["data"],
    }),
  });
  const params: UTxO = {
    txHash: "bb".repeat(32),
    outputIndex: 0,
    address,
    assets: { lovelace: 2_000_000n },
    datum: Data.to(
      {
        committee: "",
        committee_signers_hash: "cc".repeat(32),
        da_threshold: 1n,
        owners: [],
        update_threshold: 1n,
      },
      SDK.DaParamsDatum,
    ),
  };
  const lucid = {
    wallet: () => ({ address: async () => address }),
    utxosAtWithUnit: vi.fn(async (_address: string, unit: string) =>
      unit === stateQueueUnit ? [target()] : [params],
    ),
    currentSlot: () => 123,
    slotToUnixTime: () => currentTime,
    config: () => ({ provider: { submitTx } }),
    newTx,
  } as unknown as LucidEvolution;
  const deployment = {
    publisherLucid: lucid,
    references: new Map(),
    chain: { now: () => currentTime },
    contracts: {
      stateQueue: {
        policyId: stateQueuePolicyId,
        spendingScriptAddress: address,
      },
      activeOperators: { policyId: "aa".repeat(28) },
      scheduler: { policyId: "bb".repeat(28) },
      daParamsGovernor: {
        policyId: "cc".repeat(28),
        spendingScriptAddress: address,
      },
      daAttestation: { policyId: "dd".repeat(28) },
    },
  } as unknown as PublishedWatcherDeployment;
  const onStage = vi.fn();
  const actor = await createPublishedWatcherBlockActor({
    deployment,
    lucid,
    daSignerConfig: {} as never,
    onStage,
  });
  return {
    actor,
    newTx,
    submitTx,
    onStage,
    setAttested: () => {
      attested = true;
    },
    block: { header, headerHash, payloadEnvelopeCbor: Buffer.alloc(0) },
  };
};

it("rejects an expired unattested header before constructing or submitting DA init", async () => {
  const f = await fixture();
  const onSubmitted = vi.fn();
  await expect(f.actor.attest(f.block, { onSubmitted })).rejects.toThrow(
    "DA attestation apply deadline has already elapsed",
  );
  expect(f.newTx).not.toHaveBeenCalled();
  expect(f.submitTx).not.toHaveBeenCalled();
  expect(onSubmitted).not.toHaveBeenCalled();
  expect(f.onStage).not.toHaveBeenCalled();
});

it("reconciles a retained init before rejecting expiry without adding signatures", async () => {
  const f = await fixture();
  const record = {
    step: "init" as const,
    txHash: "ab".repeat(32),
    signedCbor: "80",
  };
  const reconcileSubmitted = vi.fn(async () => ({ kind: "included" as const }));
  const onSubmitted = vi.fn();
  await expect(
    f.actor.attest(f.block, {
      submitted: [record],
      reconcileSubmitted,
      onSubmitted,
    }),
  ).rejects.toThrow("DA attestation apply deadline has already elapsed");
  expect(reconcileSubmitted).toHaveBeenCalledExactlyOnceWith(record);
  expect(f.newTx).not.toHaveBeenCalled();
  expect(f.submitTx).not.toHaveBeenCalled();
  expect(onSubmitted).not.toHaveBeenCalled();
  expect(f.onStage).not.toHaveBeenCalled();
});

it("accepts an already-attested retained target after reconciliation despite the header age", async () => {
  const f = await fixture();
  const record = {
    step: "apply" as const,
    txHash: "ab".repeat(32),
    signedCbor: "80",
  };
  const reconcileSubmitted = vi.fn(async () => {
    f.setAttested();
    return { kind: "included" as const };
  });
  await expect(
    f.actor.attest(f.block, { submitted: [record], reconcileSubmitted }),
  ).resolves.toEqual({ kind: "attested", txHash: "aa".repeat(32) });
  expect(reconcileSubmitted).toHaveBeenCalledExactlyOnceWith(record);
  expect(f.newTx).not.toHaveBeenCalled();
  expect(f.submitTx).not.toHaveBeenCalled();
});
