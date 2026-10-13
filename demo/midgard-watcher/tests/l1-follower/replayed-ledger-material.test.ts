import {
  FraudProofL1CheckpointChangedError,
  FraudProofL1UnavailableError,
} from "@al-ft/midgard-fault-proofs";
import { buildCanonicalBlockFixture } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { createWatcherReplayRetention } from "../../src/l1-follower/replay-retention.js";
import {
  createWatcherFollowerReplaySource,
  type WatcherFollowerReplaySource,
} from "../../src/l1-follower/replay-source.js";
import {
  type WatcherUserEvents,
  WatcherUserEventUnavailable,
} from "../../src/l1-follower/user-events.js";
import {
  followerUserEventsDeployment,
  FORCED_ORDER_INCLUSION_TIME,
  forcedOrderTransaction,
  listOrderTransaction,
  openFollowerUserEvents,
  syntheticChain,
  transactionHash,
  userEventId,
} from "../support/follower-user-events-fixture.js";
import {
  commitTransaction,
  createSyntheticStateQueueHeader,
  initializationTransaction,
} from "../support/state-queue-observation-fixture.commit-transaction.js";

/** Constructed L1 transactions feed real projections; no Plutus validity claim. */
const commitAtClock = (raw: string, clock: bigint): string => {
  const transaction = CML.Transaction.from_cbor_hex(raw);
  const original = transaction.body();
  const outputs = CML.TransactionOutputList.new();
  for (let index = 0; index < original.outputs().len(); index += 1) {
    const output = original.outputs().get(index);
    if (index !== 1) {
      outputs.add(output);
      continue;
    }
    const linked = Data.from(
      output.datum()!.as_datum()!.to_cbor_hex(),
      SDK.LinkedListDatum,
    );
    if (!("Node" in linked.data))
      throw new Error("fixture commit output is not a queue node");
    const node = Data.castFrom(linked.data.Node.data, SDK.StateQueueNode);
    const datum = {
      ...linked,
      data: {
        Node: {
          data: Data.castTo({ ...node, block_time: clock }, SDK.StateQueueNode),
        },
      },
    };
    outputs.add(
      CML.TransactionOutput.new(
        output.address(),
        output.amount(),
        CML.DatumOption.new_datum(
          CML.PlutusData.from_cbor_hex(Data.to(datum, SDK.LinkedListDatum)),
        ),
      ),
    );
  }
  const body = CML.TransactionBody.new(
    original.inputs(),
    outputs,
    original.fee(),
  );
  body.set_mint(original.mint()!);
  body.set_script_data_hash(original.script_data_hash()!);
  return CML.Transaction.new(
    body,
    transaction.witness_set(),
    true,
  ).to_canonical_cbor_hex();
};

const setup = async () => {
  const deployment = followerUserEventsDeployment();
  const follower = await openFollowerUserEvents({ deployment });
  const chain = syntheticChain();
  const deposit = userEventId("d1"),
    withdrawal = userEventId("e1"),
    forced = userEventId("f1"),
    afterCommit = userEventId("d2");
  const header = {
    ...createSyntheticStateQueueHeader(),
    startTime: 0n,
    endTime: FORCED_ORDER_INCLUSION_TIME + 1n,
  };
  const clock = header.endTime + 10_000n;
  const init = initializationTransaction(deployment.authority);
  const commit = commitAtClock(
    commitTransaction(deployment.authority, transactionHash(init), header),
    clock,
  );
  chain.next([init]);
  chain.next([
    listOrderTransaction(deployment, "deposit", deposit),
    listOrderTransaction(deployment, "withdrawal", withdrawal),
    forcedOrderTransaction(deployment, forced),
    commit,
    listOrderTransaction(deployment, "deposit", afterCommit),
  ]);
  chain.empties(20);
  await follower.apply(chain.blocks);
  const observed = await follower.observe();
  const admitted = observed.finalizedHeaders[0]!;
  const empty = await buildCanonicalBlockFixture({
    transactions: [],
    utxos: [],
  });
  const payload: SDK.DaPayload = {
    ...empty.payload,
    block_body: {
      ...empty.payload.block_body,
      header,
      header_hash: admitted.headerHash,
      deposits: [[deposit.cborHex, "80"]],
      withdrawals: [[withdrawal.cborHex, "80"]],
      forced_transactions: [[forced.cborHex, "80"]],
    },
  };
  const sources: WatcherFollowerReplaySource[] = [];
  const source = (userEvents: WatcherUserEvents = follower.userEvents) => {
    const port = createWatcherFollowerReplaySource({
      store: follower.store,
      userEvents,
      identity: {
        deploymentManifestId: deployment.deploymentIdentity.manifestId,
        blueprintHash: deployment.deploymentIdentity.blueprintHash,
        network: deployment.deploymentIdentity.network,
      },
      scripts: {
        depositPolicyId: deployment.scripts.deposit.policyId,
        withdrawalPolicyId: deployment.scripts.withdrawal.policyId,
        forcedOrderPolicyId: deployment.scripts.forcedOrder.policyId,
        forcedOrderAddressHex: deployment.scripts.forcedOrder.addressHex,
      },
      observationAuthority: deployment.authority,
      minimumCheckpointDepth: 2,
      retention: createWatcherReplayRetention({
        slotConfig: { zeroTime: 0, zeroSlot: 0, slotLength: 1000 },
        eventWaitMs: 1n,
        maximumAdmissionRangeMs: 1n,
        maximumProtectionWaitMs: 0n,
      }),
    });
    sources.push(port);
    return port;
  };
  return {
    follower,
    payload,
    clock,
    afterCommit,
    source,
    close: async () => {
      for (const source of sources) source.close();
      await follower.close();
    },
  };
};

it("requires every due admission through the commit and reads its independent L1 commit clock", async () => {
  const {
    payload,
    clock,
    afterCommit,
    source: createSource,
    close,
  } = await setup();
  const source = createSource();
  try {
    expect((await source.materialFor(payload)).blockTimeMs).toBe(Number(clock));
    await expect(
      source.materialFor({
        ...payload,
        block_body: { ...payload.block_body, deposits: [] },
      }),
    ).rejects.toThrow("complete authenticated admission set");
    await expect(
      source.materialFor({
        ...payload,
        block_body: {
          ...payload.block_body,
          deposits: [
            ...payload.block_body.deposits,
            [afterCommit.cborHex, "80"],
          ],
        },
      }),
    ).rejects.toThrow("complete authenticated admission set");
    await expect(
      source.materialFor({
        ...payload,
        block_body: { ...payload.block_body, forced_transactions: [] },
      }),
    ).rejects.toThrow("complete authenticated admission set");
  } finally {
    await close();
  }
});

it("names unavailable or retired header fences as retryable L1 source misses", async () => {
  const h = await setup();
  try {
    const missing = h.source({
      ...h.follower.userEvents,
      headerFence: async () => {
        throw new WatcherUserEventUnavailable("replay cutoff is unavailable");
      },
    });
    await expect(missing.materialFor(h.payload)).rejects.toBeInstanceOf(
      FraudProofL1UnavailableError,
    );
    const retired = h.source({
      ...h.follower.userEvents,
      headerFence: async (header) => {
        const fence = await h.follower.userEvents.headerFence(header);
        return {
          ...fence,
          refresh: async () => {
            h.follower.userEvents.close();
            await fence.refresh();
          },
        };
      },
    });
    await expect(retired.materialFor(h.payload)).rejects.toBeInstanceOf(
      FraudProofL1CheckpointChangedError,
    );
  } finally {
    await h.close();
  }
});
