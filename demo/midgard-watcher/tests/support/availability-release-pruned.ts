import type { AvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import type { SimTx, SimUtxo } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { expect } from "vitest";

import { pinWatcherAvailabilityJournal } from "../../src/availability/journal-pins.js";
import { releaseWatcherAvailabilityWorkflows } from "../../src/availability/runtime.release-watcher-availability-workflows.js";
import { watcherAvailabilityTransactionPins } from "../../src/l1-follower/availability-pins.js";
import {
  ACTOR,
  type Chain,
  fund,
  intake,
  MANIFEST,
  pass,
  persist,
  signed,
  tipObservation,
} from "./availability-pruned-journal.js";
import { D, okValue, X } from "./l1-follower-raw-reads-fixture.js";
import {
  commitTx,
  initTx,
  queueState,
  scriptAddress,
} from "./l1-follower-state-queue-traffic.js";

/**
 * The actor's challenge of a queued header, ended by someone else's
 * terminal step, released against a follower store that prunes. The
 * follower, its reads, the journal, the pins and the release pass are the
 * real ones; the transactions are the simulator's.
 */

const nodeName = (header: string): string =>
  SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + header;

const one = (
  policy: string,
  name: string,
  quantity: bigint,
): ReadonlyMap<string, ReadonlyMap<string, bigint>> =>
  new Map([[policy, new Map([[name, quantity]])]]);

/** Lands the protocol init and one header commit; returns the header. */
export const queueHeader = async (fx: Chain): Promise<string> => {
  await fx.forward([initTx(D)]);
  const commit = commitTx(queueState(fx.chain, D)!, D);
  await fx.forward([commit]);
  return [...commit.mint!.get(D.stateQueueMint)!.keys()][0]!.slice(
    SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length,
  );
};

/** The live output holding the header's queue node. */
const nodeOf = (fx: Chain, header: string): SimUtxo => {
  const node = fx.chain
    .live()
    .find((utxo) =>
      utxo.output.assets?.get(D.stateQueueMint)?.has(nodeName(header)),
    );
  if (node === undefined) throw new Error("the header has no live node");
  return node;
};

/**
 * The actor's Open of `header`, landed and confirmed in the journal: it
 * spends and re-outputs the header's node and mints the challenge asset
 * onto the record output. Returns the node output it created.
 */
export const landOpen = async (
  fx: Chain,
  journal: AvailabilityOperationJournal,
  header: string,
): Promise<string> => {
  const [funding, collateral] = await fund(fx, 2);
  const node = nodeOf(fx, header);
  const challengeAsset = SDK.daAvailabilityChallengeAssetName({
    transactionId: funding!.txHash.toString("hex"),
    outputIndex: BigInt(funding!.index),
  });
  const record = SDK.encodeDaAvailabilityChallengeRecord({
    commitment: SDK.buildDaAvailabilityCommitment({
      deploymentIdentity: "11".repeat(28),
      headerHash: header,
      payload: Uint8Array.from({ length: 1024 }, (_, i) => i % 256),
      responseGeometry: SDK.availabilityResponseGeometry(
        SDK.DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
      ),
    }),
    challenge_asset_name: challengeAsset,
    challenger: "33".repeat(28),
    opened_at: 1_000n,
    response_deadline: SDK.daAvailabilityResponseDeadline({
      payloadByteLength: 1024,
      openedAt: 1_000n,
    }),
  });
  const { tx, intent } = signed(fx, "open", {
    headerHash: header,
    inputs: [node.outRef, funding!],
    collaterals: [collateral!],
    outputs: [
      node.output,
      {
        address: scriptAddress(D.availabilityChallengeSpend),
        lovelace: 2_000_000n,
        assets: one(D.availabilityChallengeMint, challengeAsset, 1n),
        datum: Buffer.from(record, "hex"),
      },
    ],
    mint: one(D.availabilityChallengeMint, challengeAsset, 1n),
  });
  persist(journal, intent);
  await fx.forward([tx]);
  await fx.forward([]);
  expect(await pass(fx, journal)).toMatchObject([
    { status: "confirmed", txHash: intent.txHash },
  ]);
  return `${intent.txHash}#0`;
};

const land = async (fx: Chain, tx: SimTx): Promise<string> =>
  (await fx.forward([tx])).hashes[0]!;

/** Someone else's step that spends the header's node and re-outputs it. */
export const continueNode = (fx: Chain, header: string): Promise<string> => {
  const node = nodeOf(fx, header);
  return land(fx, {
    inputs: [node.outRef],
    outputs: [node.output],
    nonce: fx.chain.nonce(),
  });
};

/** Someone else's terminal step, not yet landed: it burns the header's node. */
export const burnTx = (fx: Chain, header: string): SimTx => ({
  inputs: [nodeOf(fx, header).outRef],
  outputs: [{ address: X, lovelace: 1_000_000n }],
  mint: one(D.stateQueueMint, nodeName(header), -1n),
  nonce: fx.chain.nonce(),
});

/** Someone else's terminal step, landed: it burns the header's node. */
export const burnNode = (fx: Chain, header: string): Promise<string> =>
  land(fx, burnTx(fx, header));

/**
 * One runtime pass: the journal's reconciliation (which pins the journal's
 * transactions first), then the workflow release at the follower's tip.
 */
export const releasePass = async (
  fx: Chain,
  journal: AvailabilityOperationJournal,
  pin: (
    txHashes: readonly string[],
  ) => Promise<void> = watcherAvailabilityTransactionPins(fx.store),
) => {
  await pass(fx, journal, MANIFEST, pin);
  const observation = tipObservation(fx, D.stateQueueMint);
  return await releaseWatcherAvailabilityWorkflows(
    journal,
    ACTOR,
    (openIntent, headerHash, recorded) =>
      intake(fx).workflowRelease(observation, openIntent, headerHash, recorded),
    () => undefined,
    pinWatcherAvailabilityJournal(
      { journal, actor: ACTOR, deploymentIdentity: MANIFEST },
      pin,
    ),
  );
};

/** The actor's workflow rows: live, and released but not yet final. */
export const workflowRows = (journal: AvailabilityOperationJournal) => ({
  live: journal.workflows(ACTOR).map(({ headerHash }) => headerHash),
  unsettled: journal
    .unsettledReleases(ACTOR)
    .map(({ headerHash, release }) => ({ headerHash, txHash: release.txHash })),
});

/** The store's answer for the output `outRef`, as `row` reads it. */
export const outputRow = async (fx: Chain, outRef: string): Promise<string> => {
  const read = okValue(
    await fx.reads().utxosByOutRefAtPoint([outRef], fx.tipPoint()),
  );
  return read.outputs.length > 0
    ? "unspent"
    : read.spends.length > 0
      ? "spent"
      : read.beyondRetention.length > 0
        ? "beyond_retention"
        : "unknown";
};
