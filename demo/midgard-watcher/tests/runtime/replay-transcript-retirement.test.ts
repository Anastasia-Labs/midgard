import { DatabaseSync } from "node:sqlite";

import { afterEach, describe, expect, it, vi } from "vitest";

import { watcherReplayTranscriptRetirementHooks } from "../../src/runtime/replay-transcript-retirement.js";
import { unsafeAdmitWatcherReplayTranscriptCompletionForTest } from "../../src/storage/replay-transcript-completion.js";
import { createWatcherSqliteReplayTranscriptStore } from "../../src/storage/replay-transcript-store.js";
import { createWatcherAuthenticatedReplayTranscript } from "../../src/verification/authenticated-replay-transcript.js";
import { makeWatcherTranscriptArchiveFixture } from "../support/replay-transcript-archive-fixture.js";

const resources: DatabaseSync[] = [];
afterEach(() => resources.splice(0).forEach((database) => database.close()));
const harness = async (blockNo = "16800") => {
  const { createInput, retentionWindow, retirementObservation } =
    await makeWatcherTranscriptArchiveFixture();
  const transcript =
    await createWatcherAuthenticatedReplayTranscript(createInput);
  const database = new DatabaseSync(":memory:");
  resources.push(database);
  const store = createWatcherSqliteReplayTranscriptStore(database);
  const operationDigest = "e1".repeat(32);
  await store.compareAndSwap({
    expectedTranscriptDigest: null,
    transcript,
    lifecycle: {
      header: createInput.header,
      deploymentIdentity: createInput.deploymentIdentity,
      retentionWindow,
      operationDigest,
    },
  });
  await store.completeOperation(
    unsafeAdmitWatcherReplayTranscriptCompletionForTest({
      deploymentFingerprint: transcript.deploymentFingerprint,
      headerHash: transcript.headerHash,
      operationDigest,
      completedAtSlot: "4300",
    }),
  );
  const current = createInput.stateQueueObservation;
  let fresh = retirementObservation({
    slot: (4301n + BigInt(retentionWindow.retentionDays * 86400)).toString(),
    blockNo,
  });
  await store.retireExpired({
    observation: retirementObservation({
      slot: (BigInt(fresh.nativePoint.slot) - 2161n).toString(),
      blockNo: (BigInt(blockNo) - 2161n).toString(),
    }),
    network: "Preprod",
  });
  let latest = current;
  let recovering = false;
  let unfinished = 0;
  const onFinalized = vi.fn(async () => undefined);
  const readExact = vi.fn(async () => ({}));
  const observe = vi.fn(async () => {
    latest = fresh;
    return fresh;
  });
  // Controlled production-reader seams model an exact admitted fresh result;
  // the retirement store and its authenticated-observation checks are real.
  const dependencies = {
    recovery: {
      hooks: { onFinalized, onRollback: async () => undefined },
      status: () => ({ pending: recovering }),
    },
    durable: {
      readFinality: () => ({
        phase: "tracking",
        incident: null,
        finalized: fresh.nativePoint,
      }),
    },
    supervisor: {
      status: () => ({
        recovered: true,
        phase: "accepting",
        unfinishedObjectiveCount: unfinished,
        queuedJobCount: 0,
        activeJob: null,
        blockedJob: null,
      }),
    },
    stateQueueSource: { latestFinalizedObservation: () => latest, observe },
    stateQueueRuntime: { current: () => current },
    localObservationRuntime: { observe: readExact },
    store,
    config: { targetNetwork: "Preprod" },
  } as unknown as Parameters<typeof watcherReplayTranscriptRetirementHooks>[0];
  const hooks = watcherReplayTranscriptRetirementHooks(dependencies);
  const delivery = {
    nativeBlock: {
      blockHash: fresh.nativePoint.blockHash,
      slot: fresh.nativePoint.slot,
      blockNo: fresh.nativePoint.blockNo,
    },
    localObservation: null,
    relevance: "quiet",
  } as Parameters<typeof hooks.onFinalized>[0];
  return {
    store,
    transcript,
    readExact,
    observe,
    hooks,
    delivery,
    checkpoint: (nextBlockNo: string) => {
      fresh = retirementObservation({
        slot: (
          BigInt(delivery.nativeBlock.slot) +
          BigInt(nextBlockNo) -
          BigInt(blockNo)
        ).toString(),
        blockNo: nextBlockNo,
      });
      return {
        ...delivery,
        nativeBlock: { ...delivery.nativeBlock, ...fresh.nativePoint },
      };
    },
    suspend: () => {
      recovering = true;
    },
    pending: () => {
      unfinished = 1;
    },
  };
};
describe("runtime replay transcript retirement", () => {
  it("preserves absence between quiet durable checkpoints while ordinary deliveries await the next anchor", async () => {
    const { hooks, checkpoint, store, transcript, delivery } = await harness();
    const first = checkpoint("14639");
    await hooks.onFinalized(first);
    expect(await store.read(transcript)).not.toBeNull();
    await hooks.onFinalized({
      ...first,
      nativeBlock: {
        ...first.nativeBlock,
        blockNo: "14640",
        slot: (BigInt(first.nativeBlock.slot) + 1n).toString(),
      },
    });
    await hooks.onFinalized(checkpoint(delivery.nativeBlock.blockNo));
    expect(await store.read(transcript)).toBeNull();
  });
  it("refreshes an off-grid quiet durable checkpoint without waiting for an absolute block-number multiple", async () => {
    const { hooks, delivery, readExact, observe, store, transcript } =
      await harness("16801");
    await hooks.onFinalized(delivery);
    expect(readExact).toHaveBeenCalledOnce();
    expect(observe).toHaveBeenCalledOnce();
    expect(await store.read(transcript)).toBeNull();
  });
  it("refreshes quiet canonical progress through the actual observation door and reclaims expired SQLite evidence", async () => {
    const { hooks, delivery, readExact, observe, store, transcript } =
      await harness();
    await hooks.onFinalized(delivery);
    expect(readExact).toHaveBeenCalledOnce();
    expect(observe).toHaveBeenCalledOnce();
    expect(await store.read(transcript)).toBeNull();
  });
  it("keeps evidence when recovery starts during the fresh exact-point query", async () => {
    const { hooks, delivery, readExact, observe, store, transcript, suspend } =
      await harness();
    readExact.mockImplementationOnce(async () => {
      suspend();
      return {};
    });
    await hooks.onFinalized(delivery);
    expect(observe).not.toHaveBeenCalled();
    expect(await store.read(transcript)).not.toBeNull();
  });
  it("does not sweep while another proof objective still depends on retained evidence", async () => {
    const { hooks, delivery, readExact, store, transcript, pending } =
      await harness();
    pending();
    await hooks.onFinalized(delivery);
    expect(readExact).not.toHaveBeenCalled();
    expect(await store.read(transcript)).not.toBeNull();
  });
});
