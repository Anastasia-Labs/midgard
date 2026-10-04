import { DatabaseSync } from "node:sqlite";

import { afterEach, describe, expect, it } from "vitest";

import {
  unsafeAdmitWatcherReplayTranscriptClassificationForTest,
  unsafeAdmitWatcherReplayTranscriptCompletionForTest,
} from "../../src/storage/replay-transcript-completion.js";
import { createWatcherSqliteReplayTranscriptStore } from "../../src/storage/replay-transcript-store.js";
import { createWatcherAuthenticatedReplayTranscript } from "../../src/verification/authenticated-replay-transcript.js";
import { makeWatcherTranscriptArchiveFixture } from "../support/replay-transcript-archive-fixture.js";

const resources: DatabaseSync[] = [];
afterEach(() => resources.splice(0).forEach((database) => database.close()));
const fixture = async () => {
  const { createInput, retentionWindow, retirementObservation } =
    await makeWatcherTranscriptArchiveFixture();
  const transcript =
    await createWatcherAuthenticatedReplayTranscript(createInput);
  const database = new DatabaseSync(":memory:");
  resources.push(database);
  const store = createWatcherSqliteReplayTranscriptStore(database);
  const operationDigest = "f1".repeat(32);
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
  const classification =
    unsafeAdmitWatcherReplayTranscriptClassificationForTest({
      deploymentFingerprint: transcript.deploymentFingerprint,
      headerHash: transcript.headerHash,
      operationDigest,
      headTranscriptDigest: transcript.transcriptDigest,
    });
  const sweep = {
    observation: retirementObservation({
      slot: (
        4300n +
        BigInt(retentionWindow.retentionDays * 86400) +
        1n
      ).toString(),
    }),
    network: "Preprod" as const,
  };
  return {
    database,
    store,
    transcript,
    classification,
    operationDigest,
    sweep,
    retirementObservation,
  };
};
describe("classified transcript dependency lifecycle", () => {
  it("keeps time-expired old inclusion evidence while its recent absence remains recoverable, including the boundary", async () => {
    const { store, transcript, classification, sweep, retirementObservation } =
      await fixture();
    await store.completeClassification(classification);
    const at = (blockNo: string) => ({
      ...sweep,
      observation: retirementObservation({
        slot: sweep.observation.nativePoint.slot,
        blockNo,
      }),
    });
    expect(await store.retireExpired(at("12000"))).toBe(0);
    expect(await store.retireExpired(at("14160"))).toBe(0);
    expect((await store.read(transcript))?.chainLength).toBe(1);
    expect(await store.retireExpired(at("14161"))).toBe(1);
  });
  it.each(["restart", "rollback", "live"] as const)(
    "resets continuous absence after %s without deleting original evidence",
    async (reset) => {
      const {
        database,
        store,
        transcript,
        classification,
        sweep,
        retirementObservation,
      } = await fixture();
      await store.completeClassification(classification);
      expect(await store.retireExpired(sweep)).toBe(0);
      if (reset === "restart")
        createWatcherSqliteReplayTranscriptStore(database);
      if (reset === "rollback") await store.resetRetirementWitnesses();
      if (reset === "live")
        await store.retireExpired({
          ...sweep,
          observation: retirementObservation({
            slot: sweep.observation.nativePoint.slot,
            live: true,
          }),
        });
      const after = (blockNo: string) => ({
        ...sweep,
        observation: retirementObservation({
          slot: sweep.observation.nativePoint.slot,
          blockNo,
        }),
      });
      expect(await store.retireExpired(after("14161"))).toBe(0);
      expect((await store.read(transcript))?.chainLength).toBe(1);
      expect(await store.retireExpired(after("16322"))).toBe(1);
    },
  );
  it("refuses a substituted absence witness instead of deleting the original transcript", async () => {
    const {
      database,
      store,
      transcript,
      classification,
      sweep,
      retirementObservation,
    } = await fixture();
    await store.completeClassification(classification);
    expect(await store.retireExpired(sweep)).toBe(0);
    database.exec(
      "UPDATE watcher_replay_transcript_absence SET block_no = '0'",
    );
    await expect(
      store.retireExpired({
        ...sweep,
        observation: retirementObservation({
          slot: sweep.observation.nativePoint.slot,
          blockNo: "14161",
        }),
      }),
    ).rejects.toThrow("absence witness is corrupt");
    expect((await store.read(transcript))?.chainLength).toBe(1);
  });
  it("retires an expired removed descendant classification that never exposed a proof capture", async () => {
    const { store, transcript, classification, sweep, retirementObservation } =
      await fixture();
    expect(await store.retireExpired(sweep)).toBe(0);
    await expect(
      store.completeClassification({ ...classification }),
    ).rejects.toThrow("not admitted");
    await expect(
      store.completeClassification(
        unsafeAdmitWatcherReplayTranscriptClassificationForTest({
          ...classification,
          headTranscriptDigest: "ff".repeat(32),
        }),
      ),
    ).rejects.toThrow("head differs");
    await store.completeClassification(classification);
    expect(await store.retireExpired(sweep)).toBe(0);
    expect(
      await store.retireExpired({
        ...sweep,
        observation: retirementObservation({
          slot: sweep.observation.nativePoint.slot,
          blockNo: "14161",
        }),
      }),
    ).toBe(1);
    expect(await store.read(transcript)).toBeNull();
  });
  it("keeps classified evidence once a runner obtains its capture until canonical proof completion", async () => {
    const {
      store,
      transcript,
      classification,
      operationDigest,
      sweep,
      retirementObservation,
    } = await fixture();
    await store.completeClassification(classification);
    await store.beginProofOperation(classification);
    expect(await store.retireExpired(sweep)).toBe(0);
    await store.completeOperation(
      unsafeAdmitWatcherReplayTranscriptCompletionForTest({
        deploymentFingerprint: transcript.deploymentFingerprint,
        headerHash: transcript.headerHash,
        operationDigest,
        completedAtSlot: "4300",
      }),
    );
    expect(await store.retireExpired(sweep)).toBe(0);
    expect(
      await store.retireExpired({
        ...sweep,
        observation: retirementObservation({
          slot: sweep.observation.nativePoint.slot,
          blockNo: "14161",
        }),
      }),
    ).toBe(1);
  });
});
