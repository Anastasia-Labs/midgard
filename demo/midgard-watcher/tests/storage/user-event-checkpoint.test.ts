import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import {
  assertWatcherUserEventCheckpointSuccessor,
  makeWatcherUserEventCheckpoint,
  parseWatcherUserEventCheckpoint,
  readWatcherUserEventCheckpointPayload,
  WATCHER_USER_EVENT_CHECKPOINT_SCHEMA_VERSION,
  watcherUserEventArchiveDigest,
  watcherUserEventCheckpointExpectationMatches,
} from "../../src/storage/user-event-checkpoint.js";

const h = (byte: string) => byte.repeat(32);
const payload = new TextEncoder().encode('{"cursor":null}');
const payloadDigest = watcherUserEventArchiveDigest(payload);
const input = {
  schemaVersion: WATCHER_USER_EVENT_CHECKPOINT_SCHEMA_VERSION,
  deploymentMarker: makeDeploymentMarker(h("11")),
  network: "Preprod" as const,
  blueprintHash: h("22"),
  finalityPolicyDigest: h("33"),
  userEventPolicyDigest: h("44"),
  checkpointSequence: "0",
  predecessorCheckpointDigest: null,
  rollbackGeneration: "0",
  payloadDigest,
  requiredArchiveDigests: [payloadDigest],
};

describe("structural user-event checkpoint framing", () => {
  it("copies and freezes framing and checks deployment bindings", () => {
    const frame = makeWatcherUserEventCheckpoint(input);
    expect(parseWatcherUserEventCheckpoint(frame, input)).toEqual(frame);
    expect(Object.isFrozen(frame.requiredArchiveDigests)).toBe(true);
    expect(frame.requiredArchiveDigests).not.toBe(input.requiredArchiveDigests);
    expect(() =>
      parseWatcherUserEventCheckpoint(frame, { ...input, network: "Preview" }),
    ).toThrow("framing");
  });

  it("requires canonical fields, bounded natural counters, and complete sorted direct references", () => {
    for (const override of [
      { checkpointSequence: "00" },
      { rollbackGeneration: "18446744073709551616" },
      { requiredArchiveDigests: [] },
      { requiredArchiveDigests: [payloadDigest, payloadDigest] },
      { requiredArchiveDigests: [h("ff"), payloadDigest] },
      { requiredArchiveDigests: [h("ff")] },
      { predecessorCheckpointDigest: h("55") },
    ]) {
      expect(() =>
        makeWatcherUserEventCheckpoint({ ...input, ...override }),
      ).toThrow();
    }
    const frame = makeWatcherUserEventCheckpoint(input);
    expect(() =>
      parseWatcherUserEventCheckpoint({ ...frame, extra: true }, input),
    ).toThrow("framing");
    expect(() =>
      parseWatcherUserEventCheckpoint(
        { ...frame, checkpointDigest: h("66") },
        input,
      ),
    ).toThrow("framing");
  });

  it("keeps event sequencing separate and bindings immutable across exact successors", () => {
    const first = makeWatcherUserEventCheckpoint(input);
    const nextInput = {
      ...input,
      checkpointSequence: "1",
      predecessorCheckpointDigest: first.checkpointDigest,
    };
    const next = makeWatcherUserEventCheckpoint(nextInput);
    expect(() =>
      assertWatcherUserEventCheckpointSuccessor(null, first),
    ).not.toThrow();
    expect(() =>
      assertWatcherUserEventCheckpointSuccessor(first, next),
    ).not.toThrow();
    expect(
      watcherUserEventCheckpointExpectationMatches(first, {
        expectedCheckpointDigest: first.checkpointDigest,
        expectedCheckpointSequence: "0",
      }),
    ).toBe(true);
    expect(
      watcherUserEventCheckpointExpectationMatches(first, {
        expectedCheckpointDigest: first.checkpointDigest,
        expectedCheckpointSequence: "1",
      }),
    ).toBe(false);
    for (const override of [
      { userEventPolicyDigest: h("99") },
      { checkpointSequence: "2" },
      { network: "Preview" as const },
    ]) {
      expect(() =>
        assertWatcherUserEventCheckpointSuccessor(
          first,
          makeWatcherUserEventCheckpoint({ ...nextInput, ...override }),
        ),
      ).toThrow("exact successor");
    }
    const rolled = makeWatcherUserEventCheckpoint({
      ...input,
      rollbackGeneration: "1",
    });
    expect(() =>
      assertWatcherUserEventCheckpointSuccessor(
        rolled,
        makeWatcherUserEventCheckpoint({
          ...nextInput,
          predecessorCheckpointDigest: rolled.checkpointDigest,
        }),
      ),
    ).toThrow("exact successor");
  });

  it("requires every declared archive object and canonical object payload bytes", async () => {
    const attachment = new TextEncoder().encode("retained evidence");
    const attachmentDigest = watcherUserEventArchiveDigest(attachment);
    const frame = makeWatcherUserEventCheckpoint({
      ...input,
      requiredArchiveDigests: [payloadDigest, attachmentDigest].sort(),
    });
    const objects = new Map([
      [payloadDigest, payload],
      [attachmentDigest, attachment],
    ]);
    const archive = {
      put: async (bytes: Uint8Array) => watcherUserEventArchiveDigest(bytes),
      read: async (digest: string) => objects.get(digest) ?? null,
    };
    expect(await readWatcherUserEventCheckpointPayload(frame, archive)).toEqual(
      payload,
    );
    objects.delete(attachmentDigest);
    await expect(
      readWatcherUserEventCheckpointPayload(frame, archive),
    ).rejects.toThrow("missing");
    objects.set(attachmentDigest, new TextEncoder().encode("damaged evidence"));
    await expect(
      readWatcherUserEventCheckpointPayload(frame, archive),
    ).rejects.toThrow("digest differs");
    for (const text of ['{ "cursor": null }', "[]", "null"]) {
      const bytes = new TextEncoder().encode(text);
      const digest = watcherUserEventArchiveDigest(bytes);
      objects.set(digest, bytes);
      const noncanonical = makeWatcherUserEventCheckpoint({
        ...input,
        payloadDigest: digest,
        requiredArchiveDigests: [digest],
      });
      await expect(
        readWatcherUserEventCheckpointPayload(noncanonical, archive),
      ).rejects.toThrow("canonical JSON");
    }
  });
});
