import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import { Effect } from "effect";
import { beforeAll, describe, expect, it, vi } from "vitest";

import {
  type ForeignNativeReplayBlock,
  prepareForeignNativeReplay,
} from "../src/services/foreign-native-replay.js";
import type { NativeMpfOwnerDiagnostics } from "../src/services/mpf-native-owner/protocol.js";
import { encodeNativeMpfEventLog } from "../src/services/mpf-native-owner/service.js";
import {
  digest,
  EVENT_LOG_DIGEST_DOMAIN,
} from "../src/services/mpf-native-owner/service.normalize-owner-options.js";
import { prepareEventFlatDigest } from "../src/workers/utils/mpf-event-flat-digest.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";

const root = (byte: string) => byte.repeat(32);
const binary = root("ab");
const block = (
  parent: string,
  post: string,
  index: number,
): ForeignNativeReplayBlock => ({
  kind: "foreign",
  headerHash: String(index),
  parentHeaderHash: String(index - 1),
  parentUtxosRoot: parent,
  root: post,
  events: [
    [
      {
        key: makeOutRefCbor(index).toString("hex"),
        output: encodeMidgardTxOutput({
          address: Buffer.concat([
            Buffer.from([0x60]),
            Buffer.alloc(28, index),
          ]),
          value: { lovelace: 2_000_000n, assets: new Map() },
        }),
      },
    ],
  ],
  eventRoots: [post],
});
const fixture = (
  durableRoot = root("11"),
  blocks = [block(root("11"), root("22"), 1)],
) => {
  const handle = {
    ownerEpoch: Buffer.alloc(16, 1),
    generationId: Buffer.alloc(16, 2),
    baseRoot: durableRoot,
  };
  const roots = blocks.flatMap((value) => [...value.eventRoots]);
  const base = {
    headerHash: blocks.at(-1)!.headerHash,
    root: blocks.at(-1)!.root,
    importedBlocks: blocks,
  };
  const diagnostics: NativeMpfOwnerDiagnostics = {
    ownerEpoch: handle.ownerEpoch,
    durableRoot,
    residentNodes: 0,
    residentEdges: 0,
    residentBytes: 0,
    activeGenerations: 0,
    generatedNodes: 0,
    generatedBytes: 0,
    rssBytes: 0,
    peakRssBytes: 0,
    childRestarts: 0,
  };
  const owner = {
    diagnostics: vi.fn(async () => diagnostics),
    fork: vi.fn(async () => handle),
    discard: vi.fn(async () => undefined),
    applyEvents: vi.fn(
      async (_handle: typeof handle, eventLog: Uint8Array) => ({
        handle,
        candidateRoot: base.root,
        eventRoots: roots,
        eventLogDigest: digest(EVENT_LOG_DIGEST_DOMAIN, eventLog).toString(
          "hex",
        ),
        proofArenaDurationNs: 0,
        mutationDurationNs: 0,
      }),
    ),
  };
  const run = () =>
    Effect.runPromise(
      prepareForeignNativeReplay({ owner, base, ownerBinarySha256: binary }),
    );
  return { owner, base, handle, run };
};

describe("verified foreign native replay", () => {
  // The production owner service awaits this at startup; these fixtures
  // build event logs without it, so they must not race the digest's load.
  beforeAll(() => prepareEventFlatDigest());

  it("prepares exact restart material without promoting durable state", async () => {
    const f = fixture();
    const result = await f.run();
    expect(result?.replay).toMatchObject({
      baseRoot: root("11"),
      candidateRoot: root("22"),
      ownerBinarySha256: binary,
      eventCount: 1,
    });
    expect(Buffer.from(result!.replay.eventRoots).toString("hex")).toBe(
      root("22"),
    );
    expect(f.owner.discard).not.toHaveBeenCalled();
  });

  it("combines a local journal between two foreign blocks", async () => {
    const localLog = encodeNativeMpfEventLog(root("22"), [
      [{ type: "delete", key: Buffer.from("1122", "hex") }],
    ]);
    const local: ForeignNativeReplayBlock = {
      ...block(root("22"), root("33"), 2),
      kind: "local",
      events: [],
      nativeMpfReplay: {
        schema: 1,
        ownerBinarySha256: Buffer.from(binary, "hex"),
        baseRoot: Buffer.from(root("22"), "hex"),
        candidateRoot: Buffer.from(root("33"), "hex"),
        eventLog: localLog,
        eventLogDigest: digest(EVENT_LOG_DIGEST_DOMAIN, localLog),
        eventRoots: Buffer.from(root("33"), "hex"),
        eventCount: 1,
      },
    };
    const f = fixture(root("11"), [
      block(root("11"), root("22"), 1),
      local,
      block(root("33"), root("44"), 3),
    ]);
    const result = await f.run();
    const log = Buffer.from(result!.replay.eventLog);
    expect(log.readUInt32LE(8)).toBe(3);
    expect(log.readUInt32LE(12)).toBe(3);
    expect(result!.replay.eventCount).toBe(3);
    expect(Buffer.from(result!.replay.eventRoots).toString("hex")).toBe(
      root("22") + root("33") + root("44"),
    );
  });

  it("rejects an intermediate root mismatch even with the correct final root", async () => {
    const f = fixture(root("11"), [
      block(root("11"), root("22"), 1),
      block(root("22"), root("33"), 2),
    ]);
    f.owner.applyEvents.mockImplementationOnce(async (_handle, log) => ({
      handle: f.handle,
      candidateRoot: root("33"),
      eventRoots: [root("99"), root("33")],
      eventLogDigest: digest(EVENT_LOG_DIGEST_DOMAIN, log).toString("hex"),
      proofArenaDurationNs: 0,
      mutationDurationNs: 0,
    }));
    await expect(f.run()).rejects.toThrow(/independently verified E1 roots/);
    expect(f.owner.discard).toHaveBeenCalledWith(f.handle);
  });

  it("refuses a missing canonical segment before allocating a generation", async () => {
    const f = fixture(root("11"), [
      block(root("11"), root("22"), 1),
      block(root("33"), root("44"), 3),
    ]);
    await expect(f.run()).rejects.toThrow(/gap/);
    expect(f.owner.fork).not.toHaveBeenCalled();
  });

  it("refuses an unrelated durable root rather than resetting it", async () => {
    const f = fixture(root("77"));
    await expect(f.run()).rejects.toThrow(/not a parent/);
    expect(f.owner.fork).not.toHaveBeenCalled();
  });

  it("leaves an already verified unchanged root alone", async () => {
    const f = fixture(root("22"));
    await expect(f.run()).resolves.toBeUndefined();
    expect(f.owner.fork).not.toHaveBeenCalled();
  });

  it("joins a failed native apply and releases its unjournalled generation", async () => {
    const f = fixture();
    f.owner.applyEvents.mockRejectedValueOnce(new Error("native apply failed"));
    await expect(f.run()).rejects.toThrow(/native apply failed/);
    expect(f.owner.discard).toHaveBeenCalledWith(f.handle);
  });
});
