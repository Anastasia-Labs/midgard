/**
 * The replayed ledger over retained payloads checked against the
 * availability commitment their block's state-queue node carries
 * (`retainedPayloadReplaySource` with `availability`, through
 * `SDK.daPayloadAvailabilityBinding`). The header does not commit every body
 * section, so a source can serve the canonical header over other bytes. Once
 * the node carries a commitment, a copy that does not rebuild it is the next
 * source's to serve, within the same fetch, and only a bound copy's replay is
 * the block's. While the node is Unattested no replay refusal is the block's:
 * the copy is released and the read waits; the next read asks past it.
 */
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  createReplayedLedger,
  type ReplayedLedgerRefusal,
} from "../src/replayed-ledger.js";
import {
  type RetainedPayloadReleases,
  retainedPayloadReplaySource,
} from "../src/retained-payload-replay-source.js";
import {
  type RetainedDaPayloadSource,
  RetainedDaPayloadUnavailableError,
} from "../src/transition-trace/fetch.js";
import { buildCanonicalBlockFixture } from "./helpers/canonical-block-evidence-fixture.js";

const AUTHORITY: SDK.DaPayloadAvailabilityAuthority = {
  deploymentIdentity: "c7".repeat(28),
  responseGeometry: SDK.availabilityResponseGeometry({
    chunkByteLength: 4096,
    trancheByteLength: 64 * 1024,
    maxTrancheCount: 1024,
  }),
};

const envelope = (payload: SDK.DaPayload) =>
  wrapDaPayload(SDK.encodeDaPayload(payload), { mode: "identity" });

/** An empty block on genesis, its envelope, and copies of its header. */
const blocks = async () => {
  const honest = await buildCanonicalBlockFixture({
    transactions: [],
    utxos: [],
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
  });
  /** The block's header over a body with `alter` applied. */
  const tampered = (alter: Partial<SDK.DaPayload["block_body"]>) =>
    envelope({
      ...honest.payload,
      block_body: { ...honest.payload.block_body, ...alter },
    });
  // A block that does not open its own root: the bytes are canonical, the
  // transition is not.
  const header = { ...honest.header, utxosRoot: "99".repeat(32) };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  return {
    headerHash: honest.headerHash,
    honest: honest.payloadEnvelopeCbor,
    // Preimages of no transaction: the header does not commit this section.
    stray: await tampered({ transaction_preimages: [["ab".repeat(32), "00"]] }),
    strayOther: await tampered({
      transaction_preimages: [["cd".repeat(32), "00"]],
    }),
    fraud: {
      headerHash,
      bytes: await envelope({
        ...honest.payload,
        block_body: {
          ...honest.payload.block_body,
          header,
          header_hash: headerHash,
        },
      }),
    },
  };
};

const commitment = (headerHash: string, bytes: Uint8Array) =>
  SDK.buildDaAvailabilityCommitment({
    ...AUTHORITY,
    headerHash,
    payload: bytes,
  });
const attested = (headerHash: string, bytes: Uint8Array) => ({
  Attested: {
    commitment_hash: SDK.daAvailabilityCommitmentHash(
      commitment(headerHash, bytes),
    ),
  },
});
const published = (headerHash: string, bytes: Uint8Array) => ({
  Published: {
    terminal_commitment: SDK.daAvailabilityPublishedTerminalCommitment(
      commitment(headerHash, bytes),
    ),
  },
});

/**
 * One ledger over `served` sources (by id), with the node's status as set;
 * each `read` is one operation's view, as the watcher makes one per
 * classification or advance.
 */
const setup = (served: readonly (readonly [string, Buffer])[]) => {
  const requests: string[] = [];
  const refusals: ReplayedLedgerRefusal[] = [];
  const releases: RetainedPayloadReleases = new Map();
  const node: { status: SDK.DaAvailabilityStateQueueStatus } = {
    status: "Unattested",
  };
  const sources: RetainedDaPayloadSource[] = served.map(([id, bytes]) => ({
    sourceId: id,
    fetchPayloadByHeaderHash: async () => {
      requests.push(id);
      return {
        ok: true,
        sourceId: id,
        sourcePeerId: "peer",
        payloadEnvelopeCbor: Buffer.from(bytes),
        provenance: {
          trustClass: "public_or_permissionless_da",
          sourceId: `${id}/peer`,
          grade: "security",
        },
        attempts: [],
      };
    },
  }));
  const ledger = createReplayedLedger({
    checkpoints: {
      read: async () => undefined,
      write: async () => undefined,
    },
    onRefusal: (refusal) => refusals.push(refusal),
  });
  const read = (
    headerHash: string,
    via: readonly RetainedDaPayloadSource[] = sources,
  ) => {
    const retained = retainedPayloadReplaySource({
      sources: via,
      availability: {
        status: async () => node.status,
        authority: () => AUTHORITY,
        releases,
      },
    });
    return ledger.ledgerAt({
      payloadAt: retained.payloadAt,
      materialFor: async (payload) => ({
        blockTimeMs: Number(payload.block_body.header.endTime),
        replayUserEvent: () => Effect.die("the block has no user event"),
        verifyForcedSource: () => Effect.die("the block has no forced order"),
      }),
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      binding: retained.binding,
    })(headerHash);
  };
  return { node, read, sources, requests, refusals, releases };
};

const UNBOUND =
  /^the payload of [0-9a-f]{56} is not bound to an attested commitment yet, and this copy does not replay: .*preimage coverage mismatch/;

describe("a retained payload against its node's availability commitment", () => {
  it("takes the next source's copy within one fetch once the node is Attested", async () => {
    const b = await blocks();
    const h = setup([
      ["s0", b.stray],
      ["s1", b.honest],
    ]);
    h.node.status = attested(b.headerHash, b.honest);
    expect(await h.read(b.headerHash)).toEqual([]);
    expect(h.requests).toEqual(["s0", "s1"]);
    expect(h.refusals).toEqual([]);
  });

  it("binds a Published node's copy by its terminal commitment", async () => {
    const b = await blocks();
    const h = setup([
      ["s0", b.stray],
      ["s1", b.honest],
    ]);
    h.node.status = published(b.headerHash, b.honest);
    expect(await h.read(b.headerHash)).toEqual([]);
    expect(h.requests).toEqual(["s0", "s1"]);
    expect(h.refusals).toEqual([]);
  });

  it("refuses the block when the copy its commitment binds does not replay", async () => {
    const b = await blocks();
    const h = setup([
      ["s0", b.honest],
      ["s1", b.stray],
    ]);
    h.node.status = attested(b.headerHash, b.stray);
    expect(await h.read(b.headerHash)).toBeUndefined();
    expect(h.requests).toEqual(["s0", "s1"]);
    expect(h.refusals).toMatchObject([
      {
        headerHash: b.headerHash,
        kind: "replay-refused",
        detail: expect.stringContaining("preimage coverage mismatch"),
      },
    ]);
  });

  it("re-checks a copy another view captured first, and fetches past it", async () => {
    const b = await blocks();
    const h = setup([
      ["s0", b.stray],
      ["s1", b.honest],
    ]);
    // A classification's own snapshot, which read the block before the
    // ledger did and holds no availability check of its own.
    const snapshot = retainedPayloadReplaySource({ sources: h.sources });
    await snapshot.payloadAt(b.headerHash);
    h.node.status = attested(b.headerHash, b.honest);
    expect(await h.read(b.headerHash, snapshot.sources)).toEqual([]);
    expect(h.requests).toEqual(["s0", "s0", "s1"]);
    expect(h.refusals).toEqual([]);
  });

  it("reports the payload unavailable, not the block refused, when no source serves the bound copy", async () => {
    const b = await blocks();
    const h = setup([
      ["s0", b.stray],
      ["s1", b.strayOther],
    ]);
    h.node.status = attested(b.headerHash, b.honest);
    await expect(h.read(b.headerHash)).rejects.toBeInstanceOf(
      RetainedDaPayloadUnavailableError,
    );
    expect(h.requests).toEqual(["s0", "s1"]);
    expect(h.refusals).toEqual([]);
  });

  it("waits on an Unattested copy that does not replay, and the next read asks past it", async () => {
    const b = await blocks();
    const h = setup([
      ["s0", b.stray],
      ["s1", b.honest],
    ]);
    const first = h.read(b.headerHash);
    await expect(first).rejects.toBeInstanceOf(
      RetainedDaPayloadUnavailableError,
    );
    await expect(first).rejects.toThrow(UNBOUND);
    expect(h.refusals).toEqual([]);
    expect(await h.read(b.headerHash)).toEqual([]);
    expect(h.requests).toEqual(["s0", "s0", "s1"]);
    expect(h.refusals).toEqual([]);
  });

  it("never refuses the block while Unattested, however often its only copy fails", async () => {
    const b = await blocks();
    const h = setup([["s0", b.stray]]);
    await expect(h.read(b.headerHash)).rejects.toThrow(UNBOUND);
    for (let read = 0; read < 2; read += 1)
      await expect(h.read(b.headerHash)).rejects.toBeInstanceOf(
        RetainedDaPayloadUnavailableError,
      );
    expect(h.requests).toEqual(["s0", "s0", "s0"]);
    expect(h.refusals).toEqual([]);
  });

  it("refuses a block that does not open its root only once its node is Attested", async () => {
    const b = await blocks();
    const h = setup([["s0", b.fraud.bytes]]);
    await expect(h.read(b.fraud.headerHash)).rejects.toThrow(
      "is not bound to an attested commitment yet",
    );
    expect(h.refusals).toEqual([]);
    expect(h.releases.size).toBe(1);
    h.node.status = attested(b.fraud.headerHash, b.fraud.bytes);
    expect(await h.read(b.fraud.headerHash)).toEqual([]);
    expect(h.releases.size).toBe(0);
    expect(h.requests).toEqual(["s0", "s0"]);
    expect(h.refusals).toMatchObject([
      { headerHash: b.fraud.headerHash, kind: "root-mismatch" },
    ]);
  });
});
