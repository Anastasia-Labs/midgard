import {
  computeDaSha256Hash,
  DaRequestResponseProtocol,
  encodeDaPayloadByHeaderResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import { beforeAll, describe, expect, it } from "vitest";

import {
  DaLibp2pRetainedDaSource,
  fetchRetainedDaPayloadByHeaderHash,
  isRetainedDaPayloadUnavailableError,
  type RetainedDaLibp2pTransport,
  type RetainedDaPayloadSource,
} from "../src/transition-trace/index.js";
import { buildPayloadFixture } from "./transition-trace-challenger.build-payload-fixture.js";

type Fixture = Awaited<ReturnType<typeof buildPayloadFixture>>;

let requested: Fixture;
let other: Fixture;

beforeAll(async () => {
  requested = await buildPayloadFixture({});
  // Same shape, different header: a well-formed copy of some other block.
  other = await buildPayloadFixture({ prevUtxosRoot: "aa".repeat(32) });
  expect(other.headerHash).not.toBe(requested.headerHash);
});

/** Serves `bytes` on every call, as one public peer would. */
const servingSource = (
  sourceId: string,
  bytes: () => Buffer,
): RetainedDaPayloadSource & { readonly calls: () => number } => {
  let calls = 0;
  return {
    sourceId,
    calls: () => calls,
    fetchPayloadByHeaderHash: async () => {
      calls += 1;
      return {
        ok: true as const,
        provenance: {
          trustClass: "public_or_permissionless_da" as const,
          sourceId: `${sourceId}/peer`,
          grade: "security" as const,
        },
        sourceId,
        sourcePeerId: "peer",
        payloadEnvelopeCbor: Buffer.from(bytes()),
        attempts: [],
      };
    },
  };
};

const fetchFrom = (sources: readonly RetainedDaPayloadSource[]) =>
  fetchRetainedDaPayloadByHeaderHash({
    headerHash: requested.headerHash,
    sources,
    retries: 1,
  });

type PeerServes =
  | "not_found"
  | {
      readonly bytes: () => Buffer;
      /** The payload hash the peer claims; defaults to its bytes' own hash. */
      readonly claimedHash?: () => Buffer;
    };

/** One libp2p source over scripted peers, logging payload requests in order. */
const libp2pSource = (served: Readonly<Record<string, PeerServes>>) => {
  const requests: string[] = [];
  const transport: RetainedDaLibp2pTransport = {
    request: async ({ peer, protocol }) => {
      if (protocol !== DaRequestResponseProtocol.payloadByHeader)
        throw new Error("metadata is not served here");
      requests.push(peer.peerId);
      const serves = served[peer.peerId]!;
      const headerHash = Buffer.from(requested.headerHash, "hex");
      if (serves === "not_found")
        return encodeDaPayloadByHeaderResponseCbor({
          status: "not_found",
          headerHash,
          payloadHash: null,
          payloadBytes: null,
          chunkManifest: null,
          reasonCode: null,
        });
      const bytes = serves.bytes();
      return encodeDaPayloadByHeaderResponseCbor({
        status: "found_inline",
        headerHash,
        payloadHash: serves.claimedHash?.() ?? computeDaSha256Hash(bytes),
        payloadBytes: bytes,
        chunkManifest: null,
        reasonCode: null,
      });
    },
  };
  const source = new DaLibp2pRetainedDaSource({
    sourceId: "committee-libp2p",
    deploymentFingerprint: "11".repeat(32),
    peers: Object.keys(served).map((peerId) => ({ peerId })),
    transport,
  });
  return { source, payloadRequests: () => [...requests] };
};

describe("retained-DA payload fetch keeps trying until a copy verifies", () => {
  it("skips a peer serving another block's payload and returns the honest copy", async () => {
    const wrong = servingSource(
      "public-wrong",
      () => other.payloadEnvelopeCbor,
    );
    const honest = servingSource(
      "public-honest",
      () => requested.payloadEnvelopeCbor,
    );
    const fetched = await fetchFrom([wrong, honest]);
    expect(fetched.sourceId).toBe("public-honest");
    expect(
      fetched.payloadEnvelopeCbor.equals(requested.payloadEnvelopeCbor),
    ).toBe(true);
    // The wrong copy is a failed attempt with its reason, never retried.
    expect(wrong.calls()).toBe(1);
    expect(fetched.attempts).toEqual([
      expect.objectContaining({
        sourceId: "public-wrong",
        status: "failed_verification",
        detail: expect.stringContaining("header mismatch"),
      }),
    ]);
  });

  it("fails with a typed unavailable error naming each refusal when every copy is wrong", async () => {
    const wrongHeader = servingSource(
      "public-wrong",
      () => other.payloadEnvelopeCbor,
    );
    const malformed = servingSource("public-malformed", () =>
      Buffer.from("d87980", "hex"),
    );
    const failure = await fetchFrom([wrongHeader, malformed]).catch(
      (error: unknown) => error,
    );
    expect(isRetainedDaPayloadUnavailableError(failure)).toBe(true);
    expect(failure).toMatchObject({
      code: "fetchFailed",
      reason: "payloadUnavailable",
      headerHash: requested.headerHash,
      // No source said anything about the requested payload itself.
      availability: "unreachable",
    });
    const message = (failure as Error).message;
    expect(message).toContain("public-wrong/peer");
    expect(message).toContain("header mismatch");
    expect(message).toContain("public-malformed/peer");
    expect(message).toContain("malformed payload");
    expect(wrongHeader.calls()).toBe(1);
    expect(malformed.calls()).toBe(1);
  });

  it("makes no extra request when the first copy verifies", async () => {
    const honest = servingSource(
      "public-honest",
      () => requested.payloadEnvelopeCbor,
    );
    const unused = servingSource("public-unused", () => {
      throw new Error("must not be asked");
    });
    const fetched = await fetchFrom([honest, unused]);
    expect(fetched.sourceId).toBe("public-honest");
    expect(fetched.attempts).toEqual([]);
    expect(honest.calls()).toBe(1);
    expect(unused.calls()).toBe(0);
  });

  it("moves past a wrong peer inside one libp2p source", async () => {
    const { source, payloadRequests } = libp2pSource({
      "peer-wrong": { bytes: () => other.payloadEnvelopeCbor },
      "peer-honest": { bytes: () => requested.payloadEnvelopeCbor },
    });
    const fetched = await fetchFrom([source]);
    expect(fetched.sourcePeerId).toBe("peer-honest");
    expect(
      fetched.payloadEnvelopeCbor.equals(requested.payloadEnvelopeCbor),
    ).toBe(true);
    expect(fetched.attempts).toEqual([
      expect.objectContaining({
        sourcePeerId: "peer-wrong",
        status: "failed_verification",
      }),
    ]);
    expect(payloadRequests()).toEqual(["peer-wrong", "peer-honest"]);
  });

  it("returns the honest copy after a peer whose bytes miss its own hash", async () => {
    const { source, payloadRequests } = libp2pSource({
      "peer-inconsistent": {
        bytes: () => requested.payloadEnvelopeCbor,
        claimedHash: () => computeDaSha256Hash(other.payloadEnvelopeCbor),
      },
      "peer-honest": { bytes: () => requested.payloadEnvelopeCbor },
    });
    const fetched = await fetchFrom([source]);
    expect(fetched.sourcePeerId).toBe("peer-honest");
    expect(
      fetched.payloadEnvelopeCbor.equals(requested.payloadEnvelopeCbor),
    ).toBe(true);
    expect(fetched.attempts).toEqual([
      expect.objectContaining({
        sourcePeerId: "peer-inconsistent",
        status: "invalid_content",
      }),
    ]);
    expect(payloadRequests()).toEqual(["peer-inconsistent", "peer-honest"]);
  });

  it("reports unavailable, not a fail-closed error, when a peer is self-inconsistent and the rest lack the payload", async () => {
    const { source, payloadRequests } = libp2pSource({
      "peer-inconsistent": {
        bytes: () => requested.payloadEnvelopeCbor,
        claimedHash: () => computeDaSha256Hash(other.payloadEnvelopeCbor),
      },
      "peer-empty": "not_found",
    });
    const failure = await fetchFrom([source]).catch((error: unknown) => error);
    expect(isRetainedDaPayloadUnavailableError(failure)).toBe(true);
    expect(failure).toMatchObject({
      code: "fetchFailed",
      reason: "payloadUnavailable",
      headerHash: requested.headerHash,
      availability: "unreachable",
    });
    expect((failure as Error).message).toContain("payload hash mismatch");
    // A bad copy is never asked for again within one fetch.
    expect(payloadRequests()).toEqual(["peer-inconsistent", "peer-empty"]);
  });
});
