import "./public-da-client.watcher-public-da-client-v1-bounds-enforcement.js";

import {
  computeDaSha256Hash,
  type DaPayloadChunkManifest,
  type DaPayloadChunkResponse,
  decodeDaPayloadChunkRequestCbor,
} from "@al-ft/midgard-core/da-transport";
import { describe, expect, it } from "vitest";

import {
  HEADER_HASH,
  PEERS,
  type PeerScript,
  ScriptedTransport,
} from "./public-da-client.raw-config.js";
import {
  capabilitiesBytes,
  chunkResponseBytes,
  chunksOf,
  clientWith,
  expectClientError,
  fixture,
  payloadByHeaderBytes,
  statuses,
} from "./public-da-client.watcher-public-da-client-v1-construction.js";

// ---------------------------------------------------------------------------
// 6. Chunk manifest validation and chunked reassembly
// ---------------------------------------------------------------------------

describe("WatcherPublicDaClientV1 chunked payload retrieval", () => {
  const CHUNK_SIZE = 64;

  const chunkedScript = (options?: {
    readonly manifest?: DaPayloadChunkManifest;
    readonly chunkFor?: (index: number, chunks: readonly Buffer[]) => Buffer;
    readonly chunkHashFor?: (
      index: number,
      chunks: readonly Buffer[],
    ) => Buffer;
    readonly chunkIndexFor?: (index: number) => number;
    readonly chunkStatus?: DaPayloadChunkResponse["status"];
  }): Record<string, PeerScript> => {
    const { chunks, manifest } = chunksOf(fixture.envelope, CHUNK_SIZE);
    return {
      [PEERS[0]!]: {
        capabilities: () =>
          capabilitiesBytes({
            maxPayloadBytes: 1_000_000,
            maxInlineResponseBytes: 128,
            maxChunkBytes: CHUNK_SIZE,
          }),
        "payload-by-header": () =>
          payloadByHeaderBytes({
            status: "found_chunked",
            payloadHash: fixture.payloadHash,
            payloadBytes: null,
            chunkManifest: options?.manifest ?? manifest,
          }),
        "payload-chunk": (request) => {
          const decoded = decodeDaPayloadChunkRequestCbor(request.requestCbor);
          const index = decoded.chunkIndex;
          return chunkResponseBytes({
            status: options?.chunkStatus ?? "found",
            payloadHash: fixture.payloadHash,
            chunkIndex: options?.chunkIndexFor?.(index) ?? index,
            chunkBytes: options?.chunkFor?.(index, chunks) ?? chunks[index]!,
            chunkHash:
              options?.chunkHashFor?.(index, chunks) ??
              computeDaSha256Hash(chunks[index]!),
          });
        },
      },
    };
  };

  it("reassembles a multi-chunk payload and verifies the whole-payload hash", async () => {
    const { chunks } = chunksOf(fixture.envelope, CHUNK_SIZE);
    expect(chunks.length).toBeGreaterThan(2);

    const transport = new ScriptedTransport(chunkedScript());
    const result = await clientWith(transport).fetchPayloadByHeader({
      headerHash: HEADER_HASH,
    });

    expect(result.payloadEnvelopeCbor.equals(fixture.envelope)).toBe(true);
    expect(result.innerPayloadCbor.equals(fixture.innerCbor)).toBe(true);
    expect(transport.protocolsFor(PEERS[0]!)).toEqual([
      "capabilities",
      "payload-by-header",
      ...chunks.map(() => "payload-chunk" as const),
    ]);
  });

  it("requests every chunk index in order", async () => {
    const { chunks } = chunksOf(fixture.envelope, CHUNK_SIZE);
    const transport = new ScriptedTransport(chunkedScript());
    await clientWith(transport).fetchPayloadByHeader({
      headerHash: HEADER_HASH,
    });
    const requested = transport.calls
      .filter((call) => call.protocol === "payload-chunk")
      .map(
        (call) => decodeDaPayloadChunkRequestCbor(call.requestCbor).chunkIndex,
      );
    expect(requested).toEqual(chunks.map((_, index) => index));
  });

  it("REJECTS a chunk whose bytes do not hash to the manifest entry", async () => {
    const transport = new ScriptedTransport(
      chunkedScript({
        chunkFor: (index, chunks) => {
          if (index !== 1) {
            return chunks[index]!;
          }
          const tampered = Buffer.from(chunks[1]!);
          tampered[0] ^= 0xff;
          return tampered;
        },
        // Peer still claims the honest manifest hash for the tampered chunk.
        chunkHashFor: (index, chunks) => computeDaSha256Hash(chunks[index]!),
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
    expect(error.attempts[0]!.protocol).toBe("payload-chunk");
  });

  it("REJECTS a chunk announcing a hash that is not the manifest entry", async () => {
    const transport = new ScriptedTransport(
      chunkedScript({
        chunkHashFor: (index, chunks) =>
          index === 0
            ? computeDaSha256Hash(Buffer.from("wrong"))
            : computeDaSha256Hash(chunks[index]!),
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a chunk answering the wrong index", async () => {
    const transport = new ScriptedTransport(
      chunkedScript({ chunkIndexFor: (index) => index + 1 }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a manifest committing to a different payload hash", async () => {
    const { manifest } = chunksOf(fixture.envelope, CHUNK_SIZE);
    const transport = new ScriptedTransport(
      chunkedScript({
        manifest: {
          ...manifest,
          payloadHash: computeDaSha256Hash(Buffer.from("other")),
        },
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
    expect(error.attempts[0]!.protocol).toBe("payload-by-header");
  });

  it("REJECTS a manifest whose chunk count contradicts totalBytes/chunkSize", async () => {
    const { manifest } = chunksOf(fixture.envelope, CHUNK_SIZE);
    const transport = new ScriptedTransport(
      chunkedScript({
        manifest: {
          ...manifest,
          chunkHashes: manifest.chunkHashes.slice(0, -1),
        },
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a manifest with an empty chunk list", async () => {
    const { manifest } = chunksOf(fixture.envelope, CHUNK_SIZE);
    const transport = new ScriptedTransport(
      chunkedScript({ manifest: { ...manifest, chunkHashes: [] } }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a manifest whose chunkSize exceeds the negotiated chunk ceiling", async () => {
    const { manifest } = chunksOf(fixture.envelope, CHUNK_SIZE);
    const transport = new ScriptedTransport(
      chunkedScript({
        manifest: {
          ...manifest,
          chunkSize: CHUNK_SIZE * 4,
          chunkHashes: manifest.chunkHashes.slice(
            0,
            Math.ceil(manifest.totalBytes / (CHUNK_SIZE * 4)),
          ),
        },
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it.each([
    ["zero totalBytes", 0],
    ["totalBytes above the payload ceiling", 2_000_000],
  ])("REJECTS a manifest with %s", async (_label, totalBytes) => {
    const { manifest } = chunksOf(fixture.envelope, CHUNK_SIZE);
    const transport = new ScriptedTransport(
      chunkedScript({ manifest: { ...manifest, totalBytes } }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a final chunk of the wrong length", async () => {
    const { chunks, manifest } = chunksOf(fixture.envelope, CHUNK_SIZE);
    const lastIndex = chunks.length - 1;
    // Pad the final chunk and restate the manifest hash so only the declared
    // length check can catch it.
    const padded = Buffer.concat([chunks[lastIndex]!, Buffer.alloc(1, 0)]);
    const transport = new ScriptedTransport(
      chunkedScript({
        manifest: {
          ...manifest,
          chunkHashes: manifest.chunkHashes.map((hash, index) =>
            index === lastIndex ? computeDaSha256Hash(padded) : hash,
          ),
        },
        chunkFor: (index, allChunks) =>
          index === lastIndex ? padded : allChunks[index]!,
        chunkHashFor: (index, allChunks) =>
          index === lastIndex
            ? computeDaSha256Hash(padded)
            : computeDaSha256Hash(allChunks[index]!),
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it.each([
    ["not_found", "not_found", "not_found"],
    ["rejected", "rejected", "peer_rejected"],
  ] as const)(
    "maps a %s chunk response to attempt status %s",
    async (_label, chunkStatus, expected) => {
      const transport = new ScriptedTransport(chunkedScript({ chunkStatus }));
      const error = await expectClientError(
        clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
      );
      expect(statuses(error.attempts)).toEqual([expected]);
    },
  );
});
