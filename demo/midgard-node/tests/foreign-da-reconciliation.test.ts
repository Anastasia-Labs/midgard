import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
  DaRequestResponseProtocol,
  decodeDaPayloadByHeaderRequestCbor,
  encodeDaCapabilitiesResponseCbor,
  encodeDaPayloadByHeaderResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import {
  type DeploymentManifest,
  makeDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Option, Ref } from "effect";
import {
  WatcherPublicDaClient,
  type WatcherPublicDaLibp2pTransportV1,
  type WatcherPublicDaRequest,
} from "midgard-watcher/public-da-client";
import { beforeAll, describe, expect, it } from "vitest";

import { FOREIGN_DA_RETRY_POLICY } from "../src/da/foreign-da-retry-policy.js";
import { ForeignPayloadRetriever } from "../src/da/foreign-payload-retriever.js";
import { downloadedForeignPayloadRow } from "../src/da/foreign-payload-store.js";
import * as Authority from "../src/database/eventHistoryAuthority.js";
import {
  DaPayloadsDB,
  ForeignTipReconciliationsDB,
} from "../src/database/index.js";
import { consumeDownloadedForeignDa } from "../src/fibers/foreign-da-reconciliation.consume-payload.js";
import type { HistoryOwnerCoverage } from "../src/services/event-history-owner.js";
import { ContractDeploymentIdentity, Globals } from "../src/services/index.js";
import { resolveT2ForeignEventEvidence } from "../src/workers/t2-foreign-event-reconciliation.resolve-t2-foreign-event-evidence.js";
import { makeFinalizedDeploymentManifestFixture } from "./helpers/finalized-deployment-manifest.js";
import { oneDepositPayload } from "./helpers/foreign-da-payload.js";
import { provideDatabaseLayers } from "./utils.js";

const depositId = "61".repeat(32);
const emptyIds = { deposits: [], forcedTransactions: [], withdrawals: [] };
const peers = ["peer-a", "peer-b"].map((identity, index) => ({
  identity,
  multiaddr: `/dns4/da-${identity}.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz1234${index === 0 ? "A" : "B"}`,
}));
let manifest: DeploymentManifest;
let fixture: Awaited<ReturnType<typeof oneDepositPayload>>;
let envelope: Buffer;
// Loads and parameterises the real contracts: ~10 s on a 4-vCPU CI runner.
beforeAll(async () => {
  manifest = await makeFinalizedDeploymentManifestFixture();
  fixture = await oneDepositPayload(depositId);
  envelope = await wrapDaPayload(SDK.encodeDaPayload(fixture.payload), {
    mode: "identity",
  });
}, 120_000);

const clientFor = (transport: WatcherPublicDaLibp2pTransportV1) =>
  new WatcherPublicDaClient({
    deploymentManifest: manifest,
    peers,
    requestTimeoutMs: 1000,
    fetchTimeoutMs: 5000,
    maxConcurrency: 1,
    transport,
  });
const capability = () =>
  encodeDaCapabilitiesResponseCbor({
    deploymentFingerprint: Buffer.from(manifest.manifestId, "hex"),
    transportProtocolVersion: DA_TRANSPORT_PROTOCOL_VERSION,
    payloadSchemaVersions: [1],
    envelopeContentEncodings: [0],
    ...DA_TRANSPORT_LIMITS,
  });
const response = (bytes: Buffer | null) =>
  encodeDaPayloadByHeaderResponseCbor({
    status: bytes === null ? "not_found" : "found_inline",
    headerHash: Buffer.from(fixture.payload.block_body.header_hash, "hex"),
    payloadHash: bytes === null ? null : computeDaSha256Hash(bytes),
    payloadBytes: bytes,
    chunkManifest: null,
    reasonCode: null,
  });
const transportFor = (
  answer: (request: WatcherPublicDaRequest) => Buffer,
): WatcherPublicDaLibp2pTransportV1 => ({
  request: async (request) =>
    request.protocol === DaRequestResponseProtocol.capabilities
      ? capability()
      : answer(request),
});

const clearHistoryAuthority = Effect.flatMap(
  SqlClient.SqlClient,
  (sql) => sql`DELETE FROM event_history_authority`,
);

describe("foreign header payload retrieval", () => {
  it("rejects self-consistent transport bytes with substituted roots, tries another peer, and retains the present-event hold", async () => {
    const substituted = structuredClone(fixture.payload);
    substituted.block_body.deposits = [["62".repeat(32), "01"]];
    const bad = await wrapDaPayload(SDK.encodeDaPayload(substituted), {
      mode: "identity",
    });
    const client = clientFor(
      transportFor((request) =>
        response(request.peerIdentity === "peer-a" ? bad : envelope),
      ),
    );
    const fetched = await new ForeignPayloadRetriever(client).fetch(
      fixture.payload.block_body.header_hash,
      fixture.header,
    );
    expect(fetched?.sourcePeerIdentity).toBe("peer-b");
    expect(fetched?.attempts.map((attempt) => attempt.status)).toEqual([
      "invalid_content",
      "success",
    ]);
    const resolution = await Effect.runPromise(
      resolveT2ForeignEventEvidence({
        foreignHeaderHash: fixture.payload.block_body.header_hash,
        header: fixture.header,
        candidateIds: { ...emptyIds, deposits: [depositId] },
        payload: fixture.payload,
      }),
    );
    expect(resolution).toMatchObject({
      type: "AwaitingForeignDa",
      reason: "foreign_event_present_requires_finalization",
    });
    client.close();
  });

  it("bounds failed episodes across ticks, expires cooldown automatically, and never treats a timeout as reusable evidence", async () => {
    let now = 0;
    let available = false;
    let payloadRequests = 0;
    const client = clientFor(
      transportFor(() => {
        payloadRequests++;
        return response(available ? envelope : null);
      }),
    );
    const retriever = new ForeignPayloadRetriever(client, () => now);
    const hash = fixture.payload.block_body.header_hash;
    for (let attempt = 0; attempt < 4; attempt++) {
      await expect(retriever.fetch(hash, fixture.header)).rejects.toThrow(
        "all_peers_failed",
      );
      await expect(
        retriever.fetch(hash, fixture.header),
      ).resolves.toBeUndefined();
      now += Math.min(
        FOREIGN_DA_RETRY_POLICY.backoffMs * 2 ** attempt,
        FOREIGN_DA_RETRY_POLICY.backoffMaxMs,
      );
    }
    expect(payloadRequests).toBe(8);
    available = true;
    await expect(
      retriever.fetch(hash, fixture.header),
    ).resolves.toBeUndefined();
    expect(
      await Effect.runPromise(
        resolveT2ForeignEventEvidence({
          foreignHeaderHash: hash,
          header: fixture.header,
          candidateIds: emptyIds,
        }),
      ),
    ).toMatchObject({ type: "AwaitingForeignDa", reason: "missing" });
    now += FOREIGN_DA_RETRY_POLICY.cooldownMs;
    expect((await retriever.fetch(hash, fixture.header))?.headerHash).toBe(
      hash,
    );
    expect(payloadRequests).toBe(9);
    client.close();
  });

  it("expires a full retry cache so a later available header is not permanently starved", async () => {
    let now = 0;
    let requests = 0;
    const wantedHash = fixture.payload.block_body.header_hash;
    const client = clientFor(
      transportFor((request) => {
        requests++;
        const asked = decodeDaPayloadByHeaderRequestCbor(
          request.requestCbor,
        ).headerHash;
        return asked.toString("hex") === wantedHash
          ? response(envelope)
          : encodeDaPayloadByHeaderResponseCbor({
              status: "not_found",
              headerHash: asked,
              payloadHash: null,
              payloadBytes: null,
              chunkManifest: null,
              reasonCode: null,
            });
      }),
    );
    const retriever = new ForeignPayloadRetriever(client, () => now);
    for (let i = 0; i < FOREIGN_DA_RETRY_POLICY.maxHeaders; i++) {
      await expect(
        retriever.fetch(i.toString(16).padStart(56, "0"), fixture.header),
      ).rejects.toThrow("all_peers_failed");
    }
    expect(requests).toBe(128);
    await expect(
      retriever.fetch("ff".repeat(28), fixture.header),
    ).resolves.toBeUndefined();
    await expect(
      retriever.fetch(wantedHash, fixture.header),
    ).resolves.toBeUndefined();
    now +=
      FOREIGN_DA_RETRY_POLICY.backoffMs + FOREIGN_DA_RETRY_POLICY.cooldownMs;
    expect(
      (await retriever.fetch(wantedHash, fixture.header))?.headerHash,
    ).toBe(wantedHash);
    expect(requests).toBe(129);
    client.close();
  });

  it.skipIf(process.env.MIDGARD_SKIP_DB_TESTS === "1")(
    "stores authenticated downloaded bytes without clearing durable foreign evidence, and rejects changed deployment or content",
    async () => {
      const client = clientFor(transportFor(() => response(envelope)));
      const fetched = (await new ForeignPayloadRetriever(client).fetch(
        fixture.payload.block_body.header_hash,
        fixture.header,
      ))!;
      const marker = makeDeploymentMarker(manifest.manifestId);
      await Effect.runPromise(
        provideDatabaseLayers(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            // The authority row is a singleton: a row another file left on
            // this shard (any deployment, any owner) would refuse the claim.
            yield* clearHistoryAuthority;
            yield* sql`DELETE FROM foreign_tip_reconciliations WHERE foreign_header_hash = ${Buffer.from(fetched.headerHash, "hex")}`;
            yield* sql`DELETE FROM da_payloads WHERE header_hash = ${Buffer.from(fetched.headerHash, "hex")}`;
            yield* ForeignTipReconciliationsDB.recordMismatch({
              foreignHeaderHash: fetched.headerHash,
              replacedBaseHeaderHash: "53".repeat(28),
              foreignHeader: fixture.header,
              consensusProfile: MIDGARD_CONSENSUS_PROFILE,
              deploymentMarker: marker,
            });
            const entry = Option.getOrThrow(
              yield* ForeignTipReconciliationsDB.retrieveByForeignHeaderHash(
                fetched.headerHash,
              ),
            );
            const row = yield* Effect.promise(() =>
              downloadedForeignPayloadRow(
                entry,
                fixture.header,
                fetched,
                marker,
              ),
            );
            yield* DaPayloadsDB.upsertAvailable(row);
            const stored = Option.getOrThrow(
              yield* DaPayloadsDB.retrieveByHeaderHash(row.header_hash),
            );
            expect(stored.payload_cbor).toEqual(envelope);
            expect(
              Option.getOrThrow(
                yield* ForeignTipReconciliationsDB.retrieveByForeignHeaderHash(
                  fetched.headerHash,
                ),
              ).status,
            ).toBe("awaiting");
            const deploymentIdentity = ContractDeploymentIdentity.make({
              kind: "manifest",
              manifestId: manifest.manifestId,
              deploymentMarker: marker,
              consensusProfile: MIDGARD_CONSENSUS_PROFILE,
              manifest,
            });
            const missingOwner = yield* Ref.make(undefined);
            const held = yield* Effect.either(
              consumeDownloadedForeignDa(entry, row).pipe(
                Effect.provideService(
                  ContractDeploymentIdentity,
                  deploymentIdentity,
                ),
                Effect.provideService(
                  Globals,
                  Globals.make({ EVENT_HISTORY_OWNER: missingOwner } as never),
                ),
              ),
            );
            expect(held._tag).toBe("Left");
            expect(
              Option.getOrThrow(
                yield* ForeignTipReconciliationsDB.retrieveByForeignHeaderHash(
                  fetched.headerHash,
                ),
              ).status,
            ).toBe("awaiting");
            // Model the owner API with the real durable Ready transaction gate.
            const token = yield* Authority.acquire({
              deploymentIdentity: manifest.manifestId,
              ownerToken: "4ab543a0-3346-4990-b302-d1e42c163f14",
              leaseDurationMs: 30_000,
            });
            const capture = {
              point: { id: "81".repeat(32), slot: 1 },
              snapshotDigest: "82".repeat(32),
            };
            yield* Authority.publishReady(token, capture);
            let admissions = 0;
            const owner = {
              runProducer: <A, E, R>(
                work: (
                  token: Authority.Token,
                  guard: Effect.Effect<void>,
                  coverage: HistoryOwnerCoverage,
                ) => Effect.Effect<A, E, R>,
              ) => {
                admissions++;
                return Authority.withReady(
                  token,
                  work(token, Effect.void, {
                    point: capture.point,
                    snapshotDigest: capture.snapshotDigest,
                  } as HistoryOwnerCoverage),
                );
              },
            };
            const ownerRef = yield* Ref.make(owner);
            const resolved = yield* consumeDownloadedForeignDa(entry, row).pipe(
              Effect.provideService(
                ContractDeploymentIdentity,
                deploymentIdentity,
              ),
              Effect.provideService(
                Globals,
                Globals.make({ EVENT_HISTORY_OWNER: ownerRef } as never),
              ),
            );
            expect(admissions).toBe(1);
            expect(resolved.type).toBe("Ready");
            expect(
              Option.getOrThrow(
                yield* ForeignTipReconciliationsDB.retrieveByForeignHeaderHash(
                  fetched.headerHash,
                ),
              ).status,
            ).toBe("resolved");
            yield* Effect.promise(() =>
              expect(
                downloadedForeignPayloadRow(
                  entry,
                  fixture.header,
                  { ...fetched, deploymentFingerprint: "00".repeat(32) },
                  marker,
                ),
              ).rejects.toThrow("identity mismatch"),
            );
            const substituted = structuredClone(fixture.payload);
            substituted.block_body.deposits = [["63".repeat(32), "01"]];
            const wrongEnvelope = yield* Effect.promise(() =>
              wrapDaPayload(SDK.encodeDaPayload(substituted), {
                mode: "identity",
              }),
            );
            yield* Effect.promise(() =>
              expect(
                downloadedForeignPayloadRow(
                  entry,
                  fixture.header,
                  {
                    ...fetched,
                    payloadEnvelopeCbor: wrongEnvelope,
                    payloadHash:
                      computeDaSha256Hash(wrongEnvelope).toString("hex"),
                  },
                  marker,
                ),
              ).rejects.toThrow("root, or count"),
            );
          }).pipe(Effect.ensuring(Effect.orDie(clearHistoryAuthority))),
        ),
      ).then(() => undefined);
      client.close();
    },
  );
});
