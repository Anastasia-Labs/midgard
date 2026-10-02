import { createHash, X509Certificate } from "node:crypto";
import { type Server } from "node:net";
import { createServer as createTlsServer } from "node:tls";

import { expect } from "vitest";

import {
  evaluateWatcherFinality,
  makeWatcherFinalityPolicy,
  type WatcherFinalityPolicy,
  type WatcherFinalityState,
} from "../../src/l1/finality-engine.js";
import {
  closeWatcherL1TransportAttestationContext,
  establishWatcherExternalProviderTransport,
  normalizeWatcherL1Block,
  WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
  type WatcherL1TransportAttestationContext,
  type WatcherNormalizedL1Block,
} from "../../src/l1/l1-adapter.js";
import { evaluateWatcherMultiProviderConsistency } from "../../src/l1/multi-provider-consistency.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import {
  closeServer,
  deploymentIdentity,
  hex32,
  listen,
  type Point,
  testTlsIdentities,
  transaction,
} from "./rollback-engine.test-tls-identities.js";

/**
 * Two attested external providers that agree on every requested point. The
 * rollback state machine is source-mode agnostic, so this exercises the same
 * evidence checks as the local-node path without a compiled deployment
 * profile.
 */
export type ExternalAgreementHarness = Readonly<{
  attestations: readonly WatcherL1TransportAttestationContext[];
  policy: WatcherFinalityPolicy;
  observations: (point: Point) => readonly WatcherNormalizedL1Block[];
  agreement: (
    point: Point,
  ) => ReturnType<typeof evaluateWatcherMultiProviderConsistency>;
  pending: (point: Point) => WatcherFinalityState;
  finalized: (point: Point, finalDepth: string) => WatcherFinalityState;
  close: () => Promise<void>;
}>;

export const CONFIRMATION_DEPTH = 5;

export const openExternalAgreementHarness =
  async (): Promise<ExternalAgreementHarness> => {
    const servers: Server[] = [];
    const contexts = await Promise.all(
      testTlsIdentities.map(async ({ cert, key }, index) => {
        const server = createTlsServer({ cert, key }, (socket) => {
          socket.on("error", () => undefined);
        });
        await listen(server, 0, "127.0.0.1");
        servers.push(server);
        const address = server.address();
        if (address === null || typeof address === "string")
          throw new Error("missing TLS fixture address");
        const providerId = index === 0 ? "provider-a" : "provider-b";
        const endpoint = `https://localhost:${address.port.toString()}/${providerId}`;
        const established = await establishWatcherExternalProviderTransport({
          network: "Preprod",
          providerId,
          operatorIdentitySha256: index === 0 ? hex32("a1") : hex32("b2"),
          endpoint,
          caPem: cert,
          expectedTlsPublicIdentitySha256: createHash("sha256")
            .update(new X509Certificate(cert).raw)
            .digest("hex"),
          connectTimeoutMs: 5_000,
        });
        return { providerId, endpoint, established };
      }),
    );
    const [a, b] = contexts as [
      (typeof contexts)[number],
      (typeof contexts)[number],
    ];
    const attestations = Object.freeze([a.established, b.established]);
    const providers = [
      { id: a.providerId, identity: hex32("a1"), endpoint: a.endpoint },
      { id: b.providerId, identity: hex32("b2"), endpoint: b.endpoint },
    ];
    const source = {
      sourceMode: "external_providers",
      network: "Preprod",
      providers: providers.map(({ id, identity, endpoint }) => ({
        providerId: id,
        operatorIdentitySha256: identity,
        endpoint,
      })),
    } as const;
    const finalityPolicy = makeWatcherFinalityPolicy(
      {
        schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
        mode: "development",
        targetNetwork: "Preprod",
        l1: {
          source: {
            sourceMode: "external_providers",
            providers: providers.map(({ id, identity, endpoint }) => ({
              identity: id,
              operatorIdentitySha256: identity,
              endpoint,
            })),
          },
          requestTimeoutMs: 10_000,
          maxConcurrency: 4,
          finality: {
            depth: CONFIRMATION_DEPTH,
            rollback: {
              beforeFinality: "rewind",
              afterFinality: "quarantine",
              maxDepth: CONFIRMATION_DEPTH,
            },
          },
        },
        da: {
          peers: [
            {
              identity: "da-peer-a",
              multiaddr:
                "/dns4/da-a.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
            },
          ],
          requestTimeoutMs: 10_000,
          maxConcurrency: 4,
        },
        storage: {
          driver: "sqlite",
          path: "/var/lib/midgard-watcher/watcher.sqlite",
          rollbackAuthorityKeySource: {
            kind: "environment",
            variable: "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
          },
        },
        proverWallet: {
          keySource: {
            kind: "environment",
            variable: "MIDGARD_WATCHER_PROVER_KEY",
          },
        },
        deadlines: {
          daFetchMs: 60_000,
          daPublishMs: 60_000,
          proofConstructMs: 300_000,
          proofSubmitMs: 120_000,
        },
      },
      deploymentIdentity(),
    );
    if (finalityPolicy === null) throw new Error("Expected finality policy");
    const observations = (point: Point) =>
      attestations.map((context, index) =>
        normalizeWatcherL1Block(context, {
          schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
          network: "Preprod",
          providerId: providers[index]!.id,
          chainPoint: {
            blockHash: point.blockHash,
            parentBlockHash: point.parentBlockHash ?? null,
            slot: point.slot,
            blockNo: point.blockNo,
            depth: point.depth,
          },
          transactions:
            point.bodyHex === undefined ? [] : [transaction(point.bodyHex)],
        }),
      );
    const agreement = (point: Point) =>
      evaluateWatcherMultiProviderConsistency(
        source,
        observations(point),
        attestations,
      );
    const pending = (point: Point): WatcherFinalityState => {
      const result = evaluateWatcherFinality(
        finalityPolicy,
        null,
        agreement(point),
      );
      expect(result.action).toBe("observe_pending");
      return result.state as WatcherFinalityState;
    };
    const finalized = (point: Point, finalDepth: string) => {
      const result = evaluateWatcherFinality(
        finalityPolicy,
        pending(point),
        agreement({ ...point, depth: finalDepth }),
      );
      expect(result.action).toBe("finalize");
      return result.state as WatcherFinalityState;
    };
    return Object.freeze({
      attestations,
      policy: finalityPolicy,
      observations,
      agreement,
      pending,
      finalized,
      close: async () => {
        for (const context of attestations)
          closeWatcherL1TransportAttestationContext(context);
        await Promise.all(servers.map(closeServer));
      },
    });
  };
