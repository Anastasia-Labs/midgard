import { execFileSync } from "node:child_process";
import { randomBytes } from "node:crypto";
import { existsSync } from "node:fs";
import { writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  identityFromSeedHex,
  loadDaLibp2pIdentity,
} from "@al-ft/midgard-core/da-libp2p-identity";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_PUBLIC_RETAINED_DA_PROTOCOLS,
  DA_TRANSPORT_LIMITS,
} from "@al-ft/midgard-core/da-transport";
import { PublicRetainedDaListener } from "da-committee-node/da/libp2p";
import { libp2pSubmittedDaPayloadRecord } from "da-committee-node/store";
import { PostgresWatcherStore } from "da-committee-node/store/postgres";
import { PostgresPublicRetainedDaStore } from "da-committee-node/store/public-retained-da";

/** Real committee storage and the production TCP/Noise/Yamux public reader. */
export const startJourneyRetainedDa = async ({
  runDirectory,
  runEnv,
  deploymentFingerprint,
}: {
  runDirectory: string;
  runEnv: Readonly<Record<string, string>>;
  deploymentFingerprint: string;
}) => {
  const database = runEnv.MIDGARD_PHASE4_POSTGRES_DATABASE!;
  const username = runEnv.MIDGARD_PHASE4_POSTGRES_USER!;
  const password = runEnv.MIDGARD_PHASE4_POSTGRES_PASSWORD!;
  const databaseUrl = new URL(
    `postgres://127.0.0.1:${runEnv.MIDGARD_PHASE4_POSTGRES_PORT}/${database}`,
  );
  databaseUrl.username = username;
  databaseUrl.password = password;
  const writer = await PostgresWatcherStore.open(databaseUrl.toString());
  const cleanup: (() => Promise<void>)[] = [() => writer.close()];
  const close = async () => {
    const failures: unknown[] = [];
    for (const action of cleanup.splice(0).reverse()) {
      try {
        await action();
      } catch (cause) {
        failures.push(cause);
      }
    }
    if (failures.length > 0)
      throw new AggregateError(failures, "Could not close retained DA");
  };
  try {
    const readerRole = "watcher_journey_reader";
    const readerPassword = randomBytes(32).toString("hex");
    execFileSync(
      "docker",
      [
        "exec",
        "-i",
        `${runEnv.MIDGARD_PHASE4_COMPOSE_PROJECT}-postgres-1`,
        "psql",
        "-U",
        username,
        "-d",
        database,
        "-v",
        "ON_ERROR_STOP=1",
      ],
      {
        input: `DO $$ BEGIN IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = '${readerRole}') THEN CREATE ROLE ${readerRole} LOGIN; END IF; END $$;
      ALTER ROLE ${readerRole} PASSWORD '${readerPassword}';
      GRANT USAGE ON SCHEMA public TO ${readerRole};
      GRANT SELECT ON watcher_da_payloads, watcher_state_queue_headers TO ${readerRole};`,
        stdio: ["pipe", "pipe", "pipe"],
      },
    );
    const readerUrl = new URL(databaseUrl);
    readerUrl.username = readerRole;
    readerUrl.password = readerPassword;
    const reader = await PostgresPublicRetainedDaStore.open({
      databaseUrl: readerUrl.toString(),
      expectedRole: readerRole,
    });
    cleanup.push(() => reader.close());
    const keyPath = join(runDirectory, "secrets/public-da-key.hex");
    const identity = existsSync(keyPath)
      ? await loadDaLibp2pIdentity(`file:${keyPath}`)
      : await identityFromSeedHex(randomBytes(32).toString("hex"));
    if (!existsSync(keyPath))
      await writeFile(keyPath, identity.privateKeyProtobufHex, {
        mode: 0o600,
        flag: "wx",
      });
    const listener = new PublicRetainedDaListener({
      deploymentFingerprint,
      privateKey: identity.privateKey,
      store: reader,
      config: {
        peerId: identity.peerId,
        privateKeySource: `file:${join(runDirectory, "secrets/public-da-key.hex")}`,
        listenMultiaddrs: ["/ip4/127.0.0.1/tcp/0"],
        announceMultiaddrs: [],
        protocols: DA_PUBLIC_RETAINED_DA_PROTOCOLS,
        limits: {
          maxStreamsPerPeer: 8,
          maxInflightRequests: 32,
          maxInflightRequestsPerPeer: 8,
          maxInflightProofRequests: 8,
          requestTimeoutMs: 30_000,
        },
      },
      dataLimits: {
        maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
        maxInlineResponseBytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
        maxChunkBytes: DA_TRANSPORT_LIMITS.maxChunkBytes,
        maxStreamsPerPeer: 8,
        requestTimeoutMs: 30_000,
      },
    });
    cleanup.push(() => listener.stop());
    await listener.start();
    const [multiaddr] = listener.getMultiaddrs();
    if (multiaddr === undefined)
      throw new Error("Public DA did not bind a TCP address");
    return {
      peer: { identity: "journey-committee", multiaddr },
      retain: async (block: {
        headerHash: string;
        payloadEnvelopeCbor: Uint8Array;
      }) => {
        const payloadCbor = Buffer.from(block.payloadEnvelopeCbor);
        await unwrapDaPayload(payloadCbor, {
          maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
        });
        await writer.saveDaPayload(
          libp2pSubmittedDaPayloadRecord({
            deploymentFingerprint,
            headerHash: block.headerHash,
            payloadSchemaVersion: 1,
            payloadCbor,
            payloadSha256: computeDaSha256Hash(payloadCbor).toString("hex"),
            receivedAt: new Date(),
          }),
        );
      },
      close,
    };
  } catch (cause) {
    await close();
    throw cause;
  }
};
