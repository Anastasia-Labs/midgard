#!/usr/bin/env node
import { loadDaLibp2pIdentity } from "./da/libp2p/identity.js";
import { PublicRetainedDaListener } from "./da/libp2p/PublicRetainedDaListener.js";
import { loadPublicRetainedDaRuntimeConfig } from "./public-retained-da-config.js";
import { stopPublicRetainedDaRuntime } from "./public-retained-da-runtime.js";
import { PostgresPublicRetainedDaStore } from "./store/public-retained-da.js";

const main = async (): Promise<void> => {
  if (process.argv.includes("--help")) {
    printHelp();
    return;
  }
  if (process.argv.length > 2) {
    throw new Error("midgard-public-retained-da does not accept arguments");
  }
  const config = await loadPublicRetainedDaRuntimeConfig();
  const identity = await loadDaLibp2pIdentity(
    config.publicRetainedDa.privateKeySource,
  );
  if (identity.peerId !== config.publicRetainedDa.peerId) {
    throw new Error(
      "DA_PUBLIC_RETAINED_DA_PRIVATE_KEY_SOURCE does not match public_retained_da.peer_id",
    );
  }
  const store = await PostgresPublicRetainedDaStore.open({
    databaseUrl: config.databaseUrl,
    expectedRole: config.databaseRole,
  });
  const listener = new PublicRetainedDaListener({
    deploymentFingerprint: config.deploymentFingerprint,
    config: config.publicRetainedDa,
    store,
    privateKey: identity.privateKey,
    dataLimits: config.dataLimits,
  });
  try {
    await listener.start();
  } catch (error) {
    await stopPublicRetainedDaRuntime({ listener, store }).catch(
      () => undefined,
    );
    throw error;
  }
  process.stdout.write(
    `midgard-public-retained-da listening on ${listener.getMultiaddrs().join(", ")}\n`,
  );
  await waitForShutdown(listener, store);
};

const waitForShutdown = async (
  listener: PublicRetainedDaListener,
  store: PostgresPublicRetainedDaStore,
): Promise<void> =>
  new Promise((resolve, reject) => {
    let stopping = false;
    const shutdown = async (): Promise<void> => {
      if (stopping) return;
      stopping = true;
      try {
        await stopPublicRetainedDaRuntime({ listener, store });
        resolve();
      } catch (error) {
        reject(
          error instanceof Error
            ? error
            : new Error("Failed to stop retained DA runtime", { cause: error }),
        );
      } finally {
        stopping = true;
      }
    };
    process.once("SIGINT", () => void shutdown());
    process.once("SIGTERM", () => void shutdown());
  });

const printHelp = (): void => {
  process.stdout.write(`midgard-public-retained-da

Dedicated public retained-DA TCP/Noise/Yamux reader. It requires:
  DA_PUBLIC_RETAINED_DA_ENABLED=true
  MIDGARD_DEPLOYMENT_MANIFEST_PATH
  MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH
  DA_PUBLIC_RETAINED_DA_PRIVATE_KEY_SOURCE
  DA_PUBLIC_RETAINED_DA_DATABASE_URL
  DA_PUBLIC_RETAINED_DA_DATABASE_ROLE

The database role must have SELECT and no DML privileges on
watcher_da_payloads and watcher_state_queue_headers. File stores and the
committee WATCHER_DB_PATH/WATCHER_DATABASE_URL credentials are refused.
`);
};

main().catch((error) => {
  process.stderr.write(
    `${error instanceof Error ? (error.stack ?? error.message) : String(error)}\n`,
  );
  process.exit(1);
});
