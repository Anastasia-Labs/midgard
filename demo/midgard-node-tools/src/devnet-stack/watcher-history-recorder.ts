import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";

import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";
import { PgClient } from "@effect/sql-pg";
import { Effect, ManagedRuntime, Redacted } from "effect";

import { committeeDatabase, databaseUrl, PUBLIC_READER_ROLE } from "./da.js";
import type { DeployContext } from "./deploy.js";
import { writeDurableFile } from "./durable.js";
import type { AdmittedHistoryChild } from "./history-child-admission.js";
import type { HistoryReadinessDispatch } from "./history-child-startup.js";
import { answerHistoryRecorderReadiness } from "./history-recorder-readiness.js";
import { startHistoryRecorderSource } from "./history-recorder-source.js";
import { DEFAULT_POLICY } from "./supervisor.js";
import { HISTORY_ROLES } from "./watcher-history.js";
import { createHistoryChainFollower } from "./watcher-history-chain.js";
import {
  loadWatcherModule,
  readFinalizedManifest,
  releasePaths,
} from "./watcher-release.js";

const PAYLOAD_POLL_MS = 15_000;

/**
 * Makes every provider hold one historical payload record. A record any
 * provider already holds is copied as is, never re-derived, so a crash between
 * two providers' writes converges on the next pass. Returns false when the
 * providers already disagree, which only an operator can settle.
 */
export const retainPayloadRecord = (
  paths: readonly string[],
  derive: () => string,
): boolean => {
  const held = paths
    .filter((path) => existsSync(path))
    .map((path) => readFileSync(path, "utf8"));
  if (held.some((bytes) => bytes !== held[0])) return false;
  const bytes = held[0] ?? derive();
  for (const path of paths)
    if (!existsSync(path)) writeDurableFile(path, bytes, 0o644);
  return true;
};

/**
 * Feeds both history providers from the chain itself and from the committee's
 * retained payloads:
 *
 * - every block, through the watcher's own native chain-sync client and block
 *   admission, into the providers' canonical chain and native-script
 *   publication records (the watcher journeys' archive format), resuming from
 *   the newest block the providers already retain;
 * - every payload committee member 0 verified, once its header's commit
 *   transaction is `confirmationDepth` deep on that chain, as a historical
 *   payload record at the commit's real inclusion point.
 *
 * It reads the committee store as the SELECT-only public reader. Any failure
 * ends the process; the supervisor restarts it and it resumes.
 */
export const runHistoryRecorder = async (
  context: DeployContext,
  child: AdmittedHistoryChild,
  dispatch?: HistoryReadinessDispatch,
) => {
  const { layout, run, identities, artifacts } = context;
  const watcher = await loadWatcherModule(layout);
  // The recorder source parses this itself: the watcher copy bundled into
  // this CLI only admits configs it parsed, so a config already parsed by the
  // loaded watcher dist would be rejected there.
  const watcherConfig: unknown = JSON.parse(
    readFileSync(layout.watcherRuntimeConfig, "utf8"),
  );
  const manifest = JSON.parse(
    readFileSync(releasePaths(layout).manifest, "utf8"),
  ) as {
    manifestId: string;
    l1Finality: { confirmationDepth: number };
  };
  const stateQueuePolicyId =
    readFinalizedManifest(layout).contracts.stateQueueMint?.scriptHash;
  if (stateQueuePolicyId === undefined)
    throw new Error("the deployment has no state-queue policy");
  const depth = BigInt(manifest.l1Finality.confirmationDepth);
  const directories = HISTORY_ROLES.map((role) =>
    layout.watcherHistoryArchive(role),
  );
  const chain = createHistoryChainFollower({
    directories,
    commitsDirectory: layout.watcherHistoryCommits,
    stateQueuePolicyId,
    admit: (event) => watcher.admitWatcherNativeRollForwardBlock(event),
  });

  const native = await startHistoryRecorderSource({
    actor: child.actor,
    directories,
    chain,
    watcherConfig,
    binaryPath: artifacts.transportBinary,
    signal: dispatch?.signal,
  });
  let stopReadiness: () => void;
  try {
    stopReadiness = answerHistoryRecorderReadiness(
      {
        actor: child.actor,
        directories,
        sealer: native.sealer,
        timeoutMs: DEFAULT_POLICY.probeTimeoutMs,
        current: async (deadline) => {
          try {
            child.current(deadline);
            return true;
          } catch {
            return false;
          }
        },
      },
      dispatch,
    );
  } catch (error) {
    await native.close();
    throw error;
  }

  const database = ManagedRuntime.make(
    PgClient.layer({
      url: Redacted.make(
        databaseUrl(
          run,
          committeeDatabase(0),
          PUBLIC_READER_ROLE,
          identities.publicReaderPassword,
        ),
      ),
      maxConnections: 1,
      applicationName: "devnet-history-recorder",
    }),
  );
  const query = <T>(
    use: (sql: PgClient.PgClient) => Effect.Effect<T, unknown>,
  ) => database.runPromise(Effect.flatMap(PgClient.PgClient, use));

  const recordPath = (directory: string, headerHash: string) =>
    join(directory, "records", `${headerHash}.json`);

  const archivePayloads = async () => {
    const tip = chain.latestBlockNo();
    if (tip === undefined) return 0;
    const headers = await query(
      (sql) =>
        sql<{ headerHash: string; status: string | null }>`
        SELECT h.header_hash AS "headerHash",
               p.record->>'validationStatus' AS "status"
        FROM committee_state_queue_headers h
        JOIN committee_da_payloads p ON p.header_hash = h.header_hash
        WHERE h.record->>'deploymentFingerprint' = ${manifest.manifestId}`,
    );
    let written = 0;
    for (const { headerHash, status } of headers) {
      if (status !== "verified" || !/^[0-9a-f]{56}$/u.test(headerHash))
        continue;
      const paths = directories.map((directory) =>
        recordPath(directory, headerHash),
      );
      if (paths.every((path) => existsSync(path))) {
        chain.commits.forget(headerHash);
        continue;
      }
      let record: () => string = () => {
        throw new Error("unreachable: a provider already holds the record");
      };
      if (!paths.some((path) => existsSync(path))) {
        // The inclusion point is the block of the transaction that committed
        // the header, never wherever its state-queue node sits now.
        const point = chain.commits.get(headerHash);
        if (point === undefined || tip - BigInt(point.blockNo) + 1n < depth)
          continue;
        const [row] = await query(
          (sql) =>
            sql<{ payload: string | null }>`
            SELECT record->>'payloadCborHex' AS "payload"
            FROM committee_da_payloads WHERE header_hash = ${headerHash}`,
        );
        const payload = row?.payload;
        if (payload == null || payload.length === 0) continue;
        record = () =>
          JSON.stringify({
            schemaVersion:
              "midgard-production-historical-native-script-history-record-v1",
            deploymentFingerprint: manifest.manifestId,
            headerHash,
            payloadEnvelopeCborHex: payload,
            inclusionPoint: {
              ...point,
              pointId: computeFraudProofRawL1PointId(point),
            },
          });
      }
      const converged = retainPayloadRecord(paths, record);
      if (!converged) {
        // One header's disagreement never holds back the others.
        console.error(`payload archive: providers disagree on ${headerHash}`);
        continue;
      }
      chain.commits.forget(headerHash);
      written += 1;
    }
    return written;
  };

  const stopped = new AbortController();
  const stop = () => stopped.abort();
  if (dispatch?.signal.aborted) stop();
  else dispatch?.signal.addEventListener("abort", stop, { once: true });
  for (const signal of ["SIGTERM", "SIGINT"] as const)
    process.once(signal, stop);
  const polling = (async () => {
    while (!stopped.signal.aborted) {
      try {
        const written = await archivePayloads();
        if (written > 0)
          console.log(
            JSON.stringify({
              archivedPayloads: written,
              tip: String(chain.latestBlockNo()),
            }),
          );
      } catch (error) {
        // The reader role exists once member 0 has migrated; retry quietly.
        console.error(
          `payload archive: ${error instanceof Error ? error.message : String(error)}`,
        );
      }
      await new Promise<void>((resolve) => {
        const timer = setTimeout(resolve, PAYLOAD_POLL_MS);
        stopped.signal.addEventListener(
          "abort",
          () => {
            clearTimeout(timer);
            resolve();
          },
          { once: true },
        );
      });
    }
  })();
  console.log(
    JSON.stringify({ service: "history-recorder", state: "started" }),
  );
  try {
    await Promise.race([
      native.done,
      child.refusal.then((error) => {
        throw error;
      }),
      new Promise<void>((resolve) => {
        if (stopped.signal.aborted) resolve();
        else
          stopped.signal.addEventListener("abort", () => resolve(), {
            once: true,
          });
      }),
    ]);
    if (!stopped.signal.aborted) throw new Error("native chain-sync ended");
  } finally {
    stopReadiness();
    dispatch?.signal.removeEventListener("abort", stop);
    for (const signal of ["SIGTERM", "SIGINT"] as const)
      process.removeListener(signal, stop);
    stopped.abort();
    await polling;
    await native.close();
    await database.dispose();
  }
};
