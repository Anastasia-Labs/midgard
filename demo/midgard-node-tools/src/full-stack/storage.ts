import { readdir } from "node:fs/promises";
import { join } from "node:path";

import { stackPaths } from "./deployment.js";
import {
  readJsonIfPresent,
  type StackJournal,
  writeDurableJson,
} from "./journal.js";
import type { StackProcesses } from "./process.js";
import type { StackStep } from "./workflow.js";

/**
 * A fresh identity may only start over a store that holds no deployment. The
 * node's migrations create their own ledger rows and one calibration seed row,
 * so a migrated, never-deployed database is fresh. Never clear a store.
 */
export const FRESH_STORAGE_QUERY =
  "DO $$ DECLARE relation record; populated boolean; BEGIN FOR relation IN SELECT tablename FROM pg_tables WHERE schemaname='public' AND tablename NOT IN ('schema_migrations', 'schema_migration_events') LOOP IF relation.tablename = 'commit_build_calibration' THEN SELECT EXISTS(SELECT 1 FROM public.commit_build_calibration WHERE NOT (id = 1 AND ms_per_tx_ewma = 1.0 AND sample_count = 0)) INTO populated; ELSE EXECUTE format('SELECT EXISTS(SELECT 1 FROM public.%I LIMIT 1)', relation.tablename) INTO populated; END IF; IF populated THEN RAISE EXCEPTION 'Fresh deployment requires empty local storage: %', relation.tablename; END IF; END LOOP; END $$; SELECT '{\"empty\":true}'::json";

export const IDENTITY_TABLE_QUERY =
  "SELECT json_build_object('exists', to_regclass('public.full_stack_controller_identity') IS NOT NULL)";
export const IDENTITY_ROW_QUERY =
  "SELECT row_to_json(identity) FROM full_stack_controller_identity identity";
export const CLUSTER_IDENTITY_QUERY =
  "SELECT json_build_object('id', system_identifier::text) FROM pg_control_system()";
export const ATTACHMENT_QUERY =
  "SELECT json_build_object('manifestId', encode(deployment_identity, 'hex')) FROM event_history_authority WHERE singleton";
/** Records the identity once; a second writer's row is kept and returned, never replaced. */
export const createIdentityQuery = (marker: {
  runId: string;
  manifestId: string;
}) =>
  `CREATE TABLE IF NOT EXISTS full_stack_controller_identity (singleton boolean PRIMARY KEY CHECK(singleton), run_id text NOT NULL, manifest_id text NOT NULL); INSERT INTO full_stack_controller_identity VALUES (true,'${marker.runId}','${marker.manifestId}') ON CONFLICT DO NOTHING; ${IDENTITY_ROW_QUERY}`;

const markerFile = (processes: StackProcesses) =>
  join(processes.config.nodeRoot, "db/full-stack-identity.json");
const journalFile = (processes: StackProcesses) =>
  join(processes.config.runDirectory, "stack-journal.json");
async function sql(processes: StackProcesses, query: string) {
  return processes.compose("storage-identity", [
    "exec",
    "-T",
    "postgres",
    "psql",
    "-v",
    "ON_ERROR_STOP=1",
    "-U",
    processes.env.POSTGRES_USER!,
    "-d",
    processes.env.POSTGRES_DB!,
    "-At",
    "-c",
    query,
  ]);
}
export async function assertPreservedStorage(processes: StackProcesses) {
  const journal = (await readJsonIfPresent(
    journalFile(processes),
  )) as StackJournal;
  const marker = (await readJsonIfPresent(markerFile(processes))) as
    | { runId: string; manifestId: string }
    | undefined;
  if (
    marker ||
    journal.steps.services ||
    journal.steps["storage-identity"]?.status === "complete"
  ) {
    const manifest = (await readJsonIfPresent(
      stackPaths(processes).manifest,
    )) as { manifestId: string };
    // A lost store has no identity table at all, which psql reports only as a failed query.
    const table = (await sql(processes, IDENTITY_TABLE_QUERY)) as {
      exists: boolean;
    };
    const observed = table.exists
      ? ((await sql(processes, IDENTITY_ROW_QUERY)) as {
          run_id: string;
          manifest_id: string;
        } | null)
      : null;
    if (
      observed?.run_id !== journal.runId ||
      observed.manifest_id !== manifest?.manifestId
    )
      throw new Error(
        "Postgres deployment marker is missing or changed; preserve the deployment",
      );
    if (
      marker &&
      (marker.runId !== journal.runId ||
        marker.manifestId !== manifest.manifestId)
    )
      throw new Error("Durable node storage identity changed");
    if (!marker)
      await writeDurableJson(markerFile(processes), {
        runId: observed.run_id,
        manifestId: observed.manifest_id,
      });
    return;
  }
  const paths = stackPaths(processes);
  const manifest = (await readJsonIfPresent(paths.manifest)) as
    | { manifestId?: string; steps?: { initProtocol?: { status?: string } } }
    | undefined;
  if (
    manifest?.steps?.initProtocol?.status === "complete" &&
    !journal.steps.initialize
  ) {
    const observed = (await sql(processes, ATTACHMENT_QUERY)) as {
      manifestId: string;
    } | null;
    if (observed?.manifestId !== manifest.manifestId)
      throw new Error(
        "Existing deployment cannot attach to missing or mismatched local event history",
      );
  }
  if (
    manifest?.steps?.initProtocol?.status === "complete" ||
    journal.steps.nonce ||
    journal.steps.initialize
  )
    return;
  await sql(processes, FRESH_STORAGE_QUERY);
  const files = await readdir(join(processes.config.nodeRoot, "db")).catch(
    (error: NodeJS.ErrnoException) => {
      if (error.code === "ENOENT") return [];
      throw error;
    },
  );
  if (files.length)
    throw new Error(
      "Fresh deployment cannot reuse populated node db directory",
    );
}
export async function establishStorageIdentity(processes: StackProcesses) {
  const journal = (await readJsonIfPresent(
    journalFile(processes),
  )) as StackJournal;
  const manifest = (await readJsonIfPresent(
    stackPaths(processes).manifest,
  )) as { manifestId: string };
  if (
    !/^[0-9a-f]{64}$/.test(manifest.manifestId) ||
    !/^[0-9a-f-]{36}$/.test(journal.runId)
  )
    throw new Error("Invalid storage identity");
  const marker = { runId: journal.runId, manifestId: manifest.manifestId };
  const prior = await readJsonIfPresent(markerFile(processes));
  if (prior !== undefined) {
    await assertPreservedStorage(processes);
    return;
  }
  const observed = (await sql(processes, createIdentityQuery(marker))) as {
    run_id: string;
    manifest_id: string;
  } | null;
  if (
    observed?.run_id !== marker.runId ||
    observed.manifest_id !== marker.manifestId
  )
    throw new Error("Storage identity differs from this deployment");
  await writeDurableJson(markerFile(processes), marker);
  await assertPreservedStorage(processes);
}

/**
 * Host commands (db:migrate, submit-deposit, submit-withdrawal) reach Postgres
 * through the host port; the storage checks run inside the compose service.
 * Both must be the same cluster.
 */
export async function assertHostDatabaseIsStackDatabase(
  processes: StackProcesses,
) {
  const inside = (await sql(processes, CLUSTER_IDENTITY_QUERY)) as {
    id?: string;
  } | null;
  const host = await processes.hostDatabaseIdentity();
  if (!inside?.id || inside.id !== host)
    throw new Error(
      `127.0.0.1:${processes.env.MIDGARD_POSTGRES_HOST_PORT} does not reach this stack's Postgres; refusing host database commands`,
    );
}

export function storageIdentityStep(processes: StackProcesses): StackStep {
  return {
    id: "storage-identity",
    reconcile: async (record) => {
      if ((await readJsonIfPresent(markerFile(processes))) !== undefined) {
        await assertPreservedStorage(processes);
        return { status: "complete", data: { preserved: true } };
      }
      if (record) {
        const observed = (await sql(processes, IDENTITY_TABLE_QUERY)) as {
          exists: boolean;
        };
        if (observed.exists) {
          const journal = (await readJsonIfPresent(
            journalFile(processes),
          )) as StackJournal;
          const manifest = (await readJsonIfPresent(
            stackPaths(processes).manifest,
          )) as { manifestId: string };
          const marker = (await sql(processes, IDENTITY_ROW_QUERY)) as {
            run_id: string;
            manifest_id: string;
          } | null;
          if (
            marker?.run_id !== journal.runId ||
            marker.manifest_id !== manifest.manifestId
          )
            throw new Error("Storage identity differs from this deployment");
          await writeDurableJson(markerFile(processes), {
            runId: marker.run_id,
            manifestId: marker.manifest_id,
          });
          return { status: "complete", data: { preserved: true } };
        }
        if (record.status === "complete")
          throw new Error("Previously established storage identity is missing");
      }
      return { status: "retry" };
    },
    execute: async () => {
      await establishStorageIdentity(processes);
      return { preserved: true };
    },
  };
}
