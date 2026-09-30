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
    const observed = (await sql(
      processes,
      "SELECT row_to_json(identity) FROM full_stack_controller_identity identity",
    )) as { run_id: string; manifest_id: string };
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
    const observed = (await sql(
      processes,
      "SELECT json_build_object('manifestId', encode(deployment_identity, 'hex')) FROM event_history_authority WHERE singleton",
    )) as { manifestId: string };
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
  // A fresh identity may only start over an empty local store. Never clear it.
  const query =
    "DO $$ DECLARE relation record; populated boolean; BEGIN FOR relation IN SELECT tablename FROM pg_tables WHERE schemaname='public' AND tablename <> 'schema_migrations' LOOP EXECUTE format('SELECT EXISTS(SELECT 1 FROM public.%I LIMIT 1)', relation.tablename) INTO populated; IF populated THEN RAISE EXCEPTION 'Fresh deployment requires empty local storage: %', relation.tablename; END IF; END LOOP; END $$; SELECT '{\"empty\":true}'::json";
  await sql(processes, query);
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
  const observed = (await sql(
    processes,
    `CREATE TABLE IF NOT EXISTS full_stack_controller_identity (singleton boolean PRIMARY KEY CHECK(singleton), run_id text NOT NULL, manifest_id text NOT NULL); INSERT INTO full_stack_controller_identity VALUES (true,'${marker.runId}','${marker.manifestId}') ON CONFLICT DO NOTHING; SELECT row_to_json(identity) FROM full_stack_controller_identity identity`,
  )) as { run_id: string; manifest_id: string };
  if (
    observed?.run_id !== marker.runId ||
    observed.manifest_id !== marker.manifestId
  )
    throw new Error("Storage identity differs from this deployment");
  await writeDurableJson(markerFile(processes), marker);
  await assertPreservedStorage(processes);
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
        const observed = (await sql(
          processes,
          "SELECT json_build_object('exists', to_regclass('public.full_stack_controller_identity') IS NOT NULL)",
        )) as { exists: boolean };
        if (observed.exists) {
          const journal = (await readJsonIfPresent(
            journalFile(processes),
          )) as StackJournal;
          const manifest = (await readJsonIfPresent(
            stackPaths(processes).manifest,
          )) as { manifestId: string };
          const marker = (await sql(
            processes,
            "SELECT row_to_json(identity) FROM full_stack_controller_identity identity",
          )) as { run_id: string; manifest_id: string };
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
