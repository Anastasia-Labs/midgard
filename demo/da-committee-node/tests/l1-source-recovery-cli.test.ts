import { execFile } from "node:child_process";
import { promisify } from "node:util";

import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { afterAll, describe, expect, it } from "vitest";

import { l1SourceAuthorityDigest, loadCommitteeConfig } from "../src/config.js";
import { RECOVERABLE_L1_REASON } from "../src/l1/recovery-incident.js";
import { openCommitteeStore } from "../src/store/factory.js";
import { tempDir, writeJson } from "./helpers.js";
import {
  libp2pConfigEnv,
  libp2pManifest,
  writeConfigFiles,
} from "./helpers/l1-recovery-cli-config.js";
import { postgresTestDatabases } from "./helpers/postgres-database.js";
import { localSignature } from "./helpers/quarantined-committee-store.js";

const run = promisify(execFile);
const databases = postgresTestDatabases("codex_rel_cq_cli");
afterAll(() => databases.dropAll());
const cli = async (env: NodeJS.ProcessEnv, args: string[]) => {
  try {
    const result = await run(
      process.execPath,
      ["dist/recover-l1-source.js", ...args],
      { env, timeout: 15_000 },
    );
    return { ...result, code: 0 };
  } catch (error) {
    const result = error as { stdout: string; stderr: string; code: number };
    if (typeof result.code !== "number") throw error;
    return result;
  }
};
const rows = (stdout: string) =>
  stdout
    .trim()
    .split("\n")
    .filter(Boolean)
    .map((line) => JSON.parse(line));
const fixture = async (backend: "JSON" | "Postgres", signed = false) => {
  const dir = await tempDir();
  const files = await writeConfigFiles(dir, libp2pManifest("01".repeat(32)));
  const env: NodeJS.ProcessEnv = {
    PATH: process.env.PATH,
    NODE_ENV: "emulator",
    ...libp2pConfigEnv(dir, files.manifestPath, files.deploymentInfoPath),
  };
  const events = await writeJson(dir, "native-events.json", [
    { direction: "roll_forward", slot: 100, blockHash: "10".repeat(32) },
  ]);
  const query = await writeJson(dir, "native-query.json", []);
  env.CARDANO_LOCAL_NODE_CHAIN_SYNC_URL = `chain-sync:fixture:${events}`;
  env.CARDANO_PROVIDER_URLS = `fixture:${query}`;
  if (backend === "Postgres") {
    delete env.DA_COMMITTEE_DB_PATH;
    env.DA_COMMITTEE_DATABASE_URL = (await databases.create()).url;
  }
  const config = await loadCommitteeConfig(env);
  const store = await openCommitteeStore(config.localState);
  try {
    await store.initDeployment({
      marker: makeDeploymentMarker(config.deploymentFingerprint),
      manifestSha256: config.deploymentManifestSha256,
      contractDeploymentInfoSha256: config.contractDeploymentInfoSha256,
      manifestRaw: config.deploymentManifestRaw,
    });
    await store.saveL1SourceState({
      schemaVersion: 1,
      sourceMode: "local_node",
      network: config.network,
      authoritySha256: l1SourceAuthorityDigest(config.network, config.l1Source),
      status: "healthy",
      observations: [],
      observedAt: new Date(0).toISOString(),
      stateQueueReplayAnchor: {
        deploymentIdentityDigest: config.deploymentFingerprint,
        stateQueuePolicyId: config.stateQueuePolicyId,
        queue: [{ headerHash: null, outRef: `${"00".repeat(32)}#0` }],
        blockNo: "0",
        transactionIndex: "0",
      },
    });
    if (signed) await store.saveDaSignature(localSignature());
    await store.quarantineL1Decisions({
      ...(await store.getL1SourceState())!,
      status: "quarantined",
      quarantineReason: RECOVERABLE_L1_REASON,
      quarantinedAt: new Date(1).toISOString(),
    });
  } finally {
    await store.close?.();
  }
  return { config, env };
};

describe.each(["JSON", "Postgres"] as const)(
  "%s actual compiled recovery CLI",
  (backend) => {
    it("inspects the digest, refuses stale/native-unsupported attempts, and preserves the held source", async () => {
      const { env, config } = await fixture(backend);
      const inspection = await cli(env, ["inspect"]);
      expect(inspection.code).toBe(0);
      const incident = rows(inspection.stdout)[0].recovery.incident;
      expect(rows(inspection.stdout)[0]).toMatchObject({
        status: "quarantined",
        recovery: {
          phase: "not_attempted",
          support:
            "no_journal_responder_disabled_unsigned_new_missing_point_only",
        },
      });
      const stale = await cli(env, [
        "--incident",
        "00".repeat(32),
        "--timeout-ms",
        "1000",
      ]);
      expect(stale.code).toBe(78);
      expect(rows(stale.stdout).at(-1).recovery.code).toBe(
        "recovery_incident_changed",
      );
      const unavailable = await cli(env, [
        "--incident",
        incident,
        "--timeout-ms",
        "1000",
      ]);
      expect(unavailable.code).toBe(78);
      expect(
        rows(unavailable.stdout)
          .map((row) => row.recovery?.phase)
          .filter(Boolean),
      ).toEqual(["recovering", "recovery_refused"]);
      expect(rows(unavailable.stdout).at(-1).status).toBe("quarantined");
      const externalEnv: NodeJS.ProcessEnv = {
        ...env,
        CARDANO_L1_SOURCE_MODE: "external_providers",
        CARDANO_PROVIDER_URLS:
          "kupmios:http://127.0.0.1:49151|http://127.0.0.1:49152,kupmios:http://127.0.0.1:49153|http://127.0.0.1:49154",
        CARDANO_PROVIDER_AUTHORITY_IDS: `${"01".repeat(32)},${"02".repeat(32)}`,
        CARDANO_EXTERNAL_PROVIDER_IDENTITIES: "first,second",
      };
      delete externalEnv.CARDANO_LOCAL_NODE_AUTHORITY_ID;
      delete externalEnv.CARDANO_LOCAL_NODE_CHAIN_SYNC_URL;
      delete externalEnv.CARDANO_LOCAL_NODE_CHAIN_SYNC_CURSOR_PATH;
      const external = await cli(externalEnv, [
        "--incident",
        incident,
        "--timeout-ms",
        "1000",
      ]);
      expect(external.code).toBe(78);
      expect(rows(external.stdout).at(-1).recovery.code).toBe(
        "complete_local_native_source_required",
      );
      const store = await openCommitteeStore(config.localState);
      try {
        expect((await store.getL1SourceState())?.status).toBe("quarantined");
      } finally {
        await store.close?.();
      }
    }, 60_000);
    it("reports a stable signature-family refusal without exposing record bytes", async () => {
      const { env } = await fixture(backend, true);
      const inspection = rows((await cli(env, ["inspect"])).stdout)[0];
      expect(inspection.recovery.residues).toContain(
        "prior_da_signatures_requires_reconciliation",
      );
      const result = await cli(env, [
        "--incident",
        inspection.recovery.incident,
        "--timeout-ms",
        "1000",
      ]);
      expect(result.code).toBe(78);
      expect(rows(result.stdout).at(-1).recovery.code).toBe(
        "prior_da_signatures_requires_reconciliation",
      );
      expect(result.stdout).not.toContain(localSignature().signatureWitness);
    }, 30_000);
    it("cannot bypass the held daemon's existing exclusive instance lease", async () => {
      const { env, config } = await fixture(backend);
      const held = await openCommitteeStore(config.localState);
      try {
        const result = await cli(env, ["inspect"]);
        expect(result.code).toBe(78);
        expect(result.stderr).toContain("stop/join the held daemon");
        expect((await held.getL1SourceState())?.status).toBe("quarantined");
      } finally {
        await held.close?.();
      }
    }, 30_000);
  },
);
