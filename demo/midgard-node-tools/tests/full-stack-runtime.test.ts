import { createHash } from "node:crypto";
import { mkdir, readFile, writeFile } from "node:fs/promises";
import { createServer, type Server } from "node:http";
import type { AddressInfo } from "node:net";
import { join } from "node:path";

import { parse } from "dotenv";
import { afterEach, describe, expect, it, vi } from "vitest";

import { assertComposeVersion } from "../src/full-stack/prerequisites.js";
import { committeeIsReady } from "../src/full-stack/readiness.js";
import { runtimeInputDigest, runtimeSteps } from "../src/full-stack/runtime.js";
import { NodeFollowerUnconfiguredError } from "../src/l1-origin.js";
import {
  RecordingProcesses,
  removeStackFixtures,
  stackEnvironment,
  stackFixture,
} from "./full-stack-fixtures.js";
import { nodeFollowerPlan } from "./node-follower-plan.js";

const manifestId = "a".repeat(64);
const recordKey = "0f".repeat(32);
const committeeReady = (signerIndex: number) => ({
  ready: true,
  deployment: {
    configuredFingerprint: manifestId,
    storeMatchesConfigured: true,
  },
  peer: { signerIndex, localPeerId: `peer-${signerIndex}` },
});

describe("committee readiness", () => {
  const expected = { manifestId, peerIds: ["peer-0", "peer-1"] };
  it("accepts this deployment's committee member at its signer index", () =>
    expect(committeeIsReady(committeeReady(1), 1, expected)).toBe(true));
  it("refuses another service that answers on the committee port", () => {
    const foreign = committeeReady(1);
    for (const value of [
      { ready: true },
      { ready: true, reasons: [], settlement: { state: "waiting" } },
      {
        ...foreign,
        deployment: {
          ...foreign.deployment,
          configuredFingerprint: "b".repeat(64),
        },
      },
      {
        ...foreign,
        deployment: { ...foreign.deployment, storeMatchesConfigured: false },
      },
      { ...foreign, peer: { signerIndex: 0, localPeerId: "peer-1" } },
      { ...foreign, peer: { signerIndex: 1, localPeerId: "peer-9" } },
    ])
      expect(committeeIsReady(value, 1, expected)).toBe(false);
  });
});

const listen = async (server: Server) => {
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  return (server.address() as AddressInfo).port;
};
const servers: Server[] = [];
afterEach(async () => {
  await Promise.all(
    servers
      .splice(0)
      .map((server) => new Promise((resolve) => server.close(resolve))),
  );
  await removeStackFixtures();
});

const DEPLOYED_FOLLOWER_INPUTS = {
  HUB_ORACLE_ONE_SHOT_TX_HASH: "ab".repeat(32),
  HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: "0",
  L1_ORIGIN: `7.${"cd".repeat(32)}`,
};

/** A generated run with one committee member and live HTTP services on loopback. */
async function stack() {
  const state = { nodeStarted: false };
  const committee = createServer((_, response) =>
    response.end(JSON.stringify(committeeReady(0))),
  );
  const services = createServer((request, response) => {
    const body: Record<string, unknown> = {
      "/readyz": state.nodeStarted
        ? { ready: true, reasons: [], settlement: { state: "waiting" } }
        : undefined,
      "/v1/status": {
        liveness: "live",
        readiness: "ready",
        readinessReasons: [],
        deploymentFingerprint: manifestId,
        launchScope: { complete: true },
      },
      "/v1/identity": {
        recordAuthenticationKeyId: createHash("sha256")
          .update(Buffer.from(recordKey, "hex"))
          .digest("hex"),
      },
    };
    const value = body[request.url ?? ""];
    response.statusCode = value === undefined ? 503 : 200;
    response.end(JSON.stringify(value ?? { ready: false }));
  });
  servers.push(committee, services);
  const committeePort = await listen(committee);
  const servicesUrl = `http://127.0.0.1:${await listen(services)}`;
  const { directory, config } = await stackFixture((sample) => {
    Object.assign(sample, { endpoint: servicesUrl, timeoutMs: 1 });
    sample.da.members = sample.da.members.slice(0, 1);
  });
  config.da.ports.committeeApiBase = committeePort;
  const env = stackEnvironment(config);
  const processes = new RecordingProcesses(config, env);
  // What the deployment steps restore before the services step runs.
  Object.assign(processes.env, DEPLOYED_FOLLOWER_INPUTS);
  processes.responses["runtime-start"] = () => {
    state.nodeStarted = true;
  };
  processes.responses["native-owner-pin"] = { sha: "c".repeat(64) };
  await mkdir(join(directory, "run/services"), { recursive: true });
  await mkdir(join(directory, "deploymentInfo"));
  await writeFile(
    join(directory, "deploymentInfo/contract-deployment-info.json"),
    JSON.stringify({ manifestId }),
  );
  await writeFile(
    join(directory, "run/services/committee-0.json"),
    JSON.stringify({
      deployment: { fingerprint: manifestId },
      da_committee: { members: [{ signer_index: 0, peer_id: "peer-0" }] },
    }),
  );
  await writeFile(join(directory, "bearer"), "bearer-token\n");
  await writeFile(join(directory, "record-key"), `${recordKey}\n`);
  await writeFile(
    join(directory, "watcher.env"),
    `WATCHER_RECORD_KEY_FILE=${join(directory, "record-key")}\n`,
  );
  await writeFile(
    join(directory, "run/runtime.json"),
    JSON.stringify({
      inputDigest: await runtimeInputDigest(processes),
      compose: join(directory, "run/services/compose.json"),
      operationsEndpoint: servicesUrl,
      authorityEndpoint: servicesUrl,
      committeeServices: ["da-committee-0"],
    }),
  );
  const [configuration, , services_] = runtimeSteps(processes);
  return {
    directory,
    processes,
    state,
    configuration: configuration!,
    services: services_!,
  };
}

describe("runtime services step", () => {
  it("dials the producer's live listener whenever its container is up, even before it is ready", async () => {
    const { processes, services } = await stack();
    processes.responses["producer-container-state"] = {
      Service: "midgard-node",
      State: "restarting",
    };
    await services.execute(undefined);
    const state = processes.calls.find(
      (call) => call.id === "producer-container-state",
    )!;
    // A restarting container still holds the port, so it counts as up.
    expect(state.args.join(" ")).toContain(
      "--status running --status restarting",
    );
    const preflight = processes.calls.find(
      (call) => call.id === "producer-da-preflight",
    )!;
    expect(preflight.args).toContain("dial-only");
    expect(preflight.args).not.toContain("bind-listen");
  });
  it("starts the public reader only after granting it exactly its two tables in the migrated database", async () => {
    const { processes, services } = await stack();
    await services.execute(undefined);
    const ids = processes.calls.map((call) => call.id);
    const committee = ids.indexOf("committee-start");
    const grant = ids.indexOf("public-reader-grant-0");
    const reader = ids.indexOf("public-reader-start");
    expect(committee).toBeGreaterThan(-1);
    expect(grant).toBeGreaterThan(committee);
    expect(reader).toBeGreaterThan(grant);
    expect(processes.calls[committee]!.args).not.toContain(
      "public-retained-da",
    );
    const sql = processes.calls[grant]!.args.at(-1)!;
    expect(processes.calls[grant]!.args).toContain("midgard_da_0");
    expect(sql).toContain(
      "REVOKE ALL ON ALL TABLES IN SCHEMA public FROM midgard_da_reader;",
    );
    expect(sql).toContain("REVOKE ALL ON TABLES FROM midgard_da_reader;");
    expect(
      sql.match(/GRANT SELECT ON ([^;]+) TO midgard_da_reader;/u)?.[1],
    ).toBe("committee_da_payloads, committee_state_queue_headers");
  });
  it("binds the producer port only before its container first starts", async () => {
    const { processes, services } = await stack();
    await services.execute(undefined);
    const preflight = processes.calls.find(
      (call) => call.id === "producer-da-preflight",
    )!;
    expect(preflight.args).toContain("bind-listen");
  });
  it("writes node.env once, already pinned to the built owner and without other roles' secrets", async () => {
    const { directory, processes, services } = await stack();
    await services.execute(undefined);
    const nodeEnv = parse(
      await readFile(join(directory, "run/services/node.env")),
    );
    expect(nodeEnv.MPF_NATIVE_OWNER_BINARY_SHA256).toBe("c".repeat(64));
    expect(nodeEnv.DA_LIBP2P_PRIVATE_KEY_SOURCE).toBe(
      processes.env.STACK_DA_PRODUCER_TRANSPORT,
    );
    expect(nodeEnv.STACK_USER_SEED).toBe("");
    expect(nodeEnv.STACK_DA_SIGNER_0_SEED).toBe("");
    expect(nodeEnv.L1_OPERATOR_SEED_PHRASE).toBe(
      processes.env.L1_OPERATOR_SEED_PHRASE,
    );
    const pin = processes.calls.findIndex(
      (call) => call.id === "native-owner-pin",
    );
    const database = processes.calls.findIndex(
      (call) => call.id === "da-database-start",
    );
    expect(pin).toBeGreaterThan(-1);
    expect(database).toBeGreaterThan(pin);
  });
  it("writes a node.env whose L1 follower runs from the deployment's origin", async () => {
    const { directory, services } = await stack();
    await services.execute(undefined);
    const nodeEnv = parse(
      await readFile(join(directory, "run/services/node.env")),
    );
    const plan = nodeFollowerPlan(nodeEnv);
    expect(plan).toMatchObject({
      kind: "run",
      socketPath: "/ipc/node.socket",
      origin: { hubOracleOneShot: { index: 0 } },
    });
    if (plan.kind !== "run") throw new Error(plan.detail);
    expect(plan.origin.origin.slot).toBe(7);
  });
  it("refuses to write node.env without the deployment's origin", async () => {
    const { directory, processes, services } = await stack();
    delete processes.env.L1_ORIGIN;
    await expect(services.execute(undefined)).rejects.toThrow(
      NodeFollowerUnconfiguredError,
    );
    await expect(
      readFile(join(directory, "run/services/node.env")),
    ).rejects.toThrow(/ENOENT/);
    expect(processes.calls.map((call) => call.id)).not.toContain(
      "runtime-start",
    );
  });
  it("confirms a completed run without rebuilding or restarting it", async () => {
    const { processes, services, state } = await stack();
    state.nodeStarted = true;
    const inputDigest = await runtimeInputDigest(processes);
    const result = await services.reconcile({
      status: "complete",
      attempts: 1,
      data: { inputDigest },
    });
    expect(result).toMatchObject({ status: "complete", data: { inputDigest } });
    expect(processes.calls).toEqual([]);
  });
  it("starts services again after their generated configuration changed", async () => {
    const { directory, processes, services, configuration } = await stack();
    const started = await services.execute(undefined);
    expect(
      await services.reconcile({
        status: "complete",
        attempts: 1,
        data: started,
      }),
    ).toMatchObject({ status: "complete" });
    await writeFile(
      join(directory, "watcher.env"),
      `WATCHER_RECORD_KEY_FILE=${join(directory, "record-key")}\nOTHER=1\n`,
    );
    const complete = { status: "complete" as const, attempts: 1 };
    expect(
      await configuration.reconcile({ ...complete, data: { compose: "c" } }),
    ).toEqual({ status: "retry" });
    // Regeneration saves the new digest; the running services still carry the old one.
    const runtime = JSON.parse(
      await readFile(join(directory, "run/runtime.json"), "utf8"),
    );
    await writeFile(
      join(directory, "run/runtime.json"),
      JSON.stringify({
        ...runtime,
        inputDigest: await runtimeInputDigest(processes),
      }),
    );
    processes.calls.length = 0;
    expect(await services.reconcile({ ...complete, data: started })).toEqual({
      status: "retry",
    });
    expect(processes.calls).toEqual([]);
  });
  it("restarts an unready completed run from one snapshot, without waiting out the timeout", async () => {
    const { processes, services } = await stack();
    const data = { inputDigest: await runtimeInputDigest(processes) };
    processes.config.timeoutMs = 600_000;
    vi.useFakeTimers({ toFake: ["setTimeout", "Date"] });
    try {
      // Waiting would need the fake clock to advance, so it would never settle.
      await expect(
        services.reconcile({ status: "complete", attempts: 1, data }),
      ).resolves.toEqual({ status: "retry" });
    } finally {
      vi.useRealTimers();
    }
  }, 30_000);
  it("regenerates a completed configuration whose generation inputs changed", async () => {
    const { directory, processes, configuration } = await stack();
    const complete = {
      status: "complete" as const,
      attempts: 1,
      data: { compose: "compose.json" },
    };
    processes.config.da.ports.database += 1;
    expect(await configuration.reconcile(complete)).toEqual({
      status: "retry",
    });
    processes.config.da.ports.database -= 1;
    await writeFile(
      join(directory, "watcher.env"),
      `WATCHER_RECORD_KEY_FILE=${join(directory, "record-key")}\nOTHER=1\n`,
    );
    expect(await configuration.reconcile(complete)).toEqual({
      status: "retry",
    });
  });
  it("counts the release inputs among the generation inputs, so a change is checked again", async () => {
    const { directory, processes } = await stack();
    const releaseInput = join(directory, "release-input.json");
    processes.config.watcher.releaseInput = releaseInput;
    await writeFile(releaseInput, '{"fundingProfiles":[]}');
    const before = await runtimeInputDigest(processes);
    await writeFile(releaseInput, '{"fundingProfiles":[{}]}');
    expect(await runtimeInputDigest(processes)).not.toBe(before);
  });
  it("keeps a completed configuration and restores the host producer manifest", async () => {
    const { directory, processes, configuration } = await stack();
    const result = await configuration.reconcile({
      status: "complete",
      attempts: 1,
      data: { compose: "compose.json" },
    });
    expect(result.status).toBe("complete");
    expect(processes.env.MIDGARD_DEPLOYMENT_MANIFEST_PATH).toBe(
      join(directory, "run/services/producer.json"),
    );
  });
});

describe("Compose version", () => {
  it.each(["v2.21.0", "2.29.1", "v3.0.0"])("accepts %s", (version) =>
    expect(() => assertComposeVersion({ version })).not.toThrow(),
  );
  it.each(["v2.20.3", "v1.29.2", undefined])("refuses %s", (version) =>
    expect(() => assertComposeVersion({ version })).toThrow(
      "Docker Compose 2.21 or newer is required",
    ),
  );
});
