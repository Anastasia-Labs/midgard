import { execFile } from "node:child_process";
import { readFile, writeFile } from "node:fs/promises";
import { isAbsolute, join } from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

import { parse } from "dotenv";
import { afterEach, describe, expect, it, vi } from "vitest";

import { assertComposeVersion } from "../src/full-stack/prerequisites.js";
import { committeeIsReady } from "../src/full-stack/readiness.js";
import {
  runtimeInputDigest,
  stackNodeServices,
} from "../src/full-stack/runtime.js";
import { NodeFollowerUnconfiguredError } from "../src/l1-origin.js";
import type { RecordedCall } from "./full-stack-fixtures.js";
import {
  closeRuntimeStacks,
  committeeReady,
  manifestId,
  stack,
} from "./full-stack-runtime.fixtures.js";
import { nodeFollowerPlan } from "./node-follower-plan.js";

/** The checkout's node directory, which holds the base Compose files. */
const nodeRoot = fileURLToPath(new URL("../../midgard-node/", import.meta.url));

/**
 * The environment Docker gives one service for a recorded Compose call: the
 * files the call passes (relative ones from the checkout's node directory),
 * rendered in the stack's node directory with its env file, each env_file
 * resolved.
 */
async function composeEnvironment(
  call: RecordedCall,
  projectDirectory: string,
  service: string,
) {
  const files = call.args.flatMap((arg, index) =>
    arg === "-f" ? [call.args[index + 1]!] : [],
  );
  const envFile = call.args[call.args.indexOf("--env-file") + 1]!;
  const { stdout } = await promisify(execFile)(
    "docker",
    [
      "compose",
      "--project-directory",
      projectDirectory,
      "--env-file",
      envFile,
      ...files.flatMap((file) => [
        "-f",
        isAbsolute(file) ? file : join(nodeRoot, file),
      ]),
      "config",
      "--format",
      "json",
    ],
    { env: { PATH: process.env.PATH!, HOME: process.env.HOME! } },
  );
  return (
    JSON.parse(stdout) as {
      services: Record<string, { environment?: Record<string, string> }>;
    }
  ).services[service]!.environment!;
}

afterEach(closeRuntimeStacks);

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

describe("runtime services step", () => {
  it("starts the committee on a fresh DA database and waits for it to report ready", async () => {
    const { processes, services } = await stack();
    await expect(services.execute(undefined)).resolves.toMatchObject({
      committees: [committeeReady(0)],
    });
    expect(processes.calls.map((call) => call.id)).toContain("runtime-start");
  });
  it("stops at once on a committee whose startup failed and holds, naming its reason", async () => {
    const { processes, services, state } = await stack();
    const detail =
      "committee_startup_failed: a failure no retry is known to repair: store schema mismatch";
    state.committee = {
      status: 503,
      body: { ready: false, reasons: ["committee_startup_failed"], detail },
    };
    await expect(services.execute(undefined)).rejects.toThrow(
      `da-committee-0 cannot become ready by waiting: its startup failed and holds: ${detail}`,
    );
    expect(processes.calls.map((call) => call.id)).not.toContain(
      "runtime-start",
    );
  }, 30_000);
  it("stops at once on a committee whose submitter wallet is short, naming what is missing", async () => {
    const { processes, services, state } = await stack();
    const short = { ...committeeReady(0), ready: false };
    state.committee = {
      status: 503,
      body: {
        ...short,
        reasons: ["L1 submitter preflight failed"],
        peer: {
          ...short.peer,
          l1SubmitterPreflight: {
            status: "failed",
            detail: {
              address: "addr_test1short",
              missingPlainLovelace: "5000000",
            },
          },
        },
      },
    };
    await expect(services.execute(undefined)).rejects.toThrow(
      /da-committee-0 cannot become ready by waiting: its L1 submitter wallet check failed: .*addr_test1short.*"missingPlainLovelace":"5000000"/u,
    );
    expect(processes.calls.map((call) => call.id)).not.toContain(
      "runtime-start",
    );
  }, 30_000);
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
  it("runs the owner pin, before node.env exists, without the stack's .env, and the node with node.env", async () => {
    const { directory, processes, services, state } = await stack();
    const { config } = processes;
    // The operator's .env holds every role's secrets, as a fresh stack's does.
    await writeFile(
      config.envFile,
      Object.entries(processes.env)
        .map(([name, value]) => `${name}=${JSON.stringify(value)}\n`)
        .join(""),
    );
    /** The names of the stack's secrets whose values an environment holds. */
    const secretsIn = (environment: Record<string, string>) =>
      Object.entries(processes.env)
        .filter(
          ([name, value]) =>
            name.startsWith("STACK_") &&
            value !== "" &&
            JSON.stringify(environment).includes(value),
        )
        .map(([name]) => name);
    // The node's part of the services file the configuration step generates.
    const servicesDirectory = join(directory, "run/services");
    await writeFile(join(servicesDirectory, "producer.json"), "{}");
    await writeFile(
      join(servicesDirectory, "compose.json"),
      JSON.stringify({
        services: await stackNodeServices(processes, {
          envFile: [
            { path: join(servicesDirectory, "node.env"), required: false },
          ],
          producer: join(servicesDirectory, "producer.json"),
        }),
      }),
    );
    // Rendered when the pin runs: node.env does not exist yet.
    processes.responses["native-owner-pin"] = async (call: RecordedCall) => {
      expect(
        secretsIn(await composeEnvironment(call, directory, "midgard-node")),
      ).toEqual([]);
      return { sha: "c".repeat(64) };
    };
    let started: Record<string, string> = {};
    processes.responses["runtime-start"] = async (call: RecordedCall) => {
      started = await composeEnvironment(call, directory, "midgard-node");
      state.nodeStarted = true;
    };
    await services.execute(undefined);
    // The running node reads node.env: pinned, with other roles' secrets blanked.
    const nodeEnv = parse(await readFile(join(servicesDirectory, "node.env")));
    expect(Object.keys(started)).toEqual(
      expect.arrayContaining(Object.keys(nodeEnv)),
    );
    expect(started.MPF_NATIVE_OWNER_BINARY_SHA256).toBe("c".repeat(64));
    // Of the stack's secrets, only the node's own DA identity.
    expect(secretsIn(started)).toEqual([config.da.producerTransportEnv]);
    expect(started.DA_LIBP2P_PRIVATE_KEY_SOURCE).toBe(
      processes.env[config.da.producerTransportEnv],
    );
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
  it("confirms the running services this run built without restarting them", async () => {
    const { processes, services, state } = await stack();
    state.nodeStarted = true;
    const inputDigest = await runtimeInputDigest(processes);
    const result = await services.reconcile({
      status: "complete",
      attempts: 1,
      data: { inputDigest, buildId: processes.buildId },
    });
    expect(result).toMatchObject({ status: "complete", data: { inputDigest } });
    expect(processes.calls).toEqual([]);
  });
  it("builds services an earlier run built again, so changed source reaches their containers", async () => {
    const { processes, services, state } = await stack();
    state.nodeStarted = true;
    const earlier = await stack();
    const inputDigest = await runtimeInputDigest(processes);
    expect(
      await services.reconcile({
        status: "complete",
        attempts: 1,
        data: { inputDigest, buildId: earlier.processes.buildId },
      }),
    ).toEqual({ status: "retry" });
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
      `WATCHER_ROLLBACK_KEY_FILE=${join(directory, "rollback-key")}\nOTHER=1\n`,
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
    const data = {
      inputDigest: await runtimeInputDigest(processes),
      buildId: processes.buildId,
    };
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
      `WATCHER_ROLLBACK_KEY_FILE=${join(directory, "rollback-key")}\nOTHER=1\n`,
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
