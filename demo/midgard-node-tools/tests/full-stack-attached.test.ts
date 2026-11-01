import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { formatL1Origin } from "@al-ft/midgard-core/l1-origin";
import { parse } from "dotenv";
import { afterEach, describe, expect, it } from "vitest";

import {
  loadAttachedStackConfig,
  parseAttachedStackConfig,
} from "../src/full-stack/attached-config.js";
import { parseStackConfig } from "../src/full-stack/config.js";
import { generateDaServices } from "../src/full-stack/da-services.js";
import { StackProcesses } from "../src/full-stack/process.js";
import {
  attachedFixture,
  attachedRun,
  composeServiceHash,
  NODE_WALLETS,
  OPERATOR2_TRANSPORT,
  removeAttachedFixtures,
} from "./full-stack-attached.fixtures.js";
import { nodeFollowerPlan } from "./node-follower-plan.js";

afterEach(removeAttachedFixtures);

describe("second-instance configuration", () => {
  it("loads operator-2's node beside the running stack it attaches to", async () => {
    const fixture = await attachedFixture();
    const loaded = await loadAttachedStackConfig(fixture.path);
    expect(loaded.admittedProducer).toBe(fixture.admittedProducer);
    expect(loaded.config.attachedTo?.nodeRoot).toBe(fixture.primary.directory);
    expect(loaded.config.nodeRoot).toBe(fixture.operator2);
    // Its own node wallets only, and the stack's committee.
    expect(Object.keys(loaded.config.wallets).sort()).toEqual(
      Object.keys(NODE_WALLETS).sort(),
    );
    expect(loaded.config.da.members).toEqual(fixture.primary.config.da.members);
    expect(loaded.config.da.producerTransportEnv).toBe(OPERATOR2_TRANSPORT);
  });
  it("refuses a node the stack's committee does not admit, naming the address to admit", async () => {
    const fixture = await attachedFixture({ admitted: false });
    await expect(loadAttachedStackConfig(fixture.path)).rejects.toThrow(
      `The stack's committee does not admit this node: add ${fixture.admittedProducer} to da.admittedProducers in ${fixture.primary.path}`,
    );
  });
  it("refuses a node that would share the stack's ports or Compose project", async () => {
    const fixture = await attachedFixture();
    fixture.env.MIDGARD_POSTGRES_HOST_PORT =
      fixture.primary.env.MIDGARD_POSTGRES_HOST_PORT!;
    await fixture.writeEnv();
    await expect(loadAttachedStackConfig(fixture.path)).rejects.toThrow(
      "MIDGARD_POSTGRES_HOST_PORT is the port of the stack this instance attaches to",
    );
    fixture.env.MIDGARD_POSTGRES_HOST_PORT = "25533";
    fixture.env.COMPOSE_PROJECT_NAME = "midgard-node";
    await fixture.writeEnv();
    await expect(loadAttachedStackConfig(fixture.path)).rejects.toThrow(
      "A second instance needs its own Compose project",
    );
  });
  it.each([
    ["operator", "operator"],
    ["merge", "user"],
    ["settlement", "prover"],
  ])(
    "refuses an operator-2 %s wallet that is the stack's %s wallet",
    async (role, primaryRole) => {
      const fixture = await attachedFixture();
      const seedEnv = NODE_WALLETS[role as keyof typeof NODE_WALLETS];
      fixture.env[seedEnv] =
        fixture.primary.env[
          fixture.primary.config.wallets[primaryRole]!.seedEnv
        ]!;
      await fixture.writeEnv();
      await expect(loadAttachedStackConfig(fixture.path)).rejects.toThrow(
        `Operator-2's ${role} wallet shares a payment key with the stack's ${primaryRole} wallet`,
      );
    },
  );
  it("reads the operator-2 example as a second instance of the fast-timer twin", async () => {
    const example = async (name: string) =>
      JSON.parse(
        await readFile(new URL(`../config/${name}`, import.meta.url), "utf8"),
      ) as Record<string, unknown>;
    const twin = parseStackConfig(
      await example("preprod-testing-twin-stack.example.json"),
    );
    const config = parseAttachedStackConfig(
      await example("preprod-testing-twin-operator2.example.json"),
      twin,
    );
    expect(config.attachedTo).toBe(twin);
    expect(config.endpoint).not.toBe(twin.endpoint);
  });
  it("refuses the stack's own node directory, other wallets, or a public stack", async () => {
    const fixture = await attachedFixture();
    const primary = fixture.primary.config;
    expect(() =>
      parseAttachedStackConfig(
        {
          ...fixture.attached,
          nodeRoot: primary.nodeRoot,
          envFile: primary.envFile,
        },
        primary,
      ),
    ).toThrow("A second instance needs its own node directory");
    expect(() =>
      parseAttachedStackConfig(
        {
          ...fixture.attached,
          wallets: {
            ...fixture.attached.wallets,
            prover: { seedEnv: "STACK_PROVER_SEED", minimumLovelace: "1" },
          },
        },
        primary,
      ),
    ).toThrow("A second instance configures exactly the node wallets");
    expect(() =>
      parseAttachedStackConfig(fixture.attached, {
        ...primary,
        public: {} as NonNullable<typeof primary.public>,
      }),
    ).toThrow("A second instance attaches only to a preprod-testing stack");
  });
});

describe("second-instance workflow", () => {
  it("joins the running deployment without deploying, and starts only its node and Postgres", async () => {
    const { fixture, processes, run } = await attachedRun();
    const journal = await run();
    expect(Object.keys(journal.steps)).toEqual([
      "providers",
      "storage",
      "wallets",
      "attach",
      "operator",
      "runtime-configuration",
      "storage-identity",
      "services",
    ]);
    expect(
      Object.values(journal.steps).every(
        (record) => record.status === "complete",
      ),
    ).toBe(true);
    const ids = processes.calls.map((call) => call.id);
    for (const deploying of [
      "nonce-create-or-resume",
      "references-publish",
      "initialize-submit",
      "cardano-config-export",
      "committee-start",
    ])
      expect(ids).not.toContain(deploying);
    // Compose starts operator-2's Postgres and node, never the stack's
    // Cardano node, committee or watcher.
    const started = processes.calls
      .filter((call) => call.args.includes("up"))
      .flatMap((call) =>
        call.args
          .slice(call.args.indexOf("up") + 1)
          .filter((arg) => !arg.startsWith("-")),
      );
    expect(started).toEqual(["postgres", "midgard-node"]);
    // The deployment it joined is the stack's, byte for byte.
    expect(
      await readFile(
        join(fixture.operator2, "deploymentInfo/contract-deployment-info.json"),
      ),
    ).toEqual(await readFile(fixture.primaryManifest));
    // Operator-2 registers with its own operator wallet.
    const register = processes.calls.find(
      (call) => call.id === "operator-register-or-resume",
    )!;
    expect(
      register.env.L1_OPERATOR_SEED_PHRASE ===
        fixture.env.L1_OPERATOR_SEED_PHRASE,
    ).toBe(true);
  });
  it("runs operator-2's node on the stack's deployment, origin and Cardano node, publishing as the producer the committee admits", async () => {
    const { fixture, run, rendered } = await attachedRun();
    await run();
    const services = join(fixture.operator2, "logs/operator2/services");
    const nodeEnv = parse(await readFile(join(services, "node.env")));
    expect(
      nodeEnv.DA_LIBP2P_PRIVATE_KEY_SOURCE === fixture.env[OPERATOR2_TRANSPORT],
    ).toBe(true);
    expect(nodeEnv.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS).toBe(
      fixture.manifest.referenceScriptDeployAddress,
    );
    const plan = nodeFollowerPlan(nodeEnv);
    if (plan.kind !== "run") throw new Error(plan.detail);
    expect(nodeEnv.L1_ORIGIN).toBe(formatL1Origin(fixture.origin));
    expect(plan.origin.origin.slot).toBe(fixture.origin.slot);
    expect(plan.origin.hubOracleOneShot.index).toBe(
      fixture.manifest.hubOracleOneShot.outputIndex,
    );
    // Its producer manifests name its own peer; on the host, at exactly the
    // address the stack's committee admits.
    const producer = async (name: string) =>
      JSON.parse(await readFile(join(services, name), "utf8")) as {
        runtime_topology: { producer_peer_id: string };
        da_transport: {
          announce_multiaddrs: string[];
          bootstrap_multiaddrs: string[];
        };
      };
    const host = await producer("producer-host.json");
    const container = await producer("producer.json");
    expect(host.runtime_topology.producer_peer_id).toBe(fixture.peerId);
    expect(container.runtime_topology.producer_peer_id).toBe(fixture.peerId);
    expect(host.da_transport.announce_multiaddrs).toEqual([
      fixture.admittedProducer,
    ]);
    // It dials the stack's committee members.
    const stack = JSON.parse(
      await readFile(
        join(fixture.primary.config.runDirectory, "services/producer.json"),
        "utf8",
      ),
    ) as typeof container;
    expect(container.da_transport.bootstrap_multiaddrs).toEqual(
      stack.da_transport.bootstrap_multiaddrs,
    );
    // Docker mounts the stack's Cardano node socket and starts no Cardano node.
    const node = rendered().services["midgard-node"]!;
    expect(
      node.volumes?.find((volume) => volume.target === "/ipc")?.source,
    ).toBe(join(fixture.primary.directory, "cardano/ipc"));
    expect(Object.keys(node.depends_on ?? {}).sort()).toEqual([
      "midgard-node-migrate",
      "postgres",
    ]);
  });
  it("confirms an attached node on a later run without attaching or starting it again", async () => {
    const { processes, run } = await attachedRun();
    await run();
    processes.calls.length = 0;
    await run();
    const ids = processes.calls.map((call) => call.id);
    expect(ids).not.toContain("runtime-start");
    expect(ids).not.toContain("operator-register-or-resume");
  });
  it("recreates operator-2's node when the stack's producer manifest changes", async () => {
    const { fixture, processes, run } = await attachedRun();
    await run();
    const hash = () =>
      composeServiceHash(
        processes.calls.find((call) => call.id === "runtime-start")!,
        fixture.operator2,
        "midgard-node",
      );
    const before = await hash();
    // The stack moves its committee's transport ports and regenerates its
    // producer manifests; operator-2's node environment stays the same.
    fixture.primary.config.da.ports.committeeTransportBase += 100;
    await generateDaServices(
      new StackProcesses(fixture.primary.config, {
        ...fixture.primary.env,
        L1_ORIGIN: formatL1Origin(fixture.origin),
      }),
    );
    processes.calls.length = 0;
    await run();
    expect(processes.calls.map((call) => call.id)).toContain("runtime-start");
    expect(await hash()).not.toBe(before);
  });
  it("resumes a run stopped after attaching, before its node first ran", async () => {
    const { processes, run } = await attachedRun();
    const register = processes.responses["operator-register-or-resume"];
    processes.responses["operator-register-or-resume"] = () => {
      throw new Error("stopped");
    };
    await expect(run()).rejects.toThrow("stopped");
    // Its store is still the fresh one the attachment started from.
    processes.responses["operator-register-or-resume"] = register;
    const journal = await run();
    expect(journal.steps.services?.status).toBe("complete");
  });
  it("refuses to attach before the stack initialized its deployment", async () => {
    const { fixture, run } = await attachedRun();
    await writeFile(fixture.primaryManifest, "{}");
    await expect(run()).rejects.toThrow(
      "The stack this instance attaches to has not initialized its deployment yet",
    );
  });
});
