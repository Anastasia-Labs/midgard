/**
 * The committee node's DA libp2p runtime (ruling P31): the plan, the fresh
 * keys, the generator process, and, on an emulator deployment's finalized
 * manifest, the committee node's own configuration loader and peer check over
 * a runtime the real generator produced, in both polarities.
 */
import { writeFileSync } from "node:fs";
import {
  chmod,
  mkdir,
  mkdtemp,
  readFile,
  rm,
  stat,
  unlink,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import {
  generateDaLibp2pRuntimeManifest,
  writeDaLibp2pRuntimeManifest,
} from "midgard-node/da/libp2p-runtime-manifest";
import { publishWorkflowDeployment } from "midgard-node/tests/helpers/published-workflow-deployment";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  buildDaBondPoolCommitteeEnv,
  DA_BOND_POOL_INHERITED_ENV,
} from "./da-bond-pool-committee-process.js";
import {
  DA_BOND_POOL_COMMITTEE_RUNTIME_MANIFEST,
  daBondPoolCommitteeRuntimeArgv,
  DaBondPoolCommitteeRuntimeError,
  daBondPoolCommitteeRuntimeOptions,
  type DaBondPoolCommitteeRuntimePlan,
  daBondPoolCommitteeSettings,
  type DaBondPoolRuntimeProcessRunner,
  planDaBondPoolCommitteeRuntime,
  produceDaBondPoolCommitteeRuntime,
  readWorktreePortOffset,
  reuseDaBondPoolCommitteeRuntime,
  spawnDaBondPoolRuntimeProcess,
  verifyDaBondPoolCommitteeRuntime,
  writeFreshDaLibp2pKey,
} from "./da-bond-pool-committee-runtime.js";

const REPOSITORY_ROOT = fileURLToPath(new URL("../../../../", import.meta.url));
const CLI_BIN = join(REPOSITORY_ROOT, "demo/midgard-node/dist/index.js");

const vkey = (byte: number) => byte.toString(16).padStart(2, "0").repeat(32);
const deploymentOf = (members: number, threshold = members) => ({
  network: "Custom",
  da: {
    committeeVkeys: Array.from({ length: members }, (_, index) =>
      vkey(index + 1),
    ),
    threshold,
  },
});

const directories: string[] = [];
const freshDirectory = async (label: string) => {
  const directory = await mkdtemp(join(tmpdir(), `da-bond-pool-${label}-`));
  directories.push(directory);
  return directory;
};
afterAll(async () => {
  await Promise.all(
    directories.map((directory) =>
      rm(directory, { recursive: true, force: true }),
    ),
  );
});

describe("the committee runtime plan (P31)", () => {
  it("plans one fresh key per member, the producer and public retained-DA keys, and this checkout's ports", () => {
    const plan = planDaBondPoolCommitteeRuntime({
      runDirectory: "/run",
      deployment: deploymentOf(3, 2),
      portOffset: 250,
    });
    expect(plan.ports).toEqual({
      committee: 39_251,
      producer: 39_252,
      publicRetainedDa: 39_253,
    });
    expect(plan.threshold).toBe(2);
    expect(plan.network).toBe("Custom");
    expect(plan.contractDeploymentInfoPath).toBe(
      "/run/deploymentInfo/manifest.json",
    );
    expect(plan.outPath).toBe(
      `/run/${DA_BOND_POOL_COMMITTEE_RUNTIME_MANIFEST}`,
    );
    expect(plan.keyPaths).toEqual([
      "/run/secrets/da-bond-pool-libp2p-committee-0.key",
      "/run/secrets/da-bond-pool-libp2p-committee-1.key",
      "/run/secrets/da-bond-pool-libp2p-committee-2.key",
      "/run/secrets/da-bond-pool-libp2p-producer.key",
      "/run/secrets/da-bond-pool-libp2p-public-retained-da.key",
    ]);
    // The observer (member 0) listens on the committee port; the others get
    // their own ports after the public retained-DA port.
    expect(plan.members.map((member) => member.endpoint)).toEqual([
      undefined,
      { port: 39_254 },
      { port: 39_255 },
    ]);
    expect(plan.members.map((member) => member.daVkey)).toEqual([
      vkey(1),
      vkey(2),
      vkey(3),
    ]);
    expect(plan.observer).toEqual({
      signerIndex: 0,
      libp2pKeySource: "file:/run/secrets/da-bond-pool-libp2p-committee-0.key",
    });
  });

  it("puts a chosen observer on the committee port", () => {
    const plan = planDaBondPoolCommitteeRuntime({
      runDirectory: "/run",
      deployment: deploymentOf(3),
      portOffset: 0,
      observerSignerIndex: 1,
    });
    expect(plan.members.map((member) => member.endpoint)).toEqual([
      { port: 39_004 },
      undefined,
      { port: 39_005 },
    ]);
    expect(daBondPoolCommitteeRuntimeOptions(plan).localSignerIndex).toBe(1);
  });

  it("renders key sources, never key bytes, and every member once", () => {
    const plan = planDaBondPoolCommitteeRuntime({
      runDirectory: "/run",
      deployment: deploymentOf(2, 1),
      portOffset: 10,
    });
    expect(daBondPoolCommitteeRuntimeArgv(plan)).toEqual([
      "da-libp2p-generate-manifest",
      "--target",
      "committee",
      "--profile",
      "host",
      "--contract-deployment-info",
      "/run/deploymentInfo/manifest.json",
      "--network",
      "Custom",
      "--threshold",
      "1",
      "--producer-libp2p-key-source",
      "file:/run/secrets/da-bond-pool-libp2p-producer.key",
      "--public-retained-da-libp2p-key-source",
      "file:/run/secrets/da-bond-pool-libp2p-public-retained-da.key",
      "--committee-member",
      `0,${vkey(1)},file:/run/secrets/da-bond-pool-libp2p-committee-0.key,committee+coordinator+retrieval`,
      "--committee-member",
      `1,${vkey(2)},file:/run/secrets/da-bond-pool-libp2p-committee-1.key,committee+coordinator+retrieval,39014`,
      "--local-signer-index",
      "0",
      "--producer-port",
      "39012",
      "--committee-port",
      "39011",
      "--public-retained-da-port",
      "39013",
      "--out",
      `/run/${DA_BOND_POOL_COMMITTEE_RUNTIME_MANIFEST}`,
    ]);
  });

  it("fits eight members in a checkout's block of ten ports", () => {
    const plan = planDaBondPoolCommitteeRuntime({
      runDirectory: "/run",
      deployment: deploymentOf(8),
      portOffset: 0,
    });
    expect(plan.members.at(-1)!.endpoint).toEqual({ port: 39_010 });
  });

  it.each([
    [
      "a relative run directory",
      { runDirectory: "run" },
      /run directory run is not absolute/u,
    ],
    [
      "a port offset that is not a multiple of ten",
      { portOffset: 15 },
      /port offset 15 is not a natural multiple of 10/u,
    ],
    [
      "a committee-less deployment",
      { deployment: deploymentOf(0, 1) },
      /names no DA committee member/u,
    ],
    [
      "a threshold above the committee",
      { deployment: deploymentOf(2, 3) },
      /threshold 3 does not fit its 2 member/u,
    ],
    [
      "an observer outside the committee",
      { observerSignerIndex: 2 },
      /observer signer index 2 names no member/u,
    ],
    [
      "more members than the port block holds",
      { deployment: deploymentOf(9) },
      /9 committee members do not fit this checkout's libp2p ports 39001-39010/u,
    ],
  ] as const)("refuses %s", (_label, override, message) => {
    const plan = () =>
      planDaBondPoolCommitteeRuntime({
        runDirectory: "/run",
        deployment: deploymentOf(2),
        portOffset: 0,
        ...override,
      });
    expect(plan).toThrow(DaBondPoolCommitteeRuntimeError);
    expect(plan).toThrow(message);
  });

  it("reads this checkout's port offset as the devnet generator does", () => {
    const offset = readWorktreePortOffset(REPOSITORY_ROOT);
    expect(Number.isSafeInteger(offset)).toBe(true);
    expect(offset % 10).toBe(0);
  });
});

describe("fresh libp2p keys (P31(1))", () => {
  it("writes a 0600 key the node's identity loader reads, fresh each time", async () => {
    const directory = await freshDirectory("keys");
    const a = await writeFreshDaLibp2pKey(join(directory, "a.key"));
    const b = await writeFreshDaLibp2pKey(join(directory, "b.key"));
    expect((await stat(join(directory, "a.key"))).mode & 0o777).toBe(0o600);
    expect(a.source).toBe(`file:${join(directory, "a.key")}`);
    expect((await loadDaLibp2pIdentity(a.source)).peerId).toBe(a.peerId);
    expect(a.peerId).not.toBe(b.peerId);
  });

  it("refuses to overwrite a key and leaves it unchanged", async () => {
    const directory = await freshDirectory("keys");
    const path = join(directory, "a.key");
    await writeFreshDaLibp2pKey(path);
    const before = await readFile(path, "utf8");
    await expect(writeFreshDaLibp2pKey(path)).rejects.toThrow(
      /already exists; the adapter never overwrites one/u,
    );
    expect(await readFile(path, "utf8")).toBe(before);
  });

  it("refuses a relative key path", async () => {
    await expect(writeFreshDaLibp2pKey("a.key")).rejects.toThrow(
      /is not absolute/u,
    );
  });
});

describe("the generator process (P31(2))", () => {
  const planIn = async (): Promise<DaBondPoolCommitteeRuntimePlan> => {
    const runDirectory = await freshDirectory("process");
    await mkdir(join(runDirectory, "secrets"));
    await mkdir(join(runDirectory, "deploymentInfo"));
    return planDaBondPoolCommitteeRuntime({
      runDirectory,
      deployment: deploymentOf(2),
      portOffset: 0,
    });
  };
  const exits =
    (
      exitCode: number,
      write?: (plan: DaBondPoolCommitteeRuntimePlan) => string,
    ) =>
    (plan: DaBondPoolCommitteeRuntimePlan): DaBondPoolRuntimeProcessRunner =>
    () => {
      if (write !== undefined) writeFileSync(plan.outPath, write(plan));
      return { exitCode, signal: null, stdout: "", stderr: "boom\n" };
    };
  const produce = (
    plan: DaBondPoolCommitteeRuntimePlan,
    run: DaBondPoolRuntimeProcessRunner,
  ) =>
    produceDaBondPoolCommitteeRuntime({
      plan,
      command: ["node", "cli.js"],
      env: {},
      cwd: "/",
      run,
    });

  it("refuses a non-zero exit", async () => {
    const plan = await planIn();
    await expect(produce(plan, exits(1)(plan))).rejects.toThrow(
      /da-libp2p-generate-manifest exited 1: boom/u,
    );
  });

  it("refuses an exit 0 that wrote no manifest", async () => {
    const plan = await planIn();
    await expect(produce(plan, exits(0)(plan))).rejects.toThrow(
      /exited 0 but wrote no/u,
    );
  });

  it("refuses a manifest whose local member is not the observer", async () => {
    const plan = await planIn();
    const run = exits(0, () =>
      JSON.stringify({
        runtime_topology: { target: "committee", local_signer_index: 1 },
      }),
    )(plan);
    await expect(produce(plan, run)).rejects.toThrow(
      /is not the committee target with local signer index 0/u,
    );
  });

  it("refuses an existing manifest before writing any key", async () => {
    const plan = await planIn();
    await writeFile(plan.outPath, "{}");
    await expect(produce(plan, exits(0)(plan))).rejects.toThrow(
      /already exists; the adapter never overwrites one/u,
    );
    for (const path of plan.keyPaths)
      await expect(stat(path)).rejects.toThrow(/ENOENT/u);
  });
});

describe("reusing an earlier run's runtime (resumed journey)", () => {
  /** A plan, its keys and manifest as a finished run left them, and runtime.json. */
  const earlierRun = async () => {
    const runDirectory = await freshDirectory("reuse");
    await mkdir(join(runDirectory, "secrets"));
    await mkdir(join(runDirectory, "deploymentInfo"));
    const plan = planDaBondPoolCommitteeRuntime({
      runDirectory,
      deployment: deploymentOf(2),
      portOffset: 0,
    });
    const evidence = await produceDaBondPoolCommitteeRuntime({
      plan,
      command: ["node", "cli.js"],
      env: {},
      cwd: "/",
      run: () => {
        writeFileSync(
          plan.outPath,
          JSON.stringify({
            runtime_topology: { target: "committee", local_signer_index: 0 },
          }),
        );
        return { exitCode: 0, signal: null, stdout: "", stderr: "" };
      },
    });
    const recordedEvidencePath = join(runDirectory, "runtime.json");
    await writeFile(recordedEvidencePath, JSON.stringify(evidence));
    return { plan, evidence, recordedEvidencePath };
  };

  it("returns the recorded runtime without running the generator", async () => {
    const { plan, evidence, recordedEvidencePath } = await earlierRun();
    expect(
      reuseDaBondPoolCommitteeRuntime({ plan, recordedEvidencePath }),
    ).toEqual(evidence);
    // The default path still refuses the existing manifest.
    await expect(
      produceDaBondPoolCommitteeRuntime({
        plan,
        command: ["node", "cli.js"],
        env: {},
        cwd: "/",
        run: () => {
          throw new Error("the generator must not run");
        },
      }),
    ).rejects.toThrow(/already exists; the adapter never overwrites one/u);
  });

  it("refuses a changed manifest, a missing key and a key others can read", async () => {
    const reuse = (run: Awaited<ReturnType<typeof earlierRun>>) => () =>
      reuseDaBondPoolCommitteeRuntime(run);

    const changed = await earlierRun();
    const bytes = await readFile(changed.plan.outPath);
    bytes[0] = bytes[0]! ^ 1;
    await writeFile(changed.plan.outPath, bytes);
    expect(reuse(changed)).toThrow(DaBondPoolCommitteeRuntimeError);
    expect(reuse(changed)).toThrow(/hashes to [0-9a-f]{64}, not the recorded/u);

    const missing = await earlierRun();
    await unlink(missing.plan.keyPaths[1]!);
    expect(reuse(missing)).toThrow(/the recorded libp2p key .* is missing/u);

    const readable = await earlierRun();
    await chmod(readable.plan.keyPaths[0]!, 0o644);
    expect(reuse(readable)).toThrow(/is readable by others \(mode 644\)/u);
  });

  it("refuses evidence of another plan and a run that recorded none", async () => {
    const run = await earlierRun();
    const other = planDaBondPoolCommitteeRuntime({
      runDirectory: await freshDirectory("reuse-other"),
      deployment: deploymentOf(2),
      portOffset: 0,
    });
    expect(() =>
      reuseDaBondPoolCommitteeRuntime({
        plan: other,
        recordedEvidencePath: run.recordedEvidencePath,
      }),
    ).toThrow(/not this plan's/u);
    await unlink(run.recordedEvidencePath);
    expect(() => reuseDaBondPoolCommitteeRuntime(run)).toThrow(
      /a resumed journey needs the earlier run's/u,
    );
  });
});

describe("the committee node accepts the generated runtime (P31(6))", () => {
  let deployment: Awaited<ReturnType<typeof publishWorkflowDeployment>>;
  let runDirectory: string;
  let plan: DaBondPoolCommitteeRuntimePlan;
  let observerPeerId: string;

  beforeAll(async () => {
    deployment = await publishWorkflowDeployment();
    runDirectory = await freshDirectory("accepted");
    await mkdir(join(runDirectory, "secrets"));
    await mkdir(join(runDirectory, "deploymentInfo"));
    await mkdir(join(runDirectory, "committee"));
    await writeFile(
      join(runDirectory, "deploymentInfo/manifest.json"),
      JSON.stringify(deployment.manifest),
    );
    plan = planDaBondPoolCommitteeRuntime({
      runDirectory,
      deployment: deployment.manifest,
      portOffset: readWorktreePortOffset(REPOSITORY_ROOT),
    });
    // The real generator, in process, over fresh keys.
    const keys = [];
    for (const path of plan.keyPaths)
      keys.push(await writeFreshDaLibp2pKey(path));
    observerPeerId = keys[plan.observer.signerIndex]!.peerId;
    await writeDaLibp2pRuntimeManifest(
      plan.outPath,
      await generateDaLibp2pRuntimeManifest(
        daBondPoolCommitteeRuntimeOptions(plan),
      ),
    );
  }, 600_000);

  /** The environment the live adapter builds, over `runtimeManifestPath`. */
  const committeeEnv = async (
    runtimeManifestPath: string,
    libp2pKeySource: string,
  ) => {
    const secret = async (name: string) => {
      const path = join(runDirectory, "committee", name);
      await writeFile(path, "unused\n", { mode: 0o600 });
      return `file:${path}`;
    };
    return buildDaBondPoolCommitteeEnv({
      settings: daBondPoolCommitteeSettings({
        runtimeManifestPath,
        deploymentManifestPath: join(
          runDirectory,
          "deploymentInfo/manifest.json",
        ),
        network: deployment.manifest.network,
        kupoUrl: "http://127.0.0.1:1442",
        ogmiosUrl: "ws://127.0.0.1:1337",
        chainSyncCursorPath: join(
          runDirectory,
          "committee",
          "chain-sync-cursor.json",
        ),
        finalityDepth: deployment.manifest.l1Finality.confirmationDepth,
      }),
      l1Submitter: { source: await secret("l1.seed"), keyHash: "aa" },
      availabilitySubmitter: {
        source: await secret("availability.seed"),
        keyHash: "bb",
      },
      operationalKeyHashes: { operator: "cc" },
      libp2pKeySource,
      journalPath: join(runDirectory, "committee", "journal.sqlite"),
      databaseUrl: "postgres://u:p@127.0.0.1:1/unused",
      apiHost: "127.0.0.1",
      apiPort: 1,
      pollIntervalMs: 2_000,
      inherited: process.env,
    }).env;
  };

  it("loads the observer's configuration without a DA signer and admits its peer", async () => {
    const env = await committeeEnv(plan.outPath, plan.observer.libp2pKeySource);
    expect(env.DA_SIGNER_INDEX).toBeUndefined();
    const written = JSON.parse(await readFile(plan.outPath, "utf8")) as {
      network: string;
      runtime_topology: { target: string; local_signer_index: number };
      da_committee: { members: { da_vkey: string }[] };
    };
    expect(written.network).toBe(deployment.manifest.network);
    expect(written.runtime_topology).toMatchObject({
      target: "committee",
      local_signer_index: plan.observer.signerIndex,
    });
    expect(
      written.da_committee.members.map((member) => member.da_vkey),
    ).toEqual(deployment.manifest.da.committeeVkeys);
    expect(await verifyDaBondPoolCommitteeRuntime(env)).toEqual({
      peerId: observerPeerId,
    });
  });

  it("refuses a libp2p key that is in no committee entry", async () => {
    const foreign = await writeFreshDaLibp2pKey(
      join(runDirectory, "secrets", "foreign.key"),
    );
    const env = await committeeEnv(plan.outPath, foreign.source);
    await expect(verifyDaBondPoolCommitteeRuntime(env)).rejects.toThrow(
      `unknown DA libp2p peer ${foreign.peerId}`,
    );
  });

  it("refuses a runtime manifest whose network differs from the deployment's", async () => {
    const other =
      deployment.manifest.network === "Preprod" ? "Preview" : "Preprod";
    const mismatchedPath = join(
      runDirectory,
      "deploymentInfo",
      "mismatched.json",
    );
    await writeDaLibp2pRuntimeManifest(
      mismatchedPath,
      await generateDaLibp2pRuntimeManifest({
        ...daBondPoolCommitteeRuntimeOptions(plan),
        network: other,
      }),
    );
    const env = await committeeEnv(
      mismatchedPath,
      plan.observer.libp2pKeySource,
    );
    await expect(verifyDaBondPoolCommitteeRuntime(env)).rejects.toThrow(
      `DA runtime manifest network must exactly match contract deployment manifest network: runtime=${other}, contract=${deployment.manifest.network}`,
    );
  });

  it("the built midgard-node CLI writes the same runtime manifest from the adapter's arguments", async () => {
    // The live adapter's exact path: fresh keys, then the real process.
    const cliRun = await freshDirectory("cli");
    await mkdir(join(cliRun, "secrets"));
    await mkdir(join(cliRun, "deploymentInfo"));
    await writeFile(
      join(cliRun, "deploymentInfo/manifest.json"),
      JSON.stringify(deployment.manifest),
    );
    const cliPlan = planDaBondPoolCommitteeRuntime({
      runDirectory: cliRun,
      deployment: deployment.manifest,
      portOffset: readWorktreePortOffset(REPOSITORY_ROOT),
    });
    const evidence = await produceDaBondPoolCommitteeRuntime({
      plan: cliPlan,
      command: [process.execPath, CLI_BIN],
      env: {
        ...Object.fromEntries(
          DA_BOND_POOL_INHERITED_ENV.flatMap((name) => {
            const value = process.env[name];
            return value === undefined ? [] : [[name, value]];
          }),
        ),
        MIDGARD_CONFIG_MODE: "disabled",
        MIDGARD_DOTENV_MODE: "disabled",
      },
      cwd: cliRun,
      run: spawnDaBondPoolRuntimeProcess(120_000),
    });
    expect(evidence.exitCode).toBe(0);
    expect(evidence.observer.signerIndex).toBe(0);
    expect(evidence.argv.join(" ")).not.toMatch(/(seed|hex):/u);
    const fromCli = JSON.parse(await readFile(cliPlan.outPath, "utf8"));
    const inProcess = await generateDaLibp2pRuntimeManifest(
      daBondPoolCommitteeRuntimeOptions(cliPlan),
    );
    expect(fromCli).toEqual(JSON.parse(JSON.stringify(inProcess)));
    const env = await committeeEnv(
      cliPlan.outPath,
      cliPlan.observer.libp2pKeySource,
    );
    expect(await verifyDaBondPoolCommitteeRuntime(env)).toEqual({
      peerId: evidence.observer.peerId,
    });
  });
});
