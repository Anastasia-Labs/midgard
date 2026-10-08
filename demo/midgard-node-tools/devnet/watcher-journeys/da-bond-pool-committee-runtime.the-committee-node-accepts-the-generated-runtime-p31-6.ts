import { writeFileSync } from "node:fs";
import { chmod, mkdir, readFile, unlink, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  generateDaLibp2pRuntimeManifest,
  writeDaLibp2pRuntimeManifest,
} from "midgard-node/da/libp2p-runtime-manifest";
import { publishWorkflowDeployment } from "midgard-node/tests/helpers/published-workflow-deployment";
import { beforeAll, describe, expect, it } from "vitest";

import {
  buildDaBondPoolCommitteeEnv,
  DA_BOND_POOL_INHERITED_ENV,
} from "./da-bond-pool-committee-process.js";
import {
  DaBondPoolCommitteeRuntimeError,
  daBondPoolCommitteeRuntimeOptions,
  type DaBondPoolCommitteeRuntimePlan,
  daBondPoolCommitteeSettings,
  planDaBondPoolCommitteeRuntime,
  produceDaBondPoolCommitteeRuntime,
  readWorktreePortOffset,
  reuseDaBondPoolCommitteeRuntime,
  spawnDaBondPoolRuntimeProcess,
  verifyDaBondPoolCommitteeRuntime,
  writeFreshDaLibp2pKey,
} from "./da-bond-pool-committee-runtime.js";
import {
  CLI_BIN,
  deploymentOf,
  freshDirectory,
  REPOSITORY_ROOT,
} from "./da-bond-pool-committee-runtime.the-committee-runtime-plan-p31.js";

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
        l1Origin: { slot: 100, blockHash: "ab".repeat(32) },
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
