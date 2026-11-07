import { writeFileSync } from "node:fs";
import {
  mkdir,
  mkdtemp,
  readFile,
  rm,
  stat,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import { afterAll, describe, expect, it } from "vitest";

import {
  DA_BOND_POOL_COMMITTEE_RUNTIME_MANIFEST,
  daBondPoolCommitteeRuntimeArgv,
  DaBondPoolCommitteeRuntimeError,
  daBondPoolCommitteeRuntimeOptions,
  type DaBondPoolCommitteeRuntimePlan,
  type DaBondPoolRuntimeProcessRunner,
  planDaBondPoolCommitteeRuntime,
  produceDaBondPoolCommitteeRuntime,
  readWorktreePortOffset,
  writeFreshDaLibp2pKey,
} from "./da-bond-pool-committee-runtime.js";

export const REPOSITORY_ROOT = fileURLToPath(
  new URL("../../../../", import.meta.url),
);

export const CLI_BIN = join(REPOSITORY_ROOT, "demo/midgard-node/dist/index.js");

const vkey = (byte: number) => byte.toString(16).padStart(2, "0").repeat(32);

export const deploymentOf = (members: number, threshold = members) => ({
  network: "Custom",
  da: {
    committeeVkeys: Array.from({ length: members }, (_, index) =>
      vkey(index + 1),
    ),
    threshold,
  },
});

const directories: string[] = [];

export const freshDirectory = async (label: string) => {
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
