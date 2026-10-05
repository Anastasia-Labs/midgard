import { chmod, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import {
  closeWatcherNativeExactPointServices,
  openWatcherNativeExactPointQuery,
  readWatcherNativeExactPointQuery,
  watcherNativeExactPointServicePids,
} from "../../src/l1/native-chain-sync.js";
import {
  exactPointWatcherConfig,
  GENESIS_BYTES,
} from "../support/native-exact-point-query-config.js";

const predecessor = { blockHash: "aa".repeat(32), blockNo: "9", slot: "100" };
const target = { blockHash: "bb".repeat(32), blockNo: "10", slot: "101" };
const other = { blockHash: "dd".repeat(32), blockNo: "10", slot: "102" };
// The fixture helper never rolls this target forward.
const silent = { blockHash: "cc".repeat(32), blockNo: "10", slot: "103" };
type Point = typeof target;
type LogEntry = Readonly<{ kind: string; pid: number; id?: string }>;

const cleanup: (() => Promise<void>)[] = [];
afterEach(async () => {
  await closeWatcherNativeExactPointServices();
  for (const close of cleanup.splice(0).reverse()) await close();
});

const alive = (pid: number): boolean => {
  try {
    process.kill(pid, 0);
    return true;
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === "ESRCH") return false;
    throw error;
  }
};

const helper = async (mode = "honest") => {
  const dir = await mkdtemp(join("/var/tmp", "native-exact-service-"));
  cleanup.push(() => rm(dir, { recursive: true, force: true }));
  const nodeConfig = join(dir, "node.json");
  const genesisConfig = join(dir, "genesis.json");
  const binaryPath = join(dir, "helper");
  const logPath = join(dir, "helper.log");
  await writeFile(genesisConfig, GENESIS_BYTES);
  await writeFile(
    nodeConfig,
    JSON.stringify({ ShelleyGenesisFile: genesisConfig }),
  );
  await writeFile(logPath, "");
  await writeFile(
    binaryPath,
    `#!${process.execPath}
const { serve } = await import(${JSON.stringify(new URL("../support/native-exact-point-service-fixture.mjs", import.meta.url).href)});
serve(${JSON.stringify({ mode, logPath })});
`,
  );
  await chmod(binaryPath, 0o700);
  const watcherConfig = exactPointWatcherConfig(nodeConfig, genesisConfig);
  const log = async (): Promise<readonly LogEntry[]> =>
    (await readFile(logPath, "utf8"))
      .split("\n")
      .filter(Boolean)
      .map((line) => JSON.parse(line) as LogEntry);
  const starts = async () =>
    (await log()).filter((entry) => entry.kind === "start");
  const open = async (point: Point = target, timeoutMs = 2000) => {
    const query = await openWatcherNativeExactPointQuery({
      binaryPath,
      watcherConfig,
      predecessor,
      target: point,
      timeoutMs,
    });
    cleanup.push(query.close);
    return query;
  };
  return { open, log, starts };
};

const exited = (pid: number) =>
  expect.poll(() => alive(pid), { timeout: 12_000, interval: 20 }).toBe(false);

describe("persistent native exact-point helper", () => {
  it("serves concurrent queries as sessions of one helper and leaves no process after close", async () => {
    const { open, log, starts } = await helper();
    const [first, second] = await Promise.all([open(target), open(other)]);
    expect(
      readWatcherNativeExactPointQuery(first.receipt).event.blockHash,
    ).toBe(target.blockHash);
    expect(
      readWatcherNativeExactPointQuery(second.receipt).event.blockHash,
    ).toBe(other.blockHash);
    const helpers = await starts();
    expect(helpers).toHaveLength(1);
    const pid = helpers[0]!.pid;
    expect(watcherNativeExactPointServicePids()).toEqual([pid]);
    expect((await log()).filter((entry) => entry.kind === "open")).toHaveLength(
      2,
    );
    await first.close();
    await second.close();
    expect(() => readWatcherNativeExactPointQuery(first.receipt)).toThrow(
      /absent or stale/,
    );
    await closeWatcherNativeExactPointServices();
    expect(alive(pid)).toBe(false);
    expect(watcherNativeExactPointServicePids()).toEqual([]);
    expect((await log()).map((entry) => entry.kind)).toContain("eof");
  });

  it("stops an idle helper on its own", async () => {
    const { open, starts } = await helper();
    const query = await open();
    const pid = (await starts())[0]!.pid;
    await query.close();
    expect(alive(pid)).toBe(true);
    await exited(pid);
    expect(watcherNativeExactPointServicePids()).toEqual([]);
  });

  it("closes an expired session and keeps the helper serving", async () => {
    const { open, log, starts } = await helper();
    await expect(open(silent, 150)).rejects.toThrow(/expired|timed out|cancel/);
    const query = await open(target);
    expect(
      readWatcherNativeExactPointQuery(query.receipt).event.blockHash,
    ).toBe(target.blockHash);
    expect(await starts()).toHaveLength(1);
    expect(await log()).toContainEqual(
      expect.objectContaining({ kind: "close", id: "1" }),
    );
  });

  it("fails live and in-flight queries when the helper crashes and starts a fresh helper next", async () => {
    const { open, starts } = await helper("crash_on_second_open");
    const live = await open(target);
    const crashed = (await starts())[0]!.pid;
    await expect(open(other)).rejects.toThrow(
      "native chain-sync process exited unexpectedly",
    );
    expect(() => readWatcherNativeExactPointQuery(live.receipt)).toThrow(
      /absent or stale/,
    );
    await exited(crashed);
    const next = await open(target);
    expect(readWatcherNativeExactPointQuery(next.receipt).event.blockHash).toBe(
      target.blockHash,
    );
    const helpers = await starts();
    expect(helpers).toHaveLength(2);
    expect(helpers[1]!.pid).not.toBe(crashed);
    expect(watcherNativeExactPointServicePids()).toEqual([helpers[1]!.pid]);
  });

  it("fails an in-flight query when the helper is killed externally", async () => {
    const { open, log, starts } = await helper();
    const pending = open(silent, 60_000);
    const refusal = expect(pending).rejects.toThrow(
      "native chain-sync process exited unexpectedly",
    );
    await expect
      .poll(async () => (await log()).some((entry) => entry.kind === "open"))
      .toBe(true);
    const killed = (await starts())[0]!.pid;
    process.kill(killed, "SIGKILL");
    await refusal;
    await exited(killed);
    await open(target);
    expect(await starts()).toHaveLength(2);
  });

  it("kills a helper that never releases a killed session", async () => {
    const { open, log, starts } = await helper("ignore_close");
    const query = await open(target);
    const pid = (await starts())[0]!.pid;
    // A concurrent in-flight session keeps the helper busy, so only the
    // release bound can end it, and it fails as the helper process exits.
    const pending = open(silent, 60_000);
    const refusal = expect(pending).rejects.toThrow(
      "native chain-sync process exited unexpectedly",
    );
    await expect
      .poll(async () => (await log()).filter((e) => e.kind === "open").length)
      .toBe(2);
    await query.close();
    expect(alive(pid)).toBe(true);
    await refusal;
    await exited(pid);
    expect((await log()).map((entry) => entry.kind)).not.toContain("eof");
    expect(watcherNativeExactPointServicePids()).toEqual([]);
  }, 30_000);

  it.each(["unknown_session", "malformed"])(
    "kills a helper whose output is %s",
    async (mode) => {
      const { open, starts } = await helper(mode);
      await expect(open(target)).rejects.toThrow(
        "native chain-sync process exited unexpectedly",
      );
      await exited((await starts())[0]!.pid);
      expect(watcherNativeExactPointServicePids()).toEqual([]);
    },
  );
});
