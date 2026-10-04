import { spawn } from "node:child_process";
import { createHash } from "node:crypto";
import * as fs from "node:fs";
import { mkdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  createTrackedTempDirFactory,
  writeScript,
} from "@al-ft/midgard-test-support/temp-files";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  cleanupOwnedProcessGroupAndRecord,
  generateOwnedProcessRunToken,
  OWNED_PROCESS_GROUP_SCHEMA_VERSION,
  ownedProcessCommandSha256,
  parseOwnedProcessGroupRecord,
  terminateOwnedProcessGroup,
  validateOwnedProcessGroupRecord,
  writeOwnedProcessGroupRecord,
} from "../src/e2e/process-ownership.js";

const makeTempDir = createTrackedTempDirFactory("midgard-process-owner-");
const cleanupPids = new Set<number>();

// A plain partial mock keeps normal filesystem behavior and permits scoped
// per-case proc observations without exporting a production test seam.
vi.mock("node:fs", async (importOriginal) => ({
  ...(await importOriginal<typeof import("node:fs")>()),
}));

const alive = (pid: number): boolean => {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
};

const waitFor = async (predicate: () => boolean): Promise<void> => {
  const deadline = Date.now() + 3_000;
  while (Date.now() < deadline) {
    if (predicate()) return;
    await new Promise((resolve) => setTimeout(resolve, 20));
  }
  throw new Error("timed out waiting for process state");
};

afterEach(() => {
  vi.restoreAllMocks();
  for (const pid of cleanupPids) {
    try {
      process.kill(-pid, "SIGKILL");
    } catch {
      try {
        process.kill(pid, "SIGKILL");
      } catch {
        // Already gone.
      }
    }
  }
  cleanupPids.clear();
});

describe.skipIf(process.platform !== "linux")("owned process groups", () => {
  it.each([
    "gone-empty",
    "same-core-then-gone",
    "replaced-start-ticks",
    "replaced-pgid",
    "unreadable-leader",
    "gone-live-child",
    "gone-unreadable-entry",
  ] as const)("observes post-SIGTERM exit safely: %s", async (scenario) => {
    const dir = await makeTempDir();
    const pid = 42_001;
    const childPid = 42_002;
    const sentinelPid = 42_003;
    const spec = {
      recordPath: join(dir, "observed.json"),
      runToken: "ab".repeat(32),
    };
    const command = process.execPath;
    const args = ["-e", "synthetic-owned-process"];
    const cmdline = Buffer.from([command, ...args, ""].join("\0"));
    const bootId = "00000000-0000-0000-0000-000000000001";
    const record = {
      schemaVersion: OWNED_PROCESS_GROUP_SCHEMA_VERSION,
      runToken: spec.runToken,
      bootId,
      pid,
      pgid: pid,
      startTicks: "42",
      procCmdlineSha256: createHash("sha256").update(cmdline).digest("hex"),
      cwd: dir,
      command,
      args,
      commandSha256: ownedProcessCommandSha256({ command, args, cwd: dir }),
      createdAt: "2026-01-01T00:00:00.000Z",
    };
    await writeFile(spec.recordPath, `${JSON.stringify(record)}\n`, "utf8");
    for (const member of [pid, childPid, sentinelPid]) {
      await mkdir(join(dir, member.toString()));
    }
    const originalRead = fs.readFileSync;
    const originalReadlink = fs.readlinkSync;
    const originalReaddir = fs.readdirSync;
    const entries = originalReaddir(dir, { withFileTypes: true });
    let signalled = false;
    let postSignalLeaderReads = 0;
    let postSignalCwdReads = 0;
    let disappeared = false;
    const shouldSucceed =
      scenario === "gone-empty" || scenario === "same-core-then-gone";
    const stat = (pgid: number, ticks: string, state = "S") =>
      `${pid.toString()} (synthetic-owned) ${[state, "1", pgid.toString(), ...Array<string>(16).fill("0"), ticks].join(" ")}\n`;
    const procError = (code: string, path: string) =>
      Object.assign(new Error(`synthetic ${code} reading ${path}`), {
        code,
        path,
      });
    const kill = vi
      .spyOn(process, "kill")
      .mockImplementation((target, signal) => {
        expect(target).toBe(-pid);
        expect(signal).toBe("SIGTERM");
        signalled = true;
        return true;
      });
    vi.spyOn(fs, "readFileSync").mockImplementation((...parameters) => {
      const path = String(parameters[0]);
      if (path === "/proc/sys/kernel/random/boot_id") return bootId;
      if (path === `/proc/${pid.toString()}/cmdline`) {
        return signalled ? Buffer.alloc(0) : cmdline;
      }
      if (path === `/proc/${pid.toString()}/stat`) {
        if (!signalled || ++postSignalLeaderReads === 1) return stat(pid, "42");
        if (scenario === "same-core-then-gone" && !disappeared) {
          return stat(pid, "42");
        }
        if (scenario === "replaced-start-ticks") return stat(pid, "43", "Z");
        if (scenario === "replaced-pgid") return stat(sentinelPid, "42", "Z");
        throw procError(
          scenario === "unreadable-leader" ? "EACCES" : "ENOENT",
          path,
        );
      }
      if (path === `/proc/${childPid.toString()}/stat`) {
        if (scenario === "gone-live-child") return stat(pid, "99");
        throw procError("ENOENT", path);
      }
      if (path === `/proc/${sentinelPid.toString()}/stat`) {
        if (scenario === "gone-unreadable-entry")
          throw procError("EACCES", path);
        return stat(sentinelPid, "100");
      }
      if (path.startsWith("/proc/"))
        throw new Error(`unexpected proc read ${path}`);
      return originalRead(...parameters);
    });
    vi.spyOn(fs, "readlinkSync").mockImplementation((...parameters) => {
      if (String(parameters[0]) === `/proc/${pid.toString()}/cwd`) {
        if (
          signalled &&
          ++postSignalCwdReads > 1 &&
          scenario === "same-core-then-gone"
        ) {
          throw procError("ENOENT", `/proc/${pid.toString()}/cwd`);
        }
        return dir;
      }
      if (String(parameters[0]).startsWith("/proc/")) {
        throw new Error("unexpected mutable process identity read");
      }
      return originalReadlink(...parameters);
    });
    vi.spyOn(fs, "readdirSync").mockImplementation((...parameters) => {
      if (String(parameters[0]) === "/proc") {
        // The core observation still sees the original live leader. By the
        // group scan it has disappeared; a redundant full identity read
        // before this scan would instead encounter its missing cwd.
        if (scenario === "same-core-then-gone") disappeared = true;
        return entries as unknown as ReturnType<typeof fs.readdirSync>;
      }
      return originalReaddir(...parameters);
    });
    const result = await cleanupOwnedProcessGroupAndRecord({
      spec,
      gracefulTimeoutMs: 100,
    });
    expect(result).toMatchObject({
      attempted: true,
      signal: "SIGTERM",
      target: "process_group",
      success: shouldSucceed,
    });
    expect(kill.mock.calls).toEqual([[-pid, "SIGTERM"]]);
    if (scenario === "same-core-then-gone") {
      expect(postSignalLeaderReads).toBeGreaterThanOrEqual(2);
      expect(postSignalCwdReads).toBe(1);
      expect(disappeared).toBe(true);
    }
    if (shouldSucceed) {
      await expect(readFile(spec.recordPath, "utf8")).rejects.toMatchObject({
        code: "ENOENT",
      });
    } else {
      expect(JSON.parse(await readFile(spec.recordPath, "utf8"))).toEqual(
        record,
      );
    }
  });

  it("reclaims a recorded leader and grandchild after controller state is abandoned while preserving a sentinel", async () => {
    const dir = await makeTempDir();
    const grandchildPidPath = join(dir, "grandchild.pid");
    const script = await writeScript(
      dir,
      "leader.mjs",
      [
        "import { spawn } from 'node:child_process';",
        "import { writeFileSync } from 'node:fs';",
        "const child = spawn(process.execPath, ['-e', 'setInterval(() => {}, 1000)'], { stdio: 'ignore' });",
        `writeFileSync(${JSON.stringify(grandchildPidPath)}, String(child.pid));`,
        "setInterval(() => {}, 1000);",
      ].join("\n"),
    );
    const leader = spawn(process.execPath, [script], {
      cwd: dir,
      detached: true,
      stdio: "ignore",
    });
    const sentinel = spawn(
      process.execPath,
      ["-e", "setInterval(() => {}, 1000)"],
      {
        cwd: dir,
        detached: true,
        stdio: "ignore",
      },
    );
    expect(leader.pid).toBeDefined();
    expect(sentinel.pid).toBeDefined();
    cleanupPids.add(leader.pid!);
    cleanupPids.add(sentinel.pid!);
    const spec = {
      recordPath: join(dir, "owned.json"),
      runToken: generateOwnedProcessRunToken(),
    };
    const record = writeOwnedProcessGroupRecord({
      spec,
      pid: leader.pid!,
      command: process.execPath,
      args: [script],
      cwd: dir,
    });
    expect(parseOwnedProcessGroupRecord(record)).toEqual(record);
    const { cwd: _cwd, ...missingCwd } = record;
    expect(() => parseOwnedProcessGroupRecord(missingCwd)).toThrow(
      "missing required field",
    );
    expect(() =>
      parseOwnedProcessGroupRecord({ ...record, unexpected: true }),
    ).toThrow("unknown field");
    expect(() =>
      parseOwnedProcessGroupRecord({
        ...record,
        schemaVersion: "midgard-owned-process-group-v0",
      }),
    ).toThrow(OWNED_PROCESS_GROUP_SCHEMA_VERSION);
    expect(() =>
      parseOwnedProcessGroupRecord({
        ...record,
        commandSha256: "00".repeat(32),
      }),
    ).toThrow("command hash mismatch");
    expect(() =>
      parseOwnedProcessGroupRecord({
        ...record,
        pgid: record.pgid + 1,
      }),
    ).toThrow("process-group leader");
    expect(() =>
      parseOwnedProcessGroupRecord({
        ...record,
        startTicks: `0${record.startTicks}`,
      }),
    ).toThrow("canonical positive decimal");
    expect(() =>
      parseOwnedProcessGroupRecord({
        ...record,
        bootId: record.bootId.toUpperCase(),
      }),
    ).toThrow("lowercase UUID");
    expect(() =>
      parseOwnedProcessGroupRecord({
        ...record,
        cwd: `${record.cwd}/.`,
      }),
    ).toThrow("canonical absolute");
    await waitFor(() => {
      try {
        return Number(fs.readFileSync(grandchildPidPath, "utf8")) > 0;
      } catch {
        return false;
      }
    });
    const grandchildPid = Number(await readFile(grandchildPidPath, "utf8"));
    const result = await cleanupOwnedProcessGroupAndRecord({
      spec,
      gracefulTimeoutMs: 100,
    });
    expect(result.success, JSON.stringify(result)).toBe(true);
    expect(result).toMatchObject({ attempted: true, target: "process_group" });
    await waitFor(() => !alive(leader.pid!) && !alive(grandchildPid));
    expect(alive(sentinel.pid!)).toBe(true);
    await expect(readFile(spec.recordPath, "utf8")).rejects.toMatchObject({
      code: "ENOENT",
    });
  });

  it.each([
    ["startTicks", "0", "canonical positive decimal"],
    ["procCmdlineSha256", "0".repeat(64), "cmdline mismatch"],
    ["cwd", "/", "cwd mismatch"],
    ["commandSha256", "0".repeat(64), "command hash mismatch"],
  ] as const)(
    "refuses a stale or forged %s record",
    async (field, value, reason) => {
      const dir = await makeTempDir();
      const child = spawn(
        process.execPath,
        ["-e", "setInterval(() => {}, 1000)"],
        {
          cwd: dir,
          detached: true,
          stdio: "ignore",
        },
      );
      cleanupPids.add(child.pid!);
      const spec = {
        recordPath: join(dir, `${field}.json`),
        runToken: generateOwnedProcessRunToken(),
      };
      writeOwnedProcessGroupRecord({
        spec,
        pid: child.pid!,
        command: process.execPath,
        args: ["-e", "setInterval(() => {}, 1000)"],
        cwd: dir,
      });
      const record = JSON.parse(
        await readFile(spec.recordPath, "utf8"),
      ) as Record<string, unknown>;
      record[field] = value;
      if (field === "cwd") {
        record.commandSha256 = ownedProcessCommandSha256({
          command: String(record.command),
          args: record.args as string[],
          cwd: value,
        });
      }
      await writeFile(spec.recordPath, `${JSON.stringify(record)}\n`, "utf8");
      const validation = validateOwnedProcessGroupRecord(spec);
      expect(validation).toMatchObject({ valid: false });
      expect(validation.reason).toContain(reason);
      expect(terminateOwnedProcessGroup({ spec })).toMatchObject({
        attempted: false,
        success: false,
      });
      expect(alive(child.pid!)).toBe(true);
    },
  );

  it("refuses a record from another run token", async () => {
    const dir = await makeTempDir();
    const child = spawn(
      process.execPath,
      ["-e", "setInterval(() => {}, 1000)"],
      {
        cwd: dir,
        detached: true,
        stdio: "ignore",
      },
    );
    cleanupPids.add(child.pid!);
    const spec = {
      recordPath: join(dir, "token.json"),
      runToken: generateOwnedProcessRunToken(),
    };
    writeOwnedProcessGroupRecord({
      spec,
      pid: child.pid!,
      command: process.execPath,
      args: ["-e", "setInterval(() => {}, 1000)"],
      cwd: dir,
    });
    expect(
      terminateOwnedProcessGroup({
        spec: { ...spec, runToken: generateOwnedProcessRunToken() },
      }),
    ).toMatchObject({ attempted: false, success: false });
    expect(alive(child.pid!)).toBe(true);
  });

  it("refuses to overwrite an orphan ownership record", async () => {
    const dir = await makeTempDir();
    const child = spawn(
      process.execPath,
      ["-e", "setInterval(() => {}, 1000)"],
      {
        cwd: dir,
        detached: true,
        stdio: "ignore",
      },
    );
    cleanupPids.add(child.pid!);
    const spec = {
      recordPath: join(dir, "orphan.json"),
      runToken: generateOwnedProcessRunToken(),
    };
    writeOwnedProcessGroupRecord({
      spec,
      pid: child.pid!,
      command: process.execPath,
      args: ["-e", "setInterval(() => {}, 1000)"],
      cwd: dir,
    });
    const original = await readFile(spec.recordPath, "utf8");
    expect(() =>
      writeOwnedProcessGroupRecord({
        spec,
        pid: child.pid!,
        command: process.execPath,
        args: ["-e", "setInterval(() => {}, 1000)"],
        cwd: dir,
      }),
    ).toThrow(/EEXIST/u);
    expect(await readFile(spec.recordPath, "utf8")).toBe(original);
  });
});
