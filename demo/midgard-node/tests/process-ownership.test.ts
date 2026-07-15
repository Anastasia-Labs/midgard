import { spawn } from "node:child_process";
import { readFileSync } from "node:fs";
import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import {
  cleanupOwnedProcessGroupAndRecord,
  generateOwnedProcessRunToken,
  ownedProcessCommandSha256,
  terminateOwnedProcessGroup,
  validateOwnedProcessGroupRecord,
  writeOwnedProcessGroupRecord,
} from "@/e2e/process-ownership.js";

import {
  createTrackedTempDirFactory,
  writeScript,
} from "./helpers/temp-files.js";

const makeTempDir = createTrackedTempDirFactory("midgard-process-owner-");
const cleanupPids = new Set<number>();

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
    writeOwnedProcessGroupRecord({
      spec,
      pid: leader.pid!,
      command: process.execPath,
      args: [script],
      cwd: dir,
    });
    await waitFor(() => {
      try {
        return Number(readFileSync(grandchildPidPath, "utf8")) > 0;
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
    ["startTicks", "0", "start ticks mismatch"],
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
