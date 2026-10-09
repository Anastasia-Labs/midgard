/**
 * The native MPF owner's RPC channel (`NativeChildRpc`) when the child
 * closes its stdin and stays up: the next write fails with EPIPE on the
 * stdin stream, and the channel fails as on the child's exit (the pending
 * request rejects, the failure handler runs, the child is stopped) instead
 * of the stream's error reaching the process.
 */
import { createHash } from "node:crypto";
import { access, chmod, mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import { NativeChildRpc } from "../src/services/mpf-native-owner/service.native-child-rpc.js";

const dirs: string[] = [];
const pids: number[] = [];
afterEach(async () => {
  for (const pid of pids.splice(0)) {
    try {
      process.kill(pid, "SIGKILL");
    } catch {
      // Already gone.
    }
  }
  await Promise.all(
    dirs.splice(0).map((dir) => rm(dir, { recursive: true, force: true })),
  );
});

/** Polls for `file` every 10 ms, for up to 10 s. */
const untilExists = async (file: string) => {
  const deadline = Date.now() + 10_000;
  for (;;) {
    try {
      await access(file);
      return;
    } catch {
      if (Date.now() > deadline)
        throw new Error(`timed out waiting for ${file}`);
      await new Promise((resolve) => setTimeout(resolve, 10));
    }
  }
};

describe("the native MPF owner's RPC channel", () => {
  it("fails the channel as on the child's exit when a write meets a closed stdin", async () => {
    const dir = await mkdtemp(path.join(tmpdir(), "native-child-stdin-"));
    dirs.push(dir);
    const closed = path.join(dir, "stdin-closed");
    const binary = path.join(dir, "owner.sh");
    // Closes its stdin, says so, and stays up.
    await writeFile(
      binary,
      `#!/bin/sh\nexec 0<&-\n: > "${closed}"\nexec sleep 30\n`,
    );
    await chmod(binary, 0o755);
    const sha = createHash("sha256").update("owner").digest("hex");

    let pid: number | undefined;
    const rpc = new NativeChildRpc(binary, sha, 1 << 20, 10_000, (spawned) => {
      pid = spawned;
      pids.push(spawned);
    });
    const failures: Error[] = [];
    rpc.setFailureHandler((error) => failures.push(error));
    await untilExists(closed);

    await expect(rpc.handshake()).rejects.toThrow(
      /Native MPF owner stdin failed: .*EPIPE/,
    );
    expect(rpc.isClosed).toBe(true);
    expect(failures).toHaveLength(1);
    expect(failures[0]!.message).toMatch(/stdin failed/);
    // The child is stopped, so its restart starts a fresh one.
    expect(pid).toBeDefined();
    const deadline = Date.now() + 10_000;
    for (;;) {
      try {
        process.kill(pid!, 0);
      } catch {
        break;
      }
      if (Date.now() > deadline) throw new Error("the child is still up");
      await new Promise((resolve) => setTimeout(resolve, 10));
    }
    // A later request rejects as on a closed channel.
    await expect(rpc.handshake()).rejects.toThrow(/closed/);
  }, 30_000);
});
