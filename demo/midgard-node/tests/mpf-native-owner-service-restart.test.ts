import { createHash } from "node:crypto";
import {
  copyFile,
  mkdtemp,
  readFile,
  rename,
  rm,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Level } from "level";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import { ProductionNativeMpfOwnerService } from "../src/services/mpf-native-owner/index.js";
import { prepareEventFlatDigest } from "../src/workers/utils/mpf-event-flat-digest.js";
import {
  nativeOwnerBinaryPath,
  nativeOwnerBinaryPresent,
  warnNativeOwnerBinaryAbsent,
} from "./helpers/native-owner-binary.js";

// The service spawns the native owner binary; see the helper for the
// build/skip contract (#642).
const binaryPath = nativeOwnerBinaryPath;
const binaryPresent = nativeOwnerBinaryPresent();
if (!binaryPresent) {
  warnNativeOwnerBinaryAbsent("mpf-native-owner-service-restart");
}

/**
 * Child restarts of the production native owner: a child that dies is always
 * restarted from the durable root marker; only failed restarts count toward
 * the window's limit, and an exhausted window heals once its oldest failure
 * leaves it.
 */
describe.skipIf(!binaryPresent)("native MPF owner child restarts", () => {
  const temporaryPaths: string[] = [];
  let binarySha256 = "";

  beforeAll(async () => {
    await prepareEventFlatDigest();
    binarySha256 = createHash("sha256")
      .update(await readFile(binaryPath))
      .digest("hex");
  });

  afterEach(async () => {
    await Promise.all(
      temporaryPaths
        .splice(0)
        .map((path) => rm(path, { recursive: true, force: true })),
    );
  });

  const openEmptyRootService = async (
    prefix: string,
    options: {
      readonly binaryPath?: string;
      readonly restartLimit?: number;
      readonly restartWindowMs?: number;
      readonly restartBackoffBaseMs?: number;
      readonly restartBackoffMaxMs?: number;
    },
  ) => {
    const root = await mkdtemp(join(tmpdir(), prefix));
    temporaryPaths.push(root);
    const levelPath = join(root, "ledger");
    const seed = new Level<string, unknown>(levelPath, {
      valueEncoding: "json",
    });
    await seed.open();
    await seed.put("__root__", SDK.EMPTY_MERKLE_TREE_ROOT);
    await seed.close();
    const childPids: number[] = [];
    const service = await ProductionNativeMpfOwnerService.create({
      levelPath,
      binaryPath,
      binarySha256,
      ...options,
      onChildSpawnForTests(pid) {
        childPids.push(pid);
      },
    });
    return { root, service, childPids };
  };

  const waitUntil = async (condition: () => boolean): Promise<void> => {
    const deadline = Date.now() + 15_000;
    while (!condition() && Date.now() < deadline) {
      await new Promise((resolve) => setTimeout(resolve, 10));
    }
  };

  it("restarts a child that keeps dying, after a backoff each time, and never exhausts on deaths alone", async () => {
    // One allowed failed restart in a long window: deaths are not failures.
    const { service, childPids } = await openEmptyRootService(
      "midgard-native-owner-restart-deaths-",
      {
        restartLimit: 1,
        restartWindowMs: 600_000,
        restartBackoffBaseMs: 20,
        restartBackoffMaxMs: 80,
      },
    );
    try {
      for (let death = 1; death <= 4; death += 1) {
        process.kill(childPids[death - 1]!, "SIGKILL");
        await waitUntil(() => childPids.length === death + 1);
        expect(childPids).toHaveLength(death + 1);
        expect((await service.diagnostics()).childRestarts).toBe(death);
        expect(service.terminalFailure()).toBeUndefined();
      }
      expect(service.restartHealth()).toMatchObject({
        restartsInWindow: 4,
        failedRestartsInWindow: 0,
        exhausted: false,
      });
    } finally {
      await service.close();
    }
  });

  it("refuses while failed restarts exhaust the window, and restarts once from the durable root when the oldest leaves it, polled only through terminalFailure", async () => {
    const pinned = await mkdtemp(join(tmpdir(), "midgard-native-owner-pin-"));
    temporaryPaths.push(pinned);
    const pinnedBinaryPath = join(pinned, "architecture-g-owner");
    await copyFile(binaryPath, pinnedBinaryPath);
    const restartWindowMs = 1_000;
    const { service, childPids } = await openEmptyRootService(
      "midgard-native-owner-restart-exhaustion-",
      { binaryPath: pinnedBinaryPath, restartLimit: 1, restartWindowMs },
    );
    try {
      // A binary that is not the pinned one makes the restart itself fail.
      const original = join(pinned, "original");
      await copyFile(pinnedBinaryPath, original);
      const rebuilt = join(pinned, "rebuilt");
      await writeFile(
        rebuilt,
        Buffer.concat([await readFile(binaryPath), Buffer.from([0])]),
        { mode: 0o755 },
      );
      await rename(rebuilt, pinnedBinaryPath);
      process.kill(childPids[0]!, "SIGKILL");
      await waitUntil(() => service.terminalFailure() !== undefined);
      expect(service.terminalFailure()?.message).toMatch(
        /restart limit exhausted: 1 failed restart\(s\) within 1000 ms; restarts resume once the oldest leaves the window: .*binary SHA-256 mismatch/,
      );
      await expect(service.diagnostics()).rejects.toThrow(
        /restart limit exhausted/,
      );
      expect(() => service.createWorkerPort()).toThrow(
        /restart limit exhausted/,
      );
      expect(childPids).toHaveLength(1);

      // The pinned binary is back; once the failed restart leaves the window
      // the owner restarts, exactly once, from its durable root. Nothing but
      // the supervisor's terminalFailure poll asks for it: an idle node sends
      // the owner no operation that would.
      await rename(original, pinnedBinaryPath);
      await waitUntil(
        () => service.terminalFailure() === undefined && childPids.length === 2,
      );
      expect(service.terminalFailure()).toBeUndefined();
      expect(childPids).toHaveLength(2);
      const diagnostics = await service.diagnostics();
      expect(diagnostics.durableRoot).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
      expect(diagnostics.childRestarts).toBe(1);
      await new Promise((resolve) => setTimeout(resolve, 250));
      expect(childPids).toHaveLength(2);
    } finally {
      await service.close();
    }
  });

  it("starts no child when it is closed during a restart backoff", async () => {
    const { service, childPids } = await openEmptyRootService(
      "midgard-native-owner-restart-close-",
      { restartBackoffBaseMs: 60_000, restartBackoffMaxMs: 60_000 },
    );
    let closed = false;
    try {
      // The first restart in the window runs at once; the second waits out
      // its backoff, during which the service closes.
      process.kill(childPids[0]!, "SIGKILL");
      await waitUntil(() => childPids.length === 2);
      expect(childPids).toHaveLength(2);
      // Kill the restarted child only once it serves, so the second restart
      // follows the death of a running owner, not a restart still loading.
      expect((await service.diagnostics()).childRestarts).toBe(1);
      process.kill(childPids[1]!, "SIGKILL");
      await waitUntil(() => service.restartHealth().restartsInWindow === 2);
      expect(service.restartHealth()).toMatchObject({
        restartsInWindow: 2,
        failedRestartsInWindow: 0,
      });
      const closedAt = Date.now();
      await service.close();
      closed = true;
      expect(Date.now() - closedAt).toBeLessThan(10_000);
      expect(childPids).toHaveLength(2);
    } finally {
      if (!closed) await service.close();
    }
  });

  it("re-verifies the binary pin before a restart spawns the child again", async () => {
    const pinned = await mkdtemp(join(tmpdir(), "midgard-native-owner-pin-"));
    temporaryPaths.push(pinned);
    const pinnedBinaryPath = join(pinned, "architecture-g-owner");
    await copyFile(binaryPath, pinnedBinaryPath);
    const { service, childPids } = await openEmptyRootService(
      "midgard-native-owner-repin-",
      { binaryPath: pinnedBinaryPath },
    );
    try {
      // Replace the file the way a rebuild does, while the child runs: the
      // replacement is a working owner, but not the pinned one.
      const rebuilt = join(pinned, "rebuilt");
      await writeFile(
        rebuilt,
        Buffer.concat([await readFile(binaryPath), Buffer.from([0])]),
        { mode: 0o755 },
      );
      await rename(rebuilt, pinnedBinaryPath);
      process.kill(childPids[0]!, "SIGKILL");
      // Once the child is reaped its exit has closed the RPC, so the next
      // operation waits on the restart rather than racing the dying child.
      await waitUntil(() => {
        try {
          process.kill(childPids[0]!, 0);
          return false;
        } catch {
          return true;
        }
      });
      await expect(service.diagnostics()).rejects.toThrow(
        /binary SHA-256 mismatch/,
      );
      expect(childPids).toHaveLength(1);
    } finally {
      await service.close();
    }
  });
});
