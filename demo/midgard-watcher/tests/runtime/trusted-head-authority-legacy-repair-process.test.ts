import { fork } from "node:child_process";
import { randomUUID } from "node:crypto";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

import { build } from "esbuild";
import { afterAll, beforeAll, expect, it } from "vitest";

import { sha256 } from "../../src/runtime/trusted-head-authority.exact-record.js";
import {
  auditLegacyWatcherTrustedHeadAuthority,
  repairLegacyWatcherTrustedHeadAuthorityFinalRecord,
} from "../../src/runtime/trusted-head-authority.js";
import { legacyScene } from "./trusted-head-authority.legacy-fixture.js";
import { directory, head } from "./trusted-head-authority.policy.js";

let bundleDirectory: string, entry: string;
beforeAll(async () => {
  const support = join(dirname(fileURLToPath(import.meta.url)), "../support");
  bundleDirectory = await mkdtemp(join(support, ".repair-worker-"));
  entry = join(bundleDirectory, "entry.mjs");
  await build({
    entryPoints: [join(support, "trusted-head-repair-worker-entry.ts")],
    outfile: entry,
    bundle: true,
    platform: "node",
    target: "node22",
    format: "esm",
    conditions: ["midgard-source"],
    logLevel: "silent",
    banner: {
      js: 'import { createRequire as __createRequire } from "node:module"; const require = __createRequire(import.meta.url);',
    },
    plugins: [
      {
        name: "external-npm",
        setup(builder) {
          builder.onResolve({ filter: /^[^./]/u }, async (args) => {
            if (args.pluginData === "external-resolution") return undefined;
            if (
              args.path.startsWith("@al-ft/midgard-") ||
              args.path.startsWith("@al-ft/l1-node-transport")
            )
              return undefined;
            if (args.path.startsWith("node:"))
              return { path: args.path, external: true };
            const resolved = await builder.resolve(args.path, {
              importer: args.importer,
              resolveDir: args.resolveDir,
              kind: args.kind,
              pluginData: "external-resolution",
            });
            return { ...resolved, external: true };
          });
        },
      },
    ],
  });
});
afterAll(async () => {
  if (bundleDirectory !== undefined)
    await rm(bundleDirectory, { recursive: true, force: true });
});

it.each([
  "raw-before",
  "raw-after",
  "intent-before",
  "intent-after",
  "remove-before",
  "remove-after",
  "complete-before",
  "complete-after",
])(
  "recovers the same exact repair after actual process SIGKILL at %s",
  async (phase) => {
    const legacy = await legacyScene(3),
      torn = Buffer.from('{"head":'),
      name = "00000000000000000002.json";
    await writeFile(join(legacy.path, name), torn);
    const input = {
      legacyDirectory: legacy.path,
      recoveryDirectory: await directory(),
      policy: legacy.policy,
      recordAuthenticationKey: legacy.recordAuthenticationKey,
      attemptId: `repair-${randomUUID()}`,
      expectedPriorHead: head(legacy.policy, 1, "77"),
      expectedTornRecordName: name,
      expectedTornSha256: sha256(torn),
      reason: "Synthetic process crash recovery",
    };
    const child = fork(entry, [], {
      execArgv: [],
      stdio: ["ignore", "ignore", "pipe", "ipc"],
    });
    let stderr = "",
      message: unknown;
    child.stderr!.on("data", (bytes: Buffer) => {
      stderr += bytes.toString();
    });
    child.on("message", (value: unknown) => {
      message = value;
    });
    const done = new Promise<{ code: number | null; signal: string | null }>(
      (resolve, reject) => {
        child.once("error", reject);
        child.once("close", (code, signal) => resolve({ code, signal }));
      },
    );
    const killOnExit = () => {
      child.kill("SIGKILL");
    };
    process.once("exit", killOnExit);
    const timer = setTimeout(killOnExit, 15000);
    try {
      child.send({
        phase,
        input: {
          ...input,
          recordAuthenticationKey: [...input.recordAuthenticationKey],
        },
      });
      const result = await done;
      expect(result.signal, JSON.stringify({ message, stderr })).toBe(
        "SIGKILL",
      );
      expect(message).toBeUndefined();
      await repairLegacyWatcherTrustedHeadAuthorityFinalRecord(input);
      expect(
        await readFile(join(input.recoveryDirectory, "removed-record.bin")),
      ).toEqual(torn);
      await expect(readFile(join(legacy.path, name))).rejects.toThrow();
      expect(
        (
          await auditLegacyWatcherTrustedHeadAuthority({
            directory: legacy.path,
            policy: legacy.policy,
            recordAuthenticationKey: legacy.recordAuthenticationKey,
            liveRecordLimit: 1,
          })
        ).head,
      ).toEqual(input.expectedPriorHead);
    } finally {
      clearTimeout(timer);
      process.removeListener("exit", killOnExit);
      child.kill("SIGKILL");
      await done;
    }
  },
);
