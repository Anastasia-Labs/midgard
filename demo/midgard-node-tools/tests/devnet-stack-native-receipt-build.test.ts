import { spawn } from "node:child_process";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { build } from "tsup";
import { expect, it } from "vitest";

const root = fileURLToPath(new URL("../", import.meta.url));
/** A compiler/probe group belongs only to this test, including its native children. */
const command = (args: readonly string[]) =>
  new Promise<string>((resolve, reject) => {
    const child = spawn(process.execPath, [...args], {
      cwd: root,
      detached: true,
      stdio: ["ignore", "pipe", "pipe"],
    });
    let stdout = "";
    let stderr = "";
    let failure: Error | undefined;
    let killing: ReturnType<typeof setTimeout> | undefined;
    const signal = (kind: NodeJS.Signals) => {
      if (child.pid === undefined) return;
      try {
        process.kill(-child.pid, kind);
      } catch {
        /* Already joined. */
      }
    };
    const stop = (reason: string) => {
      failure ??= new Error(reason);
      signal("SIGTERM");
      killing ??= setTimeout(() => signal("SIGKILL"), 1000);
    };
    const timer = setTimeout(
      () => stop("compiled receipt probe exceeded its bounded test deadline"),
      20000,
    );
    child.stdout.on("data", (bytes: Buffer) => {
      stdout += bytes.toString("utf8");
      if (stdout.length + stderr.length > 1_048_576)
        stop("oversized compiled receipt probe output");
    });
    child.stderr.on("data", (bytes: Buffer) => {
      stderr += bytes.toString("utf8");
      if (stdout.length + stderr.length > 1_048_576)
        stop("oversized compiled receipt probe output");
    });
    child.once("error", (error) => {
      failure = error;
    });
    child.once("close", (code) => {
      clearTimeout(timer);
      clearTimeout(killing);
      signal("SIGKILL");
      if (failure !== undefined) reject(failure);
      else if (code !== 0)
        reject(new Error(`compiled receipt command exited ${code}: ${stderr}`));
      else resolve(stdout);
    });
  });

it("preserves actual dynamic recorder native receipts in the production tools build", async () => {
  await build({
    config: join(root, "tsup.config.ts"),
    entry: ["tests/helpers/history-native-compiled-probe.ts"],
    outDir: ".probe-dist/native-receipt-probe",
    clean: true,
    // Source-only public test fixture; real watcher runtime imports use the
    // exact production external rule. The probe imports no midgard-node.
    noExternal: ["midgard-watcher/tests/l1/native-chain-sync.config"],
  });
  const output = await command([
    join(
      root,
      ".probe-dist/native-receipt-probe/history-native-compiled-probe.js",
    ),
  ]);
  expect(output).toContain(
    "PASS compiled actual callback receipt + shared native module identity",
  );
}, 45000);
