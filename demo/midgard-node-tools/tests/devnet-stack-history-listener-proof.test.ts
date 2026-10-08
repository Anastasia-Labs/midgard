import { execFileSync, spawn } from "node:child_process";
import { createHash, randomUUID, X509Certificate } from "node:crypto";
import { readFileSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { Duplex } from "node:stream";
import { fileURLToPath } from "node:url";

import {
  WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
  type WatcherNativeChainSyncEvent,
} from "midgard-watcher";
import { build } from "tsup";
import { afterEach, beforeAll, expect, it } from "vitest";

import {
  CHILD_STATUS_ATTEMPT_ENV,
  CHILD_STATUS_CODE_ENV,
  CHILD_STATUS_SPECS_ENV,
} from "../src/devnet-stack/child-status-channel.js";
import { historyChildClient } from "../src/devnet-stack/history-child-client.js";
import {
  type HistoryWindowOffer,
  parseHistoryChildActor,
} from "../src/devnet-stack/history-child-evidence.js";
import { type HistoryListenerBinding } from "../src/devnet-stack/history-listener-evidence.js";
import { createHistoryWindowSealer } from "../src/devnet-stack/history-native-window-proof.js";
import { offerForHistorySeal } from "../src/devnet-stack/history-offer-availability.js";
import { provePinnedHistoryListener } from "../src/devnet-stack/history-pinned-listener.js";
import {
  windowFixture,
  writeSyntheticControls,
} from "./helpers/history-native-window-fixture.js";

const root = fileURLToPath(new URL("../", import.meta.url));
const cleanups: (() => Promise<void>)[] = [];
afterEach(async () => {
  for (const close of cleanups.splice(0).reverse()) await close();
});
beforeAll(async () => {
  await build({
    config: join(root, "tsup.config.ts"),
    entry: ["tests/helpers/history-listener-child.ts"],
    outDir: ".probe-dist/history-listener-probe",
    clean: true,
  });
}, 30000);
const setup = async () => {
  const f = await windowFixture(2161);
  cleanups.push(() => f.close());
  const actor = {
    role: "history-recorder" as const,
    runId: randomUUID(),
    deploymentFingerprint: "11".repeat(32),
    codeStamp: "22".repeat(32),
    serviceSpecsDigest: "33".repeat(32),
    attemptId: randomUUID(),
  };
  const sealer = createHistoryWindowSealer({
    actor,
    directories: f.directories,
    watcherConfig: f.watcherConfig,
    binaryPath: f.binaryPath,
  });
  let opening: WatcherNativeChainSyncEvent | undefined;
  await f.startMain(async (event) => {
    opening ??= event;
  });
  await expect.poll(() => opening, { timeout: 10000 }).toBeDefined();
  const target = f.points[2161];
  if (opening === undefined || target === undefined)
    throw Error("actual native fixture absent");
  await sealer.capture(opening, target, 20000);
  const seal = sealer.seal(120000);
  if (seal === null) throw Error("full native range seal absent");
  const offer = offerForHistorySeal(f.directories, seal);
  if (offer === null) throw Error("physical range offer absent");
  return { f, actor, sealer, offer };
};
const certificate = (directory: string, name: string) => {
  const keyPath = join(directory, `${name}.key`);
  const certificatePath = join(directory, `${name}.crt`);
  // Only this test's synthetic keys; no deployment credential is inspected.
  execFileSync(
    "openssl",
    [
      "req",
      "-x509",
      "-newkey",
      "rsa:2048",
      "-nodes",
      "-days",
      "1",
      "-subj",
      `/CN=${name}`,
      "-addext",
      `subjectAltName=DNS:${name}`,
      "-keyout",
      keyPath,
      "-out",
      certificatePath,
    ],
    { stdio: "ignore" },
  );
  const cert = new X509Certificate(readFileSync(certificatePath));
  const operatorIdentitySha256 = createHash("sha256")
    .update(cert.publicKey.export({ type: "spki", format: "der" }))
    .digest("hex");
  return { keyPath, certificatePath, operatorIdentitySha256 };
};
const child = async (input: {
  role:
    | "history-archive-a"
    | "history-archive-b"
    | "history-tunnel"
    | "history-recorder";
  directory: string;
  runId: string;
  listeners: readonly {
    hostname: string;
    port: number;
    caPath: string;
    binding: HistoryListenerBinding;
  }[];
  keyPath?: string;
  certificatePath?: string;
}) => {
  const config = join(input.directory, `listener-${randomUUID()}.json`);
  writeFileSync(config, JSON.stringify(input));
  const processChild = spawn(
    process.execPath,
    [
      join(
        root,
        ".probe-dist/history-listener-probe/history-listener-child.js",
      ),
      config,
    ],
    {
      detached: true,
      stdio: ["ignore", "pipe", "pipe", "pipe"],
      env: {
        ...process.env,
        [CHILD_STATUS_CODE_ENV]: "22".repeat(32),
        [CHILD_STATUS_SPECS_ENV]: "33".repeat(32),
        [CHILD_STATUS_ATTEMPT_ENV]: randomUUID(),
      },
    },
  );
  let output = "";
  let errors = "";
  processChild.stderr?.on("data", (bytes: Buffer) => {
    errors += bytes.toString("utf8");
  });
  const joined = new Promise<void>((resolve) =>
    processChild.once("close", () => resolve()),
  );
  const stop = async () => {
    const pid = processChild.pid;
    if (pid === undefined) return;
    const signal = (kind: NodeJS.Signals) => {
      try {
        process.kill(-pid, kind);
      } catch {
        /* Already joined. */
      }
    };
    signal("SIGTERM");
    const timer = setTimeout(() => signal("SIGKILL"), 1000);
    await joined;
    clearTimeout(timer);
    signal("SIGKILL");
  };
  cleanups.push(stop);
  const ready: unknown = await new Promise((resolve, reject) => {
    const timer = setTimeout(
      () =>
        reject(Error(`owned listener child did not become ready: ${errors}`)),
      10000,
    );
    processChild.once("error", (error) => {
      clearTimeout(timer);
      reject(error);
    });
    processChild.once("close", () => {
      clearTimeout(timer);
      reject(Error(`owned listener child exited before readiness: ${errors}`));
    });
    processChild.stdout?.on("data", (bytes: Buffer) => {
      output += bytes.toString("utf8");
      if (output.length > 16384) {
        clearTimeout(timer);
        reject(Error("owned listener oversized output"));
        return;
      }
      const line = output.split("\n")[0];
      if (line !== undefined && output.includes("\n")) {
        clearTimeout(timer);
        try {
          resolve(JSON.parse(line));
        } catch (error) {
          reject(error);
        }
      }
    });
  });
  if (
    ready === null ||
    typeof ready !== "object" ||
    !("port" in ready) ||
    typeof ready.port !== "number" ||
    !("actor" in ready)
  )
    throw Error("owned listener readiness malformed");
  const actor = parseHistoryChildActor(ready.actor);
  const pipe = processChild.stdio[3];
  if (
    actor === null ||
    actor.childPid !== processChild.pid ||
    !(pipe instanceof Duplex)
  )
    throw Error("owned listener actual actor/pipe absent");
  const client = historyChildClient({ actor, pipe });
  cleanups.push(async () => client.close());
  return { port: ready.port, client, stop, ready };
};
const archive = async (
  f: Awaited<ReturnType<typeof windowFixture>>,
  offer: HistoryWindowOffer,
  index: 0 | 1,
  wrongKey = false,
) => {
  const hostname = index === 0 ? "history-a.test" : "history-b.test";
  const cert = certificate(f.root, hostname);
  const binding = {
    sourceId: hostname,
    operatorIdentitySha256: wrongKey
      ? "ff".repeat(32)
      : cert.operatorIdentitySha256,
    deploymentIdentityDigest: offer.window.actor.deploymentFingerprint,
    blueprintHash: "44".repeat(32),
    policyDigest: "55".repeat(32),
  };
  const route = { hostname, port: 0, caPath: cert.certificatePath, binding };
  const running = await child({
    role: index === 0 ? "history-archive-a" : "history-archive-b",
    directory: f.directories[index],
    runId: offer.window.actor.runId,
    listeners: [route],
    ...cert,
  });
  return { ...running, route: { ...route, port: running.port } };
};
it("actual archive child proves every2160 retained row and predecessor over its pinned TLS listener", async () => {
  const { f, offer, sealer } = await setup();
  const a = await archive(f, offer, 0);
  expect(await a.client.request("seal", null, 5000)).toBeNull();
  expect(await a.client.request("prove", offer, 1)).toBeNull();
  expect(
    await provePinnedHistoryListener({
      listener: {
        hostname: a.route.hostname,
        port: a.port,
        ca: readFileSync(a.route.caPath, "utf8"),
        binding: { ...a.route.binding, policyDigest: "66".repeat(32) },
      },
      offer,
      timeoutMs: 5000,
    }),
  ).toBe(false);
  expect((await a.client.request("prove", offer, 5000))?.offer).toEqual(offer);
  expect(
    sealer.revalidate(offer.window.sealId, offer.window.generation),
  ).toEqual(offer.window);
  const row = join(f.directories[0], "canonical", "100.json");
  const before = readFileSync(row, "utf8");
  const changed: unknown = JSON.parse(before);
  if (changed === null || typeof changed !== "object")
    throw Error("synthetic row malformed");
  writeFileSync(row, JSON.stringify({ ...changed, prevHash: "aa".repeat(32) }));
  expect((await a.client.request("prove", offer, 5000))?.offer).toBeNull();
  writeFileSync(row, before);
  rmSync(join(f.directories[0], "canonical", "1.json"));
  expect((await a.client.request("prove", offer, 5000))?.offer).toBeNull();
}, 45000);
it("holds a matching HTTP proof body behind a valid TLS certificate with the wrong pinned provider key", async () => {
  const { f, offer } = await setup();
  const a = await archive(f, offer, 0, true);
  expect((await a.client.request("prove", offer, 5000))?.offer).toBeNull();
}, 45000);
it("actual tunnel child proves both pinned CONNECT routes and holds when one route disappears", async () => {
  const { f, offer } = await setup();
  const a = await archive(f, offer, 0);
  const b = await archive(f, offer, 1);
  const tunnel = await child({
    role: "history-tunnel",
    directory: f.root,
    runId: offer.window.actor.runId,
    listeners: [a.route, b.route],
  });
  expect((await tunnel.client.request("prove", offer, 5000))?.offer).toEqual(
    offer,
  );
  await b.stop();
  expect((await tunnel.client.request("prove", offer, 5000))?.offer).toBeNull();
}, 45000);

it("actual recorder FD3 replies are revoked by actual main-native rollback and never reused", async () => {
  const { f, offer } = await setup();
  const cert = certificate(f.root, "recorder.test");
  const recorder = await child({
    role: "history-recorder",
    directory: f.root,
    runId: offer.window.actor.runId,
    listeners: [
      {
        hostname: "recorder.test",
        port: 0,
        caPath: cert.certificatePath,
        binding: {
          sourceId: "recorder.test",
          operatorIdentitySha256: cert.operatorIdentitySha256,
          deploymentIdentityDigest: offer.window.actor.deploymentFingerprint,
          blueprintHash: "44".repeat(32),
          policyDigest: "55".repeat(32),
        },
      },
    ],
  });
  const current = await recorder.client.request("seal", null, 5000);
  if (current?.offer === null || current === null)
    throw Error("actual recorder offer absent");
  expect(current.offer.window.rowCount).toBe(2160);
  expect(
    (await recorder.client.request("revalidate", current.offer, 5000))?.offer,
  ).toEqual(current.offer);
  if (
    !("controlPath" in recorder.ready) ||
    typeof recorder.ready.controlPath !== "string"
  )
    throw Error("synthetic recorder control absent");
  writeSyntheticControls(recorder.ready.controlPath, [
    {
      schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
      kind: "roll_backward",
      point: { kind: "origin" },
      tip: { kind: "origin" },
    },
  ]);
  await expect
    .poll(
      async () =>
        (await recorder.client.request("revalidate", current.offer, 5000))
          ?.offer,
      { timeout: 10000 },
    )
    .toBeNull();
  expect((await recorder.client.request("seal", null, 5000))?.offer).toBeNull();
}, 45000);
