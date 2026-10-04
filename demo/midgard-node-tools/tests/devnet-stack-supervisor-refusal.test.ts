import {
  existsSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { createServer } from "node:http";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { expect, it } from "vitest";

import { makeLayout } from "../src/devnet-stack/layout.js";
import {
  consumeServiceRecovery,
  refuseService,
  requestServiceRecovery,
  serviceRefusal,
} from "../src/devnet-stack/service-refusal.js";
import { serviceReport } from "../src/devnet-stack/stack.js";
import {
  DEFAULT_POLICY,
  superviseServices,
  type SupervisorPaths,
} from "../src/devnet-stack/supervisor.js";

const policy = {
  ...DEFAULT_POLICY,
  initialBackoffMs: 20,
  maxBackoffMs: 40,
  prestartRetryMs: 20,
  probeIntervalMs: 20,
  stopGraceMs: 100,
};
const pause = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));
const until = async (condition: () => boolean) => {
  const deadline = Date.now() + 5_000;
  while (!condition()) {
    if (Date.now() >= deadline)
      throw new Error("supervisor condition timed out");
    await pause(10);
  }
};
const pathsAt = (runDir: string): SupervisorPaths => ({
  runDir,
  pidDir: join(runDir, "stack/services"),
  events: join(runDir, "events.ndjson"),
  serviceLog: (name) => join(runDir, `${name}.log`),
});
const events = (paths: SupervisorPaths): Record<string, unknown>[] =>
  !existsSync(paths.events)
    ? []
    : readFileSync(paths.events, "utf8")
        .trim()
        .split("\n")
        .map((line) => JSON.parse(line));

it("holds a real child exit 78 without blind restarts while transient exit 70 keeps retrying", async () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-refusal-"));
  const paths = pathsAt(dir);
  const abort = new AbortController();
  const service = (name: string, code: number) => ({
    name,
    command: process.execPath,
    args: ["-e", `process.exit(${code})`],
    cwd: dir,
    env: {},
  });
  const running = superviseServices(
    [
      service("refused", 78),
      service("transient", 70),
      {
        ...service("signal", 70),
        args: ["-e", "process.kill(process.pid, 'SIGTERM')"],
      },
    ],
    paths,
    abort.signal,
    policy,
  );
  try {
    await until(
      () =>
        events(paths).filter(
          (event) => event.event === "exit" && event.service === "refused",
        ).length > 0,
    );
    await until(
      () =>
        events(paths).filter(
          (event) => event.event === "start" && event.service === "transient",
        ).length >= 3,
    );
    await until(
      () =>
        events(paths).filter(
          (event) => event.event === "start" && event.service === "signal",
        ).length >= 3,
    );
    expect(
      events(paths).filter(
        (event) => event.event === "start" && event.service === "refused",
      ),
    ).toHaveLength(1);
    expect(
      events(paths).filter(
        (event) =>
          event.event === "restart-scheduled" && event.service === "refused",
      ),
    ).toHaveLength(0);
  } finally {
    abort.abort();
    await running;
    rmSync(dir, { recursive: true, force: true });
  }
});

it("preserves refusal across daemon replacement and clears only a matching one-shot attempt after real child readiness", async () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-refusal-recovery-"));
  const paths = pathsAt(dir);
  const reservation = createServer();
  await new Promise<void>((resolve) =>
    reservation.listen(0, "127.0.0.1", resolve),
  );
  const address = reservation.address();
  if (address === null || typeof address === "string")
    throw new Error("no reserved port");
  const port = address.port;
  await new Promise<void>((resolve) => reservation.close(() => resolve()));
  const config = join(dir, "correction");
  const ready = join(dir, "ready");
  let prestartReady = true;
  const spec = {
    prestart: () => Promise.resolve(prestartReady),
    name: "role",
    command: process.execPath,
    cwd: dir,
    env: {},
    readyUrl: `http://127.0.0.1:${port}/readyz`,
    args: [
      "-e",
      `const fs=require('node:fs'); const config=${JSON.stringify(config)}; const ready=${JSON.stringify(ready)}; const value=fs.existsSync(config)?fs.readFileSync(config,'utf8'):'78'; if(value!=='ok')process.exit(Number(value)); require('node:http').createServer((req,res)=>{const ok=fs.existsSync(ready);res.writeHead(ok?200:503);res.end(JSON.stringify({ready:ok}));}).listen(${port},'127.0.0.1');`,
    ],
  };
  let abort = new AbortController();
  let running = superviseServices([spec], paths, abort.signal, policy);
  try {
    await until(() => serviceRefusal(paths, "role") !== undefined);
    const first = serviceRefusal(paths, "role")!;
    expect(await serviceReport(makeLayout(dir), spec)).toMatchObject({
      alive: false,
      ready: false,
      reasons: ["configuration_or_deployment_refused"],
      refusal: { refusalId: first.refusalId },
    });
    abort.abort();
    await running;
    abort = new AbortController();
    running = superviseServices([spec], paths, abort.signal, policy);
    await pause(100);
    expect(
      events(paths).filter((event) => event.event === "start"),
    ).toHaveLength(1);
    expect(serviceRefusal(paths, "role")?.refusalId).toBe(first.refusalId);
    expect(() =>
      requestServiceRecovery(
        paths,
        "role",
        "old-token",
        "operator explanation",
      ),
    ).toThrow(/token/);
    prestartReady = false;
    requestServiceRecovery(
      paths,
      "role",
      first.refusalId,
      "operator requested a reattempt; still invalid",
    );
    await pause(100);
    expect(
      events(paths).filter((event) => event.event === "start"),
    ).toHaveLength(1);
    prestartReady = true;
    await until(
      () => serviceRefusal(paths, "role")?.refusalId !== first.refusalId,
    );
    const second = serviceRefusal(paths, "role")!;
    await pause(100);
    expect(
      events(paths).filter((event) => event.event === "start"),
    ).toHaveLength(2);
    expect(() =>
      requestServiceRecovery(paths, "role", first.refusalId, "stale request"),
    ).toThrow(/token/);
    writeFileSync(config, "70");
    requestServiceRecovery(
      paths,
      "role",
      second.refusalId,
      "retry still fails transiently before validation",
    );
    await until(() =>
      events(paths).some(
        (event) => event.event === "exit" && event.code === 70,
      ),
    );
    await pause(100);
    expect(
      events(paths).filter((event) => event.event === "start"),
    ).toHaveLength(3);
    expect(serviceRefusal(paths, "role")?.refusalId).toBe(second.refusalId);
    writeFileSync(config, "ok");
    requestServiceRecovery(
      paths,
      "role",
      second.refusalId,
      "external fixture corrected; child must validate",
    );
    await until(
      () =>
        events(paths).filter((event) => event.event === "start").length === 4,
    );
    await pause(100);
    expect(serviceRefusal(paths, "role")?.refusalId).toBe(second.refusalId);
    expect((await serviceReport(makeLayout(dir), spec)).ready).toBe(false);
    writeFileSync(ready, "child validated readiness");
    await until(() => serviceRefusal(paths, "role") === undefined);
    expect(await serviceReport(makeLayout(dir), spec)).toMatchObject({
      alive: true,
      ready: true,
    });
    expect(
      events(paths).filter((event) => event.event === "refusal-recovered"),
    ).toHaveLength(1);
  } finally {
    abort.abort();
    await running;
    rmSync(dir, { recursive: true, force: true });
  }
});

it("rejects changed public deployment binding and consumes mismatched requests without authorizing an attempt", () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-refusal-binding-"));
  const paths = {
    ...pathsAt(dir),
    deploymentBinding: "original-public-manifest",
  };
  try {
    const refusal = refuseService(paths, "role");
    expect(() =>
      requestServiceRecovery(
        { ...paths, deploymentBinding: "changed-public-manifest" },
        "role",
        refusal.refusalId,
        "operator note cannot change deployment",
      ),
    ).toThrow(/deployment manifest changed/);
    requestServiceRecovery(
      paths,
      "role",
      refusal.refusalId,
      "one scoped attempt",
    );
    expect(
      consumeServiceRecovery(
        { ...paths, deploymentBinding: "changed-public-manifest" },
        refusal,
      ),
    ).toBeUndefined();
    expect(consumeServiceRecovery(paths, refusal)).toBeUndefined();
    expect(serviceRefusal(paths, "role")?.refusalId).toBe(refusal.refusalId);
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});
