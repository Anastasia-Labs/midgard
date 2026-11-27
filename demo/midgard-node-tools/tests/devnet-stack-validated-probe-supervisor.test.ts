import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { expect, it } from "vitest";

import {
  refuseService,
  requestServiceRecovery,
  serviceRefusal,
} from "../src/devnet-stack/service-refusal.js";
import {
  DEFAULT_POLICY,
  type ServiceSpec,
  superviseServices,
  type SupervisorPaths,
} from "../src/devnet-stack/supervisor.js";

const pause = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));
const until = async (condition: () => boolean) => {
  const end = Date.now() + 5000;
  while (!condition()) {
    if (Date.now() >= end)
      throw new Error("synthetic child condition timed out");
    await pause(10);
  }
};

it.each(["stable", "code", "spec"])(
  "retains refusal if %s scope changes while a live child awaits validated readiness",
  async (drift) => {
    const dir = mkdtempSync(join(tmpdir(), "validated-role-probe-"));
    let complete: (ready: boolean) => void = () => {};
    const readiness = new Promise<boolean>((resolve) => {
      complete = resolve;
    });
    let checks = 0;
    let code = "recorded-code";
    const service: ServiceSpec = {
      name: "authority",
      command: process.execPath,
      args: ["-e", "setInterval(()=>{},1000)"],
      cwd: dir,
      env: {},
      readyProbe: {
        binding: "authenticated-recorded-identity",
        check: async () => {
          checks += 1;
          return await readiness;
        },
      },
    };
    const services = [service];
    const paths: SupervisorPaths = {
      runDir: dir,
      pidDir: join(dir, "stack/services"),
      events: join(dir, "events"),
      serviceLog: (name) => join(dir, `${name}.log`),
      runtimeCodeStamp: () => code,
      serviceSpecs: services,
    };
    const refusal = refuseService(paths, service.name);
    requestServiceRecovery(
      paths,
      service.name,
      refusal.refusalId,
      "synthetic validated correction attempt",
    );
    const abort = new AbortController();
    const running = superviseServices(services, paths, abort.signal, {
      ...DEFAULT_POLICY,
      probeIntervalMs: 10,
      prestartRetryMs: 10,
      probeTimeoutMs: 1000,
      stopGraceMs: 100,
    });
    try {
      await until(() => checks === 1);
      expect(serviceRefusal(paths, service.name)?.refusalId).toBe(
        refusal.refusalId,
      );
      if (drift === "code") code = "replacement-code";
      if (drift === "spec")
        services[0] = { ...service, env: { scope: "replacement" } };
      complete(true);
      if (drift === "stable")
        await until(() => serviceRefusal(paths, service.name) === undefined);
      else {
        await until(() => checks >= 2);
        expect(serviceRefusal(paths, service.name)?.refusalId).toBe(
          refusal.refusalId,
        );
      }
    } finally {
      complete(false);
      abort.abort();
      await running;
      rmSync(dir, { recursive: true, force: true });
    }
  },
);
