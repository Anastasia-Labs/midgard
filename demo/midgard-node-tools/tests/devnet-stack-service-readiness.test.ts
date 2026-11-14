import { expect, it } from "vitest";

import { probeServiceReadiness } from "../src/devnet-stack/service-readiness.js";
import { specsDigest } from "../src/devnet-stack/service-recovery-scope.js";
import type { ServiceSpec } from "../src/devnet-stack/supervisor.js";

const service: ServiceSpec = {
  name: "authority",
  command: "synthetic",
  args: [],
  cwd: "/synthetic",
  env: {},
};
it("keeps missing or throwing validated readiness unavailable without exposing exception contents", async () => {
  expect((await probeServiceReadiness(service, 20)).ok).toBe(false);
  const result = await probeServiceReadiness(
    {
      ...service,
      readyProbe: {
        binding: "recorded",
        check: async () => {
          throw new Error("private fixture detail");
        },
      },
    },
    20,
  );
  expect(result.ok).toBe(false);
  expect(result.body).not.toContain("private fixture detail");
});
it("bounds a stalled authenticated probe and keeps readiness closed", async () => {
  const result = await probeServiceReadiness(
    {
      ...service,
      readyProbe: {
        binding: "recorded",
        check: () => new Promise<boolean>(() => {}),
      },
    },
    10,
  );
  expect(result.ok).toBe(false);
  expect(JSON.parse(result.body)).toMatchObject({ ready: false });
});
it("binds exact declarative authenticated probe identity in queued recovery scope", () => {
  const role = {
    ...service,
    readyProbe: { binding: "recorded-A", check: async () => true },
  };
  expect(specsDigest([role], "code")).not.toBe(
    specsDigest(
      [{ ...role, readyProbe: { ...role.readyProbe, binding: "recorded-B" } }],
      "code",
    ),
  );
});
