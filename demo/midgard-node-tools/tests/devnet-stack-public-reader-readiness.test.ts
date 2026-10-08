import { expect, it, vi } from "vitest";

vi.mock("../src/devnet-stack/chain.js", () => ({ psql: vi.fn() }));
vi.mock("../src/devnet-stack/deploy.js", () => ({ nodeCli: vi.fn() }));
vi.mock("../src/devnet-stack/node-env.js", () => ({
  DA_THRESHOLD: 2,
  nodeEnvironment: () => ({}),
}));
vi.mock("../src/devnet-stack/identities.js", () => ({ walletInfos: vi.fn() }));
vi.mock("../src/devnet-stack/watcher.js", () => ({
  watcherServiceSpecs: () => [],
}));
vi.mock("../src/devnet-stack/history-pin.js", () => ({
  recordedHistoryGenesisPin: () => ({}),
}));
vi.mock("../src/devnet-stack/deployment-origin.js", () => ({
  recordedL1Origin: () => ({}),
}));
vi.mock("../src/devnet-stack/reserve-float-chain.js", () => ({
  enduranceReasons: vi.fn(),
  runEnduranceMaintainer: vi.fn(),
}));

vi.mock("../src/devnet-stack/da.js", async (original) => ({
  ...(await original<typeof import("../src/devnet-stack/da.js")>()),
  committeeEnvironment: () => ({}),
}));

import type { DeployContext } from "../src/devnet-stack/deploy.js";
import {
  makeLayout,
  type RunEnv,
  servicePorts,
} from "../src/devnet-stack/layout.js";
import { specsDigest } from "../src/devnet-stack/service-recovery-scope.js";
import { serviceSpecs } from "../src/devnet-stack/services.js";

const run: RunEnv = {
  runId: "synthetic",
  composeProject: "synthetic",
  networkMagic: 42,
  ogmiosPort: 2337,
  kupoPort: 2442,
  postgresPort: 5432,
  postgresUser: "synthetic",
  postgresPassword: "synthetic",
  postgresDatabase: "synthetic",
  cardanoImage: "synthetic",
  postgresImage: "synthetic",
  portOffset: 0,
};
const context: DeployContext = {
  layout: makeLayout("/tmp/synthetic-public-reader-no-files"),
  run,
  identities: {
    schemaVersion: "midgard-devnet-identities-v1",
    seeds: {
      operator: "synthetic",
      merge: "synthetic",
      referenceScript: "synthetic",
      settlement: "synthetic",
      daCosigner: "synthetic",
      daSubmitter0: "synthetic",
      daSubmitter1: "synthetic",
      daAvailability0: "synthetic",
      daAvailability1: "synthetic",
      watcherProver: "synthetic",
      watcherAvailability: "synthetic",
      userA: "synthetic",
      userB: "synthetic",
      userC: "synthetic",
    },
    libp2p: {
      producer: "synthetic",
      committee0: "synthetic",
      committee1: "synthetic",
      retained: "synthetic",
    },
    adminApiKey: "synthetic",
    publicReaderPassword: "synthetic",
  },
  artifacts: {
    nativeOwnerBinary: "/synthetic/owner",
    nativeOwnerSha256: "synthetic",
    transportBinary: "/synthetic/chain-sync",
  },
};

it("wires the reader's actual readyz to its exact declared loopback binding and recovery scope", () => {
  const specs = serviceSpecs(context, {
    txHash: "00".repeat(32),
    outputIndex: 0,
  });
  const reader = specs.find((spec) => spec.name === "public-retained-da");
  expect(reader).toBeDefined();
  expect(reader?.env.DA_PUBLIC_RETAINED_DA_HEALTH_HOST).toBe("127.0.0.1");
  expect(reader?.env.DA_PUBLIC_RETAINED_DA_HEALTH_PORT).toBe("7406");
  expect(reader?.healthUrl).toBe("http://127.0.0.1:7406/healthz");
  expect(reader?.readyUrl).toBe("http://127.0.0.1:7406/readyz");
  const shifted = serviceSpecs(
    { ...context, run: { ...run, ogmiosPort: 3337, portOffset: 1000 } },
    { txHash: "00".repeat(32), outputIndex: 0 },
  );
  expect(
    shifted.find((spec) => spec.name === "public-retained-da")?.readyUrl,
  ).toBe("http://127.0.0.1:8406/readyz");
  expect(specsDigest(shifted, "code")).not.toBe(specsDigest(specs, "code"));
});

it.each([-4000, 30000, 0.5])(
  "refuses invalid declared port offset %s",
  (portOffset) => {
    expect(() => servicePorts({ ...run, portOffset })).toThrow(
      "valid TCP ports",
    );
  },
);

it("refuses a public-reader binding that collides with a recorded run service", () => {
  expect(() => servicePorts({ ...run, kupoPort: 7406 })).toThrow(
    "must not overlap",
  );
});
