import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import { readDevnetCustomNetwork } from "../src/devnet-stack/chain.js";
import {
  type Identities,
  LIBP2P_IDENTITIES,
  WALLET_ROLES,
} from "../src/devnet-stack/identities.js";
import { makeLayout, type RunEnv } from "../src/devnet-stack/layout.js";
import { nodeEnvironment } from "../src/devnet-stack/node-env.js";

// A devnet deployment derives its validation-trace validators with the run's
// L1 slot mapping (slot 0 at the Shelley genesis `systemStart`), and the node
// has no default for a Custom network, so the run hands it the mapping.

const dirs: string[] = [];
afterEach(() => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const SYSTEM_START = "2026-09-01T00:00:00Z";

const runLayout = (genesis?: Record<string, unknown>) => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-stack-slots-"));
  dirs.push(dir);
  const layout = makeLayout(dir);
  mkdirSync(layout.state, { recursive: true });
  if (genesis !== undefined) {
    mkdirSync(dirname(layout.shelleyGenesis), { recursive: true });
    writeFileSync(layout.shelleyGenesis, JSON.stringify(genesis));
  }
  return layout;
};

const run: RunEnv = {
  runId: "t",
  composeProject: "p",
  networkMagic: 42,
  ogmiosPort: 1,
  kupoPort: 2,
  postgresPort: 3,
  postgresUser: "u",
  postgresPassword: "pw",
  postgresDatabase: "d",
  cardanoImage: "c",
  postgresImage: "pg",
  portOffset: 0,
};
const identities = {
  schemaVersion: "midgard-devnet-identities-v1",
  seeds: Object.fromEntries(WALLET_ROLES.map((role) => [role, `seed-${role}`])),
  libp2p: Object.fromEntries(
    LIBP2P_IDENTITIES.map((id) => [id, "00".repeat(32)]),
  ),
  adminApiKey: "k",
  publicReaderPassword: "r",
} as Identities;
const artifacts = {
  nativeOwnerBinary: "/artifacts/owner",
  nativeOwnerSha256: "h",
  transportBinary: "/artifacts/chain-sync",
};

const commandEnv = (layout: ReturnType<typeof makeLayout>) =>
  nodeEnvironment({ layout, run, identities, artifacts, role: "command" });

describe("the devnet L1 slot mapping", () => {
  it("anchors slot 0 at the genesis start with the genesis slot length", () => {
    const layout = runLayout({
      networkMagic: 42,
      systemStart: SYSTEM_START,
      slotLength: 0.1,
    });
    expect(readDevnetCustomNetwork(layout)).toEqual({
      networkMagic: 42,
      slotConfig: {
        zeroTime: Date.parse(SYSTEM_START),
        zeroSlot: 0,
        slotLength: 100,
      },
    });
  });

  it("refuses a genesis slot length that is not whole milliseconds", () => {
    const layout = runLayout({
      networkMagic: 42,
      systemStart: SYSTEM_START,
      slotLength: 0.0005,
    });
    expect(() => readDevnetCustomNetwork(layout)).toThrow(
      /not a whole number of milliseconds/u,
    );
  });

  it("hands the node the run's mapping as MIDGARD_L1_SLOT_CONFIG", () => {
    const layout = runLayout({
      networkMagic: 42,
      systemStart: SYSTEM_START,
      slotLength: 1,
    });
    const env = commandEnv(layout);
    expect(env.NETWORK).toBe("Custom");
    expect(JSON.parse(env.MIDGARD_L1_SLOT_CONFIG!)).toEqual({
      zeroTime: Date.parse(SYSTEM_START),
      zeroSlot: 0,
      slotLength: 1000,
    });
  });

  it("sets no mapping before the run has a genesis", () => {
    expect(commandEnv(runLayout()).MIDGARD_L1_SLOT_CONFIG).toBeUndefined();
  });
});
