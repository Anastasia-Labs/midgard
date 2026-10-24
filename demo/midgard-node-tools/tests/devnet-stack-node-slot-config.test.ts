import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import { readDevnetCustomNetwork } from "../src/devnet-stack/chain.js";
import { makeLayout } from "../src/devnet-stack/layout.js";

// A devnet's L1 slot mapping anchors slot 0 at the Shelley genesis
// `systemStart` with the genesis slot length. The node reads it through its
// L1 access; the watcher's Custom network is built from this reading.

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
});
