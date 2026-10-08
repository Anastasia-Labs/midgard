import { mkdirSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import type { ExecResult } from "../src/devnet-stack/exec.js";
import {
  ensureHistoryGenesisPin,
  recordedHistoryGenesisPin,
} from "../src/devnet-stack/history-pin.js";
import {
  type Identities,
  LIBP2P_IDENTITIES,
  WALLET_ROLES,
} from "../src/devnet-stack/identities.js";
import { Journal } from "../src/devnet-stack/journal.js";
import { makeLayout, type RunEnv } from "../src/devnet-stack/layout.js";
import { nodeEnvironment } from "../src/devnet-stack/node-env.js";

const dirs: string[] = [];
const runLayout = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-stack-pin-"));
  dirs.push(dir);
  const layout = makeLayout(dir);
  mkdirSync(layout.state, { recursive: true });
  return layout;
};
afterEach(() => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const PIN_A = "a1".repeat(32);
const PIN_B = "b2".repeat(32);

/** The node verb's output, with a log line ahead of the JSON as the CLI prints. */
const printed = (sha256: string, code = 0): ExecResult => ({
  code,
  signal: null,
  stdout: `some log line\n${JSON.stringify(
    {
      variable: "L1_HISTORY_GENESIS_LOSSLESS_SHA256",
      algorithm: "ogmios-shelley-result-lossless-v1",
      sha256,
    },
    null,
    2,
  )}\n`,
  stderr: "",
  log: "/transcript.log",
});

describe("ensureHistoryGenesisPin", () => {
  it("records the first pin and accepts the same chain on every later call", async () => {
    const layout = runLayout();
    let derivations = 0;
    const derive = () => {
      derivations += 1;
      return Promise.resolve(printed(PIN_A));
    };
    expect(() => recordedHistoryGenesisPin(layout)).toThrow(/run up first/);
    expect(await ensureHistoryGenesisPin(layout, derive)).toBe(PIN_A);
    expect(recordedHistoryGenesisPin(layout)).toBe(PIN_A);
    expect(await ensureHistoryGenesisPin(layout, derive)).toBe(PIN_A);
    // A resumed run still re-derives it from the live chain.
    expect(derivations).toBe(2);
  });

  it("refuses a chain whose pin differs from the recorded one and keeps the record", async () => {
    const layout = runLayout();
    await ensureHistoryGenesisPin(layout, () =>
      Promise.resolve(printed(PIN_A)),
    );
    await expect(
      ensureHistoryGenesisPin(layout, () => Promise.resolve(printed(PIN_B))),
    ).rejects.toThrow(/not the run's chain/);
    expect(recordedHistoryGenesisPin(layout)).toBe(PIN_A);
  });

  it("records nothing when the verb fails or prints no pin", async () => {
    const layout = runLayout();
    await expect(
      ensureHistoryGenesisPin(layout, () => Promise.resolve(printed(PIN_A, 1))),
    ).rejects.toThrow(/history-genesis-pin failed/);
    await expect(
      ensureHistoryGenesisPin(layout, () =>
        Promise.resolve(printed("A1".repeat(32))),
      ),
    ).rejects.toThrow(/printed no pin/);
    expect(() => recordedHistoryGenesisPin(layout)).toThrow(/run up first/);
  });

  it("survives a journal instance opened before the pin was recorded", async () => {
    const layout = runLayout();
    const earlier = new Journal(layout.journal);
    await ensureHistoryGenesisPin(layout, () =>
      Promise.resolve(printed(PIN_A)),
    );
    earlier.set("funding", { txId: "t" });
    expect(recordedHistoryGenesisPin(layout)).toBe(PIN_A);
    expect(new Journal(layout.journal).get("funding")).toEqual({ txId: "t" });
  });
});

describe("nodeEnvironment history genesis pin", () => {
  const layout = makeLayout("/run");
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
    seeds: Object.fromEntries(
      WALLET_ROLES.map((role) => [role, `seed-${role}`]),
    ),
    libp2p: Object.fromEntries(
      LIBP2P_IDENTITIES.map((id) => [id, "00".repeat(32)]),
    ),
    adminApiKey: "k",
    publicReaderPassword: "r",
  } as Identities;
  const artifacts = {
    nativeOwnerBinary: "/bin/o",
    nativeOwnerSha256: "h",
    transportBinary: "/bin/c",
  };
  const oneShot = { txHash: "00".repeat(32), outputIndex: 0 };
  const l1Origin = { slot: 1, blockHash: "11".repeat(32) };

  it("carries the pin to listen and to commands", () => {
    for (const role of ["command", "listen"] as const)
      expect(
        nodeEnvironment({
          layout,
          run,
          identities,
          artifacts,
          oneShot,
          l1Origin,
          historyGenesisPin: PIN_A,
          role,
        }).L1_HISTORY_GENESIS_LOSSLESS_SHA256,
      ).toBe(PIN_A);
  });

  it("omits it only for a command and never starts listen without it", () => {
    expect(
      nodeEnvironment({
        layout,
        run,
        identities,
        artifacts,
        historyGenesisPin: null,
        role: "command",
      }),
    ).not.toHaveProperty("L1_HISTORY_GENESIS_LOSSLESS_SHA256");
    expect(() =>
      nodeEnvironment({
        layout,
        run,
        identities,
        artifacts,
        oneShot,
        l1Origin,
        historyGenesisPin: null,
        role: "listen",
      }),
    ).toThrow(/run up first/);
  });
});
