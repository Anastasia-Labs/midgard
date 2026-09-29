import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import {
  createDaBondPoolCli,
  DA_BOND_JOURNEY_KEY_ENV,
  DA_BOND_JOURNEY_WALLET_ENV,
  DaBondCliProcessError,
  type DaBondCliProcessRunner,
  spawnDaBondCliProcess,
} from "./da-bond-pool-cli-process.js";
import type { DaBondPoolProcessRun } from "./da-bond-pool-process-evidence.js";

const TX = "ab".repeat(32);

/** A runner that answers each da-bond subcommand, and records what it saw. */
const fakeRunner = (options: { failOn?: string; noTxHash?: boolean } = {}) => {
  const seen: { argv: string[]; env: Record<string, string> }[] = [];
  const run: DaBondCliProcessRunner = async (argv, env) => {
    seen.push({ argv: [...argv], env: { ...env } });
    const command = argv[3]!;
    const exitCode = options.failOn === command ? 2 : 0;
    const stdout =
      command === "status"
        ? JSON.stringify({ state: "Bonded", lovelace: "100" })
        : command === "top-up" || command === "assemble"
          ? JSON.stringify(
              options.noTxHash
                ? { status: { state: "Bonded" } }
                : { txHash: TX, status: { state: "Bonded", unlockAt: "9" } },
            )
          : JSON.stringify({ ok: true });
    const recorded: DaBondPoolProcessRun = {
      argv: [...argv],
      exitCode,
      stdout,
      stderr: exitCode === 0 ? "" : "boom",
    };
    return recorded;
  };
  return { run, seen };
};

const cli = (
  runner: ReturnType<typeof fakeRunner>,
  confirmed: string[] = [],
  recorded: string[] = [],
) =>
  createDaBondPoolCli({
    run: runner.run,
    command: ["node", "/repo/demo/midgard-node/dist/index.js"],
    manifestPath: "/run/deploymentInfo/manifest.json",
    kupoUrl: "http://127.0.0.1:1442",
    ogmiosUrl: "http://127.0.0.1:1337",
    env: { PATH: "/usr/bin" },
    workDirectory: (label) => `/run/cli/${label.replace(/ /gu, "-")}`,
    confirm: async (txHash) => {
      confirmed.push(txHash);
      return true;
    },
    record: async (label) => {
      recorded.push(label);
    },
  });

const CHAIN = [
  "--manifest",
  "/run/deploymentInfo/manifest.json",
  "--kupo-url",
  "http://127.0.0.1:1442",
  "--ogmios-url",
  "http://127.0.0.1:1337",
];

describe("the da-bond CLI chain (P18, P27(6))", () => {
  it("tops up through status, top-up and status, with the seed only in the top-up's env", async () => {
    const runner = fakeRunner();
    const confirmed: string[] = [];
    const tx = await cli(runner, confirmed).topUp({
      amount: 5_000_000n,
      walletSeed: "seed words",
    });
    expect(runner.seen.map(({ argv }) => argv.slice(2))).toEqual([
      ["da-bond", "status", ...CHAIN],
      [
        "da-bond",
        "top-up",
        ...CHAIN,
        "--amount",
        "5000000",
        "--wallet-seed-env",
        DA_BOND_JOURNEY_WALLET_ENV,
      ],
      ["da-bond", "status", ...CHAIN],
    ]);
    expect(
      runner.seen.map(({ env }) => env[DA_BOND_JOURNEY_WALLET_ENV]),
    ).toEqual([undefined, "seed words", undefined]);
    expect(tx.txId).toBe(TX);
    expect(confirmed).toEqual([TX]);
    expect(tx.cli).toMatchObject({ steps: [], confirmedOnChain: true });
    expect(tx.cli.submit.argv[3]).toBe("top-up");
  });

  it("withdraws through build, one witness per key, assemble and status", async () => {
    const runner = fakeRunner();
    const tx = await cli(runner).withdraw({
      step: "complete",
      feeAddress: "addr_test1fee",
      signers: ["k1", "k2"],
      witnesses: [
        { role: "owner-1", seed: "one" },
        { role: "fee-payer", seed: "fee" },
      ],
      complete: { amount: 7n, to: "addr_test1to" },
    });
    const dir = "/run/cli/withdraw-complete";
    expect(runner.seen.map(({ argv }) => argv.slice(3))).toEqual([
      ["status", ...CHAIN],
      [
        "withdraw",
        "complete",
        ...CHAIN,
        "--fee-address",
        "addr_test1fee",
        "--signers",
        "k1,k2",
        "--build-unsigned",
        `${dir}/unsigned.json`,
        "--amount",
        "7",
        "--to",
        "addr_test1to",
      ],
      [
        "witness",
        `${dir}/unsigned.json`,
        "--key-env",
        DA_BOND_JOURNEY_KEY_ENV,
        "--out",
        `${dir}/witness-owner-1.json`,
      ],
      [
        "witness",
        `${dir}/unsigned.json`,
        "--key-env",
        DA_BOND_JOURNEY_KEY_ENV,
        "--out",
        `${dir}/witness-fee-payer.json`,
      ],
      [
        "assemble",
        ...CHAIN,
        `${dir}/unsigned.json`,
        `${dir}/witness-owner-1.json`,
        `${dir}/witness-fee-payer.json`,
      ],
      ["status", ...CHAIN],
    ]);
    expect(runner.seen.map(({ env }) => env[DA_BOND_JOURNEY_KEY_ENV])).toEqual([
      undefined,
      undefined,
      "one",
      "fee",
      undefined,
      undefined,
    ]);
    expect(tx.cli.steps.map(({ argv }) => argv[3])).toEqual([
      "withdraw",
      "witness",
      "witness",
    ]);
    expect(tx.output).toMatchObject({ status: { unlockAt: "9" } });
  });

  it("stops the chain at a non-zero exit and records it, with no fallback", async () => {
    const runner = fakeRunner({ failOn: "assemble" });
    const recorded: string[] = [];
    const failure = await cli(runner, [], recorded)
      .withdraw({
        step: "cancel",
        feeAddress: "addr_test1fee",
        signers: ["k1"],
        witnesses: [{ role: "owner-1", seed: "one" }],
      })
      .catch((error: unknown) => error);
    expect(failure).toBeInstanceOf(DaBondCliProcessError);
    expect(String(failure)).toMatch(/exit 2 from .* assemble .*stderr: boom/u);
    expect((failure as DaBondCliProcessError).runs).toHaveLength(4);
    expect(runner.seen.map(({ argv }) => argv[3]).at(-1)).toBe("assemble");
    expect(recorded).toEqual(["withdraw cancel"]);
  });

  it("names the signal and the timeout of a process it killed", async () => {
    const cwd = await mkdtemp(join(tmpdir(), "da-bond-cli-"));
    try {
      const run = await spawnDaBondCliProcess({
        cwd,
        timeoutMs: 200,
        inheritedNames: new Set(),
      })([process.execPath, "-e", "setTimeout(() => {}, 60000)"], {});
      expect(run).toMatchObject({
        exitCode: null,
        signal: "SIGKILL",
        timedOutAfterMs: 200,
      });
      expect(
        new DaBondCliProcessError("da-bond top-up failed", [run]).message,
      ).toMatch(/killed by SIGKILL after the 200 ms timeout from /u);
    } finally {
      await rm(cwd, { recursive: true, force: true });
    }
  });

  it("fails a submit that prints no txHash", async () => {
    await expect(
      cli(fakeRunner({ noTxHash: true })).topUp({
        amount: 1n,
        walletSeed: "s",
      }),
    ).rejects.toThrow(/printed no txHash/u);
  });
});
