import { spawn } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type UTxO,
} from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import { Journal } from "../src/devnet-stack/journal.js";
import type { Layout } from "../src/devnet-stack/layout.js";
import {
  type ChainOutput,
  ensureReserveFloat,
  FLOAT_MINIMUM_LOVELACE,
  FLOAT_TARGET_LOVELACE,
  type FloatDeps,
  type FloatMaintainer,
  type FloatRecord,
  maintainReserveFloatOnce,
  reserveFloatReasons,
  runReserveFloatMaintainer,
} from "../src/devnet-stack/reserve-float.js";
import { withFloatLock } from "../src/devnet-stack/reserve-float-chain.js";

const ADA = 1_000_000n;
const INTERVAL_MS = 60_000;

const dirs: string[] = [];
const tempDir = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-stack-float-maintainer-"));
  dirs.push(dir);
  return dir;
};
afterEach(() => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const flat = (utxo: { txHash: string; outputIndex: number }) =>
  `${utxo.txHash}#${utxo.outputIndex}`;

/**
 * A lucid-evolution emulator behind the float step's deps, the way Kupo and
 * cardano-cli serve it on the devnet: the genesis-like payer builds and signs
 * one-input payments, signed bytes are files, a resubmission of applied bytes
 * is refused and ignored, and each sleep forges a block.
 */
const emulatorChain = async () => {
  const payer = generateEmulatorAccount({ lovelace: 1_000_000n * ADA });
  const reserve = generateEmulatorAccount({ lovelace: FLOAT_TARGET_LOVELACE });
  const sink = generateEmulatorAccount({ lovelace: 0n });
  const emulator = new Emulator([payer, reserve, sink]);
  const payerLucid = await Lucid(emulator, "Custom");
  payerLucid.selectWallet.fromSeed(payer.seedPhrase);
  const reserveLucid = await Lucid(emulator, "Custom");
  reserveLucid.selectWallet.fromSeed(reserve.seedPhrase);
  const work = tempDir();
  // Kupo's view: what the last block left, blind to the mempool. The emulator
  // drops a submitted transaction's inputs at once, which Kupo never does.
  let index = new Map<string, { address: string; utxo: UTxO }>();
  const everSeen = new Set<string>();
  const reindex = async () => {
    index = new Map();
    for (const address of [payer.address, reserve.address])
      for (const utxo of await emulator.getUtxos(address)) {
        index.set(flat(utxo), { address, utxo });
        everSeen.add(flat(utxo));
      }
  };
  await reindex();
  const indexed = (address: string) =>
    [...index.values()]
      .filter((entry) => entry.address === address)
      .map((entry) => entry.utxo);
  const state = {
    built: [] as string[],
    submitted: [] as string[],
    rounds: 0,
    /** `before` dies before the bytes leave; `after` once they have. */
    kill: undefined as "before" | "after" | undefined,
    indexDown: 0,
  };
  const toChainOutput = (utxo: UTxO): ChainOutput => ({
    outRef: flat(utxo),
    lovelace: utxo.assets.lovelace ?? 0n,
    assetUnits: Object.keys(utxo.assets).filter((unit) => unit !== "lovelace")
      .length,
    datumHash: utxo.datumHash ?? null,
    scriptHash: utxo.scriptRef == null ? null : "script",
  });
  const block = async () => {
    emulator.awaitBlock(1);
    await reindex();
  };
  const deps: FloatDeps = {
    reserveAddress: reserve.address,
    unspentAt: async (address) => {
      if (state.indexDown > 0) {
        state.indexDown -= 1;
        throw new Error("Kupo is stopped");
      }
      return Promise.resolve(indexed(address).map(toChainOutput));
    },
    landed: (txId) =>
      Promise.resolve(
        emulator.transactionHistory[txId]?.status === "confirmed",
      ),
    outputState: (outRef) =>
      Promise.resolve(
        index.has(outRef)
          ? "unspent"
          : everSeen.has(outRef)
            ? "spent"
            : "unknown",
      ),
    buildPayment: async (address, lovelace, sequence) => {
      const inputs = indexed(payer.address).sort((a, b) =>
        (b.assets.lovelace ?? 0n) > (a.assets.lovelace ?? 0n) ? 1 : -1,
      );
      const input = inputs[0];
      if (input === undefined || (input.assets.lovelace ?? 0n) < lovelace)
        throw new Error(`the payer holds no output that can pay ${lovelace}`);
      const tx = await payerLucid
        .newTx()
        .collectFrom([input])
        .pay.ToAddress(address, { lovelace })
        .complete({ coinSelection: false });
      const signed = await tx.sign.withWallet().complete();
      const path = join(work, `reserve-float-${sequence}.signed`);
      writeFileSync(path, signed.toCBOR());
      state.built.push(signed.toHash());
      return { txId: signed.toHash(), signedTx: path, input: flat(input) };
    },
    submit: async (signedTx) => {
      state.submitted.push(signedTx);
      const kill = state.kill;
      state.kill = undefined;
      if (kill === "before") throw new Error("killed before submitting");
      await emulator
        .submitTx(readFileSync(signedTx, "utf8"))
        .catch(() => undefined);
      if (kill === "after") throw new Error("killed after submitting");
    },
    sleep: async (ms) => {
      if (ms === INTERVAL_MS) state.rounds += 1;
      await block();
    },
    now: () => emulator.now(),
    log: () => {},
  };
  /** A payout taking the float's lovelace, leaving `left` at the reserve. */
  const drain = async (left: bigint) => {
    const float = indexed(reserve.address).reduce((a, b) =>
      (b.assets.lovelace ?? 0n) > (a.assets.lovelace ?? 0n) ? b : a,
    );
    // The change (2 ADA less the fee) stays at the reserve, below `left`.
    const tx = await reserveLucid
      .newTx()
      .collectFrom([float])
      .pay.ToAddress(sink.address, {
        lovelace: (float.assets.lovelace ?? 0n) - left - 2n * ADA,
      })
      .pay.ToAddress(reserve.address, { lovelace: left })
      .complete({ coinSelection: false });
    await (await tx.sign.withWallet().complete()).submit();
    await block();
  };
  const reserveOutputs = () =>
    Promise.resolve(indexed(reserve.address).map(toChainOutput));
  const payerLovelace = () =>
    indexed(payer.address).reduce(
      (sum, utxo) => sum + (utxo.assets.lovelace ?? 0n),
      0n,
    );
  return { emulator, deps, state, drain, reserveOutputs, payerLovelace };
};

/** The production maintainer's shape: a fresh journal per read, one lock. */
const maintainerFor = (path: string): FloatMaintainer => {
  let held = false;
  return {
    openJournal: () => new Journal(path),
    withLock: async (step) => {
      expect(held).toBe(false);
      held = true;
      try {
        return await step();
      } finally {
        held = false;
      }
    },
  };
};

const records = (path: string) =>
  new Journal(path).withPrefix<FloatRecord>("reserve-float:");

/** Runs the maintainer until it has slept `rounds` round intervals. */
const runRounds = async (
  chain: Awaited<ReturnType<typeof emulatorChain>>,
  journalPath: string,
  rounds: number,
  lines: string[] = [],
) => {
  const abort = new AbortController();
  const target = chain.state.rounds + rounds;
  const sleep = chain.deps.sleep;
  await runReserveFloatMaintainer(
    {
      ...chain.deps,
      log: (line) => lines.push(line),
      sleep: async (ms) => {
        await sleep(ms);
        if (chain.state.rounds >= target) abort.abort();
      },
    },
    maintainerFor(journalPath),
    { intervalMs: INTERVAL_MS, signal: abort.signal },
  );
  return lines;
};

const floats = async (chain: Awaited<ReturnType<typeof emulatorChain>>) =>
  (await chain.reserveOutputs()).filter(
    (output) => output.lovelace === FLOAT_TARGET_LOVELACE,
  );

describe("reserve float maintainer on the emulator", () => {
  it("pays nothing while the float stays at or above the minimum", async () => {
    const chain = await emulatorChain();
    const journal = join(tempDir(), "journal.json");
    await chain.drain(FLOAT_MINIMUM_LOVELACE);
    expect(reserveFloatReasons(await chain.reserveOutputs())).toEqual([]);
    await runRounds(chain, journal, 5);
    expect(chain.state.built).toEqual([]);
    expect(chain.state.submitted).toEqual([]);
    expect(records(journal)).toEqual([]);
  });

  it("tops a drained float up with exactly one payment over many rounds", async () => {
    const chain = await emulatorChain();
    const journal = join(tempDir(), "journal.json");
    await chain.drain(FLOAT_MINIMUM_LOVELACE - 1n);
    expect(reserveFloatReasons(await chain.reserveOutputs())).toEqual([
      `reserve_float_below_minimum: floatLovelace=${FLOAT_MINIMUM_LOVELACE - 1n}, minimumLovelace=${FLOAT_MINIMUM_LOVELACE}`,
    ]);
    const before = chain.payerLovelace();
    const lines = await runRounds(chain, journal, 6);
    expect(chain.state.built).toHaveLength(1);
    expect(await floats(chain)).toHaveLength(1);
    expect(reserveFloatReasons(await chain.reserveOutputs())).toEqual([]);
    expect(before - chain.payerLovelace()).toBeGreaterThan(
      FLOAT_TARGET_LOVELACE,
    );
    expect(before - chain.payerLovelace()).toBeLessThan(
      FLOAT_TARGET_LOVELACE + 1n * ADA,
    );
    expect(records(journal)).toEqual([
      expect.objectContaining({
        sequence: 1,
        txId: chain.state.built[0],
        status: "confirmed",
      }),
    ]);
    expect(lines).toEqual([
      `reserve-float: paid ${FLOAT_TARGET_LOVELACE} lovelace in ${chain.state.built[0]}`,
    ]);
  });

  for (const kill of ["before", "after"] as const) {
    it(`resubmits the same journaled bytes after a kill ${kill} submission, paying once`, async () => {
      const chain = await emulatorChain();
      const journal = join(tempDir(), "journal.json");
      await chain.drain(1_000n * ADA);
      chain.state.kill = kill;
      await expect(
        maintainReserveFloatOnce(chain.deps, maintainerFor(journal)),
      ).rejects.toThrow(`killed ${kill} submitting`);
      expect(records(journal)).toEqual([
        expect.objectContaining({ status: "pending" }),
      ]);
      // The process is gone; a new one resumes from the journal alone.
      await runRounds(chain, journal, 3);
      expect(chain.state.built).toHaveLength(1);
      expect(new Set(chain.state.submitted).size).toBe(1);
      expect(chain.state.submitted).toHaveLength(2);
      expect(await floats(chain)).toHaveLength(1);
      expect(records(journal)).toEqual([
        expect.objectContaining({
          txId: chain.state.built[0],
          status: "confirmed",
        }),
      ]);
    });
  }

  it("waits out an index outage, then pays exactly once", async () => {
    const chain = await emulatorChain();
    const journal = join(tempDir(), "journal.json");
    await chain.drain(1_000n * ADA);
    chain.state.indexDown = 3;
    const lines = await runRounds(chain, journal, 6);
    expect(chain.state.built).toHaveLength(1);
    expect(await floats(chain)).toHaveLength(1);
    // One line for the standing outage, not one per failed round.
    expect(
      lines.filter((line) => line.includes("Kupo is stopped")),
    ).toHaveLength(1);
  });

  it("refuses a payer that cannot pay, journals nothing and keeps going", async () => {
    const chain = await emulatorChain();
    const journal = join(tempDir(), "journal.json");
    await chain.drain(1_000n * ADA);
    const build = chain.deps.buildPayment;
    let attempts = 0;
    chain.deps = {
      ...chain.deps,
      buildPayment: (...args) => {
        attempts += 1;
        return attempts <= 3
          ? Promise.reject(new Error("the payer holds no output"))
          : build(...args);
      },
    } as FloatDeps;
    const lines = await runRounds(chain, journal, 2);
    expect(attempts).toBe(2);
    expect(chain.state.submitted).toEqual([]);
    expect(records(journal)).toEqual([]);
    expect(reserveFloatReasons(await chain.reserveOutputs())).toHaveLength(1);
    expect(lines).toEqual([
      "reserve-float: round failed: the payer holds no output; retrying next round",
    ]);
    // Once the payer can pay again, the next rounds pay exactly once.
    await runRounds(chain, journal, 3);
    expect(chain.state.built).toHaveLength(1);
    expect(await floats(chain)).toHaveLength(1);
  });

  it("reads the journal after taking the lock, so it never reuses another holder's sequence", async () => {
    const chain = await emulatorChain();
    const journal = join(tempDir(), "journal.json");
    await chain.drain(1_000n * ADA);
    // While this round waits for the lock, the up step holds it: it tops the
    // float up, journaling sequence 1, and a payout drains the float again.
    const upHoldsTheLock: FloatMaintainer = {
      openJournal: () => new Journal(journal),
      withLock: async (step) => {
        await ensureReserveFloat(chain.deps, new Journal(journal));
        await chain.drain(1_000n * ADA);
        return step();
      },
    };
    await maintainReserveFloatOnce(chain.deps, upHoldsTheLock);
    expect(records(journal).map((r) => [r.sequence, r.txId, r.status])).toEqual(
      [
        [1, chain.state.built[0], "confirmed"],
        [2, chain.state.built[1], "confirmed"],
      ],
    );
    expect(chain.state.built).toHaveLength(2);
    expect(new Set(chain.state.built).size).toBe(2);
  });
});

describe("withFloatLock", () => {
  const layoutAt = (state: string) => ({ state }) as Layout;

  it("waits for a live holder, then runs its step once", async () => {
    const state = tempDir();
    const holder = spawn(process.execPath, [
      "-e",
      "setTimeout(() => {}, 60000)",
    ]);
    try {
      writeFileSync(join(state, "reserve-float.lock"), String(holder.pid));
      let polls = 0;
      let steps = 0;
      const result = await withFloatLock(
        layoutAt(state),
        {
          sleep: async () => {
            polls += 1;
            if (polls === 2) rmSync(join(state, "reserve-float.lock"));
            await Promise.resolve();
          },
          now: Date.now,
        },
        () => Promise.resolve((steps += 1)),
      );
      expect(polls).toBe(2);
      expect(steps).toBe(1);
      expect(result).toBe(1);
    } finally {
      holder.kill();
    }
  });

  it("takes over a dead holder's lock without waiting", async () => {
    const state = tempDir();
    writeFileSync(join(state, "reserve-float.lock"), "999999999");
    let polls = 0;
    await withFloatLock(
      layoutAt(state),
      { sleep: () => Promise.resolve(void (polls += 1)), now: Date.now },
      () => Promise.resolve(),
    );
    expect(polls).toBe(0);
  });

  it("refuses at once when the lock cannot be written at all", async () => {
    await expect(
      withFloatLock(
        layoutAt(join(tempDir(), "missing")),
        { sleep: () => Promise.resolve(), now: Date.now },
        () => Promise.resolve(),
      ),
    ).rejects.toThrow(/ENOENT/);
  });
});
