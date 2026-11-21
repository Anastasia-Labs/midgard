import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import { Journal } from "../src/devnet-stack/journal.js";
import {
  type ChainOutput,
  ensureReserveFloat,
  ensureReserveFloatRetrying,
  FLOAT_MINIMUM_LOVELACE,
  FLOAT_STEP_TIMEOUT_MS,
  FLOAT_TARGET_LOVELACE,
  type FloatDeps,
  type FloatRecord,
  type SignedPayment,
} from "../src/devnet-stack/reserve-float.js";
import { reserveAddressFromManifest } from "../src/devnet-stack/reserve-float-chain.js";

const ADA = 1_000_000n;
const RESERVE = "addr_test1reserve";
const PAYER = "addr_test1payer";

const dirs: string[] = [];
const journalPath = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-stack-float-"));
  dirs.push(dir);
  return join(dir, "journal.json");
};
afterEach(() => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const output = (
  outRef: string,
  lovelace: bigint,
  extra: Partial<ChainOutput> = {},
): ChainOutput => ({
  outRef,
  lovelace,
  assetUnits: 0,
  datumHash: null,
  scriptHash: null,
  ...extra,
});

type Entry = { address: string; output: ChainOutput; spent: boolean };

/**
 * A ledger and its index in one: a submitted payment applies once, spending
 * its input and creating the reserve output, exactly when its input is
 * unspent. `crash` makes the next submit fail before or after applying.
 */
class FakeChain {
  readonly entries: Entry[] = [];
  readonly built: SignedPayment[] = [];
  readonly submitted: string[] = [];
  crash: "before" | "after" | undefined;
  onSubmit: (signedTx: string) => void = () => {};

  add(address: string, value: ChainOutput) {
    this.entries.push({ address, output: value, spent: false });
  }

  floats() {
    return this.entries.filter(
      (e) => e.address === RESERVE && e.output.outRef.startsWith("float-"),
    );
  }

  deps(): FloatDeps {
    return {
      reserveAddress: RESERVE,
      unspentAt: (address) =>
        Promise.resolve(
          this.entries
            .filter((e) => e.address === address && !e.spent)
            .map((e) => e.output),
        ),
      landed: (txId) =>
        Promise.resolve(
          this.entries.some((e) => e.output.outRef.startsWith(`${txId}#`)),
        ),
      outputState: (outRef) => {
        const entry = this.entries.find((e) => e.output.outRef === outRef);
        return Promise.resolve(
          entry === undefined ? "unknown" : entry.spent ? "spent" : "unspent",
        );
      },
      buildPayment: (_address, _lovelace, sequence) => {
        const payer = this.entries.find((e) => e.address === PAYER && !e.spent);
        if (payer === undefined) throw new Error("no payer output");
        const payment = {
          txId: `float-${sequence}`,
          signedTx: `/work/reserve-float-${sequence}.signed`,
          input: payer.output.outRef,
        };
        this.built.push(payment);
        return Promise.resolve(payment);
      },
      submit: (signedTx) => {
        this.onSubmit(signedTx);
        this.submitted.push(signedTx);
        const crash = this.crash;
        this.crash = undefined;
        if (crash === "before")
          return Promise.reject(new Error("killed before submitting"));
        const payment = this.built.find((p) => p.signedTx === signedTx)!;
        const input = this.entries.find(
          (e) => e.output.outRef === payment.input,
        )!;
        if (
          !input.spent &&
          !this.entries.some((e) =>
            e.output.outRef.startsWith(`${payment.txId}#`),
          )
        ) {
          input.spent = true;
          this.add(RESERVE, output(`${payment.txId}#0`, FLOAT_TARGET_LOVELACE));
          this.add(
            PAYER,
            output(
              `${payment.txId}#1`,
              input.output.lovelace - FLOAT_TARGET_LOVELACE,
            ),
          );
        }
        return crash === "after"
          ? Promise.reject(new Error("killed after submitting"))
          : Promise.resolve();
      },
      sleep: () => Promise.resolve(),
      now: () => 0,
      log: () => {},
    };
  }
}

const withPayer = () => {
  const chain = new FakeChain();
  chain.add(PAYER, output("genesis#0", 1_000_000n * ADA));
  return chain;
};

const records = (path: string) =>
  new Journal(path).withPrefix<FloatRecord>("reserve-float:");

describe("ensureReserveFloat", () => {
  it("pays nothing when a pure-ADA float of the minimum already sits at the reserve", async () => {
    const chain = withPayer();
    chain.add(RESERVE, output("deposit#0", FLOAT_MINIMUM_LOVELACE));
    const outcome = await ensureReserveFloat(
      chain.deps(),
      new Journal(journalPath()),
    );
    expect(outcome).toMatchObject({
      action: "sufficient",
      float: { outRef: "deposit#0" },
    });
    expect(chain.built).toEqual([]);
    expect(chain.submitted).toEqual([]);
  });

  it("never counts token-bearing, datum or script-ref outputs, nor a float below the minimum", async () => {
    const chain = withPayer();
    const big = 50_000n * ADA;
    chain.add(RESERVE, output("tokens#0", big, { assetUnits: 1 }));
    chain.add(RESERVE, output("inline#0", big, { datumHash: "d".repeat(64) }));
    chain.add(RESERVE, output("script#0", big, { scriptHash: "s".repeat(56) }));
    chain.add(RESERVE, output("small#0", FLOAT_MINIMUM_LOVELACE - 1n));
    const path = journalPath();
    const journal = new Journal(path);
    // The intent is on disk before the bytes leave.
    chain.onSubmit = (signedTx) =>
      expect(records(path)).toEqual([
        expect.objectContaining({ signedTx, status: "pending" }),
      ]);
    const outcome = await ensureReserveFloat(chain.deps(), journal);
    expect(outcome).toEqual({
      action: "topped-up",
      txId: "float-1",
      lovelace: FLOAT_TARGET_LOVELACE,
    });
    expect(chain.submitted).toEqual(["/work/reserve-float-1.signed"]);
    expect(records(path)).toEqual([
      expect.objectContaining({
        sequence: 1,
        txId: "float-1",
        input: "genesis#0",
        status: "confirmed",
      }),
    ]);
    expect(chain.floats()).toHaveLength(1);
  });

  it("recognises a payment that landed before a crash and does not pay twice", async () => {
    const chain = withPayer();
    const path = journalPath();
    chain.crash = "after";
    await expect(
      ensureReserveFloat(chain.deps(), new Journal(path)),
    ).rejects.toThrow(/after submitting/);
    expect(records(path)).toEqual([
      expect.objectContaining({ status: "pending" }),
    ]);

    const outcome = await ensureReserveFloat(chain.deps(), new Journal(path));
    expect(outcome).toMatchObject({
      action: "sufficient",
      float: { outRef: "float-1#0" },
    });
    expect(chain.built).toHaveLength(1);
    expect(chain.submitted).toHaveLength(1);
    expect(chain.floats()).toHaveLength(1);
    expect(records(path)).toEqual([
      expect.objectContaining({ txId: "float-1", status: "confirmed" }),
    ]);
  });

  it("resubmits the journaled bytes when a crash came before they left", async () => {
    const chain = withPayer();
    const path = journalPath();
    chain.crash = "before";
    await expect(
      ensureReserveFloat(chain.deps(), new Journal(path)),
    ).rejects.toThrow(/before submitting/);
    expect(chain.floats()).toHaveLength(0);

    const outcome = await ensureReserveFloat(chain.deps(), new Journal(path));
    expect(outcome).toMatchObject({
      action: "sufficient",
      float: { outRef: "float-1#0" },
    });
    expect(chain.built).toHaveLength(1);
    expect(chain.submitted).toEqual([
      "/work/reserve-float-1.signed",
      "/work/reserve-float-1.signed",
    ]);
    expect(chain.floats()).toHaveLength(1);
  });

  it("abandons a journaled payment whose input another transaction spent, then pays anew", async () => {
    const chain = withPayer();
    chain.add(PAYER, output("genesis#1", 1_000_000n * ADA));
    const path = journalPath();
    chain.crash = "before";
    await expect(
      ensureReserveFloat(chain.deps(), new Journal(path)),
    ).rejects.toThrow();
    chain.entries.find((e) => e.output.outRef === "genesis#0")!.spent = true;

    const outcome = await ensureReserveFloat(chain.deps(), new Journal(path));
    expect(outcome).toMatchObject({ action: "topped-up", txId: "float-2" });
    expect(records(path).map((r) => [r.txId, r.status])).toEqual([
      ["float-1", "abandoned"],
      ["float-2", "confirmed"],
    ]);
    expect(chain.floats().map((e) => e.output.outRef)).toEqual(["float-2#0"]);
  });
});

describe("ensureReserveFloatRetrying", () => {
  /** The chain's deps on a clock that only sleeping advances. */
  const clocked = (chain: FakeChain) => {
    let clock = 0;
    return {
      ...chain.deps(),
      now: () => clock,
      sleep: (ms: number) => {
        clock += ms;
        return Promise.resolve();
      },
    };
  };

  it("rides out an index outage after submission and pays once", async () => {
    const chain = withPayer();
    const path = journalPath();
    const deps = clocked(chain);
    // Kupo stops right after the payment leaves (a drill), then returns.
    let down = 0;
    chain.onSubmit = () => (down = 3);
    const landed = deps.landed;
    deps.landed = (txId) =>
      down-- > 0 ? Promise.reject(new Error("Kupo is stopped")) : landed(txId);

    const outcome = await ensureReserveFloatRetrying(deps, new Journal(path));
    expect(outcome).toMatchObject({
      action: "sufficient",
      float: { outRef: "float-1#0" },
    });
    expect(chain.built).toHaveLength(1);
    expect(chain.floats()).toHaveLength(1);
    expect(records(path)).toEqual([
      expect.objectContaining({ txId: "float-1", status: "confirmed" }),
    ]);
  });

  it("gives up once the step has failed for its whole timeout", async () => {
    const chain = withPayer();
    const deps = clocked(chain);
    deps.unspentAt = () => Promise.reject(new Error("Kupo is stopped"));
    await expect(
      ensureReserveFloatRetrying(deps, new Journal(journalPath())),
    ).rejects.toThrow(/stopped/);
    expect(deps.now()).toBeGreaterThanOrEqual(FLOAT_STEP_TIMEOUT_MS);
    expect(chain.built).toEqual([]);
  });
});

describe("reserveAddressFromManifest", () => {
  const SCRIPT = "5001010023259800b452689b2b20025735";
  const HASH = "22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab";
  const manifest = (scriptHash: string) => {
    const path = join(
      mkdtempSync(join(tmpdir(), "devnet-stack-float-")),
      "manifest.json",
    );
    dirs.push(join(path, ".."));
    writeFileSync(
      path,
      JSON.stringify({
        contracts: {
          reserveSpend: {
            scriptHash,
            contract: { type: "PlutusV3", cborHex: SCRIPT },
          },
        },
      }),
    );
    return path;
  };

  it("is the enterprise address of the manifest's reserve spending script", () => {
    expect(reserveAddressFromManifest(manifest(HASH))).toBe(
      "addr_test1wq3vnggra5ljl2tunqkhd4hz4agvt422cvrfswcedj8um2cwsu3l3",
    );
  });

  it("refuses a manifest whose recorded hash is not its script's", () => {
    expect(() => reserveAddressFromManifest(manifest("00".repeat(28)))).toThrow(
      /hashes to/,
    );
  });
});
