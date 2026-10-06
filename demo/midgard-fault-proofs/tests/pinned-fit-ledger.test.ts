import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import {
  readBlueprintIdentity,
  writeOrVerifyPinnedFitLedger,
} from "./support/pinned-fit-ledger.js";

const rows: VanRossemFitMeasurement[] = [
  {
    name: "accepted/0",
    kind: "publication",
    maximumShape: "accepted",
    signedBytes: 9_000,
    memoryUnits: 0n,
    cpuUnits: 0n,
  },
  {
    name: "accepted/1",
    kind: "lifecycle",
    maximumShape: "accepted",
    signedBytes: 12_000,
    memoryUnits: 9_000_000n,
    cpuUnits: 3_000_000_000n,
  },
];

const ledgerOf = (
  measurements: readonly VanRossemFitMeasurement[],
  blueprintSha256 = "ab".repeat(32),
  {
    category = "transitionTrace",
    compilerVersion = "aiken v1.1.23+5adf783",
  }: { category?: string; compilerVersion?: string } = {},
) =>
  buildVanRossemFitLedger({
    category,
    blueprintSha256,
    compilerVersion,
    measurements,
  });

describe("writeOrVerifyPinnedFitLedger", () => {
  let directory: string;
  let path: string;
  beforeEach(async () => {
    directory = await mkdtemp(join(tmpdir(), "midgard-pinned-fit-"));
    path = join(directory, "pinned-fit-ledger.json");
    await writeFile(path, `${JSON.stringify(ledgerOf(rows), null, 2)}\n`);
  });
  afterEach(async () => {
    vi.unstubAllEnvs();
    await rm(directory, { recursive: true, force: true });
  });

  it("accepts a fresh run whose budgets differ and leaves the file alone", async () => {
    vi.stubEnv("MIDGARD_WRITE_FIT_LEDGER", undefined);
    const before = await readFile(path, "utf8");
    const fresh = ledgerOf(
      rows.map((row) => ({ ...row, cpuUnits: row.cpuUnits + 17n })),
    );
    await writeOrVerifyPinnedFitLedger(path, fresh);
    expect(await readFile(path, "utf8")).toBe(before);
  });

  it("refuses a saved ledger measured against another blueprint", async () => {
    vi.stubEnv("MIDGARD_WRITE_FIT_LEDGER", undefined);
    await expect(
      writeOrVerifyPinnedFitLedger(path, ledgerOf(rows, "cd".repeat(32))),
    ).rejects.toThrow(
      "regenerate it with `pnpm --dir demo/midgard-fault-proofs fit:regenerate pinned-fit-ledger.json`",
    );
  });

  it("refuses a saved ledger recorded under another category", async () => {
    vi.stubEnv("MIDGARD_WRITE_FIT_LEDGER", undefined);
    await expect(
      writeOrVerifyPinnedFitLedger(
        path,
        ledgerOf(rows, undefined, { category: "withdrawalMistag" }),
      ),
    ).rejects.toThrow(
      "regenerate it with `pnpm --dir demo/midgard-fault-proofs fit:regenerate pinned-fit-ledger.json`",
    );
  });

  it("refuses a saved ledger recorded under another compiler", async () => {
    vi.stubEnv("MIDGARD_WRITE_FIT_LEDGER", undefined);
    await expect(
      writeOrVerifyPinnedFitLedger(
        path,
        ledgerOf(rows, undefined, { compilerVersion: "aiken v1.1.24+0000000" }),
      ),
    ).rejects.toThrow(
      "regenerate it with `pnpm --dir demo/midgard-fault-proofs fit:regenerate pinned-fit-ledger.json`",
    );
  });

  it("refuses a saved ledger whose row roster differs from the fresh run", async () => {
    vi.stubEnv("MIDGARD_WRITE_FIT_LEDGER", undefined);
    await expect(
      writeOrVerifyPinnedFitLedger(
        path,
        ledgerOf([...rows, { ...rows[1]!, name: "accepted/2" }]),
      ),
    ).rejects.toThrow(
      "regenerate it with `pnpm --dir demo/midgard-fault-proofs fit:regenerate pinned-fit-ledger.json`",
    );
  });

  it("refuses a saved ledger whose digest does not match its body", async () => {
    vi.stubEnv("MIDGARD_WRITE_FIT_LEDGER", undefined);
    await writeFile(
      path,
      `${JSON.stringify({ ...ledgerOf(rows), ledgerSha256: "00".repeat(32) }, null, 2)}\n`,
    );
    await expect(
      writeOrVerifyPinnedFitLedger(path, ledgerOf(rows)),
    ).rejects.toThrow(
      "regenerate it with `pnpm --dir demo/midgard-fault-proofs fit:regenerate pinned-fit-ledger.json`",
    );
  });

  it("replaces the saved ledger only under MIDGARD_WRITE_FIT_LEDGER=1", async () => {
    vi.stubEnv("MIDGARD_WRITE_FIT_LEDGER", "1");
    const fresh = ledgerOf(rows, "cd".repeat(32));
    await writeOrVerifyPinnedFitLedger(path, fresh);
    expect(JSON.parse(await readFile(path, "utf8"))).toStrictEqual(fresh);
  });
});

describe("readBlueprintIdentity", () => {
  it("names the blueprint's digest and the compiler its preamble records", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-blueprint-id-"));
    try {
      const path = join(directory, "plutus.json");
      const bytes = JSON.stringify({
        preamble: { compiler: { name: "Aiken", version: "v9.9.9+abcdef0" } },
      });
      await writeFile(path, bytes);
      expect(await readBlueprintIdentity(path)).toStrictEqual({
        blueprintSha256: createHash("sha256").update(bytes).digest("hex"),
        compilerVersion: "aiken v9.9.9+abcdef0",
      });
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });
});
