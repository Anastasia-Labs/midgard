/**
 * The settlement worker runs under a fixed old-generation limit. On the lc1
 * devnet every run died there: resolving a payout's reference scripts read
 * and decoded all 945 scripts of the reference-script wallet (~410 MB of JS
 * heap) to keep a handful. These tests drive that resolution through a Kupo
 * endpoint shaped like that wallet, in a worker thread under the real limit.
 */
import { randomBytes } from "node:crypto";
import { mkdtemp, rm } from "node:fs/promises";
import { createServer } from "node:http";
import type { AddressInfo } from "node:net";
import { join } from "node:path";
import { Worker } from "node:worker_threads";

import * as SDK from "@al-ft/midgard-sdk";
import {
  applyDoubleCborEncoding,
  credentialToAddress,
  type LucidEvolution,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { build as bundleWithTsup } from "tsup";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { SETTLEMENT_WORKER_HEAP_MB } from "../src/fibers/settlement.js";
import { REFERENCE_SCRIPT_PER_TARGET_READ_LIMIT } from "../src/transactions/reference-scripts.fetch-reference-script-utxos-program.js";
import { fetchReferenceScriptUtxosProgram } from "../src/transactions/reference-scripts.js";
import type {
  SettlementHeapProbeInput,
  SettlementHeapProbeResult,
} from "./helpers/settlement-worker-heap-probe.js";

const packageRoot = join(import.meta.dirname, "..");
/** The lc1 wallet: 945 UTxOs, 940 of them carrying a ~9 KB script. */
const WALLET_UTXOS = 945;
const SCRIPT_BYTES = 9_000;
/** What the settlement worker resolves for an absorb and a payout. */
const SETTLEMENT_TARGETS = [
  "deposit spending",
  "deposit history retirement",
  "withdrawal spending",
  "payout minting",
  "payout spending",
  "reserve spending",
];

const hex = (bytes: number) => randomBytes(bytes).toString("hex");
/** Kupo's single-CBOR encoding of a flat script of `bytes` bytes. */
const kupoScript = (bytes: number) =>
  `59${bytes.toString(16).padStart(4, "0")}0101${hex(bytes - 2)}`;
const lucidScript = (script: string): Script => ({
  type: "PlutusV3",
  script: applyDoubleCborEncoding(script),
});

const policyId = hex(28);
const address = credentialToAddress("Preprod", { type: "Key", hash: hex(28) });
const kupoUnit = (name: string) => {
  const unit = SDK.referenceScriptAuthUnit(policyId, name);
  return `${unit.slice(0, 56)}.${unit.slice(56)}`;
};
const kupoOutput = (index: number, script: string | null, unit?: string) => ({
  transaction_index: 0,
  transaction_id: index.toString(16).padStart(64, "0"),
  output_index: 0,
  address,
  value: {
    coins: 40_000_000,
    assets: unit === undefined ? {} : { [unit]: 1 },
  },
  datum_hash: null,
  datum: null,
  script_hash: script === null ? null : hex(28),
  script: script === null ? null : { language: "plutus:v3", script },
  created_at: { slot_no: index, header_hash: hex(32) },
  spent_at: null,
});

const targetScripts = new Map(
  SETTLEMENT_TARGETS.map((name) => [name, kupoScript(SCRIPT_BYTES)]),
);
const wallet = Array.from({ length: WALLET_UTXOS }, (_, index) => {
  if (index < 5) return kupoOutput(index, null);
  const name = SETTLEMENT_TARGETS[index - 5];
  // Every other script holds some other role token under the same policy.
  return name === undefined
    ? kupoOutput(index, kupoScript(SCRIPT_BYTES), `${policyId}.${hex(12)}`)
    : kupoOutput(index, targetScripts.get(name)!, kupoUnit(name));
});
const expectedOutRef = (name: string) =>
  `${(SETTLEMENT_TARGETS.indexOf(name) + 5).toString(16).padStart(64, "0")}#0`;

/** Kupo's `/matches` with the `policy_id`/`asset_name` filters it serves. */
const serveKupo = async () => {
  const requests: URL[] = [];
  const server = createServer((request, response) => {
    const url = new URL(request.url ?? "/", "http://kupo");
    requests.push(url);
    const policy = url.searchParams.get("policy_id");
    const asset = url.searchParams.get("asset_name");
    const matches = wallet.filter(
      (output) =>
        policy === null ||
        Object.keys(output.value.assets).some(
          (unit) =>
            unit.startsWith(`${policy}.`) &&
            (asset === null || unit === `${policy}.${asset}`),
        ),
    );
    response.setHeader("content-type", "application/json");
    response.end(JSON.stringify(matches));
  });
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  const { port } = server.address() as AddressInfo;
  return { server, requests, url: `http://127.0.0.1:${port}` };
};

describe("settlement reference-script resolution under the worker heap limit", () => {
  let kupo: Awaited<ReturnType<typeof serveKupo>>;
  let bundleDir: string;
  beforeAll(async () => {
    kupo = await serveKupo();
    bundleDir = await mkdtemp(join(packageRoot, ".settlement-heap-"));
    // A worker thread resolves workspace packages through their dist; bundle
    // them from source, as tests/da-multi-process-10k-integration.test.ts does.
    await bundleWithTsup({
      entry: [
        join(packageRoot, "tests/helpers/settlement-worker-heap-probe.ts"),
      ],
      format: ["esm"],
      platform: "node",
      target: "node22",
      outDir: bundleDir,
      config: false,
      splitting: false,
      silent: true,
      noExternal: [/^@al-ft\//],
      loader: { ".sql": "text" },
      banner: {
        js: 'import { createRequire as __createRequire } from "node:module"; const require = __createRequire(import.meta.url);',
      },
      esbuildOptions(options) {
        options.conditions = ["midgard-source", ...(options.conditions ?? [])];
      },
    });
  }, 300_000);
  afterAll(async () => {
    await new Promise((resolve) => kupo?.server.close(resolve));
    if (bundleDir !== undefined)
      await rm(bundleDir, { recursive: true, force: true });
  });

  it("resolves every settlement target within the worker's heap limit", async () => {
    const input: SettlementHeapProbeInput = {
      kupoUrl: kupo.url,
      address,
      policyId,
      targets: SETTLEMENT_TARGETS.map((name) => ({
        name,
        script: lucidScript(targetScripts.get(name)!),
      })),
    };
    const worker = new Worker(
      join(bundleDir, "settlement-worker-heap-probe.js"),
      {
        workerData: input,
        resourceLimits: { maxOldGenerationSizeMb: SETTLEMENT_WORKER_HEAP_MB },
      },
    );
    let result: SettlementHeapProbeResult;
    try {
      result = await new Promise<SettlementHeapProbeResult>(
        (resolve, reject) => {
          worker.once("message", resolve);
          worker.once("error", reject);
          worker.once("exit", (code) =>
            reject(new Error(`probe exited (${code}) without a result`)),
          );
        },
      );
    } finally {
      await worker.terminate();
    }
    expect(result.resolved).toEqual(
      SETTLEMENT_TARGETS.map((name) => ({
        name,
        outRef: expectedOutRef(name),
      })),
    );
    expect(result.usedHeapMb).toBeLessThan(SETTLEMENT_WORKER_HEAP_MB);
    // Each target is read through its own role token, never the whole wallet.
    const matches = kupo.requests.filter((url) =>
      url.pathname.startsWith("/matches/"),
    );
    expect(matches).toHaveLength(SETTLEMENT_TARGETS.length);
    for (const url of matches) {
      expect(url.searchParams.get("policy_id")).toBe(policyId);
      expect(url.searchParams.get("asset_name")).not.toBeNull();
    }
  }, 300_000);
});

describe("role-token resolution keeps refusing a UTxO without the target's script", () => {
  const target = {
    name: "payout spending",
    script: lucidScript(kupoScript(64)),
  };
  const utxo = (txHash: string, script: Script): UTxO => ({
    txHash,
    outputIndex: 0,
    address,
    assets: {
      lovelace: 40_000_000n,
      [SDK.referenceScriptAuthUnit(policyId, target.name)]: 1n,
    },
    scriptRef: script,
  });
  const imposter = utxo("aa".repeat(32), lucidScript(kupoScript(64)));
  const genuine = utxo("bb".repeat(32), target.script);
  const resolve = (holders: readonly UTxO[]) => {
    const units: string[] = [];
    const lucid = {
      utxosAtWithUnit: async (at: string, unit: string) => {
        units.push(unit);
        return at === address ? [...holders] : [];
      },
    } as unknown as LucidEvolution;
    return Effect.runPromise(
      Effect.either(
        fetchReferenceScriptUtxosProgram(lucid, address, [target], {
          policyId,
        }),
      ),
    ).then((result) => ({ result, units }));
  };

  it("picks the role-token holder carrying the target's script", async () => {
    const { result, units } = await resolve([imposter, genuine]);
    expect(units).toEqual([SDK.referenceScriptAuthUnit(policyId, target.name)]);
    expect(result._tag).toBe("Right");
    if (result._tag === "Right") expect(result.right[0]!.utxo).toBe(genuine);
  });

  it("refuses a role-token holder with another script", async () => {
    const { result } = await resolve([imposter]);
    expect(result._tag).toBe("Left");
    if (result._tag === "Left") {
      expect(result.left.message).toBe("Missing reference script");
      expect(String(result.left.cause)).toContain(target.name);
    }
  });
});

describe("reference-script resolution reads the wallet once for a large target set", () => {
  const targets = (count: number) =>
    Object.keys(SDK.REFERENCE_SCRIPT_AUTH_TOKEN_NAMES)
      .slice(0, count)
      .map((name) => ({ name, script: lucidScript(kupoScript(64)) }));
  const holder = (target: { name: string; script: Script }, index: number) =>
    ({
      txHash: index.toString(16).padStart(64, "0"),
      outputIndex: 0,
      address,
      assets: {
        lovelace: 40_000_000n,
        [SDK.referenceScriptAuthUnit(policyId, target.name)]: 1n,
      },
      scriptRef: target.script,
    }) satisfies UTxO;
  const resolve = async (count: number) => {
    const wanted = targets(count);
    const wallet = wanted.map(holder);
    const reads = { wallet: 0, unit: 0 };
    const lucid = {
      utxosAt: async () => {
        reads.wallet++;
        return wallet;
      },
      utxosAtWithUnit: async (_: string, unit: string) => {
        reads.unit++;
        return wallet.filter((utxo) => utxo.assets[unit] !== undefined);
      },
    } as unknown as LucidEvolution;
    const resolved = await Effect.runPromise(
      fetchReferenceScriptUtxosProgram(lucid, address, wanted, { policyId }),
    );
    expect(resolved.map(({ utxo }) => utxo)).toEqual(wallet);
    return reads;
  };

  it("reads each role token for a set up to the limit", async () => {
    expect(await resolve(REFERENCE_SCRIPT_PER_TARGET_READ_LIMIT)).toEqual({
      wallet: 0,
      unit: REFERENCE_SCRIPT_PER_TARGET_READ_LIMIT,
    });
  });

  it("reads the wallet once for a set above it", async () => {
    expect(await resolve(REFERENCE_SCRIPT_PER_TARGET_READ_LIMIT + 1)).toEqual({
      wallet: 1,
      unit: 0,
    });
  });
});
