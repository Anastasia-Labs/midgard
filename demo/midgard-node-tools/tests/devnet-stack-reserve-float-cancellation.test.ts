import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, expect, it, vi } from "vitest";

import { Journal } from "../src/devnet-stack/journal.js";
import type { Layout, RunEnv } from "../src/devnet-stack/layout.js";
import { acquireLock, ControllerLockBusy } from "../src/devnet-stack/lock.js";
import {
  FLOAT_TARGET_LOVELACE,
  type FloatRecord,
} from "../src/devnet-stack/reserve-float.js";
import { provisionReserveFloat } from "../src/devnet-stack/reserve-float-chain.js";

const transport = vi.hoisted(() => ({
  labels: [] as string[],
  abortAt: "",
  abort: undefined as AbortController | undefined,
  submit: undefined as (() => Promise<void>) | undefined,
}));
vi.mock("../src/devnet-stack/chain.js", async (load) => {
  const original = await load<typeof import("../src/devnet-stack/chain.js")>();
  return {
    ...original,
    cardanoCli: async (
      _layout: unknown,
      _run: unknown,
      _args: unknown,
      label: string,
    ) => {
      transport.labels.push(label);
      if (label === transport.abortAt) transport.abort?.abort();
      if (label === "reserve-float-submit") await transport.submit?.();
      return {
        code: 0,
        stdout: label === "reserve-float-payer" ? "payer" : "aa".repeat(32),
        stderr: "",
        log: "fixture",
      };
    },
  };
});
const dirs: string[] = [];
afterEach(() => {
  vi.unstubAllGlobals();
  transport.labels = [];
  transport.abortAt = "";
  transport.abort = undefined;
  transport.submit = undefined;
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});
const setup = () => {
  const state = mkdtempSync(join(tmpdir(), "acceptance-float-cancel-"));
  dirs.push(state);
  const layout = {
    state,
    runDir: state,
    journal: join(state, "journal.json"),
    contractManifest: join(state, "manifest.json"),
  } as Layout;
  writeFileSync(
    layout.contractManifest,
    JSON.stringify({
      contracts: {
        reserveSpend: {
          scriptHash:
            "22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab",
          contract: {
            type: "PlutusV3",
            cborHex: "5001010023259800b452689b2b20025735",
          },
        },
      },
    }),
  );
  vi.stubGlobal("fetch", async (url: string) => ({
    ok: true,
    json: async () =>
      url.includes("payer?")
        ? [
            {
              transaction_id: "bb".repeat(32),
              output_index: 0,
              value: { coins: "1000000000000" },
              datum_hash: null,
              script_hash: null,
              spent_at: null,
            },
          ]
        : url.includes("?unspent")
          ? []
          : [{}],
  }));
  const abort = new AbortController();
  transport.abort = abort;
  return {
    layout,
    abort,
    run: {} as RunEnv,
    records: () =>
      new Journal(layout.journal).withPrefix<FloatRecord>("reserve-float:"),
  };
};
it("keeps the normal production build/sign/submit sequence and confirmed receipt without a signal", async () => {
  const test = setup();
  await expect(
    provisionReserveFloat(
      test.layout,
      test.run,
      new Journal(test.layout.journal),
    ),
  ).resolves.toMatchObject({
    action: "topped-up",
    lovelace: FLOAT_TARGET_LOVELACE,
  });
  expect(transport.labels).toEqual([
    "reserve-float-payer",
    "reserve-float-build",
    "reserve-float-sign",
    "reserve-float-txid",
    "reserve-float-submit",
  ]);
  expect(test.records()).toMatchObject([{ status: "confirmed" }]);
});
it.each(["reserve-float-payer", "reserve-float-build"])(
  "refuses later build/sign/submit after cancellation during %s",
  async (label) => {
    const test = setup();
    transport.abortAt = label;
    await expect(
      provisionReserveFloat(
        test.layout,
        test.run,
        new Journal(test.layout.journal),
        undefined,
        test.abort.signal,
      ),
    ).rejects.toThrow();
    expect(transport.labels).not.toContain("reserve-float-sign");
    expect(transport.labels).not.toContain("reserve-float-submit");
    expect(test.records()).toEqual([]);
  },
);
it("persists signing completed during cancellation and resumes those exact bytes without rebuilding", async () => {
  const test = setup();
  transport.abortAt = "reserve-float-sign";
  await expect(
    provisionReserveFloat(
      test.layout,
      test.run,
      new Journal(test.layout.journal),
      undefined,
      test.abort.signal,
    ),
  ).rejects.toThrow(/cancelled/);
  expect(transport.labels).toEqual([
    "reserve-float-payer",
    "reserve-float-build",
    "reserve-float-sign",
    "reserve-float-txid",
  ]);
  const [intent] = test.records();
  expect(intent).toMatchObject({
    status: "pending",
    txId: "aa".repeat(32),
    signedTx: join(test.layout.state, "work/reserve-float-1.signed"),
  });
  transport.labels = [];
  transport.abortAt = "";
  let landed = false;
  const submissions: string[] = [];
  await provisionReserveFloat(
    test.layout,
    test.run,
    new Journal(test.layout.journal),
    {
      reserveAddress: intent!.reserveAddress,
      outputState: async () => "unspent",
      landed: async () => landed,
      submit: async (bytes) => {
        submissions.push(bytes);
        landed = true;
      },
      buildPayment: async () => {
        throw new Error("must resume exact intent");
      },
      unspentAt: async () => [
        {
          outRef: intent!.txId + "#0",
          lovelace: FLOAT_TARGET_LOVELACE,
          assetUnits: 0,
          datumHash: null,
          scriptHash: null,
        },
      ],
      sleep: async () => {},
      now: Date.now,
      log: () => {},
    },
  );
  expect(submissions).toEqual([intent!.signedTx]);
  expect(test.records()).toMatchObject([
    { status: "confirmed", signedTx: intent!.signedTx },
  ]);
});
it("joins an in-flight submit on abort and preserves its unknown pending outcome", async () => {
  const test = setup();
  let release!: () => void;
  const gate = new Promise<void>((resolve) => {
    release = resolve;
  });
  transport.submit = async () => {
    test.abort.abort();
    await gate;
  };
  let settled = false;
  const task = provisionReserveFloat(
    test.layout,
    test.run,
    new Journal(test.layout.journal),
    undefined,
    test.abort.signal,
  ).finally(() => {
    settled = true;
  });
  const refusal = expect(task).rejects.toThrow(/cancelled/);
  for (
    let i = 0;
    i < 100 && !transport.labels.includes("reserve-float-submit");
    i++
  )
    await Promise.resolve();
  expect(transport.labels).toContain("reserve-float-submit");
  await new Promise<void>((resolve) => setImmediate(resolve));
  try {
    expect(settled).toBe(false);
    expect(() => {
      const releaseUnexpected = acquireLock(
        join(test.layout.state, "reserve-float.lock"),
      );
      releaseUnexpected();
    }).toThrow(ControllerLockBusy);
    expect(test.records()).toMatchObject([{ status: "pending" }]);
  } finally {
    release();
    await refusal;
  }
  expect(test.records()).toMatchObject([{ status: "pending" }]);
  expect(
    transport.labels.filter((label) => label === "reserve-float-submit"),
  ).toHaveLength(1);
});
