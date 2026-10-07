import { createDaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import { waitFor } from "midgard-watcher/tests/l1/native-chain-sync.config";
import { expect, it } from "vitest";

import { captureAcceptanceNativeRead } from "../src/devnet-stack/acceptance-native-boundary.js";
import {
  nativeFixture,
  ogmiosFixture,
  POINT,
  readConfig,
} from "./acceptance-native.fixture.js";

const refs = [{ txHash: "ee".repeat(32), outputIndex: 2 }];
const scope = (signal?: AbortSignal, timeoutMs = 2000) =>
  createDaAvailabilityReadScope({
    deadlineEpochMs: Date.now() + timeoutMs,
    attemptTimeoutMs: timeoutMs,
    signal,
  });

it("reads exact acquired current outputs losslessly and joins both physical owners before returning", async () => {
  const server = await ogmiosFixture();
  const helper = await nativeFixture();
  const read = scope();
  try {
    const result = await captureAcceptanceNativeRead(
      readConfig(server.endpoint, helper),
      read,
      async (current) => {
        expect(current.point).toEqual(POINT);
        current.assertCurrent();
        const first = await current.queryExactOutRefs(refs);
        expect(first).toContain("9007199254740993");
        expect(await current.queryExactOutRefs(refs)).toEqual(
          first.replace('"id":4', '"id":5'),
        );
        return "all-four-payouts-verified";
      },
      helper,
    );
    expect(result.value).toBe("all-four-payouts-verified");
    expect(result.boundary.point).toEqual(POINT);
    expect(result.boundary.startupDigest).toMatch(/^[0-9a-f]{64}$/);
    expect(result.boundary.eventDigest).toMatch(/^[0-9a-f]{64}$/);
    expect(helper.closed()).toBe(true);
    await waitFor(() => server.closed() === 1);
    expect(server.active()).toBe(0);
    expect(
      server.requests.find((r) => r.method === "acquireLedgerState")?.params,
    ).toEqual({ point: { id: POINT.blockHash, slot: 102 } });
  } finally {
    read.close();
    await server.close();
  }
});

it.each(["wrongAcquire", "wrongSelected", "wrongEnvelope"] as const)(
  "rejects %s before any payout read and drains transports",
  async (option) => {
    const server = await ogmiosFixture({ [option]: true });
    const helper = await nativeFixture();
    const read = scope();
    let entered = false;
    try {
      await expect(
        captureAcceptanceNativeRead(
          readConfig(server.endpoint, helper),
          read,
          async () => {
            entered = true;
            return true;
          },
          helper,
        ),
      ).rejects.toThrow(
        option === "wrongAcquire"
          ? "acquired a different"
          : option === "wrongSelected"
            ? "selected chain differs"
            : "uncorrelated",
      );
      expect(entered).toBe(false);
      expect(
        server.requests.some((r) => r.method === "queryLedgerState/utxo"),
      ).toBe(false);
      await waitFor(() => server.closed() === 1);
      expect(server.active()).toBe(0);
    } finally {
      read.close();
      await server.close();
    }
  },
);

it.each(["forward", "rollback", "exit", "error"] as const)(
  "revokes an active acquired read on native %s with physical query cancellation",
  async (command) => {
    const server = await ogmiosFixture({ stallQuery: true });
    const helper = await nativeFixture();
    const read = scope();
    let signal!: AbortSignal;
    try {
      const result = captureAcceptanceNativeRead(
        readConfig(server.endpoint, helper),
        read,
        async (current) => {
          signal = current.signal;
          return await current.queryExactOutRefs(refs);
        },
        helper,
      );
      const rejected = expect(result).rejects.toThrow();
      await waitFor(() =>
        server.requests.some((r) => r.method === "queryLedgerState/utxo"),
      );
      helper.send(command);
      await rejected;
      expect(signal.aborted).toBe(true);
      expect(helper.closed()).toBe(true);
      await waitFor(() => server.closed() === 1);
      expect(server.active()).toBe(0);
    } finally {
      read.close();
      await server.close();
    }
  },
);

it("refuses cached payout success when final selected tip advanced without an already delivered native callback", async () => {
  const server = await ogmiosFixture();
  const helper = await nativeFixture();
  const read = scope();
  try {
    await expect(
      captureAcceptanceNativeRead(
        readConfig(server.endpoint, helper),
        read,
        async (current) => {
          await current.queryExactOutRefs(refs);
          server.advance();
          return true;
        },
        helper,
      ),
    ).rejects.toThrow("selected chain differs");
    expect(helper.closed()).toBe(true);
  } finally {
    read.close();
    await server.close();
  }
});

it("refuses public run or code mutation at the final fence", async () => {
  const server = await ogmiosFixture();
  const helper = await nativeFixture();
  const read = scope();
  try {
    await expect(
      captureAcceptanceNativeRead(
        readConfig(server.endpoint, helper, async () => {
          throw new Error("public run changed");
        }),
        read,
        async (current) => await current.queryExactOutRefs(refs),
        helper,
      ),
    ).rejects.toThrow("public run changed");
    expect(helper.closed()).toBe(true);
  } finally {
    read.close();
    await server.close();
  }
});

it("does not promote a readiness tip into a fabricated current forward", async () => {
  const server = await ogmiosFixture();
  const helper = await nativeFixture("no-current");
  const read = scope(undefined, 300);
  try {
    await expect(
      captureAcceptanceNativeRead(
        readConfig(server.endpoint, helper),
        read,
        async () => true,
        helper,
      ),
    ).rejects.toThrow();
    expect(helper.closed()).toBe(true);
    expect(server.requests.some((r) => r.method === "acquireLedgerState")).toBe(
      false,
    );
  } finally {
    read.close();
    await server.close();
  }
});

it("cancels caller-revoked actual I/O and waits for an uncooperative peer's physical TCP close", async () => {
  const server = await ogmiosFixture({ stallQuery: true });
  const helper = await nativeFixture();
  const controller = new AbortController();
  const read = scope(controller.signal);
  try {
    const result = captureAcceptanceNativeRead(
      readConfig(server.endpoint, helper),
      read,
      async (current) => await current.queryExactOutRefs(refs),
      helper,
    );
    const rejected = expect(result).rejects.toThrow();
    await waitFor(() =>
      server.requests.some((r) => r.method === "queryLedgerState/utxo"),
    );
    controller.abort(new Error("external acceptance cancelled"));
    await rejected;
    expect(helper.closed()).toBe(true);
    await waitFor(() => server.closed() === 1);
    expect(server.active()).toBe(0);
  } finally {
    read.close();
    await server.close();
  }
});

it("runs the existing exact transaction/depth readers within one native boundary and physically joins each socket", async () => {
  const server = await ogmiosFixture();
  const helper = await nativeFixture();
  const read = scope();
  try {
    const result = await captureAcceptanceNativeRead(
      readConfig(server.endpoint, helper),
      read,
      async (current) => {
        const transaction = await current.readExactTransaction({
          txHash: refs[0]!.txHash,
          intersection: { slot: 101, headerHash: "aa".repeat(32) },
          blockPoint: { slot: 102, headerHash: POINT.blockHash },
          blockScanLimit: 2,
        });
        expect(transaction.transactionCbor).toBe("84a3008001800200a0f5f6");
        expect(transaction.blockPoint.blockNo).toBe(11);
        expect(
          await current.canonicalBlockDepth({
            blockHash: POINT.blockHash,
            slot: 102,
            blockNo: 11n,
          }),
        ).toBe(1n);
        return await current.queryExactOutRefs(refs);
      },
      helper,
    );
    expect(result.boundary.point).toEqual(POINT);
    await waitFor(() => server.closed() === 3);
    expect(server.active()).toBe(0);
    expect(helper.closed()).toBe(true);
  } finally {
    read.close();
    await server.close();
  }
});

it("rejects an independently answered lineage tip on another same-height fork", async () => {
  const server = await ogmiosFixture({ wrongLineageTip: true });
  const helper = await nativeFixture();
  const read = scope();
  try {
    await expect(
      captureAcceptanceNativeRead(
        readConfig(server.endpoint, helper),
        read,
        async (current) =>
          await current.canonicalBlockDepth({
            blockHash: "aa".repeat(32),
            slot: 101,
            blockNo: 10n,
          }),
        helper,
      ),
    ).rejects.toThrow("lineage selected chain differs");
    expect(helper.closed()).toBe(true);
    await waitFor(() => server.closed() === 2);
  } finally {
    read.close();
    await server.close();
  }
});

it.each(["stallDepth", "stallBlock"] as const)(
  "cancels an actual %s lineage request on native loss and joins its physical owner",
  async (option) => {
    const server = await ogmiosFixture({ [option]: true });
    const helper = await nativeFixture();
    const read = scope();
    try {
      const result = captureAcceptanceNativeRead(
        readConfig(server.endpoint, helper),
        read,
        async (current) =>
          option === "stallDepth"
            ? await current.canonicalBlockDepth({
                blockHash: POINT.blockHash,
                slot: 102,
                blockNo: 11n,
              })
            : await current.readExactTransaction({
                txHash: refs[0]!.txHash,
                intersection: { slot: 101, headerHash: "aa".repeat(32) },
                blockPoint: { slot: 102, headerHash: POINT.blockHash },
                blockScanLimit: 2,
              }),
        helper,
      );
      const rejected = expect(result).rejects.toThrow();
      await waitFor(() =>
        option === "stallDepth"
          ? server.requests.filter((r) => r.method === "findIntersection")
              .length >= 3
          : server.requests.some((r) => r.method === "nextBlock"),
      );
      helper.send("exit");
      await rejected;
      await waitFor(() => server.closed() === 2);
      expect(server.active()).toBe(0);
      expect(helper.closed()).toBe(true);
    } finally {
      read.close();
      await server.close();
    }
  },
);

it("retains callback ownership after cancellation until the underlying callback settles", async () => {
  const server = await ogmiosFixture();
  const helper = await nativeFixture();
  const read = scope();
  let release!: () => void;
  const gate = new Promise<void>((resolve) => {
    release = resolve;
  });
  let entered = false;
  let retired = false;
  try {
    const result = captureAcceptanceNativeRead(
      readConfig(server.endpoint, helper),
      read,
      async (current) => {
        await current.queryExactOutRefs(refs);
        entered = true;
        await gate;
        current.assertCurrent();
        return true;
      },
      helper,
    );
    const rejected = expect(result)
      .rejects.toThrow()
      .then(() => {
        retired = true;
      });
    await waitFor(() => entered);
    helper.send("forward");
    await waitFor(() => helper.closed());
    expect(retired).toBe(false);
    release();
    await rejected;
  } finally {
    release();
    read.close();
    await server.close();
  }
});
