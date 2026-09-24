import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import JSONBig from "json-bigint";
import { beforeAll, describe, expect, it, vi } from "vitest";

import {
  authenticateEventHistorySession,
  decodeBoundEventHistoryLedgerSnapshot,
  eventHistoryGenesisLosslessSha256,
  type EventHistorySourceBinding,
  followBoundEventHistoryChain,
  makeEventHistorySourceBinding,
  readBoundEventHistoryLedgerSnapshot,
  readBoundEventHistoryRawLedgerSnapshot,
  verifyEventHistoryCaptureHub,
} from "../src/l1-event-history-source.js";
import type {
  AcquiredLedgerSnapshot,
  LedgerSnapshotOutput,
} from "../src/l1-ledger-snapshot.js";
import type { WebSocketLike } from "../src/l1-tx-order-carriage.js";
import type { ContractDeploymentIdentityValue } from "../src/services/midgard-contracts.js";
import { makeFinalizedDeploymentManifestFixture } from "./helpers/finalized-deployment-manifest.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const genesis = {
  era: "shelley",
  startTime: "2026-01-01T00:00:00Z",
  slotLength: { milliseconds: 1000 },
  maxLovelaceSupply: 45000000000000000n,
};
let contracts: SDK.MidgardValidators;
let identity: ContractDeploymentIdentityValue;
let binding: EventHistorySourceBinding;
const build = (
  overrides: Partial<Parameters<typeof makeEventHistorySourceBinding>[0]> = {},
) =>
  Effect.runPromise(
    makeEventHistorySourceBinding({
      contracts,
      identity,
      network: "Preprod",
      ogmiosUrl: "http://localhost:1337",
      expectedGenesisLosslessSha256: eventHistoryGenesisLosslessSha256(genesis),
      ...overrides,
    }),
  );

beforeAll(async () => {
  const manifest = await makeFinalizedDeploymentManifestFixture();
  contracts = await loadRealMidgardContractsForTest({
    txHash: "ab".repeat(32),
    outputIndex: 0,
  });
  identity = {
    kind: "manifest",
    manifest,
    manifestId: manifest.manifestId,
    consensusProfile: manifest.consensusProfile,
  };
  binding = await build();
});

const capture = (): AcquiredLedgerSnapshot => {
  const outputs: LedgerSnapshotOutput[] = Object.values(
    binding.deployments,
  ).map((deployment, index) => ({
    txHash: "dd".repeat(32),
    outputIndex: index,
    address: deployment.address,
    assets: { lovelace: 3_000_000n, [deployment.policyId]: 1n },
    datum: Data.to(
      {
        position: "Root",
        next: null,
        protected_until: 0n,
        payload: "RootContent",
      },
      SDK.EventHistoryNode,
    ),
    hasReferenceScript: false,
  }));
  outputs.push({
    txHash: "ee".repeat(32),
    outputIndex: 0,
    address: binding.hubAddress,
    assets: { lovelace: 3_000_000n, [binding.hubUnit]: 1n },
    datum: binding.hubDatumCbor,
    hasReferenceScript: false,
  });
  return {
    point: { slot: 100, id: "ff".repeat(32) },
    addresses: [
      binding.hubAddress,
      ...Object.values(binding.deployments).flatMap((entry) => [
        entry.address,
        entry.retentionAddress,
      ]),
    ],
    outputs,
  };
};

type WireRequest = {
  id: number;
  method: string;
  params: Record<string, unknown>;
};
/** Exercises the actual socket framing and lossless parser. This fixture makes
 * no claim to authenticate a real network or establish canonical ancestry. */
class SourceSocket implements WebSocketLike {
  readonly requests: WireRequest[] = [];
  readonly listeners = new Map<string, ((event: never) => void)[]>();
  readonly encode = JSONBig({ useNativeBigInt: true });
  closed = false;
  genesisResponse: unknown = genesis;
  holdGenesis = false;
  pendingBlock: WireRequest | undefined;
  ledger = capture();
  factory = vi.fn(() => {
    queueMicrotask(() => this.emit("open"));
    return this;
  });
  addEventListener(type: string, listener: (event: never) => void) {
    this.listeners.set(type, [...(this.listeners.get(type) ?? []), listener]);
  }
  emit(type: string, event?: unknown) {
    for (const listener of this.listeners.get(type) ?? [])
      listener(event as never);
  }
  answer(request: WireRequest, result: unknown) {
    if (!this.closed)
      this.emit("message", {
        data: this.encode.stringify({ id: request.id, result }),
      });
  }
  send(data: string) {
    const request = JSON.parse(data) as WireRequest;
    this.requests.push(request);
    if (request.method === "nextBlock") {
      expect(this.pendingBlock).toBeUndefined();
      this.pendingBlock = request;
      return;
    }
    if (
      request.method === "queryNetwork/genesisConfiguration" &&
      this.holdGenesis
    )
      return;
    const results: Record<string, unknown> = {
      "queryNetwork/genesisConfiguration": this.genesisResponse,
      "queryLedgerState/tip": this.ledger.point,
      acquireLedgerState: { acquired: "ledgerState", point: this.ledger.point },
      "queryLedgerState/utxo": this.ledger.outputs.map((output) => {
        const value: Record<string, Record<string, bigint>> = {
          ada: { lovelace: output.assets.lovelace! },
        };
        for (const [unit, quantity] of Object.entries(output.assets)) {
          if (unit !== "lovelace")
            (value[unit.slice(0, 56)] ??= {})[unit.slice(56)] = quantity;
        }
        return {
          transaction: { id: output.txHash },
          index: output.outputIndex,
          address: output.address,
          value,
          datum: output.datum,
        };
      }),
      releaseLedgerState: { released: "ledgerState" },
      findIntersection: {
        intersection: this.ledger.point,
        tip: { ...this.ledger.point, height: 1 },
      },
      "queryNetwork/tip": { ...this.ledger.point, height: 1 },
    };
    expect(Object.hasOwn(results, request.method)).toBe(true);
    this.answer(request, results[request.method]);
  }
  close() {
    if (this.closed) return;
    this.closed = true;
    this.emit("close");
  }
  read(ogmiosUrl = "http://localhost:1337", signal?: AbortSignal) {
    return readBoundEventHistoryLedgerSnapshot({
      binding,
      ogmiosUrl,
      signal,
      timeoutMs: 200,
      webSocketFactory: this.factory,
    });
  }
  follow(ogmiosUrl = "http://localhost:1337") {
    const controller = new AbortController();
    const onIntersection = vi.fn();
    const onForward = vi.fn();
    const onUnavailable = vi.fn();
    const completion = followBoundEventHistoryChain({
      binding,
      ogmiosUrl,
      signal: controller.signal,
      intersections: [this.ledger.point],
      retainedPointLimit: 3,
      requestTimeoutMs: 200,
      heartbeatIntervalMs: 20,
      webSocketFactory: this.factory,
      onIntersection,
      onForward,
      onUnavailable,
      onRollback: vi.fn(),
      onTip: vi.fn(),
    }).catch((cause: unknown) => cause);
    return { controller, onIntersection, onForward, onUnavailable, completion };
  }
}

describe("history source verification on each actual socket", () => {
  it("authenticates before acquiring all five addresses and returns a bound capture", async () => {
    const socket = new SourceSocket();
    const snapshot = await socket.read();
    expect(socket.requests.map(({ method }) => method)).toEqual([
      "queryNetwork/genesisConfiguration",
      "queryLedgerState/tip",
      "acquireLedgerState",
      "queryLedgerState/utxo",
      "queryLedgerState/tip",
      "releaseLedgerState",
    ]);
    expect(socket.requests[0]!.params).toEqual({ era: "shelley" });
    expect(socket.requests[3]!.params).toEqual({
      addresses: [...socket.ledger.addresses].sort(),
    });
    expect(socket.ledger.addresses).toHaveLength(5);
    expect(snapshot.bindingDigest).toBe(binding.digest);
    expect(snapshot.history.deposits).toEqual([]);
    expect(snapshot.history.withdrawals).toEqual([]);
    expect(socket.closed).toBe(true);
  });

  it("captures a bound pre-initialization parent without manufacturing roots or omitting retention dust", async () => {
    const socket = new SourceSocket();
    const donation = {
      txHash: "ab".repeat(32),
      outputIndex: 3,
      address: binding.deployments.deposit.retentionAddress,
      assets: { lovelace: 3_000_000n },
      datum: "d87980",
      hasReferenceScript: false,
    };
    socket.ledger = { ...socket.ledger, outputs: [donation] };
    const verifySession = vi.fn();
    const extra = { addresses: ["substituted"], verifySession };
    const raw = await readBoundEventHistoryRawLedgerSnapshot({
      ...extra,
      binding,
      ogmiosUrl: "http://localhost:1337",
      at: socket.ledger.point,
      webSocketFactory: socket.factory,
    });
    expect(raw.bindingDigest).toBe(binding.digest);
    expect(raw.ledger).toEqual({
      ...socket.ledger,
      addresses: [...socket.ledger.addresses].sort(),
    });
    expect(socket.requests.map(({ method }) => method)).toEqual([
      "queryNetwork/genesisConfiguration",
      "acquireLedgerState",
      "queryLedgerState/utxo",
      "queryLedgerState/tip",
      "releaseLedgerState",
    ]);
    expect(socket.requests[1]!.params).toEqual({ point: socket.ledger.point });
    expect(socket.requests[2]!.params).toEqual({
      addresses: [...socket.ledger.addresses].sort(),
    });
    expect(verifySession).not.toHaveBeenCalled();
    expect(Object.isFrozen(raw)).toBe(true);
    expect(socket.closed).toBe(true);
    await expect(
      Effect.runPromise(
        decodeBoundEventHistoryLedgerSnapshot(raw.ledger, binding),
      ),
    ).rejects.toThrow(/authenticated hub/);
  });

  it("rejects raw capture on a different backend before acquiring its historical state", async () => {
    const socket = new SourceSocket();
    socket.genesisResponse = { ...genesis, startTime: "2025-01-01T00:00:00Z" };
    await expect(
      readBoundEventHistoryRawLedgerSnapshot({
        binding,
        ogmiosUrl: "http://localhost:1337",
        at: socket.ledger.point,
        webSocketFactory: socket.factory,
      }),
    ).rejects.toThrow(/approved pin/);
    expect(socket.requests.map(({ method }) => method)).toEqual([
      "queryNetwork/genesisConfiguration",
    ]);
    expect(socket.closed).toBe(true);
  });

  it("rejects raw capture endpoint substitution before opening", async () => {
    const socket = new SourceSocket();
    await expect(
      readBoundEventHistoryRawLedgerSnapshot({
        binding,
        ogmiosUrl: "http://localhost:1338",
        at: socket.ledger.point,
        webSocketFactory: socket.factory,
      }),
    ).rejects.toThrow(/endpoint/);
    expect(socket.factory).not.toHaveBeenCalled();
  });

  it("never reuses capture authentication for a different ChainSync backend at the same URL", async () => {
    await new SourceSocket().read();
    const socket = new SourceSocket();
    socket.genesisResponse = {
      ...genesis,
      maxLovelaceSupply: 45000000000000001n,
    };
    const run = socket.follow();
    expect(await run.completion).toMatchObject({
      message: expect.stringMatching(/approved pin/),
    });
    expect(socket.requests.map(({ method }) => method)).toEqual([
      "queryNetwork/genesisConfiguration",
    ]);
    expect(run.onIntersection).not.toHaveBeenCalled();
    expect(run.onForward).not.toHaveBeenCalled();
    expect(run.onUnavailable).toHaveBeenCalledTimes(1);
    expect(socket.closed).toBe(true);
  });

  it("authenticates every follower invocation before allowing its first block", async () => {
    for (let invocation = 0; invocation < 2; invocation++) {
      const socket = new SourceSocket();
      const run = socket.follow();
      await vi.waitFor(() => expect(socket.pendingBlock).toBeDefined());
      expect(socket.requests.slice(0, 3).map(({ method }) => method)).toEqual([
        "queryNetwork/genesisConfiguration",
        "findIntersection",
        "nextBlock",
      ]);
      const next = { slot: 101, id: "ac".repeat(32), height: 2 };
      socket.answer(socket.pendingBlock!, {
        direction: "forward",
        tip: next,
        block: {
          ...next,
          ancestor: socket.ledger.point.id,
          type: "praos",
          transactions: [],
        },
      });
      socket.pendingBlock = undefined;
      await vi.waitFor(() => expect(run.onForward).toHaveBeenCalledTimes(1));
      run.controller.abort();
      expect(await run.completion).toBeInstanceOf(Error);
      expect(run.onUnavailable).toHaveBeenCalledTimes(1);
      expect(socket.closed).toBe(true);
    }
  });

  it("refuses endpoint substitution before opening either socket", async () => {
    const captureSocket = new SourceSocket();
    await expect(captureSocket.read("http://localhost:1338")).rejects.toThrow(
      /endpoint/,
    );
    expect(captureSocket.factory).not.toHaveBeenCalled();
    const chainSocket = new SourceSocket();
    const run = chainSocket.follow("http://localhost:1338");
    expect(await run.completion).toMatchObject({
      message: expect.stringMatching(/endpoint/),
    });
    expect(chainSocket.factory).not.toHaveBeenCalled();
    expect(run.onUnavailable).toHaveBeenCalledTimes(1);
    expect(run.onIntersection).not.toHaveBeenCalled();
  });

  it.each([
    ["missing transaction array", undefined, /complete transaction array/],
    [
      "malformed second transaction",
      [
        { id: "a1".repeat(32), spends: "inputs", inputs: [], outputs: [] },
        { id: "a2".repeat(32), inputs: [], outputs: [] },
      ],
      /validity disposition/,
    ],
    [
      "duplicate transaction identity",
      [
        { id: "a1".repeat(32), spends: "inputs", inputs: [], outputs: [] },
        { id: "a1".repeat(32), spends: "inputs", inputs: [], outputs: [] },
      ],
      /repeats a transaction/,
    ],
  ])(
    "rejects %s before delivering any part of a block",
    async (_label, transactions, message) => {
      const socket = new SourceSocket();
      const run = socket.follow();
      await vi.waitFor(() => expect(socket.pendingBlock).toBeDefined());
      const next = { slot: 101, id: "ac".repeat(32), height: 2 };
      socket.answer(socket.pendingBlock!, {
        direction: "forward",
        tip: next,
        block: {
          ...next,
          ancestor: socket.ledger.point.id,
          type: "praos",
          transactions,
        },
      });
      expect(await run.completion).toMatchObject({
        message: expect.stringMatching(message),
      });
      expect(run.onForward).not.toHaveBeenCalled();
      expect(run.onUnavailable).toHaveBeenCalledTimes(1);
      expect(socket.closed).toBe(true);
    },
  );

  it("delivers the whole decoded block with explicit ordinary versus collateral-only disposition", async () => {
    const socket = new SourceSocket();
    const run = socket.follow();
    await vi.waitFor(() => expect(socket.pendingBlock).toBeDefined());
    const input = { transaction: { id: "aa".repeat(32) }, index: 0 };
    const next = { slot: 101, id: "ac".repeat(32), height: 2 };
    socket.answer(socket.pendingBlock!, {
      direction: "forward",
      tip: next,
      block: {
        ...next,
        ancestor: socket.ledger.point.id,
        type: "praos",
        transactions: [
          {
            id: "a1".repeat(32),
            spends: "inputs",
            inputs: [input],
            outputs: [],
          },
          {
            id: "a2".repeat(32),
            spends: "collaterals",
            inputs: [input],
            collaterals: [input],
            outputs: [],
          },
        ],
      },
    });
    socket.pendingBlock = undefined;
    await vi.waitFor(() => expect(run.onForward).toHaveBeenCalledTimes(1));
    const block = run.onForward.mock.calls[0]![0];
    expect(block).toMatchObject({
      point: next,
      parent: socket.ledger.point.id,
    });
    expect(
      block.transactions.map(
        (transaction: { spends: string }) => transaction.spends,
      ),
    ).toEqual(["inputs", "collaterals"]);
    expect(Object.isFrozen(block)).toBe(true);
    expect(Object.isFrozen(block.transactions)).toBe(true);
    expect(Object.isFrozen(block.transactions[0].inputs)).toBe(true);
    expect(block.body).toBeUndefined();
    run.controller.abort();
    expect(await run.completion).toBeInstanceOf(Error);
  });

  it("closes both sessions on a stalled genesis handshake before admitting observations", async () => {
    const captureSocket = new SourceSocket();
    captureSocket.holdGenesis = true;
    await expect(captureSocket.read()).rejects.toThrow();
    expect(captureSocket.closed).toBe(true);
    expect(captureSocket.requests).toHaveLength(1);
    const chainSocket = new SourceSocket();
    chainSocket.holdGenesis = true;
    const run = chainSocket.follow();
    expect(await run.completion).toBeInstanceOf(Error);
    expect(chainSocket.closed).toBe(true);
    expect(chainSocket.requests).toHaveLength(1);
    expect(run.onUnavailable).toHaveBeenCalledTimes(1);
    expect(run.onIntersection).not.toHaveBeenCalled();
  });

  it("honors abort during genesis and ignores a late matching response", async () => {
    const socket = new SourceSocket();
    socket.holdGenesis = true;
    const run = socket.follow();
    await vi.waitFor(() => expect(socket.requests).toHaveLength(1));
    run.controller.abort();
    socket.answer(socket.requests[0]!, genesis);
    expect(await run.completion).toBeInstanceOf(Error);
    // Deliver a transport event even after close, bypassing the fixture's normal
    // closed-socket suppression. A late response must not revive observations.
    socket.emit("message", {
      data: socket.encode.stringify({
        id: socket.requests[0]!.id,
        result: genesis,
      }),
    });
    await new Promise<void>((resolve) => setImmediate(resolve));
    expect(run.onIntersection).not.toHaveBeenCalled();
    expect(run.onUnavailable).toHaveBeenCalledTimes(1);
    expect(socket.closed).toBe(true);
    expect(socket.requests).toHaveLength(1);
  });

  it("keeps the address set and handshake wrapper-owned even with extra runtime properties", async () => {
    const socket = new SourceSocket();
    const verifySession = vi.fn();
    const extra = { addresses: ["substituted"], verifySession };
    await readBoundEventHistoryLedgerSnapshot({
      ...extra,
      binding,
      ogmiosUrl: "http://localhost:1337",
      timeoutMs: 200,
      webSocketFactory: socket.factory,
    });
    expect(verifySession).not.toHaveBeenCalled();
    expect(socket.requests[0]!.method).toBe(
      "queryNetwork/genesisConfiguration",
    );
    expect(socket.requests[3]!.params).toEqual({
      addresses: [...socket.ledger.addresses].sort(),
    });
  });
});

describe("history source's approved lossless genesis pin", () => {
  it("hashes sorted result integer tokens without changing the existing slot-artifact algorithm", () => {
    const parse = JSONBig({ useNativeBigInt: true, strict: true });
    const a = parse.parse(
      '{"quantity":9007199254740992,"nested":{"b":1,"a":2}}',
    );
    const b = parse.parse(
      '{"nested":{"a":2,"b":1},"quantity":9007199254740993}',
    );
    expect(eventHistoryGenesisLosslessSha256(a)).not.toBe(
      eventHistoryGenesisLosslessSha256(b),
    );
    expect(eventHistoryGenesisLosslessSha256(a)).toBe(
      eventHistoryGenesisLosslessSha256({
        nested: { a: 2, b: 1 },
        quantity: 9007199254740992n,
      }),
    );
    expect(eventHistoryGenesisLosslessSha256(b)).not.toBe(
      eventHistoryGenesisLosslessSha256({
        nested: { a: 2, b: 1 },
        quantity: "9007199254740993",
      }),
    );
    expect(
      eventHistoryGenesisLosslessSha256({ quantity: 45000000000000000n }),
    ).toBe(
      createHash("sha256")
        .update('{"quantity":45000000000000000}')
        .digest("hex"),
    );
  });

  it.each([NaN, Infinity, undefined, 9007199254740992, 1.5, new Date(0)])(
    "refuses lossy or unsupported genesis value %s",
    (quantity) => {
      expect(() => eventHistoryGenesisLosslessSha256({ quantity })).toThrow(
        /losslessly/,
      );
    },
  );

  it("checks each exact supplied session and never adopts a changed response", async () => {
    const request = vi
      .fn()
      .mockResolvedValueOnce(genesis)
      .mockResolvedValueOnce({
        ...genesis,
        maxLovelaceSupply: 45000000000000001n,
      });
    await expect(
      authenticateEventHistorySession({ request }, binding),
    ).resolves.toEqual({
      bindingDigest: binding.digest,
      genesisSha256: binding.genesisSha256,
    });
    await expect(
      authenticateEventHistorySession({ request }, binding),
    ).rejects.toThrow(/approved pin/);
    expect(request.mock.calls).toEqual([
      ["queryNetwork/genesisConfiguration", { era: "shelley" }],
      ["queryNetwork/genesisConfiguration", { era: "shelley" }],
    ]);
    expect(binding.genesisSha256).toBe(
      eventHistoryGenesisLosslessSha256(genesis),
    );
  });

  it("binds deployment, source endpoint and approved pin while normalizing HTTP/WS routing", async () => {
    expect((await build({ ogmiosUrl: "ws://localhost:1337/" })).digest).toBe(
      binding.digest,
    );
    expect(
      (await build({ ogmiosUrl: "http://localhost:1338" })).digest,
    ).not.toBe(binding.digest);
    expect(
      (await build({ expectedGenesisLosslessSha256: "aa".repeat(32) })).digest,
    ).not.toBe(binding.digest);
    expect(Object.isFrozen(binding.deployments.deposit)).toBe(true);
  });

  it("refuses missing pins, derived bundles and mismatched manifest/network identities", async () => {
    await expect(build({ expectedGenesisLosslessSha256: "" })).rejects.toThrow(
      /approved/,
    );
    await expect(
      build({ identity: { ...identity, kind: "derived" } }),
    ).rejects.toThrow(/admitted deployment/);
    await expect(
      build({ identity: { ...identity, manifestId: "00".repeat(32) } }),
    ).rejects.toThrow(/admitted deployment/);
    await expect(build({ network: "Preview" })).rejects.toThrow(/network/);
  });
});

describe("same-capture deployment hub", () => {
  it("binds the exact hub and both complete lists with order-independent capture identity", async () => {
    const ledger = capture();
    const first = await Effect.runPromise(
      decodeBoundEventHistoryLedgerSnapshot(ledger, binding),
    );
    const second = await Effect.runPromise(
      decodeBoundEventHistoryLedgerSnapshot(
        {
          ...ledger,
          addresses: [...ledger.addresses].reverse(),
          outputs: [...ledger.outputs].reverse(),
        },
        binding,
      ),
    );
    expect(first.history.deposits).toEqual([]);
    expect(first.history.withdrawals).toEqual([]);
    expect(first.hub.txHash).toBe("ee".repeat(32));
    expect(first.snapshotDigest).toBe(second.snapshotDigest);
    const moved = await Effect.runPromise(
      decodeBoundEventHistoryLedgerSnapshot(
        { ...ledger, point: { ...ledger.point, id: "aa".repeat(32) } },
        binding,
      ),
    );
    expect(moved.snapshotDigest).not.toBe(first.snapshotDigest);
  });

  it.each([
    [
      "wrong token",
      (hub: LedgerSnapshotOutput) => ({
        ...hub,
        assets: {
          lovelace: 3_000_000n,
          [binding.hubUnit.slice(0, 56) + "00"]: 1n,
        },
      }),
    ],
    [
      "wrong quantity",
      (hub: LedgerSnapshotOutput) => ({
        ...hub,
        assets: { ...hub.assets, [binding.hubUnit]: 2n },
      }),
    ],
    [
      "foreign asset",
      (hub: LedgerSnapshotOutput) => ({
        ...hub,
        assets: { ...hub.assets, ["00".repeat(28)]: 1n },
      }),
    ],
    [
      "datum hash",
      (hub: LedgerSnapshotOutput) => ({ ...hub, datumHash: "aa".repeat(32) }),
    ],
    [
      "reference script",
      (hub: LedgerSnapshotOutput) => ({ ...hub, hasReferenceScript: true }),
    ],
    [
      "malformed datum",
      (hub: LedgerSnapshotOutput) => ({ ...hub, datum: "d87980" }),
    ],
    [
      "substituted datum",
      (hub: LedgerSnapshotOutput) => ({
        ...hub,
        datum: Data.to(
          {
            ...Data.from(binding.hubDatumCbor, SDK.HubOracleDatum),
            deposit: "aa".repeat(28),
          },
          SDK.HubOracleDatum,
        ),
      }),
    ],
  ] as const)(
    "refuses %s instead of filtering it into success",
    (_label, mutate) => {
      const ledger = capture();
      const hub = ledger.outputs[2]!;
      expect(() =>
        verifyEventHistoryCaptureHub(
          { ...ledger, outputs: [...ledger.outputs.slice(0, 2), mutate(hub)] },
          binding,
        ),
      ).toThrow();
    },
  );

  it("refuses missing, duplicate and conflicting authenticated hubs but permits unrelated donations", () => {
    const ledger = capture();
    const hub = ledger.outputs[2]!;
    expect(() =>
      verifyEventHistoryCaptureHub(
        {
          ...ledger,
          addresses: ledger.addresses.filter(
            (address) => address !== binding.hubAddress,
          ),
        },
        binding,
      ),
    ).toThrow(/omitted/);
    expect(() =>
      verifyEventHistoryCaptureHub(
        { ...ledger, outputs: ledger.outputs.slice(0, 2) },
        binding,
      ),
    ).toThrow(/exactly one/);
    expect(() =>
      verifyEventHistoryCaptureHub(
        { ...ledger, outputs: [...ledger.outputs, { ...hub, outputIndex: 1 }] },
        binding,
      ),
    ).toThrow(/exactly one/);
    expect(() =>
      verifyEventHistoryCaptureHub(
        {
          ...ledger,
          outputs: [
            ...ledger.outputs,
            {
              ...hub,
              outputIndex: 1,
              assets: {
                lovelace: 3_000_000n,
                [binding.hubUnit.slice(0, 56) + "00"]: 1n,
              },
            },
          ],
        },
        binding,
      ),
    ).toThrow(/exactly one/);
    expect(
      verifyEventHistoryCaptureHub(
        {
          ...ledger,
          outputs: [
            ...ledger.outputs,
            {
              ...hub,
              outputIndex: 1,
              assets: { lovelace: 3_000_000n },
              datum: "d87980",
            },
          ],
        },
        binding,
      ),
    ).toBe(hub);
  });
});
