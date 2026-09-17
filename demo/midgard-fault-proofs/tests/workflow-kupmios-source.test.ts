import { readFile } from "node:fs/promises";
import { resolve } from "node:path";

import {
  CML,
  credentialToAddress,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  paymentCredentialOf,
  scriptFromNative,
  scriptHashToCredential,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import JSONBig from "json-bigint";
import { describe, expect, it, vi } from "vitest";

import {
  createCursorFamilyWorkflowAdapter,
  CURSOR_FAMILY_TRANSACTION_PORT,
} from "../src/workflow/cursor-family-adapter.js";
import { MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC } from "../src/workflow/cursor-family-spec.js";
import { FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT } from "../src/workflow/family-l1-observation.js";
import {
  admitKupoMatchAgainstTransactionOutput,
  computeFraudProofRawL1PointId,
  computeFraudProofReleaseEconomicsPolicyDigest,
  computeFraudProofReleaseFinalityPolicyDigest,
  createLocalKupmiosFraudProofRawL1SnapshotAuthority,
  createLocalKupmiosHttpOgmiosRawSource,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofRawL1Fetch,
  type FraudProofRawL1Point,
  type FraudProofRawL1WebSocketLike,
  isLocalKupmiosPointBehindKupoHead,
  LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS,
  LocalKupmiosCheckpointChangedError,
  LocalKupmiosExactPointNotCanonicalError,
  LocalKupmiosTransportUnavailableError,
  OGMIOS_RAW_TRANSACTION_CBOR_FLAG,
  pinAdmittedLocalKupmiosBoundaryAtPoint,
  readAdmittedLocalKupmiosAddressUtxosAtPoint,
  readAdmittedLocalKupmiosBoundary,
  readAdmittedLocalKupmiosPredecessorPoint,
  readAdmittedLocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosRawTransaction,
  readAdmittedLocalKupmiosReferenceBodiesAtPoint,
  readAdmittedLocalKupmiosSignedTransactionRecovery,
  readAdmittedLocalKupmiosTransactionInclusion,
  readAdmittedLocalKupmiosUnitHistoryAtPoint,
  readAdmittedLocalKupmiosUtxosByOutRefAtPoint,
  rebroadcastAdmittedLocalKupmiosSignedTransaction,
  requireOgmiosRawTransactionCbor,
  validateVerifiedFraudProofReleaseEconomicsPolicy,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "../src/workflow/index.js";
import {
  createLinearFamilyWorkflowAdapter,
  LINEAR_FAMILY_TRANSACTION_PORT,
} from "../src/workflow/linear-family-adapter.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import type { SignedWorkflowTransaction } from "../src/workflow/signed-transaction-reconciliation.js";

const hash = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(32);
const DEPLOYMENT = hash(1);
const RELEASE = hash(2);
const KUP0_HEAD = hash(3);
const TARGET = hash(4);
const ANCESTOR = hash(5);
const TIP = hash(6);
const EARLIER = hash(9);

const chainPoint = (
  slot = "400",
  blockHash = TARGET,
  blockNo = "71",
): FraudProofRawL1Point => ({
  slot,
  blockHash,
  blockNo,
  pointId: computeFraudProofRawL1PointId({ slot, blockHash, blockNo }),
});

const ordinaryTransaction = (fee: bigint) => {
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    CML.TransactionOutputList.new(),
    fee,
  );
  const value = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
  );
  return {
    id: CML.hash_transaction(body).to_hex(),
    cbor: value.to_canonical_cbor_hex(),
  };
};

const releaseFinality: VerifiedFraudProofReleaseFinalityPolicy = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: DEPLOYMENT,
  blueprintHash: RELEASE,
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest({
    confirmationDepth: 30,
    automaticRecoveryMaxDepth: 2160,
    deepRollbackPolicy: "automated_rewind_replay_incident-v1",
  }),
  policy: {
    confirmationDepth: 30,
    automaticRecoveryMaxDepth: 2160,
    deepRollbackPolicy: "automated_rewind_replay_incident-v1",
  },
};

class OgmiosBoundarySocket implements FraudProofRawL1WebSocketLike {
  readonly listeners = new Map<string, ((event: never) => void)[]>();
  nextCount = 0;
  originIntersection = false;
  intersection = { slot: 380, id: ANCESTOR };
  closeCount = 0;
  readonly frames: string[] = [];

  constructor(
    private readonly transactions: readonly unknown[] = [],
    private readonly childAncestor = ANCESTOR,
    private readonly parentHeight = 70,
    private readonly behavior: Readonly<{
      open?: boolean;
      close?: boolean;
      respond?: boolean;
      responseText?: string;
      sendError?: boolean;
      childHeight?: number;
      tipHeight?: number;
      tipSlot?: number;
      mempoolPresent?: boolean;
      submit?: (cbor: string) => Promise<string>;
    }> = {},
  ) {
    if (behavior.open !== false) queueMicrotask(() => this.emit("open", {}));
  }

  removeEventListener(type: string, listener: (event: never) => void): void {
    this.listeners.set(
      type,
      (this.listeners.get(type) ?? []).filter((value) => value !== listener),
    );
  }

  addEventListener(type: string, listener: (event: never) => void): void {
    const listeners = this.listeners.get(type) ?? [];
    listeners.push(listener);
    this.listeners.set(type, listeners);
  }

  send(data: string): void {
    if (this.behavior.sendError)
      throw new Error("ordinary socket send failure");
    if (this.behavior.respond === false) return;
    const request = JSON.parse(data) as {
      readonly id: number;
      readonly method: string;
      readonly params?: {
        readonly points?: readonly ({ slot: number; id: string } | "origin")[];
        readonly transaction?: { readonly cbor: string };
      };
    };
    if (request.method === "findIntersection") {
      const point = request.params!.points![0]!;
      this.originIntersection = point === "origin";
      if (point !== "origin") this.intersection = point;
    }
    if (
      ["submitTransaction", "acquireMempool", "hasTransaction"].includes(
        request.method,
      )
    ) {
      const operation =
        request.method === "submitTransaction"
          ? this.behavior.submit!(request.params!.transaction!.cbor).then(
              (id) => ({ transaction: { id } }),
            )
          : Promise.resolve(
              request.method === "acquireMempool"
                ? { acquired: "mempool", slot: 1000 }
                : (this.behavior.mempoolPresent ?? false),
            );
      void operation.then((result) =>
        this.emit("message", {
          data: JSON.stringify({ jsonrpc: "2.0", id: request.id, result }),
        }),
      );
      return;
    }
    const result =
      request.method === "findIntersection"
        ? {
            intersection: this.originIntersection
              ? "origin"
              : this.intersection,
            tip: {
              slot: this.behavior.tipSlot ?? 1000,
              id: TIP,
              height: this.behavior.tipHeight ?? 100,
            },
          }
        : this.nextCount++ === 0
          ? { direction: "backward", point: this.intersection }
          : {
              direction: "forward",
              block:
                this.intersection.slot < 380
                  ? {
                      slot: 380,
                      id: ANCESTOR,
                      height: this.parentHeight,
                      ancestor: EARLIER,
                      transactions: this.transactions,
                    }
                  : {
                      slot: 400,
                      id: TARGET,
                      height: this.behavior.childHeight ?? 71,
                      ancestor: this.childAncestor,
                      transactions: this.transactions,
                    },
            };
    const text =
      this.behavior.responseText ??
      JSON.stringify({ jsonrpc: "2.0", id: request.id, result });
    this.frames.push(text);
    queueMicrotask(() =>
      this.emit("message", {
        data: text,
      }),
    );
  }

  close(): void {
    this.closeCount += 1;
    if (this.behavior.close !== false)
      this.emit("close", { code: 1000, reason: "", wasClean: true });
  }

  emit(type: string, event: unknown): void {
    for (const listener of this.listeners.get(type) ?? []) {
      listener(event as never);
    }
  }
}

const response = (
  value: unknown,
  checkpointHeaders = false,
  oversized = false,
  headHash = KUP0_HEAD,
): Response =>
  new Response(JSONBig.stringify(value), {
    status: 200,
    headers: checkpointHeaders
      ? {
          "content-type": "application/json",
          "x-most-recent-checkpoint": "990",
          etag: headHash,
          ...(oversized ? { "content-length": "67108865" } : {}),
        }
      : { "content-type": "application/json" },
  });

const sourceFixture = ({
  oversizedKupo = false,
  blockTransactions = [],
  kupoMatches = [],
  childAncestor = ANCESTOR,
  parentHeight = 70,
  tipHeight = 100,
  tipSlot = 1000,
  observationDepth,
  checkpointOverride,
  signal,
  timeoutMs,
  maxResponseBytes,
  socketBehavior,
  fetchOverride,
  beforeFetch,
  matchesByPattern,
}: {
  readonly oversizedKupo?: boolean;
  readonly blockTransactions?: readonly unknown[];
  readonly kupoMatches?: readonly unknown[];
  readonly childAncestor?: string;
  readonly parentHeight?: number;
  readonly tipHeight?: number;
  readonly tipSlot?: number;
  readonly observationDepth?: "inclusion" | "release_finality";
  readonly checkpointOverride?: (
    slot: number,
  ) => { slot_no: number; header_hash: string; headHash?: string } | undefined;
  readonly signal?: AbortSignal;
  readonly timeoutMs?: number;
  readonly maxResponseBytes?: number;
  readonly socketBehavior?: ConstructorParameters<
    typeof OgmiosBoundarySocket
  >[3];
  readonly fetchOverride?: FraudProofRawL1Fetch;
  readonly beforeFetch?: (url: string) => Promise<void>;
  readonly matchesByPattern?: (pattern: string) => readonly unknown[];
} = {}) => {
  const requests: {
    readonly url: string;
    readonly init: RequestInit | undefined;
  }[] = [];
  const fetchImpl = async (
    url: string,
    init?: RequestInit,
  ): Promise<Response> => {
    requests.push({ url, init });
    await beforeFetch?.(url);
    if (fetchOverride !== undefined) return await fetchOverride(url, init);
    if (url === "http://127.0.0.1:1337") {
      return response({
        jsonrpc: "2.0",
        id: "midgard-fraud-proof-raw-tip-v1",
        result: { slot: tipSlot, id: TIP, height: tipHeight },
      });
    }
    const checkpointMatch = /\/checkpoints\/(\d+)$/u.exec(url);
    if (checkpointMatch !== null) {
      const slot = Number(checkpointMatch[1]);
      const override = checkpointOverride?.(slot);
      const checkpoint =
        override ??
        (slot >= 400
          ? { slot_no: 400, header_hash: TARGET }
          : slot >= 380
            ? { slot_no: 380, header_hash: ANCESTOR }
            : slot === 379
              ? { slot_no: 360, header_hash: EARLIER }
              : undefined);
      if (checkpoint !== undefined) {
        return response(
          { slot_no: checkpoint.slot_no, header_hash: checkpoint.header_hash },
          true,
          oversizedKupo,
          override?.headHash,
        );
      }
    }
    if (url.includes("/matches/")) {
      const pattern = decodeURIComponent(
        new URL(url).pathname.slice("/matches/".length),
      );
      return response(matchesByPattern?.(pattern) ?? kupoMatches, true);
    }
    throw new Error(`unexpected request ${url}`);
  };
  const sockets: OgmiosBoundarySocket[] = [];
  let resolveSocket!: (socket: OgmiosBoundarySocket) => void;
  const socketCreated = new Promise<OgmiosBoundarySocket>((resolve) => {
    resolveSocket = resolve;
  });
  const source = createLocalKupmiosHttpOgmiosRawSource({
    sourceId: "local-release-test",
    kupoHttpUrl: "http://127.0.0.1:1442",
    ogmiosUrl: "http://127.0.0.1:1337",
    releaseFinality,
    observationDepth,
    fetchImpl,
    ...(signal === undefined ? {} : { signal }),
    ...(timeoutMs === undefined ? {} : { timeoutMs }),
    ...(maxResponseBytes === undefined ? {} : { maxResponseBytes }),
    webSocketFactory: () => {
      const socket = new OgmiosBoundarySocket(
        blockTransactions,
        childAncestor,
        parentHeight,
        { ...socketBehavior, tipHeight, tipSlot },
      );
      sockets.push(socket);
      resolveSocket(socket);
      return socket;
    },
  });
  return { source, requests, sockets, socketCreated };
};

describe("shared concrete Kupmios snapshot captures", () => {
  it.each([false, true])(
    "holds the complete source lifetime across concurrent captures (separate authorities: %s)",
    async (separateAuthorities) => {
      let releaseScan!: () => void;
      let scanStarted!: () => void;
      const scanPending = new Promise<void>((resolve) => {
        releaseScan = resolve;
      });
      const started = new Promise<void>((resolve) => {
        scanStarted = resolve;
      });
      let firstScan = true;
      const fixture = sourceFixture({
        beforeFetch: async (url) => {
          if (url.includes("/matches/") && firstScan) {
            firstScan = false;
            scanStarted();
            await scanPending;
          }
        },
      });
      const firstAuthority = createLocalKupmiosFraudProofRawL1SnapshotAuthority(
        { source: fixture.source, releaseFinality },
      );
      const secondAuthority = separateAuthorities
        ? createLocalKupmiosFraudProofRawL1SnapshotAuthority({
            source: fixture.source,
            releaseFinality,
          })
        : firstAuthority;
      const request = {
        deploymentIdentityDigest: DEPLOYMENT,
        blueprintHash: RELEASE,
        finalityPolicyDigest: releaseFinality.policyDigest,
        headerHash: "11".repeat(28),
        scopes: [
          {
            role: "state_queue" as const,
            address: credentialToAddress(
              "Preprod",
              scriptHashToCredential("31".repeat(28)),
            ),
          },
        ],
        historyUnits: [],
      };
      const first = firstAuthority.capture(request);
      await started;
      const second = secondAuthority.capture(request);
      await new Promise<void>((resolve) => setImmediate(resolve));
      const tipQueriesWhilePending = fixture.sockets.filter(
        (socket) => socket.originIntersection,
      ).length;
      releaseScan();
      const snapshots = await Promise.all([first, second]);
      expect(tipQueriesWhilePending).toBe(1);
      expect(snapshots[0]).toEqual(snapshots[1]);
      // Each capture independently establishes and finally rechecks its boundary.
      expect(
        fixture.sockets.filter((socket) => socket.originIntersection),
      ).toHaveLength(4);
      expect(snapshots[0]).toMatchObject({
        cursor: { point: chainPoint(), confirmationDepth: 30 },
      });
    },
  );
});

describe("admitted historical Kupmios page contexts", () => {
  const unit = "31".repeat(28);
  const address = credentialToAddress("Preprod", scriptHashToCredential(unit));

  it.each(["history", "address"] as const)(
    "authenticates historical %s reads without changing the live boundary",
    async (kind) => {
      const fixture = sourceFixture();
      const boundary = await readAdmittedLocalKupmiosBoundary({
        source: fixture.source,
      });
      const point = chainPoint("380", ANCESTOR, "70");
      await expect(
        fixture.source.scanUnitHistoryPage({
          unit,
          fromGenesis: true,
          throughPoint: point,
          after: null,
        }),
      ).rejects.toThrow("outside its pinned boundary");
      if (kind === "history") {
        await expect(
          readAdmittedLocalKupmiosUnitHistoryAtPoint({
            source: fixture.source,
            unit,
            point,
          }),
        ).resolves.toEqual({ checkpoint: point, transactions: [] });
      } else {
        await expect(
          readAdmittedLocalKupmiosAddressUtxosAtPoint({
            source: fixture.source,
            address,
            point,
          }),
        ).resolves.toEqual([]);
      }
      await expect(
        fixture.source.scanUnitHistoryPage({
          unit,
          fromGenesis: true,
          throughPoint: boundary.kupoCheckpoint,
          after: null,
        }),
      ).resolves.toMatchObject({
        checkpoint: boundary.kupoCheckpoint,
        complete: true,
      });
      expect(
        fixture.sockets.filter((socket) => socket.originIntersection),
      ).toHaveLength(1);
    },
  );

  it("rejects an unpinned, future, substituted, or out-of-window historical point", async () => {
    const fixture = sourceFixture();
    const read = (point: FraudProofRawL1Point) =>
      readAdmittedLocalKupmiosUnitHistoryAtPoint({
        source: fixture.source,
        unit,
        point,
      });
    await expect(read(chainPoint("380", ANCESTOR, "70"))).rejects.toThrow(
      "outside its pinned boundary",
    );
    await readAdmittedLocalKupmiosBoundary({ source: fixture.source });
    await expect(read(chainPoint("420", hash(10), "72"))).rejects.toThrow(
      "outside the pinned release recovery window",
    );
    await expect(
      read(chainPoint("380", hash(10), "70")),
    ).rejects.toBeInstanceOf(LocalKupmiosExactPointNotCanonicalError);
    const older = sourceFixture({
      tipHeight: 3000,
      parentHeight: 2970,
      socketBehavior: { childHeight: 2971 },
    });
    await readAdmittedLocalKupmiosBoundary({ source: older.source });
    await expect(
      readAdmittedLocalKupmiosUnitHistoryAtPoint({
        source: older.source,
        unit,
        point: chainPoint("380", ANCESTOR, "70"),
      }),
    ).rejects.toThrow("outside the pinned release recovery window");
  });

  it("admits exactly 2160 blocks of historical distance and rejects 2161", async () => {
    const fixture = sourceFixture({
      tipHeight: 2230,
      socketBehavior: { childHeight: 2201 },
    });
    await readAdmittedLocalKupmiosBoundary({ source: fixture.source });
    await expect(
      readAdmittedLocalKupmiosUnitHistoryAtPoint({
        source: fixture.source,
        unit,
        point: chainPoint("380", ANCESTOR, "70"),
      }),
    ).resolves.toMatchObject({ checkpoint: { blockNo: "70" } });
    await expect(
      readAdmittedLocalKupmiosUnitHistoryAtPoint({
        source: fixture.source,
        unit,
        point: chainPoint("380", ANCESTOR, "69"),
      }),
    ).rejects.toThrow("outside the pinned release recovery window");
  });

  it("rechecks the exact historical point after scanning its page", async () => {
    let scanned = false;
    const fixture = sourceFixture({
      beforeFetch: async (url) => {
        if (url.includes("/matches/")) scanned = true;
      },
      checkpointOverride: (slot) =>
        slot === 380 && scanned
          ? { slot_no: 380, header_hash: hash(10) }
          : undefined,
    });
    await readAdmittedLocalKupmiosBoundary({ source: fixture.source });
    await expect(
      readAdmittedLocalKupmiosUnitHistoryAtPoint({
        source: fixture.source,
        unit,
        point: chainPoint("380", ANCESTOR, "70"),
      }),
    ).rejects.toBeInstanceOf(LocalKupmiosExactPointNotCanonicalError);
  });
});

describe("release-final boundary selection", () => {
  it.each([
    [1, 30],
    [20, 30],
    [1, 1],
    [20, 1],
  ])(
    "selects the most recent eligible block with %i slots per block at depth %i",
    async (spacing, depth) => {
      const tipSlot = 1000;
      const pointHash = (slot: number) => slot.toString(16).padStart(64, "0");
      const tip = {
        slot: tipSlot,
        id: pointHash(tipSlot),
        height: tipSlot / spacing,
      };
      const source = createLocalKupmiosHttpOgmiosRawSource({
        sourceId: `boundary-spacing-${spacing}`,
        kupoHttpUrl: "http://127.0.0.1:1442",
        ogmiosUrl: "http://127.0.0.1:1337",
        releaseFinality,
        fetchImpl: async (url) => {
          const requestedSlot = Number(new URL(url).pathname.split("/").at(-1));
          const slot = Math.floor(requestedSlot / spacing) * spacing;
          return response(
            { slot_no: slot, header_hash: pointHash(slot) },
            true,
          );
        },
        webSocketFactory: () => {
          const socket = new OgmiosBoundarySocket();
          let intersection: "origin" | { slot: number; id: string } = "origin";
          let acknowledge = true;
          socket.send = (data) => {
            const request = JSON.parse(data);
            let result: unknown;
            if (request.method === "findIntersection") {
              intersection = request.params.points[0];
              acknowledge = true;
              result = { intersection, tip };
            } else if (acknowledge) {
              acknowledge = false;
              result = { direction: "backward", point: intersection };
            } else {
              if (intersection === "origin")
                throw new Error("Expected exact intersection");
              const slot = intersection.slot + spacing;
              result = {
                direction: "forward",
                block: {
                  slot,
                  id: pointHash(slot),
                  height: slot / spacing,
                  ancestor: intersection.id,
                  transactions: [],
                },
              };
            }
            queueMicrotask(() =>
              socket.emit("message", {
                data: JSON.stringify({
                  jsonrpc: "2.0",
                  id: request.id,
                  result,
                }),
              }),
            );
          };
          return socket;
        },
      });
      const boundary = await readAdmittedLocalKupmiosBoundary({
        source,
        ...(depth === 1 ? { observationDepth: "inclusion" as const } : {}),
      });
      expect(boundary.kupoCheckpoint.blockNo).toBe(
        String(tip.height - depth + 1),
      );
      expect(boundary.kupoCheckpoint.slot).toBe(
        String(tipSlot - (depth - 1) * spacing),
      );
      expect(boundary.confirmationDepth).toBe(depth);
    },
  );
});

describe("concrete Kupmios transport cancellation and response bounds", () => {
  it("rejects invalid bounds and a non-platform signal before acquisition", () => {
    for (const maxResponseBytes of [0, -1, 1.5, Number.NaN, 67_108_865]) {
      expect(() => sourceFixture({ maxResponseBytes })).toThrow(
        "maxResponseBytes",
      );
    }
    expect(() =>
      sourceFixture({
        signal: Object.create(AbortSignal.prototype) as AbortSignal,
      }),
    ).toThrow("platform AbortSignal");
    const controller = new AbortController();
    controller.abort();
    expect(() => sourceFixture({ signal: controller.signal })).toThrow(
      "aborted",
    );
  });

  it("cancels a pending HTTP fetch and removes the owner listener", async () => {
    const controller = new AbortController();
    const add = vi.spyOn(controller.signal, "addEventListener");
    const remove = vi.spyOn(controller.signal, "removeEventListener");
    let started!: () => void;
    const ready = new Promise<void>((resolve) => {
      started = resolve;
    });
    let requestSignal: AbortSignal | null | undefined;
    const fixture = sourceFixture({
      signal: controller.signal,
      timeoutMs: 500,
      fetchOverride: async (_url, init) => {
        requestSignal = init?.signal;
        started();
        return await new Promise<Response>((_resolve, reject) => {
          requestSignal!.addEventListener(
            "abort",
            () => reject(requestSignal!.reason),
            { once: true },
          );
        });
      },
    });
    const outcome = fixture.source
      .readBoundary()
      .catch((error: unknown) => error);
    await ready;
    controller.abort();
    expect(await outcome).toMatchObject({ name: "AbortError" });
    expect(requestSignal?.aborted).toBe(true);
    expect(fixture.sockets).toHaveLength(1);
    expect(fixture.sockets[0]!.closeCount).toBe(1);
    expect(remove).toHaveBeenCalledWith("abort", add.mock.calls[0]![1]);
    await expect(fixture.source.readBoundary()).rejects.toThrow("aborted");
    expect(fixture.requests).toHaveLength(1);
  });

  it("cancels and releases an active HTTP body reader", async () => {
    const controller = new AbortController();
    let pulled!: () => void;
    const ready = new Promise<void>((resolve) => {
      pulled = resolve;
    });
    const cancel = vi.fn();
    const body = new ReadableStream<Uint8Array>(
      {
        pull: () => {
          pulled();
        },
        cancel,
      },
      { highWaterMark: 0 },
    );
    const fixture = sourceFixture({
      signal: controller.signal,
      timeoutMs: 500,
      fetchOverride: async () => new Response(body),
    });
    const outcome = fixture.source
      .readBoundary()
      .catch((error: unknown) => error);
    await ready;
    controller.abort();
    expect(await outcome).toMatchObject({ name: "AbortError" });
    expect(cancel).toHaveBeenCalledOnce();
    expect(body.locked).toBe(false);
  });

  it("enforces the supplied HTTP cap for declared and streamed bytes", async () => {
    for (const declared of [false, true]) {
      const cancel = vi.fn();
      const body = new ReadableStream<Uint8Array>({
        start: (stream) =>
          stream.enqueue(new TextEncoder().encode(" ".repeat(1025))),
        cancel,
      });
      const fixture = sourceFixture({
        maxResponseBytes: 1024,
        fetchOverride: async () =>
          new Response(body, {
            headers: declared ? { "content-length": "1025" } : {},
          }),
      });
      await expect(fixture.source.readBoundary()).rejects.toThrow(
        "exceeds the raw-source byte bound",
      );
      expect(cancel).toHaveBeenCalledOnce();
      expect(body.locked).toBe(false);
    }
    const exact = sourceFixture({
      maxResponseBytes: 1024,
      fetchOverride: async () => new Response("{}" + " ".repeat(1022)),
    });
    await expect(exact.source.readBoundary()).rejects.toThrow(
      "Kupo response omitted",
    );
  });

  it("bounds physical Ogmios sessions across simultaneous source instances", async () => {
    const controllers = Array.from({ length: 9 }, () => new AbortController());
    const fixtures = controllers.map((controller) =>
      sourceFixture({
        signal: controller.signal,
        socketBehavior: { respond: false },
      }),
    );
    const outcomes = fixtures.map((fixture) =>
      fixture.source.readBoundary().catch((error: unknown) => error),
    );
    try {
      await vi.waitFor(() =>
        expect(
          fixtures.flatMap(({ sockets }) => sockets).length,
        ).toBeGreaterThanOrEqual(4),
      );
      expect(fixtures.flatMap(({ sockets }) => sockets)).toHaveLength(4);
    } finally {
      controllers.forEach((controller) => controller.abort());
      await Promise.all(outcomes);
    }
  });

  it("holds capacity through delayed physical close and cancels queued acquisition", async () => {
    const controllers = Array.from({ length: 6 }, () => new AbortController());
    const fixtures = controllers.map((controller) =>
      sourceFixture({
        signal: controller.signal,
        socketBehavior: { respond: false, close: false },
      }),
    );
    const outcomes = fixtures.map((fixture) =>
      fixture.source.readBoundary().catch((error: unknown) => error),
    );
    try {
      await vi.waitFor(() =>
        expect(fixtures.flatMap(({ sockets }) => sockets)).toHaveLength(4),
      );
      controllers[5]!.abort();
      expect(await outcomes[5]).toMatchObject({ name: "AbortError" });
      controllers[0]!.abort();
      await vi.waitFor(() =>
        expect(fixtures[0]!.sockets[0]!.closeCount).toBe(1),
      );
      expect(fixtures.flatMap(({ sockets }) => sockets)).toHaveLength(4);
      fixtures[0]!.sockets[0]!.emit("close", { code: 1000 });
      await vi.waitFor(() => expect(fixtures[4]!.sockets).toHaveLength(1));
      expect(fixtures[5]!.sockets).toHaveLength(0);
    } finally {
      controllers.forEach((controller) => controller.abort());
      fixtures
        .flatMap(({ sockets }) => sockets)
        .forEach((socket) => socket.emit("close", { code: 1000 }));
      await Promise.all(outcomes);
    }
    expect(
      fixtures
        .flatMap(({ sockets }) => sockets)
        .every((socket) => [...socket.listeners.values()].flat().length === 0),
    ).toBe(true);
  });

  it("fails boundedly without releasing physically unclosed capacity", async () => {
    vi.useFakeTimers();
    const fixtures = Array.from({ length: 5 }, () =>
      sourceFixture({ timeoutMs: 25, socketBehavior: { close: false } }),
    );
    const outcomes = fixtures.map((fixture) =>
      fixture.source.readBoundary().catch((error: unknown) => error),
    );
    try {
      await vi.advanceTimersByTimeAsync(25);
      const failures = await Promise.all(outcomes);
      expect(
        failures.slice(0, 4).map((error) => (error as Error).message),
      ).toEqual(Array(4).fill("Ogmios physical socket close timed out"));
      expect(failures[4]).toMatchObject({
        message: "Ogmios session capacity wait timed out",
      });
      expect(fixtures.flatMap(({ sockets }) => sockets)).toHaveLength(4);
      expect(vi.getTimerCount()).toBe(0);
    } finally {
      fixtures
        .flatMap(({ sockets }) => sockets)
        .forEach((socket) => socket.emit("close", { code: 1000 }));
      await Promise.all(outcomes);
      vi.useRealTimers();
    }
    await expect(sourceFixture().source.readBoundary()).resolves.toBeDefined();
  });

  it("retains close diagnostics and rejects the failed RPC without retrying", async () => {
    const fixture = sourceFixture({ socketBehavior: { respond: false } });
    const outcome = fixture.source
      .readBoundary()
      .catch((error: unknown) => error);
    await fixture.socketCreated;
    await new Promise<void>((resolve) => setTimeout(resolve, 0));
    fixture.sockets[0]!.emit("close", {
      code: 1011,
      reason: "node connection resource exhausted",
      wasClean: true,
    });
    const error = await outcome;
    expect(error).toBeInstanceOf(Error);
    expect((error as Error).message).toContain(
      '"pendingMethods":["findIntersection"]',
    );
    expect((error as Error).message).toContain('"code":1011');
    expect((error as Error).message).toContain(
      "node connection resource exhausted",
    );
    expect(fixture.sockets).toHaveLength(1);
    expect([...fixture.sockets[0]!.listeners.values()].flat()).toHaveLength(0);
  });

  it("deduplicates concurrent reads of one exact block before taking session capacity", async () => {
    const fixture = sourceFixture();
    const blocks = await Promise.all(
      Array.from({ length: 9 }, () =>
        readAdmittedLocalKupmiosRawBlockAtPoint({
          source: fixture.source,
          point: chainPoint(),
        }),
      ),
    );
    expect(blocks.every((block) => block.point.blockHash === TARGET)).toBe(
      true,
    );
    expect(fixture.sockets).toHaveLength(1);
    expect(fixture.sockets[0]!.closeCount).toBe(1);
  });

  it.each(["opening", "request"] as const)(
    "aborts an Ogmios %s and disposes its timers and listeners",
    async (phase) => {
      vi.useFakeTimers();
      try {
        const controller = new AbortController();
        const add = vi.spyOn(controller.signal, "addEventListener");
        const remove = vi.spyOn(controller.signal, "removeEventListener");
        const fixture = sourceFixture({
          signal: controller.signal,
          timeoutMs: 500,
          socketBehavior:
            phase === "opening" ? { open: false } : { respond: false },
        });
        const outcome = fixture.source
          .readBoundary()
          .catch((error: unknown) => error);
        const socket = await fixture.socketCreated;
        // Allow the existing open microtask and request continuation to run.
        await Promise.resolve();
        await Promise.resolve();
        controller.abort();
        expect(await outcome).toMatchObject({ name: "AbortError" });
        expect(socket.closeCount).toBe(1);
        expect([...socket.listeners.values()].flat()).toHaveLength(0);
        expect(remove.mock.calls).toHaveLength(add.mock.calls.length);
        expect(vi.getTimerCount()).toBe(0);
        await expect(fixture.source.readBoundary()).rejects.toThrow("aborted");
        expect(fixture.sockets).toHaveLength(1);
      } finally {
        vi.useRealTimers();
      }
    },
  );

  it.each(["opening", "request"] as const)(
    "closes an Ogmios %s timeout without retained timers",
    async (phase) => {
      vi.useFakeTimers();
      try {
        const fixture = sourceFixture({
          timeoutMs: 25,
          socketBehavior:
            phase === "opening" ? { open: false } : { respond: false },
        });
        const outcome = fixture.source
          .readBoundary()
          .catch((error: unknown) => error);
        const socket = await fixture.socketCreated;
        await vi.advanceTimersByTimeAsync(25);
        expect(await outcome).toBeInstanceOf(Error);
        expect(socket.closeCount).toBe(1);
        expect([...socket.listeners.values()].flat()).toHaveLength(0);
        expect(vi.getTimerCount()).toBe(0);
      } finally {
        vi.useRealTimers();
      }
    },
  );

  it("closes opening error and active send failures through the same cleanup", async () => {
    for (const openingFailure of [true, false]) {
      const fixture = sourceFixture({
        timeoutMs: 500,
        socketBehavior: openingFailure ? { open: false } : { sendError: true },
      });
      const outcome = fixture.source
        .readBoundary()
        .catch((error: unknown) => error);
      const socket = await fixture.socketCreated;
      if (openingFailure) socket.emit("error", {});
      expect(await outcome).toBeInstanceOf(Error);
      expect(socket.closeCount).toBe(1);
      expect([...socket.listeners.values()].flat()).toHaveLength(0);
    }
  });

  it("checks UTF-8 WebSocket response bytes before JSON parsing and closes non-object responses", async () => {
    const overCap = "é".repeat(129);
    const parse = vi.spyOn(JSON, "parse");
    try {
      const fixture = sourceFixture({
        maxResponseBytes: 256,
        socketBehavior: { responseText: overCap },
      });
      await expect(fixture.source.readBoundary()).rejects.toThrow(
        "exceeds the raw-source byte bound",
      );
      expect(parse).not.toHaveBeenCalledWith(overCap);
      expect(fixture.sockets[0]?.closeCount).toBe(1);
    } finally {
      parse.mockRestore();
    }
    const scalar = sourceFixture({ socketBehavior: { responseText: "null" } });
    await expect(scalar.source.readBoundary()).rejects.toThrow(
      "non-object JSON response",
    );
    expect(scalar.sockets[0]?.closeCount).toBe(1);
  });

  it("retains omitted defaults, accepts an exact response cap, and refuses cached reads after cancellation", async () => {
    const original = sourceFixture();
    await original.source.readBoundary();
    const maximum = Math.max(
      ...original.sockets.flatMap((socket) =>
        socket.frames.map((frame) => Buffer.byteLength(frame, "utf8")),
      ),
    );
    const controller = new AbortController();
    const fixture = sourceFixture({
      signal: controller.signal,
      maxResponseBytes: maximum,
    });
    const boundary = await readAdmittedLocalKupmiosBoundary({
      source: fixture.source,
    });
    await fixture.source.scanAddressPage({
      address: "ordinary-address",
      throughPoint: boundary.kupoCheckpoint,
      after: null,
    });
    const unit = "11".repeat(28);
    await fixture.source.scanUnitHistoryPage({
      unit,
      fromGenesis: true,
      throughPoint: boundary.kupoCheckpoint,
      after: null,
    });
    const requests = fixture.requests.length;
    const sockets = fixture.sockets.length;
    controller.abort();
    await expect(
      fixture.source.scanAddressPage({
        address: "ordinary-address",
        throughPoint: boundary.kupoCheckpoint,
        after: null,
      }),
    ).rejects.toThrow("aborted");
    await expect(
      fixture.source.scanUnitHistoryPage({
        unit,
        fromGenesis: true,
        throughPoint: boundary.kupoCheckpoint,
        after: null,
      }),
    ).rejects.toThrow("aborted");
    await expect(
      readAdmittedLocalKupmiosRawBlockAtPoint({
        source: fixture.source,
        point: boundary.kupoCheckpoint,
      }),
    ).rejects.toThrow("aborted");
    await expect(
      readAdmittedLocalKupmiosPredecessorPoint({
        source: fixture.source,
        point: boundary.kupoCheckpoint,
      }),
    ).rejects.toThrow("aborted");
    expect(fixture.requests).toHaveLength(requests);
    expect(fixture.sockets).toHaveLength(sockets);
    expect(
      fixture.sockets.every(
        (socket) =>
          socket.closeCount === 1 &&
          [...socket.listeners.values()].flat().length === 0,
      ),
    ).toBe(true);
  });
});

describe("production local Kupmios raw source V1", () => {
  it("pins real Kupo checkpoint headers and requests string asset quantities", async () => {
    const fixture = sourceFixture();
    const boundary = await readAdmittedLocalKupmiosBoundary({
      source: fixture.source,
    });
    expect(boundary.kupoCheckpoint).toEqual({
      slot: "400",
      blockHash: TARGET,
      blockNo: "71",
      pointId: computeFraudProofRawL1PointId({
        slot: "400",
        blockHash: TARGET,
        blockNo: "71",
      }),
    });
    expect(boundary.confirmationDepth).toBe(30);
    await expect(
      readAdmittedLocalKupmiosBoundary({ source: { ...fixture.source } }),
    ).rejects.toThrow(/requires the admitted local Kupo\/Ogmios source/u);
    await expect(
      fixture.source.scanAddressPage({
        address:
          "addr_test1wzj2e2d2x6ns5w50z3h2zlaurqu4h9tpuv7zpkg6dj6xefcp4x24g",
        throughPoint: boundary.kupoCheckpoint,
        after: null,
      }),
    ).resolves.toMatchObject({ complete: true, nextCursor: null, utxos: [] });
    const matchRequest = fixture.requests.find(({ url }) =>
      url.includes("/matches/"),
    );
    expect(matchRequest?.url).toContain("resolve_hashes&order=oldest_first");
    expect(new Headers(matchRequest?.init?.headers).get("accept")).toBe(
      "application/json;asset-quantity=string",
    );
  });

  it.each([3_000_000n, 9_007_199_254_740_993n])(
    "preserves exact numeric Kupo lovelace %s through raw CBOR admission",
    async (coins) => {
      const address = credentialToAddress(
        "Preprod",
        scriptHashToCredential("31".repeat(28)),
      );
      const output = CML.TransactionOutput.new(
        CML.Address.from_bech32(address),
        CML.Value.from_coin(coins),
      );
      const outputs = CML.TransactionOutputList.new();
      outputs.add(output);
      const body = CML.TransactionBody.new(
        CML.TransactionInputList.new(),
        outputs,
        0n,
      );
      const transaction = CML.Transaction.new(
        body,
        CML.TransactionWitnessSet.new(),
        true,
      );
      const id = CML.hash_transaction(body).to_hex();
      const match = {
        transaction_index: 0,
        transaction_id: id,
        output_index: 0,
        address,
        value: { coins, assets: {} },
        datum_hash: null,
        script_hash: null,
        created_at: { slot_no: 400, header_hash: TARGET },
        spent_at: null,
        datum: null,
        script: null,
      };
      const fixture = sourceFixture({
        blockTransactions: [{ id, cbor: transaction.to_canonical_cbor_hex() }],
        kupoMatches: [match],
      });
      const boundary = await readAdmittedLocalKupmiosBoundary({
        source: fixture.source,
      });
      await expect(
        fixture.source.scanAddressPage({
          address,
          throughPoint: boundary.kupoCheckpoint,
          after: null,
        }),
      ).resolves.toMatchObject({
        utxos: [{ outputCbor: output.to_canonical_cbor_hex() }],
      });
      if (coins > BigInt(Number.MAX_SAFE_INTEGER)) {
        expect(() =>
          admitKupoMatchAgainstTransactionOutput({
            match: { ...match, value: { coins: Number(coins), assets: {} } },
            outputCbor: output.to_canonical_cbor_hex(),
          }),
        ).toThrow("exact nonnegative quantity");
      }
    },
  );

  it("binds Kupo value and reference-script identity to raw output CBOR", () => {
    const script = CML.Script.new_plutus_v3(
      CML.PlutusV3Script.from_raw_bytes(Uint8Array.from([1, 2, 3])),
    );
    const address = credentialToAddress(
      "Preview",
      scriptHashToCredential("31".repeat(28)),
    );
    const output = CML.TransactionOutput.new(
      CML.Address.from_bech32(address),
      CML.Value.from_coin(3_000_000n),
      undefined,
      script,
    );
    const outputScript = output.script_ref()!;
    const match = {
      transaction_index: 0,
      transaction_id: hash(7),
      output_index: 0,
      address,
      value: { coins: "3000000", assets: {} },
      datum_hash: null,
      script_hash: outputScript.hash().to_hex(),
      created_at: { slot_no: 390, header_hash: hash(8) },
      spent_at: null,
      datum: null,
      script: {
        language: "plutus:v3",
        script: Buffer.from(
          outputScript.as_plutus_v3()!.to_raw_bytes(),
        ).toString("hex"),
      },
    };
    expect(() =>
      admitKupoMatchAgainstTransactionOutput({
        match,
        outputCbor: output.to_canonical_cbor_hex(),
      }),
    ).not.toThrow();
    expect(() =>
      admitKupoMatchAgainstTransactionOutput({
        match: { ...match, value: { coins: "2999999", assets: {} } },
        outputCbor: output.to_canonical_cbor_hex(),
      }),
    ).toThrow(/value disagrees/u);
    expect(() =>
      admitKupoMatchAgainstTransactionOutput({
        match: {
          ...match,
          script: { language: "plutus:v3", script: "09" },
        },
        outputCbor: output.to_canonical_cbor_hex(),
      }),
    ).toThrow(/reference script disagrees/u);
  });

  it.each(["d8798101", "d8799f01ff"])(
    "binds inline datum identity to its original ledger bytes (%s)",
    (datumCbor) => {
      const datum = CML.PlutusData.from_cbor_hex(datumCbor);
      const address = credentialToAddress(
        "Preprod",
        scriptHashToCredential("31".repeat(28)),
      );
      const output = CML.TransactionOutput.new(
        CML.Address.from_bech32(address),
        CML.Value.from_coin(3_000_000n),
        CML.DatumOption.new_datum(datum),
      );
      const match = {
        transaction_index: 0,
        transaction_id: hash(7),
        output_index: 0,
        address,
        value: { coins: "3000000", assets: {} },
        datum_hash: CML.hash_plutus_data(datum).to_hex(),
        datum_type: "inline",
        script_hash: null,
        created_at: { slot_no: 390, header_hash: hash(8) },
        spent_at: null,
        datum: datumCbor,
        script: null,
      };
      const admit = (candidate: typeof match) =>
        admitKupoMatchAgainstTransactionOutput({
          match: candidate,
          outputCbor: output.to_cbor_hex(),
        });
      expect(() => admit(match)).not.toThrow();
      expect(() => admit({ ...match, datum_hash: hash(0xff) })).toThrow(
        /inline datum disagrees/u,
      );
      expect(() => admit({ ...match, datum: "d8798102" })).toThrow(
        /inline datum disagrees/u,
      );
      const reencoded = datumCbor === "d8798101" ? "d8799f01ff" : "d8798101";
      expect(() =>
        admit({
          ...match,
          datum: reencoded,
          datum_hash: CML.hash_plutus_data(
            CML.PlutusData.from_cbor_hex(reencoded),
          ).to_hex(),
        }),
      ).toThrow(/inline datum disagrees/u);
    },
  );

  it("fails closed when Ogmios omits raw transaction CBOR", () => {
    expect(() =>
      requireOgmiosRawTransactionCbor({
        value: { id: hash(9) },
        expectedTxHash: hash(9),
        label: "transaction",
      }),
    ).toThrow(new RegExp(OGMIOS_RAW_TRANSACTION_CBOR_FLAG, "u"));
  });

  it("rejects an oversized provider response before buffering it", async () => {
    const fixture = sourceFixture({ oversizedKupo: true });
    await expect(fixture.source.readBoundary()).rejects.toThrow(
      /exceeds the raw-source byte bound/u,
    );
  });

  it("accepts only transaction CBOR whose body hashes to the reported id", () => {
    const body = CML.TransactionBody.new(
      CML.TransactionInputList.new(),
      CML.TransactionOutputList.new(),
      0n,
    );
    const transaction = CML.Transaction.new(
      body,
      CML.TransactionWitnessSet.new(),
      true,
    );
    const txHash = CML.hash_transaction(body).to_hex();
    expect(
      requireOgmiosRawTransactionCbor({
        value: { id: txHash, cbor: transaction.to_canonical_cbor_hex() },
        expectedTxHash: txHash,
        label: "transaction",
      }),
    ).toBe(transaction.to_canonical_cbor_hex());
  });

  it("reports whether a non-canonical exact point is merely ahead of Kupo's head", async () => {
    const fixture = sourceFixture();
    await fixture.source.readBoundary();
    const lagging = await readAdmittedLocalKupmiosRawBlockAtPoint({
      source: fixture.source,
      point: chainPoint("1200", hash(0x21), "90"),
    }).then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(lagging).toBeInstanceOf(LocalKupmiosExactPointNotCanonicalError);
    expect(
      (lagging as LocalKupmiosExactPointNotCanonicalError).kupoLag,
    ).toEqual({ requestedSlot: 1200, checkpointSlot: 400, kupoHeadSlot: 990 });
    expect(isLocalKupmiosPointBehindKupoHead(lagging)).toBe(true);
    const diverged = await readAdmittedLocalKupmiosRawBlockAtPoint({
      source: fixture.source,
      point: chainPoint("400", hash(0x22), "71"),
    }).then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(diverged).toBeInstanceOf(LocalKupmiosExactPointNotCanonicalError);
    expect(
      (diverged as LocalKupmiosExactPointNotCanonicalError).kupoLag,
    ).toEqual({ requestedSlot: 400, checkpointSlot: 400, kupoHeadSlot: 990 });
    expect(isLocalKupmiosPointBehindKupoHead(diverged)).toBe(false);
    expect(isLocalKupmiosPointBehindKupoHead(new Error("other"))).toBe(false);
    expect((lagging as Error).message).toContain(
      "requested slot 1200, checkpoint slot 400, Kupo head slot 990",
    );
    // Kupo's head header can already name the requested block while the
    // checkpoint query still resolves to its predecessor; that is lag.
    const raced = new LocalKupmiosExactPointNotCanonicalError("raced", {
      requestedSlot: 1200,
      checkpointSlot: 1190,
      kupoHeadSlot: 1200,
    });
    expect(isLocalKupmiosPointBehindKupoHead(raced)).toBe(true);
    const forked = new LocalKupmiosExactPointNotCanonicalError("forked", {
      requestedSlot: 1200,
      checkpointSlot: 1200,
      kupoHeadSlot: 1300,
    });
    expect(isLocalKupmiosPointBehindKupoHead(forked)).toBe(false);
  });

  it("re-admits an exact ordered raw block only from the opaque concrete source", async () => {
    const first = ordinaryTransaction(1n);
    const second = ordinaryTransaction(2n);
    const fixture = sourceFixture({
      blockTransactions: [first, second],
    });
    const boundary = (await fixture.source.readBoundary()) as {
      readonly kupoCheckpoint: {
        readonly slot: string;
        readonly blockHash: string;
        readonly blockNo: string;
        readonly pointId: string;
      };
    };
    await expect(
      readAdmittedLocalKupmiosRawBlockAtPoint({
        source: fixture.source,
        point: boundary.kupoCheckpoint,
      }),
    ).resolves.toMatchObject({
      sourceId: fixture.source.sourceId,
      point: boundary.kupoCheckpoint,
      kupoCheckpoint: { slot: 400, blockHash: TARGET },
      transactions: [
        { txHash: first.id, transactionCbor: first.cbor },
        { txHash: second.id, transactionCbor: second.cbor },
      ],
    });
    const references = await readAdmittedLocalKupmiosReferenceBodiesAtPoint({
      source: fixture.source,
      point: boundary.kupoCheckpoint,
    });
    expect(references.targetBlock.transactions).toEqual([
      { txHash: first.id, transactionCbor: first.cbor },
      { txHash: second.id, transactionCbor: second.cbor },
    ]);
    expect(references.creatingTransactionBodies).toEqual([]);
    const rolledBackBlockHash = hash(0xec);
    await expect(
      readAdmittedLocalKupmiosRawBlockAtPoint({
        source: fixture.source,
        point: {
          ...boundary.kupoCheckpoint,
          blockHash: rolledBackBlockHash,
          pointId: computeFraudProofRawL1PointId({
            ...boundary.kupoCheckpoint,
            blockHash: rolledBackBlockHash,
          }),
        },
      }),
    ).rejects.toBeInstanceOf(LocalKupmiosExactPointNotCanonicalError);
    await expect(
      readAdmittedLocalKupmiosRawBlockAtPoint({
        source: { ...fixture.source },
        point: boundary.kupoCheckpoint,
      }),
    ).rejects.toThrow(/requires the admitted local Kupo\/Ogmios source/u);
  });

  it.each([false, true])(
    "projects an exact predecessor with cached child: %s",
    async (cached) => {
      const transactions = [ordinaryTransaction(1n), ordinaryTransaction(2n)];
      const fixture = sourceFixture({ blockTransactions: transactions });
      const point = chainPoint();
      if (cached) {
        await readAdmittedLocalKupmiosRawBlockAtPoint({
          source: fixture.source,
          point,
        });
      }
      const projection = await readAdmittedLocalKupmiosPredecessorPoint({
        source: fixture.source,
        point,
      });
      expect(projection).toEqual({
        sourceId: fixture.source.sourceId,
        point,
        predecessorPoint: chainPoint("380", ANCESTOR, "70"),
      });
      expect(Object.isFrozen(projection)).toBe(true);
      expect(Object.isFrozen(projection.point)).toBe(true);
      expect(Object.isFrozen(projection.predecessorPoint)).toBe(true);
      expect(fixture.sockets).toHaveLength(2);
      expect(fixture.sockets.map(({ intersection }) => intersection)).toEqual([
        { slot: 380, id: ANCESTOR },
        { slot: 360, id: EARLIER },
      ]);
      await expect(
        readAdmittedLocalKupmiosPredecessorPoint({
          source: fixture.source,
          point,
        }),
      ).resolves.toEqual(projection);
      expect(fixture.sockets).toHaveLength(2);
      await expect(
        readAdmittedLocalKupmiosRawBlockAtPoint({
          source: fixture.source,
          point,
        }),
      ).resolves.toEqual({
        schemaVersion: "midgard-local-kupmios-raw-block-at-point-v1",
        sourceId: fixture.source.sourceId,
        point,
        parentBlockHash: ANCESTOR,
        kupoCheckpoint: { slot: 400, blockHash: TARGET },
        transactions: transactions.map(({ id, cbor }) => ({
          txHash: id,
          transactionCbor: cbor,
        })),
      });
    },
  );

  it("binds predecessor acquisition to the captured source readers and identity", async () => {
    const fixture = sourceFixture();
    const sourceId = fixture.source.sourceId;
    fixture.source.readBlockAtPoint = async () => {
      throw new Error(
        "public block method must not supply predecessor evidence",
      );
    };
    Object.defineProperty(fixture.source, "sourceId", { value: "substituted" });
    await expect(
      readAdmittedLocalKupmiosPredecessorPoint({
        source: fixture.source,
        point: chainPoint(),
      }),
    ).resolves.toEqual({
      sourceId,
      point: chainPoint(),
      predecessorPoint: chainPoint("380", ANCESTOR, "70"),
    });
  });

  it("refuses source copies and malformed points before predecessor acquisition", async () => {
    const fixture = sourceFixture();
    await expect(
      readAdmittedLocalKupmiosPredecessorPoint({
        source: { ...fixture.source },
        point: chainPoint(),
      }),
    ).rejects.toThrow(/requires the admitted local Kupo\/Ogmios source/u);
    await expect(
      readAdmittedLocalKupmiosPredecessorPoint({
        source: fixture.source,
        point: { ...chainPoint(), pointId: hash(0xee) },
      }),
    ).rejects.toThrow(/pointId does not commit/u);
    expect(fixture.requests).toHaveLength(0);
  });

  it.each([
    { point: chainPoint("400", hash(0xef)), canonicality: true },
    { point: chainPoint("400", TARGET, "72"), canonicality: false },
  ])(
    "refuses a different requested child point: $canonicality",
    async ({ point, canonicality }) => {
      const fixture = sourceFixture();
      const result = readAdmittedLocalKupmiosPredecessorPoint({
        source: fixture.source,
        point,
      });
      if (canonicality) {
        await expect(result).rejects.toBeInstanceOf(
          LocalKupmiosExactPointNotCanonicalError,
        );
      } else {
        await expect(result).rejects.toThrow(
          "Ogmios exact block point differs from the request",
        );
      }
    },
  );

  it.each([
    { childAncestor: hash(0xef), parentHeight: 70 },
    { childAncestor: ANCESTOR, parentHeight: 69 },
    { childAncestor: ANCESTOR, parentHeight: 71 },
  ])(
    "refuses a non-direct predecessor: $childAncestor/$parentHeight",
    async (metadata) => {
      const fixture = sourceFixture(metadata);
      const result = readAdmittedLocalKupmiosPredecessorPoint({
        source: fixture.source,
        point: chainPoint(),
      });
      await expect(result).rejects.toThrow(
        "local Kupmios blocks do not form a direct predecessor",
      );
      await expect(result).rejects.not.toBeInstanceOf(
        LocalKupmiosExactPointNotCanonicalError,
      );
    },
  );

  it.each([400, 401])(
    "refuses a non-earlier checkpoint in both acquisition paths: %s",
    async (slot) => {
      const override = (lookup: number) =>
        lookup === 399 ? { slot_no: slot, header_hash: hash(0xef) } : undefined;
      const fixture = sourceFixture({ checkpointOverride: override });
      await expect(
        readAdmittedLocalKupmiosRawBlockAtPoint({
          source: fixture.source,
          point: chainPoint(),
        }),
      ).rejects.toThrow("Kupo did not return an earlier ancestor checkpoint");
      await expect(
        readAdmittedLocalKupmiosPredecessorPoint({
          source: fixture.source,
          point: chainPoint(),
        }),
      ).rejects.toThrow("Kupo did not return an earlier ancestor checkpoint");
      expect(fixture.sockets).toHaveLength(0);
    },
  );

  it("refuses a genesis parent without inventing a predecessor point", async () => {
    const fixture = sourceFixture({ childAncestor: "genesis" });
    await expect(
      readAdmittedLocalKupmiosPredecessorPoint({
        source: fixture.source,
        point: chainPoint(),
      }),
    ).rejects.toThrow("local Kupmios child has no block predecessor");
    expect(fixture.sockets).toHaveLength(1);
  });

  it("refuses a parent that ceased to be canonical", async () => {
    const fixture = sourceFixture({
      checkpointOverride: (slot) =>
        slot === 380 ? { slot_no: 380, header_hash: hash(0xef) } : undefined,
    });
    await expect(
      readAdmittedLocalKupmiosPredecessorPoint({
        source: fixture.source,
        point: chainPoint(),
      }),
    ).rejects.toBeInstanceOf(LocalKupmiosExactPointNotCanonicalError);
  });

  it("preserves a changed response head during final canonical confirmation", async () => {
    let advanceHead = false;
    const fixture = sourceFixture({
      checkpointOverride: (slot) =>
        advanceHead && slot === 399
          ? { slot_no: 380, header_hash: ANCESTOR, headHash: hash(0xef) }
          : undefined,
    });
    await fixture.source.readBoundary();
    advanceHead = true;
    await expect(
      fixture.source.confirmCanonicalPoint({ point: chainPoint() }),
    ).rejects.toThrow(
      "Kupo advanced or rolled back during raw snapshot capture",
    );
  });

  it("propagates final canonical-read failure without inventing a rollback", async () => {
    let fail = false;
    const originalError = new Error(
      "ordinary final checkpoint transport failure",
    );
    const fixture = sourceFixture({
      beforeFetch: async (url) => {
        if (fail && url.endsWith("/checkpoints/399")) throw originalError;
      },
    });
    await fixture.source.readBoundary();
    fail = true;
    await expect(
      fixture.source.confirmCanonicalPoint({ point: chainPoint() }),
    ).rejects.toBe(originalError);
  });

  it("reports a successfully observed point mismatch as noncanonical", async () => {
    const behavior = { childHeight: 71 };
    const fixture = sourceFixture({ socketBehavior: behavior });
    await fixture.source.readBoundary();
    behavior.childHeight = 72;
    await expect(
      fixture.source.confirmCanonicalPoint({ point: chainPoint() }),
    ).resolves.toEqual({ canonical: false, point: chainPoint() });
  });

  it("preserves capture-head refusal during predecessor acquisition", async () => {
    const fixture = sourceFixture({
      checkpointOverride: (slot) =>
        slot === 380
          ? { slot_no: 380, header_hash: ANCESTOR, headHash: hash(0xef) }
          : undefined,
    });
    const result = readAdmittedLocalKupmiosPredecessorPoint({
      source: fixture.source,
      point: chainPoint(),
    });
    await expect(result).rejects.toThrow(
      "Kupo advanced or rolled back during raw snapshot capture",
    );
    await expect(result).rejects.not.toBeInstanceOf(
      LocalKupmiosExactPointNotCanonicalError,
    );
  });

  it.each([false, true])(
    "rechecks the child after parent acquisition with cached blocks: %s",
    async (cached) => {
      let refuse = false;
      let childReads = 0;
      const fixture = sourceFixture({
        checkpointOverride: (slot) =>
          refuse && slot === 400 && ++childReads === 3
            ? { slot_no: 400, header_hash: hash(0xef) }
            : undefined,
      });
      if (cached) {
        await readAdmittedLocalKupmiosPredecessorPoint({
          source: fixture.source,
          point: chainPoint(),
        });
      }
      refuse = true;
      const result = readAdmittedLocalKupmiosPredecessorPoint({
        source: fixture.source,
        point: chainPoint(),
      });
      await expect(result).rejects.toBeInstanceOf(
        LocalKupmiosExactPointNotCanonicalError,
      );
      await expect(result).rejects.toThrow(
        "Kupo rolled back during predecessor point capture",
      );
      expect(childReads).toBe(3);
      expect(fixture.sockets).toHaveLength(2);
    },
  );

  it.each([false, true])(
    "re-admits exact resolved transaction bytes from the concrete source (indefinite: %s)",
    async (indefinite) => {
      const address = credentialToAddress(
        "Preview",
        scriptHashToCredential("31".repeat(28)),
      );
      const outputs = CML.TransactionOutputList.new();
      outputs.add(
        CML.TransactionOutput.new(
          CML.Address.from_bech32(address),
          CML.Value.from_coin(3_000_000n),
        ),
      );
      const body = CML.TransactionBody.new(
        CML.TransactionInputList.new(),
        outputs,
        200_000n,
      );
      const bodyCbor = indefinite
        ? `bf${body.to_canonical_cbor_hex().slice(2)}ff`
        : body.to_canonical_cbor_hex();
      const witnessSetCbor = indefinite ? "bfff" : "a0";
      const transactionCbor = `84${bodyCbor}${witnessSetCbor}f5f6`;
      const txHash = CML.hash_transaction(
        CML.TransactionBody.from_cbor_hex(bodyCbor),
      ).to_hex();
      const kupoMatch = {
        transaction_index: 0,
        transaction_id: txHash,
        output_index: 0,
        address,
        value: { coins: "3000000", assets: {} },
        datum_hash: null,
        script_hash: null,
        created_at: { slot_no: 400, header_hash: TARGET },
        spent_at: null,
        datum: null,
        script: null,
      };
      const fixture = sourceFixture({
        blockTransactions: [{ id: txHash, cbor: transactionCbor }],
        kupoMatches: [kupoMatch],
      });
      const boundary = (await fixture.source.readBoundary()) as {
        readonly kupoCheckpoint: {
          readonly slot: string;
          readonly blockHash: string;
          readonly blockNo: string;
          readonly pointId: string;
        };
      };
      await expect(
        readAdmittedLocalKupmiosRawTransaction({
          source: fixture.source,
          txHash,
          expectedInclusionPoint: boundary.kupoCheckpoint,
          minimumConfirmationDepth: 30,
        }),
      ).resolves.toMatchObject({
        txHash,
        bodyCbor,
        witnessSetCbor,
        inclusionPoint: boundary.kupoCheckpoint,
        confirmationDepth: 30,
        resolvedInputs: [],
        resolvedReferenceInputs: [],
      });
      await expect(
        readAdmittedLocalKupmiosAddressUtxosAtPoint({
          source: fixture.source,
          address,
          point: boundary.kupoCheckpoint,
        }),
      ).resolves.toEqual([
        {
          outRef: `${txHash}#0`,
          outputCbor: outputs.get(0).to_canonical_cbor_hex(),
          datumCbor: null,
          referenceScriptCbor: null,
        },
      ]);
      await expect(
        readAdmittedLocalKupmiosTransactionInclusion({
          source: fixture.source,
          txHash,
        }),
      ).resolves.toEqual(boundary.kupoCheckpoint);
      await expect(
        pinAdmittedLocalKupmiosBoundaryAtPoint({
          source: fixture.source,
          point: boundary.kupoCheckpoint,
        }),
      ).resolves.toBeUndefined();
      await expect(
        readAdmittedLocalKupmiosUtxosByOutRefAtPoint({
          source: fixture.source,
          point: boundary.kupoCheckpoint,
          outRefs: [`${txHash}#0`],
        }),
      ).resolves.toMatchObject([{ outRef: `${txHash}#0` }]);
      const spent = sourceFixture({
        blockTransactions: [{ id: txHash, cbor: transactionCbor }],
        kupoMatches: [
          {
            ...kupoMatch,
            spent_at: {
              transaction_id: hash(19),
              input_index: 0,
              slot_no: 400,
              header_hash: TARGET,
            },
          },
        ],
      });
      await pinAdmittedLocalKupmiosBoundaryAtPoint({
        source: spent.source,
        point: boundary.kupoCheckpoint,
      });
      await expect(
        readAdmittedLocalKupmiosTransactionInclusion({
          source: spent.source,
          txHash,
        }),
      ).resolves.toEqual(boundary.kupoCheckpoint);
      await expect(
        readAdmittedLocalKupmiosUtxosByOutRefAtPoint({
          source: spent.source,
          point: boundary.kupoCheckpoint,
          outRefs: [`${txHash}#0`],
        }),
      ).resolves.toEqual([]);
      await expect(
        readAdmittedLocalKupmiosTransactionInclusion({
          source: { ...fixture.source },
          txHash,
        }),
      ).rejects.toThrow("admitted local Kupo/Ogmios history");
      await expect(
        pinAdmittedLocalKupmiosBoundaryAtPoint({
          source: fixture.source,
          point: {
            ...boundary.kupoCheckpoint,
            blockNo: "72",
            pointId: computeFraudProofRawL1PointId({
              ...boundary.kupoCheckpoint,
              blockNo: "72",
            }),
          },
        }),
      ).rejects.toThrow("differs from canonical");
      await expect(
        readAdmittedLocalKupmiosUnitHistoryAtPoint({
          source: fixture.source,
          unit: `${"12".repeat(28)}aa`,
          point: boundary.kupoCheckpoint,
        }),
      ).resolves.toEqual({
        checkpoint: boundary.kupoCheckpoint,
        transactions: [{ txHash, inclusionPoint: boundary.kupoCheckpoint }],
      });
      await expect(
        readAdmittedLocalKupmiosUnitHistoryAtPoint({
          source: { ...fixture.source },
          unit: `${"12".repeat(28)}aa`,
          point: boundary.kupoCheckpoint,
        }),
      ).rejects.toThrow(/requires the admitted local Kupo\/Ogmios source/u);
      await expect(
        readAdmittedLocalKupmiosRawTransaction({
          source: { ...fixture.source },
          txHash,
          expectedInclusionPoint: boundary.kupoCheckpoint,
          minimumConfirmationDepth: 30,
        }),
      ).rejects.toThrow(/requires the admitted local Kupo\/Ogmios source/u);
      await expect(
        readAdmittedLocalKupmiosRawTransaction({
          source: fixture.source,
          txHash,
          expectedInclusionPoint: boundary.kupoCheckpoint,
          minimumConfirmationDepth: 31,
        }),
      ).rejects.toThrow(/below release finality/u);
    },
  );

  it("keeps raw transaction CBOR enabled in every checked-in Ogmios launch path", async () => {
    const repository = resolve(process.cwd(), "../..");
    const paths = [
      "l1-services/docker-compose.yml",
      "demo/midgard-node/scripts/run-ogmios.sh",
      "demo/midgard-node-tools/devnet/phase4-process/compose.yaml",
    ];
    for (const path of paths) {
      await expect(
        readFile(resolve(repository, path), "utf8"),
      ).resolves.toContain(OGMIOS_RAW_TRANSACTION_CBOR_FLAG);
    }
  });
});

describe("release-bound fraud-proof economics V1", () => {
  const testnetPolicy = {
    profile: "bounded-acceptance-v1",
    requiredBondLovelace: "900000000",
    slashingPenaltyLovelace: "500000000",
    fraudProverRewardLovelace: "400000000",
    inactivitySlashingPenaltyLovelace: "100000000",
    proverCollateralFloorLovelace: "5000000",
  } as const;

  it("admits the manifest-bound testnet profile", () => {
    expect(
      validateVerifiedFraudProofReleaseEconomicsPolicy({
        schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
        deploymentIdentityDigest: DEPLOYMENT,
        blueprintHash: RELEASE,
        policyDigest:
          computeFraudProofReleaseEconomicsPolicyDigest(testnetPolicy),
        policy: testnetPolicy,
      }),
    ).toMatchObject({ policy: testnetPolicy });
  });

  it("rejects a caller-selected reward or substituted policy digest", () => {
    const policy = { ...testnetPolicy, fraudProverRewardLovelace: "1" };
    expect(() =>
      validateVerifiedFraudProofReleaseEconomicsPolicy({
        schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
        deploymentIdentityDigest: DEPLOYMENT,
        blueprintHash: RELEASE,
        policyDigest: computeFraudProofReleaseEconomicsPolicyDigest(policy),
        policy,
      }),
    ).toThrow(/must equal|canonical launch profile/u);
  });

  it("rejects legacy or extended economics policy shapes", () => {
    const { proverCollateralFloorLovelace: _omitted, ...legacyPolicy } =
      testnetPolicy;
    for (const policy of [
      legacyPolicy,
      { ...testnetPolicy, extra: "forged" },
    ]) {
      expect(() =>
        validateVerifiedFraudProofReleaseEconomicsPolicy({
          schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
          deploymentIdentityDigest: DEPLOYMENT,
          blueprintHash: RELEASE,
          policyDigest: "00".repeat(32),
          policy,
        } as never),
      ).toThrow(/must contain exactly/u);
    }
  });
});

describe("captured reference-body reader bounds", () => {
  it("owns cold caches per operation and preserves an existing target cache", async () => {
    const fixture = sourceFixture();
    const args = { source: fixture.source, point: chainPoint() };
    const first = await readAdmittedLocalKupmiosReferenceBodiesAtPoint(args);
    expect(first.targetBlock.transactions).toEqual([]);
    expect(first.creatingTransactionBodies).toEqual([]);
    expect(fixture.sockets).toHaveLength(1);
    await expect(
      readAdmittedLocalKupmiosReferenceBodiesAtPoint(args),
    ).resolves.toEqual(first);
    expect(fixture.sockets).toHaveLength(2);
    await readAdmittedLocalKupmiosRawBlockAtPoint(args);
    expect(fixture.sockets).toHaveLength(3);
    await expect(
      readAdmittedLocalKupmiosReferenceBodiesAtPoint(args),
    ).resolves.toEqual(first);
    await expect(
      readAdmittedLocalKupmiosRawBlockAtPoint(args),
    ).resolves.toEqual(first.targetBlock);
    expect(fixture.sockets).toHaveLength(3);
    expect(fixture.requests.some(({ url }) => url.includes("/matches/"))).toBe(
      false,
    );
    expect(
      fixture.requests.some(({ url }) => url === "http://127.0.0.1:1337"),
    ).toBe(false);
    for (const value of [
      first,
      first.targetBlock,
      first.targetBlock.point,
      first.targetBlock.kupoCheckpoint,
      first.targetBlock.transactions,
      first.creatingTransactionBodies,
    ])
      expect(Object.isFrozen(value)).toBe(true);
    await expect(
      readAdmittedLocalKupmiosReferenceBodiesAtPoint({
        ...args,
        source: { ...fixture.source },
      }),
    ).rejects.toThrow("admitted local Kupo/Ogmios source");
    Object.assign(fixture.source, {
      readBlockAtPoint: vi.fn(() => {
        throw new Error("substituted public method");
      }),
    });
    await expect(
      readAdmittedLocalKupmiosReferenceBodiesAtPoint(args),
    ).resolves.toEqual(first);
    expect(fixture.sockets.every(({ closeCount }) => closeCount === 1)).toBe(
      true,
    );
  });

  it.each([
    [
      LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.targetTransactions + 1,
      "target transaction count",
    ],
    [
      LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.inspectedMembers + 1,
      "inspected-member budget",
    ],
  ] as const)(
    "refuses %s transport entries before transaction decoding",
    async (count, reason) => {
      // Collection-shape data only; no CBOR or transaction is constructed.
      const fixture = sourceFixture({
        blockTransactions: Array.from({ length: count }, () => null),
      });
      await expect(
        readAdmittedLocalKupmiosReferenceBodiesAtPoint({
          source: fixture.source,
          point: chainPoint(),
        }),
      ).rejects.toThrow(reason);
      expect(fixture.sockets.every(({ closeCount }) => closeCount === 1)).toBe(
        true,
      );
    },
  );

  it.each([false, true])(
    "shares HTTP byte budget across responses (body-null fallback: %s)",
    async (fallback) => {
      const padding = " ".repeat(
        LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.responseBytes / 2,
      );
      const fixture = sourceFixture({
        fetchOverride: async (url) => {
          const target = url.endsWith("/400");
          const text =
            padding +
            JSON.stringify({
              slot_no: target ? 400 : 380,
              header_hash: target ? TARGET : ANCESTOR,
            });
          const headers = {
            "x-most-recent-checkpoint": "990",
            etag: KUP0_HEAD,
          };
          if (!fallback) return new Response(text, { headers });
          const bytes = new TextEncoder().encode(text);
          const value = new Response(null, { headers });
          Object.defineProperty(value, "arrayBuffer", {
            value: async () => bytes.buffer,
          });
          return value;
        },
      });
      await expect(
        readAdmittedLocalKupmiosReferenceBodiesAtPoint({
          source: fixture.source,
          point: chainPoint(),
        }),
      ).rejects.toThrow(fallback ? "byte budget" : "byte bound");
      expect(fixture.requests).toHaveLength(2);
      expect(fixture.sockets).toHaveLength(0);
    },
    30_000,
  );

  it("shares HTTP/WS budget and counts text frames with unrecognized IDs", async () => {
    const padding = " ".repeat(
      LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.responseBytes - 1024 * 1024,
    );
    const fixture = sourceFixture({
      fetchOverride: async (url) => {
        const target = url.endsWith("/400");
        return new Response(
          (target ? padding : "") +
            JSON.stringify({
              slot_no: target ? 400 : 380,
              header_hash: target ? TARGET : ANCESTOR,
            }),
          {
            headers: {
              "x-most-recent-checkpoint": "990",
              etag: KUP0_HEAD,
            },
          },
        );
      },
      socketBehavior: {
        responseText:
          " ".repeat(2 * 1024 * 1024) +
          JSON.stringify({ jsonrpc: "2.0", id: 999, result: null }),
      },
    });
    await expect(
      readAdmittedLocalKupmiosReferenceBodiesAtPoint({
        source: fixture.source,
        point: chainPoint(),
      }),
    ).rejects.toThrow("reference acquisition response byte budget");
    expect(fixture.sockets).toHaveLength(1);
    expect(fixture.sockets[0]!.closeCount).toBe(1);
  }, 30_000);

  it("cancels its actual pending reader and permits no later receipt read", async () => {
    const controller = new AbortController();
    const fixture = sourceFixture({
      signal: controller.signal,
      socketBehavior: { respond: false },
    });
    const pending = readAdmittedLocalKupmiosReferenceBodiesAtPoint({
      source: fixture.source,
      point: chainPoint(),
    });
    const outcome = pending.catch((error: unknown) => error);
    await fixture.socketCreated;
    controller.abort();
    expect(await outcome).toMatchObject({ name: "AbortError" });
    expect(fixture.sockets[0]!.closeCount).toBe(1);
    await expect(
      readAdmittedLocalKupmiosReferenceBodiesAtPoint({
        source: fixture.source,
        point: chainPoint(),
      }),
    ).rejects.toThrow("aborted");
  });
});

const signedRecoveryFixture = async ({
  ttl = 1200,
  spent = false,
  referenceSpent,
  scriptOrdinary = false,
  scriptCollateral = false,
  keyCollateral = false,
  missing = false,
  mempoolPresent = false,
  included = false,
  rollbackDuringInclusion = false,
  rollbackDuringExpiry = false,
  captureHeadChanges = 0,
}: {
  ttl?: number | null;
  spent?: boolean;
  referenceSpent?:
    | "stable"
    | "volatile"
    | "mixed_stable_first"
    | "mixed_volatile_first";
  scriptOrdinary?: boolean;
  scriptCollateral?: boolean;
  keyCollateral?: boolean;
  missing?: boolean;
  mempoolPresent?: boolean;
  included?: boolean;
  rollbackDuringInclusion?: boolean;
  rollbackDuringExpiry?: boolean;
  captureHeadChanges?: number;
} = {}) => {
  const account = generateEmulatorAccount({ lovelace: 1_000_000_000n });
  const emulator = new Emulator([account]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const mixedReferences = referenceSpent?.startsWith("mixed_") === true;
  const volatileReference =
    referenceSpent !== undefined && referenceSpent !== "stable";
  const protocolScript = scriptFromNative({
    type: "sig",
    keyHash: paymentCredentialOf(account.address).hash,
  });
  const protocolAddress = validatorToAddress("Custom", protocolScript);
  const creationBuilder = lucid
    .newTx()
    .pay.ToAddress(scriptOrdinary ? protocolAddress : account.address, {
      lovelace: 10_000_000n,
    });
  if (mixedReferences)
    creationBuilder.pay.ToAddress(account.address, { lovelace: 20_000_000n });
  const creation = await (
    await creationBuilder.complete({ localUPLCEval: true })
  ).sign
    .withWallet()
    .complete();
  await creation.submit();
  emulator.awaitBlock();
  const createdOutputs = [
    ...(await lucid.wallet().getUtxos()),
    ...(scriptOrdinary ? await lucid.utxosAt(protocolAddress) : []),
  ].filter((utxo) => utxo.txHash === creation.toHash());
  const funding = createdOutputs.find((utxo) =>
    referenceSpent === undefined
      ? utxo.assets.lovelace === 10_000_000n
      : utxo.assets.lovelace !== 10_000_000n &&
        (!mixedReferences || utxo.assets.lovelace !== 20_000_000n),
  )!;
  // The reference sorts before funding so recovery must inspect later wallet
  // creation history even after observing a stable invalidating spend.
  const reference =
    referenceSpent === undefined
      ? undefined
      : createdOutputs.find((utxo) => utxo.assets.lovelace === 10_000_000n)!;
  const secondReference = mixedReferences
    ? createdOutputs.find((utxo) => utxo.assets.lovelace === 20_000_000n)!
    : undefined;
  const planned = lucid
    .newTx()
    .collectFrom([funding])
    .pay.ToAddress(account.address, { lovelace: 5_000_000n });
  if (reference !== undefined) {
    if (scriptOrdinary)
      planned
        .collectFrom([reference])
        .attach.SpendingValidator(protocolScript)
        .addSigner(account.address);
    else if (!keyCollateral) planned.readFrom([reference]);
  }
  if (secondReference !== undefined) planned.readFrom([secondReference]);
  if (ttl !== null) planned.validTo(lucid.slotToUnixTime(ttl));
  let signed = await (
    await planned.complete({
      localUPLCEval: true,
      coinSelection: false,
      presetWalletInputs: [funding],
    })
  ).sign
    .withWallet()
    .complete();
  if (scriptCollateral || keyCollateral) {
    // Include recorded collateral in recovery, even when its role overlaps an
    // ordinary input. Every exact input can establish that the body is impossible.
    const body = signed.toTransaction().body();
    const collateral = CML.TransactionInputList.new();
    collateral.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(reference!.txHash),
        BigInt(reference!.outputIndex),
      ),
    );
    body.set_collateral_inputs(collateral);
    signed = await lucid
      .fromTx(
        CML.Transaction.new(
          body,
          CML.TransactionWitnessSet.new(),
          true,
        ).to_cbor_hex(),
      )
      .sign.withWallet()
      .complete();
  }
  const conflictInputs =
    reference === undefined
      ? [funding]
      : [...(spent ? [funding] : []), reference];
  const conflictBuilder = lucid
    .newTx()
    .collectFrom(conflictInputs)
    .pay.ToAddress(account.address, { lovelace: 6_000_000n });
  if (scriptOrdinary)
    conflictBuilder.attach
      .SpendingValidator(protocolScript)
      .addSigner(account.address);
  const conflict = await (
    await conflictBuilder.complete({
      localUPLCEval: true,
      coinSelection: false,
      presetWalletInputs: conflictInputs,
    })
  ).sign
    .withWallet()
    .complete();
  const secondConflict =
    secondReference === undefined
      ? undefined
      : await (
          await lucid
            .newTx()
            .collectFrom([secondReference])
            .pay.ToAddress(account.address, { lovelace: 6_000_000n })
            .complete({
              localUPLCEval: true,
              coinSelection: false,
              presetWalletInputs: [secondReference],
            })
        ).sign
          .withWallet()
          .complete();
  const match = {
    transaction_index: 0,
    transaction_id: creation.toHash(),
    output_index: funding.outputIndex,
    address: funding.address,
    value: { coins: funding.assets.lovelace!.toString(), assets: {} },
    datum_hash: null,
    script_hash: null,
    datum: null,
    script: null,
    created_at: mixedReferences
      ? { slot_no: 380, header_hash: ANCESTOR }
      : { slot_no: 400, header_hash: TARGET },
    spent_at: spent
      ? {
          slot_no: mixedReferences ? 380 : 400,
          header_hash: mixedReferences ? ANCESTOR : TARGET,
          transaction_id: conflict.toHash(),
          input_index: 0,
        }
      : null,
  };
  const referenceMatch =
    reference === undefined
      ? undefined
      : {
          ...match,
          output_index: reference.outputIndex,
          address: reference.address,
          value: { coins: reference.assets.lovelace!.toString(), assets: {} },
          spent_at: {
            slot_no: referenceSpent === "mixed_stable_first" ? 380 : 400,
            header_hash:
              referenceSpent === "mixed_stable_first" ? ANCESTOR : TARGET,
            transaction_id: conflict.toHash(),
            input_index: 0,
          },
        };
  const secondReferenceMatch =
    secondReference === undefined
      ? undefined
      : {
          ...match,
          output_index: secondReference.outputIndex,
          value: {
            coins: secondReference.assets.lovelace!.toString(),
            assets: {},
          },
          spent_at: {
            slot_no: referenceSpent === "mixed_volatile_first" ? 380 : 400,
            header_hash:
              referenceSpent === "mixed_volatile_first" ? ANCESTOR : TARGET,
            transaction_id: secondConflict!.toHash(),
            input_index: 0,
          },
        };
  const submissions: string[] = [];
  let inclusionRead = false;
  let inclusionChecks = 0;
  const includedMatches = Array.from(
    { length: signed.toTransaction().body().outputs().len() },
    (_, index) => {
      const output = signed.toTransaction().body().outputs().get(index);
      return {
        ...match,
        transaction_id: signed.toHash(),
        output_index: index,
        value: { coins: output.amount().coin().toString(), assets: {} },
        spent_at: null,
      };
    },
  );
  const fixture = sourceFixture({
    observationDepth: "inclusion",
    tipHeight: volatileReference ? 99 : 100,
    tipSlot: volatileReference ? 620 : 1000,
    blockTransactions: [
      creation,
      conflict,
      ...(secondConflict === undefined ? [] : [secondConflict]),
      signed,
    ].map((tx) => ({
      id: tx.toHash(),
      cbor: tx.toTransaction().to_cbor_hex(),
    })),
    checkpointOverride: (slot) => {
      const tipSlot = volatileReference ? 620 : 1000;
      if (slot >= tipSlot) return { slot_no: tipSlot, header_hash: TIP };
      if (
        slot === 400 &&
        inclusionRead &&
        (rollbackDuringExpiry ||
          (rollbackDuringInclusion && ++inclusionChecks >= 2))
      )
        return { slot_no: 400, header_hash: hash(99) };
      return undefined;
    },
    matchesByPattern: (pattern) => {
      if (pattern === `*@${signed.toHash()}` && captureHeadChanges-- > 0)
        throw new LocalKupmiosCheckpointChangedError(
          "Kupo advanced during transaction inclusion capture",
        );
      if (pattern === `*@${signed.toHash()}`) {
        inclusionRead = true;
        return included ? includedMatches : [];
      }
      if (
        reference !== undefined &&
        pattern === `${reference.outputIndex}@${creation.toHash()}`
      )
        return [referenceMatch];
      if (
        secondReference !== undefined &&
        pattern === `${secondReference.outputIndex}@${creation.toHash()}`
      )
        return [secondReferenceMatch];
      return pattern === `${funding.outputIndex}@${creation.toHash()}` &&
        !missing
        ? [match]
        : [];
    },
    socketBehavior: {
      mempoolPresent,
      submit: async (cbor) => {
        submissions.push(cbor);
        return emulator.submitTx(cbor);
      },
    },
  });
  const input = {
    source: fixture.source,
    transactionHash: signed.toHash(),
    signedTransactionCborHex: signed.toTransaction().to_cbor_hex(),
  };
  return {
    ...fixture,
    input,
    signed,
    funding,
    reference,
    lucid,
    emulator,
    submissions,
  };
};

describe("production signed intent recovery through concrete Kupo/Ogmios transports", () => {
  it.each([
    [{ referenceSpent: "stable", scriptOrdinary: true }, "invalidated"],
    [{ referenceSpent: "volatile", scriptOrdinary: true }, "pending"],
    [
      { referenceSpent: "stable", scriptOrdinary: true, spent: true },
      "invalidated",
    ],
    [
      { referenceSpent: "stable", scriptOrdinary: true, missing: true },
      "unknown",
    ],
    [
      {
        referenceSpent: "stable",
        scriptOrdinary: true,
        scriptCollateral: true,
      },
      "invalidated",
    ],
    [
      { referenceSpent: "mixed_stable_first", scriptOrdinary: true },
      "invalidated",
    ],
    [
      { referenceSpent: "mixed_volatile_first", scriptOrdinary: true },
      "invalidated",
    ],
    [
      {
        referenceSpent: "mixed_stable_first",
        scriptOrdinary: true,
        spent: true,
      },
      "invalidated",
    ],
  ] as const)(
    "retires impossible signed attempts without treating input roles as objective failures: %j",
    async (options, status) => {
      const fixture = await signedRecoveryFixture(options);
      expect(fixture.reference!.outputIndex).toBeLessThan(
        fixture.funding.outputIndex,
      );
      expect(
        CML.Address.from_bech32(fixture.reference!.address)
          .payment_cred()
          ?.as_script(),
      ).toBeDefined();
      const result = await readAdmittedLocalKupmiosSignedTransactionRecovery(
        fixture.input,
      );
      expect(result.status).toBe(status);
      if (status === "invalidated")
        expect(result.inputs.map(({ outRef }) => outRef)).toContain(
          `${fixture.funding.txHash}#${fixture.funding.outputIndex}`,
        );
      expect(fixture.submissions).toEqual([]);
    },
  );

  it.each([
    [{ referenceSpent: "stable", keyCollateral: true }, "invalidated"],
    [{ referenceSpent: "volatile", keyCollateral: true }, "pending"],
    [
      { referenceSpent: "stable", keyCollateral: true, missing: true },
      "unknown",
    ],
    [{ referenceSpent: "stable" }, "invalidated"],
    [{ referenceSpent: "stable", ttl: null }, "invalidated"],
    [{ referenceSpent: "volatile", spent: true }, "pending"],
    [
      { referenceSpent: "mixed_volatile_first", keyCollateral: true },
      "invalidated",
    ],
    [{ referenceSpent: "volatile" }, "pending"],
    [{ referenceSpent: "mixed_stable_first" }, "invalidated"],
    [{ referenceSpent: "mixed_volatile_first" }, "invalidated"],
    [{ referenceSpent: "mixed_stable_first", spent: true }, "invalidated"],
    [{ referenceSpent: "stable", spent: true }, "invalidated"],
    [{ referenceSpent: "stable", missing: true }, "unknown"],
  ] as const)(
    "authenticates mixed reference and funding spends before retiring an attempt: %j",
    async (options, status) => {
      const fixture = await signedRecoveryFixture(options);
      expect(fixture.reference!.outputIndex).toBeLessThan(
        fixture.funding.outputIndex,
      );
      const result = await readAdmittedLocalKupmiosSignedTransactionRecovery(
        fixture.input,
      );
      expect(result.status).toBe(status);
      if (status === "invalidated")
        expect(result.inputs).toHaveLength(
          options.referenceSpent.startsWith("mixed_") ? 3 : 2,
        );
      expect(fixture.submissions).toEqual([]);
    },
  );

  it("authenticates release-final expiry for a signed transaction never submitted", async () => {
    const fixture = await signedRecoveryFixture({ ttl: 399 });
    const observed = await readAdmittedLocalKupmiosSignedTransactionRecovery(
      fixture.input,
    );
    expect(observed.status).toBe("expired");
    expect(observed.releaseFinalPoint.slot).toBe("400");
    expect(fixture.submissions).toEqual([]);
    expect(await fixture.lucid.utxosByOutRef([fixture.funding])).toHaveLength(
      1,
    );
  });

  it.each([
    [{ ttl: 399, missing: true }, "expired"],
    [{ ttl: 399, missing: true, referenceSpent: "stable" }, "expired"],
    [{ ttl: 900, missing: true }, "unknown"],
    [{ ttl: 399, missing: true, included: true }, "included"],
  ] as const)(
    "recovers dependent attempts after a parent rollback only with stable expiry or inclusion: %j",
    async (options, status) => {
      const fixture = await signedRecoveryFixture(options);
      const observed = await readAdmittedLocalKupmiosSignedTransactionRecovery(
        fixture.input,
      );
      expect(observed.status).toBe(status);
      expect(fixture.submissions).toEqual([]);
    },
  );

  it("distinguishes canonical expiry from merely passing TTL at the current tip", async () => {
    const fixture = await signedRecoveryFixture({ ttl: 900 });
    expect(
      (await readAdmittedLocalKupmiosSignedTransactionRecovery(fixture.input))
        .status,
    ).toBe("pending");
    expect(fixture.submissions).toEqual([]);
  });

  it("rejects stable expiry when its canonical boundary rolls back during observation", async () => {
    const fixture = await signedRecoveryFixture({
      ttl: 399,
      missing: true,
      rollbackDuringExpiry: true,
    });
    await expect(
      readAdmittedLocalKupmiosSignedTransactionRecovery(fixture.input),
    ).rejects.toThrow();
    expect(fixture.submissions).toEqual([]);
  });

  it.each([1200, null])(
    "rebroadcasts the exact signed bytes only after authorization and the emulator accepts them (TTL: %s)",
    async (ttl) => {
      const fixture = await signedRecoveryFixture({ ttl });
      expect(
        (await readAdmittedLocalKupmiosSignedTransactionRecovery(fixture.input))
          .status,
      ).toBe("rebroadcast");
      const authorize = vi.fn(async (input) => {
        expect(input.signedTransactionCborHex).toBe(
          fixture.input.signedTransactionCborHex,
        );
        expect(fixture.submissions).toEqual([]);
      });
      expect(
        await rebroadcastAdmittedLocalKupmiosSignedTransaction({
          ...fixture.input,
          authorizeResubmission: authorize,
        }),
      ).toBe(fixture.signed.toHash());
      expect(authorize).toHaveBeenCalledOnce();
      expect(fixture.submissions).toEqual([
        fixture.input.signedTransactionCborHex,
      ]);
      fixture.emulator.awaitBlock();
      expect(await fixture.lucid.utxosByOutRef([fixture.funding])).toEqual([]);
    },
  );

  it("never broadcasts when the live authorization check refuses", async () => {
    const fixture = await signedRecoveryFixture();
    await expect(
      rebroadcastAdmittedLocalKupmiosSignedTransaction({
        ...fixture.input,
        authorizeResubmission: async () => {
          throw new Error("read-only reconciliation");
        },
      }),
    ).rejects.toThrow("read-only reconciliation");
    expect(fixture.submissions).toEqual([]);
  });

  it.each([
    [{ spent: true }, "invalidated"],
    [{ missing: true }, "unknown"],
    [{ ttl: null }, "rebroadcast"],
    [{ ttl: null, mempoolPresent: true }, "pending"],
    [{ ttl: null, missing: true }, "unknown"],
    [{ ttl: null, spent: true }, "invalidated"],
    [{ ttl: null, included: true }, "included"],
    [{ mempoolPresent: true }, "pending"],
  ] as const)(
    "distinguishes safely retired attempts from unresolved attempts: %j",
    async (options, status) => {
      const fixture = await signedRecoveryFixture(options);
      expect(
        (await readAdmittedLocalKupmiosSignedTransactionRecovery(fixture.input))
          .status,
      ).toBe(status);
      expect(fixture.submissions).toEqual([]);
    },
  );
});

it("rechecks canonicality before returning an included signed transaction", async () => {
  const included = await signedRecoveryFixture({ included: true });
  expect(
    (await readAdmittedLocalKupmiosSignedTransactionRecovery(included.input))
      .status,
  ).toBe("included");
  const rolledBack = await signedRecoveryFixture({
    included: true,
    rollbackDuringInclusion: true,
  });
  await expect(
    readAdmittedLocalKupmiosSignedTransactionRecovery(rolledBack.input),
  ).rejects.toThrow();
});

it.each([
  [{ referenceSpent: "stable", scriptOrdinary: true }, "not_found"],
  [{ referenceSpent: "volatile", scriptOrdinary: true }, "pending"],
  [
    { referenceSpent: "stable", scriptOrdinary: true, spent: true },
    "not_found",
  ],
  [
    { referenceSpent: "stable", scriptOrdinary: true, scriptCollateral: true },
    "not_found",
  ],
  [
    { referenceSpent: "stable", scriptOrdinary: true, missing: true },
    "unknown",
  ],
  [{ referenceSpent: "stable", keyCollateral: true }, "not_found"],
  [{ referenceSpent: "volatile", keyCollateral: true }, "pending"],
  [{ referenceSpent: "stable" }, "not_found"],
  [{ referenceSpent: "stable", ttl: null }, "not_found"],
  [{ referenceSpent: "volatile", spent: true }, "pending"],
  [{ referenceSpent: "volatile" }, "pending"],
  [{ referenceSpent: "stable", spent: true }, "not_found"],
  [{ referenceSpent: "stable", missing: true }, "unknown"],
  [{ ttl: 399 }, "not_found"],
  [{ missing: true }, "unknown"],
  [{ ttl: null }, "pending"],
  [{ ttl: null, mempoolPresent: true }, "pending"],
  [{ ttl: null, missing: true }, "unknown"],
  [{ ttl: null, included: true }, "pending"],
  [{ ttl: null, spent: true }, "not_found"],
  [{ spent: true }, "not_found"],
  [{}, "pending"],
] as const)(
  "cursor and linear production adapters reconcile real signed source evidence: %j",
  async (options, outcome) => {
    for (const family of ["cursor", "linear"] as const) {
      const fixture = await signedRecoveryFixture(options);
      const protocolRemoval =
        "scriptOrdinary" in options && options.scriptOrdinary;
      const proofOutRef = `${hash(78)}#0`;
      const target = fixture.reference ?? fixture.funding;
      let currentHeaderOutRef = `${target.txHash}#${target.outputIndex}`;
      const baseL1 = {
        portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
        publications: {
          observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
          observeExact: async (): Promise<never> => {
            throw new Error("unused publication observer");
          },
        },
        observeHeader: async (): Promise<never> => {
          throw new Error("unused header observer");
        },
        transactionConfirmed: async () => false,
        observe: async () => ({
          provenance: {
            trustClass: "authenticated_cardano_l1",
            sourceId: "local-kupmios",
            grade: "security",
          } as const,
          stage: protocolRemoval
            ? ({
                kind: "proof_token",
                fraudProofOutRef: proofOutRef,
                stateQueueBlockOutRef: currentHeaderOutRef,
                nextRemovalOutRef: currentHeaderOutRef,
              } as const)
            : ({
                kind: "not_started",
                stateQueueBlockOutRef: currentHeaderOutRef,
              } as const),
        }),
        observeSignedTransaction: (input: SignedWorkflowTransaction) =>
          readAdmittedLocalKupmiosSignedTransactionRecovery({
            ...input,
            source: fixture.source,
          }),
        rebroadcastSignedTransaction: (
          input: SignedWorkflowTransaction & {
            authorizeResubmission: (
              input: SignedWorkflowTransaction,
            ) => Promise<void>;
          },
        ) =>
          rebroadcastAdmittedLocalKupmiosSignedTransaction({
            ...input,
            source: fixture.source,
          }),
      };
      const prepare = async (): Promise<never> => {
        throw new Error("recovery must not prepare new evidence");
      };
      const capture = async (): Promise<never> => {
        throw new Error("recovery must not rebuild signed transaction");
      };
      const stateQueueMutationLeaseCoordinator = {
        acquire: async (): Promise<never> => {
          throw new Error("unused lease");
        },
      };
      const adapter =
        family === "cursor"
          ? createCursorFamilyWorkflowAdapter({
              spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
              l1: {
                ...baseL1,
                category: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC.category,
              },
              transactions: {
                portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
                category: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC.category,
                prepare,
                capture,
              },
              stateQueueMutationLeaseCoordinator,
            })
          : createLinearFamilyWorkflowAdapter({
              category: "daHashPreimage",
              l1: { ...baseL1, category: "daHashPreimage" },
              transactions: {
                portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
                category: "daHashPreimage",
                prepare,
                capture,
              },
              stateQueueMutationLeaseCoordinator,
            });
      const context = {
        identity: {
          schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
          deploymentFingerprint: DEPLOYMENT,
          category: adapter.category,
          target: { kind: "state_queue_header", headerHash: "aa".repeat(28) },
        } as const,
        workflowId: hash(88),
        artifact: {},
        entries: [],
      };
      const observed = await adapter.observe(context);
      if (observed.kind !== "action_required")
        throw new Error("Expected predecessor action");
      // A DA attachment or linked-list update recreates the same HeaderV1
      // while its signed init/removal still uses the old exact output.
      if (fixture.reference !== undefined)
        currentHeaderOutRef = `${hash(77)}#0`;
      const authorizeResubmission = vi.fn(async () => {});
      expect(
        (
          await adapter.reconcile({
            ...context,
            action: observed.action,
            txHash: fixture.input.transactionHash,
            signedTransactionCborHex: fixture.input.signedTransactionCborHex,
            authorizeResubmission,
          })
        ).kind,
      ).toBe(outcome);
      if (protocolRemoval && outcome === "not_found") {
        const replacement = await adapter.observe(context);
        if (replacement.kind !== "action_required")
          throw new Error("missing replacement removal");
        expect(replacement.action.actionId).not.toBe(observed.action.actionId);
        expect(replacement.action.input).toMatchObject({
          stage: "remove",
          fraudProofOutRef: proofOutRef,
          stateQueueBlockOutRef: currentHeaderOutRef,
          nextRemovalOutRef: currentHeaderOutRef,
        });
      }
      if (
        Object.keys(options).length === 0 ||
        ("ttl" in options &&
          options.ttl === null &&
          Object.keys(options).length === 1)
      ) {
        expect(authorizeResubmission).toHaveBeenCalledOnce();
        expect(fixture.submissions).toEqual([
          fixture.input.signedTransactionCborHex,
        ]);
      } else expect(fixture.submissions).toEqual([]);
    }
  },
);

it("restarts at most three complete boundary captures after typed head changes", async () => {
  for (const changes of [1, 2, 3]) {
    const fixture = sourceFixture();
    const readBoundary = fixture.source.readBoundary.bind(fixture.source);
    let remaining = changes;
    const capture = vi
      .spyOn(fixture.source, "readBoundary")
      .mockImplementation(async () => {
        if (remaining > 0) {
          remaining -= 1;
          throw new LocalKupmiosCheckpointChangedError(
            "Kupo advanced or rolled back during raw snapshot capture: test",
          );
        }
        return await readBoundary();
      });
    const result = readAdmittedLocalKupmiosBoundary({
      source: fixture.source,
    });
    if (changes < 3)
      await expect(result).resolves.toMatchObject({ confirmationDepth: 30 });
    else
      await expect(result).rejects.toBeInstanceOf(
        LocalKupmiosCheckpointChangedError,
      );
    expect(capture).toHaveBeenCalledTimes(Math.min(changes + 1, 3));
  }
});

it("propagates a boundary failure that is not a typed head change unchanged", async () => {
  const fixture = sourceFixture();
  const failure = new Error("ordinary boundary transport failure");
  const capture = vi
    .spyOn(fixture.source, "readBoundary")
    .mockRejectedValue(failure);
  await expect(
    readAdmittedLocalKupmiosBoundary({ source: fixture.source }),
  ).rejects.toBe(failure);
  expect(capture).toHaveBeenCalledOnce();
});

it("restarts at most three complete signed-recovery captures after typed head changes", async () => {
  for (const changes of [2, 3]) {
    const fixture = await signedRecoveryFixture({
      ttl: 399,
      captureHeadChanges: changes,
    });
    const capture = vi.spyOn(fixture.source, "readBoundary");
    const result = readAdmittedLocalKupmiosSignedTransactionRecovery(
      fixture.input,
    );
    if (changes === 2)
      await expect(result).resolves.toMatchObject({ status: "expired" });
    else
      await expect(result).rejects.toBeInstanceOf(
        LocalKupmiosCheckpointChangedError,
      );
    expect(capture).toHaveBeenCalledTimes(3);
    expect(fixture.submissions).toEqual([]);
  }
});

describe("typed raw-source transport failures", () => {
  it.each([429, 500, 502, 503, 504])(
    "marks HTTP %i temporary without accepting response data",
    async (status) => {
      const fixture = sourceFixture({
        fetchOverride: async () =>
          new Response("temporarily unavailable", { status }),
      });
      await expect(fixture.source.readBoundary()).rejects.toBeInstanceOf(
        LocalKupmiosTransportUnavailableError,
      );
    },
  );

  it.each([400, 401, 403, 404])(
    "keeps HTTP %i refusal hard",
    async (status) => {
      const fixture = sourceFixture({
        fetchOverride: async () => new Response("refused", { status }),
      });
      const error = await fixture.source
        .readBoundary()
        .catch((cause: unknown) => cause);
      expect(error).toBeInstanceOf(Error);
      expect(error).not.toBeInstanceOf(LocalKupmiosTransportUnavailableError);
    },
  );

  it("classifies structured network failure but never a message substring", async () => {
    const network = Object.assign(new TypeError("fetch failed"), {
      cause: Object.assign(new Error("socket ended"), { code: "ECONNRESET" }),
    });
    const transport = sourceFixture({
      fetchOverride: async () => {
        throw network;
      },
    });
    await expect(transport.source.readBoundary()).rejects.toMatchObject({
      name: "LocalKupmiosTransportUnavailableError",
      cause: network,
    });
    const ordinary = new Error("ECONNRESET malformed checkpoint");
    const malformed = sourceFixture({
      fetchOverride: async () => {
        throw ordinary;
      },
    });
    await expect(malformed.source.readBoundary()).rejects.toBe(ordinary);
  });

  it("distinguishes internal HTTP timeout from caller cancellation", async () => {
    const fetchOverride: FraudProofRawL1Fetch = async (_url, init) =>
      new Promise((_resolve, reject) => {
        init!.signal!.addEventListener(
          "abort",
          () => reject(new DOMException("request aborted", "AbortError")),
          { once: true },
        );
      });
    await expect(
      sourceFixture({ fetchOverride, timeoutMs: 25 }).source.readBoundary(),
    ).rejects.toBeInstanceOf(LocalKupmiosTransportUnavailableError);
    const controller = new AbortController();
    const fixture = sourceFixture({
      fetchOverride,
      signal: controller.signal,
      timeoutMs: 500,
    });
    const outcome = fixture.source
      .readBoundary()
      .catch((cause: unknown) => cause);
    await vi.waitFor(() => expect(fixture.requests.length).toBeGreaterThan(0));
    controller.abort();
    expect(await outcome).toMatchObject({ name: "AbortError" });
    expect(await outcome).not.toBeInstanceOf(
      LocalKupmiosTransportUnavailableError,
    );
  });

  it.each([
    { label: "JSON", fetchOverride: async () => new Response("{broken") },
    {
      label: "checkpoint headers",
      fetchOverride: async () =>
        new Response("{}", {
          headers: { "x-most-recent-checkpoint": "no", etag: "bad" },
        }),
    },
    {
      label: "byte budget",
      fetchOverride: async () =>
        new Response("failure", {
          status: 503,
          headers: { "content-length": "67108865" },
        }),
    },
  ])(
    "keeps $label failure hard even when transport is available",
    async ({ fetchOverride }) => {
      const error = await sourceFixture({ fetchOverride })
        .source.readBoundary()
        .catch((cause: unknown) => cause);
      expect(error).toBeInstanceOf(Error);
      expect(error).not.toBeInstanceOf(LocalKupmiosTransportUnavailableError);
    },
  );

  it.each(["error", "close"])("types a WebSocket %s event", async (event) => {
    const fixture = sourceFixture({
      timeoutMs: 100,
      socketBehavior: { open: false },
    });
    const outcome = fixture.source
      .readBoundary()
      .catch((cause: unknown) => cause);
    const socket = await fixture.socketCreated;
    socket.emit(event, {
      code: 1006,
      reason: "connection lost",
      wasClean: false,
    });
    expect(await outcome).toBeInstanceOf(LocalKupmiosTransportUnavailableError);
    expect([...socket.listeners.values()].flat()).toHaveLength(0);
  });

  it.each([1002, 1008, 1009])(
    "keeps explicit WebSocket protocol/policy refusal %i hard",
    async (code) => {
      const fixture = sourceFixture({
        timeoutMs: 100,
        socketBehavior: { open: false },
      });
      const outcome = fixture.source
        .readBoundary()
        .catch((cause: unknown) => cause);
      const socket = await fixture.socketCreated;
      socket.emit("close", { code, reason: "peer refusal", wasClean: true });
      expect(await outcome).toBeInstanceOf(Error);
      expect(await outcome).not.toBeInstanceOf(
        LocalKupmiosTransportUnavailableError,
      );
    },
  );

  it.each(["opening", "request"])(
    "types internal WebSocket %s timeout",
    async (phase) => {
      const fixture = sourceFixture({
        timeoutMs: 25,
        socketBehavior:
          phase === "opening" ? { open: false } : { respond: false },
      });
      await expect(fixture.source.readBoundary()).rejects.toBeInstanceOf(
        LocalKupmiosTransportUnavailableError,
      );
      expect(fixture.sockets[0]?.closeCount).toBe(1);
    },
  );

  it.each([
    "{broken",
    JSON.stringify({
      id: 0,
      error: { code: 1000, message: "invalid request" },
    }),
  ])("keeps malformed/RPC error frames hard: %s", async (responseText) => {
    const fixture = sourceFixture({ socketBehavior: { responseText } });
    const error = await fixture.source
      .readBoundary()
      .catch((cause: unknown) => cause);
    expect(error).toBeInstanceOf(Error);
    expect(error).not.toBeInstanceOf(LocalKupmiosTransportUnavailableError);
  });

  it("does not conceal malformed data with a later socket-close timeout", async () => {
    const fixture = sourceFixture({
      timeoutMs: 25,
      socketBehavior: { responseText: "{broken", close: false },
    });
    try {
      const error = await fixture.source
        .readBoundary()
        .catch((cause: unknown) => cause);
      expect(error).toMatchObject({
        message: expect.stringContaining("malformed JSON"),
      });
      expect(error).not.toBeInstanceOf(LocalKupmiosTransportUnavailableError);
    } finally {
      for (const socket of fixture.sockets)
        socket.emit("close", { code: 1000, wasClean: true });
    }
  });
});
