// The submit-ingress load harness (#871): its corpus must be what it claims
// (every valid tx admitted and accepted against the corpus genesis, every junk
// kind refused where and why the manifest says), and its client must send what
// the report then counts.
import fs from "node:fs";
import http from "node:http";
import os from "node:os";
import path from "node:path";

import { decodeMidgardProofSubmission } from "@al-ft/midgard-core/cek-proof";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  type QueuedTx,
  RejectCodes,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";
import { afterAll, describe, expect, it } from "vitest";

import {
  CLIENT_IP_HEADER,
  loopbackSourceAddress,
  outcomeLabel,
  quantile,
  runLoad,
  syntheticClientIp,
} from "../scripts/submit-ingress-load.client.mjs";
import {
  buildCorpus,
  type CorpusKind,
  parseJunkMix,
  readCorpus,
  readCorpusGenesis,
} from "../scripts/submit-ingress-load.corpus.mjs";
import {
  clockTicks,
  cpuShares,
  parseProcStatTicks,
  sampleProcessCpu,
} from "../scripts/submit-ingress-load.report.mjs";
import { checkSubmittedTransaction } from "../src/commands/http/transaction-admission.js";

const scratch = fs.mkdtempSync(path.join(os.tmpdir(), "submit-ingress-load-"));
afterAll(() => fs.rmSync(scratch, { recursive: true, force: true }));

const EVERY_JUNK_KIND =
  "bad-cbor=0.1,bad-signature=0.1,low-fee=0.1,missing-input=0.1,double-spend=0.1";

type Verdict = { readonly stage: string; readonly code: string };

// Runs each corpus request the way the node does: `/submit`'s check (which
// refuses Phase A's stateless junk classes) with the corpus rules, then
// Phase A with the corpus fee parameters, then Phase B against the corpus
// genesis, all in one batch in send order. A valid tx that reaches Phase B
// accepted is one ingress did not refuse.
const verdictsOf = async (directory: string) => {
  const { manifest, requests } = readCorpus(directory);
  const genesis = new Map<string, Buffer>();
  for (const { outRef, output } of readCorpusGenesis(directory))
    genesis.set(outRef.toString("hex"), output);
  const verdicts = new Map<number, Verdict>();
  const queued: QueuedTx[] = [];
  const indexByTxId = new Map<string, number>();
  const rules = {
    expectedNetworkId: BigInt(manifest.networkId),
    minFeeA: BigInt(manifest.minFeeA),
    minFeeB: BigInt(manifest.minFeeB),
    slotConfig: { ...NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod },
  };
  const nowMs = Date.now();
  requests.forEach((request, index) => {
    const { transactionCbor } = decodeMidgardProofSubmission(request.body);
    const checked = checkSubmittedTransaction(
      transactionCbor,
      32_768,
      rules,
      nowMs,
    );
    if (!checked.ok) {
      verdicts.set(index, {
        stage: "ingress",
        code: `${checked.status.toString()} ${String(checked.body.error)}`,
      });
      return;
    }
    indexByTxId.set(checked.normalized.txIdHex, index);
    queued.push({
      txId: checked.normalized.txId,
      txCbor: checked.normalized.txCanonicalCbor,
      arrivalSeq: BigInt(index),
      createdAt: new Date(0),
    });
  });
  const phaseA = await Effect.runPromise(
    runPhaseAValidation(queued, {
      ...rules,
      concurrency: 1,
      strictnessProfile: "phase1_midgard",
    }),
  );
  const phaseB = await Effect.runPromise(
    runPhaseBValidationWithPatch(phaseA.accepted, genesis, {
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      nowCardanoSlotNo: 100n,
      bucketConcurrency: 1,
      enforceScriptBudget: true,
    }),
  );
  const at = (txId: Buffer) => indexByTxId.get(txId.toString("hex"))!;
  for (const rejected of phaseA.rejected)
    verdicts.set(at(rejected.txId), { stage: "phase-a", code: rejected.code });
  for (const rejected of phaseB.rejected)
    verdicts.set(at(rejected.txId), { stage: "phase-b", code: rejected.code });
  for (const accepted of phaseB.accepted)
    verdicts.set(at(accepted.ledgerTx.txId), {
      stage: "phase-b",
      code: "accepted",
    });
  return { manifest, requests, verdicts };
};

describe("submit-ingress load corpus", () => {
  it("builds valid txs the node accepts and junk it refuses for the named reason", async () => {
    const directory = path.join(scratch, "every-kind");
    const manifest = buildCorpus({
      directory,
      seed: "corpus-test",
      validCount: 20,
      junkMix: parseJunkMix(EVERY_JUNK_KIND),
      keyCount: 7,
    });
    expect(manifest.counts).toEqual({
      valid: 20,
      "bad-cbor": 4,
      "bad-signature": 4,
      "low-fee": 4,
      "missing-input": 4,
      "double-spend": 4,
    });
    const { requests, verdicts } = await verdictsOf(directory);
    expect(requests).toHaveLength(40);
    const expected: Record<CorpusKind, Verdict> = {
      valid: { stage: "phase-b", code: "accepted" },
      "bad-cbor": {
        stage: "ingress",
        code: "400 Invalid canonical transaction CBOR payload",
      },
      "bad-signature": {
        stage: "ingress",
        code: `400 ${RejectCodes.InvalidSignature}`,
      },
      "low-fee": { stage: "ingress", code: `400 ${RejectCodes.MinFee}` },
      "missing-input": { stage: "phase-b", code: RejectCodes.InputNotFound },
      "double-spend": { stage: "phase-b", code: RejectCodes.DoubleSpend },
    };
    requests.forEach((request, index) =>
      expect({ index, kind: request.kind, ...verdicts.get(index) }).toEqual({
        index,
        kind: request.kind,
        ...expected[request.kind],
      }),
    );
  });

  it("is the same corpus for the same seed and a different one for another", () => {
    const build = (name: string, seed: string) => {
      const directory = path.join(scratch, name);
      buildCorpus({
        directory,
        seed,
        validCount: 6,
        junkMix: parseJunkMix("double-spend=0.25,low-fee=0.25"),
        keyCount: 3,
      });
      return fs.readFileSync(path.join(directory, "requests.bin"));
    };
    const first = build("seed-a1", "a");
    expect(build("seed-a2", "a").equals(first)).toBe(true);
    expect(build("seed-b", "b").equals(first)).toBe(false);
  });

  it("refuses a corpus file cut short", () => {
    const directory = path.join(scratch, "truncated");
    buildCorpus({
      directory,
      validCount: 3,
      junkMix: parseJunkMix(""),
      keyCount: 2,
    });
    const file = path.join(directory, "requests.bin");
    fs.truncateSync(file, fs.statSync(file).size - 1);
    expect(() => readCorpus(directory)).toThrow(
      /malformed request record at byte/u,
    );
  });

  it.each([
    ["typo=0.1", /unknown junk kind "typo"/u],
    ["low-fee=1", /must be in \[0, 1\)/u],
    ["low-fee=-0.1", /must be in \[0, 1\)/u],
    ["low-fee=0.6,bad-cbor=0.5", /must sum to under 1/u],
  ])("refuses the junk mix %s", (text, message) => {
    expect(() => parseJunkMix(text)).toThrow(message);
  });
});

// A local server that records what reaches it and answers like `/submit`:
// 400 for a body starting "junk", 202 otherwise.
const recordingServer = async () => {
  const seen: {
    body: string;
    clientIp: string | undefined;
    connection: string | undefined;
    remoteAddress: string | undefined;
  }[] = [];
  const server = http.createServer((request, response) => {
    const chunks: Buffer[] = [];
    request.on("data", (chunk: Buffer) => chunks.push(chunk));
    request.on("end", () => {
      const body = Buffer.concat(chunks).toString("utf8");
      seen.push({
        body,
        clientIp: request.headers[CLIENT_IP_HEADER] as string | undefined,
        connection: request.headers.connection,
        remoteAddress: request.socket.remoteAddress,
      });
      const refused = body.startsWith("junk");
      response.writeHead(refused ? 400 : 202, {
        "content-type": "application/json",
      });
      response.end(
        JSON.stringify(refused ? { error: "E_JUNK" } : { status: "queued" }),
      );
    });
  });
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  const { port } = server.address() as { port: number };
  return {
    seen,
    url: new URL(`http://127.0.0.1:${port.toString()}/`),
    close: () =>
      new Promise<void>((resolve) => {
        server.closeAllConnections();
        server.close(() => resolve());
      }),
  };
};

describe("submit-ingress load client", () => {
  it("labels outcomes by status and the body's error or status", () => {
    expect(outcomeLabel(202, '{"status":"queued","txId":"ab"}')).toBe(
      "202 queued",
    );
    expect(outcomeLabel(400, '{"error":"E_MIN_FEE"}')).toBe("400 E_MIN_FEE");
    expect(outcomeLabel(503, "not json")).toBe("503");
    expect(outcomeLabel(400, JSON.stringify({ error: "x".repeat(100) }))).toBe(
      `400 ${"x".repeat(61)}...`,
    );
  });

  it("takes nearest-rank quantiles", () => {
    const sorted = Array.from({ length: 100 }, (_, index) => index + 1);
    expect([0.5, 0.99, 1].map((q) => quantile(sorted, q))).toEqual([
      50, 99, 100,
    ]);
    expect(quantile([], 0.5)).toBeNull();
  });

  it("offers each request once, at the rate, on mostly new connections from many client addresses", async () => {
    const server = await recordingServer();
    const requests = Array.from({ length: 200 }, (_, index) => ({
      kind: index % 4 === 0 ? "junk" : "valid",
      body: Buffer.from(`${index % 4 === 0 ? "junk" : "valid"}-${index}`),
    }));
    try {
      const result = await runLoad({
        requests,
        target: { url: server.url },
        ratePerSecond: 400,
        durationMs: 10_000,
        keepAliveShare: 0.1,
        clientIpCount: 50,
      });
      expect({
        sent: result.sent,
        completed: result.completed,
        admitted: result.admitted,
        outcomes: result.outcomes,
        byKind: result.byKind,
      }).toEqual({
        sent: 200,
        completed: 200,
        admitted: 150,
        outcomes: { "202 queued": 150, "400 E_JUNK": 50 },
        byKind: { junk: { "400 E_JUNK": 50 }, valid: { "202 queued": 150 } },
      });
      // 200 requests at 400/s: about half a second, never a burst.
      expect(result.offeredMs).toBeGreaterThan(450);
      expect(new Set(server.seen.map(({ body }) => body)).size).toBe(200);
      expect(new Set(server.seen.map(({ clientIp }) => clientIp))).toEqual(
        new Set(
          Array.from({ length: 50 }, (_, index) => syntheticClientIp(index)),
        ),
      );
      expect(
        server.seen.filter(({ connection }) => connection === "close"),
      ).toHaveLength(180);
      expect(result.latencyMs.count).toBe(200);
      expect(result.admittedLatencyMs.count).toBe(150);
    } finally {
      await server.close();
    }
  });

  it("binds each connection to one of the loopback sources when asked", async () => {
    if (process.platform !== "linux") return;
    const server = await recordingServer();
    try {
      const result = await runLoad({
        requests: Array.from({ length: 40 }, (_, index) => ({
          kind: "valid",
          body: Buffer.from(`valid-${index}`),
        })),
        target: { url: server.url },
        ratePerSecond: 400,
        durationMs: 10_000,
        keepAliveShare: 0,
        useClientIpHeader: false,
        loopbackSources: 8,
      });
      expect(result.admitted).toBe(40);
      expect(server.seen.every(({ clientIp }) => clientIp === undefined)).toBe(
        true,
      );
      expect(
        new Set(server.seen.map(({ remoteAddress }) => remoteAddress)),
      ).toEqual(
        new Set(
          Array.from({ length: 8 }, (_, index) => loopbackSourceAddress(index)),
        ),
      );
    } finally {
      await server.close();
    }
  });
});

describe("submit-ingress load cpu readings", () => {
  it("reads user and system ticks past a command name with spaces and parentheses", () => {
    expect(
      parseProcStatTicks(
        "4242 (node (main) x) S 1 4242 4242 0 -1 4194560 100 0 0 0 731 92 0 0 20 0 11 0 5000",
      ),
    ).toEqual({ userTicks: 731, systemTicks: 92 });
  });

  it("counts a busy main thread as about one core", () => {
    if (process.platform !== "linux") return;
    const before = sampleProcessCpu(process.pid);
    const until = Date.now() + 300;
    let spin = 0;
    while (Date.now() < until) spin += 1;
    const shares = cpuShares(before, sampleProcessCpu(process.pid));
    expect(spin).toBeGreaterThan(0);
    expect(shares.mainThreadCores).toBeGreaterThan(0.5);
    // The kernel rounds the thread and process totals to ticks separately,
    // so the process total can trail the main thread's by one tick.
    const oneTick = 1 / clockTicks() / shares.seconds;
    expect(shares.processCores).toBeGreaterThanOrEqual(
      shares.mainThreadCores - oneTick,
    );
  });
});
