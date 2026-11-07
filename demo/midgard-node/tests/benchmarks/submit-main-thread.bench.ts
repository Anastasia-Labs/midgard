// M2 of the public submit ingress design: main-thread CPU per single-tx
// `POST /submit`, with the admission writer stubbed, so what is left is the
// node's own work on the event loop (HTTP parse, ingress checks, body read,
// envelope decode, the submit check, the response) and nothing from Postgres.
//
//   pnpm --dir demo --filter midgard-node run bench:submit-main-thread
//
// The production `/submit` handler runs behind the production ingress on a
// real HTTP server in this process. The load client is the harness CLI
// (`scripts/submit-ingress-load.mjs run`) in a child process, so none of its
// CPU lands on this process's main thread. The main thread's CPU is read
// from /proc/<pid>/task/<pid>/stat over the run and divided by the requests
// it answered. The submit check alone is timed on the same txs, in process,
// and reported beside it.
//
// The client stands behind a trusted proxy (`x-midgard-client-ip`), as behind
// the nginx edge, and the ingress rate limits are off: this measures cost per
// request, not the limits. Plain HTTP; TLS is at the edge (D7).
//
// SUBMIT_MAIN_THREAD_RATE (default 1000/s), SUBMIT_MAIN_THREAD_SECONDS
// (default 10) and SUBMIT_MAIN_THREAD_KEEP_ALIVE (share of keep-alive
// requests, default 0.1) set the load. The child CLI imports the built
// workspace packages: build @al-ft/midgard-core first.
import { execFile } from "node:child_process";
import { once } from "node:events";
import fs from "node:fs";
import { createServer } from "node:http";
import type { AddressInfo } from "node:net";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

import { decodeMidgardProofSubmission } from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { Effect, Layer, Logger, LogLevel, ManagedRuntime } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildCorpus,
  parseJunkMix,
  readCorpus,
} from "../../scripts/submit-ingress-load.corpus.mjs";
import {
  cpuShares,
  sampleProcessCpu,
} from "../../scripts/submit-ingress-load.report.mjs";
import { makeSubmitRequestHandler } from "../../src/commands/http/submit.js";
import { checkSubmittedTransaction } from "../../src/commands/http/transaction-admission.js";
import { createHttpIngress } from "../../src/commands/listen-http-ingress.js";
import { AdmissionWriter } from "../../src/services/admission-writer.js";
import { DEFAULT_HTTP_RATE_LIMITS } from "../../src/services/http-rate-limits.js";
import { Globals, NodeConfig } from "../../src/services/index.js";

const RATE = Number(process.env.SUBMIT_MAIN_THREAD_RATE ?? "1000");
const SECONDS = Number(process.env.SUBMIT_MAIN_THREAD_SECONDS ?? "10");
const KEEP_ALIVE = Number(process.env.SUBMIT_MAIN_THREAD_KEEP_ALIVE ?? "0.1");

const packageRoot = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../..",
);

// The writer knows no tx and acknowledges every admission at once as new.
// It never settles, so the gauge only grows; the backlog bounds are unbounded.
const stubWriter = Layer.succeed(AdmissionWriter, {
  lookup: () => ({ _tag: "Unknown" }) as const,
  admit: (request) => {
    const now = new Date();
    return Effect.succeed({
      kind: "new" as const,
      txIdHex: request.txId.toString("hex"),
      status: "queued",
      firstSeenAt: now,
      lastSeenAt: now,
    });
  },
  pendingSince: () => undefined,
  health: Effect.succeed({} as never),
  flush: Effect.void,
  stats: Effect.succeed({} as never),
});

// The fields the submit route and the ingress read.
const benchConfig = Layer.succeed(NodeConfig, {
  HTTP_TRUSTED_PROXY_PEERS: ["127.0.0.1"],
  HTTP_RATE_LIMITS: { ...DEFAULT_HTTP_RATE_LIMITS, enabled: false },
  MAX_DURABLE_ADMISSION_BACKLOG: Number.MAX_SAFE_INTEGER,
  MAX_DURABLE_ADMISSION_BACKLOG_BYTES: Number.MAX_SAFE_INTEGER,
  SUBMIT_INGRESS_MAX_CONCURRENCY: 4,
  SUBMIT_INGRESS_MAX_IN_FLIGHT_BYTES:
    MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes,
  MAX_SUBMIT_TX_CBOR_BYTES: MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes,
} as never);

describe("submit main-thread cost (M2)", () => {
  it("measures main-thread CPU per /submit request", async () => {
    const scratch = fs.mkdtempSync(path.join(os.tmpdir(), "submit-m2-"));
    try {
      const corpusDirectory = path.join(scratch, "corpus");
      buildCorpus({
        directory: corpusDirectory,
        seed: "m2",
        validCount: Math.ceil(RATE * SECONDS),
        junkMix: parseJunkMix(""),
      });

      // The submit check alone, on the same txs.
      const txs = readCorpus(corpusDirectory).requests.map(
        ({ body }) => decodeMidgardProofSubmission(body).transactionCbor,
      );
      for (const tx of txs.slice(0, 500)) checkSubmittedTransaction(tx, 32_768);
      const checkStartedAt = process.hrtime.bigint();
      for (const tx of txs) checkSubmittedTransaction(tx, 32_768);
      const checkUsPerTx =
        Number(process.hrtime.bigint() - checkStartedAt) / 1e3 / txs.length;

      const runtime = ManagedRuntime.make(
        Layer.mergeAll(
          benchConfig,
          Globals.Default,
          stubWriter,
          Logger.minimumLogLevel(LogLevel.Info),
        ),
      );
      const handler = await runtime.runPromise(
        makeSubmitRequestHandler({
          wakeTxQueueProcessor: Effect.void,
          ingress: createHttpIngress({
            policy: { ...DEFAULT_HTTP_RATE_LIMITS, enabled: false },
          }),
        }),
      );
      const server = createServer((request, response) =>
        handler(request, response),
      );
      server.listen(0, "127.0.0.1");
      await once(server, "listening");
      const { port } = server.address() as AddressInfo;
      const reportFile = path.join(scratch, "report.json");
      try {
        const before = sampleProcessCpu(process.pid);
        await promisify(execFile)(
          process.execPath,
          [
            path.join(packageRoot, "scripts/submit-ingress-load.mjs"),
            "run",
            "--corpus",
            corpusDirectory,
            "--url",
            `http://127.0.0.1:${port.toString()}`,
            "--rate",
            RATE.toString(),
            "--duration",
            (SECONDS + 5).toString(),
            "--keep-alive-share",
            KEEP_ALIVE.toString(),
            "--report",
            reportFile,
          ],
          { cwd: packageRoot, maxBuffer: 16 * 1024 * 1024 },
        );
        const after = sampleProcessCpu(process.pid);
        const report = JSON.parse(fs.readFileSync(reportFile, "utf8")) as {
          load: {
            completed: number;
            admitted: number;
            outcomes: Record<string, number>;
            offeredMs: number;
            notSentWhenDue: number;
            latencyMs: { p50: number; p99: number };
          };
        };
        expect(report.load.admitted).toBe(txs.length);
        const shares = cpuShares(before, after);
        const mainThreadUsPerRequest =
          (shares.mainThreadCores * shares.seconds * 1e6) /
          report.load.completed;
        const row = {
          offeredRatePerSecond: RATE,
          seconds: Number(shares.seconds.toFixed(2)),
          keepAliveShare: KEEP_ALIVE,
          requests: report.load.completed,
          notSentWhenDue: report.load.notSentWhenDue,
          txBytes: txs[0]!.length,
          mainThreadCores: Number(shares.mainThreadCores.toFixed(3)),
          processCores: Number(shares.processCores.toFixed(3)),
          mainThreadUsPerRequest: Number(mainThreadUsPerRequest.toFixed(1)),
          submitCheckUsPerTx: Number(checkUsPerTx.toFixed(1)),
          mainThreadUsPerRequestLessCheck: Number(
            (mainThreadUsPerRequest - checkUsPerTx).toFixed(1),
          ),
          latencyP50Ms: report.load.latencyMs.p50,
          latencyP99Ms: report.load.latencyMs.p99,
        };
        console.table([row]);
        console.log(`SUBMIT_MAIN_THREAD ${JSON.stringify(row)}`);
      } finally {
        server.closeAllConnections();
        server.close();
        await runtime.dispose();
      }
    } finally {
      fs.rmSync(scratch, { recursive: true, force: true });
    }
  });
});
