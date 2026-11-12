// M2 of the public submit ingress design: main-thread CPU per single-tx
// `POST /submit`, with the admission writer stubbed, so what is left is the
// node's own work on the event loop (HTTP parse, ingress checks, body read,
// the hand-off to the submit check workers, the response) and nothing from
// Postgres. The envelope decode and the submit check run on the workers.
//
//   pnpm --dir demo --filter midgard-node run bench:submit-main-thread
//
// The production `/submit` handler runs behind the production ingress on a
// real HTTP server in this process. The load client is the harness CLI
// (`scripts/submit-ingress-load.mjs run`) in a child process, so none of its
// CPU lands on this process's main thread. The main thread's CPU is read
// from /proc/<pid>/task/<pid>/stat over the run and divided by the requests
// it answered. The submit check alone is timed on the same txs, in process,
// and reported beside it: that is the work the workers take off the main
// thread.
//
// The client stands behind a trusted proxy (`x-midgard-client-ip`), as behind
// the nginx edge, and the ingress rate limits are off: this measures cost per
// request, not the limits. Plain HTTP; TLS is at the edge (D7).
//
// SUBMIT_MAIN_THREAD_RATE (default 1000/s), SUBMIT_MAIN_THREAD_SECONDS
// (default 10) and SUBMIT_MAIN_THREAD_KEEP_ALIVE (share of keep-alive
// requests, default 0.1) set the load. A warm-up run of
// SUBMIT_MAIN_THREAD_WARMUP_SECONDS (default 3) at the same rate, on its own
// txs, comes first and is left out of the numbers: a cold process (JIT, the
// first worker batches) can answer its first second's requests late enough
// that the ingress refuses some for capacity, which says nothing about the
// steady cost per request. Every request of the measured run must be
// admitted.
//
// SUBMIT_MAIN_THREAD_WORKERS sizes the check pool. By default it is sized
// from the submit check cost measured below, so the workers run at about
// half load at RATE: a pool short of the offered rate queues bodies until
// the ingress refuses for capacity, which measures the workers, not the main
// thread. The pool needs the built dist (`submit-check.js`). The child CLI
// imports the built workspace packages: build @al-ft/midgard-core first.
import { execFile } from "node:child_process";
import { once } from "node:events";
import fs from "node:fs";
import { createServer } from "node:http";
import type { AddressInfo } from "node:net";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { decodeMidgardProofSubmission } from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  Effect,
  Either,
  Layer,
  Logger,
  LogLevel,
  ManagedRuntime,
} from "effect";
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
import { submissionCheckConfig } from "../../src/commands/http/submission-check.js";
import { makeSubmitRequestHandler } from "../../src/commands/http/submit.js";
import {
  checkSubmittedTransaction,
  type SubmittedTxRules,
} from "../../src/commands/http/transaction-admission.js";
import { createHttpIngress } from "../../src/commands/listen-http-ingress.js";
import { AdmissionWriter } from "../../src/services/admission-writer.js";
import { DEFAULT_HTTP_RATE_LIMITS } from "../../src/services/http-rate-limits.js";
import { Globals, NodeConfig } from "../../src/services/index.js";
import { createSubmitCheckPool } from "../../src/services/submit-check-pool.js";
import { makeTestNodeConfig } from "../helpers/test-node-config.js";

const RATE = Number(process.env.SUBMIT_MAIN_THREAD_RATE ?? "1000");
const SECONDS = Number(process.env.SUBMIT_MAIN_THREAD_SECONDS ?? "10");
const KEEP_ALIVE = Number(process.env.SUBMIT_MAIN_THREAD_KEEP_ALIVE ?? "0.1");
const WORKERS_OVERRIDE = process.env.SUBMIT_MAIN_THREAD_WORKERS;
const WARMUP_SECONDS = Number(
  process.env.SUBMIT_MAIN_THREAD_WARMUP_SECONDS ?? "3",
);

const packageRoot = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../..",
);

// The writer knows no tx and acknowledges every admission at once as new.
// It never settles, so the gauge only grows; the backlog bounds are unbounded.
const newAck = (request: { readonly txId: Buffer }) => {
  const now = new Date();
  return {
    kind: "new" as const,
    txIdHex: request.txId.toString("hex"),
    status: "queued" as const,
    firstSeenAt: now,
    lastSeenAt: now,
  };
};
const stubWriter = Layer.succeed(AdmissionWriter, {
  lookup: () => ({ _tag: "Unknown" }) as const,
  admit: (request) => Effect.succeed(newAck(request)),
  admitBatch: (requests) =>
    requests.map((request) => Either.right(newAck(request))),
  pendingSince: () => undefined,
  health: Effect.succeed({} as never),
  flush: Effect.void,
  stats: Effect.succeed({} as never),
});

// The corpus's network and fee rule; the corpus sets no validity interval.
const RULES: SubmittedTxRules = {
  expectedNetworkId: 0n,
  minFeeA: 1n,
  minFeeB: 2_000n,
  slotConfig: { ...NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod },
};
// The fields the submit route and the ingress read.
const benchConfig = Layer.succeed(
  NodeConfig,
  makeTestNodeConfig({
    HTTP_TRUSTED_PROXY_PEERS: ["127.0.0.1"],
    HTTP_RATE_LIMITS: { ...DEFAULT_HTTP_RATE_LIMITS, enabled: false },
    MAX_DURABLE_ADMISSION_BACKLOG: Number.MAX_SAFE_INTEGER,
    MAX_DURABLE_ADMISSION_BACKLOG_BYTES: Number.MAX_SAFE_INTEGER,
  }),
);

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
      const warmupDirectory = path.join(scratch, "warmup");
      if (WARMUP_SECONDS > 0)
        buildCorpus({
          directory: warmupDirectory,
          seed: "m2-warmup",
          validCount: Math.ceil(RATE * WARMUP_SECONDS),
          junkMix: parseJunkMix(""),
        });

      // The submit check alone, on the same txs.
      const txs = readCorpus(corpusDirectory).requests.map(
        ({ body }) => decodeMidgardProofSubmission(body).transactionCbor,
      );
      await ledgerEd25519SignatureReady();
      for (const tx of txs.slice(0, 500))
        checkSubmittedTransaction(tx, 32_768, RULES, Date.now());
      const checkStartedAt = process.hrtime.bigint();
      for (const tx of txs)
        checkSubmittedTransaction(tx, 32_768, RULES, Date.now());
      const checkUsPerTx =
        Number(process.hrtime.bigint() - checkStartedAt) / 1e3 / txs.length;
      const workers =
        WORKERS_OVERRIDE === undefined
          ? Math.max(2, Math.ceil((2 * RATE * checkUsPerTx) / 1e6))
          : Number(WORKERS_OVERRIDE);

      const runtime = ManagedRuntime.make(
        Layer.mergeAll(
          benchConfig,
          Globals.Default,
          stubWriter,
          Logger.minimumLogLevel(LogLevel.Info),
        ),
      );
      const checkPool = createSubmitCheckPool({
        size: workers,
        maxQueued: 256,
        maxQueuedBytes: MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes,
        config: submissionCheckConfig(
          {
            MAX_SUBMIT_TX_CBOR_BYTES:
              MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes,
          },
          RULES,
        ),
      });
      // The load starts once every worker has loaded the check.
      const loadDeadline = Date.now() + 60_000;
      while (checkPool.stats().readyWorkers < workers) {
        if (Date.now() > loadDeadline)
          throw new Error("the submit check workers did not load");
        await new Promise((resolve) => setTimeout(resolve, 50));
      }
      const handler = await runtime.runPromise(
        makeSubmitRequestHandler({
          wakeTxQueueProcessor: Effect.void,
          checkPool,
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
      const load = (corpus: string, seconds: number, report: string) =>
        promisify(execFile)(
          process.execPath,
          [
            path.join(packageRoot, "scripts/submit-ingress-load.mjs"),
            "run",
            "--corpus",
            corpus,
            "--url",
            `http://127.0.0.1:${port.toString()}`,
            "--rate",
            RATE.toString(),
            "--duration",
            (seconds + 5).toString(),
            "--keep-alive-share",
            KEEP_ALIVE.toString(),
            "--report",
            report,
          ],
          { cwd: packageRoot, maxBuffer: 16 * 1024 * 1024 },
        );
      try {
        if (WARMUP_SECONDS > 0)
          await load(
            warmupDirectory,
            WARMUP_SECONDS,
            path.join(scratch, "warmup-report.json"),
          );
        const before = sampleProcessCpu(process.pid);
        await load(corpusDirectory, SECONDS, reportFile);
        const after = sampleProcessCpu(process.pid);
        const report = JSON.parse(fs.readFileSync(reportFile, "utf8")) as {
          load: {
            completed: number;
            admitted: number;
            outcomes: Record<string, number>;
            offeredMs: number;
            notSentWhenDue: number;
            maxLagRequests: number;
            latencyMs: { p50: number; p99: number };
          };
        };
        // What each run got back, so a refused request can be told apart
        // from a client burst (`maxLagRequests`: requests the client sent at
        // once after falling behind).
        console.log(
          `SUBMIT_MAIN_THREAD_OUTCOMES ${JSON.stringify({
            warmup:
              WARMUP_SECONDS > 0
                ? (
                    JSON.parse(
                      fs.readFileSync(
                        path.join(scratch, "warmup-report.json"),
                        "utf8",
                      ),
                    ) as { load: unknown }
                  ).load
                : undefined,
            measured: report.load,
          })}`,
        );
        const shares = cpuShares(before, after);
        const mainThreadUsPerRequest =
          (shares.mainThreadCores * shares.seconds * 1e6) /
          report.load.completed;
        const row = {
          offeredRatePerSecond: RATE,
          seconds: Number(shares.seconds.toFixed(2)),
          keepAliveShare: KEEP_ALIVE,
          warmupSeconds: WARMUP_SECONDS,
          checkWorkers: workers,
          requests: report.load.completed,
          notSentWhenDue: report.load.notSentWhenDue,
          maxLagRequests: report.load.maxLagRequests,
          txBytes: txs[0]!.length,
          mainThreadCores: Number(shares.mainThreadCores.toFixed(3)),
          processCores: Number(shares.processCores.toFixed(3)),
          mainThreadUsPerRequest: Number(mainThreadUsPerRequest.toFixed(1)),
          submitCheckUsPerTx: Number(checkUsPerTx.toFixed(1)),
          latencyP50Ms: report.load.latencyMs.p50,
          latencyP99Ms: report.load.latencyMs.p99,
        };
        console.table([row]);
        console.log(`SUBMIT_MAIN_THREAD ${JSON.stringify(row)}`);
        // The numbers are printed first, so a run with refusals still shows
        // its cost; it fails here.
        expect(report.load.admitted).toBe(txs.length);
      } finally {
        server.closeAllConnections();
        server.close();
        await checkPool.close();
        await runtime.dispose();
      }
    } finally {
      fs.rmSync(scratch, { recursive: true, force: true });
    }
  });
});
