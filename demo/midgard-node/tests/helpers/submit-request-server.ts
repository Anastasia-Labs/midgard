/**
 * The raw `POST /submit` handler on a real HTTP server, for the handler's
 * tests: a stub admission writer that acknowledges everything as new, the
 * fields of the node configuration the handler reads, and a client behind
 * the trusted proxy.
 */
import { once } from "node:events";
import { Agent, createServer, request as httpRequest } from "node:http";
import type { AddressInfo } from "node:net";
import path from "node:path";
import { pathToFileURL } from "node:url";

import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { Effect, Either, Layer, Logger, ManagedRuntime, Metric } from "effect";

import { submissionCheckConfig } from "../../src/commands/http/submission-check.js";
import { submitRefusalCounter } from "../../src/commands/http/submission-metrics.js";
import { makeSubmitRequestHandler } from "../../src/commands/http/submit.js";
import type { SubmittedTxRules } from "../../src/commands/http/transaction-admission.js";
import { createHttpIngress } from "../../src/commands/listen-http-ingress.js";
import {
  AdmissionWriter,
  type AdmissionWriteRequest,
} from "../../src/services/admission-writer.js";
import {
  DEFAULT_HTTP_RATE_LIMITS,
  type HttpRateLimitPolicy,
} from "../../src/services/http-rate-limits.js";
import { Globals, Lucid, NodeConfig } from "../../src/services/index.js";
import {
  createSubmitCheckPool,
  type SubmitCheckPool,
  type SubmitCheckPoolOptions,
} from "../../src/services/submit-check-pool.js";

export const MEDIA = "application/vnd.midgard.v1+cbor";
export const CLIENT = "198.51.100.7";

/** Every admission the stub writer acknowledged: tx id and full hash, hex. */
export const admitted: {
  readonly txId: string;
  readonly txFullHash: string;
}[] = [];

// Acknowledges every admission at once as new.
const acknowledge = (request: AdmissionWriteRequest) => {
  admitted.push({
    txId: request.txId.toString("hex"),
    txFullHash: request.txFullHash.toString("hex"),
  });
  const now = new Date();
  return {
    kind: "new" as const,
    txIdHex: request.txId.toString("hex"),
    status: "queued" as const,
    firstSeenAt: now,
    lastSeenAt: now,
  };
};
export const stubWriter = Layer.succeed(AdmissionWriter, {
  lookup: () => ({ _tag: "Unknown" }) as const,
  admit: (request) => Effect.succeed(acknowledge(request)),
  admitBatch: (requests) =>
    requests.map((request) => Either.right(acknowledge(request))),
  pendingSince: () => undefined,
  health: Effect.succeed({} as never),
  flush: Effect.void,
  stats: Effect.succeed({} as never),
});

/** Preprod with no fee floor, as the database fixtures' txs are built for. */
export const RULES: SubmittedTxRules = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  slotConfig: { ...NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod },
};

/** A Lucid service holding only the rules' L1 slot mapping. */
export const lucidLayer = Layer.succeed(Lucid, {
  api: { config: () => ({ slotConfig: RULES.slotConfig }) },
} as never);

export const configLayer = (policy: HttpRateLimitPolicy) =>
  Layer.succeed(NodeConfig, {
    NETWORK: "Preprod",
    MIN_FEE_A: RULES.minFeeA,
    MIN_FEE_B: RULES.minFeeB,
    HTTP_TRUSTED_PROXY_PEERS: ["127.0.0.1"],
    HTTP_RATE_LIMITS: policy,
    MAX_DURABLE_ADMISSION_BACKLOG: Number.MAX_SAFE_INTEGER,
    MAX_DURABLE_ADMISSION_BACKLOG_BYTES: Number.MAX_SAFE_INTEGER,
    SUBMIT_INGRESS_MAX_IN_FLIGHT_BYTES:
      MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes,
    SUBMIT_INGRESS_WORKER_POOL_SIZE: 1,
    SUBMIT_INGRESS_WORKER_QUEUE: 256,
    MAX_SUBMIT_TX_CBOR_BYTES: MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes,
  } as never);

const checkConfig = submissionCheckConfig(
  {
    MAX_SUBMIT_TX_CBOR_BYTES: MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes,
  },
  RULES,
);

export const fixtureWorker = (name: string) =>
  pathToFileURL(path.resolve("tests/fixtures", name));

export const startPool = (options: Partial<SubmitCheckPoolOptions> = {}) =>
  createSubmitCheckPool({
    size: 1,
    maxQueued: 256,
    maxQueuedBytes: MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes,
    config: checkConfig,
    ...options,
  });

// One production pool for the tests that do not need their own; a test
// file closes it with `closeSharedPool` after its last test.
let sharedPool: SubmitCheckPool | undefined;
export const closeSharedPool = async () => {
  await sharedPool?.close();
  sharedPool = undefined;
};

export const noLimits: HttpRateLimitPolicy = {
  ...DEFAULT_HTTP_RATE_LIMITS,
  enabled: false,
};

export type Reply = {
  readonly status: number;
  readonly headers: Record<string, string | string[] | undefined>;
  readonly body: Record<string, unknown>;
};

/** Run, newest first, by `runCleanups` after each test. */
export const cleanups: (() => Promise<void>)[] = [];
export const runCleanups = async () => {
  for (const cleanup of cleanups.splice(0).reverse()) await cleanup();
};

/** The raw handler on a real server; log lines are captured, not printed. */
export const serveSubmit = async (
  policy: HttpRateLimitPolicy = noLimits,
  checkPool: SubmitCheckPool = (sharedPool ??= startPool()),
) => {
  const lines: string[] = [];
  const runtime = ManagedRuntime.make(
    Layer.mergeAll(
      configLayer(policy),
      Globals.Default,
      stubWriter,
      Logger.replace(
        Logger.defaultLogger,
        Logger.make(({ message }) => {
          lines.push(
            Array.isArray(message) ? message.join(" ") : String(message),
          );
        }),
      ),
    ),
  );
  const handler = await runtime.runPromise(
    makeSubmitRequestHandler({
      wakeTxQueueProcessor: Effect.void,
      checkPool,
      ingress: createHttpIngress({ policy }),
    }),
  );
  let connections = 0;
  const server = createServer(handler);
  server.on("connection", () => {
    connections += 1;
  });
  server.listen(0, "127.0.0.1");
  await once(server, "listening");
  const port = (server.address() as AddressInfo).port;
  // The ingress holds a client to 4 requests in flight; a request now waits
  // for a worker, so a fifth concurrent one would be refused.
  const agent = new Agent({ keepAlive: true, maxSockets: 4 });
  cleanups.push(async () => {
    agent.destroy();
    server.closeAllConnections();
    await new Promise<void>((resolve) => server.close(() => resolve()));
    await runtime.dispose();
  });
  const call = (
    options: {
      readonly method?: string;
      readonly path?: string;
      readonly body?: Buffer;
      readonly headers?: Record<string, string>;
      readonly chunked?: boolean;
      readonly client?: string | null;
    } = {},
  ) =>
    new Promise<Reply>((resolve, reject) => {
      const body = options.body ?? Buffer.alloc(0);
      const client = options.client === undefined ? CLIENT : options.client;
      const req = httpRequest(
        {
          host: "127.0.0.1",
          port,
          path: options.path ?? "/submit",
          method: options.method ?? "POST",
          agent,
          headers: {
            "content-type": MEDIA,
            ...(options.chunked === true
              ? { "transfer-encoding": "chunked" }
              : { "content-length": body.length.toString() }),
            ...(client === null ? {} : { "x-midgard-client-ip": client }),
            ...options.headers,
          },
        },
        (res) => {
          const chunks: Buffer[] = [];
          res.on("data", (chunk: Buffer) => chunks.push(chunk));
          res.once("end", () =>
            resolve({
              status: res.statusCode!,
              headers: res.headers,
              body: JSON.parse(Buffer.concat(chunks).toString("utf8")),
            }),
          );
        },
      );
      req.once("error", reject);
      req.end(body);
    });
  return { call, lines, connections: () => connections, runtime, port };
};

export const refusals = <R>(
  runtime: ManagedRuntime.ManagedRuntime<R, never>,
  reason: string,
) =>
  runtime
    .runPromise(
      Metric.value(submitRefusalCounter.pipe(Metric.tagged("reason", reason))),
    )
    .then((state) => state.count);
