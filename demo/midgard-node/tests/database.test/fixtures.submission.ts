import { createServer, request as httpRequest } from "node:http";
import type { AddressInfo } from "node:net";

import { encodeMidgardProofSubmission } from "@al-ft/midgard-core/cek-proof";
import {
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonical,
  encodeMidgardVersionedScriptListPreimage,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonical,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { expect } from "vitest";

import { submissionCheckConfig } from "../../src/commands/http/submission-check.js";
import { makeSubmitRequestHandler } from "../../src/commands/http/submit.js";
import { createHttpIngress } from "../../src/commands/listen-http-ingress.js";
import { MempoolDB, TxAdmissionsDB } from "../../src/database/index.js";
import { AdmissionWriter } from "../../src/services/admission-writer.js";
import { NodeConfig } from "../../src/services/config.js";
import type { NodeConfigDep } from "../../src/services/config.node-config-dep.js";
import { BatchSql } from "../../src/services/database.js";
import { Globals } from "../../src/services/globals.js";
import { DEFAULT_HTTP_RATE_LIMITS } from "../../src/services/http-rate-limits.js";
import { Lucid } from "../../src/services/lucid.js";
import { MempoolLedgerCache } from "../../src/services/mempool-ledger-cache.js";
import {
  createSubmitCheckPool,
  type SubmitCheckPool,
} from "../../src/services/submit-check-pool.js";
import { ValidationPool } from "../../src/services/validation-pool.js";
import { WriteBehind } from "../../src/services/write-behind.js";
import { makeCardanoSignedMapOutputTxBytes } from "../helpers/cardano-native-fixtures.js";
import { makeOutRefCbor } from "../midgard-output-helpers.js";
import { databaseTxHash } from "./fixtures.bytes.js";

type SubmitHttpResult = {
  readonly status: number;
  readonly body: Record<string, unknown>;
  readonly retryAfter?: string;
};

export type TxQueueWakeRequirements =
  | BatchSql
  | NodeConfig
  | Globals
  | Lucid
  | WriteBehind
  | ValidationPool
  | MempoolLedgerCache;

/** The fixture isolates the admission contract, so it sets no rate limits. */
const fixtureIngress = createHttpIngress({
  policy: { ...DEFAULT_HTTP_RATE_LIMITS, enabled: false },
});

/**
 * One submit check pool for every fixture submission, sized from the first
 * caller's configuration; its workers do not hold the process open.
 */
let fixtureCheckPool: SubmitCheckPool | undefined;
const checkPoolFor = (config: NodeConfigDep): SubmitCheckPool =>
  (fixtureCheckPool ??= createSubmitCheckPool({
    size: config.SUBMIT_INGRESS_WORKER_POOL_SIZE,
    maxQueued: config.SUBMIT_INGRESS_WORKER_QUEUE,
    maxQueuedBytes: config.SUBMIT_INGRESS_MAX_IN_FLIGHT_BYTES,
    config: submissionCheckConfig(config),
  }));

/**
 * Each POST comes from a loopback address of its own, so the ingress's
 * per-client in-flight bound never refuses a fixture's concurrent
 * submissions: the fixture isolates the admission contract.
 */
let fixtureClients = 0;
const nextClientAddress = () => {
  const n = fixtureClients++ % 65_536;
  return `127.1.${(n >> 8).toString()}.${(n & 255).toString()}`;
};

/** One POST of `body` to `port`, on a connection of its own. */
const postSubmit = (
  port: number,
  body: Buffer,
  contentType: string,
): Promise<{
  readonly status: number;
  readonly body: Record<string, unknown>;
  readonly retryAfter?: string;
}> =>
  new Promise((resolve, reject) => {
    const outgoing = httpRequest(
      {
        host: "127.0.0.1",
        port,
        path: "/submit",
        method: "POST",
        agent: false,
        localAddress: nextClientAddress(),
        headers: {
          "content-type": contentType,
          "content-length": body.length.toString(),
        },
      },
      (response) => {
        const chunks: Buffer[] = [];
        response.on("data", (chunk: Buffer) => chunks.push(chunk));
        response.once("error", reject);
        response.once("end", () => {
          const retryAfter = response.headers["retry-after"];
          const parsed = JSON.parse(
            Buffer.concat(chunks).toString("utf8"),
          ) as Record<string, unknown>;
          const status = response.statusCode ?? 0;
          resolve(
            retryAfter === undefined
              ? { status, body: parsed }
              : { status, body: parsed, retryAfter },
          );
        });
      },
    );
    outgoing.once("error", reject);
    outgoing.end(body);
  });

/**
 * Submits `requestBody` to the raw `/submit` handler, built from the
 * services in context, over a real HTTP connection.
 */
export const submitThroughRouter = <R>(
  requestBody: Buffer,
  wakeTxQueueProcessor: Effect.Effect<void, never, R>,
  options: {
    readonly contentType?: string;
    /** Wait for the admission writer to write what it acknowledged
     * (default). `false` returns as soon as the handler answers. */
    readonly flush?: boolean;
  } = {},
): Effect.Effect<
  SubmitHttpResult,
  unknown,
  R | SqlClient.SqlClient | NodeConfig | Globals | AdmissionWriter
> =>
  Effect.gen(function* () {
    const handler = yield* makeSubmitRequestHandler({
      wakeTxQueueProcessor,
      checkPool: checkPoolFor(yield* NodeConfig),
      ingress: fixtureIngress,
    });
    const result = yield* Effect.acquireUseRelease(
      Effect.async<ReturnType<typeof createServer>>((resume) => {
        const server = createServer(handler);
        server.listen(0, "127.0.0.1", () => resume(Effect.succeed(server)));
      }),
      (server) =>
        Effect.tryPromise({
          try: () =>
            postSubmit(
              (server.address() as AddressInfo).port,
              requestBody,
              options.contentType ?? "application/vnd.midgard.v1+cbor",
            ),
          catch: (cause) => cause,
        }),
      (server) =>
        Effect.async<void>((resume) => {
          server.closeAllConnections();
          server.close(() => resume(Effect.void));
        }),
    );
    // The handler answers from memory; wait for the admission writer's
    // group commit so callers can read the rows it acknowledged.
    if (options.flush !== false) yield* (yield* AdmissionWriter).flush;
    return result;
  });

/** A native tx spending `11…11#spentInputIndex`: txs that must all be
 * admitted need distinct indices, since a node admits only the first spender
 * of an input. */
export const makeNativeSubmitTx = (
  spentInputIndex = 0n,
): {
  readonly txId: Buffer;
  readonly txIdHex: string;
  readonly txCanonicalCbor: Buffer;
} => {
  const txCanonicalCbor = cardanoTxBytesToMidgardNativeTxCanonicalCbor(
    makeCardanoSignedMapOutputTxBytes(undefined, spentInputIndex),
  );
  const txId = computeMidgardNativeTxId(
    decodeMidgardNativeTxFullFromCanonicalCbor(txCanonicalCbor),
  );
  return { txId, txIdHex: txId.toString("hex"), txCanonicalCbor };
};

export const makeProofSubmitTx = (): {
  readonly txId: Buffer;
  readonly txIdHex: string;
  readonly txCanonicalCbor: Buffer;
} => {
  const canonical: MidgardNativeTxCanonical = {
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      fee: 0n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  };
  const txCanonicalCbor = encodeMidgardNativeTxCanonical(
    materializeMidgardNativeTxFromCanonical(canonical),
  );
  const txId = computeMidgardNativeTxId(
    decodeMidgardNativeTxFullFromCanonicalCbor(txCanonicalCbor),
  );
  return { txId, txIdHex: txId.toString("hex"), txCanonicalCbor };
};

export const makeScriptProofSubmitTx = (
  nonce: number,
  spentInputIndex = 0n,
) => {
  const scriptBytes = encodeCbor(Buffer.from("0101002499", "hex"));
  const base = decodeMidgardNativeTxFullFromCanonicalCbor(
    makeNativeSubmitTx(spentInputIndex).txCanonicalCbor,
  );
  const canonical: MidgardNativeTxCanonical = {
    version: base.version,
    validity: base.validity,
    body: {
      ...base.body,
      fee: base.body.fee + BigInt(nonce),
    },
    witnessSet: {
      ...base.witnessSet,
      scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage([
        {
          language: "MidgardV1",
          scriptBytes,
        },
      ]),
    },
  };
  const txCanonicalCbor = encodeMidgardNativeTxCanonical(
    materializeMidgardNativeTxFromCanonical(canonical),
  );
  const txId = computeMidgardNativeTxId(
    decodeMidgardNativeTxFullFromCanonicalCbor(txCanonicalCbor),
  );
  return {
    txId,
    txIdHex: txId.toString("hex"),
    txCanonicalCbor,
    proofEnvelope: encodeMidgardProofSubmission({
      transactionCbor: txCanonicalCbor,
    }),
  };
};

/**
 * The same transaction with its script witnesses dropped: the tx id, which
 * covers only the body, is unchanged, but the canonical bytes and the full
 * hash differ.
 */
export const withoutScriptWitnesses = (txCanonicalCbor: Buffer): Buffer => {
  const full = decodeMidgardNativeTxFullFromCanonicalCbor(txCanonicalCbor);
  return encodeMidgardNativeTxCanonical(
    materializeMidgardNativeTxFromCanonical({
      version: full.version,
      validity: full.validity,
      body: full.body,
      witnessSet: {
        ...full.witnessSet,
        scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      },
    }),
  );
};

export const makeReferenceScriptProofSubmitTx = (nonce: number) => {
  const attached = makeScriptProofSubmitTx(nonce);
  const referenceOutRef = makeOutRefCbor(
    databaseTxHash(`accepted-reference-script-${nonce.toString()}`),
    0,
  );
  const attachedCanonical = decodeMidgardNativeTxFullFromCanonicalCbor(
    attached.txCanonicalCbor,
  );
  const canonical: MidgardNativeTxCanonical = {
    ...attachedCanonical,
    body: {
      ...attachedCanonical.body,
      referenceInputsPreimageCbor: encodeCbor([referenceOutRef]),
    },
    witnessSet: {
      ...attachedCanonical.witnessSet,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  };
  const txCanonicalCbor = encodeMidgardNativeTxCanonical(
    materializeMidgardNativeTxFromCanonical(canonical),
  );
  const txId = computeMidgardNativeTxId(
    decodeMidgardNativeTxFullFromCanonicalCbor(txCanonicalCbor),
  );
  return {
    ...attached,
    txId,
    txIdHex: txId.toString("hex"),
    txCanonicalCbor,
    referenceOutRef,
    proofEnvelope: encodeMidgardProofSubmission({
      transactionCbor: txCanonicalCbor,
    }),
  };
};

export const expectSubmitBody = (
  result: SubmitHttpResult,
  expected: {
    readonly status: 200 | 202;
    readonly txIdHex: string;
    readonly duplicate: boolean;
  },
): void => {
  expect(result.status).toBe(expected.status);
  expect(result.body).toEqual({
    txId: expected.txIdHex,
    status: TxAdmissionsDB.Status.Queued,
    firstSeenAt: expect.any(String),
    lastSeenAt: expect.any(String),
    duplicate: expected.duplicate,
  });
};

export const retrieveAllMempool = MempoolDB.retrievePage({
  limit: 100_000,
}).pipe(Effect.map((page) => page.entries));

export const wrapNativeSubmitTx = (txCanonicalCbor: Buffer): Buffer =>
  encodeMidgardProofSubmission({
    transactionCbor: txCanonicalCbor,
  });
