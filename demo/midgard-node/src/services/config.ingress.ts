import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { Config } from "effect";

import { SUBMIT_HTTP_BODY_MAX_BYTES } from "../commands/http/submission-capacity.js";
import {
  DEFAULT_MAX_DURABLE_ADMISSION_BACKLOG,
  DEFAULT_MAX_DURABLE_ADMISSION_BACKLOG_BYTES,
  readinessDurableAdmissionBacklogLimit,
} from "./config.admission-backlog.js";
import { httpIngressConfig } from "./config.http-proxy-peers.js";
import { l2TxPeersConfig } from "./config.l2-tx-peers.js";
import {
  integerConfig,
  nonNegativeSafeIntegerConfig,
  positiveSafeIntegerConfig,
  stringConfig,
} from "./config.parse.js";

/**
 * What the node serves and takes in: its HTTP API, the L2 peers it shares
 * transactions with, submission ingress and the durable admission backlog,
 * readiness and telemetry.
 */
export const ingressConfig = Config.all([
  Config.all({
    PORT: integerConfig("PORT", 3000),
    /** The other operators' node URLs this node shares accepted L2
     * transactions with (`config.l2-tx-peers.ts`); empty turns sharing off. */
    L2_TX_PEERS: l2TxPeersConfig,
    MAX_DURABLE_ADMISSION_BACKLOG: integerConfig(
      "MAX_DURABLE_ADMISSION_BACKLOG",
      DEFAULT_MAX_DURABLE_ADMISSION_BACKLOG,
    ),
    MAX_DURABLE_ADMISSION_BACKLOG_BYTES: positiveSafeIntegerConfig(
      "MAX_DURABLE_ADMISSION_BACKLOG_BYTES",
      DEFAULT_MAX_DURABLE_ADMISSION_BACKLOG_BYTES,
    ),
    SUBMIT_INGRESS_MAX_CONCURRENCY: positiveSafeIntegerConfig(
      "SUBMIT_INGRESS_MAX_CONCURRENCY",
      4,
    ),
    /** Threads of the submit check pool (`services/submit-check-pool.ts`),
     * which checks `/submit` and `/peer/txs` alike: sized for cluster TPS. */
    SUBMIT_INGRESS_WORKER_POOL_SIZE: positiveSafeIntegerConfig(
      "SUBMIT_INGRESS_WORKER_POOL_SIZE",
      4,
    ),
    /** `/submit` requests read, queued for or in a submit check worker at
     * once, and the HTTP ingress's in-flight bound for `/submit`; past it a
     * request is answered 503. */
    SUBMIT_INGRESS_WORKER_QUEUE: positiveSafeIntegerConfig(
      "SUBMIT_INGRESS_WORKER_QUEUE",
      256,
    ),
    /** Body bytes `/submit` requests hold while read, queued for or in a
     * submit check worker; a peer request's bytes are bounded on their own.
     * At least one largest submission body must fit. */
    SUBMIT_INGRESS_MAX_IN_FLIGHT_BYTES: integerConfig(
      "SUBMIT_INGRESS_MAX_IN_FLIGHT_BYTES",
      MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes,
    ).pipe(
      Config.mapAttempt((value) => {
        if (
          !Number.isSafeInteger(value) ||
          value < SUBMIT_HTTP_BODY_MAX_BYTES
        ) {
          throw new Error(
            `SUBMIT_INGRESS_MAX_IN_FLIGHT_BYTES must be a safe integer at least ${SUBMIT_HTTP_BODY_MAX_BYTES.toString()}`,
          );
        }
        return value;
      }),
    ),
    MAX_SUBMIT_TX_CBOR_BYTES: integerConfig(
      "MAX_SUBMIT_TX_CBOR_BYTES",
      MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes,
    ).pipe(
      Config.mapAttempt((value) => {
        if (
          !Number.isSafeInteger(value) ||
          value <= 0 ||
          value > MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes
        ) {
          throw new Error(
            `MAX_SUBMIT_TX_CBOR_BYTES must be between 1 and ${MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes.toString()}`,
          );
        }
        return value;
      }),
    ),
    ADMISSION_BACKLOG_REFRESH_MS: positiveSafeIntegerConfig(
      "ADMISSION_BACKLOG_REFRESH_MS",
      500,
    ),
    /** Terminal admission rows older than this are deleted; 0 keeps them. */
    ADMISSION_TERMINAL_RETENTION_MS: nonNegativeSafeIntegerConfig(
      "ADMISSION_TERMINAL_RETENTION_MS",
      3_600_000,
    ),
    READINESS_MAX_HEARTBEAT_AGE_MS: integerConfig(
      "READINESS_MAX_HEARTBEAT_AGE_MS",
      120_000,
    ),
    READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS: positiveSafeIntegerConfig(
      "READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS",
      30_000,
    ),
    READINESS_MAX_DURABLE_ADMISSION_AGE_MS: integerConfig(
      "READINESS_MAX_DURABLE_ADMISSION_AGE_MS",
      120_000,
    ),
    PROM_METRICS_PORT: integerConfig("PROM_METRICS_PORT", 9464),
    OLTP_EXPORTER_URL: stringConfig(
      "OLTP_EXPORTER_URL",
      "http://0.0.0.0:4318/v1/traces",
    ),
  }),
  httpIngressConfig,
]).pipe(
  Config.map(([settings, httpIngress]) => ({
    ...settings,
    ...httpIngress,
    READINESS_MAX_DURABLE_ADMISSION_BACKLOG:
      readinessDurableAdmissionBacklogLimit(
        settings.MAX_DURABLE_ADMISSION_BACKLOG,
      ),
  })),
);
