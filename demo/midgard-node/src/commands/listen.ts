/**
 * Runtime entrypoint for the long-running midgard node process.
 * This module wires startup invariants, the HTTP server, and background fibers,
 * but should stay free of endpoint logic and other domain-specific details.
 */

import "node:http";
import "@al-ft/midgard-core/error-format";
import "@effect/opentelemetry";
import "@effect/platform";
import "@effect/platform-node";
import "@effect/sql";
import "@opentelemetry/exporter-prometheus";
import "@opentelemetry/exporter-trace-otlp-http";
import "@opentelemetry/sdk-trace-base";
import "effect";
import "../da/libp2p-producer.run-da-libp2p-preflight.js";
import "../da/libp2p-producer.parse-da-producer-publication-manifest.js";
import "../da/libp2p-producer.probe-da-envelope-capabilities.js";
import "../da/libp2p-producer.create-da-libp2p-producer-transport.js";
import "../da/libp2p-producer.publication-transport.js";
import "../da/startup.js";
import "../database/index.js";
import "../e2e/phase1-accept-crash-checkpoint.js";
import "../fibers/admission-backlog-gauge.js";
import "../fibers/tx-queue-processor.tx-queue-processor-drain-once.js";
import "../fibers/settlement.js";
import "../genesis.js";
import "../provider-retry.js";
import "../services/midgard-contracts.make-midgard-contract-runtime.js";
import "../services/database.js";
import "../services/lucid.js";
import "../services/config.make-config.js";
import "../services/globals.globals.js";
import "../services/globals.with-l1-control-plane-held.js";
import "../services/admission-writer.validate-options.js";
import "../services/mempool-ledger-cache.make-mempool-ledger-cache.js";
import "../services/admission-input-claims.rebuild.js";
import "../services/validation-pool.make-validation-pool.js";
import "../services/write-behind.summarize-write-behind-telemetry.js";
import "../services/native-mpf-startup.js";
import "../services/settlement.reconcile-attempt.js";
import "../workers/commit-block-header/da-payload-backfill.js";
import "./http/router.js";
import "./http/submit.js";
import "./listen-startup.ensure-protocol-initialized-on-startup.js";
import "./startup-policy.js";
import "./listen.retained-payload-server-thread.js";
import "./listen.run-node.js";
export { runNode } from "./listen.run-node.js";
