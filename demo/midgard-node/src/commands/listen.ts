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
import "../da/libp2p-producer.js";
import "../da/startup.js";
import "../database/index.js";
import "../e2e/phase1-accept-crash-checkpoint.js";
import "../fibers/index.js";
import "../fibers/settlement.js";
import "../genesis.js";
import "../provider-retry.js";
import "../services/event-history-runtime.js";
import "../services/index.js";
import "../services/native-mpf-startup.js";
import "../services/settlement.js";
import "../workers/commit-block-header/da-payload-backfill.js";
import "./listen-router.js";
import "./listen-startup.js";
import "./startup-policy.js";
import "./listen.retained-payload-server-thread.js";
import "./listen.run-node.js";
export { runNode } from "./listen.run-node.js";
