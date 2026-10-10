#!/usr/bin/env node

/**
 * midgard-node-tools: the e2e, stress, and acceptance tooling that drives a
 * Midgard node from the outside. It is a separate binary on purpose — none of
 * these commands belong in the operator's `dist/index.js` (AGENTS.md: demo and
 * benchmark behavior must be explicit, isolated, and unavailable by default).
 *
 * midgard-node is compiled into this bundle from source through its
 * `midgard-source` exports condition; the operator package publishes no
 * per-module dist for anything else to resolve.
 */

import "node:crypto";
import "node:fs/promises";
import "node:path";
import "@effect/sql";
import "commander";
import "effect";
import "midgard-node/commands/cli-runtime";
import "midgard-node/commands/command-utils";
import "midgard-node/commands/submit-l2-transfer";
import "midgard-node/e2e/env";
import "midgard-node/runtime-env";
import "midgard-node/services/index";
import "midgard-node/transactions/reference-scripts";
import "midgard-node/transactions/submit-deposit";
import "../package.json" with { type: "json" };
import "./commands/e2e-finalize-summary.js";
import "./commands/e2e-journal-kill-recovery-acceptance.js";
import "./commands/e2e-process-cleanup.js";
import "./commands/e2e-service.js";
import "./commands/e2e-stress-l2-throughput/index.js";
import "./commands/phase4-genesis-ledger.js";
import "./commands/stress-corpus-generate.js";
import "./commands/stress-db-metrics.js";
import "./commands/stress-environment-fingerprint.js";
import "./commands/stress-stage-metrics.js";
import "./commands/stress-wallets/index.js";
import "./e2e/runner.js";
import "./environment.js";
import "./index.registration.js";
import "./index.registration-2.js";
import "./index.registration-3.js";
import "./index.registration-4.js";
import "./index.registration-5.js";
import "./index.registration-6.js";
import "./index.registration-7.js";
import "./index.registration-8.js";
