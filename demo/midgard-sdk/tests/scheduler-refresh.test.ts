/**
 * Scheduler-refresh builder, exercised against a real Lucid Emulator.
 *
 * The index oracle deliberately does **not** come from a hand-written model of
 * Lucid's input ordering: every expected index is read back off the transaction
 * body Lucid actually assembled (and, for the scheduler output, off the ledger
 * after the transaction is submitted to the emulator). A drift between the
 * builder's `RedeemerContext` arithmetic and the serialized transaction is what
 * the on-chain validator would see, so that is what these tests compare.
 */

import "node:fs";
import "node:path";
import "node:url";
import "@al-ft/midgard-test-support/hex";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/index.js";
import "./scheduler-refresh.setup-scheduler-scene.js";
import "./scheduler-refresh.scheduler-refresh-sdk-builder-on-the-lucid-emulator.js";
import "./scheduler-refresh.scheduler-refresh-redeemer-callback-consistency.js";
