/**
 * DA bond pool builders. Refusals are checked before any transaction is
 * assembled. Redeemer indices, `unlock_at` and output values are read back off
 * the transaction Lucid actually serialized and the ledger after submission,
 * on a Lucid emulator. The layout tests carry the shared always-succeeds
 * script in the pool's place; the pool validator's own checks run in the
 * node's emulator lifecycle suites. One test runs the real `InitPool` from the
 * local blueprint and measures what the pool adds to a transaction.
 */

import "node:fs";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/availability-challenge.js";
import "../src/da-attestation.js";
import "../src/da-bond-pool.js";
import "../src/da-bond-pool-transactions.js";
import "../src/fraud-proof/contracts/blueprint.js";
import "../src/index.js";
import "../src/protocol-contracts.js";
import "./da-bond-pool-transactions.setup-scene.js";
import "./da-bond-pool-transactions.init-pool.js";
import "./da-bond-pool-transactions.da-bond-pool-builders-refusals-before-assembly.js";
import "./da-bond-pool-transactions.da-bond-pool-builders-the-transaction-lucid-assembles.js";
import "./da-bond-pool-transactions.da-bond-pool-the-real-init-pool.js";
