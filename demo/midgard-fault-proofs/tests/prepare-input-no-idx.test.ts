/**
 * `Q13` — `input-no-idx` (`nonExistentInputNoIndex`) off-chain surface.
 *
 * Acceptance (GOAL_SPEC.md §9.1 outputs 6-8): a canonical evidence definition,
 * evidence built from retained public data through the Q03 evidence-source
 * API, resumable prepare tooling, a valid-block negative, and a complete-item
 * proof-fit measurement.
 */

import "node:fs/promises";
import "node:os";
import "node:path";
import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk/core";
import "@al-ft/midgard-sdk/da";
import "@al-ft/midgard-sdk/fraud-proof";
import "@al-ft/midgard-test-support/hex";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/evidence/canonical-block-evidence.js";
import "../src/index.js";
import "../src/prepare-double-spend.resolve-double-spend-pair.js";
import "../src/prepare-input-no-idx.read-value.js";
import "../src/prepare-input-no-idx.midgard-tx-output-from-canonical-cbor.js";
import "../src/prepare-input-no-idx.prepare-input-no-idx-from-transactions.js";
import "../src/prepare-input-no-idx.prepare-input-no-idx-from-canonical-evidence.js";
import "./helpers/canonical-block-evidence-fixture.js";
import "./prepare-input-no-idx.make-native-tx.js";
import "./prepare-input-no-idx.q13-input-no-idx-canonical-evidence.js";
import "./prepare-input-no-idx.q13-input-no-idx-q03-evidence-gates.js";
