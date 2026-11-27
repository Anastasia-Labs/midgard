/**
 * Canonical evidence-source API.
 *
 * Builders consume verified `DaPayloadV1`/proof bundles and authenticated L1
 * observations, not operator-private REST/DB/files except labelled
 * diagnostics.
 *
 * Positive coverage proves builders reach proof material from DA + L1 only.
 * Negative coverage proves every other input path is refused: prohibited trust
 * classes, unknown classes, diagnostic records, unauthenticated observations,
 * mutated payloads, foreign headers, and valid blocks with no violation.
 */

import "@al-ft/midgard-core/da-transport";
import "@al-ft/midgard-sdk/core";
import "@al-ft/midgard-sdk/da";
import "@al-ft/midgard-sdk/users";
import "@al-ft/midgard-sdk/fraud-proof";
import "vitest";
import "../src/bin.js";
import "../src/evidence/canonical-block-evidence.js";
import "../src/evidence/prepare-from-evidence.js";
import "../src/index.js";
import "../src/prepare-double-spend.resolve-double-spend-pair.js";
import "../src/transition-trace/fetch.da-libp2p-retained-da-source.js";
import "../src/transition-trace/fetch.fetch-retained-da-payload-by-header-hash.js";
import "./helpers/canonical-block-evidence-fixture.js";
import "./canonical-evidence-source.provenance-admission.js";
import "./canonical-evidence-source.canonical-block-evidence.js";
import "./canonical-evidence-source.canonical-evidence-builders.js";
import "./canonical-evidence-source.labelled-diagnostics.js";
