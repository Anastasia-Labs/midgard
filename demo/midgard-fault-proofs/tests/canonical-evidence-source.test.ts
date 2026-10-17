/**
 * `Q03` — canonical evidence-source API.
 *
 * Acceptance (GOAL_SPEC.md §9.2): "Builders consume verified `DaPayloadV1`/proof
 * bundles and authenticated L1 observations, not operator-private REST/DB/files
 * except labelled diagnostics."
 *
 * Positive coverage proves builders reach proof material from DA + L1 only.
 * Negative coverage proves every other input path is refused: prohibited trust
 * classes, unknown classes, diagnostic records, unauthenticated observations,
 * mutated payloads, foreign headers, and valid blocks with no violation.
 */

import "@al-ft/midgard-core/da-transport";
import "@al-ft/midgard-sdk";
import "vitest";
import "../src/bin.js";
import "../src/evidence/index.js";
import "../src/index.js";
import "../src/prepare-double-spend.js";
import "../src/transition-trace/fetch.js";
import "./helpers/canonical-block-evidence-fixture.js";
import "./canonical-evidence-source.q03-provenance-admission.js";
import "./canonical-evidence-source.q03-canonical-block-evidence.js";
import "./canonical-evidence-source.q03-canonical-evidence-builders.js";
import "./canonical-evidence-source.q03-labelled-diagnostics.js";
