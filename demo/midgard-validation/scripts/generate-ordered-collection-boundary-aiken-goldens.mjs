#!/usr/bin/env node

/**
 * Rebinds the Aiken constants produced by the four genuine signed-Cardano
 * ordered-collection boundary suites:
 *
 *   * C20-7 — field 4/7, the coupled signer/vkey-witness maximum
 *     (`tests/ordered-collection-signer-witness-boundary.test.ts`);
 *   * C20-6 — field 3/6, the observer/native-script maximum
 *     (`tests/ordered-collection-observer-native-script-boundary.test.ts`);
 *   * the field-8 spend-redeemer maximum
 *     (`tests/ordered-collection-redeemer-boundary.test.ts`);
 *   * the field-2 inline-datum blob maximum
 *     (`tests/blob-chunk-boundary.test.ts`).
 *
 * All four were hand-mirrored families before #588: the suites search for the
 * exact transaction that sits on the Cardano envelope, and the Aiken modules then
 * assert against its bytes — but nothing carried those bytes across. Greening the
 * Aiken side after a codec change meant a human copying ~30 kB of hex out of a
 * terminal, which is how cross-language drift is born, and which #586 is the live
 * proof of.
 *
 * **Why this generator runs the suites instead of recomputing the boundary.**
 * The boundary is not a value that can be recomputed from a short declaration: it
 * is the result of a binary search over signed Cardano transactions built through
 * an emulator, and that search lives in `tests/helpers/ordered-collection-boundary.ts`
 * — 2,600 lines that reach into four sibling packages' `src/`. There is exactly
 * one implementation of it and it is only loadable under vitest. So the suites
 * remain the producers, and they publish their vectors on the
 * `MIDGARD_WRITE_AIKEN_VECTOR` channel (`tests/helpers/aiken-vector-channel.ts`)
 * *after* asserting them against their own pinned expectations. This script owns
 * the other half: the mapping from vector to Aiken constant, and the `--check`
 * contract. A vector this generator can see is a vector its suite has already
 * accepted.
 *
 * The suites take about fifteen seconds in total.
 *
 * usage: node scripts/generate-ordered-collection-boundary-aiken-goldens.mjs [--check]
 */

import "node:child_process";
import "node:fs";
import "node:os";
import "node:path";
import "node:url";
import "@al-ft/midgard-core/scripts/golden-channel.mjs";
import "@al-ft/midgard-core";
import "./generate-ordered-collection-boundary-aiken-goldens.terminal-fixture-constants.mjs";
import "./generate-ordered-collection-boundary-aiken-goldens.aiken-families.mjs";
