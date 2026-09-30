/**
 * `native-script-decoding` envelope/frontier measurement suite (offchain
 * design §8.2 item 1; #635).
 *
 * This file runs FIRST in the family's §9 build order because its step-02
 * check is the one place the offchain wave can still escalate (design §2.3):
 * the step-02 redeemer is the family's only step whose worst admissible
 * instance stacks three MPF membership openings, a full block header and a
 * forced leaf into one redeemer. If that worst case cannot fit the 16,384-byte
 * L1 fault-proof envelope, the fix is an on-chain format change on the wave
 * branch — a completeness finding to escalate, never something to absorb
 * offchain. Every other chart here feeds the §5.2 segment planner its byte
 * frontiers.
 *
 * Methodology is inherited from `submit-init-emulator-max-proof-fit.test.ts`:
 * measure a real instance at branch depth 0 and at the grinded adversarial
 * depth, derive the constant marginal cost per further branch level from the
 * difference, and turn the envelope into an exact exhaustion depth. The
 * conclusion for work-bounded axes follows the Q1X-F5 convention — record that
 * a 2^128 adversary can exhaust the envelope, do not pretend safety.
 *
 * Everything here is `Data.to` byte measurement plus blueprint arithmetic; no
 * transaction is evaluated, so this file does not pay the wasm UPLC heap tax
 * its emulator siblings isolate against. The redeemer-to-transaction gap is
 * covered by `STEP_TX_OVERHEAD_ALLOWANCE_BYTES` below and re-measured against
 * complete signed transactions by the §8.2(4–7) emulator journeys.
 */

import "@aiken-lang/merkle-patricia-forestry";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/native-script-decoding/contracts.js";
import "./support/submit-init-emulator-fixtures.js";
import "./support/submit-init-emulator-shared.js";
import "./native-script-decoding-envelope.step-01-redeemer-envelope-chart-both-carriages-q4.js";
import "./native-script-decoding-envelope.step-02-redeemer-envelope-chart-the-escalation-capable-check-2-3.js";
import "./native-script-decoding-envelope.step-03-redeemer-envelope-chart-windows-frames-tier-frontier.js";
