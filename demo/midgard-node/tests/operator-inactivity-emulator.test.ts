/**
 * Stalled-operator strike and takeover, against the real compiled validators.
 *
 * The emulator does not run phase-2 scripts, so every positive case here is
 * proven by submitting the transaction (`localUPLCEval: true` evaluates the
 * deployed scheduler and active-operators scripts while the transaction is
 * completed) and every negative case is proven by that same evaluation
 * refusing to complete it. `expectInactivityStrikeRefusal` additionally checks
 * the refusal did not come from one of the builder's own pre-flight guards, so
 * a refusal always means an on-chain check fired.
 *
 * Scheduler rotation, which decides the whole topology of these tests: the
 * next shift belongs to the active-operators element whose `next` link points
 * at the current operator, so the shift walks the key-ascending list
 * *backwards*, and rewinds to the tail once it reaches the element the root
 * links to. `AppointFirstOperator` can therefore only appoint the tail — the
 * greatest key hash — and the fixture's operators are sorted ascending, so
 * `operators[n-1]` starts the rotation and `operators[0]` is the element that
 * rewinds.
 */

import "@al-ft/midgard-sdk";
import "effect";
import "vitest";
import "./helpers/operator-inactivity.js";
import "./operator-inactivity-emulator.expect-neglected-event-strike.js";
import "./operator-inactivity-emulator.stalled-operator-strike-and-takeover.js";
import "./operator-inactivity-emulator.inactivity-takeover-planner.js";
