/**
 * #621 route-freedom journey harness: the full validation-dispute lifecycle
 * (setup → init → open → source → bisection reveals → enter-resolution →
 * prepare-resolution → prepare-selected) staged once, so each test drives the
 * semantic-resolution leg with its own build-time delivery routing — forced
 * routes, refusal probes against the same staged thread, and the recoveries.
 *
 * The recipe is the one `submit-init-emulator-validation-dispute.test.ts`
 * runs; it is duplicated here rather than refactored out of that file because
 * that file holds the two recorded expected-red rows that stand until #617's
 * regeneration, and this ticket does not reshape them.
 *
 * Everything here speaks the **Option B wire** (#619/#620/#621): the
 * committed evidence is transition-only and the two-parameter item-semantic
 * validator enforces it. Against the stale deployed blueprint these journeys
 * would all die at prepare-selected with the same `Spend[0] unexpected empty
 * list` signature as the recorded rows — an unfalsifiable red that proves
 * nothing — so suites built on this harness call
 * {@link assertRealBlueprintSpeaksOptionBV1} and fail at collection instead.
 */

import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../../src/index.js";
import "./emulator/reference-script-publisher.js";
import "./legacy-submit-emulator.js";
import "./submit-init-emulator-fixtures.js";
import "./submit-init-emulator-shared.js";
import "./route-freedom-journey.print-route-freedom-campaign-table.js";
import "./route-freedom-journey.prepare-route-freedom-journey.js";
export { prepareRouteFreedomJourney } from "./route-freedom-journey.prepare-route-freedom-journey.js";
export {
  assertRealBlueprintSpeaksOptionBV1,
  blueprintSpeaksOptionBCompleteItemWire,
  type CapturedLifecycleStage,
  type CapturedSemanticSubmission,
  printRouteFreedomCampaignTable,
  type RouteFreedomJourney,
} from "./route-freedom-journey.print-route-freedom-campaign-table.js";
