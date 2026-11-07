/**
 * The watcher judges a forced order due by its inclusion time alone, as the
 * on-chain transition-trace proof does. The order's native validity interval is
 * in slots and only decides the machine verdict at `block_slot`; a due check
 * that compared it with the header's millisecond event window would read a
 * bounded interval as never due, so the watcher would prepare a doomed
 * out-of-window proof against the honest block below.
 */
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { detectStructuralTransitionTraceFaults } from "../src/transition-trace/replay-authority.js";
import { RELEASE_L1_FINALITY_POLICY } from "../src/workflow/release-finality-policy.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import {
  buildRetainedPlutusIdentityFixture,
  captureRetainedPlutusIdentityOrigins,
} from "./support/retained-reason-classifier.js";

// The fixture block's slot and event window (`blockSlot` and
// `blockEndTimeMs` defaults, with the default 60 s window).
const BLOCK_SLOT = 100n;
const BLOCK_END_TIME = 1_750_000_000_000n;

const coverageFor = async (inclusionTime: bigint) => {
  const retained = await buildRetainedPlutusIdentityFixture(
    { verdict: "accepted" },
    {
      sourceKind: "forced",
      validityInterval: { start: BLOCK_SLOT - 10n, end: BLOCK_SLOT + 10n },
    },
  );
  const l1Events = await captureRetainedPlutusIdentityOrigins(retained, {
    inclusionTime,
  });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(retained.block),
    payloadEnvelopeCbor: retained.block.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "retained-fixture/forced-due-window",
      grade: "security",
    },
    minimumConfirmationDepth: RELEASE_L1_FINALITY_POLICY.confirmationDepth,
  });
  return {
    orderKey: retained.orderKey,
    ...(await detectStructuralTransitionTraceFaults({ evidence, l1Events })),
  };
};

describe("transition-trace forced order due window", () => {
  it("finds nothing against a block that includes a due bounded-validity order", async () => {
    const { coverage, detections } = await coverageFor(BLOCK_END_TIME);
    expect(coverage.omitted).toEqual([]);
    expect(coverage.outside).toEqual([]);
    expect(detections).toEqual([]);
  });

  it("reports the same order outside the window once its inclusion time is past the block", async () => {
    const { coverage, orderKey } = await coverageFor(BLOCK_END_TIME + 1n);
    expect(coverage.omitted).toEqual([]);
    expect(coverage.outside).toEqual([
      expect.objectContaining({
        kind: "forcedTransaction",
        txOrderId: orderKey,
      }),
    ]);
  });
});
