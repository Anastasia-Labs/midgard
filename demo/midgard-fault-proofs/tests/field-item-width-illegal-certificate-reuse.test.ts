/**
 * Step 02 at a Certified-tier opening, driven turn by turn through the
 * workflow capture boundary: once the field certificate is on chain, the
 * next captured transaction is the step itself, and only one certificate is
 * ever minted.
 */
import { describe, expect, it } from "vitest";

import { createOwnOutputJournal } from "../src/workflow/own-outputs.js";
import {
  acceptedBlock,
  acceptedEvidence,
} from "./field-item-width-illegal-lifecycle.forced-block.js";
import { makeHarness } from "./field-item-width-illegal-lifecycle.make-harness.js";
import {
  driveCapturedStep,
  fieldPreimageCertificateCount,
} from "./support/captured-step-turns.js";
import { maximumIllegalOutputShape } from "./support/field-item-width-illegal-shapes.js";

describe("fieldItemWidthIllegal step 02 under the workflow capture boundary", () => {
  it("reuses the field certificate already on chain, so the next captured transaction is the step", async () => {
    const h = await makeHarness();
    const maximum = maximumIllegalOutputShape();
    const { setup, inclusions } = await acceptedBlock(h, [maximum]);
    const evidence = acceptedEvidence(maximum);
    expect(evidence.carriage).toBe("Certified");
    const initialized = await h.init(setup.fraudulentBlockOutRef);
    const bound = await h.step01Accepted(
      initialized.result,
      evidence,
      inclusions[0]!,
      setup.fraudulentBlockOutRef,
    );
    const threadOutRef = bound.result.nextThreadOutRef;
    const ownOutputs = createOwnOutputJournal();
    const drive = await driveCapturedStep({
      emulator: h.harness.emulator,
      threadOutRef,
      maxTurns: 6,
      invoke: async (boundary) =>
        await h.step02Turn(
          threadOutRef,
          evidence,
          maximum,
          boundary,
          ownOutputs,
        ),
    });
    expect(drive.prerequisites.length).toBeGreaterThan(0);
    expect(
      await fieldPreimageCertificateCount({
        lucid: h.harness.proverLucid,
        certificatePolicyId: h.contracts.fieldPreimageCertificatePolicyId,
      }),
    ).toBe(1n);
  }, 900_000);
});
