import { expect } from "vitest";

import { observerOrderInvalidEvidenceCloses } from "../src/observer-order-invalid/family.js";
import {
  coverage,
  REASON_ARM,
  record,
} from "./observer-order-invalid-lifecycle.authentication-seams.js";
import {
  acceptedBlock,
  acceptedFinding,
  evidenceOf,
  stagedOf,
} from "./observer-order-invalid-lifecycle.forced-success.js";
import { makeHarness } from "./observer-order-invalid-lifecycle.make-harness.js";
import { type ObserverFieldShape } from "./support/observer-order-invalid-raw.js";

/** Init -> accepted step 01 -> step 02 -> every scan -> step 04 -> removal. */
export const acceptedSuccess = async (
  prefix: string,
  shape: ObserverFieldShape,
  observerIndex: number,
) => {
  const h = await makeHarness();
  const { setup, inclusions } = await acceptedBlock(h, [shape]);
  const finding = acceptedFinding(shape, observerIndex);
  const evidence = evidenceOf(shape, finding);
  const staged = stagedOf(shape, observerIndex);
  expect(evidence.violation).toBe(true);
  expect(observerOrderInvalidEvidenceCloses(evidence)).toBe(true);
  await h.publishField(shape);
  const initialized = await h.init(
    setup.fraudulentBlockOutRef,
    setup.headerHash,
  );
  record(`${prefix}-init`, shape.label, initialized.measurement);
  const bound = await h.step01Accepted(
    initialized.result,
    finding,
    inclusions[0]!,
    setup.fraudulentBlockOutRef,
  );
  record(`${prefix}-step01`, shape.label, bound.measurement);
  const opened = await h.step02(
    bound.result.nextThreadOutRef,
    evidence,
    shape,
    staged,
  );
  record(`${prefix}-step02`, shape.label, opened.measurement);
  const decided = await h.scanAll(
    opened.result.nextThreadOutRef,
    evidence,
    shape,
    staged,
    prefix,
  );
  const proven = await h.step04(decided, evidence);
  expect(proven.result.fraudProofUnit).toBeTruthy();
  record(`${prefix}-step04-proof-mint`, shape.label, proven.measurement);
  coverage.reason(REASON_ARM, "accepted_invalid");
  coverage.scenario("wrongful_acceptance_success");
  record(
    `${prefix}-remove`,
    shape.label,
    (await h.removal(setup.headerHash)).measurement,
  );
};
