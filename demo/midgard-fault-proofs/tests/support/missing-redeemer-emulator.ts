/** Real-contract emulator fixtures for the `missingRedeemer` family: the
 * harness, the 39 published role scripts, and a driver from a genuine Init
 * through step-01 and the planned custody to the terminal. */
import type * as SDK from "@al-ft/midgard-sdk";

import type { DirectPurposeRole } from "../../src/direct-purpose-family/index.js";
import {
  MISSING_REDEEMER_FAMILY,
  type MissingRedeemerProofPlan,
  prepareMissingRedeemerProof,
} from "../../src/missing-redeemer/index.js";
import {
  DIRECT_PURPOSE_STEP_BOUNDS,
  type DirectPurposeHarness,
  driveDirectPurposeThread,
  makeDirectPurposeHarness,
  publishDirectPurposeRoles,
  type RoleReference,
} from "./direct-purpose-emulator.js";
import type { MissingRedeemerScenario } from "./missing-redeemer-emulator.subject.js";

export {
  type RoleReference,
  type StepMeasurement,
  stepTable,
} from "./direct-purpose-emulator.js";

export const makeMissingRedeemerHarness = async () => {
  const harness = await makeDirectPurposeHarness("missingRedeemer");
  return { ...harness, missingRedeemer: harness.direct };
};

export type MissingRedeemerHarness = DirectPurposeHarness & {
  readonly missingRedeemer: DirectPurposeHarness["direct"];
};

/** Publishes every role script once through the signed publication path,
 * which refuses any publication above 15,872 bytes. */
export const publishMissingRedeemerRoles = (harness: DirectPurposeHarness) =>
  publishDirectPurposeRoles(harness, MISSING_REDEEMER_FAMILY);

/** Genuine Init, the source-appropriate step-01, and the planned custody up
 * to the terminal's input. Every thread transaction is measured. */
export const driveMissingRedeemerThread = ({
  harness,
  scenario,
  references,
  observed,
  transitions,
}: {
  readonly harness: DirectPurposeHarness;
  readonly scenario: MissingRedeemerScenario;
  readonly references: ReadonlyMap<DirectPurposeRole, RoleReference>;
  readonly observed?: SDK.DirectPurposeCoordinate;
  /** Replaces the planned custody (e.g. a prefix for refusal rows). */
  readonly transitions?: (
    prepared: MissingRedeemerProofPlan,
  ) => MissingRedeemerProofPlan["transitions"];
}) =>
  driveDirectPurposeThread({
    harness,
    family: MISSING_REDEEMER_FAMILY,
    scenario,
    references,
    prepare: (args) =>
      prepareMissingRedeemerProof({
        ...args,
        ...(observed === undefined ? {} : { observed }),
      }),
    ...(transitions === undefined ? {} : { transitions }),
  });

/** Per-step ex-unit and size bounds every thread transaction must meet. */
export const MISSING_REDEEMER_STEP_BOUNDS = DIRECT_PURPOSE_STEP_BOUNDS;
