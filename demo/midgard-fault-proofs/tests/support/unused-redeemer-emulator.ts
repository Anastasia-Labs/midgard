/** Real-contract emulator fixtures for the `unusedRedeemer` family: the
 * harness, the 25 published role scripts, and a driver from a genuine Init
 * through step-01 and the planned custody to the terminal. Subjects are the
 * direct purpose subjects of `missing-redeemer-emulator.subject.ts`. */
import type * as SDK from "@al-ft/midgard-sdk";

import type { DirectPurposeRole } from "../../src/direct-purpose-family/index.js";
import {
  prepareUnusedRedeemerProof,
  UNUSED_REDEEMER_FAMILY,
  type UnusedRedeemerProofPlan,
} from "../../src/unused-redeemer/index.js";
import {
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
  DIRECT_PURPOSE_STEP_BOUNDS as UNUSED_REDEEMER_STEP_BOUNDS,
} from "./direct-purpose-emulator.js";

export const makeUnusedRedeemerHarness = async () => {
  const harness = await makeDirectPurposeHarness("unusedRedeemer");
  return { ...harness, unusedRedeemer: harness.direct };
};

export type UnusedRedeemerHarness = DirectPurposeHarness & {
  readonly unusedRedeemer: DirectPurposeHarness["direct"];
};

/** Publishes every role script once through the signed publication path,
 * which refuses any publication above 15,872 bytes. */
export const publishUnusedRedeemerRoles = (harness: DirectPurposeHarness) =>
  publishDirectPurposeRoles(harness, UNUSED_REDEEMER_FAMILY);

/** Genuine Init, the source-appropriate step-01, and the planned custody up
 * to the terminal's input. Every thread transaction is measured. */
export const driveUnusedRedeemerThread = ({
  harness,
  scenario,
  references,
  redeemerIndex,
  transitions,
}: {
  readonly harness: DirectPurposeHarness;
  readonly scenario: MissingRedeemerScenario;
  readonly references: ReadonlyMap<DirectPurposeRole, RoleReference>;
  /** The redeemer to walk; the first convicting one otherwise. */
  readonly redeemerIndex?: bigint;
  /** Replaces the planned custody (e.g. a prefix for refusal rows). */
  readonly transitions?: (
    prepared: UnusedRedeemerProofPlan,
  ) => UnusedRedeemerProofPlan["transitions"];
}) =>
  driveDirectPurposeThread({
    harness,
    family: UNUSED_REDEEMER_FAMILY,
    scenario,
    references,
    prepare: (args) =>
      prepareUnusedRedeemerProof({
        ...args,
        ...(redeemerIndex === undefined ? {} : { redeemerIndex }),
      }),
    ...(transitions === undefined ? {} : { transitions }),
  });

export const unusedRedeemerVerdict = (
  redeemerIndex: bigint,
): SDK.OperatorVerdict => ({
  ForcedTxInvalid: {
    reason: { UnusedRedeemer: { redeemer_index: redeemerIndex } },
  },
});
