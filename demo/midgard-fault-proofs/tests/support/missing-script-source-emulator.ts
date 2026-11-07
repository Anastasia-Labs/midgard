/** Real-contract emulator fixtures for the `missingScriptSource` family: the
 * harness, the 37 published role scripts, and a driver from a genuine Init
 * through step-01 and the planned custody to the terminal. Subjects are the
 * direct purpose subjects of `missing-redeemer-emulator.subject.ts`. */
import type * as SDK from "@al-ft/midgard-sdk";

import type { DirectPurposeRole } from "../../src/direct-purpose-family/index.js";
import {
  MISSING_SCRIPT_SOURCE_FAMILY,
  type MissingScriptSourceProofPlan,
  prepareMissingScriptSourceProof,
} from "../../src/missing-script-source/index.js";
import {
  type DirectPurposeHarness,
  driveDirectPurposeThread,
  makeDirectPurposeHarness,
  publishDirectPurposeRoles,
  type RoleReference,
} from "./direct-purpose-emulator.js";
import type { MissingRedeemerScenario } from "./missing-redeemer-emulator.subject.js";

export {
  DIRECT_PURPOSE_STEP_BOUNDS as MISSING_SCRIPT_SOURCE_STEP_BOUNDS,
  type RoleReference,
  type StepMeasurement,
  stepTable,
} from "./direct-purpose-emulator.js";

export const makeMissingScriptSourceHarness = async () => {
  const harness = await makeDirectPurposeHarness("missingScriptSource");
  return { ...harness, missingScriptSource: harness.direct };
};

export type MissingScriptSourceHarness = DirectPurposeHarness & {
  readonly missingScriptSource: DirectPurposeHarness["direct"];
};

/** Publishes every role script once through the signed publication path,
 * which refuses any publication above 15,872 bytes. */
export const publishMissingScriptSourceRoles = (
  harness: DirectPurposeHarness,
) => publishDirectPurposeRoles(harness, MISSING_SCRIPT_SOURCE_FAMILY);

/** Genuine Init, the source-appropriate step-01, and the planned custody up
 * to the terminal's input. Every thread transaction is measured. */
export const driveMissingScriptSourceThread = ({
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
    prepared: MissingScriptSourceProofPlan,
  ) => MissingScriptSourceProofPlan["transitions"];
}) =>
  driveDirectPurposeThread({
    harness,
    family: MISSING_SCRIPT_SOURCE_FAMILY,
    scenario,
    references,
    prepare: (args) =>
      prepareMissingScriptSourceProof({
        ...args,
        ...(observed === undefined ? {} : { observed }),
      }),
    ...(transitions === undefined ? {} : { transitions }),
  });

export const scriptSourceMissingVerdict = (
  purposeKind: bigint,
  purposeIndex: bigint,
): SDK.OperatorVerdict => ({
  ForcedTxInvalid: {
    reason: {
      ScriptSourceMissing: {
        purpose_kind: purposeKind,
        purpose_index: purposeIndex,
      },
    },
  },
});
