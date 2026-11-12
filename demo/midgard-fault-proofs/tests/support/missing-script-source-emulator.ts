/** The `missingScriptSource` family's emulator driver: the walk-keyed
 * driver with the purpose to observe, and the verdict its forced rows
 * record. Subjects are the direct purpose subjects of
 * `missing-redeemer-emulator.subject.ts`. */
import type * as SDK from "@al-ft/midgard-sdk";

import type { DirectPurposeRole } from "../../src/direct-purpose-family/index.js";
import {
  MISSING_SCRIPT_SOURCE_DIRECT_WALK,
  type MissingScriptSourceProofPlan,
  prepareMissingScriptSourceProof,
} from "../../src/missing-script-source/index.js";
import {
  type DirectWalkHarness,
  driveDirectWalkThread,
  type RoleReference,
} from "./direct-walk-emulator.js";
import type { MissingRedeemerScenario } from "./missing-redeemer-emulator.subject.js";

/** Genuine Init, the source-appropriate step-01, and the planned custody up
 * to the terminal's input. Every thread transaction is measured. */
export const driveMissingScriptSourceThread = ({
  harness,
  scenario,
  references,
  observed,
  transitions,
}: {
  readonly harness: DirectWalkHarness;
  readonly scenario: MissingRedeemerScenario;
  readonly references: ReadonlyMap<DirectPurposeRole, RoleReference>;
  readonly observed?: SDK.DirectPurposeCoordinate;
  /** Replaces the planned custody (e.g. a prefix for refusal rows). */
  readonly transitions?: (
    prepared: MissingScriptSourceProofPlan,
  ) => MissingScriptSourceProofPlan["transitions"];
}) =>
  driveDirectWalkThread({
    harness,
    walk: MISSING_SCRIPT_SOURCE_DIRECT_WALK,
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
