/** The `missingRedeemer` family's emulator driver: the walk-keyed driver
 * with the purpose to observe. */
import type * as SDK from "@al-ft/midgard-sdk";

import type { DirectPurposeRole } from "../../src/direct-purpose-family/index.js";
import {
  MISSING_REDEEMER_DIRECT_WALK,
  type MissingRedeemerProofPlan,
  prepareMissingRedeemerProof,
} from "../../src/missing-redeemer/index.js";
import {
  type DirectWalkHarness,
  driveDirectWalkThread,
  type RoleReference,
} from "./direct-walk-emulator.js";
import type { MissingRedeemerScenario } from "./missing-redeemer-emulator.subject.js";

/** Genuine Init, the source-appropriate step-01, and the planned custody up
 * to the terminal's input. Every thread transaction is measured. */
export const driveMissingRedeemerThread = ({
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
    prepared: MissingRedeemerProofPlan,
  ) => MissingRedeemerProofPlan["transitions"];
}) =>
  driveDirectWalkThread({
    harness,
    walk: MISSING_REDEEMER_DIRECT_WALK,
    scenario,
    references,
    prepare: (args) =>
      prepareMissingRedeemerProof({
        ...args,
        ...(observed === undefined ? {} : { observed }),
      }),
    ...(transitions === undefined ? {} : { transitions }),
  });
