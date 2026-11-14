/** The `unusedRedeemer` family's emulator driver: the walk-keyed driver
 * with the redeemer to walk, and the verdict its forced rows record.
 * Subjects are the direct purpose subjects of
 * `missing-redeemer-emulator.subject.ts`. */
import type * as SDK from "@al-ft/midgard-sdk";

import type { DirectPurposeRole } from "../../src/direct-purpose-family/index.js";
import {
  prepareUnusedRedeemerProof,
  UNUSED_REDEEMER_DIRECT_WALK,
  type UnusedRedeemerProofPlan,
} from "../../src/unused-redeemer/index.js";
import {
  type DirectWalkHarness,
  driveDirectWalkThread,
  type RoleReference,
} from "./direct-walk-emulator.js";
import type { MissingRedeemerScenario } from "./missing-redeemer-emulator.subject.js";

/** Genuine Init, the source-appropriate step-01, and the planned custody up
 * to the terminal's input. Every thread transaction is measured. */
export const driveUnusedRedeemerThread = ({
  harness,
  scenario,
  references,
  redeemerIndex,
  transitions,
}: {
  readonly harness: DirectWalkHarness;
  readonly scenario: MissingRedeemerScenario;
  readonly references: ReadonlyMap<DirectPurposeRole, RoleReference>;
  /** The redeemer to walk; the first convicting one otherwise. */
  readonly redeemerIndex?: bigint;
  /** Replaces the planned custody (e.g. a prefix for refusal rows). */
  readonly transitions?: (
    prepared: UnusedRedeemerProofPlan,
  ) => UnusedRedeemerProofPlan["transitions"];
}) =>
  driveDirectWalkThread({
    harness,
    walk: UNUSED_REDEEMER_DIRECT_WALK,
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
