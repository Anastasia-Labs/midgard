/** The one-step resolution's published reference scripts, as the installed
 * dispute reads them from the deployment: each holder a thread of ours
 * visits past the game, the yields its semantic resolvers invoke, and the
 * script-decode deployment a CEK script selection reads. Each is named by
 * its deployment manifest member, so the watcher resolves them like any
 * other roster entry; a yield the deployment has no member for is left out,
 * and a thread at a step that needs it waits. */
import { DEPLOYMENT_MEMBERS } from "@al-ft/midgard-core";
import type { UTxO } from "@lucid-evolution/lucid";

import { SCRIPT_DECODE_REFERENCE_SCRIPTS } from "../workflow/script-decode-prerequisite.deployment.js";
import { CANONICAL_DECODE_HOLDERS } from "./one-step-canonical-decode.js";
import { DRIVEN_CEK_CONTEXT_STAGES } from "./one-step-context.js";
import { CONTEXT_FOLLOWER_HOLDERS } from "./one-step-context-followers.js";
import { CONTEXT_ITEM_HOLDERS } from "./one-step-context-item-holders.js";
import {
  CEK_CONTEXT_STAGE_HOLDERS,
  VALIDATION_TRACE_ONE_STEP_CONTRACT_NAMES,
} from "./one-step-holders.js";
import {
  isValidationTracePhase,
  PHASE_HOLDERS,
  PHASE_RESOLUTIONS,
  phaseYields,
  type ValidationTracePhaseYield,
} from "./one-step-phase.js";
import { SCRIPT_SOURCES_REDEEMER_HOLDERS } from "./one-step-script-sources-redeemer.js";
import type { ValidationTraceOneStepReferences } from "./transactions.js";

type Holder = keyof typeof VALIDATION_TRACE_ONE_STEP_CONTRACT_NAMES;

const SELECTION_YIELDS = Object.freeze([
  "cekSelectionAuthenticate",
  "cekSelectionSuccessor",
] as const);

const PHASE_YIELDS: readonly ValidationTracePhaseYield[] = Object.freeze([
  ...new Set(
    Object.keys(PHASE_RESOLUTIONS)
      .filter(isValidationTracePhase)
      .flatMap(phaseYields),
  ),
]);

/** The manifest member of the dispute's yield `name`, if it has one. */
const yieldMember = (name: string): string | undefined =>
  Object.entries(DEPLOYMENT_MEMBERS).find(
    ([, { family, familyPaths }]) =>
      family === "validationTraceDispute" &&
      familyPaths.some(([kind, path]) => kind === "yields" && path === name),
  )?.[0];

const YIELD_MEMBERS: Readonly<Record<string, string>> = Object.freeze(
  Object.fromEntries(
    [...SELECTION_YIELDS, ...PHASE_YIELDS].flatMap((name) => {
      const member = yieldMember(name);
      return member === undefined ? [] : [[name, member]];
    }),
  ),
);

/**
 * The one-step resolution's roster entries: each role is the manifest
 * member it names, for the holders, the yields and the script-decode
 * deployment.
 */
export const VALIDATION_TRACE_ONE_STEP_REFERENCE_SCRIPTS: Readonly<
  Record<string, string>
> = Object.freeze(
  Object.fromEntries(
    [
      ...Object.values(VALIDATION_TRACE_ONE_STEP_CONTRACT_NAMES),
      ...Object.values(YIELD_MEMBERS),
      ...Object.values(SCRIPT_DECODE_REFERENCE_SCRIPTS),
    ].map((member) => [member, member]),
  ),
);

/** The script-decode deployment's references, by role, each read through
 * `reference` by its manifest member. */
export const validationTraceScriptDecodeReferences = (
  reference: (member: string) => UTxO,
): Readonly<Record<string, UTxO>> =>
  Object.freeze(
    Object.fromEntries(
      Object.entries(SCRIPT_DECODE_REFERENCE_SCRIPTS).map(([role, member]) => [
        role,
        reference(member),
      ]),
    ),
  );

/**
 * The one-step references the runner drives, each read through `reference`
 * by its manifest member: every holder, the selection's yields and each
 * phase yield the deployment has.
 */
export const validationTraceOneStepReferences = (
  reference: (member: string) => UTxO,
): ValidationTraceOneStepReferences => {
  const holder = (name: Holder) =>
    reference(VALIDATION_TRACE_ONE_STEP_CONTRACT_NAMES[name]);
  const holders = <Name extends Holder>(names: readonly Name[]) =>
    Object.freeze(
      Object.fromEntries(names.map((name) => [name, holder(name)])) as Record<
        Name,
        UTxO
      >,
    );
  const yieldOf = (name: string): UTxO | undefined => {
    const member = YIELD_MEMBERS[name];
    return member === undefined ? undefined : reference(member);
  };
  const [authenticate, successor] = SELECTION_YIELDS.map(yieldOf);
  return Object.freeze({
    boundary: holder("boundary"),
    cekPrepare: holder("prepare"),
    cekCore: holder("core"),
    settle: holder("settle"),
    arms: Object.freeze({
      compute: holder("compute"),
      machine: holder("machine"),
      mapConversion: holder("mapConversion"),
      caseConstant: holder("caseConstant"),
    }),
    ...(authenticate === undefined || successor === undefined
      ? {}
      : {
          cekSelection: Object.freeze({
            semantic: holder("selection"),
            authenticate,
            successor,
          }),
        }),
    cekContext: Object.freeze({
      semantic: holder("context"),
      control: holder("contextControl"),
      settle: holder("contextSettle"),
      stages: Object.freeze(
        Object.fromEntries(
          DRIVEN_CEK_CONTEXT_STAGES.map((stage) => [
            stage,
            holder(CEK_CONTEXT_STAGE_HOLDERS[stage]),
          ]),
        ) as Record<(typeof DRIVEN_CEK_CONTEXT_STAGES)[number], UTxO>,
      ),
      items: holders(CONTEXT_ITEM_HOLDERS),
      followers: holders(CONTEXT_FOLLOWER_HOLDERS),
    }),
    canonicalDecode: holders(CANONICAL_DECODE_HOLDERS),
    phases: holders(PHASE_HOLDERS),
    phaseYields: Object.freeze(
      Object.fromEntries(
        PHASE_YIELDS.flatMap((name) => {
          const found = yieldOf(name);
          return found === undefined ? [] : [[name, found]];
        }),
      ),
    ),
    scriptSourcesRedeemers: holders(SCRIPT_SOURCES_REDEEMER_HOLDERS),
  });
};
