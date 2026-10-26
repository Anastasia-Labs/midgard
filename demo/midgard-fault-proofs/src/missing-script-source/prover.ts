/** Plans a whole `missingScriptSource` proof from the bound source and
 * predicts its conviction before any transaction is spent. An honest leaf is
 * refused.
 *
 * Supported: an accepted leaf convicted through a spend, mint, observer or
 * receive purpose whose script has no source; a rejected leaf whose reason
 * names no purpose coordinate, through such a purpose when the purpose phase
 * does not admit its reason first; a rejected leaf whose `ScriptSourceMissing`
 * reason is refuted at its own coordinate; and a rejected leaf whose
 * `ScriptSourceMissing` or `RedeemerMissing` reason names a purpose after a
 * purpose whose script has no source. */
import * as SDK from "@al-ft/midgard-sdk";

import {
  directPurposeClaimEntry,
  directPurposeClaimRoute,
  directPurposeLeafAdmits,
} from "../direct-purpose-family/claim-route.js";
import {
  directPurposeFieldPlans,
  type DirectPurposeTransition,
} from "../direct-purpose-family/custody.js";
import type { DirectPurposeContracts } from "../direct-purpose-family/family.js";
import {
  boundIndex,
  type RouteInputs,
} from "../direct-purpose-family/pointer-scan.js";
import {
  directPurposeScriptPurposes,
  directPurposeSpendDescriptorPresent,
} from "../direct-purpose-family/script-purposes.js";
import {
  directPurposeBindTransitions,
  type DirectPurposeSourceEvidence,
  directPurposeSourceTransitions,
} from "../direct-purpose-family/source-walk.js";
import { buildDirectSourceIndexEvidence } from "../direct-script-derivation/index.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import {
  MISSING_SCRIPT_SOURCE_CATEGORY_LABEL,
  missingScriptSourceContractRoles,
} from "./contracts.js";
import {
  missingScriptSourceObservedContext,
  missingScriptSourceObservedRoute,
} from "./routes.js";

export class MissingScriptSourceHonestLeaf extends Error {
  constructor(message: string) {
    super(`${MISSING_SCRIPT_SOURCE_CATEGORY_LABEL}: ${message}`);
    this.name = "MissingScriptSourceHonestLeaf";
  }
}

type Coordinate = SDK.DirectPurposeCoordinate;

/** Script purposes whose script has no source, in purpose order. */
export const missingScriptSourceObservedCandidates = async (
  material: DirectScriptMaterial,
  evidence: DirectPurposeSourceEvidence,
  purposes: readonly SDK.DirectPurpose[] = directPurposeScriptPurposes(
    material,
  ),
): Promise<readonly SDK.DirectPurpose[]> => {
  const out: SDK.DirectPurpose[] = [];
  for (const purpose of purposes)
    if ((await evidence.lookup(purpose.script_hash)).selected === null)
      out.push(purpose);
  return out;
};

/** A reason naming a spend with no predecessor descriptor: its claim domain
 * cannot show the spend, so no claim verdict is reachable. */
const claimedSpendUnshown = (material: DirectScriptMaterial): boolean => {
  const reason = material.bound.subject.rejection_reason;
  const coordinate =
    reason === null ? null : SDK.purposeCoordinateOfReason(reason);
  return (
    coordinate !== null &&
    SDK.purposeCoordinateIsValid(coordinate) &&
    coordinate.kind === 0n &&
    coordinate.index < BigInt(material.fields[0]!.length) &&
    !directPurposeSpendDescriptorPresent(material, coordinate.index)
  );
};

const isSourceMissingReason = (reason: SDK.RejectionReason): boolean =>
  typeof reason === "object" && "ScriptSourceMissing" in reason;

/** How a leaf is convicted: through the claimed-only entry (`null`), or
 * through the observed missing source at a coordinate. */
type MissingScriptSourceRouteChoice = { readonly observed: Coordinate | null };

/** The route that convicts the leaf, or `null` when none does. The claim
 * verdicts are decided without custody: the claim route is planned against
 * the observed context the observed decision would hand over. */
const chooseRoute = async (
  inputs: RouteInputs,
  observed?: Coordinate,
): Promise<MissingScriptSourceRouteChoice | null> => {
  const { material, evidence } = inputs;
  const reason = material.bound.subject.rejection_reason;
  const purposes = directPurposeScriptPurposes(material);
  if (reason === null || SDK.purposeCoordinateOfReason(reason) === null) {
    if (observed !== undefined) return { observed };
    // Such a reason is admitted at every purpose coordinate or at none.
    if (
      purposes.length === 0 ||
      (reason !== null &&
        directPurposeLeafAdmits(material.bound, {
          coordinate: purposes[0]!.coordinate,
          subcheck: "SourceRequired",
        }))
    )
      return null;
    const [first] = await missingScriptSourceObservedCandidates(
      material,
      evidence,
      purposes,
    );
    return first === undefined ? null : { observed: first.coordinate };
  }
  if (
    !isSourceMissingReason(reason) &&
    !(typeof reason === "object" && "RedeemerMissing" in reason)
  )
    return null;
  if (claimedSpendUnshown(material)) return null;
  if (observed === undefined && isSourceMissingReason(reason)) {
    const claim = await directPurposeClaimRoute(
      inputs,
      directPurposeClaimEntry(inputs).context,
    );
    if (SDK.directClaimConvicts(claim.final)) return { observed: null };
  }
  const candidates = await missingScriptSourceObservedCandidates(
    material,
    evidence,
    purposes,
  );
  for (const purpose of candidates) {
    if (observed !== undefined && !sameCoordinate(observed, purpose.coordinate))
      continue;
    const context = missingScriptSourceObservedContext(inputs, {
      selection: {
        ...boundIndex(inputs),
        coordinate: purpose.coordinate,
        purpose,
      },
      source: null,
    });
    const claim = await directPurposeClaimRoute(inputs, context);
    if (SDK.directClaimConvicts(claim.final))
      return { observed: purpose.coordinate };
  }
  return null;
};

const sameCoordinate = (left: Coordinate, right: Coordinate) =>
  left.kind === right.kind && left.index === right.index;

export type MissingScriptSourceProofPlan = {
  readonly evidence: DirectPurposeSourceEvidence;
  readonly plan: ReturnType<typeof directPurposeFieldPlans>;
  /** Every custody transition from `bindEvent` to the terminal's input. */
  readonly transitions: readonly DirectPurposeTransition[];
  readonly final: SDK.DirectClaimFinalState;
  readonly convicts: boolean;
};

export const prepareMissingScriptSourceProof = async ({
  material,
  contracts,
  prover,
  observed,
}: {
  readonly material: DirectScriptMaterial;
  readonly contracts: DirectPurposeContracts;
  readonly prover: string;
  /** The missing-source purpose to observe; the first convicting route
   * otherwise. */
  readonly observed?: Coordinate;
}): Promise<MissingScriptSourceProofPlan> => {
  const roles = missingScriptSourceContractRoles(contracts);
  const evidence = await buildDirectSourceIndexEvidence(material);
  const plan = directPurposeFieldPlans(material, prover);
  const inputs = { material, evidence, prover, plan };
  const source = [
    ...directPurposeBindTransitions(material, prover),
    ...directPurposeSourceTransitions({
      material,
      evidence,
      prover,
      plan,
      resolvedScriptHash: roles.sourceResolved.spendingScriptHash,
      witnessesScriptHash: roles.sourceWitnesses.spendingScriptHash,
    }),
  ];
  const reason = material.bound.subject.rejection_reason;
  const choice =
    (await chooseRoute(inputs, observed)) ??
    // An honest leaf still plans its claimed-only or first observed route so
    // the caller can show the terminal refuse it.
    (reason !== null && isSourceMissingReason(reason)
      ? { observed: null }
      : null);
  if (choice === null)
    throw new MissingScriptSourceHonestLeaf(
      "no script purpose lacks its source before the leaf's reason",
    );
  let route: readonly DirectPurposeTransition[];
  let claim: Awaited<ReturnType<typeof directPurposeClaimRoute>>;
  if (choice.observed === null) {
    const entry = directPurposeClaimEntry(inputs);
    claim = await directPurposeClaimRoute(inputs, entry.context);
    route = [entry.transition];
  } else {
    const observedRoute = await missingScriptSourceObservedRoute(
      inputs,
      choice.observed,
    );
    claim = await directPurposeClaimRoute(inputs, observedRoute.context);
    route = observedRoute.transitions;
  }
  return {
    evidence,
    plan,
    transitions: [...source, ...route, ...claim.transitions],
    final: claim.final,
    convicts: SDK.directClaimConvicts(claim.final),
  };
};

/** The plan, refused before spending when the final state cannot convict. */
export const requireConvictingMissingScriptSourceProof = async (
  params: Parameters<typeof prepareMissingScriptSourceProof>[0],
): Promise<MissingScriptSourceProofPlan> => {
  const prepared = await prepareMissingScriptSourceProof(params);
  if (!prepared.convicts)
    throw new MissingScriptSourceHonestLeaf(
      "the leaf's recorded reason is established; nothing to prove",
    );
  return prepared;
};

/** Stands in for the prover key where only the verdict matters: no datum
 * built against it is ever submitted, and conviction never reads it. */
const DETECTION_PROVER = "00".repeat(28);

/**
 * Whether the bound leaf convicts, without planning custody: the observed
 * missing-source coordinate of the convicting route, `null` for the
 * claimed-only route, or `null` overall when no route convicts.
 */
export const missingScriptSourceDetection = async (
  material: DirectScriptMaterial,
): Promise<MissingScriptSourceRouteChoice | null> => {
  const evidence = await buildDirectSourceIndexEvidence(material);
  return chooseRoute({
    material,
    evidence,
    prover: DETECTION_PROVER,
    plan: directPurposeFieldPlans(material, DETECTION_PROVER),
  });
};
