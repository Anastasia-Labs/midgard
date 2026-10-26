/** Plans a whole `missingRedeemer` proof from the bound source and predicts
 * its conviction before any transaction is spent. An honest leaf is refused.
 *
 * Supported: an accepted leaf convicted through a spend, mint, observer or
 * receive purpose whose redeemer-bearing source has no redeemer; a rejected
 * leaf whose reason names no purpose coordinate, through such a purpose when
 * the purpose phase does not admit its reason first; a rejected leaf whose
 * `RedeemerMissing` reason names any purpose coordinate. */
import * as SDK from "@al-ft/midgard-sdk";

import {
  directPurposeClaimEntry,
  directPurposeClaimRoute,
  directPurposeLeafAdmits,
  isRedeemerBearingLanguage,
} from "../direct-purpose-family/claim-route.js";
import {
  directPurposeFieldPlans,
  type DirectPurposeTransition,
} from "../direct-purpose-family/custody.js";
import type { DirectPurposeContracts } from "../direct-purpose-family/family.js";
import {
  directPurposeScriptPurposes,
  directPurposeSpendDescriptorPresent,
} from "../direct-purpose-family/script-purposes.js";
import {
  directPurposeBindTransitions,
  type DirectPurposeSourceEvidence,
  directPurposeSourceTransitions,
} from "../direct-purpose-family/source-walk.js";
import {
  buildDirectSourceIndexEvidence,
  directRedeemerAt,
} from "../direct-script-derivation/index.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import {
  MISSING_REDEEMER_CATEGORY_LABEL,
  missingRedeemerContractRoles,
} from "./contracts.js";
import { missingRedeemerObservedRoute } from "./routes.js";

export class MissingRedeemerHonestLeaf extends Error {
  constructor(message: string) {
    super(`${MISSING_REDEEMER_CATEGORY_LABEL}: ${message}`);
    this.name = "MissingRedeemerHonestLeaf";
  }
}

type Coordinate = SDK.DirectPurposeCoordinate;

/** Script purposes whose redeemer-bearing source has no redeemer. */
export const missingRedeemerObservedCandidates = async (
  material: DirectScriptMaterial,
  evidence: DirectPurposeSourceEvidence,
  purposes: readonly SDK.DirectPurpose[] = directPurposeScriptPurposes(
    material,
  ),
): Promise<readonly Coordinate[]> => {
  const out: Coordinate[] = [];
  for (const purpose of purposes) {
    const { selected } = await evidence.lookup(purpose.script_hash);
    if (
      selected !== null &&
      isRedeemerBearingLanguage(selected.language_tag) &&
      directRedeemerAt(material.fields[8]!, purpose.coordinate) === null
    )
      out.push(purpose.coordinate);
  }
  return out;
};

export type MissingRedeemerProofPlan = {
  readonly evidence: DirectPurposeSourceEvidence;
  readonly plan: ReturnType<typeof directPurposeFieldPlans>;
  /** Every custody transition from `bindEvent` to the terminal's input. */
  readonly transitions: readonly DirectPurposeTransition[];
  readonly final: SDK.DirectClaimFinalState;
  readonly convicts: boolean;
};

export const prepareMissingRedeemerProof = async ({
  material,
  contracts,
  prover,
  observed,
}: {
  readonly material: DirectScriptMaterial;
  readonly contracts: DirectPurposeContracts;
  readonly prover: string;
  /** Leaves whose reason names no purpose: the purpose to scan; the first
   * candidate otherwise. */
  readonly observed?: Coordinate;
}): Promise<MissingRedeemerProofPlan> => {
  const roles = missingRedeemerContractRoles(contracts);
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
  let route: readonly DirectPurposeTransition[];
  let claim: Awaited<ReturnType<typeof directPurposeClaimRoute>>;
  if (reason === null || SDK.purposeCoordinateOfReason(reason) === null) {
    const coordinate =
      observed ??
      (await missingRedeemerObservedCandidates(material, evidence))[0];
    if (coordinate === undefined)
      throw new MissingRedeemerHonestLeaf(
        "every script purpose with a redeemer-bearing source has a redeemer",
      );
    const observedRoute = await missingRedeemerObservedRoute(
      inputs,
      coordinate,
    );
    claim = await directPurposeClaimRoute(inputs, observedRoute.context);
    route = observedRoute.transitions;
  } else {
    if (typeof reason === "string" || !("RedeemerMissing" in reason))
      throw new Error(
        `${MISSING_REDEEMER_CATEGORY_LABEL}: the leaf reason is not RedeemerMissing`,
      );
    const entry = directPurposeClaimEntry(inputs);
    claim = await directPurposeClaimRoute(inputs, entry.context);
    route = [entry.transition];
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
export const requireConvictingMissingRedeemerProof = async (
  params: Parameters<typeof prepareMissingRedeemerProof>[0],
): Promise<MissingRedeemerProofPlan> => {
  const prepared = await prepareMissingRedeemerProof(params);
  if (!prepared.convicts)
    throw new MissingRedeemerHonestLeaf(
      "the leaf's claimed missing redeemer is established; nothing to prove",
    );
  return prepared;
};

/** Stands in for the prover key where only the verdict matters: no datum
 * built against it is ever submitted, and conviction never reads it. */
const DETECTION_PROVER = "00".repeat(28);

/**
 * Whether the bound leaf convicts, without planning custody: an accepted leaf,
 * or a rejection whose reason names no purpose and is not admitted before a
 * purpose's missing redeemer, yields its first convicting purpose; a
 * `RedeemerMissing` rejection yields its claim verdict. Every other leaf, and
 * a claimed spend with no predecessor descriptor to show, is `null`.
 */
export const missingRedeemerDetection = async (
  material: DirectScriptMaterial,
): Promise<{ readonly observed: Coordinate | null } | null> => {
  const reason = material.bound.subject.rejection_reason;
  if (reason === null || SDK.purposeCoordinateOfReason(reason) === null) {
    const purposes = directPurposeScriptPurposes(material);
    // Such a reason is admitted at every purpose coordinate or at none.
    if (
      purposes.length === 0 ||
      (reason !== null &&
        directPurposeLeafAdmits(material.bound, {
          coordinate: purposes[0]!.coordinate,
          subcheck: "RedeemerRequired",
        }))
    )
      return null;
    const evidence = await buildDirectSourceIndexEvidence(material);
    const [coordinate] = await missingRedeemerObservedCandidates(
      material,
      evidence,
      purposes,
    );
    return coordinate === undefined ? null : { observed: coordinate };
  }
  if (typeof reason === "string" || !("RedeemerMissing" in reason)) return null;
  const coordinate = SDK.purposeCoordinateOfReason(reason);
  if (
    coordinate !== null &&
    SDK.purposeCoordinateIsValid(coordinate) &&
    coordinate.kind === 0n &&
    coordinate.index < BigInt(material.fields[0]!.length) &&
    !directPurposeSpendDescriptorPresent(material, coordinate.index)
  )
    return null;
  const evidence = await buildDirectSourceIndexEvidence(material);
  const inputs = {
    material,
    evidence,
    prover: DETECTION_PROVER,
    plan: directPurposeFieldPlans(material, DETECTION_PROVER),
  };
  const claim = await directPurposeClaimRoute(
    inputs,
    directPurposeClaimEntry(inputs).context,
  );
  return SDK.directClaimConvicts(claim.final) ? { observed: null } : null;
};
