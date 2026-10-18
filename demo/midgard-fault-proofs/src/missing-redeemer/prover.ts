/** Plans a whole `missingRedeemer` proof from the bound source and predicts
 * its conviction before any transaction is spent. An honest leaf is refused.
 *
 * Supported: an accepted leaf convicted through an observer purpose whose
 * redeemer-bearing source has no reward redeemer; a rejected leaf whose
 * `RedeemerMissing` reason names an invalid or observer coordinate. Spend,
 * mint and receive coordinates fail closed. */
import * as SDK from "@al-ft/midgard-sdk";

import {
  buildDirectSourceIndexEvidence,
  directPurposeAt,
  directRedeemerAt,
} from "../direct-script-derivation/index.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import {
  MISSING_REDEEMER_CATEGORY_LABEL,
  missingRedeemerContractRoles,
  type MissingRedeemerContracts,
} from "./contracts.js";
import {
  missingRedeemerFieldPlans,
  type MissingRedeemerTransition,
} from "./custody.js";
import {
  isRedeemerBearingLanguage,
  missingRedeemerClaimEntry,
  missingRedeemerClaimRoute,
  missingRedeemerConvicts,
  missingRedeemerObservedRoute,
} from "./routes.js";
import {
  missingRedeemerBindTransitions,
  type MissingRedeemerSourceEvidence,
  missingRedeemerSourceTransitions,
} from "./source-walk.js";

export class MissingRedeemerHonestLeaf extends Error {
  constructor(message: string) {
    super(`${MISSING_REDEEMER_CATEGORY_LABEL}: ${message}`);
    this.name = "MissingRedeemerHonestLeaf";
  }
}

/** Observer purposes whose redeemer-bearing source has no reward redeemer. */
export const missingRedeemerObservedCandidates = async (
  material: DirectScriptMaterial,
  evidence: MissingRedeemerSourceEvidence,
): Promise<readonly bigint[]> => {
  const out: bigint[] = [];
  for (let index = 0n; index < BigInt(material.fields[3]!.length); index++) {
    const coordinate = { kind: 2n, index };
    const purpose = directPurposeAt(material, coordinate);
    if (purpose === null) continue;
    const { selected } = await evidence.lookup(purpose.script_hash);
    if (
      selected !== null &&
      isRedeemerBearingLanguage(selected.language_tag) &&
      directRedeemerAt(material.fields[8]!, coordinate) === null
    )
      out.push(index);
  }
  return out;
};

export type MissingRedeemerProofPlan = {
  readonly evidence: MissingRedeemerSourceEvidence;
  readonly plan: ReturnType<typeof missingRedeemerFieldPlans>;
  /** Every custody transition from `bindEvent` to the terminal's input. */
  readonly transitions: readonly MissingRedeemerTransition[];
  readonly final: SDK.MissingRedeemerFinalState;
  readonly convicts: boolean;
};

export const prepareMissingRedeemerProof = async ({
  material,
  contracts,
  prover,
  observedIndex,
}: {
  readonly material: DirectScriptMaterial;
  readonly contracts: MissingRedeemerContracts;
  readonly prover: string;
  /** Accepted leaves only: the observer to scan; the first candidate otherwise. */
  readonly observedIndex?: bigint;
}): Promise<MissingRedeemerProofPlan> => {
  const roles = missingRedeemerContractRoles(contracts);
  const evidence = await buildDirectSourceIndexEvidence(material);
  const plan = missingRedeemerFieldPlans(material, prover);
  const inputs = { material, evidence, prover, plan };
  const source = [
    ...missingRedeemerBindTransitions(material, prover),
    ...missingRedeemerSourceTransitions({
      material,
      evidence,
      prover,
      plan,
      resolvedScriptHash: roles.sourceResolved.spendingScriptHash,
      witnessesScriptHash: roles.sourceWitnesses.spendingScriptHash,
    }),
  ];
  const reason = material.bound.subject.rejection_reason;
  let route: readonly MissingRedeemerTransition[];
  let claim: Awaited<ReturnType<typeof missingRedeemerClaimRoute>>;
  if (reason === null) {
    const index =
      observedIndex ??
      (await missingRedeemerObservedCandidates(material, evidence))[0];
    if (index === undefined)
      throw new MissingRedeemerHonestLeaf(
        "every observer purpose with a redeemer-bearing source has a redeemer",
      );
    const observed = await missingRedeemerObservedRoute(inputs, index);
    claim = await missingRedeemerClaimRoute(inputs, observed.context);
    route = observed.transitions;
  } else {
    if (typeof reason === "string" || !("RedeemerMissing" in reason))
      throw new Error(
        `${MISSING_REDEEMER_CATEGORY_LABEL}: the leaf reason is not RedeemerMissing`,
      );
    const entry = missingRedeemerClaimEntry(inputs);
    claim = await missingRedeemerClaimRoute(inputs, entry.context);
    route = [entry.transition];
  }
  return {
    evidence,
    plan,
    transitions: [...source, ...route, ...claim.transitions],
    final: claim.final,
    convicts: missingRedeemerConvicts(claim.final),
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
 * Whether the bound leaf convicts on a shape this prover can carry, without
 * planning custody: an accepted leaf yields its first convicting observer; a
 * `RedeemerMissing` rejection naming no valid purpose or an observer purpose
 * yields its claim verdict. Every other leaf is `null`.
 */
export const missingRedeemerDetection = async (
  material: DirectScriptMaterial,
): Promise<{ readonly observedIndex: bigint | null } | null> => {
  const reason = material.bound.subject.rejection_reason;
  if (reason === null && material.fields[3]!.length === 0) return null;
  if (
    reason !== null &&
    (typeof reason === "string" || !("RedeemerMissing" in reason))
  )
    return null;
  const evidence = await buildDirectSourceIndexEvidence(material);
  if (reason === null) {
    const [index] = await missingRedeemerObservedCandidates(material, evidence);
    return index === undefined ? null : { observedIndex: index };
  }
  const coordinate = SDK.purposeCoordinateOfReason(reason);
  if (
    coordinate !== null &&
    SDK.purposeCoordinateIsValid(coordinate) &&
    coordinate.kind !== 2n
  )
    return null;
  const inputs = {
    material,
    evidence,
    prover: DETECTION_PROVER,
    plan: missingRedeemerFieldPlans(material, DETECTION_PROVER),
  };
  const claim = await missingRedeemerClaimRoute(
    inputs,
    missingRedeemerClaimEntry(inputs).context,
  );
  return missingRedeemerConvicts(claim.final) ? { observedIndex: null } : null;
};
