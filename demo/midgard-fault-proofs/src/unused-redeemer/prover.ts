/** Plans a whole `unusedRedeemer` proof from the bound source and predicts its
 * conviction before any transaction is spent. An honest leaf is refused.
 *
 * Supported: an accepted leaf convicted through a redeemer the producer
 * leaves unused (a repeat of an earlier pointer, a pointer with no purpose,
 * or a native purpose's pointer); a rejected leaf whose `UnusedRedeemer`
 * reason is refuted at its own index (a non-native purpose consumes the
 * redeemer, or the field has no redeemer there); and a rejected leaf whose
 * reason is recorded after an earlier unused redeemer's audit. A forced
 * admission carries no redeemers, so an admitted forced leaf has none to
 * prove. */
import * as SDK from "@al-ft/midgard-sdk";

import { directPurposeClaimRoute } from "../direct-purpose-family/claim-route.js";
import {
  directPurposeFieldPlans,
  type DirectPurposeTransition,
} from "../direct-purpose-family/custody.js";
import type { DirectPurposeContracts } from "../direct-purpose-family/family.js";
import {
  boundIndex,
  type RouteInputs,
} from "../direct-purpose-family/pointer-scan.js";
import { directPurposeSpendDescriptorPresent } from "../direct-purpose-family/script-purposes.js";
import {
  directPurposeBindTransitions,
  type DirectPurposeSourceEvidence,
  directPurposeSourceTransitions,
} from "../direct-purpose-family/source-walk.js";
import {
  buildDirectSourceIndexEvidence,
  directPurposeAt,
} from "../direct-script-derivation/index.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import {
  UNUSED_REDEEMER_CATEGORY_LABEL,
  unusedRedeemerContractRoles,
} from "./contracts.js";
import {
  unusedRedeemerClaimContext,
  unusedRedeemerCoordinate,
  unusedRedeemerWalk,
  unusedRedeemerWalkResult,
} from "./walk.js";

export class UnusedRedeemerHonestLeaf extends Error {
  constructor(message: string) {
    super(`${UNUSED_REDEEMER_CATEGORY_LABEL}: ${message}`);
    this.name = "UnusedRedeemerHonestLeaf";
  }
}

/** The raw field-8 index a rejected leaf's `UnusedRedeemer` reason names. */
export const unusedRedeemerLeafIndex = (
  material: DirectScriptMaterial,
): bigint | null => {
  const reason = material.bound.subject.rejection_reason;
  return reason !== null &&
    typeof reason === "object" &&
    "UnusedRedeemer" in reason
    ? reason.UnusedRedeemer.redeemer_index
    : null;
};

/**
 * The terminal's final state for the redeemer at `redeemerIndex`, decided
 * without custody: the walk's repeat or range result, else the claimed
 * purpose's source lookup. `null` when the pointer names a spend whose
 * descriptor the bound source cannot show, so no claim verdict is reachable.
 */
export const unusedRedeemerFinalState = async (
  inputs: RouteInputs,
  redeemerIndex: bigint,
): Promise<SDK.DirectClaimFinalState | null> => {
  const { material, evidence } = inputs;
  const walk = unusedRedeemerWalkResult(material, redeemerIndex);
  const context = unusedRedeemerClaimContext(
    inputs,
    redeemerIndex,
    walk.pointer,
  );
  if (!walk.reached) return { context, claim: "RedeemerOutOfRange" };
  if (walk.repeated) return { context, claim: "RedeemerRepeated" };
  const coordinate = unusedRedeemerCoordinate(walk.pointer);
  if (!SDK.purposeCoordinateIsValid(coordinate))
    return { context, claim: { ImpossiblePurpose: { coordinate } } };
  if (
    coordinate.kind === 0n &&
    coordinate.index < BigInt(material.fields[0]!.length) &&
    !directPurposeSpendDescriptorPresent(material, coordinate.index)
  )
    return null;
  const purpose = directPurposeAt(material, coordinate);
  if (purpose === null)
    return { context, claim: { ImpossiblePurpose: { coordinate } } };
  const { selected: source } = await evidence.lookup(purpose.script_hash);
  const selection: SDK.DirectSourceSelection = {
    selection: { ...boundIndex(inputs), coordinate, purpose },
    source,
  };
  return {
    context,
    claim:
      source === null
        ? { SourceAbsent: { selection } }
        : source.language_tag === 0n
          ? { NativeSource: { selection } }
          : { SourcePresent: { selection } },
  };
};

/** The redeemer whose walk and claim convict the leaf: the leaf's own index
 * first, then every field-8 index in order. */
type UnusedRedeemerRouteChoice = { readonly redeemerIndex: bigint };

const chooseRoute = async (
  inputs: RouteInputs,
): Promise<UnusedRedeemerRouteChoice | null> => {
  const own = unusedRedeemerLeafIndex(inputs.material);
  const count = BigInt(inputs.material.fields[8]!.length);
  const candidates = new Set([
    ...(own === null ? [] : [own]),
    ...Array.from({ length: Number(count) }, (_, index) => BigInt(index)),
  ]);
  for (const redeemerIndex of candidates) {
    const final = await unusedRedeemerFinalState(inputs, redeemerIndex);
    if (final !== null && SDK.directClaimConvicts(final))
      return { redeemerIndex };
  }
  return null;
};

export type UnusedRedeemerProofPlan = {
  readonly evidence: DirectPurposeSourceEvidence;
  readonly plan: ReturnType<typeof directPurposeFieldPlans>;
  readonly redeemerIndex: bigint;
  /** Every custody transition from `bindEvent` to the terminal's input. */
  readonly transitions: readonly DirectPurposeTransition[];
  readonly final: SDK.DirectClaimFinalState;
  readonly convicts: boolean;
};

export const prepareUnusedRedeemerProof = async ({
  material,
  contracts,
  prover,
  redeemerIndex,
}: {
  readonly material: DirectScriptMaterial;
  readonly contracts: DirectPurposeContracts;
  readonly prover: string;
  /** The redeemer to walk; the first convicting one otherwise. */
  readonly redeemerIndex?: bigint;
}): Promise<UnusedRedeemerProofPlan> => {
  const roles = unusedRedeemerContractRoles(contracts);
  const evidence = await buildDirectSourceIndexEvidence(material);
  const plan = directPurposeFieldPlans(material, prover);
  const inputs = { material, evidence, prover, plan };
  const chosen =
    redeemerIndex ??
    (await chooseRoute(inputs))?.redeemerIndex ??
    // An honest leaf naming a redeemer still plans that redeemer's route so
    // the caller can show the terminal refuse it.
    unusedRedeemerLeafIndex(material);
  if (chosen === null)
    throw new UnusedRedeemerHonestLeaf(
      "no redeemer is unused before the leaf's reason",
    );
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
  const walk = unusedRedeemerWalk(inputs, chosen);
  const claim =
    walk.context === null
      ? { transitions: [], final: walk.final! }
      : await directPurposeClaimRoute(inputs, walk.context);
  return {
    evidence,
    plan,
    redeemerIndex: chosen,
    transitions: [...source, ...walk.transitions, ...claim.transitions],
    final: claim.final,
    convicts: SDK.directClaimConvicts(claim.final),
  };
};

/** The plan, refused before spending when the final state cannot convict. */
export const requireConvictingUnusedRedeemerProof = async (
  params: Parameters<typeof prepareUnusedRedeemerProof>[0],
): Promise<UnusedRedeemerProofPlan> => {
  const prepared = await prepareUnusedRedeemerProof(params);
  if (!prepared.convicts)
    throw new UnusedRedeemerHonestLeaf(
      "the leaf's recorded reason is established; nothing to prove",
    );
  return prepared;
};

/** Stands in for the prover key where only the verdict matters: no datum
 * built against it is ever submitted, and conviction never reads it. */
const DETECTION_PROVER = "00".repeat(28);

/** The convicting redeemer, without planning custody; `null` when none
 * convicts. */
export const unusedRedeemerDetection = async (
  material: DirectScriptMaterial,
): Promise<UnusedRedeemerRouteChoice | null> => {
  const evidence = await buildDirectSourceIndexEvidence(material);
  return chooseRoute({
    material,
    evidence,
    prover: DETECTION_PROVER,
    plan: directPurposeFieldPlans(material, DETECTION_PROVER),
  });
};
