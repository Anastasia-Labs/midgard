/** Plans a whole `receivePurposeLanguage` proof from the bound source and
 * predicts its conviction before any transaction is spent. An honest leaf is
 * refused.
 *
 * Supported: a leaf whose recorded `ReceivePurposePlutusV3Forbidden` is
 * false (a coordinate that is not a receive coordinate, an ordinal past the
 * last receive purpose, an absent or non-PlutusV3 source, a PlutusV3 source
 * no redeemer reaches, or a forbidden execution at an earlier receive
 * purpose); an accepted leaf, or a rejected leaf whose reason does not
 * precede the execution, with a receive purpose whose PlutusV3 source a
 * redeemer reaches. */
import * as SDK from "@al-ft/midgard-sdk";

import {
  directPurposeFieldPlans,
  type DirectPurposeTransition,
} from "../direct-purpose-family/custody.js";
import type { DirectPurposeContracts } from "../direct-purpose-family/family.js";
import type { RouteInputs } from "../direct-purpose-family/pointer-scan.js";
import { directPurposeScriptPurposes } from "../direct-purpose-family/script-purposes.js";
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
  RECEIVE_PURPOSE_LANGUAGE_CATEGORY_LABEL,
  receivePurposeLanguageContractRoles,
} from "./contracts.js";
import {
  receivePurposeLanguageContradictionRoute,
  receivePurposeLanguageReceiveRoute,
  type ReceivePurposeLanguageRoute,
} from "./routes.js";

export class ReceivePurposeLanguageHonestLeaf extends Error {
  constructor(message: string) {
    super(`${RECEIVE_PURPOSE_LANGUAGE_CATEGORY_LABEL}: ${message}`);
    this.name = "ReceivePurposeLanguageHonestLeaf";
  }
}

type Coordinate = SDK.DirectPurposeCoordinate;

const RECEIVE_KIND = 3n;

/** Whether the bound leaf records `ReceivePurposePlutusV3Forbidden`. */
const recordsReceiveLanguage = (material: DirectScriptMaterial): boolean => {
  const reason = material.bound.subject.rejection_reason;
  return (
    reason !== null &&
    typeof reason === "object" &&
    "ReceivePurposePlutusV3Forbidden" in reason
  );
};

/** Receive purposes whose source is PlutusV3 and a redeemer reaches, in
 * receive order: each execution the producer forbids. */
export const receivePurposeLanguageForbiddenExecutions = async (
  material: DirectScriptMaterial,
  evidence: DirectPurposeSourceEvidence,
): Promise<readonly Coordinate[]> => {
  const out: Coordinate[] = [];
  for (const purpose of directPurposeScriptPurposes(material)) {
    if (purpose.coordinate.kind !== RECEIVE_KIND) continue;
    const { selected } = await evidence.lookup(purpose.script_hash);
    if (
      selected !== null &&
      selected.language_tag === SDK.RECEIVE_PURPOSE_LANGUAGE_PLUTUS_V3_TAG &&
      directRedeemerAt(material.fields[8]!, purpose.coordinate) !== null
    )
      out.push(purpose.coordinate);
  }
  return out;
};

/** Forbidden executions the leaf's recorded reason does not precede. */
const convictingExecutions = async (
  material: DirectScriptMaterial,
  evidence: DirectPurposeSourceEvidence,
): Promise<readonly Coordinate[]> =>
  (await receivePurposeLanguageForbiddenExecutions(material, evidence)).filter(
    (coordinate) =>
      !SDK.leafAdmitsLaterFault(
        material.bound.subject,
        material.bound.network_order_context,
        { phase: "cek", position: { coordinate, subcheck: "ReceiveLanguage" } },
      ),
  );

export type ReceivePurposeLanguageProofPlan = {
  readonly evidence: DirectPurposeSourceEvidence;
  readonly plan: ReturnType<typeof directPurposeFieldPlans>;
  /** Every custody transition from `bindEvent` to the terminal's input. */
  readonly transitions: readonly DirectPurposeTransition[];
  readonly final: SDK.DirectClaimFinalState;
  readonly convicts: boolean;
};

/** The route after the source walk. A leaf recording this arm takes its
 * recorded coordinate, or, when that execution is forbidden, the first
 * forbidden execution before it. `observed` overrides both. Any other leaf
 * takes `observed`, or the first convicting execution. */
const familyRoute = async (
  inputs: RouteInputs,
  observed: Coordinate | undefined,
): Promise<ReceivePurposeLanguageRoute> => {
  const recorded = SDK.receivePurposeLanguageRecordedCoordinate({
    bound: inputs.material.bound,
    source_index: inputs.evidence.index,
  });
  if (recorded !== null && observed === undefined) {
    if (recorded.kind !== RECEIVE_KIND || recorded.index < 0n)
      return receivePurposeLanguageContradictionRoute(inputs);
    const route = await receivePurposeLanguageReceiveRoute(
      inputs,
      recorded.index,
    );
    if (SDK.receivePurposeLanguageConvicts(route.final)) return route;
    const [earlier] = await convictingExecutions(
      inputs.material,
      inputs.evidence,
    );
    return earlier === undefined
      ? route
      : await receivePurposeLanguageReceiveRoute(inputs, earlier.index);
  }
  const coordinate =
    observed ??
    (await convictingExecutions(inputs.material, inputs.evidence))[0];
  if (coordinate === undefined)
    throw new ReceivePurposeLanguageHonestLeaf(
      "no receive purpose executes a PlutusV3 source the leaf's reason does not precede",
    );
  if (coordinate.kind !== RECEIVE_KIND)
    throw new Error(
      `${RECEIVE_PURPOSE_LANGUAGE_CATEGORY_LABEL}: the observed purpose is not a receive purpose`,
    );
  return await receivePurposeLanguageReceiveRoute(inputs, coordinate.index);
};

export const prepareReceivePurposeLanguageProof = async ({
  material,
  contracts,
  prover,
  observed,
}: {
  readonly material: DirectScriptMaterial;
  readonly contracts: DirectPurposeContracts;
  readonly prover: string;
  /** The receive purpose to observe; the leaf's recorded coordinate or
   * first convicting execution otherwise. */
  readonly observed?: Coordinate;
}): Promise<ReceivePurposeLanguageProofPlan> => {
  const roles = receivePurposeLanguageContractRoles(contracts);
  const evidence = await buildDirectSourceIndexEvidence(material);
  const plan = directPurposeFieldPlans(material, prover);
  const inputs = { material, evidence, prover, plan };
  const route = await familyRoute(inputs, observed);
  return {
    evidence,
    plan,
    transitions: [
      ...directPurposeBindTransitions(material, prover),
      ...directPurposeSourceTransitions({
        material,
        evidence,
        prover,
        plan,
        resolvedScriptHash: roles.sourceResolved.spendingScriptHash,
        witnessesScriptHash: roles.sourceWitnesses.spendingScriptHash,
      }),
      ...route.transitions,
    ],
    final: route.final,
    convicts: SDK.receivePurposeLanguageConvicts(route.final),
  };
};

/** The plan, refused before spending when the final state cannot convict. */
export const requireConvictingReceivePurposeLanguageProof = async (
  params: Parameters<typeof prepareReceivePurposeLanguageProof>[0],
): Promise<ReceivePurposeLanguageProofPlan> => {
  const prepared = await prepareReceivePurposeLanguageProof(params);
  if (!prepared.convicts)
    throw new ReceivePurposeLanguageHonestLeaf(
      "the leaf's receive-language outcome is established; nothing to prove",
    );
  return prepared;
};

/** Stands in for the prover key where only the verdict matters: no datum
 * built against it is ever submitted, and conviction never reads it. */
const DETECTION_PROVER = "00".repeat(28);

/**
 * Whether the bound leaf convicts, without planning custody: a leaf
 * recording this arm yields its claim verdict (`observed: null`); any other
 * leaf yields its first forbidden execution its reason does not precede.
 * Every other leaf is `null`.
 */
export const receivePurposeLanguageDetection = async (
  material: DirectScriptMaterial,
): Promise<{ readonly observed: Coordinate | null } | null> => {
  const evidence = await buildDirectSourceIndexEvidence(material);
  if (!recordsReceiveLanguage(material)) {
    const [coordinate] = await convictingExecutions(material, evidence);
    return coordinate === undefined ? null : { observed: coordinate };
  }
  const route = await familyRoute(
    {
      material,
      evidence,
      prover: DETECTION_PROVER,
      plan: directPurposeFieldPlans(material, DETECTION_PROVER),
    },
    undefined,
  );
  return SDK.receivePurposeLanguageConvicts(route.final)
    ? { observed: null }
    : null;
};
