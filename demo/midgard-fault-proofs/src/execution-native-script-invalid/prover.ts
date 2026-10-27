/** Plans a whole `executionNativeScriptInvalid` proof from the bound source
 * and predicts its conviction before any transaction is spent. An honest
 * leaf is refused.
 *
 * Every script purpose runs in execution order: spends, mints, observers,
 * then receives, each by ordinal. A leaf that records no reason, or a reason
 * this purpose phase does not precede, convicts on the first native script
 * that evaluates false. A leaf recording `ExecutionNativeScriptFalse`
 * convicts on an earlier native script that evaluates false, else at its own
 * coordinate when nothing executes there or the native script there is
 * satisfied. */
import * as SDK from "@al-ft/midgard-sdk";

import type { DirectPurposeTransition } from "../direct-purpose-family/custody.js";
import type { RouteInputs } from "../direct-purpose-family/pointer-scan.js";
import {
  directPurposeInputDescriptorsPresent,
  directPurposeScriptPurposes,
} from "../direct-purpose-family/script-purposes.js";
import {
  directPurposeBindTransitions,
  type DirectPurposeSourceEvidence,
  directPurposeSourceTransitions,
} from "../direct-purpose-family/source-walk.js";
import {
  buildDirectSourceIndexEvidence,
  directPurposeAt,
  type DirectScriptMaterial,
  directSpendInputAt,
} from "../direct-script-derivation/index.js";
import { CanonicalReplayPrerequisiteError } from "../workflow/replay-prerequisite.js";
import {
  EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL,
  executionNativeScriptInvalidContractRoles,
  type ExecutionNativeScriptInvalidContracts,
} from "./contracts.js";
import {
  executionNativeScriptInvalidFieldPlans,
  executionNativeScriptInvalidSharedTransition,
  type ExecutionNativeScriptInvalidTransition,
} from "./custody.js";
import { nativeEvaluationResult } from "./evaluate.js";
import {
  type ExecutionNativeScriptInvalidEntry,
  executionNativeScriptInvalidRecorded,
  executionNativeScriptInvalidRoute,
} from "./routes.js";
import {
  executionNativeScriptInvalidEvaluationContext,
  type ExecutionNativeScriptInvalidWalk,
  type ExecutionNativeScriptInvalidWalkBatch,
} from "./walk.js";

export class ExecutionNativeScriptInvalidHonestLeaf extends Error {
  constructor(message: string) {
    super(`${EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL}: ${message}`);
    this.name = "ExecutionNativeScriptInvalidHonestLeaf";
  }
}

type Coordinate = SDK.DirectPurposeCoordinate;
type Final = SDK.ExecutionNativeScriptInvalidFinalState;

const compareCoordinates = (left: Coordinate, right: Coordinate) =>
  left.kind !== right.kind
    ? left.kind < right.kind
      ? -1
      : 1
    : left.index < right.index
      ? -1
      : left.index > right.index
        ? 1
        : 0;

/** The outcome of the native script a purpose executes: `null` when its
 * selected source is not native. */
const nativeOutcome = (
  material: DirectScriptMaterial,
  evidence: DirectPurposeSourceEvidence,
  purpose: SDK.DirectPurpose,
): boolean | null => {
  const source = evidence.selected.get(purpose.script_hash);
  if (source === undefined || source.descriptor.language_tag !== 0n)
    return null;
  return nativeEvaluationResult(
    executionNativeScriptInvalidEvaluationContext(material, source),
  );
};

/** Every script purpose with a native source, in execution order, with
 * whether its script is satisfied. */
const nativeOutcomes = (
  material: DirectScriptMaterial,
  evidence: DirectPurposeSourceEvidence,
): readonly {
  readonly coordinate: Coordinate;
  readonly satisfied: boolean;
}[] =>
  [...directPurposeScriptPurposes(material)]
    .sort((left, right) =>
      compareCoordinates(left.coordinate, right.coordinate),
    )
    .flatMap((purpose) => {
      const satisfied = nativeOutcome(material, evidence, purpose);
      return satisfied === null
        ? []
        : [{ coordinate: purpose.coordinate, satisfied }];
    });

/** How the family entry refutes the recorded coordinate when no native
 * script runs there: an invalid coordinate, an observer or spend ordinal past
 * its field, a key-locked spend, or the shared purpose route otherwise. */
const recordedEntry = (
  material: DirectScriptMaterial,
  recorded: Coordinate,
): ExecutionNativeScriptInvalidEntry => {
  if (!SDK.purposeCoordinateIsValid(recorded)) return "Contradict";
  if (
    recorded.kind === 2n &&
    recorded.index >= BigInt(material.fields[3]!.length)
  )
    return "ObserveAbsent";
  if (recorded.kind === 0n) {
    if (recorded.index >= BigInt(material.fields[0]!.length))
      return "SpendAbsent";
    if (
      directSpendInputAt(material, recorded.index) !== null &&
      directPurposeAt(material, recorded) === null
    )
      return "SpendKey";
  }
  return { Observe: recorded };
};

export type ExecutionNativeScriptInvalidChoice = Readonly<{
  entry: ExecutionNativeScriptInvalidEntry;
  /** The fact the terminal reads. */
  final: Final;
}>;

const evaluatedAt = (
  material: DirectScriptMaterial,
  coordinate: Coordinate,
  satisfied: boolean,
): Final => ({
  bound: material.bound,
  fact: { Evaluated: { coordinate, satisfied } },
});

/** The convicting route, decided without custody; `null` when none
 * convicts. */
const chooseRoute = (
  material: DirectScriptMaterial,
  evidence: DirectPurposeSourceEvidence,
): ExecutionNativeScriptInvalidChoice | null => {
  const outcomes = nativeOutcomes(material, evidence);
  const recorded = executionNativeScriptInvalidRecorded(material.bound);
  for (const { coordinate, satisfied } of outcomes) {
    if (satisfied) continue;
    if (recorded !== null && compareCoordinates(coordinate, recorded) >= 0)
      break;
    const final = evaluatedAt(material, coordinate, false);
    if (SDK.executionNativeScriptInvalidConvicts(final))
      return { entry: { Observe: coordinate }, final };
  }
  if (recorded === null) return null;
  const at = outcomes.find(
    ({ coordinate }) => compareCoordinates(coordinate, recorded) === 0,
  );
  const final: Final =
    at === undefined
      ? {
          bound: material.bound,
          fact: { NoExecution: { coordinate: recorded } },
        }
      : evaluatedAt(material, recorded, at.satisfied);
  return SDK.executionNativeScriptInvalidConvicts(final)
    ? {
        entry:
          at === undefined
            ? recordedEntry(material, recorded)
            : { Observe: recorded },
        final,
      }
    : null;
};

export type ExecutionNativeScriptInvalidProofPlan = {
  readonly evidence: DirectPurposeSourceEvidence;
  readonly plan: ReturnType<typeof executionNativeScriptInvalidFieldPlans>;
  readonly entry: ExecutionNativeScriptInvalidEntry;
  /** The bind, source and shared purpose custody as the kit plans it, for
   * paging. */
  readonly source: readonly DirectPurposeTransition[];
  /** Every custody transition from `bindEvent` to the terminal's input. */
  readonly transitions: readonly ExecutionNativeScriptInvalidTransition[];
  /** The paged evaluation, when the route evaluates a native source. */
  readonly walk: ExecutionNativeScriptInvalidWalk | null;
  readonly final: Final;
  readonly convicts: boolean;
};

export const prepareExecutionNativeScriptInvalidProof = async ({
  material,
  contracts,
  prover,
  entry,
  batch,
}: {
  readonly material: DirectScriptMaterial;
  readonly contracts: ExecutionNativeScriptInvalidContracts;
  readonly prover: string;
  /** The route to take; the first convicting one otherwise. */
  readonly entry?: ExecutionNativeScriptInvalidEntry;
  readonly batch?: ExecutionNativeScriptInvalidWalkBatch;
}): Promise<ExecutionNativeScriptInvalidProofPlan> => {
  const roles = executionNativeScriptInvalidContractRoles(contracts);
  const evidence = await buildDirectSourceIndexEvidence(material);
  const plan = executionNativeScriptInvalidFieldPlans(material, prover);
  const recorded = executionNativeScriptInvalidRecorded(material.bound);
  const chosen =
    entry ??
    chooseRoute(material, evidence)?.entry ??
    // An honest leaf recording this reason still plans its own coordinate's
    // route so the caller can show the terminal refuse it.
    (recorded === null ? null : recordedEntry(material, recorded));
  if (chosen === null)
    throw new ExecutionNativeScriptInvalidHonestLeaf(
      "no native script evaluates false before the leaf's reason",
    );
  const inputs: RouteInputs = { material, evidence, prover, plan };
  const route = await executionNativeScriptInvalidRoute(inputs, chosen, batch);
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
    ...route.shared,
  ];
  return {
    evidence,
    plan,
    entry: chosen,
    source,
    transitions: [
      ...source.map(executionNativeScriptInvalidSharedTransition),
      ...route.local,
      ...(route.walk?.transitions ?? []),
    ],
    walk: route.walk,
    final: route.final,
    convicts: SDK.executionNativeScriptInvalidConvicts(route.final),
  };
};

/** The plan, refused before spending when the final state cannot convict. */
export const requireConvictingExecutionNativeScriptInvalidProof = async (
  params: Parameters<typeof prepareExecutionNativeScriptInvalidProof>[0],
): Promise<ExecutionNativeScriptInvalidProofPlan> => {
  const prepared = await prepareExecutionNativeScriptInvalidProof(params);
  if (!prepared.convicts)
    throw new ExecutionNativeScriptInvalidHonestLeaf(
      "the native scripts evaluate as the leaf records; nothing to prove",
    );
  return prepared;
};

/** The convicting route, without planning custody; `null` when none
 * convicts, when a spent or referenced input has no predecessor descriptor,
 * or when the committed bytes do not open or evaluate. */
export const executionNativeScriptInvalidDetection = async (
  material: DirectScriptMaterial,
): Promise<ExecutionNativeScriptInvalidChoice | null> => {
  try {
    if (!directPurposeInputDescriptorsPresent(material)) return null;
    return chooseRoute(
      material,
      await buildDirectSourceIndexEvidence(material),
    );
  } catch (error) {
    if (error instanceof CanonicalReplayPrerequisiteError) throw error;
    return null;
  }
};
