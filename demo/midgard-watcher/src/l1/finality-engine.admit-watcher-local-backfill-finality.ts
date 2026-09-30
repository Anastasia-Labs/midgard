import { performance } from "node:perf_hooks";

import { parseWatcherConfig } from "../runtime/config.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
} from "../runtime/deployment-identity.js";
import { sha256Canonical } from "./finality-engine.clone-external-providers.js";
import { evaluateWatcherFinality } from "./finality-engine.evaluate-watcher-finality.js";
import { watcherFinalityConfiguredSource } from "./finality-engine.external-provider-bindings-match-policy.js";
import { makeWatcherFinalityPolicy } from "./finality-engine.parse-watcher-finality-policy.js";
import {
  type WatcherFinalityPolicy,
  type WatcherFinalityResult,
} from "./finality-engine.watcher-finality-reason-codes.js";
import {
  readWatcherLocalBackfillObservation,
  type WatcherLocalBackfillObservationReceipt,
} from "./l1-adapter.js";
import {
  evaluateWatcherLocalBackfillConsistency,
  type WatcherMultiProviderConsistency,
} from "./multi-provider-consistency.js";

const localBackfillFinalityBrand = Symbol("local-backfill-finality");

export type WatcherLocalBackfillFinalityReceipt = Readonly<{
  [localBackfillFinalityBrand]: true;
}>;

type LocalBackfillFinalityRead = Readonly<{
  consistency: WatcherMultiProviderConsistency;
  result: WatcherFinalityResult;
  policy: WatcherFinalityPolicy;
  bindingDigest: string;
  sourceIdentityDigest: string;
  acquisitionDigest: string;
  point: ReturnType<
    typeof readWatcherLocalBackfillObservation
  >["capture"]["point"];
  step: 1 | 2;
  startedAtMonotonicMs: number;
  admittedAtMonotonicMs: number;
}>;

type LocalBackfillAcceptedWitness = Readonly<{
  finality: LocalBackfillFinalityRead;
  observation: ReturnType<typeof readWatcherLocalBackfillObservation>;
}>;

type LocalBackfillStep = {
  readonly observation: WatcherLocalBackfillObservationReceipt;
  readonly value: LocalBackfillFinalityRead;
  readonly witness: LocalBackfillAcceptedWitness;
  readonly predecessor: WatcherLocalBackfillFinalityReceipt | null;
  successor: WatcherLocalBackfillFinalityReceipt | null;
};

const localBackfillSteps = new WeakMap<
  WatcherLocalBackfillFinalityReceipt,
  LocalBackfillStep
>();

const localBackfillAcceptedObservations = new WeakMap<
  WatcherLocalBackfillObservationReceipt,
  WatcherLocalBackfillFinalityReceipt
>();

/** A live descriptive view; serialized views cannot restore admission. */
export const readWatcherLocalBackfillFinality = (
  receipt: WatcherLocalBackfillFinalityReceipt,
): LocalBackfillFinalityRead => {
  const owner = localBackfillSteps.get(receipt);
  if (owner === undefined)
    throw new Error("local backfill finality receipt is absent");
  readWatcherLocalBackfillObservation(owner.observation);
  return owner.value;
};

/** Reads only the identical currently live observation owned by this step. */
export const readWatcherLocalBackfillFinalityObservation = ({
  finality,
  observation,
}: Readonly<{
  finality: WatcherLocalBackfillFinalityReceipt;
  observation: WatcherLocalBackfillObservationReceipt;
}>): Readonly<{
  finality: ReturnType<typeof readWatcherLocalBackfillFinality>;
  observation: ReturnType<typeof readWatcherLocalBackfillObservation>;
}> => {
  const owner = localBackfillSteps.get(finality);
  if (owner === undefined || owner.observation !== observation)
    throw new Error(
      "local backfill finality and observation are not the identical admitted pair",
    );
  const observed = readWatcherLocalBackfillObservation(observation);
  const step = readWatcherLocalBackfillFinality(finality);
  if (
    readWatcherLocalBackfillObservation(observation) !== observed ||
    readWatcherLocalBackfillFinality(finality) !== step
  )
    throw new Error("local backfill pair changed during its read");
  return Object.freeze({ finality: step, observation: observed });
};

/**
 * Reads original accepted facts through their currently live finalized pair.
 * The first capture stays closed; these descriptive values restore no authority.
 * Re-read the current pair after asynchronous work before relying on this view.
 */
export const readWatcherLocalBackfillFinalityOriginalWitness = (
  input: Readonly<{
    finality: WatcherLocalBackfillFinalityReceipt;
    observation: WatcherLocalBackfillObservationReceipt;
  }>,
): Readonly<{
  first: LocalBackfillAcceptedWitness;
  current: LocalBackfillAcceptedWitness;
}> => {
  const pair = readWatcherLocalBackfillFinalityObservation(input);
  const owner = localBackfillSteps.get(input.finality);
  if (
    owner === undefined ||
    pair.finality !== owner.witness.finality ||
    pair.observation !== owner.witness.observation ||
    owner.value.step !== 2 ||
    owner.value.result.action !== "finalize" ||
    owner.value.result.protocolDecision !== "finality_granted" ||
    owner.value.result.state?.finalized?.visibilityCount !== "2"
  )
    throw new Error(
      "local backfill original witness requires a finalized pair",
    );
  const first =
    owner.predecessor === null
      ? undefined
      : localBackfillSteps.get(owner.predecessor);
  if (
    first === undefined ||
    first.predecessor !== null ||
    first.successor !== input.finality ||
    first.value.step !== 1 ||
    first.value.result.action !== "observe_pending" ||
    first.value.result.state?.pending?.visibilityCount !== "1" ||
    first.value.bindingDigest !== owner.value.bindingDigest ||
    first.value.sourceIdentityDigest !== owner.value.sourceIdentityDigest ||
    first.value.policy.policyDigest !== owner.value.policy.policyDigest
  )
    throw new Error("local backfill original witness predecessor differs");
  const current = readWatcherLocalBackfillFinalityObservation(input);
  if (
    current.finality !== pair.finality ||
    current.observation !== pair.observation
  )
    throw new Error("local backfill pair changed during original witness read");
  return Object.freeze({ first: first.witness, current: owner.witness });
};

/** Two process-local transitions, each backed by its own currently live capture. */
export const admitWatcherLocalBackfillFinality = (
  input: Readonly<{
    watcherConfig: unknown;
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    observation: WatcherLocalBackfillObservationReceipt;
    previous: WatcherLocalBackfillFinalityReceipt | null;
  }>,
): Readonly<{
  result: WatcherFinalityResult;
  admitted: WatcherLocalBackfillFinalityReceipt | null;
}> => {
  const {
    watcherConfig,
    deploymentIdentity,
    observation: receipt,
    previous,
  } = input;
  const observation = readWatcherLocalBackfillObservation(receipt);
  const capture = observation.capture;
  assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
  const config = parseWatcherConfig(watcherConfig);
  if (
    deploymentIdentity.manifestId !== capture.deploymentIdentityDigest ||
    deploymentIdentity.blueprintHash !== capture.blueprintHash ||
    deploymentIdentity.network !== capture.network ||
    config.targetNetwork !== capture.network ||
    sha256Canonical(config.l1.finality) !==
      sha256Canonical(capture.finalityConfig)
  )
    throw new Error(
      "local backfill deployment or finality configuration differs from capture",
    );
  const policy = makeWatcherFinalityPolicy(config, deploymentIdentity);
  const finality = capture.finalityConfig;
  if (
    policy === null ||
    policy.sourceMode !== "local_node" ||
    policy.confirmationDepth !== finality.depth.toString() ||
    policy.maximumPreFinalityRollbackDepth !==
      finality.rollback.maxDepth.toString() ||
    policy.maximumPostFinalityRecoveryDepth !==
      finality.rollback.postFinalityRecoveryMaxDepth.toString() ||
    policy.beforeFinalityRollback !== finality.rollback.beforeFinality ||
    policy.afterFinalityRollback !== finality.rollback.afterFinality
  )
    throw new Error(
      "local backfill policy differs from captured finality configuration",
    );
  const assertCurrent = () => {
    assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
    if (readWatcherLocalBackfillObservation(receipt) !== observation)
      throw new Error(
        "local backfill observation changed during finality admission",
      );
  };
  assertCurrent();
  const consistency = evaluateWatcherLocalBackfillConsistency(receipt);
  assertCurrent();
  if (
    consistency.configuredSourceDigest !==
    sha256Canonical(watcherFinalityConfiguredSource(policy))
  )
    throw new Error("local backfill policy source differs from capture");
  const bindingDigest = sha256Canonical({
    policyDigest: policy.policyDigest,
    sourceIdentityDigest: observation.sourceIdentityDigest,
    pointDigest: observation.native.chainPoint.pointDigest,
    blockContentDigest: observation.native.blockContentDigest,
  });
  // Historical private records survive closure only as facts of accepted steps.
  // They never refresh a capture or act as the current observation.
  const prior =
    previous === null ? undefined : localBackfillSteps.get(previous);
  if (previous !== null && prior === undefined)
    throw new Error("local backfill previous step is not privately admitted");
  if (prior !== undefined && prior.value.bindingDigest !== bindingDigest)
    throw new Error("local backfill previous step binding differs");
  const accepted = localBackfillAcceptedObservations.get(receipt);
  const priorSuccessor =
    prior?.successor === null || prior?.successor === undefined
      ? undefined
      : localBackfillSteps.get(prior.successor);
  const replay =
    accepted === undefined ? undefined : localBackfillSteps.get(accepted);
  const previousState =
    priorSuccessor?.value.result.state ??
    prior?.value.result.state ??
    replay?.value.result.state ??
    null;
  const evaluated = evaluateWatcherFinality(policy, previousState, consistency);
  assertCurrent();
  const noAdmission = () =>
    Object.freeze({ result: evaluated, admitted: null });
  if (
    accepted !== undefined ||
    priorSuccessor !== undefined ||
    prior?.value.step === 2
  )
    return noAdmission();
  const first = prior === undefined;
  if (
    first &&
    BigInt(capture.depthAtObservedTip) < BigInt(policy.confirmationDepth)
  )
    throw new Error(
      "local backfill first visibility is below confirmation depth",
    );
  if (
    first
      ? evaluated.action !== "observe_pending" ||
        evaluated.state?.pending?.visibilityCount !== "1"
      : evaluated.action !== "finalize" ||
        evaluated.state?.finalized?.visibilityCount !== "2"
  )
    return noAdmission();
  if (
    !first &&
    (capture.startedAtMonotonicMs <= prior.value.admittedAtMonotonicMs ||
      observation.acquisitionDigest === prior.value.acquisitionDigest ||
      BigInt(capture.depthAtObservedTip) <=
        BigInt(prior.value.result.state!.pending!.currentDepth))
  )
    throw new Error(
      "local backfill successor requires a later-started capture and actual greater depth",
    );
  assertCurrent();
  const value: LocalBackfillFinalityRead = Object.freeze({
    consistency,
    result: evaluated,
    policy,
    bindingDigest,
    sourceIdentityDigest: observation.sourceIdentityDigest,
    acquisitionDigest: observation.acquisitionDigest,
    point: capture.point,
    step: first ? 1 : 2,
    startedAtMonotonicMs: capture.startedAtMonotonicMs,
    admittedAtMonotonicMs: performance.now(),
  });
  const witness: LocalBackfillAcceptedWitness = Object.freeze({
    finality: value,
    observation,
  });
  // No callback or asynchronous gap occurs between this read and map mutation.
  assertCurrent();
  const admitted = Object.freeze({
    [localBackfillFinalityBrand]: true as const,
  });
  localBackfillSteps.set(admitted, {
    observation: receipt,
    value,
    witness,
    predecessor: previous,
    successor: null,
  });
  localBackfillAcceptedObservations.set(receipt, admitted);
  if (prior !== undefined) prior.successor = admitted;
  return Object.freeze({ result: evaluated, admitted });
};
