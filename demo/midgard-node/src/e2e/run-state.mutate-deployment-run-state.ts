import {
  type DeploymentRunState,
  RunStateError,
} from "./run-state.deployment-run-identity.js";
import {
  createDeploymentRunState,
  loadDeploymentRunState,
  transitionDeploymentStep,
  withDeploymentRunStateLock,
  writeDeploymentRunStateAtomic,
} from "./run-state.parse-deployment-run-state.js";

export const mutateDeploymentRunState = async (
  path: string,
  createInitial: () => DeploymentRunState,
  mutate: (
    state: DeploymentRunState,
  ) => DeploymentRunState | Promise<DeploymentRunState>,
): Promise<DeploymentRunState> =>
  withDeploymentRunStateLock(path, async () => {
    const current = (await loadDeploymentRunState(path)) ?? createInitial();
    const next = await mutate(current);
    await writeDeploymentRunStateAtomic(path, next);
    return next;
  });

/** Run-state step holding every protocol initialization signed for `nonce`. */
export const INIT_PROTOCOL_STEP = "initProtocol";

/** One signed protocol initialization. */
export type InitProtocolAttempt = {
  readonly txHash: string;
  /** The first slot the transaction is no longer valid in (its TTL). */
  readonly invalidHereafterSlot: number;
};

const invalidHereafterKey = (txHash: string): string =>
  `${txHash}.invalidHereafterSlot`;

/**
 * The initialization attempts recorded in `state` for the hub-oracle nonce
 * `nonce` (`<txHash>#<index>`), oldest first.
 */
export const recordedInitProtocolAttempts = (
  state: DeploymentRunState | null,
  nonce: string,
): readonly InitProtocolAttempt[] => {
  const step = state?.steps[INIT_PROTOCOL_STEP];
  if (step?.details?.nonce !== nonce) return [];
  return (step.txHashes ?? []).map((txHash) => {
    const recorded = step.details?.[invalidHereafterKey(txHash)];
    const invalidHereafterSlot = Number(recorded);
    if (recorded === undefined || !Number.isSafeInteger(invalidHereafterSlot))
      throw new RunStateError(
        `${INIT_PROTOCOL_STEP} records no validity bound for ${txHash}`,
      );
    return { txHash, invalidHereafterSlot };
  });
};

/**
 * Records a protocol initialization and its validity bound before it is
 * submitted, the same way the nonce is, so a stop between submission and the
 * manifest write keeps its hash. Every attempt spends the same nonce, so the
 * attempts accumulate; a record for another nonce starts over.
 */
export const recordInitProtocolSubmission = (
  path: string,
  {
    network,
    nonce,
    attempt,
  }: {
    readonly network: string;
    readonly nonce: string;
    readonly attempt: InitProtocolAttempt;
  },
): Promise<DeploymentRunState> =>
  mutateDeploymentRunState(
    path,
    () => createDeploymentRunState({ mode: "resume", identity: { network } }),
    (state) => {
      const attempts = [
        ...recordedInitProtocolAttempts(state, nonce).filter(
          ({ txHash }) => txHash !== attempt.txHash,
        ),
        attempt,
      ];
      return transitionDeploymentStep(state, INIT_PROTOCOL_STEP, "submitted", {
        txHashes: attempts.map(({ txHash }) => txHash),
        message: "signed_before_submission",
        details: Object.fromEntries([
          ["nonce", nonce],
          ...attempts.map(({ txHash, invalidHereafterSlot }) => [
            invalidHereafterKey(txHash),
            invalidHereafterSlot.toString(),
          ]),
        ]),
      });
    },
  );
