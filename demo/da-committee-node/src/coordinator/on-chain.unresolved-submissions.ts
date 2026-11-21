import type {
  DaAttestationCandidateRecord,
  L1SubmissionRecord,
} from "../domain.js";
import { findSubmitOutcomeUnknown } from "../l1/submit-outcome-unknown.js";
import {
  type CoordinatorPlan,
  type OnChainAttestationSubmitter,
  sameCoordinatorAction,
} from "./on-chain.on-chain-lifecycle-coordinator-deps.js";

/**
 * A header's submission whose outcome is unknown: the node may have taken
 * it. Kept until the follower's view of its id resolves it; no replacement
 * is planned meanwhile.
 */
export type UnresolvedSubmission = Readonly<{
  txHash: string;
  txKind: L1SubmissionRecord["txKind"];
  inputsUsed: readonly string[];
  /** The planned action the transaction carried out. */
  action: CoordinatorPlan;
}>;

/** The candidates a reconcile read and the action it planned from them. */
type Planned = Readonly<{
  candidates: readonly DaAttestationCandidateRecord[];
  action: CoordinatorPlan;
}>;

/** The coordinator's unresolved submissions, at most one per header. */
export class UnresolvedSubmissions {
  readonly #byHeader = new Map<string, UnresolvedSubmission>();

  /**
   * Runs one submission. One whose outcome is unknown is remembered for the
   * header under its transaction id, then the error is rethrown.
   */
  async submitting<T>(
    headerHash: string,
    submission: Omit<UnresolvedSubmission, "txHash">,
    submit: () => Promise<T>,
  ): Promise<T> {
    try {
      return await submit();
    } catch (error) {
      const txHash = findSubmitOutcomeUnknown(error)?.txHash;
      if (txHash !== undefined && txHash !== null) {
        this.#byHeader.set(headerHash, { ...submission, txHash });
      }
      throw error;
    }
  }

  /**
   * Plans the header, once its unresolved submission, if any, is resolved
   * by the follower's view of its id (`submissionStatus`):
   * - still pending, or its status unreadable: throws, and nothing is
   *   planned (a submitter that cannot resolve an id holds it the same way);
   * - released (it never landed and can no longer): planned afresh;
   * - landed: re-planned once the chain shows it (`planOnceShown` returns a
   *   different action), then recorded and forgotten; until then it throws
   *   and stays. A landed apply ends the lifecycle: `undefined`.
   */
  async plan(
    headerHash: string,
    steps: Readonly<{
      submitter: Pick<OnChainAttestationSubmitter, "submissionStatus">;
      plan: () => Promise<Planned>;
      planOnceShown: (action: CoordinatorPlan) => Promise<Planned>;
      record: (landed: UnresolvedSubmission) => Promise<void>;
    }>,
  ): Promise<Planned | undefined> {
    const unresolved = this.#byHeader.get(headerHash);
    if (unresolved === undefined) return steps.plan();
    const status =
      (await steps.submitter.submissionStatus?.(unresolved.txHash)) ??
      "pending";
    if (status === "pending") {
      throw new Error(
        `DA attestation ${unresolved.txKind} transaction ${unresolved.txHash} for ${headerHash} has an unknown submit outcome and has not landed or been released; no replacement is built meanwhile`,
      );
    }
    if (status === "released") {
      this.#byHeader.delete(headerHash);
      return steps.plan();
    }
    const planned =
      unresolved.txKind === "apply"
        ? undefined
        : await steps.planOnceShown(unresolved.action);
    if (
      planned !== undefined &&
      sameCoordinatorAction(planned.action, unresolved.action)
    ) {
      throw new Error(
        `DA attestation ${unresolved.txKind} transaction ${unresolved.txHash} for ${headerHash} landed but was not visible after confirmation`,
      );
    }
    this.#byHeader.delete(headerHash);
    await steps.record(unresolved);
    return planned;
  }
}
