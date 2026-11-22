/** Whether a staging output of ours is at confirmation depth. */
import { isSafe } from "@al-ft/midgard-l1-follower/heads";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import {
  FraudProofL1UnavailableError,
  isFraudProofL1WaitError,
} from "../workflow/l1-source.js";
import type { ValidationTraceStagingOutput } from "./one-step-staging-types.js";

/** Whether `staging`'s transaction is safe: at depth `confirmationDepth` or
 * deeper, as the L1 provider counts its confirmations; a transaction the
 * provider shows confirmed with no count is a wait on the L1 source, which
 * is the orchestrator's to report. */
export const validationTraceStagingDepth =
  ({
    lucid,
    confirmationDepth,
  }: {
    readonly lucid: LucidEvolution;
    readonly confirmationDepth: number;
  }) =>
  async (staging: ValidationTraceStagingOutput): Promise<boolean> => {
    const depthOf = async (): Promise<number | undefined> => {
      const status = await lucid
        .config()
        .provider?.getTransactionStatus?.(staging.txHash);
      if (status?.status !== "confirmed") return undefined;
      const { confirmations } = status.confirmation;
      // Landed, at a place on the chain the follower cannot count from.
      if (confirmations === undefined)
        throw new FraudProofL1UnavailableError(
          `the L1 provider shows staging transaction ${staging.txHash} confirmed but cannot say how deep it stands`,
        );
      return confirmations;
    };
    try {
      const at = await depthOf();
      return at !== undefined && isSafe(at, { confirmationDepth });
    } catch (cause) {
      if (isFraudProofL1WaitError(cause)) throw cause;
      return false;
    }
  };
