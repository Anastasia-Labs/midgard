import { WALLET_SEED_PENDING } from "@al-ft/midgard-l1-follower";

import type { CommitteeL1Readiness, CommitteeL1Source } from "./l1-follower.js";

/**
 * Resolves once the follower holds the committee for nothing but an owed
 * wallet seed (the committee's next view read settles it). Never gives up:
 * while any other reason holds, it reports the reasons to `onHeld` on every
 * poll and waits, so a rollback beyond k or a missing configuration holds
 * the committee unready with the process up. `onHeld` may throw to stop the
 * wait (a one-shot run does on a reason no wait clears).
 */
export const untilCommitteeL1SourceReady = async (
  source: Pick<CommitteeL1Source, "readiness">,
  options: Readonly<{
    onHeld?: (reasons: readonly CommitteeL1Readiness[]) => void;
    pollMs?: number;
  }> = {},
): Promise<void> => {
  for (;;) {
    const reasons = source
      .readiness()
      .filter(({ reason }) => reason !== WALLET_SEED_PENDING);
    if (reasons.length === 0) return;
    options.onHeld?.(reasons);
    await new Promise((resolve) =>
      setTimeout(resolve, options.pollMs ?? 1_000),
    );
  }
};

/**
 * Resolves once the follower holds the committee for nothing, its own
 * wallets seeded included: what builds L1 transactions at startup (the
 * submitter's wallet selection and preflight, reference scripts) reads the
 * facts at the follower's cursor and the wallets' outputs. Waits the way
 * `untilCommitteeL1SourceReady` does, stepping an owed seed on every poll.
 */
export const untilCommitteeL1SourceSeeded = async (
  source: Pick<CommitteeL1Source, "readiness" | "seedWallets">,
  options: Readonly<{
    onHeld?: (reasons: readonly CommitteeL1Readiness[]) => void;
    pollMs?: number;
  }> = {},
): Promise<void> => {
  for (;;) {
    await source.seedWallets();
    const reasons = source.readiness();
    if (reasons.length === 0) return;
    options.onHeld?.(reasons);
    await new Promise((resolve) =>
      setTimeout(resolve, options.pollMs ?? 1_000),
    );
  }
};
