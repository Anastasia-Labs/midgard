/** Every installed family's replay in catalogue order, as the catalogue
 * replay builds it, without the transition-trace family, which needs raw L1
 * events admitted for the header. The benchmark blocks hold only normal
 * transactions: no fabricated-event authority is asked for chain history. */
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  credentialToAddress,
  type LucidEvolution,
} from "@lucid-evolution/lucid";

import * as completeReplay from "../src/workflow/complete-replay.js";
import { createFabricatedDepositEvidenceAuthority } from "../src/workflow/fabricated-deposit-evidence.js";
import { createFabricatedWithdrawalEvidenceAuthority } from "../src/workflow/fabricated-withdrawal-evidence.js";
import { INSTALLED_FRAUD_PROOF_CATEGORY_ORDER } from "../src/workflow/installed-categories.js";

const unusedEventProvider = new Proxy(
  {},
  {
    get: () => () => {
      throw new Error("the benchmark block has no deposits or withdrawals");
    },
  },
) as LucidEvolution;

const retention = {
  inlineLimitBytes: 512n,
  maxPayloadBytes: 5000n,
  maxPayloadNodes: 512n,
  retentionAddress: credentialToAddress("Preprod", {
    type: "Script",
    hash: "ee".repeat(28),
  }),
};

const authority = {
  lucid: unusedEventProvider,
  network: "Preprod" as const,
  hubOraclePolicyId: "c1".repeat(28),
  minimumConfirmationDepth: 1,
  history: retention,
};

export const installedFamiliesWithoutTransitionTrace = () => {
  const owner = "b1".repeat(28);
  const replays = [
    ...Object.values(completeReplay),
    completeReplay.createValidationTraceDisputeCompleteCanonicalReplay(
      NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    ),
    completeReplay.createFabricatedDepositCompleteCanonicalReplay({
      authority: createFabricatedDepositEvidenceAuthority(authority),
      owner,
    }),
    completeReplay.createFabricatedWithdrawalCompleteCanonicalReplay({
      authority: createFabricatedWithdrawalEvidenceAuthority(authority),
      owner,
    }),
  ];
  const byCategory = new Map<string, completeReplay.CompleteCanonicalReplay>();
  for (const value of replays)
    if (
      typeof value === "object" &&
      value !== null &&
      "replayVersion" in value &&
      value.replayVersion === completeReplay.COMPLETE_CANONICAL_REPLAY &&
      (value as completeReplay.CompleteCanonicalReplay).launchScope.length === 1
    ) {
      const replay = value as completeReplay.CompleteCanonicalReplay;
      byCategory.set(replay.launchScope[0]!, replay);
    }
  return INSTALLED_FRAUD_PROOF_CATEGORY_ORDER.filter(
    (category) => category !== "transitionTrace",
  ).map((category) => {
    const member = byCategory.get(category);
    if (member === undefined)
      throw new Error(`no complete replay installs ${category}`);
    return { category, member };
  });
};
