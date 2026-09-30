import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { MutationJobsDB } from "../database/index.js";
import { loadPhasMembershipWithdrawalScript } from "../phas-membership.js";
import { Lucid, MidgardContracts } from "../services/index.js";
import {
  ensurePhasMembershipRewardAccountRegisteredProgram,
  queryPhasMembershipRewardAccountRegisteredProgram,
} from "../transactions/phas-membership-registration.js";
import {
  evidence,
  type ReconciliationEvidence,
  type ReconciliationResult,
  result,
} from "./reconcile.parse-reconciliation-result.js";

type CanonicalStateQueueHeader = {
  readonly headerHash: string;
  readonly outRef: string;
  readonly utxo: SDK.StateQueueUTxO;
};

const stateQueueOutRef = (utxo: SDK.StateQueueUTxO): string =>
  `${utxo.utxo.txHash}#${utxo.utxo.outputIndex.toString()}`;

export const fetchCanonicalStateQueueHeaders = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const sorted = yield* SDK.fetchSortedStateQueueUTxOsProgram(lucid.api, {
    stateQueuePolicyId: contracts.stateQueue.policyId,
    stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
  });
  return sorted.flatMap((utxo): CanonicalStateQueueHeader[] =>
    utxo.datum.key === "Empty"
      ? []
      : [
          {
            headerHash: utxo.datum.key.Key.key,
            outRef: stateQueueOutRef(utxo),
            utxo,
          },
        ],
  );
});

export const fetchCanonicalStateQueueHeaderHashes =
  fetchCanonicalStateQueueHeaders.pipe(
    Effect.map((headers) => headers.map((header) => header.headerHash)),
  );

export const localFinalizationJobId = (headerHashHex: string): string =>
  `local_block_finalization:${headerHashHex}`;

export const unfinishedLocalFinalizationJobEvidence = (
  headerHashHex: string,
  jobs: readonly MutationJobsDB.Entry[],
): ReconciliationEvidence => {
  const jobId = localFinalizationJobId(headerHashHex);
  const job = jobs.find(
    (entry) => entry[MutationJobsDB.Columns.JOB_ID] === jobId,
  );
  return evidence(
    "local_finalization_job",
    job === undefined
      ? { present: false, jobId }
      : {
          present: true,
          jobId,
          kind: job[MutationJobsDB.Columns.KIND],
          status: job[MutationJobsDB.Columns.STATUS],
          attempts: job[MutationJobsDB.Columns.ATTEMPTS],
          lastError: job[MutationJobsDB.Columns.LAST_ERROR],
          updatedAt: job[MutationJobsDB.Columns.UPDATED_AT].toISOString(),
        },
  );
};

export const reconcilePhasRegisteredProgram = ({
  repair,
}: {
  readonly repair: boolean;
}): Effect.Effect<ReconciliationResult, unknown, Lucid> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const identity = yield* Effect.try({
      try: () => {
        const network = lucid.api.config().network;
        if (network === undefined) {
          throw new Error("Lucid network is undefined");
        }
        return SDK.phasMembershipIdentity(
          network,
          loadPhasMembershipWithdrawalScript(),
        );
      },
      catch: (cause) =>
        new SDK.UnspecifiedNetworkError({
          message: "Failed to derive PHAS membership identity",
          cause,
        }),
    });

    const registeredAttempt = yield* Effect.either(
      queryPhasMembershipRewardAccountRegisteredProgram(
        lucid.api,
        identity.rewardAddress,
      ),
    );
    if (registeredAttempt._tag === "Left") {
      if (!repair) {
        return result({
          milestone: "phas-registered",
          target: {
            rewardAddress: identity.rewardAddress,
            scriptHash: identity.scriptHash,
          },
          status: "blocked",
          safeToRetryOriginalStep: false,
          evidence: [
            evidence("reward_account_registration_error", {
              error: formatUnknownError(registeredAttempt.left, {
                includeCause: true,
              }),
            }),
          ],
          nextAction:
            "Fix provider reward-account registration lookup or rerun with --repair only when submission is safe.",
        });
      }
      const repairAttempt = yield* Effect.either(
        ensurePhasMembershipRewardAccountRegisteredProgram(lucid.api),
      );
      if (repairAttempt._tag === "Left") {
        return result({
          milestone: "phas-registered",
          target: {
            rewardAddress: identity.rewardAddress,
            scriptHash: identity.scriptHash,
          },
          status: "failed",
          safeToRetryOriginalStep: false,
          evidence: [
            evidence("phas_registration_error", {
              error: formatUnknownError(repairAttempt.left, {
                includeCause: true,
              }),
            }),
          ],
          repairActions: ["register_phas_membership_reward_account"],
          nextAction:
            "Registration repair failed; inspect provider and transaction error before retrying.",
        });
      }
      return result({
        milestone: "phas-registered",
        target: {
          rewardAddress: repairAttempt.right.rewardAddress,
          scriptHash: repairAttempt.right.scriptHash,
        },
        status:
          repairAttempt.right.status === "already_registered"
            ? "satisfied"
            : "repaired",
        safeToRetryOriginalStep: true,
        evidence: [evidence("phas_registration_result", repairAttempt.right)],
        repairActions:
          repairAttempt.right.status === "already_registered"
            ? []
            : ["register_phas_membership_reward_account"],
      });
    }
    const registered = registeredAttempt.right;
    if (registered) {
      return result({
        milestone: "phas-registered",
        target: {
          rewardAddress: identity.rewardAddress,
          scriptHash: identity.scriptHash,
        },
        status: "satisfied",
        safeToRetryOriginalStep: true,
        evidence: [
          evidence("reward_account_registration", { registered: true }),
        ],
      });
    }
    if (!repair) {
      return result({
        milestone: "phas-registered",
        target: {
          rewardAddress: identity.rewardAddress,
          scriptHash: identity.scriptHash,
        },
        status: "pending",
        safeToRetryOriginalStep: true,
        evidence: [
          evidence("reward_account_registration", { registered: false }),
        ],
        nextAction:
          "Run this reconciler with --repair or rerun the idempotent PHAS registration step.",
      });
    }

    const repairedAttempt = yield* Effect.either(
      ensurePhasMembershipRewardAccountRegisteredProgram(lucid.api),
    );
    if (repairedAttempt._tag === "Left") {
      return result({
        milestone: "phas-registered",
        target: {
          rewardAddress: identity.rewardAddress,
          scriptHash: identity.scriptHash,
        },
        status: "failed",
        safeToRetryOriginalStep: false,
        evidence: [
          evidence("phas_registration_error", {
            error: formatUnknownError(repairedAttempt.left, {
              includeCause: true,
            }),
          }),
        ],
        repairActions: ["register_phas_membership_reward_account"],
        nextAction:
          "Registration repair failed; inspect provider and transaction error before retrying.",
      });
    }
    const repaired = repairedAttempt.right;
    return result({
      milestone: "phas-registered",
      target: {
        rewardAddress: repaired.rewardAddress,
        scriptHash: repaired.scriptHash,
      },
      status: "repaired",
      safeToRetryOriginalStep: true,
      evidence: [evidence("phas_registration_result", repaired)],
      repairActions: ["register_phas_membership_reward_account"],
    });
  });
