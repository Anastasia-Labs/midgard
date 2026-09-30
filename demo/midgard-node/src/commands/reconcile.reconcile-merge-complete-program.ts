import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Effect } from "effect";

import {
  MutationJobsDB,
  StateQueueMutationLeasesDB,
} from "../database/index.js";
import { mergeAction } from "../fibers/merge.js";
import {
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import {
  evidence,
  type ReconciliationResult,
  result,
} from "./reconcile.parse-reconciliation-result.js";
import {
  mergeCompletionEvidence,
  mergeCompletionVerdict,
  mergeResultEvidence,
  observeMergeCompletion,
} from "./reconcile.reconcile-local-finalization-program.js";

export const reconcileMergeCompleteProgram = ({
  headerHash,
  repair,
}: {
  readonly headerHash: Buffer;
  readonly repair: boolean;
}): Effect.Effect<
  ReconciliationResult,
  unknown,
  Database | Lucid | MidgardContracts | Globals | NodeConfig
> =>
  Effect.gen(function* () {
    const headerHashHex = headerHash.toString("hex");
    const target = { headerHash: headerHashHex };
    const jobId = MutationJobsDB.confirmedMergeFinalizationJobId(headerHashHex);
    const observed = yield* observeMergeCompletion(headerHash, jobId);
    const repairActions: string[] = [];
    if (repair && observed.canonical) {
      // The state queue merges strictly oldest-first, so only the oldest queued
      // block can be the target of a merge.
      const oldest = observed.canonicalHeaders[0];
      if (oldest !== headerHashHex) {
        return result({
          milestone: "merge-complete",
          target,
          status: "blocked",
          evidence: mergeCompletionEvidence(jobId, observed),
          repairActions,
          nextAction: `Only the oldest queued block can be merged; reconcile merge-complete for ${String(oldest)} first.`,
        });
      }
      const attempt = yield* mergeAction(true, {
        expectedHeaderHash: headerHashHex,
      }).pipe(
        Effect.map((mergeResult) => ({ _tag: "Ran" as const, mergeResult })),
        Effect.catchTag("MergeProducerPermitUnavailable", (unavailable) =>
          Effect.succeed({ _tag: "PermitUnavailable" as const, unavailable }),
        ),
      );
      if (attempt._tag === "PermitUnavailable") {
        // Nothing ran: no L1 work and no local write happened.
        return result({
          milestone: "merge-complete",
          target,
          status: "blocked",
          evidence: [
            evidence("merge_producer_permit", {
              available: false,
              reason: `${attempt.unavailable.message}: ${formatUnknownError(
                attempt.unavailable.cause,
                {
                  includeCause: true,
                },
              )}`,
            }),
            ...mergeCompletionEvidence(jobId, observed),
          ],
          repairActions,
          nextAction:
            "A merge finalizes history rows and needs the running node's history producer permit. A standalone process cannot hold it; on the running node it is unavailable until the history owner is Ready. Trigger the merge through the node's admin GET /merge endpoint, which merges the oldest queued block, once its history owner is Ready.",
        });
      }
      const { mergeResult } = attempt;
      repairActions.push("merge_action");
      const after = yield* observeMergeCompletion(headerHash, jobId);
      const verdict = mergeCompletionVerdict(after);
      return result({
        milestone: "merge-complete",
        target,
        status:
          verdict.status === "satisfied"
            ? "repaired"
            : after.canonical
              ? "pending"
              : "failed",
        evidence: [
          mergeResultEvidence(mergeResult),
          ...mergeCompletionEvidence(jobId, after),
        ],
        repairActions,
        nextAction:
          verdict.status === "satisfied"
            ? null
            : after.canonical
              ? "Merge did not remove the header yet; inspect merge_result and the state-queue lease."
              : verdict.nextAction,
      });
    }

    const verdict = mergeCompletionVerdict(observed);
    return result({
      milestone: "merge-complete",
      target,
      status: verdict.status,
      evidence: [
        ...mergeCompletionEvidence(jobId, observed),
        evidence(
          "state_queue_lease",
          StateQueueMutationLeasesDB.encodeInspectionJson(
            yield* StateQueueMutationLeasesDB.inspect(),
          ),
        ),
      ],
      repairActions,
      nextAction: verdict.nextAction,
    });
  });
