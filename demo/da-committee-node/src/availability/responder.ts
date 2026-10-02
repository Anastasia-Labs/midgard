import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import type { CommitteeStore } from "../store.js";
import { retainedAvailabilityPayload } from "./retained-payload.js";

export type AvailabilityResponderChallenge = Readonly<{
  /**
   * The challenge record (`ChallengeRecordV1`) at the availability script,
   * found by its DACH token. It carries the commitment, the challenger and the
   * response deadline; the pooled DA bond is not involved in responding.
   */
  record: {
    readonly utxo: UTxO;
    readonly datum: SDK.DaAvailabilityChallengeRecord;
  };
  terminal: {
    readonly utxo: UTxO;
    readonly datum: SDK.DaAvailabilityTerminalAccumulatorDatum;
  };
  queue: UTxO;
  tranches: readonly {
    readonly utxo: UTxO;
    readonly datum: SDK.DaAvailabilityTrancheDatum;
    readonly carrier?: UTxO;
  }[];
}>;

export type AvailabilityResponderAction =
  | Readonly<{
      kind: "publish";
      challenge: AvailabilityResponderChallenge;
      tranche: AvailabilityResponderChallenge["tranches"][number];
      publication: SDK.DaAvailabilityPublicationDatum;
    }>
  | Readonly<{
      kind: "settle";
      challenge: AvailabilityResponderChallenge;
      tranche: AvailabilityResponderChallenge["tranches"][number];
    }>
  | Readonly<{ kind: "close"; challenge: AvailabilityResponderChallenge }>;

export type AvailabilityResponderReport = Readonly<{
  challenges: number;
  action?: AvailabilityResponderAction["kind"];
  headerHash?: string;
  status:
    | "idle"
    | "pending"
    | "included"
    | "confirmed"
    | "unavailable"
    | "awaiting_scan"
    | "failed";
  detail?: string;
  /**
   * Challenges this step found past their response deadline with an
   * unanswered tranche: no answer can land any more, so each is a challenge
   * this committee has lost unless the challenger never times it out.
   */
  missedDeadlines?: readonly AvailabilityResponderMissedDeadline[];
}>;

export type AvailabilityResponderMissedDeadline = Readonly<{
  headerHash: string;
  responseDeadline: string;
}>;

/** Steps one drain runs at most, so a drain always ends. */
export const AVAILABILITY_RESPONDER_MAX_DRAIN_STEPS = 32;

/**
 * The committee's chain-sync cursor is not at the aligned Kupmios tip, or the
 * tip moved while a boundary was read. Every responder step is refused until
 * the committee's next L1 scan catches the cursor up, so this aborts the step
 * like any error; but it is the normal wait for that scan, not a failure, and
 * the tick reports it as `awaiting_scan` instead of throwing it.
 */
export class AvailabilityResponderAwaitingScanError extends Error {
  constructor() {
    super(
      "Availability responder awaits the next canonical committee node L1 scan before acting",
    );
    this.name = "AvailabilityResponderAwaitingScanError";
  }
}

const awaitingScanReport = (
  error: AvailabilityResponderAwaitingScanError,
): Pick<AvailabilityResponderReport, "status" | "detail"> => ({
  status: "awaiting_scan",
  detail: error.message,
});

/**
 * The log line for one tick's report: none when idle, and stderr only for a
 * report an operator must look at. Waiting on the committee's next scan is
 * routine, so it is one compact stdout line.
 */
export const availabilityResponderReportLine = (
  report: AvailabilityResponderReport,
):
  | { readonly stream: "stdout" | "stderr"; readonly line: string }
  | undefined =>
  report.status === "idle"
    ? undefined
    : {
        stream:
          report.status === "failed" || report.status === "unavailable"
            ? "stderr"
            : "stdout",
        line: `${JSON.stringify({ event: "availability_responder", ...report })}\n`,
      };

export type AvailabilityResponderDeps = Readonly<{
  deploymentFingerprint: string;
  deploymentIdentity: string;
  store: Pick<CommitteeStore, "getDaPayload">;
  /** The concrete adapter authenticates policy units and all linked datums. */
  discover: () => Promise<readonly AvailabilityResponderChallenge[]>;
  /** Called before discovery so an ambiguous submission never creates new work. */
  reconcile: () => Promise<"ready" | "pending">;
  execute: (
    action: AvailabilityResponderAction,
  ) => Promise<"confirmed" | "included" | "pending">;
  now?: () => number;
}>;

/**
 * One mutation per step; each later step derives progress from live L1 state.
 * Challenges are taken nearest response deadline first.
 */
export class AvailabilityResponder {
  constructor(private readonly deps: AvailabilityResponderDeps) {}

  /**
   * Steps until one leaves nothing ready to act on: another step follows
   * only one whose action confirmed, since anything else (an inclusion still
   * pending, a lagging cursor, a failure) needs L1 to move first. Ends after
   * `maxSteps` steps. Resolves the last step's report, carrying every missed
   * deadline the drain saw.
   */
  async drain(
    maxSteps = AVAILABILITY_RESPONDER_MAX_DRAIN_STEPS,
  ): Promise<AvailabilityResponderReport> {
    const missed = new Map<string, AvailabilityResponderMissedDeadline>();
    let report: AvailabilityResponderReport;
    let steps = 0;
    do {
      report = await this.tick();
      steps += 1;
      for (const entry of report.missedDeadlines ?? []) {
        missed.set(entry.headerHash, entry);
      }
    } while (report.status === "confirmed" && steps < maxSteps);
    return missed.size === 0
      ? report
      : { ...report, missedDeadlines: [...missed.values()] };
  }

  /**
   * One responder step. A step refused while the committee's cursor lags the
   * L1 tip is reported as `awaiting_scan`; every other error still throws.
   */
  async tick(): Promise<AvailabilityResponderReport> {
    try {
      return await this.step();
    } catch (error) {
      if (!(error instanceof AvailabilityResponderAwaitingScanError))
        throw error;
      return { challenges: 0, ...awaitingScanReport(error) };
    }
  }

  private async step(): Promise<AvailabilityResponderReport> {
    if ((await this.deps.reconcile()) === "pending") {
      return { challenges: 0, status: "pending" };
    }
    const challenges = [...(await this.deps.discover())].sort((a, b) =>
      a.record.datum.response_deadline < b.record.datum.response_deadline
        ? -1
        : a.record.datum.response_deadline > b.record.datum.response_deadline
          ? 1
          : 0,
    );
    const missedDeadlines: AvailabilityResponderMissedDeadline[] = [];
    const withMissed = (
      report: AvailabilityResponderReport,
    ): AvailabilityResponderReport =>
      missedDeadlines.length === 0 ? report : { ...report, missedDeadlines };
    let deferred: AvailabilityResponderReport | undefined;
    for (const challenge of challenges) {
      const record = challenge.record.datum;
      const base = {
        challenges: challenges.length,
        headerHash: record.commitment.header_hash,
      };
      let executionStarted = false;
      try {
        const action = await this.nextAction(challenge);
        if (action === "deadline_passed") {
          missedDeadlines.push({
            headerHash: record.commitment.header_hash,
            responseDeadline: record.response_deadline.toString(),
          });
          deferred ??= {
            ...base,
            status: "unavailable",
            detail:
              "The response deadline has passed; no answer can land and terminal timeout remains available to the challenger",
          };
          continue;
        }
        if (action === undefined) {
          deferred ??= {
            ...base,
            status: "unavailable",
            detail:
              "No retained answer is available; terminal timeout remains available to the challenger",
          };
          continue;
        }
        executionStarted = true;
        const status = await this.deps.execute(action);
        return withMissed({ ...base, action: action.kind, status });
      } catch (error) {
        if (error instanceof AvailabilityResponderAwaitingScanError)
          return withMissed({ ...base, ...awaitingScanReport(error) });
        const failure = {
          ...base,
          status: "failed" as const,
          detail: error instanceof Error ? error.message : String(error),
        };
        if (executionStarted) return withMissed(failure);
        deferred ??= failure;
      }
    }
    return withMissed(
      deferred ?? { challenges: challenges.length, status: "idle" },
    );
  }

  private async nextAction(
    challenge: AvailabilityResponderChallenge,
  ): Promise<AvailabilityResponderAction | "deadline_passed" | undefined> {
    const record = challenge.record.datum;
    const terminal = challenge.terminal.datum;
    if (
      terminal.next_tranche_index ===
      BigInt(record.commitment.tranche_descriptors.length)
    ) {
      return terminal.has_timed_out_tranche
        ? undefined
        : { kind: "close", challenge };
    }
    const nextTranche = challenge.tranches.find(({ datum }) => {
      const fields = "Active" in datum ? datum.Active : datum.Receipt;
      return fields.descriptor.tranche_index === terminal.next_tranche_index;
    });
    if (nextTranche === undefined) {
      throw new Error(
        "Authenticated challenge is missing its next unsettled tranche",
      );
    }
    if ("Receipt" in nextTranche.datum) {
      return { kind: "settle", challenge, tranche: nextTranche };
    }
    const now = BigInt((this.deps.now ?? Date.now)());
    if (now >= record.response_deadline) return "deadline_passed";
    const payload = await retainedAvailabilityPayload({
      store: this.deps.store,
      deploymentFingerprint: this.deps.deploymentFingerprint,
      deploymentIdentity: this.deps.deploymentIdentity,
      commitment: record.commitment,
    });
    if (payload === undefined) return undefined;
    const plans = SDK.planDaAvailabilityPublications({
      commitment: record.commitment,
      challengeAssetName: record.challenge_asset_name,
      payload,
    });
    const active = nextTranche.datum.Active;
    const publication = plans
      .find(
        (plan) =>
          plan.descriptor.tranche_index === active.descriptor.tranche_index,
      )
      ?.publications.find((item) => item.chunk_offset === active.next_offset);
    if (
      publication === undefined ||
      publication.previous_accumulator !== active.accumulator
    ) {
      throw new Error(
        "Retained publication does not continue the authenticated tranche offset and accumulator",
      );
    }
    return { kind: "publish", challenge, tranche: nextTranche, publication };
  }
}
