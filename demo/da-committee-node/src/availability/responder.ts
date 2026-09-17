import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import type { WatcherStore } from "../store.js";
import { retainedAvailabilityPayload } from "./retained-payload.js";

export type AvailabilityResponderChallenge = Readonly<{
  bond: {
    readonly utxo: UTxO;
    readonly datum: Extract<
      SDK.DaAvailabilityBondDatum,
      { ChallengedBond: unknown }
    >;
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
    | "failed";
  detail?: string;
}>;

export type AvailabilityResponderDeps = Readonly<{
  deploymentFingerprint: string;
  deploymentIdentity: string;
  store: Pick<WatcherStore, "getDaPayload">;
  /** The concrete adapter authenticates policy units and all linked datums. */
  discover: () => Promise<readonly AvailabilityResponderChallenge[]>;
  /** Called before discovery so an ambiguous submission never creates new work. */
  reconcile: () => Promise<"ready" | "pending">;
  execute: (
    action: AvailabilityResponderAction,
  ) => Promise<"confirmed" | "included" | "pending">;
  now?: () => number;
}>;

/** One mutation per tick; each subsequent tick derives progress from live L1 state. */
export class AvailabilityResponder {
  constructor(private readonly deps: AvailabilityResponderDeps) {}

  async tick(): Promise<AvailabilityResponderReport> {
    if ((await this.deps.reconcile()) === "pending") {
      return { challenges: 0, status: "pending" };
    }
    const challenges = await this.deps.discover();
    let deferred: AvailabilityResponderReport | undefined;
    for (const challenge of challenges) {
      const bond = challenge.bond.datum.ChallengedBond;
      const base = {
        challenges: challenges.length,
        headerHash: bond.commitment.header_hash,
      };
      let executionStarted = false;
      try {
        const action = await this.nextAction(challenge);
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
        return { ...base, action: action.kind, status };
      } catch (error) {
        const failure = {
          ...base,
          status: "failed" as const,
          detail: error instanceof Error ? error.message : String(error),
        };
        if (executionStarted) return failure;
        deferred ??= failure;
      }
    }
    return deferred ?? { challenges: challenges.length, status: "idle" };
  }

  private async nextAction(
    challenge: AvailabilityResponderChallenge,
  ): Promise<AvailabilityResponderAction | undefined> {
    const bond = challenge.bond.datum.ChallengedBond;
    const terminal = challenge.terminal.datum;
    if (
      terminal.next_tranche_index ===
      BigInt(bond.commitment.tranche_descriptors.length)
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
    if (now >= bond.response_deadline) return undefined;
    const payload = await retainedAvailabilityPayload({
      store: this.deps.store,
      deploymentFingerprint: this.deps.deploymentFingerprint,
      deploymentIdentity: this.deps.deploymentIdentity,
      commitment: bond.commitment,
    });
    if (payload === undefined) return undefined;
    const plans = SDK.planDaAvailabilityPublications({
      commitment: bond.commitment,
      challengeAssetName: bond.challenge_asset_name,
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
