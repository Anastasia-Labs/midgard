export type RecoveryObservation =
  | { readonly kind: "submitted" }
  | { readonly kind: "included"; readonly inclusionHash: string }
  | { readonly kind: "confirmed"; readonly inclusionHash: string }
  | {
      readonly kind: "recovery-final";
      readonly inclusionHash: string;
      readonly authenticated: true;
    }
  | { readonly kind: "orphaned" }
  | { readonly kind: "contradictory" };

/** Reference state for fixtures; finality authentication remains the adapter's job. */
export class RecoveryScenario {
  private generation = 0;
  private inclusionHash: string | undefined;
  private observation: RecoveryObservation = { kind: "submitted" };
  private resourcesHeld = true;

  plan(): {
    readonly generation: number;
    readonly observation: RecoveryObservation;
  } {
    return { generation: this.generation, observation: this.observation };
  }

  observe(observation: RecoveryObservation): void {
    if ("inclusionHash" in observation) {
      if (
        this.inclusionHash &&
        this.inclusionHash !== observation.inclusionHash
      ) {
        this.observation = { kind: "contradictory" };
        this.resourcesHeld = true;
        this.generation += 1;
        return;
      }
      this.inclusionHash = observation.inclusionHash;
    } else if (observation.kind === "orphaned") this.inclusionHash = undefined;
    this.observation = observation;
    this.resourcesHeld = true;
    this.generation += 1;
  }

  release(plan: ReturnType<RecoveryScenario["plan"]>): void {
    if (plan.generation !== this.generation)
      throw new Error("stale recovery plan generation");
    if (
      this.observation.kind !== "recovery-final" ||
      this.observation.authenticated !== true
    )
      throw new Error(
        "funding release needs authenticated recovery-final evidence",
      );
    this.resourcesHeld = false;
  }

  snapshot(): {
    readonly generation: number;
    readonly observation: RecoveryObservation;
    readonly resourcesHeld: boolean;
  } {
    return {
      generation: this.generation,
      observation: this.observation,
      resourcesHeld: this.resourcesHeld,
    };
  }
}

export const REQUIRED_RECOVERY_CASES = [
  {
    direction: "acceptance",
    descendants: 0,
    operators: "same",
    restart: "prepared",
    rollback: "shallow",
  },
  {
    direction: "rejection",
    descendants: 1,
    operators: "same",
    restart: "signed",
    rollback: "deep",
  },
  {
    direction: "acceptance",
    descendants: 2,
    operators: "distinct",
    restart: "submitted",
    rollback: "deep",
  },
  {
    direction: "rejection",
    descendants: 2,
    operators: "distinct",
    restart: "removal",
    rollback: "shallow",
  },
  {
    direction: "acceptance",
    descendants: 1,
    operators: "distinct",
    restart: "completed",
    rollback: "shallow",
  },
  {
    direction: "rejection",
    descendants: 0,
    operators: "same",
    restart: "completed",
    rollback: "deep",
  },
] as const;
