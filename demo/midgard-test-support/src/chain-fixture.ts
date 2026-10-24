import { createHash } from "node:crypto";

export interface SyntheticChainPoint {
  readonly synthetic: true;
  readonly hash: string;
  readonly parentHash: string | undefined;
  readonly height: number;
  readonly slot: number;
}

/** Heights, slots, ancestry and provider observations are independent inputs. */
export class ChainFixture {
  private readonly points = new Map<string, SyntheticChainPoint>();
  private readonly observations = new Map<string, string>();
  private sequence = 0;

  append(input: {
    parent?: SyntheticChainPoint;
    slot: number;
    label?: string;
  }): SyntheticChainPoint {
    if (
      !Number.isSafeInteger(input.slot) ||
      input.slot < 0 ||
      (input.parent && input.slot <= input.parent.slot)
    )
      throw new Error("fixture slot must increase along an edge");
    if (input.parent && this.points.get(input.parent.hash) !== input.parent)
      throw new Error("fixture parent belongs to another chain");
    const hash = createHash("sha256")
      .update(
        JSON.stringify([
          "synthetic-midgard-chain",
          this.sequence++,
          input.parent?.hash,
          input.slot,
          input.label,
        ]),
      )
      .digest("hex");
    const point: SyntheticChainPoint = Object.freeze({
      synthetic: true,
      hash,
      parentHash: input.parent?.hash,
      height: input.parent ? input.parent.height + 1 : 0,
      slot: input.slot,
    });
    this.points.set(hash, point);
    return point;
  }

  observe(provider: string, point: SyntheticChainPoint): void {
    if (this.points.get(point.hash) !== point)
      throw new Error("unknown fixture observation");
    this.observations.set(provider, point.hash);
  }

  rollback(provider: string, point: SyntheticChainPoint): void {
    const previous = this.observations.get(provider);
    if (!previous || this.distance(point.hash, previous) === undefined)
      throw new Error(
        "rollback must select an ancestor of that provider's tip",
      );
    this.observe(provider, point);
  }

  distance(ancestor: string, descendant: string): number | undefined {
    let current = this.points.get(descendant);
    let distance = 0;
    while (current) {
      if (current.hash === ancestor) return distance;
      current = current.parentHash
        ? this.points.get(current.parentHash)
        : undefined;
      distance += 1;
    }
    return undefined;
  }

  evidence(
    inclusion: SyntheticChainPoint,
    provider: string,
  ): { confirmations: number; recoveryDistance: number } | undefined {
    const observed = this.observations.get(provider);
    const distance = observed
      ? this.distance(inclusion.hash, observed)
      : undefined;
    return distance === undefined
      ? undefined
      : { confirmations: distance + 1, recoveryDistance: distance };
  }
}
