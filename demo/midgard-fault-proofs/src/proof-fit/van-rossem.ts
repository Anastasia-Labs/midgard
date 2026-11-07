/**
 * Van Rossem (protocol version 11) limits on one complete signed L1
 * transaction, and the tighter byte target a data publication must meet.
 *
 * A publication carries data or a reference script and runs no script, so it
 * has zero execution units. It keeps a 512-byte reserve below the transaction
 * maximum so a later witness or fee change cannot push a sized-to-fit
 * publication over the L1 limit.
 */
export const VAN_ROSSEM_MAX_SIGNED_TX_BYTES = 16_384;
export const VAN_ROSSEM_PUBLICATION_RESERVE_BYTES = 512;
export const VAN_ROSSEM_PUBLICATION_TARGET_BYTES =
  VAN_ROSSEM_MAX_SIGNED_TX_BYTES - VAN_ROSSEM_PUBLICATION_RESERVE_BYTES;
export const VAN_ROSSEM_MAX_MEMORY_UNITS = 16_500_000n;
export const VAN_ROSSEM_MAX_CPU_UNITS = 10_000_000_000n;

/** What one complete signed transaction costs on L1. */
export type VanRossemCost = {
  readonly signedBytes: number;
  readonly memoryUnits: bigint;
  readonly cpuUnits: bigint;
};

export type VanRossemAxis =
  | "signed bytes"
  | "memory units"
  | "cpu units"
  | "publication target bytes";

/** Thrown by {@link assertFitsVanRossem}; `margin` is negative. */
export class VanRossemFitError extends Error {
  readonly axis: VanRossemAxis;
  readonly margin: bigint;

  constructor(label: string, axis: VanRossemAxis, margin: bigint) {
    const limit =
      axis === "signed bytes"
        ? `the ${VAN_ROSSEM_MAX_SIGNED_TX_BYTES.toString()}-byte Van Rossem transaction maximum`
        : axis === "memory units"
          ? `the ${VAN_ROSSEM_MAX_MEMORY_UNITS.toString()} Van Rossem memory-unit maximum`
          : axis === "cpu units"
            ? `the ${VAN_ROSSEM_MAX_CPU_UNITS.toString()} Van Rossem cpu-unit maximum`
            : `the ${VAN_ROSSEM_PUBLICATION_TARGET_BYTES.toString()}-byte publication target (a transaction with zero execution units is a publication)`;
    super(`${label} exceeds ${limit}: ${axis} margin ${margin.toString()}`);
    this.name = "VanRossemFitError";
    this.axis = axis;
    this.margin = margin;
  }
}

/** A transaction that runs no script is a publication. */
export const isVanRossemPublication = (cost: VanRossemCost): boolean =>
  cost.memoryUnits === 0n && cost.cpuUnits === 0n;

/**
 * Refuses a signed transaction over any Van Rossem maximum, and a publication
 * over the publication target, naming the axis and its (negative) margin.
 */
export const assertFitsVanRossem = (
  cost: VanRossemCost,
  label = "signed transaction",
): void => {
  if (
    !Number.isSafeInteger(cost.signedBytes) ||
    cost.signedBytes < 0 ||
    cost.memoryUnits < 0n ||
    cost.cpuUnits < 0n
  ) {
    throw new Error(`${label} has a negative or unsafe cost`);
  }
  const bytes = BigInt(cost.signedBytes);
  const margins: readonly (readonly [VanRossemAxis, bigint])[] = [
    ["signed bytes", BigInt(VAN_ROSSEM_MAX_SIGNED_TX_BYTES) - bytes],
    ["memory units", VAN_ROSSEM_MAX_MEMORY_UNITS - cost.memoryUnits],
    ["cpu units", VAN_ROSSEM_MAX_CPU_UNITS - cost.cpuUnits],
    ...(isVanRossemPublication(cost)
      ? [
          [
            "publication target bytes",
            BigInt(VAN_ROSSEM_PUBLICATION_TARGET_BYTES) - bytes,
          ] as const,
        ]
      : []),
  ];
  for (const [axis, margin] of margins) {
    if (margin < 0n) throw new VanRossemFitError(label, axis, margin);
  }
};
