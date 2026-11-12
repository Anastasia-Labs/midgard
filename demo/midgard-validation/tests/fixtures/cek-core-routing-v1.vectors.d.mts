/**
 * Types for `cek-core-routing-v1.vectors.mjs` (plain ESM so the golden
 * generator loads it on bare `node`).
 */

export type CekCoreRoutedRow = {
  readonly mode: number;
  readonly arm: number;
  readonly tag: number;
  readonly group: number;
};

export type CekCoreRefusedRow = {
  readonly mode: number;
  readonly arm: number;
  readonly tag: number;
};

export type CekSemanticRoutedRow = {
  readonly tag: number;
  readonly group: number;
};

export type CekFailureRoutedRow = {
  readonly tag: number;
  readonly inputWords: number | null;
  readonly route: "material" | "staged" | "loop";
};

export type CekCoreRoutingRows = {
  readonly coreGroup: {
    readonly routed: readonly CekCoreRoutedRow[];
    readonly refused: readonly CekCoreRefusedRow[];
  };
  readonly semanticGroup: {
    readonly routed: readonly CekSemanticRoutedRow[];
    readonly refused: readonly { readonly tag: number }[];
  };
  readonly failureRoute: {
    readonly routed: readonly CekFailureRoutedRow[];
    readonly refused: readonly { readonly tag: number }[];
  };
};

export declare const routingRows: (twins: {
  readonly coreGroup: (
    mode: bigint,
    arm: number,
    builtinTag?: bigint,
  ) => number | undefined;
  readonly semanticGroup: (tag: bigint) => number | null;
  readonly failureRoute: (
    tag: bigint,
    inputWords: bigint | null,
  ) => CekFailureRoutedRow["route"] | undefined;
}) => CekCoreRoutingRows;
