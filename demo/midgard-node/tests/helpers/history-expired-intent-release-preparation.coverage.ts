/** The canonical coverage a preparation test models, and the loader result
 * its mock returns. Imports nothing from src, so a mock factory can load it. */

export type Coverage =
  | "unavailable"
  | Readonly<{
      head: number;
      start: number;
      /** Height of each valid transaction in the retained chain. */
      txs: Readonly<Record<string, number>>;
    }>;

/** The coverage the mocked loader returns for `fixture`. */
export const coverageOf = (coverage: Exclude<Coverage, "unavailable">) => {
  const heights = [...new Set(Object.values(coverage.txs))];
  return {
    head: { height: coverage.head },
    start: { height: coverage.start },
    blocks: heights.map((height) => ({
      point: { height },
      transactions: Object.entries(coverage.txs)
        .filter(([, at]) => at === height)
        .map(([txHash]) => ({ txHash, spends: "inputs" })),
    })),
  };
};
