export type FixtureChain = {
  /** The node accepts `txHash` into its mempool. */
  readonly accept: (txHash: string) => void;
  readonly land: (txHash: string) => void;
  /** `txHash` is in neither the ledger nor the mempool: it was dropped or
   * rolled back. */
  readonly drop: (txHash: string) => void;
  readonly landed: (txHash: string) => boolean;
  /** Whether the next state shows `txHash`: landed, or in the mempool. */
  readonly seen: (txHash: string) => boolean;
};

export const createFixtureChain = (): FixtureChain => {
  const ledger = new Set<string>();
  const mempool = new Set<string>();
  const landed = (txHash: string) => ledger.has(txHash);
  return {
    accept: (txHash) => {
      if (!landed(txHash)) mempool.add(txHash);
    },
    land: (txHash) => {
      mempool.delete(txHash);
      ledger.add(txHash);
    },
    drop: (txHash) => {
      mempool.delete(txHash);
      ledger.delete(txHash);
    },
    landed,
    seen: (txHash) => landed(txHash) || mempool.has(txHash),
  };
};
