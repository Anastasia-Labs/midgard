import { type MerkleRoot } from "../common.js";

/**
 * The counted `withdrawals_root` a header must carry for a raw withdrawals MPF
 * root and cardinality. Re-exported through the family so a builder never
 * re-derives the counted-root tag itself.
 */
export type FabricatedWithdrawalCountedRootInput = {
  readonly phasRoot: MerkleRoot;
  readonly count: bigint;
};
