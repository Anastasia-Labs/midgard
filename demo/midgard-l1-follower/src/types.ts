/**
 * Public value types of the L1 follower (plan §4.3, §5.2).
 *
 * Byte fields are `Buffer`s holding the exact on-chain bytes. Slots, heights
 * and indexes are JavaScript numbers (safe for every Cardano slot); token
 * quantities are `bigint`s.
 */

/** A chain point: a block's slot and header hash. */
export type Point = Readonly<{ slot: number; hash: Buffer }>;

/** A stored block (an `l1_blocks` row). */
export type StoredBlock = Readonly<{
  slot: number;
  hash: Buffer;
  height: number;
  parentHash: Buffer | null;
  qualifyingTxCount: number;
}>;

/** A transaction output reference. Encoded as 34 bytes: hash || u16 BE index. */
export type OutRef = Readonly<{ txHash: Buffer; index: number }>;

/** `{policyIdHex: {assetNameHex: quantity}}`; lovelace is never in here. */
export type Assets = ReadonlyMap<string, ReadonlyMap<string, bigint>>;

export type ScriptType = "native" | "plutus_v1" | "plutus_v2" | "plutus_v3";

export type ScriptRef = Readonly<{
  hash: Buffer;
  type: ScriptType;
  /** Native: the script's CBOR. Plutus: the script bytes the ledger hashes. */
  bytes: Buffer;
}>;

export type Credential = Readonly<{ hash: Buffer; isScript: boolean }>;

export type OutputSummary = Readonly<{
  address: Buffer;
  paymentCredential: Credential | null;
  stakeCredential: Buffer | null;
  lovelace: bigint;
  assets: Assets;
  datumHash: Buffer | null;
  /** Inline datum: the exact Plutus data CBOR. */
  datum: Buffer | null;
  scriptRef: ScriptRef | null;
}>;

export type RedeemerPurpose =
  | "spend"
  | "mint"
  | "cert"
  | "reward"
  | "voting"
  | "proposing";

export type RedeemerSummary = Readonly<{
  purpose: RedeemerPurpose;
  index: number;
  /** The exact redeemer data CBOR. */
  data: Buffer;
}>;

export type WithdrawalSummary = Readonly<{
  rewardAccount: Buffer;
  amount: bigint;
}>;

/** One transaction of a decoded block, valid or phase-2-failed. */
export type TxSummary = Readonly<{
  hash: Buffer;
  /** Position in the block. */
  index: number;
  isValid: boolean;
  /** Exact bytes from the block; blake2b-256(bodyCbor) = hash. */
  bodyCbor: Buffer;
  witnessCbor: Buffer;
  auxCbor: Buffer | null;
  /** Ledger-sorted (tx hash, then index). */
  inputs: readonly OutRef[];
  referenceInputs: readonly OutRef[];
  collaterals: readonly OutRef[];
  outputs: readonly OutputSummary[];
  /** Created at index `outputs.length` when the tx fails phase 2. */
  collateralReturn: OutputSummary | null;
  /** Signed quantities: negative means burn. */
  mint: Assets;
  withdrawals: readonly WithdrawalSummary[];
  redeemers: readonly RedeemerSummary[];
  invalidBefore: number | null;
  invalidAfter: number | null;
}>;

/** The decoded form of one block: what the sequential writer applies. */
export type BlockSummary = Readonly<{
  point: Point;
  height: number;
  parentHash: Buffer | null;
  txs: readonly TxSummary[];
}>;

/**
 * The static part of the tracked set (§5.2), derived per role from the
 * finalized manifest. Keys are lowercase hex.
 */
export type TrackedSet = Readonly<{
  addresses: ReadonlySet<string>;
  paymentCredentials: ReadonlySet<string>;
  policies: ReadonlySet<string>;
}>;

/** The `l1_follower_cursor` row. */
export type Cursor = Readonly<{
  point: Point;
  height: number;
  generation: number;
  origin: Point;
  /**
   * Pruning may have removed spent outputs, closed temporal rows and blocks
   * at or below this slot; facts are complete at every slot at or above it.
   * Reads at a point and rewind targets must not go below it.
   */
  prunedThroughSlot: number;
}>;

/** A view V = (g, P_b) (§8.1). */
export type View = Readonly<{
  generation: number;
  point: Point;
  height: number;
}>;

/** A tracked output as stored, live or spent. */
export type StoredOutput = Readonly<{
  outRef: OutRef;
  output: OutputSummary;
  /** Null only for seed rows. */
  created: Readonly<{ slot: number; txIndex: number }> | null;
  seedSlot: number | null;
  spent: Readonly<{ slot: number; txHash: Buffer }> | null;
}>;

export type StoredTx = Readonly<{
  hash: Buffer;
  blockSlot: number;
  blockTxIndex: number;
  isValid: boolean;
  inputs: readonly OutRef[];
  referenceInputs: readonly OutRef[];
  collaterals: readonly OutRef[];
  outputCount: number;
  hasCollateralReturn: boolean;
  mint: Assets;
  withdrawals: readonly WithdrawalSummary[];
  redeemers: readonly RedeemerSummary[];
  invalidBefore: number | null;
  invalidAfter: number | null;
  bodyCbor: Buffer;
  witnessCbor: Buffer;
  auxCbor: Buffer | null;
}>;

/** Intervention reasons this package can raise (§7.5). */
export type InterventionReason =
  | "rollback_beyond_k"
  | "intersection_outside_history"
  | "store_integrity";

export type Intervention = Readonly<{
  kind: "intervention";
  reason: InterventionReason;
  detail: string;
}>;
