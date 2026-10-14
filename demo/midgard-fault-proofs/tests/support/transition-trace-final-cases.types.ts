export type TransitionTraceFinalCase = Readonly<{
  assetCount: number;
  outputCount: number;
  outputBytes?: number;
  fieldBytes?: number;
  honest?: boolean;
  depth?: number;
  kind?: string;
  cancelAt?: number;
  corruptAssetIndex?: boolean;
  /** Forge the restored stack entries of the output datum walk. */
  corruptWalk?: boolean;
  corruptSourceReference?: boolean;
  datumBytes?: number;
  /** An output datum's CBOR, in place of a `datumBytes` byte string. */
  datumCbor?: string;
  drained?: boolean;
  /** Ledger entries the transaction spends besides any drained entry. */
  spentCount?: number;
  /** Unrelated ledger entries that deepen every MPF proof. */
  ledgerFill?: number;
}>;
