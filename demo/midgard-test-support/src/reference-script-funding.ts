/**
 * Funding inputs for fixtures that build a reference-script publication by
 * hand: the plain outputs (ADA only, no datum, no reference script), largest
 * first and then by out-ref, until they cover `targetLovelace`; none when
 * they cannot. The node selects its own production funding; this exists only
 * so test fixtures need not each spell the same selection.
 */
export type FundingCandidate = {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly assets: Readonly<Record<string, bigint>>;
  readonly datum?: string | null;
  readonly datumHash?: string | null;
  readonly scriptRef?: unknown;
};

const lovelaceOf = (utxo: FundingCandidate): bigint =>
  utxo.assets["lovelace"] ?? 0n;

const isPlain = (utxo: FundingCandidate): boolean =>
  utxo.datum === undefined &&
  utxo.datumHash === undefined &&
  utxo.scriptRef === undefined &&
  lovelaceOf(utxo) > 0n &&
  Object.entries(utxo.assets).every(
    ([unit, amount]) => unit === "lovelace" || amount <= 0n,
  );

const compareFunding = (
  left: FundingCandidate,
  right: FundingCandidate,
): number => {
  const leftLovelace = lovelaceOf(left);
  const rightLovelace = lovelaceOf(right);
  if (leftLovelace !== rightLovelace)
    return leftLovelace > rightLovelace ? -1 : 1;
  if (left.txHash !== right.txHash) return left.txHash < right.txHash ? -1 : 1;
  return left.outputIndex - right.outputIndex;
};

export const selectPlainFundingUtxos = <U extends FundingCandidate>(
  utxos: readonly U[],
  targetLovelace: bigint,
): readonly U[] => {
  if (targetLovelace <= 0n) return [];
  const selected: U[] = [];
  let covered = 0n;
  for (const utxo of utxos.filter(isPlain).sort(compareFunding)) {
    selected.push(utxo);
    covered += lovelaceOf(utxo);
    if (covered >= targetLovelace) return selected;
  }
  return [];
};
