/**
 * Where a reservation's wallet inputs stand on L1, from the follower's
 * facts. Only these facts let the watcher reclaim a reservation whose
 * recorded decision is missing: a spent input means its transaction or
 * another one landed, and an input unspent at a final view may return to
 * the wallet.
 */
export type WatcherFundingInputStanding = Readonly<{
  /** Spent at or below the release-final point. */
  spent: readonly string[];
  /** Unspent at the release-final point and still unspent at the tip. */
  unspent: readonly string[];
  /** Why some input's standing is not final yet, or null when every input's is. */
  undetermined: string | null;
}>;

export type WatcherFundingInputFacts = Readonly<{
  standing(outRefs: readonly string[]): Promise<WatcherFundingInputStanding>;
}>;
