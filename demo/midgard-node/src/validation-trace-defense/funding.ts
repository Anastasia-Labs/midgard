/**
 * How the defending operator funds its transactions (pure), from the
 * wallet views of its operator wallet (Base) and its defense pocket
 * (Enterprise), plain-ADA outputs only:
 *
 * - a Defend spends from both, keeping one pocket output that can stand as
 *   a thread move's collateral; when the pocket has none, or the Defend
 *   needs the one it has, the Defend pays one into it. It needs the thread's floor, that collateral when paid,
 *   and a margin for its fee, collateral and change;
 * - a thread move (a reveal, an expiry) takes its collateral, and any fee
 *   the thread does not pay, from the pocket alone, so a commit, which
 *   selects from the Base wallet, never takes it.
 */
import {
  isPlainFundingOutput,
  VALIDATION_TRACE_POCKET_COLLATERAL_LOVELACE,
  VALIDATION_TRACE_THREAD_FLOOR_LOVELACE,
} from "@al-ft/midgard-fault-proofs";
import type { UTxO } from "@lucid-evolution/lucid";

/** What a Defend keeps above the floor for its fee, collateral and change. */
export const VALIDATION_TRACE_DEFEND_MARGIN_LOVELACE = 10_000_000n;

export type DefendFunding =
  | Readonly<{
      kind: "funded";
      /** The inputs the Defend may spend. */
      inputs: readonly UTxO[];
      /** Whether the Defend pays the pocket a collateral output. */
      payPocket: boolean;
    }>
  | Readonly<{ kind: "short"; shortfall: bigint }>;

const lovelaceOf = (utxo: UTxO): bigint => utxo.assets.lovelace ?? 0n;

const total = (utxos: readonly UTxO[]): bigint =>
  utxos.reduce((sum, utxo) => sum + lovelaceOf(utxo), 0n);

/** The pocket's outputs that can stand as a move's collateral, smallest first. */
const collateralCandidates = (pocket: readonly UTxO[]): UTxO[] =>
  pocket
    .filter(
      (utxo) =>
        isPlainFundingOutput(utxo) &&
        lovelaceOf(utxo) >= VALIDATION_TRACE_POCKET_COLLATERAL_LOVELACE,
    )
    .sort((a, b) => (lovelaceOf(a) < lovelaceOf(b) ? -1 : 1));

const sameOutRef = (a: UTxO, b: UTxO) =>
  a.txHash === b.txHash && a.outputIndex === b.outputIndex;

/** The Defend's funding that keeps `kept` in the pocket, or pays the
 * pocket a collateral output when nothing is kept. */
const fundKeeping = (
  base: readonly UTxO[],
  pocket: readonly UTxO[],
  kept: UTxO | undefined,
): DefendFunding => {
  const inputs = [...base, ...pocket].filter(
    (utxo) =>
      isPlainFundingOutput(utxo) &&
      (kept === undefined || !sameOutRef(utxo, kept)),
  );
  const payPocket = kept === undefined;
  const needed =
    VALIDATION_TRACE_THREAD_FLOOR_LOVELACE +
    VALIDATION_TRACE_DEFEND_MARGIN_LOVELACE +
    (payPocket ? VALIDATION_TRACE_POCKET_COLLATERAL_LOVELACE : 0n);
  const available = total(inputs);
  return available >= needed && inputs.length > 0
    ? { kind: "funded", inputs, payPocket }
    : { kind: "short", shortfall: needed - available };
};

/**
 * The Defend's funding from the `base` and `pocket` views: it keeps the
 * pocket's smallest collateral output when the rest covers it; otherwise it
 * spends that output too and pays the pocket a fresh one.
 */
export const planDefendFunding = (
  base: readonly UTxO[],
  pocket: readonly UTxO[],
): DefendFunding => {
  const kept = collateralCandidates(pocket)[0];
  const keeping = fundKeeping(base, pocket, kept);
  if (keeping.kind === "funded" || kept === undefined) return keeping;
  const spending = fundKeeping(base, pocket, undefined);
  return spending.kind === "funded" || spending.shortfall < keeping.shortfall
    ? spending
    : keeping;
};

/**
 * The pocket's plain outputs, a thread move's own wallet, when one can
 * stand as its collateral; otherwise what the pocket lacks.
 */
export const planMoveFunding = (
  pocket: readonly UTxO[],
):
  | Readonly<{ kind: "funded"; inputs: readonly UTxO[] }>
  | Readonly<{ kind: "short"; shortfall: bigint }> =>
  collateralCandidates(pocket).length > 0
    ? { kind: "funded", inputs: pocket.filter(isPlainFundingOutput) }
    : { kind: "short", shortfall: VALIDATION_TRACE_POCKET_COLLATERAL_LOVELACE };
