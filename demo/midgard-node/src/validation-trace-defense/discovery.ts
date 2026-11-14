/**
 * What the follower's facts say about the validation-trace cases a
 * defending operator may answer, read in one follower snapshot:
 *
 * - a case is a live output at the case validator holding exactly one
 *   case-policy token, with a case datum. The policy mints a case token
 *   only when a case opens, so an output that merely carries a case datum
 *   is never a case;
 * - its history is followed back by out-ref, from the live case output
 *   through the transaction that created it and the case input that
 *   transaction spent, to the transaction that minted the token. The
 *   history names every operator that ever defended the case. When the
 *   follower no longer holds a spent link (its retention pruned it), the
 *   history is incomplete and is not returned;
 * - our thread of a case is found by its token (the case policy's, named
 *   by the case and our key), which only our own Defend mints: at a thread
 *   holder it is live; at the award it is lost; burned, it ended (lost when
 *   the burn minted the lost-defense receipt); spent to an output the
 *   follower does not track, its one-step resolution is running.
 */
import {
  readValidationTraceThread,
  type ValidationTraceCaseTransaction,
  type ValidationTraceThread,
  type ValidationTraceThreadHolder,
  validationTraceThreadTokenName,
} from "@al-ft/midgard-fault-proofs";
import {
  changedUtxosIn,
  type Dialect,
  liveUtxosIn,
  type OutRef,
  type SqlTx,
  type StoredOutput,
  type StoredTx,
  tipIn,
  txByHashIn,
} from "@al-ft/midgard-l1-follower";
import { toLucidUtxo } from "@al-ft/midgard-l1-follower/provider";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

/** The dispute contracts a defense reads, by hash. */
export type ValidationTraceDefenseChain = Readonly<{
  /** The case validator's policy: case and thread tokens. */
  casePolicyId: string;
  /** The case validator's payment credential. */
  caseCredential: string;
  holders: Readonly<Record<ValidationTraceThreadHolder | "award", string>>;
  /** The lost-defense receipts' policy. */
  awardPolicyId: string;
}>;

type DisputeContracts = Readonly<
  Record<
    "case" | "game" | "timeout" | "boundary",
    Readonly<{ spendingScriptHash: string; policyId?: string }>
  > & {
    award: Readonly<{ spendingScriptHash: string; policyId: string }>;
    case: Readonly<{ spendingScriptHash: string; policyId: string }>;
  }
>;

export const validationTraceDefenseChain = (
  dispute: DisputeContracts,
): ValidationTraceDefenseChain => ({
  casePolicyId: dispute.case.policyId,
  caseCredential: dispute.case.spendingScriptHash,
  holders: {
    game: dispute.game.spendingScriptHash,
    timeout: dispute.timeout.spendingScriptHash,
    boundary: dispute.boundary.spendingScriptHash,
    award: dispute.award.spendingScriptHash,
  },
  awardPolicyId: dispute.award.policyId,
});

/** A case found by its token. */
export type DiscoveredCase = Readonly<{
  caseUnit: string;
  caseId: string;
  utxo: UTxO;
  datum: SDK.Case;
  /**
   * The case token's history, oldest first, from the transaction that
   * minted it; undefined when the follower no longer holds all of it.
   */
  history: readonly ValidationTraceCaseTransaction[] | undefined;
}>;

/** Where our thread of a case stands in the facts. */
export type OurThreadFacts =
  /** No thread of ours was ever seen. */
  | Readonly<{ kind: "none" }>
  | Readonly<{ kind: "live"; thread: ValidationTraceThread }>
  /** At the award: the challenger won it. */
  | Readonly<{ kind: "awarded" }>
  /** Spent past the boundary, into the one-step resolution. */
  | Readonly<{ kind: "resolving" }>
  /** Burned; `lost` when the burn minted our lost-defense receipt. */
  | Readonly<{ kind: "closed"; lost: boolean }>;

/** Our thread of a case: its token and where it stands. */
export type OurThread = Readonly<{
  caseId: string;
  unit: string;
  facts: OurThreadFacts;
}>;

export type DefenseFacts = Readonly<{
  /** The follower's cursor the facts are read at. */
  point: Readonly<{ slot: number; hash: string }>;
  cases: readonly DiscoveredCase[];
  /** Our threads: of every live case, and every live one whose case is gone. */
  threads: readonly OurThread[];
}>;

const hexOf = (bytes: Buffer) => bytes.toString("hex");
const outRefLabel = (outRef: OutRef) =>
  `${hexOf(outRef.txHash)}#${outRef.index.toString()}`;

const unitParts = (unit: string) => ({
  policyId: Buffer.from(unit.slice(0, 56), "hex"),
  assetName: Buffer.from(unit.slice(56), "hex"),
});

const quantityOf = (stored: StoredOutput, unit: string): bigint =>
  stored.output.assets.get(unit.slice(0, 56))?.get(unit.slice(56)) ?? 0n;

const minted = (tx: StoredTx, unit: string): bigint =>
  tx.mint.get(unit.slice(0, 56))?.get(unit.slice(56)) ?? 0n;

const decoded = <T>(datum: Buffer | null, as: (hex: string) => T) => {
  if (datum === null) return;
  try {
    return as(datum.toString("hex"));
  } catch {
    return;
  }
};

/** The case unit of `stored`, when it holds exactly one case token. */
const caseUnitOf = (
  stored: StoredOutput,
  casePolicyId: string,
): string | undefined => {
  const names = stored.output.assets.get(casePolicyId);
  if (names === undefined || names.size !== 1) return;
  const [entry] = [...names];
  return entry?.[1] === 1n ? casePolicyId + entry[0] : undefined;
};

/**
 * The history of `caseUnit` back from `live`, oldest first: each link's
 * creating transaction, until the one that minted the token. Undefined when
 * a link is no longer stored.
 */
const caseHistoryIn = async (
  tx: SqlTx,
  dialect: Dialect,
  live: StoredOutput,
  caseUnit: string,
): Promise<ValidationTraceCaseTransaction[] | undefined> => {
  const history: ValidationTraceCaseTransaction[] = [];
  const seen = new Set<string>();
  for (let link: StoredOutput | undefined = live; link !== undefined; ) {
    const creator = await txByHashIn(tx, dialect, link.outRef.txHash);
    if (creator === null || seen.has(hexOf(creator.hash))) return;
    seen.add(hexOf(creator.hash));
    history.unshift({
      txHash: hexOf(creator.hash),
      bodyCbor: hexOf(creator.bodyCbor),
    });
    if (minted(creator, caseUnit) === 1n) return history;
    link = undefined;
    for (const input of creator.inputs) {
      const spent = await changedUtxosIn(
        tx,
        dialect,
        { by: "outref", outRefs: [input] },
        null,
      );
      const row = spent.kind === "ok" ? spent.utxos[0] : undefined;
      if (row !== undefined && quantityOf(row, caseUnit) === 1n) {
        link = row;
        break;
      }
    }
  }
  return;
};

const liveAt = async (
  tx: SqlTx,
  dialect: Dialect,
  credential: string,
): Promise<readonly StoredOutput[]> => {
  const read = await liveUtxosIn(tx, dialect, {
    by: "payment_credential",
    hash: Buffer.from(credential, "hex"),
  });
  if (read.kind !== "ok")
    throw new Error(`the follower cannot read live outputs: ${read.kind}`);
  return read.utxos;
};

const holderOf = (
  chain: ValidationTraceDefenseChain,
  stored: StoredOutput,
): ValidationTraceThreadHolder | "award" | undefined => {
  const credential = stored.output.paymentCredential;
  if (credential === null || !credential.isScript) return;
  const hash = hexOf(credential.hash);
  return (["game", "timeout", "boundary", "award"] as const).find(
    (holder) => chain.holders[holder] === hash,
  );
};

/** Where our thread `unit` of case `caseId` stands. */
const ourThreadIn = async (
  tx: SqlTx,
  dialect: Dialect,
  input: Readonly<{
    chain: ValidationTraceDefenseChain;
    ourVkh: string;
    caseId: string;
    unit: string;
    /** The case's bounty claimant, when the case is live. */
    prover: string | undefined;
  }>,
): Promise<OurThreadFacts> => {
  const { chain, ourVkh, caseId, unit } = input;
  const rows = await changedUtxosIn(
    tx,
    dialect,
    { by: "unit", ...unitParts(unit) },
    null,
  );
  if (rows.kind !== "ok")
    throw new Error(`the follower cannot read our thread: ${rows.kind}`);
  const live = rows.utxos.find((row) => row.spent === null);
  if (live !== undefined) {
    const at = holderOf(chain, live);
    if (at === "award") return { kind: "awarded" };
    if (at === undefined) return { kind: "resolving" };
    const utxo = toLucidUtxo(live.outRef, live.output);
    const datum = decoded(live.output.datum, (hex) =>
      Data.from(hex, SDK.CaseThreadDatum),
    );
    const thread = readValidationTraceThread({
      utxo,
      unit,
      caseId,
      defenderVkh: ourVkh,
      prover: input.prover ?? datum?.fraud_prover ?? "",
      at,
    });
    if (thread === undefined)
      throw new Error(
        `our thread ${unit} at ${outRefLabel(live.outRef)} does not read as a thread`,
      );
    return { kind: "live", thread };
  }
  const created = (row: StoredOutput) =>
    row.created === null ? -1 : row.created.slot * 1e6 + row.created.txIndex;
  const last = [...rows.utxos].sort((a, b) => created(a) - created(b)).at(-1);
  if (last?.spent == null) return { kind: "none" };
  const spender = await txByHashIn(tx, dialect, last.spent.txHash);
  if (spender === null) return { kind: "none" };
  if (minted(spender, unit) === -1n)
    return {
      kind: "closed",
      lost: minted(spender, chain.awardPolicyId + unit.slice(56)) === 1n,
    };
  return { kind: "resolving" };
};

/**
 * The cases and our threads in the follower's facts now, in one snapshot.
 * Throws when the follower cannot answer (not initialized).
 */
export const readDefenseFactsIn = async (
  tx: SqlTx,
  dialect: Dialect,
  input: Readonly<{ chain: ValidationTraceDefenseChain; ourVkh: string }>,
): Promise<DefenseFacts> => {
  const { chain, ourVkh } = input;
  const tip = await tipIn(tx, dialect);
  if (tip === null) throw new Error("the follower is not initialized");
  const cases: DiscoveredCase[] = [];
  const threads = new Map<string, OurThread>();
  for (const stored of await liveAt(tx, dialect, chain.caseCredential)) {
    const caseUnit = caseUnitOf(stored, chain.casePolicyId);
    const datum = decoded(stored.output.datum, (hex) =>
      Data.from(hex, SDK.Case),
    );
    if (caseUnit === undefined || datum === undefined) continue;
    const caseId = caseUnit.slice(56);
    cases.push({
      caseUnit,
      caseId,
      utxo: toLucidUtxo(stored.outRef, stored.output),
      datum,
      history: await caseHistoryIn(tx, dialect, stored, caseUnit),
    });
    const unit =
      chain.casePolicyId + validationTraceThreadTokenName(caseId, ourVkh);
    threads.set(unit, {
      caseId,
      unit,
      facts: await ourThreadIn(tx, dialect, {
        chain,
        ourVkh,
        caseId,
        unit,
        prover: datum.bounty_claimant,
      }),
    });
  }
  // Our live threads whose case is gone: by their datum's case and key,
  // proven ours by the token only our Defend mints.
  for (const holder of ["game", "timeout", "boundary"] as const)
    for (const stored of await liveAt(tx, dialect, chain.holders[holder])) {
      const datum = decoded(stored.output.datum, (hex) =>
        Data.from(hex, SDK.CaseThreadDatum),
      );
      const thread = datum?.data;
      if (thread == null || thread.header.defender_vkh !== ourVkh) continue;
      const unit =
        chain.casePolicyId +
        validationTraceThreadTokenName(thread.case_id, ourVkh);
      if (threads.has(unit) || quantityOf(stored, unit) !== 1n) continue;
      threads.set(unit, {
        caseId: thread.case_id,
        unit,
        facts: await ourThreadIn(tx, dialect, {
          chain,
          ourVkh,
          caseId: thread.case_id,
          unit,
          prover: undefined,
        }),
      });
    }
  return {
    point: { slot: tip.point.slot, hash: hexOf(tip.point.hash) },
    cases,
    threads: [...threads.values()],
  };
};
