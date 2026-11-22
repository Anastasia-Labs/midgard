/** Where the threads of our case are found: along our case output's spend
 * chain. Every Defend on our case spends the case output and leaves the
 * thread it opens beside the case's successor, and every thread close spends
 * the case again, so the case token's complete L1 history from genesis names
 * every thread the case ever had and every one closed since. That history is
 * the durable record of the threads found: it holds a thread whatever becomes
 * of its defender (an operator removed from both operator lists mid-case is
 * still in it), and it survives any restart. Each thread a Defend opened is
 * re-read by its token until the history shows a close of the case burned
 * it. */
import type { PendingTransaction } from "@al-ft/midgard-l1-follower/mempool";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKDa from "@al-ft/midgard-sdk/da";
import {
  CML,
  coreToTxOutput,
  Data,
  type TxOutput,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { addMintedAssets } from "../workflow/minted-assets.js";
import { admitFraudProofRawL1Snapshot } from "../workflow/raw-l1-snapshot.admit-fraud-proof-raw-l1-snapshot.js";
import type {
  FraudProofRawL1SnapshotAuthority,
  FraudProofRawL1SnapshotRequest,
  FraudProofRawL1Transaction,
} from "../workflow/raw-l1-snapshot.compute-fraud-proof-raw-l1-snapshot-evidence-digest.js";
import type { FraudProofRawL1Snapshot } from "../workflow/raw-l1-snapshot.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "../workflow/release-finality-policy.js";

const blake2b256 = (hex: string): string =>
  Effect.runSync(SDK.hashHexWithBlake2b(hex, 32));

/** hash(case id ‖ defender key hash): a thread token's and receipt's name. */
export const validationTraceThreadTokenName = (
  caseId: string,
  defenderVkh: string,
): string => blake2b256(caseId + defenderVkh);

/** One transaction of the case token's history, as the chain holds it. */
export type ValidationTraceCaseTransaction = Pick<
  FraudProofRawL1Transaction,
  "txHash" | "bodyCbor"
>;

/** The case token's complete L1 history. */
export type ValidationTraceCaseHistory = Readonly<{
  /**
   * Every transaction that consumed or produced the token `caseUnit`,
   * complete from genesis, observed for the dispute over `headerHash`.
   * `alongside` names further units the capture reads with it, so the
   * observer holds their history under the header too (a receipt whose
   * slash still reads the case once the case token is burned); only the
   * case token's transactions are returned.
   */
  transactions(input: {
    readonly headerHash: string;
    readonly caseUnit: string;
    readonly alongside?: readonly string[];
  }): Promise<readonly ValidationTraceCaseTransaction[]>;
  /**
   * One authenticated capture for the dispute over `headerHash`: the
   * outputs at `scopes` and the complete history of each of `historyUnits`,
   * which the observer then holds under the header. Absent where the
   * history is served without the raw L1 view. `admitted` holds
   * transactions an earlier capture admitted, by hash: one served again in
   * exactly the same bytes is not decoded again. The snapshot comes with
   * our node's mempool at its tip, which never enters the snapshot.
   */
  snapshot?(
    input: Pick<
      FraudProofRawL1SnapshotRequest,
      "headerHash" | "scopes" | "historyUnits"
    > & {
      readonly admitted?: ReadonlyMap<string, FraudProofRawL1Transaction>;
    },
  ): Promise<ValidationTraceDisputeSnapshot>;
}>;

/** One admitted snapshot of the dispute and our node's mempool at its tip,
 * in mempool order. */
export type ValidationTraceDisputeSnapshot = Readonly<{
  snapshot: FraudProofRawL1Snapshot;
  pending: readonly PendingTransaction[];
}>;

/** The case history over the authenticated raw L1 view, strictly admitted. */
export const createValidationTraceCaseHistory = ({
  authority,
  releaseFinality,
}: {
  readonly authority: FraudProofRawL1SnapshotAuthority;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}): ValidationTraceCaseHistory => {
  const snapshot: NonNullable<ValidationTraceCaseHistory["snapshot"]> = async ({
    admitted,
    ...input
  }) => {
    const request: FraudProofRawL1SnapshotRequest = {
      hubOraclePolicyId: releaseFinality.hubOraclePolicyId,
      blueprintHash: releaseFinality.blueprintHash,
      finalityPolicyDigest: releaseFinality.policyDigest,
      ...input,
    };
    const { snapshot: value, pending } = await authority.capture(request);
    return {
      snapshot: admitFraudProofRawL1Snapshot({
        value,
        request,
        releaseFinality,
        observationDepth: "inclusion",
        admitted,
      }),
      pending,
    };
  };
  return Object.freeze({
    snapshot,
    transactions: async ({ headerHash, caseUnit, alongside = [] }) => {
      const { snapshot: captured } = await snapshot({
        headerHash,
        scopes: [],
        historyUnits: [
          caseUnit,
          ...new Set(alongside.filter((unit) => unit !== caseUnit)),
        ],
      });
      // The request names no scope, so the snapshot holds the histories
      // alone; the case token's is the one returned.
      const ofCase = new Set(
        captured.history.find(
          ({ unit }) => unit === caseUnit,
        )?.transactionHashes,
      );
      return captured.transactions.filter(({ txHash }) => ofCase.has(txHash));
    },
  });
};

/** A thread the case's history held: its token and its defender. */
export type ValidationTraceRecordedThread = Readonly<{
  unit: string;
  defenderVkh: string;
}>;

/** The defender named by `output`'s thread datum, if it has one. */
const threadDefender = (output: TxOutput): string | undefined => {
  if (typeof output.datum !== "string") return;
  try {
    return (
      Data.from(output.datum, SDKDa.CaseThreadDatum).data?.header
        .defender_vkh ?? undefined
    );
  } catch {
    return;
  }
};

/** Every thread token the history names, with its defender, its net mint
 * over the history and whether one transaction of the history minted it. */
const caseThreadTokens = ({
  transactions,
  caseUnit,
}: {
  readonly transactions: readonly ValidationTraceCaseTransaction[];
  readonly caseUnit: string;
}) => {
  const policyId = caseUnit.slice(0, 56);
  const caseId = caseUnit.slice(56);
  const units = new Map<string, string>();
  // Each token's net mint over the history: 1 while its thread is open.
  const net = new Map<string, bigint>();
  // The tokens a single transaction minted: each one's thread was opened.
  const opened = new Set<string>();
  for (const transaction of transactions) {
    const body = CML.TransactionBody.from_cbor_hex(transaction.bodyCbor);
    const minted = new Map<string, bigint>();
    addMintedAssets(minted, body.mint());
    for (const [unit, quantity] of minted) {
      net.set(unit, (net.get(unit) ?? 0n) + quantity);
      if (quantity > 0n) opened.add(unit);
    }
    const outputs = body.outputs();
    for (let index = 0; index < outputs.len(); index += 1) {
      const output = coreToTxOutput(outputs.get(index));
      const defenderVkh = threadDefender(output);
      if (defenderVkh === undefined) continue;
      const unit =
        policyId + validationTraceThreadTokenName(caseId, defenderVkh);
      units.set(unit, defenderVkh);
    }
  }
  return [...units].map(([unit, defenderVkh]) => ({
    unit,
    defenderVkh,
    open: net.get(unit) === 1n,
    opened: opened.has(unit),
  }));
};

/**
 * The live threads case `caseUnit`'s history holds, sorted by token: one for
 * each defender a thread datum of the history names whose thread token (the
 * token the case policy names for our case and that defender) the history
 * minted, in that defender's Defend, and has not burned since, in a close of
 * the case. An output whose datum merely looks like a thread, for a defender
 * no Defend minted a token for, opens none, and a closed thread is no longer
 * held. Nothing else is decided here: a re-read output is a live thread of
 * our case for that defender only as its own datum says.
 */
export const recordedValidationTraceThreads = (input: {
  readonly transactions: readonly ValidationTraceCaseTransaction[];
  readonly caseUnit: string;
}): readonly ValidationTraceRecordedThread[] =>
  Object.freeze(
    caseThreadTokens(input)
      .filter(({ open }) => open)
      .map(({ unit, defenderVkh }) => Object.freeze({ unit, defenderVkh }))
      .sort((a, b) => (a.unit < b.unit ? -1 : a.unit > b.unit ? 1 : 0)),
  );

/**
 * Every defender case `caseUnit`'s history holds, sorted: each one a thread
 * datum of the history names whose thread token a transaction of the history
 * minted, whether its thread is still open or has closed since. These are
 * the keys the case's defenders root holds; a datum that merely looks like a
 * thread names none.
 */
export const recordedValidationTraceDefenders = (input: {
  readonly transactions: readonly ValidationTraceCaseTransaction[];
  readonly caseUnit: string;
}): readonly string[] =>
  Object.freeze(
    caseThreadTokens(input)
      .filter(({ opened }) => opened)
      .map(({ defenderVkh }) => defenderVkh)
      .sort(),
  );
