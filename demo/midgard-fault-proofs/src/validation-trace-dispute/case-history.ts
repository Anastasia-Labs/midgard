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
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  Data,
  type TxOutput,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { addMintedAssets } from "../workflow/funding-reservation-permit.ledger-accounting.js";
import {
  admitFraudProofRawL1Snapshot,
  type FraudProofRawL1SnapshotAuthority,
  type FraudProofRawL1SnapshotRequest,
  type FraudProofRawL1Transaction,
} from "../workflow/raw-l1-snapshot.js";
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
   */
  transactions(input: {
    readonly headerHash: string;
    readonly caseUnit: string;
  }): Promise<readonly ValidationTraceCaseTransaction[]>;
}>;

/** The case history over the authenticated raw L1 view, strictly admitted. */
export const createValidationTraceCaseHistory = ({
  authority,
  releaseFinality,
}: {
  readonly authority: FraudProofRawL1SnapshotAuthority;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}): ValidationTraceCaseHistory =>
  Object.freeze({
    transactions: async ({ headerHash, caseUnit }) => {
      const request: FraudProofRawL1SnapshotRequest = {
        deploymentIdentityDigest: releaseFinality.deploymentIdentityDigest,
        blueprintHash: releaseFinality.blueprintHash,
        finalityPolicyDigest: releaseFinality.policyDigest,
        headerHash,
        scopes: [],
        historyUnits: [caseUnit],
      };
      const snapshot = admitFraudProofRawL1Snapshot({
        value: await authority.capture(request),
        request,
        releaseFinality,
        observationDepth: "inclusion",
      });
      // The request names no scope, so the snapshot holds the history alone.
      return snapshot.transactions;
    },
  });

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
      Data.from(output.datum, SDK.CaseThreadDatum).data?.header.defender_vkh ??
      undefined
    );
  } catch {
    return;
  }
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
export const recordedValidationTraceThreads = ({
  transactions,
  caseUnit,
}: {
  readonly transactions: readonly ValidationTraceCaseTransaction[];
  readonly caseUnit: string;
}): readonly ValidationTraceRecordedThread[] => {
  const policyId = caseUnit.slice(0, 56);
  const caseId = caseUnit.slice(56);
  const units = new Map<string, string>();
  // Each token's net mint over the history: 1 while its thread is open.
  const net = new Map<string, bigint>();
  for (const transaction of transactions) {
    const body = CML.TransactionBody.from_cbor_hex(transaction.bodyCbor);
    addMintedAssets(net, body.mint());
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
  return Object.freeze(
    [...units]
      .filter(([unit]) => net.get(unit) === 1n)
      .map(([unit, defenderVkh]) => Object.freeze({ unit, defenderVkh }))
      .sort((a, b) => (a.unit < b.unit ? -1 : a.unit > b.unit ? 1 : 0)),
  );
};
