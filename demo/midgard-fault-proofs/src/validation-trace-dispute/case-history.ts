/** Where the threads of our case are found: along our case output's spend
 * chain. Every Defend on our case spends the case output and leaves the
 * thread it opens beside the case's successor, and every thread close spends
 * the case again, so the case token's complete L1 history from genesis names
 * every thread the case ever had and every one closed since. That history is
 * the durable record of the threads found: it holds a thread whatever becomes
 * of its defender (an operator removed from both operator lists mid-case is
 * still in it), and it survives any restart. Each thread it holds is re-read
 * by its token until a close of the case burns it. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  Data,
  type TxOutput,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

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
 * The threads case `caseUnit`'s history ever held, sorted by token: one for
 * each defender a thread datum names in an output a transaction of the
 * history left (the case's own output has none), under the thread token
 * the case policy names for our case and that defender. Nothing else is
 * decided here: a closed thread's token is burned, so its re-read by token
 * finds nothing, and a re-read output is a live thread of our case for that
 * defender only as its own datum says.
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
  for (const transaction of transactions) {
    const outputs = CML.TransactionBody.from_cbor_hex(
      transaction.bodyCbor,
    ).outputs();
    for (let index = 0; index < outputs.len(); index += 1) {
      const defenderVkh = threadDefender(coreToTxOutput(outputs.get(index)));
      if (defenderVkh !== undefined)
        units.set(
          policyId + validationTraceThreadTokenName(caseId, defenderVkh),
          defenderVkh,
        );
    }
  }
  return Object.freeze(
    [...units]
      .map(([unit, defenderVkh]) => Object.freeze({ unit, defenderVkh }))
      .sort((a, b) => (a.unit < b.unit ? -1 : a.unit > b.unit ? 1 : 0)),
  );
};
