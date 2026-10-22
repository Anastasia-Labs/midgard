/** The bond slash of a removed block's operator by its lost-defense receipt.
 * In a dispute family whose case can be defended, the block's operator can
 * be a defender who lost a thread of the case; its slash by that thread's
 * receipt (operator-directory.ak `SlashOperatorForLostDefense`) then ends
 * its bond, and the block's removal proceeds without a bond slash of its
 * own. That slash stands for the removal's only when it spends the receipt
 * this case's award minted for this operator: the receipt's name binds the
 * case and the defender (validation-case-v1.ak `receipt_token_name`). */
import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  historicalTransactions,
  operatorBondInputs,
  transactionOutputs,
} from "./raw-l1-family-derivation.derive-retained-state-queue-header-observation-from-raw-l1.js";
import {
  type FraudProofRawL1TerminalDefinition,
  mintQuantity,
} from "./raw-l1-family-derivation.state-queue-topology.js";
import {
  admitFraudProofRawL1Snapshot,
  type FraudProofRawL1Snapshot,
  type FraudProofRawL1SnapshotAuthority,
  type FraudProofRawL1Transaction,
  type FraudProofRawL1Utxo,
} from "./raw-l1-snapshot.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";

/** One token's complete authenticated L1 history. */
export type FraudProofRawL1HistoryReader = (
  unit: string,
) => Promise<readonly FraudProofRawL1Transaction[]>;

/** A token's history over the authenticated raw L1 view, strictly admitted. */
export const fraudProofRawL1HistoryReader =
  ({
    authority,
    releaseFinality,
    headerHash,
  }: {
    readonly authority: FraudProofRawL1SnapshotAuthority;
    readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
    readonly headerHash: string;
  }): FraudProofRawL1HistoryReader =>
  async (unit) => {
    const request = {
      deploymentIdentityDigest: releaseFinality.deploymentIdentityDigest,
      blueprintHash: releaseFinality.blueprintHash,
      finalityPolicyDigest: releaseFinality.policyDigest,
      headerHash,
      scopes: [],
      historyUnits: [unit],
    };
    // The request names no scope, so the snapshot holds the history alone.
    return admitFraudProofRawL1Snapshot({
      value: await authority.capture(request),
      request,
      releaseFinality,
      observationDepth: "inclusion",
    }).transactions;
  };

/** The one case token the computation thread's outputs carried. */
const caseIdOf = ({
  snapshot,
  definition,
  casePolicyId,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1TerminalDefinition;
  readonly casePolicyId: string;
}): string | undefined => {
  const threadUnit = toUnit(
    definition.computationThread.policyId,
    `${definition.categoryId}${definition.headerHash}`,
  );
  const cases = new Set<string>();
  for (const transaction of historicalTransactions({
    snapshot,
    unit: threadUnit,
  }))
    for (const { output } of transactionOutputs(transaction)) {
      const { assets } = coreToTxOutput(output);
      if ((assets[threadUnit] ?? 0n) !== 1n) continue;
      for (const unit of Object.keys(assets))
        if (unit.startsWith(casePolicyId) && unit !== casePolicyId)
          cases.add(unit.slice(56));
    }
  return cases.size === 1 ? [...cases][0] : undefined;
};

/**
 * The slash of the removed block's operator by the receipt of this case's
 * thread it lost, when the family's cases can be defended and that is how
 * its bond ended: the one transaction of the receipt's history that burns
 * the receipt and consumes the operator's bond. `undefined` otherwise.
 */
export const lostDefenseReceiptSlash = async ({
  snapshot,
  definition,
  operatorCredential,
  activeUnit,
  retiredUnit,
  history,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1TerminalDefinition;
  readonly operatorCredential: string;
  readonly activeUnit: string;
  readonly retiredUnit: string;
  readonly history: FraudProofRawL1HistoryReader | undefined;
}): Promise<
  | Readonly<{
      transaction: FraudProofRawL1Transaction;
      bondInput: FraudProofRawL1Utxo;
    }>
  | undefined
> => {
  const lostDefense = definition.lostDefense;
  if (lostDefense === undefined || history === undefined) return undefined;
  const caseId = caseIdOf({
    snapshot,
    definition,
    casePolicyId: lostDefense.casePolicyId,
  });
  if (caseId === undefined) return undefined;
  const receiptUnit = toUnit(
    lostDefense.receiptPolicyId,
    Effect.runSync(SDK.hashHexWithBlake2b(caseId + operatorCredential, 32)),
  );
  const slashes = (await history(receiptUnit)).flatMap((transaction) => {
    const body = CML.TransactionBody.from_cbor_hex(transaction.bodyCbor);
    if (mintQuantity(body, receiptUnit) !== -1n) return [];
    const inputs = operatorBondInputs({
      transaction,
      definition,
      activeUnit,
      retiredUnit,
    });
    return inputs.length === 1 ? [{ transaction, bondInput: inputs[0]! }] : [];
  });
  return slashes.length === 1 ? slashes[0] : undefined;
};
