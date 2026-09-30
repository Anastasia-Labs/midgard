import * as SDK from "@al-ft/midgard-sdk";
import {
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  paymentCredentialOf,
} from "@lucid-evolution/lucid";

import {
  type OperatorExitFixture,
  runProgram,
  SHIFT_DURATION_MS,
} from "./operator-exit-emulator.build-operator-exit-snapshot.js";
import {
  alignMs,
  fetchDirectorySnapshot,
  fundAccount,
  registerAndActivate,
  resyncWallet,
  submitSigned,
} from "./operator-exit-emulator.build-retire-tx.js";

/**
 * Ends the current shift and hands it to the predecessor of the scheduled
 * operator through `GoToNextDueToEndOfShift`. The incoming operator has to
 * sign its own shift, and which active key precedes the scheduled one depends
 * on their (random) order, so the caller supplies every operator's wallet.
 * Returns the incoming operator's key.
 */
export const advanceShiftToPredecessor = async ({
  fixture,
  wallets,
  scheduledKeyHash,
  shiftStart,
}: {
  readonly fixture: OperatorExitFixture;
  readonly wallets: ReadonlyMap<string, LucidEvolution>;
  readonly scheduledKeyHash: string;
  readonly shiftStart: bigint;
}): Promise<string> => {
  const { lucid, contracts, scriptRefs } = fixture;
  const snapshot = await fetchDirectorySnapshot(lucid, contracts);
  const advanceTarget = snapshot.active.find(
    (node) => SDK.nodeKeyHex(node.datum.next) === scheduledKeyHash,
  );
  if (advanceTarget === undefined) {
    throw new Error("Expected a predecessor of the scheduled operator");
  }
  const nextOperator = SDK.nodeKeyHex(advanceTarget.datum.key);
  if (nextOperator === null) {
    throw new Error("Expected the advance target to be a list member");
  }
  const nextLucid = wallets.get(nextOperator);
  if (nextLucid === undefined) {
    throw new Error("Expected to know the incoming operator's wallet");
  }
  await resyncWallet(nextLucid);
  const validFrom = alignMs(lucid, shiftStart + SHIFT_DURATION_MS + 30_000n);
  const { tx } = await runProgram(
    SDK.buildUnsignedSchedulerRefreshTxProgram({
      lucid: nextLucid,
      scheduler: contracts.scheduler,
      operatorKeyHash: nextOperator,
      schedulerInput: snapshot.scheduler.utxo,
      refreshedDatum: {
        ActiveOperator: { operator: nextOperator, start_time: validFrom },
      },
      validFrom,
      validTo: validFrom + 8n * 60n * 1000n,
      selection: {
        kind: "Advance",
        activeNode: { utxo: advanceTarget.utxo },
      },
      schedulerSpendingScriptRef: scriptRefs.schedulerSpending,
    }),
  );
  await submitSigned(nextLucid, tx);
  return nextOperator;
};

/**
 * Like {@link addActivatedOperator}, but with a key that sorts after
 * `aboveKeyHash`, so its active node is inserted with that operator's node as
 * its anchor and the two share a transaction hash.
 */
export const addActivatedOperatorAbove = async (
  fixture: OperatorExitFixture,
  aboveKeyHash: string,
): Promise<{
  readonly lucid: LucidEvolution;
  readonly operatorKeyHash: string;
}> => {
  let account = generateEmulatorAccount({ lovelace: 0n });
  while (paymentCredentialOf(account.address).hash <= aboveKeyHash) {
    account = generateEmulatorAccount({ lovelace: 0n });
  }
  await fundAccount(fixture.lucid, account.address, 3_000_000_000n);
  const operatorLucid = await Lucid(fixture.emulator, "Custom");
  operatorLucid.selectWallet.fromSeed(account.seedPhrase);
  await registerAndActivate({
    operatorLucid,
    referenceScriptsLucid: fixture.referenceScriptsLucid,
    contracts: fixture.contracts,
    emulator: fixture.emulator,
  });
  return {
    lucid: operatorLucid,
    operatorKeyHash: paymentCredentialOf(account.address).hash,
  };
};

export const requireActiveOperator = (
  datum: SDK.SchedulerDatum,
): { readonly operator: string; readonly start_time: bigint } => {
  if (datum === "NoActiveOperators") {
    throw new Error("Expected the scheduler to name an operator");
  }
  return datum.ActiveOperator;
};
