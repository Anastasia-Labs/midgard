import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  type LucidEvolution,
  paymentCredentialOf,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";

import {
  activateOperatorProgram,
  registerOperatorProgram,
} from "../src/transactions/register-active-operator.js";
import { alignUnixTimeToSlotBoundary } from "../src/workers/utils/commit-end-time.js";
import {
  BOND_LOVELACE,
  describeFailure,
  INACTIVITY_SLASHING_PENALTY_LOVELACE,
  type OperatorExitFixture,
  type OperatorExitScriptRefs,
  runProgram,
} from "./operator-exit-emulator.build-operator-exit-snapshot.js";

// ---------------------------------------------------------------------------
// Shared helpers
// ---------------------------------------------------------------------------

/**
 * Replaces the wallet's cached UTxO set with what the ledger actually holds.
 * The node's own transaction programs override the wallet view with predicted
 * outputs, and a stale entry there is picked as collateral and rejected at
 * submission, so every builder here starts from a fresh read.
 */
export const resyncWallet = async (lucid: LucidEvolution): Promise<void> => {
  const address = await lucid.wallet().address();
  lucid.overrideUTxOs(await lucid.utxosAt(address));
};

/**
 * Lucid's own promises reject with an Effect `FiberFailure`, so every
 * submission goes through {@link describeFailure} as well.
 */
export const submitSigned = async (
  lucid: LucidEvolution,
  tx: TxSignBuilder,
): Promise<string> => {
  try {
    const signed = await tx.sign.withWallet().complete();
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash);
    await resyncWallet(lucid);
    return txHash;
  } catch (cause) {
    throw new Error(describeFailure(cause));
  }
};

export const fetchDirectorySnapshot = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Promise<SDK.OperatorDirectorySnapshot> =>
  runProgram(SDK.fetchOperatorDirectorySnapshotProgram(lucid, contracts));

export const keyHashOf = async (lucid: LucidEvolution): Promise<string> => {
  const credential = paymentCredentialOf(await lucid.wallet().address());
  if (credential?.type !== "Key") {
    throw new Error("Expected a key payment credential");
  }
  return credential.hash;
};

export const walletLovelace = async (lucid: LucidEvolution): Promise<bigint> =>
  (await lucid.utxosAt(await lucid.wallet().address())).reduce(
    (total, utxo) => total + (utxo.assets["lovelace"] ?? 0n),
    0n,
  );

/** `alignUnixTimeToSlotBoundary` works in numbers; every builder here in ms. */
export const alignMs = (lucid: LucidEvolution, posixMs: bigint): bigint =>
  BigInt(alignUnixTimeToSlotBoundary(lucid, Number(posixMs)));

/**
 * A closed, slot-aligned range well inside the 8-minute maximum. A Lucid
 * instance created from the emulator takes the slot of its creation as slot
 * zero, so the lower bound never reaches before that instance's zero time.
 */
const exitValidityWindow = (
  lucid: LucidEvolution,
  emulator: Emulator,
): { readonly validFrom: bigint; readonly validTo: bigint } => {
  const now = BigInt(emulator.now());
  const zeroTime = BigInt(lucid.config().slotConfig?.zeroTime ?? 0);
  const lookback = now - 60_000n;
  return {
    validFrom: alignMs(lucid, lookback > zeroTime ? lookback : zeroTime),
    validTo: alignMs(lucid, now + 120_000n),
  };
};

export const fundAccount = async (
  lucid: LucidEvolution,
  address: string,
  lovelace: bigint,
): Promise<void> => {
  const completed = await lucid
    .newTx()
    .pay.ToAddress(address, { lovelace })
    .complete({ localUPLCEval: true });
  await submitSigned(lucid, completed);
};

/**
 * Registers and activates an operator through the node's own programs, which
 * is the only route that also exercises the local duplicate refusal.
 */
export const registerAndActivate = async ({
  operatorLucid,
  referenceScriptsLucid,
  contracts,
  emulator,
}: {
  readonly operatorLucid: LucidEvolution;
  readonly referenceScriptsLucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
  readonly emulator: Emulator;
}): Promise<void> => {
  await runProgram(
    registerOperatorProgram(
      operatorLucid,
      contracts,
      BOND_LOVELACE,
      referenceScriptsLucid,
    ),
  );
  emulator.awaitSlot(180);
  await runProgram(
    activateOperatorProgram(
      operatorLucid,
      contracts,
      BOND_LOVELACE,
      referenceScriptsLucid,
    ),
  );
};

export const BOND_PARAMETERS = {
  requiredBondLovelace: BOND_LOVELACE,
  inactivitySlashingPenaltyLovelace: INACTIVITY_SLASHING_PENALTY_LOVELACE,
} as const;

/**
 * Hands the shift to `operatorKeyHash`, which the scheduler only accepts for
 * the last node of the active list while no registration can activate.
 */
export const appointFirstOperator = async ({
  fixture,
  operatorLucid,
  operatorKeyHash,
}: {
  readonly fixture: OperatorExitFixture;
  readonly operatorLucid: LucidEvolution;
  readonly operatorKeyHash: string;
}): Promise<void> => {
  const { contracts, emulator, scriptRefs } = fixture;
  await resyncWallet(operatorLucid);
  const snapshot = await fetchDirectorySnapshot(operatorLucid, contracts);
  const activeNode = SDK.findNodeByKey(snapshot.active, operatorKeyHash);
  const registeredWitnessNode = SDK.findTailNode(snapshot.registered);
  if (activeNode === undefined || registeredWitnessNode === undefined) {
    throw new Error("Missing witnesses for the first scheduler appointment");
  }
  const { validFrom, validTo } = exitValidityWindow(operatorLucid, emulator);
  const { tx } = await runProgram(
    SDK.buildUnsignedSchedulerRefreshTxProgram({
      lucid: operatorLucid,
      scheduler: contracts.scheduler,
      operatorKeyHash,
      schedulerInput: snapshot.scheduler.utxo,
      refreshedDatum: {
        ActiveOperator: { operator: operatorKeyHash, start_time: validTo - 1n },
      },
      validFrom,
      validTo,
      selection: {
        kind: "AppointFirst",
        activeNode: { utxo: activeNode.utxo },
        registeredWitnessNode: { utxo: registeredWitnessNode.utxo },
      },
      schedulerSpendingScriptRef: scriptRefs.schedulerSpending,
    }),
  );
  await submitSigned(operatorLucid, tx);
};

type RetireOverrides = Partial<SDK.RetireOperatorTxConfig>;

/** What the retirement builders need from a fixture, exit or inactivity. */
export type RetireFixture = {
  readonly emulator: Emulator;
  readonly contracts: SDK.MidgardValidators;
  readonly scriptRefs: OperatorExitScriptRefs;
};

export const buildRetireTx = async ({
  fixture,
  submitterLucid,
  operatorKeyHash,
  mode,
  validity,
  overrides = {},
}: {
  readonly fixture: RetireFixture;
  readonly submitterLucid: LucidEvolution;
  readonly operatorKeyHash: string;
  readonly mode: SDK.RetirementMode;
  readonly validity?: {
    readonly validFrom: bigint;
    readonly validTo: bigint;
  };
  readonly overrides?: RetireOverrides;
}): Promise<SDK.RetireOperatorTxResult> => {
  const { contracts, emulator, scriptRefs } = fixture;
  await resyncWallet(submitterLucid);
  const snapshot = await fetchDirectorySnapshot(submitterLucid, contracts);
  const { validFrom, validTo } =
    validity ?? exitValidityWindow(submitterLucid, emulator);
  const witnesses = SDK.deriveRetireOperatorWitnesses({
    snapshot,
    contracts,
    operatorKeyHash,
    validTo,
    schedulerSpendingScriptRef: scriptRefs.schedulerSpending,
  });
  return runProgram(
    SDK.buildUnsignedRetireOperatorTxProgram({
      lucid: submitterLucid,
      contracts,
      operatorKeyHash,
      activeOperatorScriptRefs: scriptRefs.activeOperators,
      retiredOperatorScriptRefs: scriptRefs.retiredOperators,
      hubOracleRefInput: snapshot.hubOracle.utxo,
      activeNode: witnesses.activeNode,
      activeAnchor: witnesses.activeAnchor,
      retiredInsertionAnchor: witnesses.retiredInsertionAnchor,
      activeNodeUnit: witnesses.activeNodeUnit,
      retiredNodeUnit: witnesses.retiredNodeUnit,
      bondUnlockTime: witnesses.bondUnlockTime,
      retiredNodeLovelace: SDK.retiredOperatorBondTranche(
        mode,
        BOND_PARAMETERS,
      ),
      mode,
      inactivitySlashingPenaltyLovelace: INACTIVITY_SLASHING_PENALTY_LOVELACE,
      schedulerSync: witnesses.schedulerSync,
      validFrom,
      validTo,
      ...overrides,
    }),
  );
};

export const retireOperator = async (input: {
  readonly fixture: RetireFixture;
  readonly submitterLucid: LucidEvolution;
  readonly operatorKeyHash: string;
  readonly mode: SDK.RetirementMode;
  readonly overrides?: RetireOverrides;
}): Promise<string> => {
  const { tx } = await buildRetireTx(input);
  return submitSigned(input.submitterLucid, tx);
};

export const recoverOperatorBond = async ({
  fixture,
  submitterLucid,
  operatorKeyHash,
  overrides = {},
}: {
  readonly fixture: OperatorExitFixture;
  readonly submitterLucid: LucidEvolution;
  readonly operatorKeyHash: string;
  readonly overrides?: Partial<SDK.RecoverOperatorBondTxConfig>;
}): Promise<string> => {
  const { contracts, emulator, scriptRefs } = fixture;
  await resyncWallet(submitterLucid);
  const snapshot = await fetchDirectorySnapshot(submitterLucid, contracts);
  const witnesses = SDK.deriveRecoverOperatorBondWitnesses({
    snapshot,
    contracts,
    operatorKeyHash,
  });
  const { validFrom, validTo } = exitValidityWindow(submitterLucid, emulator);
  const { tx } = await runProgram(
    SDK.buildUnsignedRecoverOperatorBondTxProgram({
      lucid: submitterLucid,
      contracts,
      operatorKeyHash,
      retiredOperatorScriptRefs: scriptRefs.retiredOperators,
      retiredNode: witnesses.retiredNode,
      retiredAnchor: witnesses.retiredAnchor,
      retiredNodeUnit: witnesses.retiredNodeUnit,
      validFrom,
      validTo,
      ...overrides,
    }),
  );
  return submitSigned(submitterLucid, tx);
};

/** Completes a transaction builder, flattening lucid's `FiberFailure`. */
export const completeTx = async (builder: {
  readonly complete: (options: {
    readonly localUPLCEval: boolean;
  }) => Promise<TxSignBuilder>;
}): Promise<TxSignBuilder> => {
  try {
    return await builder.complete({ localUPLCEval: true });
  } catch (cause) {
    throw new Error(describeFailure(cause));
  }
};
