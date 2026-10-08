import * as SDK from "@al-ft/midgard-sdk";
import { createReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  paymentCredentialOf,
  PROTOCOL_PARAMETERS_DEFAULT,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { IntentJournalWithoutFollower } from "../../src/services/intent-journal.js";
import {
  buildAtomicProtocolInitTxProgram,
  ensureAtomicProtocolInitReferenceScriptsProgram,
} from "../../src/transactions/initialization.js";
import {
  activateOperatorProgram,
  registerOperatorProgram,
} from "../../src/transactions/register-active-operator.js";
import { ensureEventHistoryRewardAccountsRegisteredProgram } from "../../src/transactions/script-reward-registration.js";
import { alignUnixTimeToSlotBoundary } from "../../src/workers/utils/commit-end-time.js";
import {
  type CaptureSnapshot,
  restoreCapture,
  snapshotCapture,
} from "./emulator-chain-capture.js";
import { runWithoutFollower } from "./intent-journal.js";
import { loadRealMidgardContractsForTest } from "./real-midgard-contracts.js";

export const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: 65_536,
  maxCollateralInputs: 3,
} as const;

/**
 * `operator-directory/registered-operators.ak` enforces
 * `registered_node_lovelace == env.required_bond`, and `env/testnet.ak` — the
 * env this blueprint is built with — sets it to
 * `slashing_penalty + fraud_prover_reward`.
 */
export const EMULATOR_REQUIRED_BOND_LOVELACE = 900_000_000n;

export const EMPTY_FRAUD_PROOF_CATALOGUE_ROOT = "00".repeat(32);

export const EMULATOR_REFERENCE_SCRIPT_AUTH_TIMELOCK_MS = 24 * 60 * 60 * 1000;

export const REGISTRATION_ACTIVATION_DELAY_SLOTS = 180;

/** A closed, short strike window; 120s is a whole number of emulator slots. */
export const STRIKE_VALIDITY_WINDOW_MS = 120_000n;

export type InactivityOperatorAccount = {
  readonly seedPhrase: string;
  readonly address: string;
  readonly keyHash: string;
};

type EmulatorState = Pick<
  Emulator,
  | "ledger"
  | "mempool"
  | "chain"
  | "blockHeight"
  | "slot"
  | "time"
  | "protocolParameters"
  | "datumTable"
  | "treasury"
  | "transactionHistory"
> &
  CaptureSnapshot;

type DeploymentSnapshot = {
  readonly emulatorState: EmulatorState;
  readonly contracts: SDK.MidgardValidators;
  readonly referenceScriptsSeedPhrase: string;
  readonly operators: readonly InactivityOperatorAccount[];
};

const snapshotEmulator = (emulator: Emulator): EmulatorState => ({
  ledger: structuredClone(emulator.ledger),
  mempool: structuredClone(emulator.mempool),
  chain: structuredClone(emulator.chain),
  blockHeight: emulator.blockHeight,
  slot: emulator.slot,
  time: emulator.time,
  protocolParameters: structuredClone(emulator.protocolParameters),
  datumTable: structuredClone(emulator.datumTable),
  treasury: emulator.treasury,
  transactionHistory: structuredClone(emulator.transactionHistory),
  ...snapshotCapture(emulator),
});

const cloneEmulator = (snapshot: EmulatorState): Emulator => {
  const emulator = new Emulator(
    [],
    structuredClone(snapshot.protocolParameters),
    snapshot.treasury,
  );
  emulator.ledger = structuredClone(snapshot.ledger);
  emulator.mempool = structuredClone(snapshot.mempool);
  emulator.chain = structuredClone(snapshot.chain);
  emulator.blockHeight = snapshot.blockHeight;
  emulator.slot = snapshot.slot;
  emulator.time = snapshot.time;
  emulator.datumTable = structuredClone(snapshot.datumTable);
  emulator.transactionHistory = structuredClone(snapshot.transactionHistory);
  restoreCapture(emulator, snapshot);
  return emulator;
};

const accountFromEmulatorAccount = (account: {
  seedPhrase: string;
  address: string;
}): InactivityOperatorAccount => {
  const credential = paymentCredentialOf(account.address);
  if (credential?.type !== "Key") {
    throw new Error("Expected an emulator account with a key credential");
  }
  return {
    seedPhrase: account.seedPhrase,
    address: account.address,
    keyHash: credential.hash,
  };
};

const submitAndAwait = async (
  lucid: LucidEvolution,
  tx: TxSignBuilder,
): Promise<string> => {
  const signed = await tx.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/**
 * Builds the authenticated deployment with `operatorCount` registered and
 * activated operators. The active-operators list is ordered by key hash, so
 * the returned accounts are sorted the same way.
 */
const buildDeploymentSnapshot = async (
  operatorCount: number,
): Promise<DeploymentSnapshot> => {
  const primary = generateEmulatorAccount({ lovelace: 30_000_000_000n });
  const referenceScripts = generateEmulatorAccount({
    lovelace: 200_000_000_000n,
  });
  const emulator = new Emulator(
    [primary, referenceScripts],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const lucid = await Lucid(emulator, "Custom");
  const referenceScriptsLucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(primary.seedPhrase);
  referenceScriptsLucid.selectWallet.fromSeed(referenceScripts.seedPhrase);

  const nonceUtxo = (await lucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected at least one wallet UTxO in emulator");
  }
  const referenceScriptAuth = await createReferenceScriptAuthPolicy(
    referenceScriptsLucid,
    emulator.now(),
    EMULATOR_REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
  );
  const contracts = await loadRealMidgardContractsForTest(
    { txHash: nonceUtxo.txHash, outputIndex: nonceUtxo.outputIndex },
    referenceScriptAuth,
  );
  const publishedReferenceScripts = await runWithoutFollower(
    ensureAtomicProtocolInitReferenceScriptsProgram(
      referenceScriptsLucid,
      contracts,
    ),
  );
  await runWithoutFollower(
    ensureEventHistoryRewardAccountsRegisteredProgram(
      referenceScriptsLucid,
      contracts,
    ),
  );
  const initTx = await Effect.runPromise(
    buildAtomicProtocolInitTxProgram(
      lucid,
      contracts,
      {
        HUB_ORACLE_ONE_SHOT_TX_HASH: nonceUtxo.txHash,
        HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: nonceUtxo.outputIndex,
        L1_OPERATOR_SEED_PHRASE: primary.seedPhrase,
        NETWORK: "Preprod",
      },
      EMPTY_FRAUD_PROOF_CATALOGUE_ROOT,
      undefined,
      publishedReferenceScripts,
    ).pipe(Effect.provide(IntentJournalWithoutFollower)),
  );
  await submitAndAwait(lucid, await initTx.complete({ localUPLCEval: true }));

  const extras = Array.from({ length: Math.max(0, operatorCount - 1) }, () =>
    generateEmulatorAccount({ lovelace: 0n }),
  );
  if (extras.length > 0) {
    let funding = lucid.newTx();
    for (const extra of extras) {
      funding = funding.pay.ToAddress(extra.address, {
        lovelace: 4_000_000_000n,
      });
    }
    await submitAndAwait(
      lucid,
      await funding.complete({ localUPLCEval: true }),
    );
  }

  const accounts = [primary, ...extras].map(accountFromEmulatorAccount);
  for (const account of accounts) {
    const operatorLucid = await Lucid(emulator, "Custom");
    operatorLucid.selectWallet.fromSeed(account.seedPhrase);
    await runWithoutFollower(
      registerOperatorProgram(
        operatorLucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    emulator.awaitSlot(REGISTRATION_ACTIVATION_DELAY_SLOTS);
    await runWithoutFollower(
      activateOperatorProgram(
        operatorLucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
  }

  return {
    emulatorState: snapshotEmulator(emulator),
    contracts,
    referenceScriptsSeedPhrase: referenceScripts.seedPhrase,
    operators: [...accounts].sort((left, right) =>
      left.keyHash.localeCompare(right.keyHash),
    ),
  };
};

export const deploymentSnapshots = new Map<
  number,
  Promise<DeploymentSnapshot>
>();

export type OperatorInactivityFixture = {
  readonly emulator: Emulator;
  /** The primary operator's wallet. It pays every strike fee. */
  readonly lucid: LucidEvolution;
  readonly referenceScriptsLucid: LucidEvolution;
  readonly referenceScriptsAddress: string;
  readonly referenceScriptsSeedPhrase: string;
  readonly contracts: SDK.MidgardValidators;
  /** Active operators, ordered by key hash (i.e. in list order). */
  readonly operators: readonly InactivityOperatorAccount[];
  readonly lucidFor: (keyHash: string) => Promise<LucidEvolution>;
};

/**
 * Clones the cached deployment for `operatorCount` activated operators.
 */
export const initOperatorInactivityFixture = async (
  operatorCount = 1,
): Promise<OperatorInactivityFixture> => {
  let pending = deploymentSnapshots.get(operatorCount);
  if (pending === undefined) {
    pending = buildDeploymentSnapshot(operatorCount);
    deploymentSnapshots.set(operatorCount, pending);
  }
  const snapshot = await pending;
  const emulator = cloneEmulator(snapshot.emulatorState);
  const lucid = await Lucid(emulator, "Custom");
  const referenceScriptsLucid = await Lucid(emulator, "Custom");
  const primary = snapshot.operators[0];
  if (primary === undefined) {
    throw new Error("Deployment snapshot carries no operators");
  }
  lucid.selectWallet.fromSeed(primary.seedPhrase);
  referenceScriptsLucid.selectWallet.fromSeed(
    snapshot.referenceScriptsSeedPhrase,
  );
  return {
    emulator,
    lucid,
    referenceScriptsLucid,
    referenceScriptsAddress: await referenceScriptsLucid.wallet().address(),
    referenceScriptsSeedPhrase: snapshot.referenceScriptsSeedPhrase,
    contracts: snapshot.contracts,
    operators: snapshot.operators,
    lucidFor: async (keyHash: string) => {
      const account = snapshot.operators.find(
        (operator) => operator.keyHash === keyHash,
      );
      if (account === undefined) {
        throw new Error(`No fixture operator with key hash ${keyHash}`);
      }
      const operatorLucid = await Lucid(emulator, "Custom");
      operatorLucid.selectWallet.fromSeed(account.seedPhrase);
      return operatorLucid;
    },
  };
};

/** The last slot boundary at or before `unixTimeMs`. */
export const alignedUnixTimeAtOrBefore = (
  lucid: LucidEvolution,
  unixTimeMs: bigint,
): bigint => BigInt(alignUnixTimeToSlotBoundary(lucid, Number(unixTimeMs)));

/**
 * Advances the emulator until its clock is strictly after `targetMs`.
 */
export const advanceEmulatorPastUnixTime = (
  emulator: Emulator,
  targetMs: bigint,
): void => {
  const nowMs = BigInt(emulator.now());
  if (nowMs > targetMs) {
    return;
  }
  const slots = Number((targetMs - nowMs) / 1000n) + 2;
  emulator.awaitSlot(slots);
};
