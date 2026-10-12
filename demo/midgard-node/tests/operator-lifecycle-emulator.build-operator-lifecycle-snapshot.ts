import * as SDK from "@al-ft/midgard-sdk";
import { createReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  paymentCredentialOf,
  PROTOCOL_PARAMETERS_DEFAULT,
  toUnit,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { IntentJournalWithoutFollower } from "../src/services/intent-journal.js";
import {
  buildAtomicProtocolInitTxProgram,
  ensureAtomicProtocolInitReferenceScriptsProgram,
} from "../src/transactions/initialization.js";
import { ensureEventHistoryRewardAccountsRegisteredProgram } from "../src/transactions/script-reward-registration.js";
import {
  type CaptureSnapshot,
  restoreCapture,
  snapshotCapture,
} from "./helpers/emulator-chain-capture.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

export const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: 65_536,
  maxCollateralInputs: 3,
} as const;

// Keep fragmented UTxOs large enough so Lucid's default collateral selector
// can satisfy collateral + collateral-return constraints within max inputs (3).
const MIN_COLLATERAL_SAFE_FRAGMENT_LOVELACE = 2_300_000n;

// Wave-current on-chain bond. `operator-directory/registered-operators.ak` now
// enforces `registered_node_lovelace == env.required_bond` (it used to accept
// `>=`), and `env/testnet.ak` — the env this blueprint is built with, matching
// `.github/workflows/midgard-node-ci.yml` — sets
// `required_bond = slashing_penalty (500_000_000) + fraud_prover_reward
// (400_000_000)`. `SDK.getProtocolParameters` carries the same 900_000_000n for
// every non-mainnet profile. Any other value now makes the registration mint
// crash, so this constant is derived from the contract, not chosen.
export const EMULATOR_REQUIRED_BOND_LOVELACE = 900_000_000n;

const EMPTY_FRAUD_PROOF_CATALOGUE_ROOT = "00".repeat(32);

export const EMULATOR_REFERENCE_SCRIPT_AUTH_TIMELOCK_MS = 24 * 60 * 60 * 1000;

export const loadOperatorContracts = (
  oneShotOutRef: {
    txHash: string;
    outputIndex: number;
  },
  referenceScriptAuth: SDK.MintingValidator,
) => loadRealMidgardContractsForTest(oneShotOutRef, referenceScriptAuth);

const buildOperatorAwareInitializationTx = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  referenceScriptsLucid: Awaited<ReturnType<typeof Lucid>>,
  contracts: SDK.MidgardValidators,
  nonceUtxo: UTxO,
  operatorSeedPhrase: string,
) => {
  const referenceScripts = await runWithoutFollower(
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
  return Effect.runPromise(
    buildAtomicProtocolInitTxProgram(
      lucid,
      contracts,
      {
        HUB_ORACLE_ONE_SHOT_TX_HASH: nonceUtxo.txHash,
        HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: nonceUtxo.outputIndex,
        L1_OPERATOR_SEED_PHRASE: operatorSeedPhrase,
        NETWORK: "Preprod",
      },
      EMPTY_FRAUD_PROOF_CATALOGUE_ROOT,
      undefined,
      referenceScripts,
    ).pipe(Effect.provide(IntentJournalWithoutFollower)),
  );
};

type OperatorLifecycleSnapshot = {
  readonly operatorSeedPhrase: string;
  readonly referenceScriptsSeedPhrase: string;
  readonly emulatorState: Pick<
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
  readonly contracts: SDK.MidgardValidators;
  readonly operatorKeyHash: string;
  readonly activeNodeUnit: string;
};

const snapshotEmulator = (
  emulator: Emulator,
): OperatorLifecycleSnapshot["emulatorState"] => ({
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

const cloneEmulator = (
  snapshot: OperatorLifecycleSnapshot["emulatorState"],
): Emulator => {
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

/**
 * Builds the expensive authenticated protocol deployment exactly once. Every
 * test receives a deep-cloned emulator ledger and fresh Lucid instances, so
 * transaction history, wallet churn, slots, datums and stake state cannot
 * bleed between scenarios.
 */
const buildOperatorLifecycleSnapshot =
  async (): Promise<OperatorLifecycleSnapshot> => {
    const operator = generateEmulatorAccount({
      lovelace: 30_000_000_000n,
    });
    // Fund the complete canonical reference registry and its remaining
    // publication reserve without drawing on the operator's pinned nonce.
    const referenceScripts = generateEmulatorAccount({
      lovelace: 200_000_000_000n,
    });
    const emulator = new Emulator(
      [operator, referenceScripts],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const lucid = await Lucid(emulator, "Custom");
    const referenceScriptsLucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(operator.seedPhrase);
    referenceScriptsLucid.selectWallet.fromSeed(referenceScripts.seedPhrase);

    const nonceUtxo = (await lucid.wallet().getUtxos())[0];
    if (!nonceUtxo) {
      throw new Error("Expected at least one wallet UTxO in emulator");
    }
    const referenceScriptAuth = await createReferenceScriptAuthPolicy(
      referenceScriptsLucid,
      emulator.now(),
      EMULATOR_REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
    );
    const contracts = await loadOperatorContracts(
      {
        txHash: nonceUtxo.txHash,
        outputIndex: nonceUtxo.outputIndex,
      },
      referenceScriptAuth,
    );
    const initTx = await buildOperatorAwareInitializationTx(
      lucid,
      referenceScriptsLucid,
      contracts,
      nonceUtxo,
      operator.seedPhrase,
    );
    const initCompleted = await initTx.complete({ localUPLCEval: true });
    const initSigned = await initCompleted.sign.withWallet().complete();
    const initTxHash = await initSigned.submit();
    await lucid.awaitTx(initTxHash);

    const operatorAddress = await lucid.wallet().address();
    const paymentCredential = paymentCredentialOf(operatorAddress);
    if (paymentCredential?.type !== "Key") {
      throw new Error("Expected operator wallet payment credential to be Key");
    }
    const operatorKeyHash = paymentCredential.hash;

    const activeNodeUnit = toUnit(
      contracts.activeOperators.policyId,
      SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorKeyHash,
    );

    return {
      operatorSeedPhrase: operator.seedPhrase,
      referenceScriptsSeedPhrase: referenceScripts.seedPhrase,
      emulatorState: snapshotEmulator(emulator),
      contracts,
      operatorKeyHash,
      activeNodeUnit,
    };
  };

export let operatorLifecycleSnapshotPromise:
  | Promise<OperatorLifecycleSnapshot>
  | undefined;

const getOperatorLifecycleSnapshot = (): Promise<OperatorLifecycleSnapshot> => {
  operatorLifecycleSnapshotPromise ??= buildOperatorLifecycleSnapshot();
  return operatorLifecycleSnapshotPromise;
};

export const initOperatorLifecycleFixture = async () => {
  const snapshot = await getOperatorLifecycleSnapshot();
  const emulator = cloneEmulator(snapshot.emulatorState);
  const lucid = await Lucid(emulator, "Custom");
  const referenceScriptsLucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(snapshot.operatorSeedPhrase);
  referenceScriptsLucid.selectWallet.fromSeed(
    snapshot.referenceScriptsSeedPhrase,
  );

  return {
    emulator,
    lucid,
    referenceScriptsLucid,
    operatorSeedPhrase: snapshot.operatorSeedPhrase,
    referenceScriptsSeedPhrase: snapshot.referenceScriptsSeedPhrase,
    contracts: snapshot.contracts,
    operatorKeyHash: snapshot.operatorKeyHash,
    activeNodeUnit: snapshot.activeNodeUnit,
  };
};

export const reconcileLiveWalletUtxos = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  utxos: readonly UTxO[],
): Promise<readonly UTxO[]> => {
  if (utxos.length === 0) {
    return [];
  }
  const uniqueOutRefs = Array.from(
    new Map(
      utxos.map((utxo) => [
        `${utxo.txHash}#${utxo.outputIndex.toString()}`,
        {
          txHash: utxo.txHash,
          outputIndex: utxo.outputIndex,
        },
      ]),
    ).values(),
  );
  return lucid.utxosByOutRef(uniqueOutRefs);
};

export const fragmentOperatorWalletUtxos = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  {
    outputs,
    lovelacePerOutput,
  }: {
    outputs: number;
    lovelacePerOutput: bigint;
  },
) => {
  if (outputs <= 0) {
    throw new Error("fragmentOperatorWalletUtxos requires outputs > 0");
  }
  const effectiveLovelacePerOutput =
    lovelacePerOutput < MIN_COLLATERAL_SAFE_FRAGMENT_LOVELACE
      ? MIN_COLLATERAL_SAFE_FRAGMENT_LOVELACE
      : lovelacePerOutput;
  const operatorAddress = await lucid.wallet().address();
  const liveWalletInputs = await reconcileLiveWalletUtxos(
    lucid,
    await lucid.wallet().getUtxos(),
  ).then((utxos) => utxos.filter((utxo) => utxo.scriptRef === undefined));
  let tx = lucid.newTx();
  for (let index = 0; index < outputs; index += 1) {
    tx = tx.pay.ToAddress(operatorAddress, {
      lovelace: effectiveLovelacePerOutput,
    });
  }
  const completed = await tx.complete({
    localUPLCEval: true,
    presetWalletInputs: [...liveWalletInputs],
  });
  const signed = await completed.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/**
 * Builds a deterministic random-number generator for repeatable tests.
 */
export const mkDeterministicRng = (seed: number) => {
  let state = seed >>> 0;
  return () => {
    state = (state * 1664525 + 1013904223) >>> 0;
    return state / 0x1_0000_0000;
  };
};
