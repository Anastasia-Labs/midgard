import { inspect } from "node:util";

import * as SDK from "@al-ft/midgard-sdk";
import { createReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  paymentCredentialOf,
  PROTOCOL_PARAMETERS_DEFAULT,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Cause, Effect, Exit } from "effect";

import { IntentJournalWithoutFollower } from "../src/services/intent-journal.js";
import {
  buildAtomicProtocolInitTxProgram,
  ensureAtomicProtocolInitReferenceScriptsProgram,
} from "../src/transactions/initialization.js";
import {
  referenceScriptTargetsByCommand,
  resolveReferenceScriptTargetsProgram,
} from "../src/transactions/reference-scripts.js";
import { ensureEventHistoryRewardAccountsRegisteredProgram } from "../src/transactions/script-reward-registration.js";
import { withoutFollowerJournal } from "./helpers/intent-journal.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: 65_536,
  maxCollateralInputs: 3,
} as const;

// `env/testnet.ak` — the env this blueprint is built with — sets
// `required_bond = slashing_penalty (500 ADA) + fraud_prover_reward (400 ADA)`,
// `inactivity_slashing_penalty = 100 ADA` and `max_inactivity_strikes = 5`.
// `registered-operators.ak` enforces the bond exactly, so these are derived
// from the contract, not chosen.
export const BOND_LOVELACE = 900_000_000n;

export const SLASHING_PENALTY_LOVELACE = 500_000_000n;

export const INACTIVITY_SLASHING_PENALTY_LOVELACE = 100_000_000n;

export const { REGISTRATION_DURATION_MS, SHIFT_DURATION_MS } = SDK;

// `advanceShiftToPredecessor` opens its range 30 s past the shift end and
// keeps it open 8 minutes; waiting out the shift plus 200 slots lands inside.
export const PAST_SHIFT_END_SLOTS = Number(SHIFT_DURATION_MS / 1000n) + 200;

const EMPTY_FRAUD_PROOF_CATALOGUE_ROOT = "00".repeat(32);

const EMULATOR_REFERENCE_SCRIPT_AUTH_TIMELOCK_MS = 24 * 60 * 60 * 1000;

const OPERATOR_EXIT_REFERENCE_SCRIPT_COMMANDS = [
  "scheduler",
  "registered-operators",
  "active-operators",
  "retired-operators",
] as const;

type OperatorExitSnapshot = {
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
  >;
  readonly contracts: SDK.MidgardValidators;
  readonly operatorKeyHash: string;
};

/**
 * Flattens an error value and its `cause` chain into one line. Effect's own
 * `FiberFailure` inspection stops at `[Object]`, which hides the on-chain
 * evaluation text these tests assert on.
 */
export const describeFailure = (value: unknown): string => {
  if (typeof value === "string") {
    return value;
  }
  if (typeof value === "object" && value !== null) {
    const record = value as Record<string, unknown>;
    // A nested `FiberFailure` (a program run inside another program) and a raw
    // `Cause` both hide their payload from `inspect`.
    if (record["_id"] === "FiberFailure") {
      return describeFailure(record["cause"]);
    }
    if (record["_id"] === "Cause") {
      const inner = value as Cause.Cause<unknown>;
      return (
        [
          ...Array.from(Cause.failures(inner)).map(describeFailure),
          ...Array.from(Cause.defects(inner)).map(describeFailure),
        ].join("; ") || Cause.pretty(inner)
      );
    }
    const parts: string[] = [];
    if (typeof record["message"] === "string") {
      parts.push(record["message"]);
    }
    if (record["cause"] !== undefined) {
      parts.push(describeFailure(record["cause"]));
    }
    if (parts.length > 0) {
      return parts.join(" | ");
    }
  }
  return inspect(value, { depth: 8 });
};

/** Runs an Effect program, reporting failures through {@link describeFailure}. */
export const runProgram = async <A, E>(
  program: Effect.Effect<A, E, never>,
): Promise<A> => {
  const exit = await Effect.runPromiseExit(program);
  if (Exit.isSuccess(exit)) {
    return exit.value;
  }
  const flattened =
    [
      ...Array.from(Cause.failures(exit.cause)).map(describeFailure),
      ...Array.from(Cause.defects(exit.cause)).map(describeFailure),
    ].join("; ") || Cause.pretty(exit.cause);
  throw new Error(flattened);
};

const snapshotEmulator = (
  emulator: Emulator,
): OperatorExitSnapshot["emulatorState"] => ({
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
});

const cloneEmulator = (
  snapshot: OperatorExitSnapshot["emulatorState"],
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
  return emulator;
};

/**
 * The authenticated protocol deployment is built exactly once; every test
 * clones the resulting ledger so slots, wallets and transaction history cannot
 * bleed between scenarios.
 */
const buildOperatorExitSnapshot = async (): Promise<OperatorExitSnapshot> => {
  const operator = generateEmulatorAccount({ lovelace: 30_000_000_000n });
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
  const referenceScriptPublications = await runProgram(
    withoutFollowerJournal(
      ensureAtomicProtocolInitReferenceScriptsProgram(
        referenceScriptsLucid,
        contracts,
      ),
    ),
  );
  await runProgram(
    withoutFollowerJournal(
      ensureEventHistoryRewardAccountsRegisteredProgram(
        referenceScriptsLucid,
        contracts,
      ),
    ),
  );
  const initTx = await runProgram(
    buildAtomicProtocolInitTxProgram(
      lucid,
      contracts,
      {
        HUB_ORACLE_ONE_SHOT_TX_HASH: nonceUtxo.txHash,
        HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: nonceUtxo.outputIndex,
        L1_OPERATOR_SEED_PHRASE: operator.seedPhrase,
        NETWORK: "Preprod",
      },
      EMPTY_FRAUD_PROOF_CATALOGUE_ROOT,
      undefined,
      referenceScriptPublications,
    ).pipe(Effect.provide(IntentJournalWithoutFollower)),
  );
  const initCompleted = await initTx.complete({ localUPLCEval: true });
  const initSigned = await initCompleted.sign.withWallet().complete();
  await lucid.awaitTx(await initSigned.submit());

  const paymentCredential = paymentCredentialOf(await lucid.wallet().address());
  if (paymentCredential?.type !== "Key") {
    throw new Error("Expected operator wallet payment credential to be Key");
  }

  return {
    operatorSeedPhrase: operator.seedPhrase,
    referenceScriptsSeedPhrase: referenceScripts.seedPhrase,
    emulatorState: snapshotEmulator(emulator),
    contracts,
    operatorKeyHash: paymentCredential.hash,
  };
};

export let operatorExitSnapshotPromise:
  | Promise<OperatorExitSnapshot>
  | undefined;

const getOperatorExitSnapshot = (): Promise<OperatorExitSnapshot> => {
  operatorExitSnapshotPromise ??= buildOperatorExitSnapshot();
  return operatorExitSnapshotPromise;
};

export type OperatorExitFixture = {
  readonly emulator: Emulator;
  readonly lucid: LucidEvolution;
  readonly referenceScriptsLucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
  readonly operatorKeyHash: string;
  readonly scriptRefs: OperatorExitScriptRefs;
};

export type OperatorExitScriptRefs = {
  readonly scheduler: readonly SDK.ReferenceScriptPublication[];
  readonly registeredOperators: readonly SDK.ReferenceScriptPublication[];
  readonly activeOperators: readonly SDK.ReferenceScriptPublication[];
  readonly retiredOperators: readonly SDK.ReferenceScriptPublication[];
  readonly schedulerSpending: UTxO;
};

export const resolveOperatorExitScriptRefs = async (
  referenceScriptsLucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Promise<OperatorExitScriptRefs> => {
  const targetsByCommand = referenceScriptTargetsByCommand(contracts);
  const resolved = await runProgram(
    resolveReferenceScriptTargetsProgram(
      referenceScriptsLucid,
      "operator exit",
      OPERATOR_EXIT_REFERENCE_SCRIPT_COMMANDS.flatMap(
        (command) => targetsByCommand[command],
      ),
      contracts.referenceScriptAuth,
    ),
  );
  const byPrefix = (
    prefix: string,
  ): readonly SDK.ReferenceScriptPublication[] =>
    resolved.filter(({ name }) => name.startsWith(prefix));
  const schedulerSpending = resolved.find(
    ({ name }) => name === "scheduler spending",
  );
  if (schedulerSpending === undefined) {
    throw new Error("Missing the published scheduler spending script");
  }
  return {
    scheduler: byPrefix("scheduler "),
    registeredOperators: byPrefix("registered-operators "),
    activeOperators: byPrefix("active-operators "),
    retiredOperators: byPrefix("retired-operators "),
    schedulerSpending: schedulerSpending.utxo,
  };
};

export const initOperatorExitFixture =
  async (): Promise<OperatorExitFixture> => {
    const snapshot = await getOperatorExitSnapshot();
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
      contracts: snapshot.contracts,
      operatorKeyHash: snapshot.operatorKeyHash,
      scriptRefs: await resolveOperatorExitScriptRefs(
        referenceScriptsLucid,
        snapshot.contracts,
      ),
    };
  };
