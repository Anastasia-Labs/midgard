/**
 * An availability-challenge timeout and an operator-directory change go in
 * separate transactions. Composed into one, the timeout withdrawal refuses;
 * submitted apart, both land in the same block.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  Data,
  type EmulatorAccount,
  generateEmulatorAccountFromPrivateKey,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { buildTimeoutMirror } from "./availability-challenge-pool-slash-lifecycle.complete-timeout-mirror.js";
import {
  attestAndOpen,
  expireAndSettle,
  snapshot,
  timeoutParams,
  timeoutYieldWithdrawIndex,
} from "./availability-challenge-pool-slash-lifecycle.submit-built.js";
import {
  assertAvailabilityRefusal,
  availabilityDeployment,
  type AvailabilityFixture,
  createAvailabilityFixture,
} from "./helpers/availability-challenge-emulator.js";
import {
  BOND_LOVELACE,
  INACTIVITY_SLASHING_PENALTY_LOVELACE,
  SLASHING_PENALTY_LOVELACE,
} from "./operator-exit-emulator.build-operator-exit-snapshot.js";

const DIRECTORY_ROOT_LOVELACE = 5_000_000n;

/** An active operator with every strike, so anyone may retire it. */
const OPERATOR_KEY_HASH = "0f".repeat(28);

const MAX_INACTIVITY_STRIKES = 5n;

const utxoAt = (
  address: string,
  assets: Assets,
  datum: string,
): EmulatorAccount => ({
  seedPhrase: "",
  privateKey: "",
  address,
  assets,
  outputData: { inline: datum },
});

const rootDatum = (next: SDK.LinkedListNodeView["next"]) =>
  SDK.encodeLinkedListNodeView({ key: "Empty", next, data: "" });

/**
 * The active and retired directories and the scheduler: one active operator
 * at the bond with every strike, an empty retired list, and a scheduler that
 * names nobody.
 */
const directoryGenesis = (
  contracts: SDK.MidgardValidators,
): readonly EmulatorAccount[] => {
  const { activeOperators, retiredOperators, scheduler } = contracts;
  const operatorKey = { Key: { key: OPERATOR_KEY_HASH } };
  return [
    utxoAt(
      activeOperators.spendingScriptAddress,
      {
        lovelace: DIRECTORY_ROOT_LOVELACE,
        [toUnit(
          activeOperators.policyId,
          SDK.ACTIVE_OPERATORS_ROOT_ASSET_NAME,
        )]: 1n,
      },
      rootDatum(operatorKey),
    ),
    utxoAt(
      activeOperators.spendingScriptAddress,
      {
        lovelace: BOND_LOVELACE,
        [SDK.activeOperatorNodeUnit(
          activeOperators.policyId,
          OPERATOR_KEY_HASH,
        )]: 1n,
      },
      SDK.encodeLinkedListNodeView({
        key: operatorKey,
        next: "Empty",
        data: SDK.castActiveOperatorDatumToData({
          bond_unlock_time: null,
          inactivity_strikes: MAX_INACTIVITY_STRIKES,
        }) as SDK.LinkedListNodeView["data"],
      }),
    ),
    utxoAt(
      retiredOperators.spendingScriptAddress,
      {
        lovelace: DIRECTORY_ROOT_LOVELACE,
        [toUnit(
          retiredOperators.policyId,
          SDK.RETIRED_OPERATORS_ROOT_ASSET_NAME,
        )]: 1n,
      },
      rootDatum("Empty"),
    ),
    utxoAt(
      scheduler.spendingScriptAddress,
      {
        lovelace: DIRECTORY_ROOT_LOVELACE,
        [toUnit(scheduler.policyId, SDK.SCHEDULER_ASSET_NAME)]: 1n,
      },
      Data.to("NoActiveOperators", SDK.SchedulerDatum),
    ),
  ];
};

const directoryReferenceScripts = (
  contracts: SDK.MidgardValidators,
): readonly SDK.ReferenceScriptTarget[] => [
  {
    name: "active-operators spending",
    script: contracts.activeOperators.spendingScript,
  },
  {
    name: "active-operators minting",
    script: contracts.activeOperators.mintingScript,
  },
  {
    name: "retired-operators spending",
    script: contracts.retiredOperators.spendingScript,
  },
  {
    name: "retired-operators minting",
    script: contracts.retiredOperators.mintingScript,
  },
];

const publications = (f: AvailabilityFixture, names: readonly string[]) =>
  names.map((name) => ({ name, utxo: f.reference(name) }));

/** The forced retirement of the operator, inside `[validFrom, validTo)`. */
const forcedRetirementConfig = async (
  f: AvailabilityFixture,
  validFrom: bigint,
  validTo: bigint,
): Promise<SDK.RetireOperatorTxConfig> => {
  const directory = await Effect.runPromise(
    SDK.fetchOperatorDirectorySnapshotProgram(f.lucid, f.contracts),
  );
  const witnesses = SDK.deriveRetireOperatorWitnesses({
    snapshot: directory,
    contracts: f.contracts,
    operatorKeyHash: OPERATOR_KEY_HASH,
    validTo,
  });
  expect(witnesses.schedulerSync.kind).toBe("OperatorIsInactive");
  return {
    lucid: f.lucid,
    contracts: f.contracts,
    operatorKeyHash: OPERATOR_KEY_HASH,
    activeOperatorScriptRefs: publications(f, [
      "active-operators spending",
      "active-operators minting",
    ]),
    retiredOperatorScriptRefs: publications(f, [
      "retired-operators spending",
      "retired-operators minting",
    ]),
    hubOracleRefInput: directory.hubOracle.utxo,
    activeNode: witnesses.activeNode,
    activeAnchor: witnesses.activeAnchor,
    retiredInsertionAnchor: witnesses.retiredInsertionAnchor,
    activeNodeUnit: witnesses.activeNodeUnit,
    retiredNodeUnit: witnesses.retiredNodeUnit,
    bondUnlockTime: witnesses.bondUnlockTime,
    retiredNodeLovelace: SDK.retiredOperatorBondTranche("forced-inactivity", {
      requiredBondLovelace: BOND_LOVELACE,
      inactivitySlashingPenaltyLovelace: INACTIVITY_SLASHING_PENALTY_LOVELACE,
    }),
    mode: "forced-inactivity",
    inactivitySlashingPenaltyLovelace: INACTIVITY_SLASHING_PENALTY_LOVELACE,
    schedulerSync: witnesses.schedulerSync,
    validFrom,
    validTo,
  };
};

const directoryState = async (f: AvailabilityFixture) => {
  const directory = await Effect.runPromise(
    SDK.fetchOperatorDirectorySnapshotProgram(f.lucid, f.contracts),
  );
  return {
    active: SDK.findNodeByKey(directory.active, OPERATOR_KEY_HASH),
    retired: SDK.findNodeByKey(directory.retired, OPERATOR_KEY_HASH),
  };
};

describe("availability-challenge timeout beside an operator directory change", () => {
  it("refuses a timeout composed with a forced retirement, and lands both apart in one block", async () => {
    const submitter = generateEmulatorAccountFromPrivateKey({
      lovelace: 1_000_000_000n,
    });
    const f = await createAvailabilityFixture(1, 0, 0, {
      genesis: (contracts) => [submitter, ...directoryGenesis(contracts)],
      referenceScriptTargets: directoryReferenceScripts,
    });
    const d = availabilityDeployment(f);
    const s = await attestAndOpen(f, d);
    const terminal = await expireAndSettle(f, d, s);
    const pool = await f.getPool();
    const slash = SDK.planDaBondPoolSlash({
      poolLovelace: pool.assets.lovelace,
      parameters: f.parameters,
    });
    expect(slash.feePart).toBe(INACTIVITY_SLASHING_PENALTY_LOVELACE);
    expect(slash.feePart).toBeLessThan(SLASHING_PENALTY_LOVELACE);
    const remaining = Data.from(
      terminal.datum!,
      SDK.DaAvailabilityTerminalAccumulatorDatum,
    ).remaining_challenger_lovelace;
    const validFrom = BigInt(f.emulator.now());
    const validTo = validFrom + 60_000n;
    const retirement = await forcedRetirementConfig(f, validFrom, validTo);

    f.lucid.selectWallet.fromPrivateKey(f.challenger.privateKey);
    const bundle = buildTimeoutMirror(
      f,
      s,
      terminal,
      {
        pool,
        poolOutputLovelace: slash.poolOutputLovelace,
        challengerOutputLovelace:
          remaining + f.parameters.challenge_record_lovelace + slash.payout,
      },
      {
        inputs: [
          retirement.activeAnchor.utxo,
          retirement.activeNode.utxo,
          retirement.retiredInsertionAnchor.utxo,
        ],
        references: [
          ...retirement.activeOperatorScriptRefs.map(({ utxo }) => utxo),
          ...retirement.retiredOperatorScriptRefs.map(({ utxo }) => utxo),
          retirement.hubOracleRefInput,
          ...(retirement.schedulerSync.kind === "OperatorIsInactive"
            ? [retirement.schedulerSync.schedulerRefInput]
            : []),
        ],
        policies: [
          f.contracts.activeOperators.policyId,
          f.contracts.retiredOperators.policyId,
        ],
      },
    )
      .compose(SDK.buildRetireOperatorTx(retirement))
      .pay.ToAddress(f.responder.address, {
        lovelace: INACTIVITY_SLASHING_PENALTY_LOVELACE,
      })
      .setMinFee(slash.feePart);
    await assertAvailabilityRefusal(
      bundle.complete({ coinSelection: false, localUPLCEval: true }),
      {
        purpose: "withdraw",
        script: "availability-challenge timeout withdrawal",
        index: timeoutYieldWithdrawIndex(f),
      },
      f.scriptNames,
    );
    expect((await directoryState(f)).active).toBeDefined();

    // Apart, both land in the same block.
    const built = await Effect.runPromise(
      SDK.buildTimeoutDaAvailabilityChallengeTxProgram(
        f.lucid,
        d,
        await timeoutParams(f, s, terminal, pool),
      ),
    );
    const timeout = await built.tx.sign.withWallet().complete();
    f.lucid.selectWallet.fromPrivateKey(submitter.privateKey);
    const { tx } = await Effect.runPromise(
      SDK.buildUnsignedRetireOperatorTxProgram({
        ...retirement,
        walletInputs: await f.lucid.wallet().getUtxos(),
      }),
    );
    const retire = await tx.sign.withWallet().complete();
    const ids = [await timeout.submit(), await retire.submit()];
    const height = f.emulator.blockHeight;
    f.emulator.awaitBlock(1);
    expect(f.emulator.blockHeight).toBe(height + 1);
    const landed: UTxO[] = await f.lucid.utxosByOutRef(
      ids.map((txHash) => ({ txHash, outputIndex: 0 })),
    );
    expect(landed).toHaveLength(2);
    expect((await snapshot(f, d)).record).toBeUndefined();
    const after = await directoryState(f);
    expect(after.active).toBeUndefined();
    expect(after.retired?.utxo.assets.lovelace).toBe(
      BOND_LOVELACE - INACTIVITY_SLASHING_PENALTY_LOVELACE,
    );
  }, 300_000);
});
