import * as SDK from "@al-ft/midgard-sdk";
import {
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { registerOperatorProgram } from "../src/transactions/register-active-operator.js";
import { withoutFollowerJournal } from "./helpers/intent-journal.js";
import {
  appointFirstSchedulerOperator,
  initOperatorInactivityFixture,
  strikeOperatorToMaxStrikes,
} from "./helpers/operator-inactivity.js";
import {
  BOND_LOVELACE,
  INACTIVITY_SLASHING_PENALTY_LOVELACE,
  initOperatorExitFixture,
  type OperatorExitFixture,
  resolveOperatorExitScriptRefs,
  runProgram,
} from "./operator-exit-emulator.build-operator-exit-snapshot.js";
import {
  alignMs,
  buildRetireTx,
  fetchDirectorySnapshot,
  fundAccount,
  keyHashOf,
  recoverOperatorBond,
  registerAndActivate,
  type RetireFixture,
  retireOperator,
  submitSigned,
  walletLovelace,
} from "./operator-exit-emulator.build-retire-tx.js";
import {
  expectOnChainRefusal,
  fetchRetiredNodes,
} from "./operator-exit-emulator.force-activate-operator.js";

describe("operator exit emulator", () => {
  it("retires an unscheduled operator and returns the bond on recovery", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid, contracts, operatorKeyHash, emulator } = fixture;
    await registerAndActivate({
      operatorLucid: lucid,
      referenceScriptsLucid: fixture.referenceScriptsLucid,
      contracts,
      emulator,
    });

    const beforeRetire = await fetchDirectorySnapshot(lucid, contracts);
    expect(beforeRetire.scheduler.datum).toEqual("NoActiveOperators");
    expect(
      SDK.findNodeByKey(beforeRetire.active, operatorKeyHash),
    ).toBeDefined();

    const { layout } = await buildRetireTx({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
      mode: "voluntary",
    });
    // The scheduler names nobody, so the retirement only has to reference it.
    expect(layout.schedulerSync.kind).toEqual("OperatorIsInactive");

    await retireOperator({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
      mode: "voluntary",
    });

    const afterRetire = await fetchDirectorySnapshot(lucid, contracts);
    expect(
      SDK.findNodeByKey(afterRetire.active, operatorKeyHash),
    ).toBeUndefined();
    const retiredNode = SDK.findNodeByKey(afterRetire.retired, operatorKeyHash);
    if (retiredNode === undefined) {
      throw new Error("Expected a retired-operators node for the operator");
    }
    expect(retiredNode.utxo.assets["lovelace"]).toEqual(BOND_LOVELACE);
    // Activation pins `bond_unlock_time: None` and the retirement copies the
    // removed active node's value verbatim.
    expect(retiredNode.retired?.bond_unlock_time ?? null).toBeNull();
    expect(afterRetire.scheduler.datum).toEqual("NoActiveOperators");

    const status = SDK.deriveOperatorStatus(
      afterRetire,
      operatorKeyHash,
      BigInt(emulator.now()),
      { maxInactivityStrikes: 5n },
    );
    expect(status.state).toEqual("retired");
    expect(status.duplicate).toBe(false);
    expect(status.bondLovelace).toEqual(BOND_LOVELACE);
    expect(status.bondRecoveryAllowedNow).toBe(true);
    expect(status.bondRecoveryAllowedFrom).toBeNull();
    expect(status.holdsShift).toBe(false);

    // A retired key must not be able to register again, and the node has to
    // say so locally instead of building a transaction that gets slashed.
    await expect(
      runProgram(
        withoutFollowerJournal(
          registerOperatorProgram(
            lucid,
            contracts,
            BOND_LOVELACE,
            fixture.referenceScriptsLucid,
          ),
        ),
      ),
    ).rejects.toThrow(/already in the operator directory/);

    const balanceBeforeRecovery = await walletLovelace(lucid);
    await recoverOperatorBond({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
    });
    const balanceAfterRecovery = await walletLovelace(lucid);
    const delta = balanceAfterRecovery - balanceBeforeRecovery;
    expect(delta).toBeLessThanOrEqual(BOND_LOVELACE);
    expect(delta).toBeGreaterThan(BOND_LOVELACE - 5_000_000n);
    expect(await fetchRetiredNodes(lucid, contracts)).toHaveLength(1);
  }, 240_000);

  it("refuses a retirement the operator did not sign, and a forced retirement below the strike limit", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid, contracts, operatorKeyHash, emulator } = fixture;
    await registerAndActivate({
      operatorLucid: lucid,
      referenceScriptsLucid: fixture.referenceScriptsLucid,
      contracts,
      emulator,
    });

    const unsignedRefusal = await expectOnChainRefusal(() =>
      buildRetireTx({
        fixture,
        submitterLucid: lucid,
        operatorKeyHash,
        mode: "voluntary",
        overrides: { requireOperatorSignature: false },
      }),
    );
    expect(unsignedRefusal).toContain("operator retirement tx");

    // Strikes are 0 on a freshly activated node, so the penalized route must
    // be refused on chain even though its fee is exactly the penalty. A forced
    // retirement is permissionless, so a stranger submits it from one coin,
    // which backs the collateral and funds the min-ADA top-ups.
    const forcedSubmitter = generateEmulatorAccount({ lovelace: 0n });
    await fundAccount(lucid, forcedSubmitter.address, 500_000_000n);
    const forcedSubmitterLucid = await Lucid(emulator, "Custom");
    forcedSubmitterLucid.selectWallet.fromSeed(forcedSubmitter.seedPhrase);
    const forcedRefusal = await expectOnChainRefusal(() =>
      buildRetireTx({
        fixture,
        submitterLucid: forcedSubmitterLucid,
        operatorKeyHash,
        mode: "forced-inactivity",
      }),
    );
    expect(forcedRefusal).toContain("operator retirement tx");

    // The operator is still active: neither refusal changed the directory.
    const snapshot = await fetchDirectorySnapshot(lucid, contracts);
    expect(SDK.findNodeByKey(snapshot.active, operatorKeyHash)).toBeDefined();
    expect(snapshot.retired).toHaveLength(1);
  }, 240_000);

  // The forced (partially slashed) retirement only succeeds once an active
  // node carries `inactivity_strikes >= 5`, which is reachable only through
  // the active-operators `StrikeForInactivity` spend path — so this case runs
  // on the inactivity fixture, whose helpers drive that path.
  it("retires a struck operator without its signature, paying exactly the inactivity penalty", async () => {
    const inactivity = await initOperatorInactivityFixture(1);
    const scriptRefs = await resolveOperatorExitScriptRefs(
      inactivity.referenceScriptsLucid,
      inactivity.contracts,
    );
    const fixture: RetireFixture = {
      emulator: inactivity.emulator,
      contracts: inactivity.contracts,
      scriptRefs,
    };
    const { contracts } = inactivity;
    await appointFirstSchedulerOperator(inactivity);
    const operatorKeyHash = inactivity.operators[0]?.keyHash;
    if (operatorKeyHash === undefined) {
      throw new Error("The inactivity fixture carries no operator");
    }
    const { inactivityStrikes } = await strikeOperatorToMaxStrikes(
      inactivity,
      operatorKeyHash,
    );
    expect(inactivityStrikes).toBeGreaterThanOrEqual(
      SDK.MAX_INACTIVITY_STRIKES,
    );

    // At the cap the operator may no longer leave on its own terms.
    const cappedRefusal = await expectOnChainRefusal(() =>
      retireOperator({
        fixture,
        submitterLucid: inactivity.lucid,
        operatorKeyHash,
        mode: "voluntary",
      }),
    );
    expect(cappedRefusal).toContain("operator retirement tx");

    // One coin: it backs the collateral and covers the retired list's min-ADA
    // top-ups, since collateral is only taken when phase-2 validation fails.
    const stranger = generateEmulatorAccount({ lovelace: 0n });
    await fundAccount(inactivity.lucid, stranger.address, 500_000_000n);
    const strangerLucid = await Lucid(inactivity.emulator, "Custom");
    strangerLucid.selectWallet.fromSeed(stranger.seedPhrase);

    const beforeRetire = await fetchDirectorySnapshot(strangerLucid, contracts);
    const activeNode = SDK.findNodeByKey(beforeRetire.active, operatorKeyHash);
    if (activeNode?.active === undefined || activeNode.active === null) {
      throw new Error("Expected the struck operator to still be active");
    }

    // Nobody signs for the operator: a forced retirement is permissionless.
    const validFrom = alignMs(strangerLucid, BigInt(inactivity.emulator.now()));
    const { tx } = await buildRetireTx({
      fixture,
      submitterLucid: strangerLucid,
      operatorKeyHash,
      mode: "forced-inactivity",
      validity: { validFrom, validTo: validFrom + 120_000n },
    });
    expect(tx.toTransaction().body().fee()).toEqual(
      INACTIVITY_SLASHING_PENALTY_LOVELACE,
    );
    await submitSigned(strangerLucid, tx);

    const afterRetire = await fetchDirectorySnapshot(strangerLucid, contracts);
    expect(
      SDK.findNodeByKey(afterRetire.active, operatorKeyHash),
    ).toBeUndefined();
    const retiredNode = SDK.findNodeByKey(afterRetire.retired, operatorKeyHash);
    if (retiredNode?.retired === undefined || retiredNode.retired === null) {
      throw new Error("Expected the struck operator to be retired");
    }
    // The penalty is kept back from the bond: 900 ADA in, 800 ADA parked.
    expect(retiredNode.utxo.assets["lovelace"]).toEqual(
      BOND_LOVELACE - INACTIVITY_SLASHING_PENALTY_LOVELACE,
    );
    expect(retiredNode.retired.bond_unlock_time).toEqual(
      activeNode.active.bond_unlock_time,
    );
  }, 600_000);

  // `bond_unlock_time: Some(_)` is unreachable from the operator-exit
  // endpoints alone: `ActivateOperator` pins the inserted active node to
  // `bond_unlock_time: None`, and both `RetireOperator` handlers copy that
  // value verbatim and cross-check it. A hold is only ever written by the
  // active-operators `UpdateBondHoldNewState` spend (which requires a
  // state-queue `CommitBlockHeader` mint) or `UpdateBondHoldNewSettlement`
  // (which requires a settlement `AttachResolutionClaim` spend). Setup once
  // either is available here: commit a block as the operator so its active
  // node carries a hold, retire it, then attempt recovery inside the hold and
  // expect the `is_entirely_after(bond_unlock_time)` check to refuse it.
  it.todo("refuses bond recovery before the bond unlock time");

  it("refuses bond recovery that the retired operator did not sign", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid, contracts, operatorKeyHash, emulator } = fixture;
    await registerAndActivate({
      operatorLucid: lucid,
      referenceScriptsLucid: fixture.referenceScriptsLucid,
      contracts,
      emulator,
    });
    await retireOperator({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
      mode: "voluntary",
    });

    const stranger = generateEmulatorAccount({ lovelace: 0n });
    await fundAccount(lucid, stranger.address, 2_000_000_000n);
    const strangerLucid = await Lucid(emulator, "Custom");
    strangerLucid.selectWallet.fromSeed(stranger.seedPhrase);

    const refusal = await expectOnChainRefusal(() =>
      recoverOperatorBond({
        fixture,
        submitterLucid: strangerLucid,
        operatorKeyHash,
        overrides: { requireOperatorSignature: false },
      }),
    );
    expect(refusal).toContain("operator bond recovery tx");

    const snapshot = await fetchDirectorySnapshot(lucid, contracts);
    expect(SDK.findNodeByKey(snapshot.retired, operatorKeyHash)).toBeDefined();
  }, 240_000);
});

/** A funded wallet that is neither an operator nor the reference-script payer. */
export const newFundedWallet = async (
  fixture: OperatorExitFixture,
  lovelace: bigint,
): Promise<LucidEvolution> => {
  const account = generateEmulatorAccount({ lovelace: 0n });
  await fundAccount(fixture.lucid, account.address, lovelace);
  const wallet = await Lucid(fixture.emulator, "Custom");
  wallet.selectWallet.fromSeed(account.seedPhrase);
  return wallet;
};

/** A second, fully activated operator on the same fixture. */
export const addActivatedOperator = async (
  fixture: OperatorExitFixture,
): Promise<{
  readonly lucid: LucidEvolution;
  readonly operatorKeyHash: string;
}> => {
  const operatorLucid = await newFundedWallet(fixture, 3_000_000_000n);
  await registerAndActivate({
    operatorLucid,
    referenceScriptsLucid: fixture.referenceScriptsLucid,
    contracts: fixture.contracts,
    emulator: fixture.emulator,
  });
  return {
    lucid: operatorLucid,
    operatorKeyHash: await keyHashOf(operatorLucid),
  };
};
