import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  FRAUD_PROVER_REWARD_LOVELACE,
  initOperatorExitFixture,
} from "./operator-exit-emulator.build-operator-exit-snapshot.js";
import {
  bountyAddressUtxos,
  exitValidityWindow,
  fetchDirectorySnapshot,
  keyHashOf,
  recoverOperatorBond,
  registerAndActivate,
  retireOperator,
} from "./operator-exit-emulator.build-retire-tx.js";
import {
  completeExactFee,
  duplicateSlashConfig,
  expectOnChainRefusal,
  forceRegisterOperator,
  slashDuplicateOperator,
  slashingPenaltyFeePlan,
} from "./operator-exit-emulator.force-activate-operator.js";
import { newFundedWallet } from "./operator-exit-emulator.operator-exit-emulator.js";

describe("one operator slash per transaction", () => {
  it("refuses a duplicate-registration slash sharing its transaction with a bond recovery, and accepts each alone", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid, contracts, operatorKeyHash, emulator, scriptRefs } = fixture;
    // The submitter is a retired operator whose bond is recoverable now, and
    // claims the bounty for another key's duplicate registration.
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
    const duplicateLucid = await newFundedWallet(fixture, 3_000_000_000n);
    const duplicateKeyHash = await keyHashOf(duplicateLucid);
    const register = () =>
      forceRegisterOperator({
        fixture,
        operatorLucid: duplicateLucid,
        operatorKeyHash: duplicateKeyHash,
      });
    const first = await register();
    emulator.awaitSlot(3);
    const second = await register();
    const snapshot = await fetchDirectorySnapshot(lucid, contracts);
    const proofNode = SDK.findNodeByKey(snapshot.registered, first.nodeKey);
    if (proofNode === undefined) {
      throw new Error("Expected the first registration to be present");
    }
    const slash = {
      fixture,
      submitterLucid: lucid,
      operatorKeyHash: duplicateKeyHash,
      removedRegisteredNodeKey: second.nodeKey,
      duplicateProof: { kind: "registered", node: proofNode },
    } as const;

    // Slash and recovery together: the slash's inputs pay exactly its
    // outputs and fee, so the recovered bond is the only remainder, back to
    // the submitter's wallet.
    const recovery = SDK.deriveRecoverOperatorBondWitnesses({
      snapshot,
      contracts,
      operatorKeyHash,
    });
    const plan = await slashingPenaltyFeePlan({
      submitterLucid: lucid,
      remainderAddress: await lucid.wallet().address(),
      remainderLovelace: recovery.bondLovelace,
      declaredOutputCount: 3,
    });
    const bundle = SDK.buildSlashDuplicateOperatorTx({
      ...(await duplicateSlashConfig(slash)),
      exactFeePlan: plan,
    }).compose(
      SDK.buildRecoverOperatorBondTx({
        lucid,
        contracts,
        operatorKeyHash,
        retiredOperatorScriptRefs: scriptRefs.retiredOperators,
        retiredNode: recovery.retiredNode,
        retiredAnchor: recovery.retiredAnchor,
        retiredNodeUnit: recovery.retiredNodeUnit,
        ...exitValidityWindow(lucid, emulator),
      }),
    );
    // Script purposes are ordered by policy id; the registered-operators
    // policy refuses, and the retired-operators policy accepts its recovery.
    const registeredMintIndex = [
      contracts.registeredOperators.policyId,
      contracts.retiredOperators.policyId,
    ]
      .sort()
      .indexOf(contracts.registeredOperators.policyId);
    const refusal = await expectOnChainRefusal(() =>
      completeExactFee(bundle, plan),
    );
    expect(refusal).toContain(`Mint[${registeredMintIndex.toString()}]`);
    expect(await bountyAddressUtxos(lucid, operatorKeyHash)).toHaveLength(0);

    // Each alone is accepted.
    await slashDuplicateOperator(slash);
    const [bounty, ...others] = await bountyAddressUtxos(
      lucid,
      operatorKeyHash,
    );
    expect(others).toHaveLength(0);
    expect(bounty?.assets).toEqual({ lovelace: FRAUD_PROVER_REWARD_LOVELACE });
    await recoverOperatorBond({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
    });
    const after = await fetchDirectorySnapshot(lucid, contracts);
    expect(SDK.findNodeByKey(after.retired, operatorKeyHash)).toBeUndefined();
    expect(SDK.findNodeByKey(after.registered, second.nodeKey)).toBeUndefined();
  }, 300_000);
});
