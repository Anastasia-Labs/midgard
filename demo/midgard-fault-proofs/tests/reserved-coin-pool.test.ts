/**
 * A reservation holds one pool of reserved coins. A transaction may spend any
 * of them, post any of them as collateral, or both at once, as the ledger
 * allows; it may use no coin outside the pool for either, and a refusal
 * names the coin.
 *
 * The removal of a block whose operator still holds its bond pays the
 * slashing penalty as its exact fee, so its collateral is that fee's
 * percentage: more than any one coin of the drill's prover held. Real permit,
 * real restricted wallet, real emulator; only the reservation store is the
 * test's.
 */
import {
  applyDoubleCborEncoding,
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type SpendingValidator,
  type UTxO,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { resolveProverSigner } from "../src/runtime.js";
import {
  beginWorkflowFundingReservationAction,
  bindWorkflowFundingReservationJournal,
  createWorkflowFundingReservationPermit,
  restrictWorkflowFundingSigner,
  type WorkflowFundingReservationSnapshot,
} from "../src/workflow/funding-reservation-permit.js";
import { readWorkflowRuntimeFundingPolicy } from "../src/workflow/runtime-funding-policy.js";
import {
  bindWorkflowPreflightTransaction,
  workflowTransactionCollateralInputOutRefs,
  workflowTransactionInputOutRefs,
} from "../src/workflow/transaction-boundary.js";
import { runtimeFundingPolicyFixture } from "./helpers/runtime-funding-policy-fixture.js";
import {
  admittedActuation,
  DEPLOYMENT,
  discardingWorkflowJournal,
} from "./workflow-runtime.admitted-actuation.js";
import {
  runtimeFunding,
  signedFundingTransaction,
} from "./workflow-runtime.runtime-funding.js";

const network = "Custom" as const;
const ada = (amount: bigint) => amount * 1_000_000n;
const label = ({ txHash, outputIndex }: UTxO) =>
  `${txHash}#${outputIndex.toString()}`;

// An always-succeeding spending validator, so the action runs a script and
// needs collateral.
const validator: SpendingValidator = {
  type: "PlutusV3",
  script: applyDoubleCborEncoding("5101010023259800a518a4d136564004ae69"),
};

/**
 * A prover whose reservation holds `coins`, begun for a removal, with `lucid`
 * building through the restricted wallet.
 */
const reservationOf = async (coins: readonly bigint[]) => {
  const proverAccount = generateEmulatorAccount({});
  const signer = resolveProverSigner({
    network,
    walletSeedPhrase: proverAccount.seedPhrase,
  });
  const funderAccount = generateEmulatorAccount({ lovelace: ada(1_000n) });
  const emulator = new Emulator([
    funderAccount,
    {
      ...funderAccount,
      // Stands in for the bond a removal's fee comes from.
      address: validatorToAddress(network, validator),
      assets: { lovelace: ada(600n) },
      outputData: { inline: Data.void() },
    },
    ...coins.map((lovelace) => ({
      ...proverAccount,
      address: signer.address,
      assets: { lovelace },
    })),
  ]);
  // The emulator's genesis outputs, one per account in order.
  const [, scriptUtxo, ...reserved] = (
    await emulator.getUtxosByOutRef(
      Array.from({ length: coins.length + 2 }, (_, outputIndex) => ({
        txHash: "00".repeat(32),
        outputIndex,
      })),
    )
  ).sort((left, right) => left.outputIndex - right.outputIndex);
  const actuation = await admittedActuation();
  const { runner, policy } = runtimeFundingPolicyFixture({
    deploymentFingerprint: DEPLOYMENT,
    fundingPaymentKeyHash: signer.paymentKeyHash,
  });
  const funded = readWorkflowRuntimeFundingPolicy(policy);
  const snapshot: WorkflowFundingReservationSnapshot = Object.freeze({
    reservationId: "c1".repeat(32),
    deploymentFingerprint: DEPLOYMENT,
    decisionDigest: actuation.decisionDigest,
    policyDigest: funded.policyDigest,
    reservationBasisDigest: "c2".repeat(32),
    rollbackGeneration: "7",
    revision: "0",
    walletAddress: signer.address,
    fundingPaymentKeyHash: signer.paymentKeyHash,
    state: "active" as const,
    activeInputs: Object.freeze(
      reserved
        .map((utxo) =>
          Object.freeze({
            outRef: label(utxo),
            lovelace: utxo.assets.lovelace!.toString(),
            assets: Object.freeze([]),
          }),
        )
        .sort((left, right) => (left.outRef < right.outRef ? -1 : 1)),
    ),
  });
  const unused = async () => {
    throw new Error("the action records no transition");
  };
  const permit = await createWorkflowFundingReservationPermit({
    category: "doubleSpend",
    runner,
    policy,
    actuationPermit: actuation.actuationPermit,
    rollbackGeneration: "7",
    port: {
      load: async () => snapshot,
      refreshIdle: async () => {
        throw new Error("the reservation already holds what the action needs");
      },
      resolveInputs: async (outRefs) =>
        await emulator.getUtxosByOutRef(
          outRefs.map((outRef) => {
            const [txHash, outputIndex] = outRef.split("#");
            return { txHash: txHash!, outputIndex: Number(outputIndex) };
          }),
        ),
      resolveConfirmedInput: async () => null,
      readPendingTransition: async () => null,
      readLandedSupersededAttempt: async () => null,
      readRecordedSignedTransaction: async () => null,
      resolveProtocolInputAuthority: unused,
      prepare: unused,
      confirm: unused,
      abandon: unused,
      markConflict: unused,
      release: unused,
    },
  });
  const journal = discardingWorkflowJournal();
  bindWorkflowFundingReservationJournal({ journal, permit });
  await beginWorkflowFundingReservationAction({
    journal,
    action: { actionId: "remove", input: { actionKind: "remove" } },
  });
  const lucid = await Lucid(emulator, network);
  restrictWorkflowFundingSigner({ signer, permit }).selectWallet(lucid);
  return {
    emulator,
    lucid,
    funded,
    address: signer.address,
    reserved: reserved.map((utxo) => utxo!),
    scriptUtxo: scriptUtxo!,
  };
};

it("lands a bond-backed removal whose collateral comes partly from a reserved coin it also spends", async () => {
  // The drill's prover: no one coin covers the slashing penalty's collateral.
  const { emulator, lucid, funded, address, reserved, scriptUtxo } =
    await reservationOf([ada(395n), ada(500n)]);
  const fee = BigInt(funded.economics.policy.slashingPenaltyLovelace);
  expect(BigInt(funded.maximumSlashCollateralLovelace)).toBeGreaterThan(
    ada(500n),
  );
  const [spent] = reserved;
  // Exact fee from the script output, balanced without coin selection, as a
  // bond-backed removal is; the reserved coin it spends comes back whole.
  const signed = await (
    await lucid
      .newTx()
      .collectFrom([spent!])
      .collectFrom([scriptUtxo], Data.void())
      .attach.SpendingValidator(validator)
      .pay.ToAddress(address, {
        lovelace: scriptUtxo.assets.lovelace! + spent!.assets.lovelace! - fee,
      })
      .setMinFee(fee)
      .complete({ coinSelection: false })
  ).sign
    .withWallet()
    .complete();
  expect(signed.toTransaction().body().fee()).toBe(fee);
  expect({
    collateral: [...workflowTransactionCollateralInputOutRefs(signed)].sort(),
    inputs: [...workflowTransactionInputOutRefs(signed)].sort(),
  }).toEqual({
    collateral: reserved.map(label).sort(),
    inputs: [label(spent!), label(scriptUtxo)].sort(),
  });
  await signed.submit();
  emulator.awaitBlock();
  expect(await emulator.getUtxosByOutRef([scriptUtxo, spent!])).toEqual([]);
});

it("lands a bond-backed removal that spends no reserved coin and posts its collateral from two of them", async () => {
  // The drill's shape: the bond pays the fee, the prover's coins only stand
  // behind it as collateral.
  const { emulator, lucid, funded, address, reserved, scriptUtxo } =
    await reservationOf([ada(395n), ada(500n)]);
  const fee = BigInt(funded.economics.policy.slashingPenaltyLovelace);
  const signed = await (
    await lucid
      .newTx()
      .collectFrom([scriptUtxo], Data.void())
      .attach.SpendingValidator(validator)
      .pay.ToAddress(address, {
        lovelace: scriptUtxo.assets.lovelace! - fee,
      })
      .setMinFee(fee)
      .complete({ coinSelection: false })
  ).sign
    .withWallet()
    .complete();
  expect({
    collateral: [...workflowTransactionCollateralInputOutRefs(signed)].sort(),
    inputs: [...workflowTransactionInputOutRefs(signed)],
  }).toEqual({
    collateral: reserved.map(label).sort(),
    inputs: [label(scriptUtxo)],
  });
  await signed.submit();
  emulator.awaitBlock();
  expect(await emulator.getUtxosByOutRef([scriptUtxo])).toEqual([]);
  expect((await emulator.getUtxosByOutRef(reserved)).map(label).sort()).toEqual(
    reserved.map(label).sort(),
  );
});

describe("the reserved pool at admission", () => {
  const prepared = async (
    shape: (fundingOutRefs: readonly string[]) => {
      inputOutRefs: readonly string[];
      collateralOutRefs: readonly string[];
    },
  ) => {
    const runtime = await runtimeFunding("step-one");
    const { inputOutRefs, collateralOutRefs } = shape(
      runtime.selected.fundingOutRefs,
    );
    const preflight = bindWorkflowPreflightTransaction(
      Object.freeze({ txHash: "pool" }),
      signedFundingTransaction({
        inputOutRefs,
        outputLovelace: 14_800_000n,
        redeemerMemory: 1n,
        collateral: {
          outRefs: collateralOutRefs,
          total: 300_000n,
          returned: 1_700_000n,
        },
      }),
    );
    return {
      runtime,
      admitted: runtime.prepareTransaction({
        action: { actionId: "step-one", input: { actionKind: "step-one" } },
        preflight,
      }),
    };
  };

  it("admits a reserved coin that is both spent and posted as collateral", async () => {
    const both = `${"72".repeat(32)}#0`;
    const { runtime, admitted } = await prepared((fundingOutRefs) => {
      expect(fundingOutRefs).toContain(both);
      return { inputOutRefs: fundingOutRefs, collateralOutRefs: [both] };
    });
    await expect(admitted).resolves.toBeUndefined();
    expect(runtime.prepare).toHaveBeenCalledTimes(1);
  });

  it("names collateral outside the reservation", async () => {
    const outside = `${"98".repeat(32)}#0`;
    const { runtime, admitted } = await prepared((fundingOutRefs) => ({
      inputOutRefs: fundingOutRefs,
      collateralOutRefs: [outside],
    }));
    await expect(admitted).rejects.toThrow(
      `signed transaction uses collateral outside the reservation: ${outside}`,
    );
    expect(runtime.prepare).not.toHaveBeenCalled();
  });

  it("names a wallet input outside the reservation", async () => {
    const outside = `${"99".repeat(32)}#0`;
    const both = `${"72".repeat(32)}#0`;
    const { runtime, admitted } = await prepared((fundingOutRefs) => ({
      inputOutRefs: [...fundingOutRefs, outside],
      collateralOutRefs: [both],
    }));
    await expect(admitted).rejects.toThrow(
      `signed transaction consumed an unreserved wallet input: ${outside}`,
    );
    expect(runtime.prepare).not.toHaveBeenCalled();
  });
});
