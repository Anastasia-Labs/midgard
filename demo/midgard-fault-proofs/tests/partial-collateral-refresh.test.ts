/**
 * Collateral a reservation still holds after a confirmation released part of
 * it, and the next action that needs more.
 *
 * A confirmed transaction's collateral lease ends at confirmation, so a
 * reservation whose collateral was two coins keeps only the one its last
 * confirmed transaction did not name. A removal whose fee is the slashing
 * penalty needs collateral of that fee's percentage, more than one coin. The
 * next action must begin with the reservation's collateral restored, so the
 * transaction takes its collateral from reserved collateral alone, never from
 * a reserved funding coin. Real permit, real restricted wallet, real emulator;
 * only the reservation store is the test's.
 *
 * A removal of an operator already slashed pays its fee from the largest
 * coin its wallet offers: that is reserved funding, never the reserved
 * collateral. And a transaction that does break the separation is refused by
 * name.
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
import { selectFeeInput } from "../src/step-support.js";
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
 * A prover holding one funding coin and two collateral coins, a reservation
 * that still holds the first collateral coin (a confirmation released the
 * other), and an idle refresh that reserves both again. The reservation has
 * begun for a removal, and `lucid` builds through the restricted wallet.
 */
const reservationAfterPartialRelease = async ({
  fundingLovelace,
  collateralLovelace,
}: {
  readonly fundingLovelace: bigint;
  readonly collateralLovelace: readonly [bigint, bigint];
}) => {
  const proverAccount = generateEmulatorAccount({});
  const signer = resolveProverSigner({
    network,
    walletSeedPhrase: proverAccount.seedPhrase,
  });
  const funderAccount = generateEmulatorAccount({ lovelace: ada(1_000n) });
  const coins = [fundingLovelace, ...collateralLovelace];
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
  const [, scriptUtxo, funding, remaining, released] = (
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
  const snapshot = (
    revision: string,
    collateral: readonly UTxO[],
  ): WorkflowFundingReservationSnapshot =>
    Object.freeze({
      reservationId: "c1".repeat(32),
      deploymentFingerprint: DEPLOYMENT,
      decisionDigest: actuation.decisionDigest,
      policyDigest: funded.policyDigest,
      reservationBasisDigest: "c2".repeat(32),
      rollbackGeneration: "7",
      revision,
      walletAddress: signer.address,
      fundingPaymentKeyHash: signer.paymentKeyHash,
      state: "active" as const,
      activeInputs: Object.freeze(
        [
          { utxo: funding!, role: "funding" as const },
          ...collateral.map((utxo) => ({
            utxo,
            role: "collateral" as const,
          })),
        ]
          .map(({ utxo, role }) =>
            Object.freeze({
              outRef: label(utxo),
              role,
              lovelace: utxo.assets.lovelace!.toString(),
              assets: Object.freeze([]),
            }),
          )
          .sort((left, right) => (left.outRef < right.outRef ? -1 : 1)),
      ),
    });
  let current = snapshot("0", [remaining!]);
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
      load: async () => current,
      // Releasing the idle reservation and reserving the wallet as it is now
      // takes both collateral coins again.
      refreshIdle: async () => {
        current = snapshot("1", [remaining!, released!]);
        return current;
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
    funding: label(funding!),
    remaining: label(remaining!),
    released: label(released!),
    scriptUtxo: scriptUtxo!,
  };
};

/**
 * A removal whose fee is exactly the slashing penalty, paid from the script
 * output and balanced without coin selection, as a bond-backed one is: it
 * spends no reserved funding.
 */
const removalAfterPartialRelease = async (
  collateralLovelace: readonly [bigint, bigint],
) => {
  const reservation = await reservationAfterPartialRelease({
    fundingLovelace: ada(2_000n),
    collateralLovelace,
  });
  const fee = BigInt(
    reservation.funded.economics.policy.slashingPenaltyLovelace,
  );
  const unsigned = await reservation.lucid
    .newTx()
    .collectFrom([reservation.scriptUtxo], Data.void())
    .attach.SpendingValidator(validator)
    .pay.ToAddress(reservation.address, {
      lovelace: reservation.scriptUtxo.assets.lovelace! - fee,
    })
    .setMinFee(fee)
    .complete({ coinSelection: false });
  const signed = await unsigned.sign.withWallet().complete();
  expect(signed.toTransaction().body().fee()).toBe(fee);
  return { ...reservation, signed };
};

it("restores a reservation's collateral before an action, so a removal's collateral comes from reserved collateral alone", async () => {
  // Each coin is short of the slashing penalty's collateral on its own.
  const { emulator, funded, signed, remaining, released, scriptUtxo } =
    await removalAfterPartialRelease([ada(500n), ada(500n)]);
  expect(BigInt(funded.maximumSlashCollateralLovelace)).toBeGreaterThan(
    ada(500n),
  );
  expect({
    collateral: [...workflowTransactionCollateralInputOutRefs(signed)].sort(),
    inputs: [...workflowTransactionInputOutRefs(signed)].sort(),
  }).toEqual({
    collateral: [remaining, released].sort(),
    inputs: [label(scriptUtxo)],
  });
  // The transaction is valid on the chain.
  await signed.submit();
  emulator.awaitBlock();
  expect(await emulator.getUtxosByOutRef([scriptUtxo])).toEqual([]);
});

it("keeps the collateral a reservation still holds when it covers the slashing penalty's collateral", async () => {
  const { signed, remaining } = await removalAfterPartialRelease([
    ada(800n),
    ada(500n),
  ]);
  expect(workflowTransactionCollateralInputOutRefs(signed)).toEqual([
    remaining,
  ]);
});

it("pays a bondless removal's fee from reserved funding even when the reserved collateral is the wallet's largest coin", async () => {
  // The drill's shape: the collateral coin is larger than any funding coin.
  const { lucid, funding, remaining, released, scriptUtxo } =
    await reservationAfterPartialRelease({
      fundingLovelace: ada(395n),
      collateralLovelace: [ada(500n), ada(500n)],
    });
  const offered = await lucid.wallet().getUtxos();
  expect(offered.map(label)).toEqual([funding]);
  const feeInput = selectFeeInput(offered);
  const signed = await (
    await lucid
      .newTx()
      .collectFrom([feeInput])
      .collectFrom([scriptUtxo], Data.void())
      .attach.SpendingValidator(validator)
      .complete()
  ).sign
    .withWallet()
    .complete();
  expect({
    inputs: [...workflowTransactionInputOutRefs(signed)].sort(),
    collateralFromReserved: workflowTransactionCollateralInputOutRefs(
      signed,
    ).every((outRef) => [remaining, released].includes(outRef)),
  }).toEqual({
    inputs: [funding, label(scriptUtxo)].sort(),
    collateralFromReserved: true,
  });
});

describe("a signed transaction that breaks the reserved separation", () => {
  const refusal = async (
    shape: (selected: {
      fundingOutRefs: readonly string[];
      collateralOutRefs: readonly string[];
    }) => {
      inputOutRefs: readonly string[];
      collateralOutRefs: readonly string[];
    },
  ) => {
    const runtime = await runtimeFunding("step-one", { collateral: true });
    const { inputOutRefs, collateralOutRefs } = shape(runtime.selected);
    const preflight = bindWorkflowPreflightTransaction(
      Object.freeze({ txHash: "separation" }),
      signedFundingTransaction({
        inputOutRefs,
        outputLovelace: 9_000_000n,
        redeemerMemory: 1n,
        collateral: {
          outRefs: collateralOutRefs,
          total: 3_000_000n,
          returned: 2_000_000n,
        },
      }),
    );
    return {
      runtime,
      refused: runtime.prepareTransaction({
        action: { actionId: "step-one", input: { actionKind: "step-one" } },
        preflight,
      }),
    };
  };

  it("names a reserved funding coin taken as collateral", async () => {
    const funding = `${"73".repeat(32)}#0`;
    const { runtime, refused } = await refusal(({ fundingOutRefs }) => ({
      inputOutRefs: fundingOutRefs.filter((outRef) => outRef !== funding),
      collateralOutRefs: [funding],
    }));
    await expect(refused).rejects.toThrow(
      `signed transaction changed reserved ordinary/collateral separation: collateral ${funding} is reserved funding`,
    );
    expect(runtime.prepare).not.toHaveBeenCalled();
  });

  it("names a reserved collateral coin spent as an input", async () => {
    const { runtime, refused } = await refusal(
      ({ fundingOutRefs, collateralOutRefs }) => ({
        inputOutRefs: [...fundingOutRefs, ...collateralOutRefs],
        collateralOutRefs,
      }),
    );
    await expect(refused).rejects.toThrow(
      `signed transaction changed reserved ordinary/collateral separation: reserved collateral ${`${"76".repeat(32)}#0`} is spent as an input`,
    );
    expect(runtime.prepare).not.toHaveBeenCalled();
  });
});
