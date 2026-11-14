/**
 * The prover signer as the watcher hands it to a builder: the real funding
 * reservation permit, begun for one action, and the real restricted wallet
 * (`restrictWorkflowFundingSigner`) over the prover's live emulator coins.
 *
 * Only the reservation store is the test's: its snapshot reserves one
 * collateral coin and every other plain coin of the wallet as funding, the
 * shape the watcher's planner leases from a fresh wallet. The permit is the
 * fixture double-spend objective's; the restricted wallet does not depend on
 * the category, so any family's builder runs under it.
 */
import type { Emulator, LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { isPlainFundingOutput } from "../../src/funding/plain-funding-output.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import {
  beginWorkflowFundingReservationAction,
  bindWorkflowFundingReservationJournal,
  createWorkflowFundingReservationPermit,
  restrictWorkflowFundingSigner,
  unsafeWorkflowFundingReservationSelectedOutRefsForTest,
} from "../../src/workflow/funding-reservation-permit.js";
import { readWorkflowRuntimeFundingPolicy } from "../../src/workflow/runtime-funding-policy.js";
import {
  type FraudProofPreSubmitBoundary,
  workflowTransactionCollateralInputOutRefs,
  workflowTransactionInputOutRefs,
} from "../../src/workflow/transaction-boundary.js";
import { runtimeFundingPolicyFixture } from "../helpers/runtime-funding-policy-fixture.js";
import {
  admittedActuation,
  DEPLOYMENT,
  discardingWorkflowJournal,
} from "../workflow-runtime.admitted-actuation.js";

const label = ({ txHash, outputIndex }: UTxO): string =>
  `${txHash}#${outputIndex.toString()}`;

const outRefOf = (outRef: string) => {
  const [txHash, outputIndex] = outRef.split("#");
  return { txHash: txHash!, outputIndex: Number(outputIndex) };
};

/** The funder pays the prover the coin the reservation will hold as collateral. */
export const fundReservedCollateral = async ({
  emulator,
  funderLucid,
  signer,
}: {
  readonly emulator: Emulator;
  readonly funderLucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
}): Promise<UTxO> => {
  const paid = await funderLucid
    .newTx()
    .pay.ToAddress(signer.address, { lovelace: 5_000_000n })
    .complete();
  const txHash = await (await paid.sign.withWallet().complete()).submit();
  emulator.awaitBlock();
  const [coin] = (await emulator.getUtxos(signer.address)).filter(
    (utxo) => utxo.txHash === txHash,
  );
  return coin!;
};

/**
 * `signer` restricted to a reservation begun for one action over the wallet
 * as it is now, and a pre-submit boundary that holds the signed transaction
 * to the watcher's reserved ordinary/collateral separation.
 */
export const restrictedProverFunding = async ({
  emulator,
  signer,
  collateral,
}: {
  readonly emulator: Emulator;
  readonly signer: ResolvedProverSigner;
  readonly collateral: UTxO;
}): Promise<{
  readonly signer: ResolvedProverSigner;
  readonly preSubmitBoundary: FraudProofPreSubmitBoundary;
  readonly checked: () => number;
}> => {
  // The snapshot lists its inputs in out-ref order (hex hashes and decimal
  // indexes, so code-unit order).
  const coins = (await emulator.getUtxos(signer.address))
    .filter(isPlainFundingOutput)
    .sort((left, right) => (label(left) < label(right) ? -1 : 1));
  expect(coins.map(label)).toContain(label(collateral));
  const actuation = await admittedActuation();
  const { runner, policy } = runtimeFundingPolicyFixture({
    deploymentFingerprint: DEPLOYMENT,
    fundingPaymentKeyHash: signer.paymentKeyHash,
  });
  const snapshot = Object.freeze({
    reservationId: "c1".repeat(32),
    deploymentFingerprint: DEPLOYMENT,
    decisionDigest: actuation.decisionDigest,
    policyDigest: readWorkflowRuntimeFundingPolicy(policy).policyDigest,
    reservationBasisDigest: "c2".repeat(32),
    rollbackGeneration: "7",
    revision: "0",
    walletAddress: signer.address,
    fundingPaymentKeyHash: signer.paymentKeyHash,
    state: "active" as const,
    activeInputs: Object.freeze(
      coins.map((utxo) =>
        Object.freeze({
          outRef: label(utxo),
          role:
            label(utxo) === label(collateral)
              ? ("collateral" as const)
              : ("funding" as const),
          lovelace: utxo.assets.lovelace!.toString(),
          assets: Object.freeze([]),
        }),
      ),
    ),
  });
  const unused = async () => {
    throw new Error("restricted prover funding records no transition");
  };
  const permit = await createWorkflowFundingReservationPermit({
    category: "doubleSpend",
    runner,
    policy,
    actuationPermit: actuation.actuationPermit,
    rollbackGeneration: "7",
    port: {
      load: async () => snapshot,
      resolveInputs: async (outRefs) =>
        await emulator.getUtxosByOutRef(outRefs.map(outRefOf)),
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
    action: { actionId: "step-one", input: { actionKind: "step-one" } },
  });
  const reserved =
    unsafeWorkflowFundingReservationSelectedOutRefsForTest(permit);
  expect(reserved.collateralOutRefs).toEqual([label(collateral)]);
  let checked = 0;
  return {
    signer: restrictWorkflowFundingSigner({ signer, permit }),
    // The rule `prepareWorkflowFundingReservationTransaction` enforces before
    // it records an attempt: no reserved collateral among the body inputs,
    // and collateral only from the reserved collateral.
    preSubmitBoundary: ({ signed }) => {
      checked += 1;
      const bodyInputs = workflowTransactionInputOutRefs(signed);
      const collateralInputs =
        workflowTransactionCollateralInputOutRefs(signed);
      expect(
        {
          bodyInputsHoldingCollateral: bodyInputs.filter((outRef) =>
            reserved.collateralOutRefs.includes(outRef),
          ),
          collateralInputs: [...collateralInputs],
        },
        "signed transaction changed reserved ordinary/collateral separation",
      ).toEqual({
        bodyInputsHoldingCollateral: [],
        collateralInputs: [...reserved.collateralOutRefs],
      });
    },
    checked: () => checked,
  };
};
