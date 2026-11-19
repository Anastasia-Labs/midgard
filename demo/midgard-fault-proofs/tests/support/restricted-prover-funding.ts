/**
 * The prover signer as the watcher hands it to a builder: the real funding
 * reservation permit, begun for one action, and the real restricted wallet
 * (`restrictWorkflowFundingSigner`) over the prover's live emulator coins.
 *
 * Only the reservation store is the test's: its snapshot reserves every plain
 * coin of the wallet as one pool, the shape the watcher's planner leases from
 * a fresh wallet. The permit is the
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
} from "../../src/workflow/funding-reservation-permit.js";
import { readWorkflowRuntimeFundingPolicy } from "../../src/workflow/runtime-funding-policy.js";
import {
  type LocallyEvaluatedTransaction,
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

/** The funder pays the prover a small plain coin the reservation will hold. */
export const fundReservedCoin = async ({
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
 * as it is now, and a check, run before submission, that holds the signed
 * transaction to the reserved pool: every wallet coin it spends and every coin it posts
 * as collateral is reserved.
 */
export const restrictedProverFunding = async ({
  emulator,
  signer,
}: {
  readonly emulator: Emulator;
  readonly signer: ResolvedProverSigner;
}): Promise<{
  readonly signer: ResolvedProverSigner;
  readonly beforeSubmit: (
    transaction: LocallyEvaluatedTransaction,
  ) => Promise<void>;
  readonly checked: () => number;
}> => {
  // The snapshot lists its inputs in out-ref order (hex hashes and decimal
  // indexes, so code-unit order).
  const coins = (await emulator.getUtxos(signer.address))
    .filter(isPlainFundingOutput)
    .sort((left, right) => (label(left) < label(right) ? -1 : 1));
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
  const pool = new Set(coins.map(label));
  let checked = 0;
  return {
    signer: restrictWorkflowFundingSigner({ signer, permit }),
    // The rule `prepareWorkflowFundingReservationTransaction` enforces before
    // it records an attempt: wallet coins spent and coins posted as
    // collateral all come from the reservation.
    beforeSubmit: async ({ signed }) => {
      checked += 1;
      const wallet = new Set(
        (await emulator.getUtxos(signer.address)).map(label),
      );
      const collateralInputs = [
        ...workflowTransactionCollateralInputOutRefs(signed),
      ];
      expect(
        collateralInputs,
        "signed transaction posted no collateral",
      ).not.toEqual([]);
      expect(
        [
          ...workflowTransactionInputOutRefs(signed).filter((outRef) =>
            wallet.has(outRef),
          ),
          ...collateralInputs,
        ].filter((outRef) => !pool.has(outRef)),
        "signed transaction used a coin outside the reservation",
      ).toEqual([]);
    },
    checked: () => checked,
  };
};
