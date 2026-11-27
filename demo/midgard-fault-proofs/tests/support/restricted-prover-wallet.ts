/**
 * The prover signer as the watcher hands it to a builder: the real
 * restricted wallet (`restrictWorkflowWallet`) over the prover's live
 * emulator coins. The objective is the fixture double-spend one; the
 * restricted wallet does not depend on the category, so any family's builder
 * runs under it.
 */
import type { Emulator, UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { isPlainFundingOutput } from "../../src/funding/plain-funding-output.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import {
  type LocallyEvaluatedTransaction,
  workflowTransactionCollateralInputOutRefs,
  workflowTransactionInputOutRefs,
} from "../../src/workflow/transaction-boundary.js";
import { createWorkflowObjectiveWallet } from "../../src/workflow/workflow-run.js";
import { restrictWorkflowWallet } from "../../src/workflow/workflow-wallet.js";

const label = ({ txHash, outputIndex }: UTxO): string =>
  `${txHash}#${outputIndex.toString()}`;

/**
 * `signer` restricted to the plain coins of its wallet, and a check, run
 * before submission, that holds the signed transaction to them: it posts
 * collateral, and every wallet coin it spends and coin it posts as
 * collateral is a plain coin the wallet holds now.
 */
export const restrictedProverWallet = ({
  emulator,
  signer,
}: {
  readonly emulator: Emulator;
  readonly signer: ResolvedProverSigner;
}): {
  readonly signer: ResolvedProverSigner;
  readonly beforeSubmit: (
    transaction: LocallyEvaluatedTransaction,
  ) => Promise<void>;
  readonly checked: () => number;
} => {
  const objectiveWallet = createWorkflowObjectiveWallet({
    category: "doubleSpend",
    headerHash: "ab".repeat(28),
  });
  let checked = 0;
  return {
    signer: restrictWorkflowWallet({ signer, objectiveWallet }),
    beforeSubmit: async ({ signed }) => {
      checked += 1;
      const wallet = new Map(
        (await emulator.getUtxos(signer.address)).map((utxo) => [
          label(utxo),
          utxo,
        ]),
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
        ].filter((outRef) => {
          const coin = wallet.get(outRef);
          return coin === undefined || !isPlainFundingOutput(coin);
        }),
        "signed transaction used a coin the restricted wallet does not offer",
      ).toEqual([]);
    },
    checked: () => checked,
  };
};
