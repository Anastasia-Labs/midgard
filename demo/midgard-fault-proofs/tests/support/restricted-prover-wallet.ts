/**
 * The prover signer as the watcher hands it to a builder: the real
 * restricted wallet (`restrictWorkflowWallet`) over the prover's live
 * emulator coins, in a fresh process whose other actions hold the coins a
 * test names. The objective is the fixture double-spend one; the restricted
 * wallet does not depend on the category, so any family's builder runs
 * under it.
 */
import type { Emulator, LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { isPlainFundingOutput } from "../../src/funding/plain-funding-output.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import { createWorkflowInFlight } from "../../src/workflow/in-flight.js";
import {
  type LocallyEvaluatedTransaction,
  workflowTransactionCollateralInputOutRefs,
  workflowTransactionInputOutRefs,
} from "../../src/workflow/transaction-boundary.js";
import { restrictWorkflowWallet } from "../../src/workflow/workflow-wallet.js";

const label = ({ txHash, outputIndex }: UTxO): string =>
  `${txHash}#${outputIndex.toString()}`;

/** The funder pays the prover a small plain coin. */
export const fundProverCoin = async ({
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
 * `signer` restricted to the plain coins of its wallet that no other action
 * of a fresh process holds, `held` being held by one, and a check, run
 * before submission, that holds the signed transaction to them: it posts
 * collateral, and no wallet coin it spends or coin it posts as collateral is
 * held or anything but a plain coin.
 */
export const restrictedProverWallet = ({
  emulator,
  signer,
  held = [],
}: {
  readonly emulator: Emulator;
  readonly signer: ResolvedProverSigner;
  readonly held?: readonly UTxO[];
}): {
  readonly signer: ResolvedProverSigner;
  readonly beforeSubmit: (
    transaction: LocallyEvaluatedTransaction,
  ) => Promise<void>;
  readonly checked: () => number;
} => {
  const inFlight = createWorkflowInFlight();
  inFlight.holdWalletInputs({
    outRefs: held.map(label),
    untilMs: Number.MAX_SAFE_INTEGER,
  });
  const sends = inFlight.forObjective({
    category: "doubleSpend",
    headerHash: "ab".repeat(28),
  });
  let checked = 0;
  return {
    signer: restrictWorkflowWallet({ signer, sends }),
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
          return (
            coin === undefined ||
            !isPlainFundingOutput(coin) ||
            sends.isHeld(outRef)
          );
        }),
        "signed transaction used a coin the restricted wallet does not offer",
      ).toEqual([]);
    },
    checked: () => checked,
  };
};
