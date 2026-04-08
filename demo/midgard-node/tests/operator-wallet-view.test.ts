import { describe, expect, it } from "vitest";
import { CML, walletFromSeed, type UTxO } from "@lucid-evolution/lucid";
import {
  applySubmittedTxToOperatorWalletView,
  availableOperatorWalletUtxos,
  isPotentiallyStaleOperatorWalletViewError,
  makeOperatorWalletView,
  mergeOperatorWalletViews,
  noteConsumedOperatorWalletInputs,
} from "@/operator-wallet-view.js";

const TEST_SEED =
  "cupboard digital guitar diesel critic will afford salon game dolphin phrase baby dad urban machine barely rack acoustic blood vote misery enemy salute depart";
const OTHER_TEST_SEED =
  "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail";

const makeUtxo = (
  txHash: string,
  outputIndex: number,
  address: string,
  lovelace: bigint,
): UTxO => ({
  txHash,
  outputIndex,
  address,
  assets: {
    lovelace,
  },
});

const makeSubmittedTx = ({
  spentInput,
  walletAddress,
  otherAddress,
  walletOutputLovelace,
  otherOutputLovelace,
}: {
  readonly spentInput: UTxO;
  readonly walletAddress: string;
  readonly otherAddress: string;
  readonly walletOutputLovelace: bigint;
  readonly otherOutputLovelace: bigint;
}): CML.Transaction => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(spentInput.txHash),
      BigInt(spentInput.outputIndex),
    ),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(otherAddress),
      CML.Value.from_coin(otherOutputLovelace),
    ),
  );
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(walletAddress),
      CML.Value.from_coin(walletOutputLovelace),
    ),
  );
  return CML.Transaction.new(
    CML.TransactionBody.new(inputs, outputs, 0n),
    CML.TransactionWitnessSet.new(),
    true,
    undefined,
  );
};

describe("operator-wallet-view", () => {
  it("filters consumed outrefs from the available wallet view", () => {
    const wallet = walletFromSeed(TEST_SEED, { network: "Preprod" });
    const retained = makeUtxo("aa".repeat(32), 0, wallet.address, 7_000_000n);
    const consumed = makeUtxo("bb".repeat(32), 1, wallet.address, 5_000_000n);

    const view = noteConsumedOperatorWalletInputs(
      makeOperatorWalletView(wallet.address, [retained, consumed]),
      [consumed],
    );

    expect(availableOperatorWalletUtxos(view)).toEqual([retained]);
  });

  it("applies locally submitted wallet spends and same-wallet outputs", () => {
    const wallet = walletFromSeed(TEST_SEED, { network: "Preprod" });
    const other = walletFromSeed(OTHER_TEST_SEED, { network: "Preprod" });
    const spentInput = makeUtxo("cc".repeat(32), 2, wallet.address, 11_000_000n);
    const untouched = makeUtxo("dd".repeat(32), 3, wallet.address, 4_000_000n);

    const initialView = makeOperatorWalletView(wallet.address, [
      spentInput,
      untouched,
    ]);
    const updatedView = applySubmittedTxToOperatorWalletView(
      initialView,
      makeSubmittedTx({
        spentInput,
        walletAddress: wallet.address,
        otherAddress: other.address,
        walletOutputLovelace: 8_000_000n,
        otherOutputLovelace: 3_000_000n,
      }),
      "ee".repeat(32),
    );

    expect(updatedView.consumedOutRefs).toContain(`${spentInput.txHash}#2`);
    expect(availableOperatorWalletUtxos(updatedView)).toEqual([
      untouched,
      makeUtxo("ee".repeat(32), 1, wallet.address, 8_000_000n),
    ]);
  });

  it("merges wallet views without reintroducing consumed outputs", () => {
    const wallet = walletFromSeed(TEST_SEED, { network: "Preprod" });
    const persistent = makeUtxo("11".repeat(32), 0, wallet.address, 9_000_000n);
    const consumedLater = makeUtxo("22".repeat(32), 1, wallet.address, 6_000_000n);
    const newOutput = makeUtxo("33".repeat(32), 2, wallet.address, 5_000_000n);

    const previous = makeOperatorWalletView(wallet.address, [
      persistent,
      consumedLater,
    ]);
    const current = noteConsumedOperatorWalletInputs(
      makeOperatorWalletView(wallet.address, [newOutput]),
      [consumedLater],
    );

    const merged = mergeOperatorWalletViews(current, previous);

    expect(merged.consumedOutRefs).toEqual([`${consumedLater.txHash}#1`]);
    expect(availableOperatorWalletUtxos(merged)).toEqual([
      newOutput,
      persistent,
    ]);
  });

  it("classifies stale-input style submit failures for wallet-view reloads", () => {
    expect(
      isPotentiallyStaleOperatorWalletViewError(
        new Error("BadInputsUTxO: input disappeared from provider view"),
      ),
    ).toBe(true);
    expect(
      isPotentiallyStaleOperatorWalletViewError(
        new Error("OutsideValidityIntervalUTxO: too early"),
      ),
    ).toBe(false);
  });
});
