/** One fault-proof step funded, completed and submitted through
 * `step-completion.ts` on a real emulator: what its funding may spend, where
 * its collateral comes from under the watcher's restricted wallet, and what
 * happens around submission. */
import { layerMethod } from "@al-ft/midgard-test-support/method-layer";
import {
  applyDoubleCborEncoding,
  Constr,
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  type SpendingValidator,
  type TxBuilder,
  type UTxO,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { describe, expect, expectTypeOf, it } from "vitest";

import { resolveProverSigner } from "../src/runtime.js";
import {
  completeStep,
  fundStep,
  type StepFunding,
  submitStep,
} from "../src/step-completion.js";
import {
  workflowTransactionCollateralInputOutRefs,
  workflowTransactionInputOutRefs,
} from "../src/workflow/transaction-boundary.js";
import {
  fundReservedCollateral,
  restrictedProverFunding,
} from "./support/restricted-prover-funding.js";

const network = "Custom" as const;
const ada = (amount: bigint) => amount * 1_000_000n;
const label = ({ txHash, outputIndex }: UTxO) =>
  `${txHash}#${outputIndex.toString()}`;

// An always-succeeding spending validator, so the step runs a script and
// needs collateral.
const validator: SpendingValidator = {
  type: "PlutusV3",
  script: applyDoubleCborEncoding("5101010023259800a518a4d136564004ae69"),
};

/** A prover whose wallet holds `coins`, in order, beside a funder. A coin
 * with a datum stands for a published proof chunk at the prover's key
 * address. */
const prover = async (
  coins: readonly { readonly lovelace: bigint; readonly datum?: string }[],
) => {
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
      address: validatorToAddress(network, validator),
      assets: { lovelace: ada(2n) },
      outputData: { inline: Data.void() },
    },
    ...coins.map(({ lovelace, datum }) => ({
      ...proverAccount,
      address: signer.address,
      assets: { lovelace },
      ...(datum === undefined ? {} : { outputData: { inline: datum } }),
    })),
  ]);
  const lucid = await Lucid(emulator, network);
  const funderLucid = await Lucid(emulator, network);
  funderLucid.selectWallet.fromSeed(funderAccount.seedPhrase);
  // The emulator's genesis outputs, one per account in order.
  const [, scriptUtxo, ...proverUtxos] = (
    await emulator.getUtxosByOutRef(
      Array.from({ length: coins.length + 2 }, (_, outputIndex) => ({
        txHash: "00".repeat(32),
        outputIndex,
      })),
    )
  ).sort((left, right) => left.outputIndex - right.outputIndex);
  return {
    emulator,
    lucid,
    funderLucid,
    signer,
    funder: funderAccount.address,
    scriptUtxo: scriptUtxo!,
    proverUtxos,
  };
};

const unspent = async (lucid: LucidEvolution, utxos: readonly UTxO[]) => {
  const live = new Set(
    (await lucid.utxosByOutRef([...utxos])).map((utxo) => label(utxo)),
  );
  return utxos.filter((utxo) => live.has(label(utxo))).map(label);
};

describe("fundStep and completeStep", () => {
  it("spend the largest plain coin and coin-select only plain coins outside the step's own, never a proof chunk", async () => {
    const chunk = Data.to(new Constr(0, ["ab".repeat(32)]));
    const { lucid, signer, funder, proverUtxos } = await prover([
      { lovelace: ada(20n) },
      { lovelace: ada(3n) },
      { lovelace: ada(100n), datum: chunk },
      { lovelace: ada(60n) },
    ]);
    const [fee, extra, chunkUtxo, referenced] = proverUtxos;

    const funding = await fundStep(lucid, {
      signer,
      excluding: [referenced!],
    });
    const signed = await completeStep(
      funding.tx.pay.ToAddress(funder, { lovelace: ada(21n) }),
      funding,
    );

    expect([...workflowTransactionInputOutRefs(signed)].sort()).toEqual(
      [fee!, extra!].map(label).sort(),
    );
    const requiredSigners = signed.toTransaction().body().required_signers();
    expect(requiredSigners?.len()).toBe(1);
    expect(requiredSigners?.get(0).to_hex()).toBe(signer.paymentKeyHash);
    await submitStep(lucid, signed, {
      referenceScripts: [],
      awaitConfirmation: true,
    });
    expect(await unspent(lucid, proverUtxos)).toEqual(
      [chunkUtxo!, referenced!].map(label),
    );
  });

  it("coin-select a seed wallet from the coins fundStep read, never one that reaches it before completion", async () => {
    const { emulator, lucid, funderLucid, signer, funder, proverUtxos } =
      await prover([{ lovelace: ada(20n) }, { lovelace: ada(3n) }]);
    const [fee, extra] = proverUtxos;

    const funding = await fundStep(lucid, { signer });
    const lateHash = await (
      await (
        await funderLucid
          .newTx()
          .pay.ToAddress(signer.address, { lovelace: ada(100n) })
          .complete()
      ).sign
        .withWallet()
        .complete()
    ).submit();
    emulator.awaitBlock();
    const late = (await lucid.wallet().getUtxos()).find(
      ({ txHash }) => txHash === lateHash,
    );
    expect(late).toBeDefined();

    const signed = await completeStep(
      funding.tx.pay.ToAddress(funder, { lovelace: ada(21n) }),
      funding,
    );
    expect([...workflowTransactionInputOutRefs(signed)].sort()).toEqual(
      [fee!, extra!].map(label).sort(),
    );
  });

  it("take no completion without the funding fundStep began, so coin selection never reads the whole wallet", () => {
    expectTypeOf(completeStep).parameters.toEqualTypeOf<
      [TxBuilder, StepFunding]
    >();
  });

  it("refuse a wallet with no plain coin outside the step's own", async () => {
    const { lucid, signer, proverUtxos } = await prover([
      { lovelace: ada(100n), datum: Data.void() },
      { lovelace: ada(60n) },
    ]);
    await expect(
      fundStep(lucid, { signer, excluding: [proverUtxos[1]!] }),
    ).rejects.toThrow(
      "Prover wallet must contain a pure-ADA UTxO outside the transaction's own inputs and references for coin selection.",
    );
  });

  it("take a script step's collateral from the restricted wallet's reserved collateral and fund it from reserved funding", async () => {
    const { emulator, lucid, funderLucid, signer, scriptUtxo } = await prover([
      { lovelace: ada(40n) },
      { lovelace: ada(30n) },
    ]);
    const collateral = await fundReservedCollateral({
      emulator,
      funderLucid,
      signer,
    });
    const restricted = await restrictedProverFunding({
      emulator,
      signer,
      collateral,
    });

    const funding = await fundStep(lucid, { signer: restricted.signer });
    const signed = await completeStep(
      funding.tx
        .collectFrom([scriptUtxo], Data.void())
        .attach.SpendingValidator(validator),
      funding,
    );
    await submitStep(lucid, signed, {
      referenceScripts: [],
      boundary: restricted.preSubmitBoundary,
      awaitConfirmation: true,
    });

    expect(restricted.checked()).toBe(1);
    expect(workflowTransactionCollateralInputOutRefs(signed)).toEqual([
      label(collateral),
    ]);
    expect(await unspent(lucid, [collateral])).toEqual([label(collateral)]);
  });
});

describe("submitStep", () => {
  const signedStep = async () => {
    const setup = await prover([{ lovelace: ada(20n) }]);
    const funding = await fundStep(setup.lucid, { signer: setup.signer });
    const signed = await completeStep(
      funding.tx.pay.ToAddress(setup.funder, { lovelace: ada(5n) }),
      funding,
    );
    return { ...setup, signed };
  };

  it("hands the boundary the transaction and its reference scripts, and submits nothing when the boundary refuses", async () => {
    const { emulator, lucid, signed, proverUtxos } = await signedStep();
    const referenceScripts = [
      { role: "step", outRef: `${"cd".repeat(32)}#0`, scriptHash: "ef" },
    ];
    let seen: unknown;
    await expect(
      submitStep(lucid, signed, {
        referenceScripts,
        boundary: (transaction) => {
          seen = transaction;
          throw new Error("refused at the boundary");
        },
        awaitConfirmation: true,
      }),
    ).rejects.toThrow("refused at the boundary");

    expect(seen).toEqual({
      txHash: signed.toHash(),
      signed,
      referenceScripts,
    });
    emulator.awaitBlock();
    expect(await unspent(lucid, proverUtxos)).toEqual(proverUtxos.map(label));
  });

  it("returns the hash once the transaction confirms, and refuses a provider that reports another", async () => {
    const accepted = await signedStep();
    expect(
      await submitStep(accepted.lucid, accepted.signed, {
        referenceScripts: [],
        awaitConfirmation: true,
      }),
    ).toBe(accepted.signed.toHash());
    expect(await unspent(accepted.lucid, accepted.proverUtxos)).toEqual([]);

    const { emulator, lucid, signed } = await signedStep();
    const otherHash = "00".repeat(32);
    const provider = layerMethod(
      emulator,
      "submitTx",
      () => async () => otherHash,
    );
    try {
      await expect(
        submitStep(lucid, signed, {
          referenceScripts: [],
          awaitConfirmation: false,
        }),
      ).rejects.toThrow(
        `provider returned transaction hash ${otherHash}, expected ${signed.toHash()}`,
      );
    } finally {
      provider.restore();
    }
  });
});
