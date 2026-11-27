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
  type TxSigned,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, expectTypeOf, it } from "vitest";

import { resolveProverSigner } from "../src/runtime.js";
import {
  completeStep,
  finishStep,
  fundStep,
  type StepFunding,
} from "../src/step-completion.js";
import {
  submitOutsideWorkflow,
  workflowTransactionInputOutRefs,
} from "../src/workflow/transaction-boundary.js";
import { restrictedProverWallet } from "./support/restricted-prover-wallet.js";

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

/** A prover holding `coins`, in order, beside a funder. A coin with a datum
 * stands for a published proof chunk, at the prover's proof-materials
 * address; the others are its wallet's, at its funding address. */
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
      address:
        datum === undefined ? signer.address : signer.proofMaterialsAddress,
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
    await submitOutsideWorkflow(
      lucid,
      finishStep(signed, { referenceScripts: [] }),
      {
        label: "test step-01",
        awaitConfirmation: true,
      },
    );
    expect(await unspent(lucid, proverUtxos)).toEqual(
      [chunkUtxo!, referenced!].map(label),
    );
  });

  it("coin-select a seed wallet from the plain coins fundStep read, never a coin or the prover's own carriage that reaches it before completion", async () => {
    const { emulator, lucid, funderLucid, signer, funder, proverUtxos } =
      await prover([{ lovelace: ada(20n) }, { lovelace: ada(3n) }]);
    const [fee, extra] = proverUtxos;

    const funding = await fundStep(lucid, { signer });
    // A plain coin and a tagged refund reach the wallet, the prover's own
    // carriage its proof-materials address.
    const carriage = Data.to(new Constr(0, ["cd".repeat(32)]));
    const tag = Data.to(new Constr(0, ["ef".repeat(32), 0n]));
    const lateHash = await (
      await (
        await funderLucid
          .newTx()
          .pay.ToAddress(signer.address, { lovelace: ada(100n) })
          .pay.ToAddressWithData(
            signer.address,
            { kind: "inline", value: tag },
            { lovelace: ada(150n) },
          )
          .pay.ToAddressWithData(
            signer.proofMaterialsAddress,
            { kind: "inline", value: carriage },
            { lovelace: ada(200n) },
          )
          .complete()
      ).sign
        .withWallet()
        .complete()
    ).submit();
    emulator.awaitBlock();
    const late = (await lucid.wallet().getUtxos()).filter(
      ({ txHash }) => txHash === lateHash,
    );
    expect(late.map(({ datum }) => datum ?? null).sort()).toEqual(
      [tag, null].sort(),
    );

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

  it("fund a script step and post its collateral from the coins the restricted wallet offers", async () => {
    const { emulator, lucid, signer, scriptUtxo } = await prover([
      { lovelace: ada(40n) },
      { lovelace: ada(30n) },
    ]);
    const restricted = restrictedProverWallet({ emulator, signer });

    const funding = await fundStep(lucid, { signer: restricted.signer });
    const signed = await completeStep(
      funding.tx
        .collectFrom([scriptUtxo], Data.void())
        .attach.SpendingValidator(validator),
      funding,
    );
    const transaction = finishStep(signed, { referenceScripts: [] });
    await restricted.beforeSubmit(transaction);
    await submitOutsideWorkflow(lucid, transaction, {
      label: "test step-01",
      awaitConfirmation: true,
    });

    expect(restricted.checked()).toBe(1);
  });
});

describe("finishStep", () => {
  it("returns the signed step with its hash and the reference scripts it was given, and submits nothing", async () => {
    const setup = await prover([{ lovelace: ada(20n) }]);
    const funding = await fundStep(setup.lucid, { signer: setup.signer });
    const signed = await completeStep(
      funding.tx.pay.ToAddress(setup.funder, { lovelace: ada(5n) }),
      funding,
    );
    const referenceScripts = [
      { role: "step", outRef: `${"cd".repeat(32)}#0`, scriptHash: "ef" },
    ];

    expect(finishStep(signed, { referenceScripts })).toEqual({
      txHash: signed.toHash(),
      signed,
      referenceScripts,
    });
    setup.emulator.awaitBlock();
    expect(await unspent(setup.lucid, setup.proverUtxos)).toEqual(
      setup.proverUtxos.map(label),
    );
  });

  it("refuses a signed body whose hash is not 32 bytes of hex", () => {
    const signed = { toHash: () => "ab".repeat(31) } as unknown as TxSigned;
    expect(() => finishStep(signed, { referenceScripts: [] })).toThrow(
      "locally evaluated transaction returned an invalid body hash",
    );
  });

  it("keeps exactly the reference-script candidates the signed body reads", async () => {
    const { emulator, lucid, funderLucid, signer, funder, scriptUtxo } =
      await prover([{ lovelace: ada(40n) }, { lovelace: ada(30n) }]);
    const publishHash = await (
      await (
        await funderLucid
          .newTx()
          .pay.ToAddressWithData(
            funder,
            { kind: "inline", value: Data.void() },
            { lovelace: ada(20n) },
            validator,
          )
          .complete()
      ).sign
        .withWallet()
        .complete()
    ).submit();
    emulator.awaitBlock();
    const [published] = await lucid.utxosByOutRef([
      { txHash: publishHash, outputIndex: 0 },
    ]);
    expect(published?.scriptRef).toBeDefined();
    const [unread] = await lucid.utxosByOutRef([
      { txHash: publishHash, outputIndex: 1 },
    ]);

    const funding = await fundStep(lucid, { signer });
    const signed = await completeStep(
      funding.tx.collectFrom([scriptUtxo], Data.void()).readFrom([published!]),
      funding,
    );

    expect(
      finishStep(signed, {
        referenceScriptCandidates: [
          { role: "absent", utxo: undefined },
          { role: "unread", utxo: unread },
          { role: "step", utxo: published, expectedScript: validator },
        ],
      }).referenceScripts,
    ).toEqual([
      {
        role: "step",
        outRef: label(published!),
        scriptHash: validatorToScriptHash(validator),
      },
    ]);
  });
});

describe("submitOutsideWorkflow", () => {
  const finishedStep = async () => {
    const setup = await prover([{ lovelace: ada(20n) }]);
    const funding = await fundStep(setup.lucid, { signer: setup.signer });
    const signed = await completeStep(
      funding.tx.pay.ToAddress(setup.funder, { lovelace: ada(5n) }),
      funding,
    );
    return {
      ...setup,
      signed,
      transaction: finishStep(signed, { referenceScripts: [] }),
    };
  };

  it("returns the hash and waits for confirmation only when asked", async () => {
    const pending = await finishedStep();
    expect(
      await submitOutsideWorkflow(pending.lucid, pending.transaction, {
        label: "test step-01",
        awaitConfirmation: false,
      }),
    ).toBe(pending.signed.toHash());
    expect(
      await pending.emulator.getTransactionStatus(pending.signed.toHash()),
    ).toMatchObject({ status: "pending" });

    const confirmed = await finishedStep();
    expect(
      await submitOutsideWorkflow(confirmed.lucid, confirmed.transaction, {
        label: "test step-01",
        awaitConfirmation: true,
      }),
    ).toBe(confirmed.signed.toHash());
    expect(
      await confirmed.emulator.getTransactionStatus(confirmed.signed.toHash()),
    ).toMatchObject({ status: "confirmed" });
  });

  it("refuses a provider that reports another hash, naming the transaction", async () => {
    const { emulator, lucid, signed, transaction } = await finishedStep();
    const otherHash = "00".repeat(32);
    const provider = layerMethod(
      emulator,
      "submitTx",
      () => async () => otherHash,
    );
    try {
      await expect(
        submitOutsideWorkflow(lucid, transaction, {
          label: "test step-01",
          awaitConfirmation: false,
        }),
      ).rejects.toThrow(
        `test step-01: provider returned transaction hash ${otherHash}, expected ${signed.toHash()}`,
      );
    } finally {
      provider.restore();
    }
  });
});
