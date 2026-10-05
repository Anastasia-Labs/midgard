import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  paymentCredentialOf,
  scriptFromNative,
  validatorToAddress,
} from "@lucid-evolution/lucid";

import { LocalKupmiosCheckpointChangedError } from "../src/workflow/index.js";
import {
  ANCESTOR,
  hash,
  TARGET,
  TIP,
} from "./workflow-kupmios-source.ogmios-boundary-socket.js";
import { sourceFixture } from "./workflow-kupmios-source.source-fixture.js";

export const signedRecoveryFixture = async ({
  ttl = 6000,
  spent = false,
  referenceSpent,
  scriptOrdinary = false,
  scriptCollateral = false,
  keyCollateral = false,
  missing = false,
  mempoolPresent = false,
  included = false,
  rollbackDuringInclusion = false,
  rollbackDuringExpiry = false,
  captureHeadChanges = 0,
}: {
  ttl?: number | null;
  spent?: boolean;
  referenceSpent?:
    | "stable"
    | "volatile"
    | "mixed_stable_first"
    | "mixed_volatile_first";
  scriptOrdinary?: boolean;
  scriptCollateral?: boolean;
  keyCollateral?: boolean;
  missing?: boolean;
  mempoolPresent?: boolean;
  included?: boolean;
  rollbackDuringInclusion?: boolean;
  rollbackDuringExpiry?: boolean;
  captureHeadChanges?: number;
} = {}) => {
  const account = generateEmulatorAccount({ lovelace: 1_000_000_000n });
  const emulator = new Emulator([account]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const mixedReferences = referenceSpent?.startsWith("mixed_") === true;
  const volatileReference =
    referenceSpent !== undefined && referenceSpent !== "stable";
  const protocolScript = scriptFromNative({
    type: "sig",
    keyHash: paymentCredentialOf(account.address).hash,
  });
  const protocolAddress = validatorToAddress("Custom", protocolScript);
  const creationBuilder = lucid
    .newTx()
    .pay.ToAddress(scriptOrdinary ? protocolAddress : account.address, {
      lovelace: 10_000_000n,
    });
  if (mixedReferences)
    creationBuilder.pay.ToAddress(account.address, { lovelace: 20_000_000n });
  const creation = await (
    await creationBuilder.complete({ localUPLCEval: true })
  ).sign
    .withWallet()
    .complete();
  await creation.submit();
  emulator.awaitBlock();
  const createdOutputs = [
    ...(await lucid.wallet().getUtxos()),
    ...(scriptOrdinary ? await lucid.utxosAt(protocolAddress) : []),
  ].filter((utxo) => utxo.txHash === creation.toHash());
  const funding = createdOutputs.find((utxo) =>
    referenceSpent === undefined
      ? utxo.assets.lovelace === 10_000_000n
      : utxo.assets.lovelace !== 10_000_000n &&
        (!mixedReferences || utxo.assets.lovelace !== 20_000_000n),
  )!;
  // The reference sorts before funding so recovery must inspect later wallet
  // creation history even after observing a stable invalidating spend.
  const reference =
    referenceSpent === undefined
      ? undefined
      : createdOutputs.find((utxo) => utxo.assets.lovelace === 10_000_000n)!;
  const secondReference = mixedReferences
    ? createdOutputs.find((utxo) => utxo.assets.lovelace === 20_000_000n)!
    : undefined;
  const planned = lucid
    .newTx()
    .collectFrom([funding])
    .pay.ToAddress(account.address, { lovelace: 5_000_000n });
  if (reference !== undefined) {
    if (scriptOrdinary)
      planned
        .collectFrom([reference])
        .attach.SpendingValidator(protocolScript)
        .addSigner(account.address);
    else if (!keyCollateral) planned.readFrom([reference]);
  }
  if (secondReference !== undefined) planned.readFrom([secondReference]);
  if (ttl !== null) planned.validTo(lucid.slotToUnixTime(ttl));
  let signed = await (
    await planned.complete({
      localUPLCEval: true,
      coinSelection: false,
      presetWalletInputs: [funding],
    })
  ).sign
    .withWallet()
    .complete();
  if (scriptCollateral || keyCollateral) {
    // Include recorded collateral in recovery, even when its role overlaps an
    // ordinary input. Every exact input can establish that the body is impossible.
    const body = signed.toTransaction().body();
    const collateral = CML.TransactionInputList.new();
    collateral.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(reference!.txHash),
        BigInt(reference!.outputIndex),
      ),
    );
    body.set_collateral_inputs(collateral);
    signed = await lucid
      .fromTx(
        CML.Transaction.new(
          body,
          CML.TransactionWitnessSet.new(),
          true,
        ).to_cbor_hex(),
      )
      .sign.withWallet()
      .complete();
  }
  const conflictInputs =
    reference === undefined
      ? [funding]
      : [...(spent ? [funding] : []), reference];
  const conflictBuilder = lucid
    .newTx()
    .collectFrom(conflictInputs)
    .pay.ToAddress(account.address, { lovelace: 6_000_000n });
  if (scriptOrdinary)
    conflictBuilder.attach
      .SpendingValidator(protocolScript)
      .addSigner(account.address);
  const conflict = await (
    await conflictBuilder.complete({
      localUPLCEval: true,
      coinSelection: false,
      presetWalletInputs: conflictInputs,
    })
  ).sign
    .withWallet()
    .complete();
  const secondConflict =
    secondReference === undefined
      ? undefined
      : await (
          await lucid
            .newTx()
            .collectFrom([secondReference])
            .pay.ToAddress(account.address, { lovelace: 6_000_000n })
            .complete({
              localUPLCEval: true,
              coinSelection: false,
              presetWalletInputs: [secondReference],
            })
        ).sign
          .withWallet()
          .complete();
  const match = {
    transaction_index: 0,
    transaction_id: creation.toHash(),
    output_index: funding.outputIndex,
    address: funding.address,
    value: { coins: funding.assets.lovelace!.toString(), assets: {} },
    datum_hash: null,
    script_hash: null,
    datum: null,
    script: null,
    created_at: mixedReferences
      ? { slot_no: 380, header_hash: ANCESTOR }
      : { slot_no: 400, header_hash: TARGET },
    spent_at: spent
      ? {
          slot_no: mixedReferences ? 380 : 400,
          header_hash: mixedReferences ? ANCESTOR : TARGET,
          transaction_id: conflict.toHash(),
          input_index: 0,
        }
      : null,
  };
  const referenceMatch =
    reference === undefined
      ? undefined
      : {
          ...match,
          output_index: reference.outputIndex,
          address: reference.address,
          value: { coins: reference.assets.lovelace!.toString(), assets: {} },
          spent_at: {
            slot_no: referenceSpent === "mixed_stable_first" ? 380 : 400,
            header_hash:
              referenceSpent === "mixed_stable_first" ? ANCESTOR : TARGET,
            transaction_id: conflict.toHash(),
            input_index: 0,
          },
        };
  const secondReferenceMatch =
    secondReference === undefined
      ? undefined
      : {
          ...match,
          output_index: secondReference.outputIndex,
          value: {
            coins: secondReference.assets.lovelace!.toString(),
            assets: {},
          },
          spent_at: {
            slot_no: referenceSpent === "mixed_volatile_first" ? 380 : 400,
            header_hash:
              referenceSpent === "mixed_volatile_first" ? ANCESTOR : TARGET,
            transaction_id: secondConflict!.toHash(),
            input_index: 0,
          },
        };
  const submissions: string[] = [];
  let inclusionRead = false;
  let inclusionChecks = 0;
  const includedMatches = Array.from(
    { length: signed.toTransaction().body().outputs().len() },
    (_, index) => {
      const output = signed.toTransaction().body().outputs().get(index);
      return {
        ...match,
        transaction_id: signed.toHash(),
        output_index: index,
        value: { coins: output.amount().coin().toString(), assets: {} },
        spent_at: null,
      };
    },
  );
  const fixture = sourceFixture({
    observationDepth: "inclusion",
    tipHeight: volatileReference ? 2231 : 2232,
    tipSlot: 4719,
    blockTransactions: [
      creation,
      conflict,
      ...(secondConflict === undefined ? [] : [secondConflict]),
      signed,
    ].map((tx) => ({
      id: tx.toHash(),
      cbor: tx.toTransaction().to_cbor_hex(),
    })),
    checkpointOverride: (slot) => {
      const tipSlot = 4719;
      if (slot >= tipSlot) return { slot_no: tipSlot, header_hash: TIP };
      if (
        slot === 400 &&
        inclusionRead &&
        (rollbackDuringExpiry ||
          (rollbackDuringInclusion && ++inclusionChecks >= 2))
      )
        return { slot_no: 400, header_hash: hash(99) };
      return undefined;
    },
    matchesByPattern: (pattern) => {
      if (pattern === `*@${signed.toHash()}` && captureHeadChanges-- > 0)
        throw new LocalKupmiosCheckpointChangedError(
          "Kupo advanced during transaction inclusion capture",
        );
      if (pattern === `*@${signed.toHash()}`) {
        inclusionRead = true;
        return included ? includedMatches : [];
      }
      if (
        reference !== undefined &&
        pattern === `${reference.outputIndex}@${creation.toHash()}`
      )
        return [referenceMatch];
      if (
        secondReference !== undefined &&
        pattern === `${secondReference.outputIndex}@${creation.toHash()}`
      )
        return [secondReferenceMatch];
      return pattern === `${funding.outputIndex}@${creation.toHash()}` &&
        !missing
        ? [match]
        : [];
    },
    socketBehavior: {
      mempoolPresent,
      submit: async (cbor) => {
        submissions.push(cbor);
        return emulator.submitTx(cbor);
      },
    },
  });
  const input = {
    source: fixture.source,
    transactionHash: signed.toHash(),
    signedTransactionCborHex: signed.toTransaction().to_cbor_hex(),
  };
  return {
    ...fixture,
    input,
    signed,
    funding,
    reference,
    lucid,
    emulator,
    submissions,
  };
};
