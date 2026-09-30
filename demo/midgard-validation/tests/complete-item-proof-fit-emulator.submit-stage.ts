import {
  requireInputIndex,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Constr,
  credentialToAddress,
  Data,
  type Script,
  scriptHashToCredential,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type CompleteSignedTransactionMeasurement,
  type EmulatorHarness,
  measureCompleteSignedTransaction,
  sameDatumValue,
  type StageContract,
} from "./complete-item-proof-fit-emulator.build-canonical-decode-item-case.js";
import { type CanonicalDecodeItemCase } from "./complete-item-proof-fit-emulator.build-trace-with-outputs.js";

// FLIPPED TO THE OPTION B PIPELINE (#617 wave sign-off, item 1; #620/#621/#622).
// This harness used to speak the wire #620 retired — the four-field `Verify`,
// the `VerifyReference` arm, and the two-part evidence hash — and to measure
// the AUTHENTICATE stage, because that was the stage the §5.1 preimage rode.
// Since Option B the preimage rides the observe stage's §8.8 door, `Verify` is
// `(input_index, output_index, transition)`, the semantic reference arm is
// gone, and `evidence_hash` commits to `(transition, NoAuxiliaryWitness)`. So
// the size-bearing rows below measure the OBSERVE transaction, and the harness
// walks the real staged chain (authenticate -> source -> observe) to reach it
// rather than stopping at the first stage.
//
// Framing note (#622 caveat 1, carried deliberately): the byte counts this
// harness measures are its OWN framing's, not the production journey's. The
// consensus-profile rows are pinned from the production journey
// (`demo/midgard-fault-proofs/tests/submit-init-emulator-option-b-*.test.ts`);
// every assertion here is therefore a RELATION (fits / does not fit / smaller
// than) against those pins, never an equality restating them.

type StageSubmission = {
  readonly nextThreadUtxo: UTxO;
  readonly measurement: CompleteSignedTransactionMeasurement;
  readonly outputDatum: string;
  readonly signedCbor: string;
};

const feeInputFor = async (harness: EmulatorHarness): Promise<UTxO> => {
  const candidates = (await harness.lucid.wallet().getUtxos()).filter(
    (utxo) => utxo.assets[harness.threadUnit] === undefined,
  );
  return candidates.reduce((left, right) =>
    (left.assets.lovelace ?? 0n) >= (right.assets.lovelace ?? 0n)
      ? left
      : right,
  );
};

/**
 * Publishes a validator as a plain reference script, parked at a salted script
 * address so several parked scripts stay individually addressable and none of
 * them is reachable by coin selection. The parking transaction rides the raised
 * emulator ceiling and is not part of any measurement.
 */
export const publishReferenceScript = async (
  harness: EmulatorHarness,
  script: Script,
  salt: string,
): Promise<UTxO> => {
  const parkAddress = credentialToAddress(
    "Custom",
    scriptHashToCredential(salt.repeat(28)),
  );
  const unsigned = await harness.lucid
    .newTx()
    .pay.ToAddressWithData(
      parkAddress,
      undefined,
      { lovelace: 60_000_000n },
      script,
    )
    .complete();
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await harness.lucid.awaitTx(txHash);
  const utxo = (await harness.lucid.utxosAt(parkAddress)).find(
    (candidate) => candidate.txHash === txHash && candidate.scriptRef != null,
  );
  if (utxo === undefined) {
    throw new Error("reference script failed to park");
  }
  return utxo;
};

/**
 * One stage of the staged canonical-decode chain, built, signed and submitted
 * against the applied validators. The thread token and its lovelace are handed
 * on unchanged, exactly as the production submitter hands them on.
 */
const submitStage = async ({
  harness,
  inputUtxo,
  inputContract,
  outputContract,
  outputDatum,
  label,
  encode,
  scriptReference,
  extraReferences,
}: {
  readonly harness: EmulatorHarness;
  readonly inputUtxo: UTxO;
  readonly inputContract: StageContract;
  readonly outputContract: StageContract;
  readonly outputDatum: string;
  readonly label: string;
  readonly encode: (layout: {
    readonly inputIndex: bigint;
    readonly outputIndex: bigint;
    readonly referenceInputIndex: (utxo: UTxO) => bigint;
  }) => string;
  readonly scriptReference?: UTxO;
  readonly extraReferences?: readonly UTxO[];
}): Promise<StageSubmission> => {
  const makeRedeemer: BuildTxWithRedeemer = (ctx) =>
    encode({
      inputIndex: requireInputIndex(ctx, inputUtxo, label),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        (output) =>
          output.address === outputContract.spendingScriptAddress &&
          output.datum != null &&
          sameDatumValue(output.datum, outputDatum) &&
          output.assets[harness.threadUnit] === 1n,
        label,
      ),
      referenceInputIndex: (utxo) =>
        requireReferenceInputIndex(ctx, utxo, label),
    });
  let tx = harness.lucid
    .newTx()
    .collectFrom([await feeInputFor(harness)])
    .collectFrom([inputUtxo], makeRedeemer);
  if (scriptReference !== undefined) {
    tx = tx.readFrom([scriptReference]);
  }
  if (extraReferences !== undefined && extraReferences.length > 0) {
    tx = tx.readFrom([...extraReferences]);
  }
  tx = tx.pay
    .ToContract(
      outputContract.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      {
        lovelace: inputUtxo.assets.lovelace ?? 0n,
        [harness.threadUnit]: 1n,
      },
    )
    .addSignerKey(harness.signerHash);
  if (scriptReference === undefined) {
    tx = tx.attach.SpendingValidator(inputContract.spendingScript);
  }
  let unsigned;
  try {
    unsigned = await tx.complete({ localUPLCEval: true });
  } catch (cause) {
    throw new Error(
      `${label} local evaluation failed: ${
        cause instanceof Error ? cause.message : String(cause)
      }`,
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const signedCbor = signed.toCBOR();
  const txHash = await signed.submit();
  await harness.lucid.awaitTx(txHash);
  const nextThreadUtxo = (
    await harness.lucid.utxosAt(outputContract.spendingScriptAddress)
  ).find(
    (utxo) => utxo.txHash === txHash && utxo.assets[harness.threadUnit] === 1n,
  );
  if (nextThreadUtxo === undefined) {
    throw new Error(`${label} did not hand the thread on`);
  }
  return {
    nextThreadUtxo,
    measurement: measureCompleteSignedTransaction(signedCbor),
    outputDatum,
    signedCbor,
  };
};

/** The `Verify` redeemer, Option B shape. */
const verifyRedeemer = (
  transition: Data,
  layout: {
    readonly inputIndex: bigint;
    readonly outputIndex: bigint;
  },
): string =>
  Data.to(
    new Constr(1, [
      new Constr(0, [layout.inputIndex, layout.outputIndex, transition]),
    ]),
  );

/** The stage-2 `Continue` redeemer (source binding takes indices only). */
const indicesRedeemer = (layout: {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
}): string =>
  Data.to(
    new Constr(1, [new Constr(0, [layout.inputIndex, layout.outputIndex])]),
  );

/**
 * How the §8.8 door is handed the field's §5.1 preimage: inline in the observe
 * redeemer (tier-1 `Inline`, the only tier this harness builds), or by
 * reference to a published proof item.
 */
type ObserveDelivery =
  | { readonly kind: "inline"; readonly fieldPreimageHex?: string }
  | { readonly kind: "reference"; readonly publication: UTxO };

export type CompleteItemJourney = {
  readonly harness: EmulatorHarness;
  readonly itemCase: CanonicalDecodeItemCase;
  readonly authenticate: StageSubmission;
  readonly source: StageSubmission;
  readonly observe: StageSubmission;
};

/**
 * Walks one thread from the semantic resolver to the observe door. Both
 * validators that carry a body worth referencing — the semantic resolver and
 * the observe validator — are sourced from parked reference scripts, the
 * production basis since the #617 reference-script wiring; embedding either
 * would put a validator body inside the transaction whose size is the
 * measurement.
 */
export const runJourneyToObserve = async ({
  harness,
  itemCase,
  threadUtxo,
  delivery,
  observeScriptReference,
  semanticScriptReference,
}: {
  readonly harness: EmulatorHarness;
  readonly itemCase: CanonicalDecodeItemCase;
  readonly threadUtxo: UTxO;
  readonly delivery: ObserveDelivery;
  /**
   * Omitted only by the embedded-basis probe, which attaches the observe
   * validator to its own transaction instead of reading the published copy.
   */
  readonly observeScriptReference?: UTxO;
  readonly semanticScriptReference: UTxO;
}): Promise<CompleteItemJourney> => {
  const semanticContract = {
    spendingScriptAddress: harness.semanticAddress,
    spendingScript: harness.semanticScript,
  };
  const authenticate = await submitStage({
    harness,
    inputUtxo: threadUtxo,
    inputContract: semanticContract,
    outputContract: harness.stages.source,
    outputDatum: itemCase.authenticatedDatum,
    label: "canonical item authentication",
    scriptReference: semanticScriptReference,
    encode: (layout) => verifyRedeemer(itemCase.transitionData, layout),
  });
  const source = await submitStage({
    harness,
    inputUtxo: authenticate.nextThreadUtxo,
    inputContract: harness.stages.source,
    outputContract: harness.stages.observe,
    outputDatum: itemCase.preparedDatum,
    label: "canonical item source binding",
    encode: indicesRedeemer,
  });
  const observe = await submitStage({
    harness,
    inputUtxo: source.nextThreadUtxo,
    inputContract: harness.stages.observe,
    outputContract: harness.stages.proof,
    outputDatum: itemCase.observedDatum,
    label: "canonical item observation",
    ...(observeScriptReference === undefined
      ? {}
      : { scriptReference: observeScriptReference }),
    ...(delivery.kind === "reference"
      ? { extraReferences: [delivery.publication] }
      : {}),
    encode: ({ inputIndex, outputIndex, referenceInputIndex }) =>
      delivery.kind === "inline"
        ? Data.to(
            new Constr(1, [
              new Constr(0, [
                inputIndex,
                outputIndex,
                new Constr(0, [
                  delivery.fieldPreimageHex ?? itemCase.fieldPreimageHex,
                ]),
              ]),
            ]),
          )
        : Data.to(
            new Constr(1, [
              new Constr(1, [
                inputIndex,
                outputIndex,
                referenceInputIndex(delivery.publication),
              ]),
            ]),
          ),
  });
  return { harness, itemCase, authenticate, source, observe };
};
