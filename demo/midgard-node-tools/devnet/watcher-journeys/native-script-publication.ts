import { readFile } from "node:fs/promises";

import {
  CML,
  type LucidEvolution,
  type Network,
  paymentCredentialOf,
  type Script,
  scriptFromNative,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { writeJourneyArtifact } from "./artifacts.js";

export const JOURNEY_NATIVE_REFERENCE_SCRIPT: Script = {
  type: "Native",
  script: "820180",
};
export const JOURNEY_NATIVE_REFERENCE_HASH = validatorToScriptHash(
  JOURNEY_NATIVE_REFERENCE_SCRIPT,
);

type PublicationJournal = {
  schemaVersion: "midgard-journey-native-script-publication-v1";
  deploymentFingerprint: string;
  ownerAddress: string;
  destination: string;
  scriptHash: string;
  txHash: string;
  signedCbor: string;
  inputs: { txHash: string; outputIndex: number }[];
  outputIndex: number;
  outcome: "prepared" | "submitted" | "confirmed" | "unknown";
};

/**
 * Publish an arbitrary native preimage without deployment authority or role tokens.
 * Deployment references continue to use the existing bounded publication chain.
 * This reference lives under a separate signature-locked address.
 * Ordinary owner-wallet selection cannot consume the resulting reference.
 * The caller supplies its existing Lucid/provider and owns when this runs.
 */
export const publishJourneyNativeScriptReference = async (input: {
  lucid: LucidEvolution;
  network: Network;
  deploymentFingerprint: string;
  journalPath: string;
}) => {
  const { lucid } = input;
  const provider = lucid.config().provider;
  if (provider === undefined)
    throw new Error("Native publication requires the configured provider");
  const ownerAddress = await lucid.wallet().address();
  const owner = paymentCredentialOf(ownerAddress);
  if (owner.type !== "Key")
    throw new Error("Native publication requires a key-owned funding wallet");
  const lock = scriptFromNative({ type: "sig", keyHash: owner.hash });
  const destination = validatorToAddress(input.network, lock);
  let journal: PublicationJournal;
  try {
    journal = JSON.parse(
      await readFile(input.journalPath, "utf8"),
    ) as PublicationJournal;
  } catch (cause) {
    if (!(cause instanceof Error && "code" in cause && cause.code === "ENOENT"))
      throw cause;
    const funds = (await lucid.utxosAt(ownerAddress))
      .filter(
        (utxo) =>
          utxo.datum == null &&
          utxo.datumHash == null &&
          utxo.scriptRef == null &&
          Object.keys(utxo.assets).every((unit) => unit === "lovelace") &&
          utxo.assets.lovelace >= 5_000_000n,
      )
      .sort(
        (a, b) =>
          a.txHash.localeCompare(b.txHash) || a.outputIndex - b.outputIndex,
      );
    const funding = funds[0];
    if (funding === undefined)
      throw new Error(
        "Native publication needs one ordinary funding input of at least 5 ADA",
      );
    const unsigned = await lucid
      .newTx()
      .collectFrom([funding])
      .pay.ToAddressWithData(
        destination,
        undefined,
        { lovelace: 3_000_000n },
        JOURNEY_NATIVE_REFERENCE_SCRIPT,
      )
      .complete({
        coinSelection: false,
        localUPLCEval: true,
        changeAddress: ownerAddress,
      });
    const signed = await unsigned.sign.withWallet().complete();
    const transaction = CML.Transaction.from_cbor_hex(signed.toCBOR());
    const outputs = transaction.body().outputs();
    const indices = Array.from(
      { length: outputs.len() },
      (_, index) => index,
    ).filter(
      (index) =>
        outputs
          .get(index)
          .script_ref()
          ?.as_native()
          ?.to_canonical_cbor_hex() === JOURNEY_NATIVE_REFERENCE_SCRIPT.script,
    );
    if (indices.length !== 1)
      throw new Error(
        "Native publication did not construct exactly one reference output",
      );
    journal = {
      schemaVersion: "midgard-journey-native-script-publication-v1",
      deploymentFingerprint: input.deploymentFingerprint,
      ownerAddress,
      destination,
      scriptHash: JOURNEY_NATIVE_REFERENCE_HASH,
      txHash: signed.toHash(),
      signedCbor: signed.toCBOR(),
      inputs: [{ txHash: funding.txHash, outputIndex: funding.outputIndex }],
      outputIndex: indices[0]!,
      outcome: "prepared",
    };
    await writeJourneyArtifact(input.journalPath, journal);
  }
  const transaction = CML.Transaction.from_cbor_hex(journal.signedCbor);
  const body = transaction.body();
  const bodyInputs = body.inputs();
  const recordedInputs = Array.from(
    { length: bodyInputs.len() },
    (_, index) => ({
      txHash: bodyInputs.get(index).transaction_id().to_hex(),
      outputIndex: Number(bodyInputs.get(index).index()),
    }),
  );
  if (
    journal.schemaVersion !== "midgard-journey-native-script-publication-v1" ||
    journal.deploymentFingerprint !== input.deploymentFingerprint ||
    journal.ownerAddress !== ownerAddress ||
    journal.destination !== destination ||
    journal.scriptHash !== JOURNEY_NATIVE_REFERENCE_HASH ||
    CML.hash_transaction(body).to_hex() !== journal.txHash ||
    JSON.stringify(recordedInputs) !== JSON.stringify(journal.inputs) ||
    body.outputs().get(journal.outputIndex).address().to_bech32() !==
      destination ||
    body
      .outputs()
      .get(journal.outputIndex)
      .script_ref()
      ?.as_native()
      ?.to_canonical_cbor_hex() !== JOURNEY_NATIVE_REFERENCE_SCRIPT.script
  )
    throw new Error(
      "Native publication journal changed its deployment, inputs or exact signed transaction",
    );
  const ref = { txHash: journal.txHash, outputIndex: journal.outputIndex };
  const observe = async () => {
    const outputs = await provider.getUtxosByOutRef([ref]);
    if (outputs.length === 0) return undefined;
    if (
      outputs.length !== 1 ||
      outputs[0]!.address !== destination ||
      outputs[0]!.scriptRef === undefined ||
      validatorToScriptHash(outputs[0]!.scriptRef!) !==
        JOURNEY_NATIVE_REFERENCE_HASH
    )
      throw new Error(
        "Observed native publication output differs from signed record",
      );
    return outputs[0]!;
  };
  let reference = await observe();
  if (reference === undefined) {
    const unspent = await provider.getUtxosByOutRef(journal.inputs);
    if (unspent.length !== journal.inputs.length) {
      journal.outcome = "unknown";
      await writeJourneyArtifact(input.journalPath, journal);
      throw new Error(
        "Native publication inputs are spent but its reference is absent; canonical reconciliation is required",
      );
    }
    try {
      const hash = await provider.submitTx(journal.signedCbor);
      if (hash !== journal.txHash)
        throw new Error("Native publication provider returned another hash");
      journal.outcome = "submitted";
      await writeJourneyArtifact(input.journalPath, journal);
      if (!(await provider.awaitTx(journal.txHash)))
        throw new Error("Native publication confirmation is unresolved");
      reference = await observe();
      if (reference === undefined)
        throw new Error("Confirmed native publication reference is absent");
    } catch (cause) {
      journal.outcome = "unknown";
      await writeJourneyArtifact(input.journalPath, journal);
      throw new Error(
        `Native publication ${journal.txHash} requires reconciliation`,
        { cause },
      );
    }
  }
  journal.outcome = "confirmed";
  await writeJourneyArtifact(input.journalPath, journal);
  return {
    reference,
    txHash: journal.txHash,
    signedCbor: journal.signedCbor,
    destination,
    scriptHash: JOURNEY_NATIVE_REFERENCE_HASH,
  };
};
