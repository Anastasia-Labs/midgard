import {
  encodeMidgardFieldPreimage,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  buildDirectSourceIndexEvidence,
  prepareDirectScriptMaterial,
} from "../../src/direct-script-derivation/index.js";
import {
  checkLedgerAgainstRoot,
  ledgerEntriesRoot,
} from "../../src/ledger-at.js";
import { reconstructDaPayload } from "../../src/transition-trace/reconstruct.js";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
} from "../helpers/canonical-block-evidence-fixture.js";

/** Genuine sixteen-entry predecessor trie, eight actual items in each input
 * field, authenticated no-script None cases, duplicate reference/witness
 * sources and canonical out-ref priority across both fields. */
export const directSourceBatchFixture = async (
  options: { readonly maximumWitnessField?: boolean } = {},
) => {
  const scripts = Array.from({ length: 8 }, (_, index) => ({
    language: "PlutusV3" as const,
    scriptBytes: Buffer.from([1, index]),
  }));
  const input = (byte: number, index: number) =>
    encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, byte),
      outputIndex: index,
    });
  const spend = scripts.map((_, index) => input(0x20 + index, index));
  const reference = scripts.map((_, index) => input(0x10 + index, index));
  const entries = [...spend, ...reference].map((outRef, index) => ({
    outRef,
    output: encodeMidgardTxOutput({
      address: Buffer.from("70" + "22".repeat(28), "hex"),
      value: { lovelace: 5_000_000n, assets: new Map() },
      ...(index % 8 === 0 ? {} : { script_ref: scripts[index % 8]! }),
    }),
  }));
  const root = await ledgerEntriesRoot(entries);
  const predecessor = await checkLedgerAgainstRoot({
    entries,
    utxosRoot: root,
    label: "N6 actual sixteen-entry predecessor",
  });
  // One field array head + one outer bstr head + the versioned-script array,
  // tag and inner bstr head use nine bytes at this actual payload width.
  const scriptWitnesses = options.maximumWitnessField
    ? [
        encodeMidgardVersionedScript({
          language: "PlutusV3",
          scriptBytes: Buffer.alloc(32768 - 9, 0x2a),
        }),
      ]
    : scripts.map(encodeMidgardVersionedScript);
  if (
    options.maximumWitnessField &&
    encodeMidgardFieldPreimage(scriptWitnesses).length !== 32768
  )
    throw new Error(
      "N6 maximum source witness field is not exactly 32768 bytes",
    );
  const transaction = buildFixtureTransaction({
    spendInputs: spend,
    referenceInputs: reference,
    scriptWitnesses,
    fee: 0n,
  });
  const eventKey: SDK.EventKey = {
    L2TransactionEventKey: { tx_id: transaction.txId },
  };
  const fixture = await buildCanonicalBlockFixture({
    transactions: [transaction],
    prevUtxosRoot: root,
    transitionSteps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: eventKey,
        phase: "L2Transaction",
        pre_utxos_root: root,
        post_utxos_root: root,
      },
    ],
  });
  const material = await prepareDirectScriptMaterial({
    current: await reconstructDaPayload({
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    }),
    eventKey,
    predecessor,
  });
  const evidence = await buildDirectSourceIndexEvidence(material);
  const steps = [...evidence.steps];
  const inputs = evidence.inputs.map((entry) => ({
    ...entry,
    step: entry.source === null ? null : steps.shift()!,
  }));
  return { material, evidence, inputs, witnessSteps: steps };
};

export const directSourceAuthenticationRedeemer =
  (witnesses: SDK.DirectResolvedSourceWitness[]) =>
  (indices: { input_index: bigint; output_index: bigint }) =>
    Data.to(
      { Continue: [{ ...indices, witnesses }] },
      SDK.DirectSourceAuthenticateRedeemer,
    );
export const directSourceAdmissionRedeemer =
  (admissions: (SDK.DirectSourceAdmission | null)[]) =>
  (indices: { input_index: bigint; output_index: bigint }) =>
    Data.to(
      { Continue: [{ ...indices, admissions }] },
      SDK.DirectSourceMapRedeemer,
    );
