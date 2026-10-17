import { writeFile } from "node:fs/promises";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  committeeSignerIndex,
  daLocalSigners,
} from "midgard-node/da/local-signers";
import { expect, it } from "vitest";

import { loadJourneyContext } from "./live-context.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;

it.skipIf(
  runDirectory === undefined ||
    process.env.MIDGARD_WATCHER_DIAGNOSE_ATTESTATION !== "1",
)(
  "evaluates signatures against a pending on-chain DA attestation",
  async () => {
    const { deployment, accounts, provider } = await loadJourneyContext(
      runDirectory!,
    );
    const evaluate = provider.evaluateTx.bind(provider);
    provider.evaluateTx = async (tx, additionalUtxos) => {
      await writeFile(
        join(runDirectory!, "work/attestation-evaluation-request.json"),
        JSON.stringify({
          jsonrpc: "2.0",
          method: "evaluateTransaction",
          params: { transaction: { cbor: tx } },
          id: "attestation-diagnosis",
        }),
      );
      return evaluate(tx, additionalUtxos);
    };
    const { operatorLucid: lucid, contracts, references } = deployment;
    const pending = (
      await lucid.utxosAt(contracts.daAttestation.spendingScriptAddress)
    )
      .map((utxo) => ({
        utxo,
        datum: Data.from(utxo.datum!, SDK.DaAttestationDatum),
      }))
      .filter(({ datum }) => datum.attestation_count === 0n);
    expect(pending).toHaveLength(1);
    const attestation = pending[0]!;
    const params = await lucid.utxosAtWithUnit(
      contracts.daParamsGovernor.spendingScriptAddress,
      SDK.daParamsUnit(contracts.daParamsGovernor),
    );
    expect(params).toHaveLength(1);
    const daParamsUtxo = params[0]!;
    const daParamsDatum = Data.from(daParamsUtxo.datum!, SDK.DaParamsDatum);
    const message = SDK.daAvailabilityAttestationMessage(
      attestation.datum.availability_commitment,
    );
    const witnesses = daLocalSigners({
      NETWORK: "Custom",
      L1_OPERATOR_SEED_PHRASE: accounts.operator.seedPhrase,
      DA_COSIGNER_SEED_PHRASE: accounts.cosigner.seedPhrase,
    }).map((signer) => {
      const signerIndex = committeeSignerIndex(
        daParamsDatum.committee,
        signer.verificationKeyHex,
      );
      if (signerIndex === null)
        throw new Error("Signer is absent from committee");
      return { signerIndex, signatureHex: signer.sign(message) };
    });
    const builder = await Effect.runPromise(
      SDK.incompleteAddDaAttestationSignaturesTxProgram(lucid, contracts, {
        daParamsUtxo,
        daParamsDatum,
        attestation,
        witnesses,
        referenceScripts: {
          daAttestationSpending: references.get("daAttestationSpend")!,
        },
      }),
    );
    await builder.complete({
      localUPLCEval:
        process.env.MIDGARD_WATCHER_ATTESTATION_EVALUATOR !== "node",
    });
  },
  60_000,
);
