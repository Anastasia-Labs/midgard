/**
 * A forced order is due by inclusion time alone. Its native validity interval
 * is in slots and only decides the machine verdict at `block_slot`; it must
 * never make an honest inclusion convictable nor an omission unconvictable.
 * The order below carries a bounded slot interval containing the block slot,
 * so a slot/millisecond mix-up in the due check would read it as never due.
 */
import {
  computeMidgardForcedTxProofCommitment,
  computeMidgardNativeTxId,
  outRefLabel,
} from "@al-ft/midgard-core";
import {
  deriveMidgardForcedTxProofSource,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildCountedRoot,
  buildOmittedDueL1EventFault,
  submitTransitionTraceProof,
  transitionTraceFinalIndex,
} from "../src/index.js";
import {
  alignedHeaderStart,
  removeAndAssertPermanentProof,
} from "./submit-init-emulator-transition-trace-subvariants.remove-and-assert-permanent-proof.js";
import {
  firstThreadUtxo,
  makeHarness,
  reconstruct,
  setupChallenge,
} from "./submit-init-emulator-transition-trace-subvariants.setup-challenge.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import {
  expectSingleUtxoWithUnit,
  funderPaymentKeyHash,
  ledgerOrderedIndex,
  makeHeader,
  network,
  transitionTraceOutRef,
} from "./support/submit-init-emulator-shared.js";

describe("transition-trace forced order validity interval", () => {
  it.each(["honest-inclusion", "late-inclusion", "omission"] as const)(
    "judges a bounded-validity forced order by inclusion time alone (%s)",
    async (scenario) => {
      const { harness, publications, transitionTraceReferenceScripts } =
        await makeHarness();
      const operator = await funderPaymentKeyHash(harness.funderLucid);
      const baseHeader = makeHeader(
        operator,
        await alignedHeaderStart(harness),
      );
      const blockSlot = BigInt(
        harness.funderLucid.unixTimeToSlot(Number(baseHeader.endTime)),
      );
      const id = transitionTraceOutRef("92");
      const submitted = makeNativeTx({
        spendInputCbors: [],
        outputCbors: [],
        fee: 0n,
        validityIntervalStart: blockSlot - 10n,
        validityIntervalEnd: blockSlot + 10n,
      });
      const source = deriveMidgardForcedTxProofSource(
        materializeMidgardForcedTxFromCanonical(submitted),
      );
      const submittedSource: SDK.ForcedTxProofSource = {
        compact_cbor: source.compactCbor.toString("hex"),
        witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          source.fieldPreimageLengthsCbor.toString("hex"),
      };
      const committed: SDK.ForcedInclusionTxV1 = {
        tx_id: computeMidgardNativeTxId(submitted).toString("hex"),
        submitted_source: submittedSource,
        verdict: "ForcedTxValid",
      };
      const root = await buildCountedRoot(
        SDK.ROOT_DOMAINS.forcedTransactionsV1,
        [
          {
            key: Buffer.from(Data.to(id, SDK.OutputReference), "hex"),
            value: Buffer.from(
              Data.to(committed, SDK.ForcedInclusionTxV1),
              "hex",
            ),
          },
        ],
      );
      const header: SDK.Header =
        scenario === "omission"
          ? { ...baseHeader, blockSlot }
          : {
              ...baseHeader,
              blockSlot,
              forcedTransactionsRoot: root.root,
              forcedTransactionCount: 1n,
              totalEventCount: 1n,
              transitionStepCount: 1n,
              validationTraceCount: 1n,
              transitionTraceRoot: "ab".repeat(32),
              eventToStepRoot: "bc".repeat(32),
              validationTracesRoot: "cd".repeat(32),
            };
      const lifecycle = await setupChallenge({
        harness,
        publications,
        transitionTraceReferenceScripts,
        header,
      });
      const assetName = "97",
        unit = toUnit(harness.contracts.txOrder.policyId, assetName);
      const datum: SDK.TxOrderDatum = {
        event: {
          id,
          tx: {
            tx_id: committed.tx_id,
            submitted_source: submittedSource,
            transaction_commitment: Buffer.from(
              computeMidgardForcedTxProofCommitment(source),
            ).toString("hex"),
          },
        },
        inclusion_time:
          scenario === "late-inclusion" ? header.endTime + 1n : header.endTime,
        witness: "76".repeat(28),
        refund_address: {
          paymentCredential: { PublicKeyCredential: ["77".repeat(28)] },
          stakeCredential: null,
        },
        refund_datum: "NoDatum",
      };
      const signed = await (
        await harness.funderLucid
          .newTx()
          .mintAssets({ [unit]: 1n }, Data.void())
          .pay.ToContract(
            harness.contracts.txOrder.spendingScriptAddress,
            { kind: "inline", value: Data.to(datum, SDK.TxOrderDatum) },
            { lovelace: 5000000n, [unit]: 1n },
          )
          .attach.MintingPolicy(harness.contracts.txOrder.mintingScript)
          .complete()
      ).sign
        .withWallet()
        .complete();
      await harness.funderLucid.awaitTx(await signed.submit());
      const event = await expectSingleUtxoWithUnit(
        harness.funderLucid,
        harness.contracts.txOrder.spendingScriptAddress,
        unit,
      );
      const refs = [
        lifecycle.setup.hubOracle,
        transitionTraceReferenceScripts.fraudProofTransitionTraceL1Event!.utxo,
        harness.witnessReferenceScripts.computationThreadMint!,
        harness.witnessReferenceScripts.fraudProofMint!,
        event,
      ];
      const eventRefInputIndex = ledgerOrderedIndex(
        refs,
        event,
        "bounded-validity order",
      );
      const fault: SDK.TransitionFault =
        scenario === "omission"
          ? await buildOmittedDueL1EventFault({
              reconstruction: await reconstruct({ header }),
              evidence: {
                kind: "forcedTransaction",
                txOrderId: id,
                eventRefInputIndex,
                eventAssetName: assetName,
                validityOverride: committed.verdict,
              },
            })
          : {
              OutOfWindowSourceEvent: {
                witness: {
                  OutOfWindowForcedTransaction: {
                    event_ref_input_index: eventRefInputIndex,
                    event_asset_name: assetName,
                    validity_override: committed.verdict,
                    source_membership: {
                      domain: root.domain,
                      root: root.root,
                      phas_root: root.phasRoot,
                      count: root.count,
                      key: id,
                      value: committed,
                      proof: [],
                    },
                  },
                },
              },
            };
      const proof: SDK.TransitionFaultProof = {
        challenged_header_hash: lifecycle.setup.headerHash,
        header,
        fault,
      };
      expect(transitionTraceFinalIndex(proof)).toBe(6);
      const thread = await firstThreadUtxo({ harness, init: lifecycle.init });
      const run = () =>
        submitTransitionTraceProof({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          deploymentInfo: lifecycle.deploymentInfo,
          network,
          signer: harness.proverSigner,
          threadOutRef: outRefLabel(thread),
          proof,
          additionalReferenceInputs: [event],
          witnessReferenceScripts: harness.witnessReferenceScripts,
          awaitConfirmation: true,
        });
      // The honest and late inclusions differ only in the order's inclusion
      // time, and the late one passes the same L1-event yield withdrawal, so
      // the honest refusal there is the due-window check and nothing else.
      if (scenario === "honest-inclusion")
        await expect(run()).rejects.toThrow(
          /failed script execution Withdraw\[0\]/u,
        );
      else
        await removeAndAssertPermanentProof({
          harness,
          setup: lifecycle.setup,
          deploymentInfo: lifecycle.deploymentInfo,
          proofResult: await run(),
        });
    },
    180_000,
  );
});
