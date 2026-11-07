import { inspect } from "node:util";

import { computeHash32 } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { faultProofFieldOpening } from "../../src/field-opening.js";
import { createDirectComponentStepper } from "./direct-component-stepper.js";
import {
  directSourceAdmissionRedeemer,
  directSourceAuthenticationRedeemer,
} from "./direct-source-fixture.js";
import type { DirectSourceResourceFixture } from "./direct-source-resource-fixture.js";
import { setupDirectSourceResourceThread } from "./direct-source-resource-setup.js";
import { TRACED_REFUSALS } from "./emulator/traced-refusals.js";
import {
  EMULATOR_PROTOCOL_PARAMETERS,
  measureCompleteSignedTransaction,
} from "./submit-init-emulator-shared.js";

type Transition = Parameters<
  ReturnType<typeof createDirectComponentStepper>["complete"]
>[0];
type SignedMeasurement = {
  bytes: number;
  memory: string;
  steps: string;
  fee: string;
  signedCborBlake2b256: string;
  requiredSigners: string[];
  signingKeys: string[];
  withinLedger: boolean;
  withinAcceptanceMargin: boolean;
};
type Attempt = {
  label: string;
  status: "accepted" | "refused" | "not-submitted-over-ledger";
  phase: string;
  mandatoryDescriptorPayloadBytes: number;
  minimalIndexRedeemerDataBytes: number;
  minimalIndexRedeemerCbor: string;
  completedRedeemerCbor?: string;
  inputThread: UTxO;
  outputDatumCbor: string;
  outputDatumBytes: number;
  signedCbor?: string;
  signed?: SignedMeasurement;
  transactionHash?: string;
  successor?: UTxO;
  error?: string;
  oldThreadStillUnspent?: boolean | null;
  observerError?: string;
};

/** Build/evaluate real transactions through the normal component stepper.
 * Its `complete` seam omits `accept`'s release-margin assertions. We compare
 * ledger and margin separately, without changing protocol parameters. */
export const measureDirectSourceResourcePath = async (
  fixture: DirectSourceResourceFixture,
  budget: 1 | 100,
) => {
  if (TRACED_REFUSALS)
    throw new Error(
      "F2 resource measurement requires untraced scripts and unchanged publication limits",
    );
  const { material, inputs, evidence, root, count } = fixture;
  const setup = await setupDirectSourceResourceThread(fixture);
  const {
    prover,
    certificate,
    roles,
    unit,
    initialDatum,
    assets,
    ownerLucid,
    references,
    publications,
    plan,
    capturedCarriage,
    carriage,
  } = setup;
  let thread = setup.thread;
  const attempts: Attempt[] = [];

  const attempt = async (
    transition: Transition,
    mandatoryDescriptorPayloadBytes = 0,
  ): Promise<Attempt> => {
    const oldThread = thread;
    const minimalIndexRedeemerCbor = transition.redeemer(
      { input_index: 0n, output_index: 0n },
      (field) =>
        faultProofFieldOpening({
          planned: plan.openingPlan,
          referenceInputs: [...(carriage.get(field) ?? [])],
          certificatePolicyId: certificate.policyId,
          label: transition.label,
        }),
    );
    const record: Attempt = {
      label: transition.label,
      status: "refused",
      phase: "completion",
      mandatoryDescriptorPayloadBytes,
      // Minimal nonnegative indices: useful structural measurement, not the
      // completed transaction size. Descriptor payload sum is the strict
      // serialization lower bound independent of index spelling/proof size.
      minimalIndexRedeemerDataBytes: minimalIndexRedeemerCbor.length / 2,
      minimalIndexRedeemerCbor,
      inputThread: oldThread,
      outputDatumCbor: transition.datum,
      outputDatumBytes: transition.datum.length / 2,
    };
    attempts.push(record);
    try {
      // One normal stepper per transaction binds it to the actual requeried
      // predecessor. We do not call accept/refuse or alter their assertions.
      const stepper = createDirectComponentStepper({
        initialThread: oldThread,
        unit,
        references,
        carriage,
        opening: (field, referenceInputs) => {
          expect(field).toBe(0);
          return faultProofFieldOpening({
            planned: plan.openingPlan,
            referenceInputs,
            certificatePolicyId: certificate.policyId,
            label: transition.label,
          });
        },
      });
      const unsigned = await stepper.complete(
        {
          ...transition,
          redeemer: (indices, open) => {
            const cbor = transition.redeemer(indices, open);
            record.completedRedeemerCbor = cbor;
            return cbor;
          },
        },
        ownerLucid,
      );
      record.phase = "signing";
      const signed = await unsigned.sign.withWallet().complete();
      const cbor = signed.toCBOR();
      record.signedCbor = cbor;
      const measured = measureCompleteSignedTransaction(cbor);
      const tx = CML.Transaction.from_cbor_hex(cbor);
      const required = tx.body().required_signers();
      const keys = tx.witness_set().vkeywitnesses();
      record.signed = {
        bytes: measured.completeSignedBytes,
        memory: measured.executionMemory.toString(),
        steps: measured.executionSteps.toString(),
        fee: tx.body().fee().toString(),
        signedCborBlake2b256: computeHash32(Buffer.from(cbor, "hex")).toString(
          "hex",
        ),
        requiredSigners: Array.from(
          { length: required?.len() ?? 0 },
          (_, index) => required!.get(index).to_hex(),
        ),
        signingKeys: Array.from({ length: keys?.len() ?? 0 }, (_, index) =>
          keys!.get(index).vkey().hash().to_hex(),
        ),
        withinLedger:
          measured.completeSignedBytes <=
            EMULATOR_PROTOCOL_PARAMETERS.maxTxSize &&
          measured.executionMemory <= EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem &&
          measured.executionSteps <= EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps,
        withinAcceptanceMargin:
          measured.completeSignedBytes <= 15_872 &&
          measured.executionMemory <= 13_200_000n &&
          measured.executionSteps <= 8_000_000_000n,
      };
      // Every continuation is the prover's to sign.
      expect(record.signed.requiredSigners).toContain(prover);
      expect(record.signed.signingKeys).toContain(prover);
      if (!record.signed.withinLedger) {
        record.status = "not-submitted-over-ledger";
        record.phase = "full-signed-envelope";
        // This is a measured envelope failure, not an observed ledger refusal.
        record.oldThreadStillUnspent = (
          await ownerLucid.utxosByOutRef([
            {
              txHash: oldThread.txHash,
              outputIndex: oldThread.outputIndex,
            },
          ])
        ).some((item) => item.assets[unit] === 1n);
        return record;
      }
      record.phase = "submission";
      const hash = await signed.submit();
      record.transactionHash = hash;
      record.phase = "confirmation-and-output-visibility";
      await ownerLucid.awaitTx(hash);
      const outputs = await ownerLucid.utxosByOutRef(
        Array.from({ length: tx.body().outputs().len() }, (_, outputIndex) => ({
          txHash: hash,
          outputIndex,
        })),
      );
      const successors = outputs.filter((item) => item.assets[unit] === 1n);
      expect(successors).toHaveLength(1);
      const successor = successors[0]!;
      expect(successor.address).toBe(transition.destination);
      expect(Data.from(successor.datum!)).toEqual(Data.from(transition.datum));
      expect(successor.assets).toEqual(assets);
      thread = successor;
      record.successor = successor;
      record.phase = "accepted";
      record.status = "accepted";
      return record;
    } catch (cause) {
      // No expectOnchainRefusal: retain balancing/evaluation/builder errors.
      // A generic refusal is never automatically classified as resource proof.
      record.error = inspect(cause, { depth: 12, maxStringLength: Infinity });
      try {
        const retained = await ownerLucid.utxosByOutRef([
          {
            txHash: oldThread.txHash,
            outputIndex: oldThread.outputIndex,
          },
        ]);
        record.oldThreadStillUnspent = retained.some(
          (item) => item.assets[unit] === 1n,
        );
      } catch (observerCause) {
        record.oldThreadStillUnspent = null;
        record.observerError = inspect(observerCause, {
          depth: 12,
          maxStringLength: Infinity,
        });
      }
      return record;
    }
  };

  const scanDatum = (data: SDK.DirectSourceScanState) =>
    Data.to({ fraud_prover: prover, data }, SDK.DirectSourceScanDatum);
  let state: SDK.DirectSourceScanState = {
    cursor: plan.position("Walking", 0).cursor,
    source_index: { root: SDK.EMPTY_MERKLE_TREE_ROOT, count: 0n },
    complete: false,
  };
  let processed = 0;
  const result = (outcome: string) => ({
    outcome,
    budget,
    count,
    processed,
    root,
    fieldBytes: plan.openingPlan.preimage.length,
    fieldTier: plan.openingPlan.plan.tier,
    prover,
    protocol: {
      maxTxSize: EMULATOR_PROTOCOL_PARAMETERS.maxTxSize,
      maxTxExMem: EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem.toString(),
      maxTxExSteps: EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps.toString(),
    },
    scripts: Object.fromEntries(
      Object.entries(roles).map(([role, value]) => [
        role,
        {
          hash: value.spendingScriptHash,
          script: value.spendingScript,
        },
      ]),
    ),
    sourceInputs: {
      ...fixture.sourceInputs,
      headerCbor: SDK.encodeHeaderCbor(material.sourceBound.header).toString(
        "hex",
      ),
      boundCbor: Data.to(material.bound, SDK.DirectScriptBound),
      initialDatum,
      compactCbor: material.bound.compact_cbor,
      witnessSetCompactCbor:
        material.material.proofSource.witnessSetCompactCbor.toString("hex"),
      fieldPreimageCbor: plan.openingPlan.preimage.toString("hex"),
      outRefs: plan.items.map((item) => item.toString("hex")),
      inputs: inputs.map((entry) => ({
        fieldIndex: entry.fieldIndex,
        itemIndex: entry.itemIndex,
        witness: entry.witness,
        witnessCbor: Data.to(entry.witness, SDK.DirectResolvedSourceWitness),
        source: entry.source.descriptor,
        sourceItemCbor: entry.source.item.toString("hex"),
        admission: entry.step.admission,
        admissionCbor: Data.to(entry.step.admission, SDK.DirectSourceAdmission),
        indexBefore: entry.step.before,
        indexAfter: entry.step.after,
      })),
      fieldCarriageUtxos: capturedCarriage.result,
      fieldCarriageSignedCbors: capturedCarriage.transactionCbors,
    },
    publications,
    attempts,
    finalState: state,
    expectedFinalIndex: evidence.index,
    qualification:
      "Seeded authenticated bound; actual start/select/authenticate/map. No CT Init, full L2 validity, family terminal, proof mint, SQ removal, live-chain or latency claim. No resource limit or acceptance margin was raised.",
  });
  const stopped = (
    door: "select" | "authenticate" | "map",
    receipt: Attempt,
  ) => {
    if (door === "select")
      return {
        ...result("select-refused-inconclusive"),
        notReached: "authenticate and map: no accepted Select output exists",
      };
    // Only bytes that every valid witness must carry establish an unavoidable
    // size failure. An oversized constructed envelope may have another fit.
    const mandatoryByteLowerBoundExceedsLedger =
      receipt.mandatoryDescriptorPayloadBytes >
      EMULATOR_PROTOCOL_PARAMETERS.maxTxSize;
    const outcome =
      mandatoryByteLowerBoundExceedsLedger &&
      receipt.oldThreadStillUnspent === true
        ? `accepted-select-${door}-unserviceable-mandatory-byte-lower-bound`
        : receipt.status === "not-submitted-over-ledger"
          ? `accepted-select-${door}-constructed-attempt-over-ledger`
          : `accepted-select-${door}-refused-unclassified`;
    return {
      ...result(outcome),
      mandatoryByteLowerBoundExceedsLedger,
      notReached:
        door === "authenticate"
          ? "map: authentication did not produce an accepted map input"
          : null,
    };
  };
  const start = await attempt({
    label: "actual source start before selection",
    role: roles.start,
    destination: roles.resolved.spendingScriptAddress,
    datum: scanDatum(state),
    fields: [0],
    redeemer: (indices, open) =>
      Data.to(
        { Continue: [{ ...indices, opening: open(0) }] },
        SDK.DirectSourceStartRedeemer,
      ),
  });
  if (start.status !== "accepted") return result("start-refused-inconclusive");
  for (let first = 0; first < count; first += budget) {
    const end = Math.min(first + budget, count);
    const selected = inputs.slice(first, end);
    const advanced: SDK.DirectSourceScanState = {
      ...state,
      cursor: plan.position("Walking", end).cursor,
      complete: end === count,
    };
    const resolved: SDK.DirectSourceResolvedState = {
      resume_script_hash: roles.resolved.spendingScriptHash,
      resolved: {
        advanced,
        first_item_index: BigInt(first),
        out_refs: plan.items
          .slice(first, end)
          .map((item) => item.toString("hex")),
      },
    };
    const selection = await attempt({
      label: `ResolveScan budget${budget} items ${first}..${end}`,
      role: roles.resolved,
      destination: roles.authenticate.spendingScriptAddress,
      datum: Data.to(
        { fraud_prover: prover, data: resolved },
        SDK.DirectSourceResolvedDatum,
      ),
      fields: [0],
      redeemer: (indices, open) =>
        Data.to(
          {
            Continue: [
              {
                ResolveScan: {
                  ...indices,
                  opening: open(0),
                  checkpoint: plan.position("Walking", first).checkpointBytes,
                  budget: BigInt(budget),
                },
              },
            ],
          },
          SDK.DirectSourceResolveRedeemer,
        ),
    });
    if (selection.status !== "accepted") return stopped("select", selection);
    const pending: SDK.DirectSourcePendingState = {
      resume_script_hash: roles.resolved.spendingScriptHash,
      pending: {
        advanced,
        candidates: selected.map((entry) => entry.source.descriptor),
      },
    };
    const authenticationTransition: Transition = {
      label: `mandatory exact authentication of ${selected.length} selected descriptors`,
      role: roles.authenticate,
      destination: roles.map.spendingScriptAddress,
      datum: Data.to(
        { fraud_prover: prover, data: pending },
        SDK.DirectSourcePendingDatum,
      ),
      redeemer: directSourceAuthenticationRedeemer(
        selected.map((entry) => entry.witness),
      ),
    };
    if (budget === 1 && first === 0) {
      const changed = await attempt({
        ...authenticationTransition,
        label:
          "singleton differential: changed rooted descriptor must fail Spend",
        redeemer: directSourceAuthenticationRedeemer([
          {
            ...selected[0]!.witness,
            descriptor_cbor: "00",
          },
        ]),
      });
      console.log(
        "N6 F2 singleton differential",
        JSON.stringify(changed, (_, value: unknown) =>
          typeof value === "bigint" ? value.toString() : value,
        ),
      );
      expect(changed.status, "changed descriptor was accepted").toBe("refused");
      expect(changed.phase).toBe("completion");
      expect(
        changed.error,
        "negative must execute the spending validator, not fail in balancing or size checks",
      ).toMatch(/failed script execution[\s\S]*Spend\[\d+\]/u);
      expect(changed.oldThreadStillUnspent).toBe(true);
    }
    const authentication = await attempt(
      authenticationTransition,
      selected.reduce(
        (sum, entry) => sum + entry.witness.descriptor_cbor.length / 2,
        0,
      ),
    );
    if (authentication.status !== "accepted")
      return stopped("authenticate", authentication);
    const next: SDK.DirectSourceScanState = {
      ...advanced,
      source_index: selected.at(-1)!.step.after,
    };
    const mutation = await attempt({
      label: `mandatory exact map admission of ${selected.length} candidates`,
      role: roles.map,
      destination: roles.resolved.spendingScriptAddress,
      datum: scanDatum(next),
      redeemer: directSourceAdmissionRedeemer(
        selected.map((entry) => entry.step.admission),
      ),
    });
    if (mutation.status !== "accepted") return stopped("map", mutation);
    state = next;
    processed = end;
  }
  return result("serviceable");
};
