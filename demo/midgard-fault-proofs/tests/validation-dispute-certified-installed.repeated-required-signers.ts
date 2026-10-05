import { join } from "node:path";

import { CML } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import {
  DirectoryFraudProofWorkflowJournalStore,
  type JournalJsonObject,
} from "../src/workflow/journal.js";
import { stageInstalledValidationTraceDisputeJourney } from "./support/installed-validation-trace-dispute-journey.js";

export const testRepeatedRequiredSignerCarriage = (hooks: {
  binding: unknown;
  authority: unknown;
}) => {
  it("binds the repeated canonical signer field to the forced source and honest replay", async () => {
    const { buildInstalledSignatureFixture } = await import(
      "./support/installed-signature-fixture.js"
    );
    const fixture = await buildInstalledSignatureFixture({
      operatorVkey: "11".repeat(28),
      now: 1_000_000,
      repeatedRequiredSigners: true,
    });
    const auxiliary =
      fixture.challengerTrace.witnesses[fixture.disputedLowIndex]!.auxiliary;
    expect(auxiliary?.kind).toBe("requiredSignerItem");
    if (auxiliary?.kind !== "requiredSignerItem")
      throw new Error("missing required signer source");
    expect(auxiliary.fieldIndex).toBe(4);
    expect(auxiliary.fieldPreimage.length).toBe(30303);
    expect(auxiliary.fieldPreimage.subarray(0, 15148)).toEqual(
      auxiliary.fieldPreimage.subarray(15148, 30296),
    );
    expect(fixture.challengerTrace.rejectionCode).toBe(
      "E_MISSING_REQUIRED_WITNESS",
    );
  });

  it.each([1, 2])(
    "completes installed certified repeated signer carriage with %i existing matching publications",
    async (existingCopies) => {
      const { createRawCommittedFieldCarriagePlan } = await import(
        "../src/workflow/field-carriage-prerequisite.js"
      );
      const {
        buildUnsignedFieldPreimagePublicationProgram,
        fieldPreimagePublicationDatumCbor,
        deriveFieldPreimageCertification,
        FieldPreimageCertificate,
        FieldPreimageCertificateMintRedeemer,
      } = await import("@al-ft/midgard-sdk");
      const { midgardFieldCommitment } = await import("@al-ft/midgard-core");
      const { compareOutRefs } = await import("@al-ft/midgard-core/out-ref");
      const { Data } = await import("@lucid-evolution/lucid");
      const { Effect } = await import("effect");
      const journey = await stageInstalledValidationTraceDisputeJourney(
        hooks,
        false,
        true,
        true,
      );
      const auxiliary =
        journey.fixture.challengerTrace.witnesses[
          journey.fixture.disputedLowIndex
        ]!.auxiliary;
      if (auxiliary?.kind !== "requiredSignerItem")
        throw new Error("missing authentic required signer source");
      const frozenBytes = Buffer.from(auxiliary.fieldPreimage);
      const planned = createRawCommittedFieldCarriagePlan({
        sourceKind: 1n,
        owner: journey.config.signer.paymentKeyHash,
        nativeTxId:
          journey.fixture.challengerTrace.states[
            journey.fixture.disputedLowIndex
          ]!.transactionId.toString("hex"),
        fieldIndex: 4,
        preimage: frozenBytes,
      });
      expect(planned.plan.tier).toBe("Certified");
      expect(
        planned.plan.publications.map(
          (publication) => publication.bytes.length,
        ),
      ).toEqual([15148, 15148, 7]);
      expect(planned.plan.publications[0]!.bytes).toEqual(
        planned.plan.publications[1]!.bytes,
      );
      const certification = deriveFieldPreimageCertification(planned.plan);
      const journal = new DirectoryFraudProofWorkflowJournalStore(
        join(journey.directory, "journal"),
      );
      const seeded: string[] = [];
      let certificateChecked = false;
      let semanticChecked = false;
      let exactChunks: string[] = [];
      const measurements: Record<string, unknown> = {};
      try {
        // Genuine signed publication transactions, each built from the source plan.
        for (const index of existingCopies === 1 ? [0, 2] : [0, 1, 2]) {
          const publication = planned.plan.publications[index]!;
          const unsigned = await Effect.runPromise(
            buildUnsignedFieldPreimagePublicationProgram(journey.config.lucid, {
              publication: {
                chunkIndex: publication.chunkIndex,
                datumCbor: fieldPreimagePublicationDatumCbor(publication.bytes),
                byteLength: publication.bytes.length,
                digestHex: publication.digest.toString("hex"),
              },
              publisherAddress: journey.config.signer.address,
            }),
          );
          const signed = await unsigned.sign.withWallet().complete();
          await signed.submit();
          journey.emulator.awaitBlock();
          const actual = (
            await journey.config.lucid.utxosAt(journey.config.signer.address)
          ).find(
            (utxo) =>
              utxo.txHash === signed.toHash() &&
              utxo.datum ===
                fieldPreimagePublicationDatumCbor(publication.bytes),
          );
          if (actual === undefined)
            throw new Error("seed publication absent from ledger");
          seeded.push(`${actual.txHash}#${actual.outputIndex}`);
        }
        let { result } = await journey.runCold();
        for (let hop = 0; hop < 250 && result.kind !== "completed"; hop++) {
          journey.emulator.awaitBlock();
          if (result.kind === "awaiting_counterparty") {
            await journey.operatorResponds();
            journey.emulator.awaitBlock();
          }
          const entries = await journal.load(result.workflowId);
          for (const { event } of entries) {
            if (event.kind !== "submission_intent") continue;
            const cbor = journey.recorder.signedCbors.get(event.txHash);
            if (cbor === undefined) continue;
            const tx = CML.Transaction.from_cbor_hex(cbor);
            const keys = tx.witness_set().vkeywitnesses()!;
            expect(keys.len()).toBeGreaterThan(0);
            for (let index = 0; index < keys.len(); index++) {
              const key = keys.get(index);
              expect(
                key
                  .vkey()
                  .verify(
                    CML.hash_transaction(tx.body()).to_raw_bytes(),
                    key.ed25519_signature(),
                  ),
              ).toBe(true);
            }
            const redeemers = tx
              .witness_set()
              .redeemers()
              ?.as_arr_legacy_redeemer();
            let memory = 0n;
            let steps = 0n;
            for (let index = 0; index < (redeemers?.len() ?? 0); index++) {
              memory += redeemers!.get(index).ex_units().mem();
              steps += redeemers!.get(index).ex_units().steps();
            }
            expect(cbor.length / 2).toBeLessThanOrEqual(16_384);
            expect(memory).toBeLessThanOrEqual(13_200_000n);
            expect(steps).toBeLessThanOrEqual(8_000_000_000n);
            measurements[event.txHash] = {
              stage: event.actionInput.stage,
              completeSignedBytes: cbor.length / 2,
              memory: memory.toString(),
              steps: steps.toString(),
            };
            const refs = tx.body().reference_inputs();
            const actualRefs = Array.from(
              { length: refs?.len() ?? 0 },
              (_, index) => {
                const ref = refs!.get(index);
                return {
                  txHash: ref.transaction_id().to_hex(),
                  outputIndex: Number(ref.index()),
                };
              },
            );
            // Ledger indices use canonical order over the complete signed set.
            actualRefs.sort(compareOutRefs);
            const labels = actualRefs.map(
              (ref) => `${ref.txHash}#${ref.outputIndex}`,
            );
            expect(new Set(labels).size).toBe(labels.length);
            if (
              event.actionInput.stage === "certify_field_carriage" &&
              !certificateChecked
            ) {
              expect(labels).toHaveLength(4); // Three distinct chunks and the actual mint reference script.
              const policyRef =
                journey.binding.referenceScriptsByContract
                  .fieldPreimageCertificateMint!;
              expect(labels).toContain(policyRef.outRef);
              exactChunks = labels.filter(
                (label) => label !== policyRef.outRef,
              );
              expect(exactChunks).toHaveLength(3);
              for (const seed of seeded) expect(exactChunks).toContain(seed);
              const outputs = tx.body().outputs();
              const certificateOutput = Array.from(
                { length: outputs.len() },
                (_, index) => outputs.get(index),
              ).find(
                (output) =>
                  output.datum()?.as_datum()?.to_cbor_hex() ===
                  certification.datumCbor,
              );
              expect(certificateOutput).toBeDefined();
              const certificate = Data.from(
                certification.datumCbor,
                FieldPreimageCertificate,
              );
              expect(certificate.field_hash).toBe(
                midgardFieldCommitment(frozenBytes).toString("hex"),
              );
              expect(certificate.chunk_digests[0]).toBe(
                certificate.chunk_digests[1],
              );
              const redeemers = tx
                .witness_set()
                .redeemers()!
                .as_arr_legacy_redeemer()!;
              const mint = Array.from({ length: redeemers.len() }, (_, index) =>
                redeemers.get(index),
              ).find((redeemer) => redeemer.tag() === CML.RedeemerTag.Mint)!;
              const decoded = Data.from(
                mint.data().to_cbor_hex(),
                FieldPreimageCertificateMintRedeemer,
              );
              if (decoded === "Retire")
                throw new Error("certificate used wrong redeemer");
              expect(
                new Set(decoded.Certify.chunk_ref_input_indices).size,
              ).toBe(3);
              const chunks = await journey.config.lucid.utxosByOutRef(
                decoded.Certify.chunk_ref_input_indices.map(
                  (index) => actualRefs[Number(index)]!,
                ),
              );
              for (const [
                index,
                referenceIndex,
              ] of decoded.Certify.chunk_ref_input_indices.entries()) {
                const ref = actualRefs[Number(referenceIndex)]!;
                expect(
                  chunks.find(
                    (chunk) =>
                      chunk.txHash === ref.txHash &&
                      chunk.outputIndex === ref.outputIndex,
                  )?.datum,
                ).toBe(
                  fieldPreimagePublicationDatumCbor(
                    planned.plan.publications[index]!.bytes,
                  ),
                );
              }
              certificateChecked = true;
            }
            if (
              event.actionInput.stage === "semantic_resolution" &&
              !semanticChecked
            ) {
              const route = event.durableRecovery!
                .durableRouteInput as JournalJsonObject;
              const binding = route.fieldCarriageBinding as JournalJsonObject;
              expect(binding.fieldIndex).toBe(4);
              expect(binding.fieldCommitment).toBe(
                midgardFieldCommitment(frozenBytes).toString("hex"),
              );
              expect([...labels].sort()).toEqual(binding.referenceOutRefs);
              expect(labels).toHaveLength(5); // Chunks, certificate, semantic script.
              for (const chunk of exactChunks) expect(labels).toContain(chunk);
              semanticChecked = true;
            }
          }
          ({ result } = await journey.runCold());
        }
        expect(result.kind).toBe("completed");
        const entries = await journal.load(result.workflowId);
        const intents = entries
          .map(({ event }) => event)
          .filter((event) => event.kind === "submission_intent");
        const publications = intents.filter(
          (event) => event.actionInput.stage === "publish_field_carriage",
        );
        expect(publications).toHaveLength(existingCopies === 1 ? 1 : 0);
        if (existingCopies === 1)
          expect(
            {
              actionId: publications[0]!.actionId,
              input: publications[0]!.actionInput,
            }.input.publicationIndex,
          ).toBe(1);
        expect(
          intents.filter(
            (event) => event.actionInput.stage === "certify_field_carriage",
          ),
        ).toHaveLength(1);
        expect(certificateChecked).toBe(true);
        expect(semanticChecked).toBe(true);
        expect(auxiliary.fieldPreimage).toEqual(frozenBytes);
        if (process.env.MIDGARD_MULTIPLICITY_MEASUREMENTS !== undefined) {
          const { mkdir, writeFile } = await import("node:fs/promises");
          const directory = process.env.MIDGARD_MULTIPLICITY_MEASUREMENTS;
          await mkdir(directory, { recursive: true });
          await writeFile(
            join(directory, `installed-${existingCopies}.json`),
            JSON.stringify(
              {
                existingCopies,
                result,
                seeded,
                exactChunks,
                fieldHex: frozenBytes.toString("hex"),
                certification,
                entries,
                measurements,
                sourceCanonicalCbor:
                  journey.fixture.challengerReplayInput.canonicalTransactionCbor.toString(
                    "hex",
                  ),
                sourceClaim: journey.fixture.claim,
                signedTransactions: Object.fromEntries(
                  journey.recorder.signedCbors,
                ),
              },
              (_, value: unknown) =>
                typeof value === "bigint" ? value.toString() : value,
              2,
            ) + "\n",
          );
        }
      } finally {
        await journey.cleanup();
      }
    },
    600_000,
  );
};
