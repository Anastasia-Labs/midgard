import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
} from "../src/field-opening.js";
import { nextMintFieldCursor } from "../src/mint-item-non-canonical/field-scan.js";
import {
  submitMintItemNonCanonicalStep01Accepted,
  submitMintItemNonCanonicalStep02,
  submitMintItemNonCanonicalStep04,
} from "../src/mint-item-non-canonical/index.js";
import type {
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowJournalStore,
} from "../src/workflow/journal.js";
import { captureLocallyEvaluatedTransaction } from "../src/workflow/transaction-boundary.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import {
  scanStateOf,
  submitMintStep02Raw,
  submitMintStep03Raw,
  submitMintStep04Raw,
} from "./support/mint-item-non-canonical-emulator.js";
import {
  committedInclusions,
  evidenceOf,
  journaledMintContinuation,
  registeredContracts,
  witnessSetCborOf,
} from "./support/mint-item-non-canonical-lifecycle.js";
import {
  canonicalMintItems,
  lateMalformedMintItems,
  malformedMintItems,
  maximumCanonicalMintItem,
  mintField,
  mintItem,
} from "./support/mint-item-vectors.js";
import { setupFraudulentBlock } from "./support/submit-init-emulator-fixtures.js";

describe("mintItemNonCanonical registered Lucid Evolution lifecycle", () => {
  it("proves grammar and ordering faults, refuses honest mint/burn and forged evidence, cancels, resumes and removes", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realMintItemNonCanonical: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const registered = await registeredContracts(harness);
    const { contracts, references, common } = registered;
    const malformedMaximum = maximumCanonicalMintItem();
    malformedMaximum[malformedMaximum.length - 1] = 0;
    const vectors = [
      ...malformedMintItems.map(([label, hex]) => ({
        label,
        items: [Buffer.from(hex, "hex")],
        index: 0,
        canonical: false,
      })),
      ...canonicalMintItems.map((item, i) => ({
        label: `canonical-${i}`,
        items: [item],
        index: 0,
        canonical: true,
      })),
      {
        label: "duplicate policies",
        items: [mintItem(), mintItem()],
        index: 1,
        canonical: false,
      },
      {
        label: "descending policies",
        items: [mintItem("a14001", "22"), mintItem()],
        index: 1,
        canonical: false,
      },
      {
        label: "ascending policies",
        items: [mintItem(), mintItem("a14001", "22")],
        index: 1,
        canonical: true,
      },
      {
        label: "maximum canonical",
        items: [maximumCanonicalMintItem()],
        index: 0,
        canonical: true,
      },
      {
        label: "maximum final-byte fault",
        items: [malformedMaximum],
        index: 0,
        canonical: false,
      },
    ];
    vectors.push({
      label: "late policy fault",
      items: lateMalformedMintItems(),
      index: 909,
      canonical: false,
    });
    const txs = vectors.map((v, i) =>
      makeNativeTx({
        spendInputCbors: [],
        fee: BigInt(i),
        mintPreimageCbor: mintField(...v.items),
      }),
    );
    const committed = await committedInclusions(txs);
    const setup = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue: registered.catalogue,
      fixture: {
        transactionsRoot: committed.transactionsRoot,
        l2TransactionCount: BigInt(txs.length),
      },
    });
    const certificateReference = await registered.publishReferences();
    const cancelled = new Set<number>();
    let fraudulentHeaderHash = "";
    let sawResume = false;
    let transactions = 0;
    let maxMemory = 0n,
      maxSteps = 0n,
      maxBytes = 0;
    const measured = async <T>(action: () => Promise<T>) => {
      const result = await captureEmulatorSubmission(harness.emulator, action);
      for (const measurement of result.measurements) {
        expect(measurement.l1ByteMargin).toBeGreaterThan(0);
        expect(measurement.executionMemory).toBeLessThanOrEqual(16_500_000n);
        expect(measurement.executionSteps).toBeLessThanOrEqual(10_000_000_000n);
        if (measurement.executionMemory > maxMemory)
          maxMemory = measurement.executionMemory;
        if (measurement.executionSteps > maxSteps)
          maxSteps = measurement.executionSteps;
        maxBytes = Math.max(maxBytes, measurement.completeSignedBytes);
        transactions += 1;
      }
      return result.result;
    };
    for (const [i, vector] of vectors.entries()) {
      const tx = txs[i]!;
      const inclusion = committed.inclusions[i]!;
      const evidence = evidenceOf(
        acceptedVerdictSubject(inclusion.nativeTxId),
        tx,
        vector.index,
      );
      expect(evidence.canonical, vector.label).toBe(vector.canonical);
      let init = await registered.init(setup.fraudulentBlockOutRef);
      fraudulentHeaderHash = init.result.fraudulentHeaderHash;
      if (!cancelled.has(0)) {
        await registered.cancel(
          `${init.result.txHash}#${init.result.firstStepOutputIndex}`,
          0,
        );
        cancelled.add(0);
        init = await registered.init(setup.fraudulentBlockOutRef);
      }
      const bind = async (finding = evidence) => {
        const threadUtxo = await registered.threadUtxoOf(
          init.result.txHash,
          init.result.firstStepOutputIndex,
        );
        return measured(() =>
          submitMintItemNonCanonicalStep01Accepted({
            lucid: harness.proverLucid,
            blueprint: harness.realBlueprint,
            network: "Custom",
            contracts,
            signer: harness.proverSigner,
            finding,
            threadUtxo,
            threadToken: {
              unit: init.result.computationThreadUnit,
              fraudulentHeaderHash,
            },
            stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
            txInclusion: inclusion,
            referenceScriptUtxo: references[0]!,
            witnessReferenceScripts: harness.witnessReferenceScripts,
          }),
        );
      };
      if (i === 0)
        await expectOnchainRefusal(() =>
          bind({
            ...evidence,
            subject: acceptedVerdictSubject("ff".repeat(32)),
          }),
        );
      let bound = await bind();
      if (!cancelled.has(1)) {
        await registered.cancel(bound.nextThreadOutRef, 1);
        cancelled.add(1);
        init = await registered.init(setup.fraudulentBlockOutRef);
        bound = await bind();
      }
      if (i === 0) {
        const planned = planFaultProofFieldOpening({
          anchorSourceKind: 0n,
          fieldIndex: 5,
          anchorTxId: inclusion.nativeTxId,
          nativeTxCompactCbor: inclusion.nativeTxCompactCbor,
          itemCbors: vector.items,
          owner: harness.proverSigner.paymentKeyHash,
          publish: false,
          label: "mint authentication negative",
        });
        const opening = faultProofFieldOpening({
          planned,
          referenceInputs: [references[1]!],
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
          label: "mint authentication negative",
        });
        await expectOnchainRefusal(() =>
          submitMintStep02Raw({
            ...common(bound.nextThreadOutRef, 1),
            opening,
            nextState: {
              ...scanStateOf(evidence, 0, 0n),
              item_hash: "ff".repeat(32),
            },
          }),
        );
        await expectOnchainRefusal(() =>
          submitMintStep02Raw({
            ...common(bound.nextThreadOutRef, 1),
            opening,
            nextState: scanStateOf(evidence, 0, 0n),
            nextStepIndex: 3,
          }),
        );
      }
      const open = async () => {
        let openingRef = bound.nextThreadOutRef;
        const journalEntries: FraudProofWorkflowJournalEntry[] = [];
        const store: FraudProofWorkflowJournalStore = {
          load: async () => journalEntries,
          append: async (entry, expected) => {
            expect(journalEntries.length).toBe(expected);
            journalEntries.push(entry);
          },
        };
        let carriage:
          | Awaited<ReturnType<typeof submitMintItemNonCanonicalStep02>>
          | undefined;
        for (;;) {
          if (carriage !== undefined) {
            const advanced = await measured(() =>
              journaledMintContinuation({
                common: common(openingRef, 1),
                evidence,
                nativeTxCompactCbor: inclusion.nativeTxCompactCbor,
                witnessSetCompactCbor: witnessSetCborOf(tx),
                certificateReference,
                carriage,
                store,
                action: "submitStep02",
                fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
              }),
            );
            openingRef = advanced.nextThreadOutRef;
            if (advanced.terminal) return { ...carriage, ...advanced };
            continue;
          }
          const advanced = await measured(() =>
            submitMintItemNonCanonicalStep02({
              ...common(openingRef, 1),
              evidence,
              nativeTxCompactCbor: inclusion.nativeTxCompactCbor,
              witnessSetCompactCbor: witnessSetCborOf(tx),
              publishCarriage: true,
              publishedCarriageUtxos: carriage?.publishedCarriageUtxos,
              certificateUtxo: carriage?.certificateUtxo,
              certificateReferenceScriptUtxo: certificateReference,
            }),
          );
          if (!advanced.terminal) {
            const planned = planFaultProofFieldOpening({
              anchorSourceKind: 0n,
              fieldIndex: 5,
              anchorTxId: inclusion.nativeTxId,
              nativeTxCompactCbor: inclusion.nativeTxCompactCbor,
              itemCbors: vector.items,
              owner: harness.proverSigner.paymentKeyHash,
              publish: false,
              label: "mint cursor substitution",
            });
            const extraReferenceInputs =
              advanced.certificateUtxo === undefined
                ? []
                : [advanced.certificateUtxo];
            const opening = faultProofFieldOpening({
              planned,
              referenceInputs: [
                ...advanced.publishedCarriageUtxos,
                references[1]!,
                ...extraReferenceInputs,
              ],
              certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
              label: "mint cursor substitution",
            });
            const firstCursor = nextMintFieldCursor(evidence, null).cursor;
            const expectedCursor = nextMintFieldCursor(
              evidence,
              firstCursor,
            ).cursor;
            await expectOnchainRefusal(() =>
              submitMintStep02Raw({
                ...common(advanced.nextThreadOutRef, 1),
                opening,
                carriageUtxos: advanced.publishedCarriageUtxos,
                extraReferenceInputs,
                nextStepIndex: 1,
                nextState: {
                  subject: evidence.subject,
                  item_index: BigInt(evidence.itemIndex),
                  field_cursor: {
                    ...expectedCursor,
                    next_index: expectedCursor.next_index + 1n,
                  },
                },
              }),
            );
            await expectOnchainRefusal(() =>
              submitMintStep02Raw({
                ...common(advanced.nextThreadOutRef, 1),
                opening,
                carriageUtxos: advanced.publishedCarriageUtxos,
                extraReferenceInputs,
                nextState: scanStateOf(evidence, 0, 0n),
              }),
            );
          }
          carriage = advanced;
          openingRef = advanced.nextThreadOutRef;
          if (advanced.terminal) return advanced;
        }
      };
      let opened = await open();
      if (!cancelled.has(2)) {
        await registered.cancel(opened.nextThreadOutRef, 2);
        cancelled.add(2);
        init = await registered.init(setup.fraudulentBlockOutRef);
        bound = await bind();
        opened = await open();
      }
      let threadOutRef = opened.nextThreadOutRef;
      if (i === 0) {
        await expectOnchainRefusal(() =>
          submitMintStep03Raw({
            ...common(threadOutRef, 2),
            window: Buffer.from([0]),
            nextState: scanStateOf(evidence, 0, 2n),
            nextStepIndex: 3,
          }),
        );
        await expectOnchainRefusal(() =>
          submitMintStep03Raw({
            ...common(threadOutRef, 2),
            window: vector.items[0]!,
            nextState: {
              ...scanStateOf(evidence, 0, 2n),
              item_hash: "ff".repeat(32),
            },
            nextStepIndex: 3,
          }),
        );
      }
      if (vector.canonical)
        await expectOnchainRefusal(() =>
          submitMintStep03Raw({
            ...common(threadOutRef, 2),
            window: vector.items[vector.index]!.subarray(0, 8190),
            nextState: scanStateOf(evidence, 1, 0n),
            nextStepIndex: 3,
          }),
        );
      const scan = async () => {
        const journalEntries: FraudProofWorkflowJournalEntry[] = [];
        const store: FraudProofWorkflowJournalStore = {
          load: async () => journalEntries,
          append: async (entry, expected) => {
            expect(journalEntries.length).toBe(expected);
            journalEntries.push(entry);
          },
        };
        let scans = 0;
        for (;;) {
          const fresh = evidenceOf(
            acceptedVerdictSubject(inclusion.nativeTxId),
            tx,
            vector.index,
          );
          const advanced = await measured(() =>
            journaledMintContinuation({
              common: common(threadOutRef, 2),
              evidence: fresh,
              nativeTxCompactCbor: inclusion.nativeTxCompactCbor,
              witnessSetCompactCbor: witnessSetCborOf(tx),
              certificateReference,
              store,
              action: "submitStep03",
              fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
            }),
          );
          threadOutRef = advanced.nextThreadOutRef;
          scans += 1;
          if (advanced.terminal) break;
        }
        sawResume ||= scans > 2;
      };
      await scan();
      if (!cancelled.has(3)) {
        await registered.cancel(threadOutRef, 3);
        cancelled.add(3);
        init = await registered.init(setup.fraudulentBlockOutRef);
        bound = await bind();
        opened = await open();
        threadOutRef = opened.nextThreadOutRef;
        await scan();
      }
      if (vector.canonical) {
        await expectOnchainRefusal(() =>
          submitMintStep04Raw({
            ...common(threadOutRef, 3),
            witnessReferenceScripts: harness.witnessReferenceScripts,
          }),
        );
        await registered.cancel(threadOutRef, 3);
      } else {
        const args = {
          ...common(threadOutRef, 3),
          evidence,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        };
        if (i === 0) {
          const finalized = await measured(() =>
            submitMintItemNonCanonicalStep04(args),
          );
          expect(finalized.fraudProofUnit).toBeTruthy();
        } else {
          await captureLocallyEvaluatedTransaction((preSubmitBoundary) =>
            submitMintItemNonCanonicalStep04({ ...args, preSubmitBoundary }),
          );
          await registered.cancel(threadOutRef, 3);
        }
      }
    }
    expect([...cancelled].sort()).toEqual([0, 1, 2, 3]);
    expect(sawResume).toBe(true);
    const removed = await registered.removal(fraudulentHeaderHash);
    expect(removed.result.fraudCategoryId).toBe("00000036");
    console.info(
      `mintItemNonCanonical: ${vectors.length} vectors, ${transactions} measured transactions; maximum field and cancellation/resume verified; maxima ${maxBytes} bytes, ${maxMemory} memory, ${maxSteps} CPU`,
    );
  }, 900_000);
});
