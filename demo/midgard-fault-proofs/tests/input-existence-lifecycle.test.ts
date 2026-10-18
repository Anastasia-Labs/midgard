/** Actual registered four-step input-existence route. The chronology cases
 * compute all event roots from full ledger outputs and execute the selected
 * event's pre-root. They do not claim full machine or live withdrawal validity;
 * maximum-carriage and final integrated gates remain separate. */
import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { prepareInputExistence } from "../src/input-existence/prepare.js";
import { submitInputExistenceStep } from "../src/input-existence/submit.js";
import { checkLedgerAgainstRoot } from "../src/ledger-at.js";
import { submitInit } from "../src/submit-init.js";
import { keyValuePhasNonMembershipProof } from "../src/transition-trace/phas.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import {
  type InputChronology,
  inputExistenceChronologyFixture,
} from "./helpers/input-existence-chronology-fixture.js";
import { chronologySeedDeposits } from "./helpers/input-existence-chronology-seed.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import { submitSecondHeaderTx } from "./support/emulator/setup-tx.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import { submitRawInputExistenceStep } from "./support/raw-input-existence-step.js";
import { registerPexcludesExclusionRewardAccount } from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  funderPaymentKeyHash,
  network,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import { depositEventsRetainedBlock } from "./support/transition-trace-retained.deposit-events-retained-block.js";

type Case =
  | "normal_missing"
  | "forced_missing"
  | "honest_forced_rejection"
  | "out_of_domain_rejection"
  | "prior_ledger_present_rejection"
  | InitialAbsence
  | InputChronology;
/** A prover that carries an absence proof taken against the block-initial
 * ledger, for an input an earlier event of the same block created. */
type InitialAbsence =
  | "forced_creator_initial_absence"
  | "normal_creator_initial_absence";
const initialAbsenceSource = {
  forced_creator_initial_absence: "forced_creator",
  normal_creator_initial_absence: "normal_creator",
} as const satisfies Record<InitialAbsence, InputChronology>;
const isInitialAbsence = (kind: Case): kind is InitialAbsence =>
  Object.hasOwn(initialAbsenceSource, kind);
const cases: readonly Case[] = [
  "normal_missing",
  "forced_missing",
  "honest_forced_rejection",
  "out_of_domain_rejection",
  "prior_ledger_present_rejection",
  "forced_creator",
  "normal_creator",
  "forced_creator_initial_absence",
  "normal_creator_initial_absence",
  "later_creator",
  "forced_spend",
  "normal_spend",
  "withdrawal_consumed",
  "rejected_creator",
  "forced_target",
];
const checkFit = (measurement: CompleteSignedTransactionMeasurement) => {
  expect(measurement.completeSignedBytes).toBeLessThanOrEqual(16_384);
  expect(measurement.executionMemory).toBeLessThanOrEqual(13_200_000n);
  expect(measurement.executionSteps).toBeLessThanOrEqual(8_000_000_000n);
};

describe("registered event-relative input-existence lifecycle", () => {
  for (const field of [0, 1] as const) {
    it.each(cases)(
      `runs field ${field} through all four physical validators for %s`,
      async (kind) => {
        const h = await makeFaultProofEmulatorHarness({
          contractOptions: {
            realNonExistentInput: field === 0,
            realNoReferenceInput: field === 1,
            alwaysFraudProofCatalogue: true,
          },
          registerAdditionalRewardAccounts:
            registerPexcludesExclusionRewardAccount,
        });
        const category = field === 0 ? "nonExistentInput" : "noReferenceInput";
        const family = h.contracts.fraudProofContracts[category];
        for (const step of family.steps)
          expect(step.spendingScript.script.length / 2).toBeLessThanOrEqual(
            16_384,
          );
        const key = encodeMidgardSpendInputItem({
          txId: Buffer.alloc(32, 0x66),
          outputIndex: 0,
        });
        const nativeTx = makeNativeTx({
          spendInputCbors: field === 0 ? [key] : [],
          ...(field === 1 ? { referenceByte: "66" } : {}),
          fee: 0n,
        });
        const rejected =
          kind === "honest_forced_rejection" ||
          kind === "out_of_domain_rejection" ||
          kind === "prior_ledger_present_rejection";
        const chronology = isInitialAbsence(kind)
          ? initialAbsenceSource[kind]
          : (
              [
                "forced_creator",
                "normal_creator",
                "later_creator",
                "forced_spend",
                "normal_spend",
                "withdrawal_consumed",
                "rejected_creator",
                "forced_target",
                "prior_ledger_present_rejection",
              ] as const
            ).find((scenario) => scenario === kind);
        const outOfDomain = kind === "out_of_domain_rejection";
        const reason: SDK.RejectionReason = {
          InputNotFound: {
            source_kind: BigInt(field),
            input_index: outOfDomain ? 1n : 0n,
          },
        };
        const operatorVkey = await funderPaymentKeyHash(h.funderLucid);
        const startTime = BigInt(
          alignUnixTimeToEmulatorSlotBoundary(
            h.funderLucid,
            h.emulator.now() + 120_000,
          ) - 1,
        );
        const priorDeposit =
          chronology === undefined
            ? undefined
            : await depositEventsRetainedBlock({
                operatorVkey,
                startTime: startTime - 30_000n,
                endTime: startTime,
                blockSlot: 0n,
                prevHeaderHash: SDK.GENESIS_HEADER_HASH,
                prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                priorLedger: [],
                events: chronologySeedDeposits(),
              });
        const history =
          chronology === undefined || priorDeposit === undefined
            ? undefined
            : await inputExistenceChronologyFixture(chronology, field, {
                operatorVkey,
                startTime,
                predecessorHeaderHash: priorDeposit.headerHash,
              });
        const selected =
          history === undefined
            ? await (async () => {
                const block = await buildDecodingBlockFixture({
                  operatorVkey,
                  startTime,
                  priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                  subject:
                    kind === "normal_missing"
                      ? { kind: "normal", nativeTx }
                      : {
                          kind: "forced",
                          nativeTx,
                          orderKey: {
                            transactionId: "77".repeat(32),
                            outputIndex: 0n,
                          },
                          verdict: rejected
                            ? { ForcedTxInvalid: { reason } }
                            : "ForcedTxValid",
                        },
                });
                if (
                  kind !== "normal_missing" &&
                  block.forcedOrderKey === undefined
                )
                  throw new Error(
                    "forced fixture has no authenticated order key",
                  );
                const eventKey: SDK.EventKey =
                  kind === "normal_missing"
                    ? { L2TransactionEventKey: { tx_id: block.nativeTxId } }
                    : {
                        ForcedTransactionEventKey: {
                          tx_order_id: block.forcedOrderKey!,
                        },
                      };
                return {
                  evidence: await canonicalBlockEvidenceFromVerifiedPayload({
                    observation: authenticatedHeaderObservation(block),
                    payloadEnvelopeCbor: block.payloadEnvelopeCbor,
                    daProvenance: {
                      trustClass: "public_or_permissionless_da",
                      sourceId: "emulator/input-existence",
                      grade: "security",
                    },
                  }),
                  eventKey,
                  predecessor: await checkLedgerAgainstRoot({
                    entries: [],
                    utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                    label: "registered prior-block ledger",
                  }),
                };
              })()
            : {
                evidence: history.evidence,
                eventKey: history.targetEvent,
                predecessor: history.predecessor,
              };
        if (history !== undefined && priorDeposit !== undefined) {
          expect(history.predecessor.utxosRoot).toBe(
            priorDeposit.header.utxosRoot,
          );
          const committedPredecessor = await submitSetupTx({
            lucid: h.funderLucid,
            contracts: h.contracts,
            nonceUtxo: h.nonceUtxo,
            catalogue: h.catalogue,
            header: priorDeposit.header,
          });
          expect(committedPredecessor.headerHash).toBe(priorDeposit.headerHash);
          if (kind === "forced_creator" || kind === "normal_creator") {
            const full = materializeMidgardForcedTxFromCanonical(
              decodeMidgardNativeTxFullFromCanonicalCbor(
                history.creator.canonicalCbor,
              ),
            );
            expect(
              await nodeForcedVerdict({
                transactionId: Buffer.from(history.creator.txId, "hex"),
                forcedCanonicalCbor: encodeMidgardForcedTxCanonical(full),
                ledger: history.predecessorEntries.map((entry) => [
                  entry.outRef,
                  entry.output,
                ]),
              }),
            ).toBe("ForcedTxValid");
          }
        }
        const setup =
          priorDeposit === undefined
            ? await submitSetupTx({
                lucid: h.funderLucid,
                contracts: h.contracts,
                nonceUtxo: h.nonceUtxo,
                catalogue: h.catalogue,
                header: selected.evidence.header,
              })
            : await (async () => {
                const committed = await submitSecondHeaderTx({
                  lucid: h.funderLucid,
                  contracts: h.contracts,
                  header: selected.evidence.header,
                });
                return {
                  header: committed.header,
                  headerHash: committed.headerHash,
                  fraudulentBlockOutRef: committed.blockOutRef,
                };
              })();
        expect(setup.headerHash).toBe(selected.evidence.headerHash);
        const prepared = await prepareInputExistence({
          block: selected.evidence,
          eventKey: selected.eventKey,
          field,
          inputIndex: rejected ? undefined : 0n,
          predecessor: selected.predecessor,
          owner: h.proverSigner.paymentKeyHash,
        });
        expect(prepared.source.subject.direction).toBe(rejected ? 1n : 0n);
        const present =
          kind === "forced_creator" ||
          kind === "normal_creator" ||
          isInitialAbsence(kind) ||
          kind === "prior_ledger_present_rejection";
        if (present) expect(prepared.ledgerEvidence).toHaveProperty("Present");
        else if (outOfDomain)
          expect(prepared.ledgerEvidence).toBe("OutOfDomain");
        else expect(prepared.ledgerEvidence).toHaveProperty("Absent");
        const carried = await (async () => {
          if (!isInitialAbsence(kind)) return prepared;
          if (history === undefined)
            throw new Error("initial-absence case has no chronology");
          // The block-initial ledger lacks the created input, so a genuine
          // non-membership proof exists against it, but not against the
          // target event's pre-root.
          expect(history.predecessor.utxosRoot).toBe(
            selected.evidence.header.prevUtxosRoot,
          );
          expect(history.targetPreRoot).not.toBe(history.predecessor.utxosRoot);
          expect(
            history.predecessor.trie.entries.some(({ key }) =>
              key.equals(history.selected),
            ),
          ).toBe(false);
          return {
            ...prepared,
            ledgerEvidence: {
              Absent: {
                proof: await keyValuePhasNonMembershipProof(
                  history.predecessor.trie,
                  history.selected,
                ),
              },
            },
          };
        })();
        const stepReferences: UTxO[] = [];
        for (const [index, step] of family.steps.entries())
          stepReferences.push(
            (
              await publishPlainReferenceScriptUtxo({
                lucid: h.funderLucid,
                script: step.spendingScript,
                label: `input existence ${category} step${index + 1}`,
              })
            ).utxo,
          );
        const ct = (
          await publishPlainReferenceScriptUtxo({
            lucid: h.funderLucid,
            script: h.contracts.computationThread.mintingScript,
            label: "input existence CT mint",
          })
        ).utxo;
        const fp = (
          await publishPlainReferenceScriptUtxo({
            lucid: h.funderLucid,
            script: h.contracts.fraudProof.mintingScript,
            label: "input existence proof mint",
          })
        ).utxo;
        const deploymentInfo = buildRemovalDeploymentInfo(
          h.contracts,
          h.catalogue,
        );
        const init = await captureEmulatorSubmission(h.emulator, () =>
          submitInit({
            lucid: h.proverLucid,
            blueprint: h.realBlueprint,
            deploymentInfo,
            network,
            signer: h.proverSigner,
            fraudCategory: category,
            fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
            witnessReferenceScripts: h.witnessReferenceScripts,
          }),
        );
        checkFit(init.measurement);
        let threadOutRef = `${init.result.txHash}#${init.result.firstStepOutputIndex}`;
        // The local submitter admits only evidence that matches the event's
        // pre-root, so the carried absence goes through the raw producer,
        // which claims the block-initial root as the event's pre-root.
        const submitStep = isInitialAbsence(kind)
          ? (args: Parameters<typeof submitRawInputExistenceStep>[0]) =>
              submitRawInputExistenceStep({
                ...args,
                claimedPreUtxosRoot: selected.evidence.header.prevUtxosRoot,
              })
          : submitInputExistenceStep;
        const send = (stepIndex: 0 | 1 | 2 | 3) =>
          submitStep({
            lucid: h.proverLucid,
            blueprint: h.realBlueprint,
            network,
            contracts: {
              hubOraclePolicyId: h.contracts.hubOracle.policyId,
              steps: family.steps,
              computationThread: h.contracts.computationThread,
              fraudProof: h.contracts.fraudProof,
            },
            categoryId: h.catalogue.categories[category].categoryId,
            signer: h.proverSigner,
            threadOutRef,
            prepared: carried,
            stepIndex,
            stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
            referenceScripts: {
              steps: stepReferences,
              computationThreadMint: ct,
              fraudProofMint: fp,
            },
            witnessReferenceScripts: h.witnessReferenceScripts,
          });
        const accepted = isInitialAbsence(kind)
          ? ([0, 1] as const)
          : ([0, 1, 2] as const);
        for (const index of accepted) {
          const submitted = await captureEmulatorSubmission(h.emulator, () =>
            send(index),
          );
          checkFit(submitted.measurement);
          threadOutRef = submitted.result.nextThreadOutRef;
        }
        if (isInitialAbsence(kind)) {
          // The pre-state step binds the target event's own pre-root, so the
          // block-initial root cannot become the ledger state.
          await expectOnchainRefusal(() => send(2));
          const [txHash, index] = threadOutRef.split("#");
          expect(
            await h.proverLucid.utxosByOutRef([
              { txHash: txHash!, outputIndex: Number(index) },
            ]),
          ).toHaveLength(1);
        } else if (
          kind === "honest_forced_rejection" ||
          (present && !rejected)
        ) {
          await expectOnchainRefusal(() => send(3));
          // A refusal must preserve the actual step-04 thread and mint no proof.
          const [txHash, index] = threadOutRef.split("#");
          expect(
            await h.proverLucid.utxosByOutRef([
              { txHash: txHash!, outputIndex: Number(index) },
            ]),
          ).toHaveLength(1);
        } else {
          const final = await captureEmulatorSubmission(h.emulator, () =>
            send(3),
          );
          checkFit(final.measurement);
          expect(final.result.fraudProofOutRef).toBe(
            final.result.nextThreadOutRef,
          );
          const [txHash, index] = final.result.fraudProofOutRef!.split("#");
          const [proof] = await h.proverLucid.utxosByOutRef([
            { txHash: txHash!, outputIndex: Number(index) },
          ]);
          expect(proof!.address).toBe(
            h.contracts.fraudProof.spendingScriptAddress,
          );
          expect(
            Object.entries(proof!.assets).filter(([unit]) =>
              unit.startsWith(h.contracts.fraudProof.policyId),
            ),
          ).toHaveLength(1);
          expect(
            Object.entries(proof!.assets).filter(([unit]) =>
              unit.startsWith(h.contracts.computationThread.policyId),
            ),
          ).toHaveLength(0);
        }
      },
      600_000,
    );
  }
});
