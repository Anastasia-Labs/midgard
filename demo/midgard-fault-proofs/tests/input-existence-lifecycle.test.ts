/** Actual registered four-step input-existence route. The chronology cases
 * compute all event roots from full ledger outputs and execute the selected
 * event's pre-root. They do not claim full machine validity; the withdrawal
 * chronology runs on a submitted withdrawal in
 * input-existence-real-withdrawal.test.ts, and maximum-carriage and final
 * integrated gates remain separate. */
import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { prepareInputExistence } from "../src/input-existence/prepare.js";
import { checkLedgerAgainstRoot } from "../src/ledger-at.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import {
  type InputChronology,
  inputExistenceChronologyFixture,
} from "./helpers/input-existence-chronology-fixture.js";
import { chronologySeedDeposits } from "./helpers/input-existence-chronology-seed.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { runInputExistenceRoute } from "./support/emulator/input-existence-route.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import { submitSecondHeaderTx } from "./support/emulator/setup-tx.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import { registerPexcludesExclusionRewardAccount } from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import { depositEventsRetainedBlock } from "./support/transition-trace-retained.deposit-events-retained-block.js";

type Case =
  | "normal_missing"
  | "forced_missing"
  | "honest_forced_rejection"
  | "out_of_domain_rejection"
  | "prior_ledger_present_rejection"
  | Exclude<InputChronology, `withdrawal_${string}`>;
const cases: readonly Case[] = [
  "normal_missing",
  "forced_missing",
  "honest_forced_rejection",
  "out_of_domain_rejection",
  "prior_ledger_present_rejection",
  "forced_creator",
  "normal_creator",
  "later_creator",
  "forced_spend",
  "normal_spend",
  "rejected_creator",
  "forced_target",
];

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
        const chronology = (
          [
            "forced_creator",
            "normal_creator",
            "later_creator",
            "forced_spend",
            "normal_spend",
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
          kind === "prior_ledger_present_rejection";
        if (present) expect(prepared.ledgerEvidence).toHaveProperty("Present");
        else if (outOfDomain)
          expect(prepared.ledgerEvidence).toBe("OutOfDomain");
        else expect(prepared.ledgerEvidence).toHaveProperty("Absent");
        await runInputExistenceRoute({
          h,
          field,
          prepared,
          fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
          outcome:
            kind === "honest_forced_rejection" || (present && !rejected)
              ? {
                  kind: "refusal",
                  refuse: (step04) => expectOnchainRefusal(step04),
                }
              : { kind: "conviction" },
        });
      },
      600_000,
    );
  }
});
