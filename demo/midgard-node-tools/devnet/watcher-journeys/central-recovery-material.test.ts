import { existsSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { join } from "node:path";

import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { prepareFieldItemWidthIllegalRecoveryMaterial } from "@al-ft/midgard-fault-proofs/field-item-width-illegal";
import { prepareOutputReferenceScriptDecodingRecoveryMaterial } from "@al-ft/midgard-fault-proofs/output-reference-script-decoding";
import { prepareProtectedOutputSignerMissingRecoveryMaterial } from "@al-ft/midgard-fault-proofs/protected-output-signer-missing";
import { prepareResolvedOutputNonCanonicalRecoveryMaterial } from "@al-ft/midgard-fault-proofs/resolved-output-non-canonical";
import { prepareSpendInputSignerMissingRecoveryMaterial } from "@al-ft/midgard-fault-proofs/spend-input-signer-missing";
import { prepareTransactionOutputNonCanonicalRecoveryMaterial } from "@al-ft/midgard-fault-proofs/transaction-output-non-canonical";
import { prepareWitnessScriptDecodingRecoveryMaterial } from "@al-ft/midgard-fault-proofs/witness-script-decoding";
import {
  encodeWorkflowArtifact,
  requireWorkflowArtifactMatches,
} from "@al-ft/midgard-fault-proofs/workflow/artifact-codec";
import { readPublishedDepositHistory } from "midgard-watcher/tests/support/published-deposit-history";
import type { PublishedDepositTraceCheckpoint } from "midgard-watcher/tests/support/published-deposit-trace";
import { beforeAll, describe, expect, it } from "vitest";

import { readJourneyArtifact } from "./artifacts.js";
import { prepareJourneyForcedTransaction } from "./forced-transaction.js";
import { readCheckpointJourneyLedgerBlock } from "./journey-checkpoint-ledger.test-support.js";
import { classifyFullCatalogueTransactionFixture } from "./journey-classification.test-support.js";
import { depositEventsRetainedBlock } from "./journey-fixture-ledger.test-support.js";
import { journeyLedgerPairs } from "./journey-ledger.js";
import { buildJourneyScriptForcedFault } from "./script-cases.js";
import {
  buildJourneyTransactionFault,
  type JourneyTransactionInput,
} from "./transaction-cases.js";
import { buildJourneyForcedTransaction } from "./transaction-forced-cases.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;
const stagedPath =
  runDirectory === undefined
    ? undefined
    : join(runDirectory, "work/journeys/transition-trace/staged.json");
const categories = [
  "fieldItemWidthIllegal",
  "transactionOutputNonCanonical",
  "resolvedOutputNonCanonical",
  "spendInputSignerMissing",
  "protectedOutputSignerMissing",
  "outputReferenceScriptDecoding",
  "witnessScriptDecoding",
] as const;

// These gates use real retained deposit material and synthetic local transport.
// They do not constitute a live watcher journey or a real submission.
describe.skipIf(stagedPath === undefined || !existsSync(stagedPath))(
  "central family durable material from full installed local replay",
  () => {
    let staged: PublishedDepositTraceCheckpoint;
    let input: JourneyTransactionInput;
    let predecessor: Awaited<ReturnType<typeof depositEventsRetainedBlock>>;
    beforeAll(async () => {
      if (runDirectory === undefined || stagedPath === undefined)
        throw new Error("Retained journey directory required");
      staged =
        await readJourneyArtifact<PublishedDepositTraceCheckpoint>(stagedPath);
      const accounts: { operator: { seedPhrase: string } } = JSON.parse(
        await readFile(
          join(runDirectory, "secrets/journey-accounts.json"),
          "utf8",
        ),
      );
      const deposit = readPublishedDepositHistory(
        staged.depositHistory,
        staged.depositMetadata,
      );
      predecessor = await depositEventsRetainedBlock({
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        operatorVkey: staged.current.header.operatorVkey,
        startTime: staged.predecessor.header.endTime,
        endTime: staged.current.header.endTime,
        blockSlot: staged.current.header.blockSlot,
        prevHeaderHash: staged.predecessor.headerHash,
        prevUtxosRoot: staged.predecessor.header.utxosRoot,
        priorLedger: journeyLedgerPairs(
          (
            await readCheckpointJourneyLedgerBlock(
              runDirectory!,
              staged.predecessor,
            )
          ).ledger,
        ),
        events: [
          {
            event: deposit.event,
            originalAssets: deposit.originalAssets,
            honest: true,
          },
        ],
      });
      input = {
        predecessor,
        ledgerOwnerSeedPhrase: accounts.operator.seedPhrase,
        operatorVkey: predecessor.header.operatorVkey,
        endTime: predecessor.header.endTime + 40000n,
        blockSlot: predecessor.header.blockSlot + 40n,
      };
    });
    it.each(categories)(
      "re-admits exact %s material after durable JSON round trip",
      async (category) => {
        const orderKey = { transactionId: "a1".repeat(32), outputIndex: 0n };
        const candidate = async () => {
          if (
            category === "spendInputSignerMissing" ||
            category === "protectedOutputSignerMissing"
          )
            return {
              block: await buildJourneyTransactionFault({ ...input, category }),
            };
          if (
            category === "witnessScriptDecoding" ||
            category === "outputReferenceScriptDecoding"
          ) {
            const block = await buildJourneyScriptForcedFault({
              ...input,
              category,
              orderKey,
            });
            return {
              block,
              forced: { transaction: block.material.transaction, orderKey },
            };
          }
          return {
            block: await buildJourneyForcedTransaction({
              ...input,
              category,
              orderKey,
            }),
            forced: {
              transaction: prepareJourneyForcedTransaction(input).transaction,
              orderKey,
            },
          };
        };
        const full = await classifyFullCatalogueTransactionFixture({
          ...(await candidate()),
          predecessor,
          history: [staged.predecessor],
        });
        expect(full.decision).toMatchObject({
          decision: "fault_detected",
          category,
        });
        if (
          full.decision.decision !== "fault_detected" ||
          full.evidence === undefined
        )
          throw new Error("Expected exact full catalogue canonical evidence");
        const evidence = full.evidence;
        const detectionId = full.decision.detectionId;
        const prepare = async () => {
          switch (category) {
            case "fieldItemWidthIllegal":
              return await prepareFieldItemWidthIllegalRecoveryMaterial(
                evidence,
                detectionId,
              );
            case "transactionOutputNonCanonical":
              return await prepareTransactionOutputNonCanonicalRecoveryMaterial(
                evidence,
                detectionId,
              );
            case "protectedOutputSignerMissing":
              return await prepareProtectedOutputSignerMissingRecoveryMaterial(
                evidence,
                detectionId,
              );
            case "outputReferenceScriptDecoding":
              return await prepareOutputReferenceScriptDecodingRecoveryMaterial(
                evidence,
                detectionId,
              );
            case "witnessScriptDecoding":
              return await prepareWitnessScriptDecodingRecoveryMaterial(
                evidence,
                detectionId,
              );
            case "resolvedOutputNonCanonical":
            case "spendInputSignerMissing": {
              const admitted = full.predecessor;
              if (admitted === undefined)
                throw new Error("Exact admitted predecessor required");
              return category === "resolvedOutputNonCanonical"
                ? await prepareResolvedOutputNonCanonicalRecoveryMaterial(
                    evidence,
                    detectionId,
                    admitted,
                  )
                : await prepareSpendInputSignerMissingRecoveryMaterial(
                    evidence,
                    detectionId,
                    admitted,
                  );
            }
          }
        };
        const material = await prepare();
        expect(material).toMatchObject({
          category,
          headerHash: evidence.headerHash,
          detectionId,
        });
        const recorded = JSON.parse(
          JSON.stringify(encodeWorkflowArtifact(material)),
        );
        const fresh = await prepare();
        expect(requireWorkflowArtifactMatches(recorded, fresh)).toBe(fresh);
        expect(() =>
          requireWorkflowArtifactMatches(recorded, {
            ...fresh,
            detectionId: "changed",
          }),
        ).toThrow("freshly authenticated material");
      },
      120_000,
    );
  },
);
