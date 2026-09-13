import { existsSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { join } from "node:path";

import * as FP from "@al-ft/midgard-fault-proofs";
import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import type { PublishedDepositTraceCheckpoint } from "midgard-watcher/tests/support/published-deposit-trace";
import { beforeAll, describe, expect, it } from "vitest";

import { readJourneyArtifact } from "./artifacts.js";
import { classifyFullCatalogueTransactionFixture } from "./full-catalogue-verification.js";
import { buildJourneyScriptForcedFault } from "./script-cases.js";
import {
  buildJourneyTransactionFault,
  type JourneyTransactionInput,
} from "./transaction-cases.js";
import {
  buildJourneyForcedTransaction,
  prepareJourneyForcedTransaction,
} from "./transaction-forced-cases.js";

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
      predecessor = await depositEventsRetainedBlock({
        operatorVkey: staged.current.header.operatorVkey,
        startTime: staged.predecessor.header.endTime,
        endTime: staged.current.header.endTime,
        blockSlot: staged.current.header.blockSlot,
        prevHeaderHash: staged.predecessor.headerHash,
        prevUtxosRoot: staged.predecessor.header.utxosRoot,
        priorLedger: staged.predecessor.payload.block_body.utxos,
        events: [
          {
            event: staged.depositEvent,
            depositPolicyId: staged.depositMetadata.depositAuthUnit.slice(
              0,
              56,
            ),
            assetName: staged.depositMetadata.depositAssetName,
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
              return await FP.prepareFieldItemWidthIllegalRecoveryMaterial(
                evidence,
                detectionId,
              );
            case "transactionOutputNonCanonical":
              return await FP.prepareTransactionOutputNonCanonicalRecoveryMaterial(
                evidence,
                detectionId,
              );
            case "protectedOutputSignerMissing":
              return await FP.prepareProtectedOutputSignerMissingRecoveryMaterial(
                evidence,
                detectionId,
              );
            case "outputReferenceScriptDecoding":
              return await FP.prepareOutputReferenceScriptDecodingRecoveryMaterial(
                evidence,
                detectionId,
              );
            case "witnessScriptDecoding":
              return await FP.prepareWitnessScriptDecodingRecoveryMaterial(
                evidence,
                detectionId,
              );
            case "resolvedOutputNonCanonical":
            case "spendInputSignerMissing": {
              const corpus = full.historicalNativeScriptCorpus;
              if (corpus === undefined)
                throw new Error("Exact admitted history required");
              return category === "resolvedOutputNonCanonical"
                ? await FP.prepareResolvedOutputNonCanonicalRecoveryMaterial(
                    evidence,
                    detectionId,
                    corpus,
                  )
                : await FP.prepareSpendInputSignerMissingRecoveryMaterial(
                    evidence,
                    detectionId,
                    corpus,
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
          JSON.stringify(FP.encodeWorkflowArtifact(material)),
        );
        const fresh = await prepare();
        expect(FP.requireWorkflowArtifactMatches(recorded, fresh)).toBe(fresh);
        expect(() =>
          FP.requireWorkflowArtifactMatches(recorded, {
            ...fresh,
            detectionId: "changed",
          }),
        ).toThrow("freshly authenticated material");
      },
      120_000,
    );
  },
);
