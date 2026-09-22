import { existsSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { join } from "node:path";

import * as FaultProofs from "@al-ft/midgard-fault-proofs";
import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import type { PublishedDepositTraceCheckpoint } from "midgard-watcher/tests/support/published-deposit-trace";
import { beforeAll, describe, expect, it } from "vitest";

import { readJourneyArtifact } from "./artifacts.js";
import { verifyJourneyFixture } from "./fixture-verification.js";
import { classifyFullCatalogueTransactionFixture } from "./full-catalogue-verification.js";
import { buildJourneyScriptForcedSuccessor } from "./script-cases.js";
import {
  buildJourneyTransactionControl,
  buildJourneyTransactionFault,
  JOURNEY_TRANSACTION_CATEGORIES,
  type JourneyTransactionCategory,
  type JourneyTransactionInput,
} from "./transaction-cases.js";
import {
  buildJourneyForcedTransaction,
  JOURNEY_FORCED_TRANSACTION_CATEGORIES,
  type JourneyForcedTransactionCategory,
  prepareJourneyForcedTransaction,
} from "./transaction-forced-cases.js";
import { prepareJourneyTransactionProof } from "./transaction-proof-material.js";
import {
  buildJourneyTransactionSourceFault,
  JOURNEY_TRANSACTION_SOURCE_CATEGORIES,
  type JourneyTransactionSourceCategory,
} from "./transaction-source-cases.js";

const replayers = {
  daHashPreimage: FaultProofs.DA_HASH_PREIMAGE_COMPLETE_CANONICAL_REPLAY,
  canonicalDecodability:
    FaultProofs.CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY,
  committedFieldShape:
    FaultProofs.COMMITTED_FIELD_SHAPE_COMPLETE_CANONICAL_REPLAY,
  l2TxMistag: FaultProofs.L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY,
  mintAuthorization: FaultProofs.MINT_AUTHORIZATION_COMPLETE_CANONICAL_REPLAY,
  zeroInput: FaultProofs.ZERO_INPUT_COMPLETE_CANONICAL_REPLAY,
  invalidRange: FaultProofs.INVALID_RANGE_COMPLETE_CANONICAL_REPLAY,
  invalidSignature: FaultProofs.INVALID_SIGNATURE_COMPLETE_CANONICAL_REPLAY,
  missingSignature: FaultProofs.MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY,
  inputSetUniqueness:
    FaultProofs.INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY,
  networkId: FaultProofs.NETWORK_ID_COMPLETE_CANONICAL_REPLAY,
  minFee: FaultProofs.MIN_FEE_COMPLETE_CANONICAL_REPLAY,
  minAda: FaultProofs.MIN_ADA_COMPLETE_CANONICAL_REPLAY,
  valueNotPreserved: FaultProofs.VALUE_NOT_PRESERVED_COMPLETE_CANONICAL_REPLAY,
  spendInputSignerMissing:
    FaultProofs.SPEND_INPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY,
  protectedOutputSignerMissing:
    FaultProofs.PROTECTED_OUTPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY,
  observersForbiddenOnUntaggedNetwork:
    FaultProofs.OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_COMPLETE_CANONICAL_REPLAY,
  observerOrderInvalid:
    FaultProofs.OBSERVER_ORDER_INVALID_COMPLETE_CANONICAL_REPLAY,
  transactionOutputNonCanonical:
    FaultProofs.TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  resolvedOutputNonCanonical:
    FaultProofs.RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  fieldPreimageLengthMismatch:
    FaultProofs.FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  fieldItemWidthIllegal:
    FaultProofs.FIELD_ITEM_WIDTH_ILLEGAL_COMPLETE_CANONICAL_REPLAY,
  mintDeclaredAssetLimit:
    FaultProofs.MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY,
  mintItemNonCanonical:
    FaultProofs.MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  distinctAssetAccumulationLimit:
    FaultProofs.DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
} satisfies Record<
  | JourneyTransactionCategory
  | JourneyForcedTransactionCategory
  | JourneyTransactionSourceCategory,
  FaultProofs.CompleteCanonicalReplay
>;

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;
const stagedPath =
  runDirectory === undefined
    ? undefined
    : join(runDirectory, "work/journeys/transition-trace/staged.json");

describe.skipIf(stagedPath === undefined || !existsSync(stagedPath))(
  "transaction fixtures against a retained real deposit",
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
        operatorVkey: staged.current.header.operatorVkey,
        endTime: predecessor.header.endTime + 40_000n,
        blockSlot: predecessor.header.blockSlot + 40n,
      };
    });

    const verify = async (
      category: keyof typeof replayers,
      block: Parameters<typeof verifyJourneyFixture>[0]["block"],
      expected: "fault" | "healthy",
    ) =>
      verifyJourneyFixture({
        category,
        replayer: replayers[category],
        block,
        predecessor,
        history: [staged.predecessor],
        expected,
      });

    it.each(JOURNEY_TRANSACTION_CATEGORIES)(
      "classifies %s exactly and accepts its signed valid control",
      async (category) => {
        const block = await buildJourneyTransactionFault({
          ...input,
          category,
        });
        expect(block.replay.trace.verdict).toBe("rejected");
        expect(block.header.prevUtxosRoot).toBe(predecessor.header.utxosRoot);
        const verified = await verify(category, block, "fault");
        expect(
          await prepareJourneyTransactionProof({
            category,
            block,
            verified,
            predecessor,
          }),
        ).toBeDefined();
        const control = await buildJourneyTransactionControl({
          ...input,
          category,
        });
        expect(control.replay.trace.verdict).toBe("accepted");
        await verify(category, control, "healthy");
      },
    );

    it.each(JOURNEY_FORCED_TRANSACTION_CATEGORIES)(
      "classifies wrongful %s rejection and accepts the same forced transaction",
      async (category) => {
        // Diagnostic local order identity only. Live staging must use the confirmed
        // real order output; this test is never counted as live journey evidence.
        const forcedInput = {
          ...input,
          category,
          orderKey: { transactionId: "52".repeat(32), outputIndex: 0n },
        };
        const fault = await buildJourneyForcedTransaction(forcedInput);
        expect(fault.replay.trace.verdict).toBe("accepted");
        const verified = await verify(category, fault, "fault");
        expect(
          await prepareJourneyTransactionProof({
            category,
            block: fault,
            verified,
            predecessor,
          }),
        ).toBeDefined();
        const control = await buildJourneyForcedTransaction({
          ...forcedInput,
          honest: true,
        });
        expect(control.replay.trace.verdict).toBe("accepted");
        await verify(category, control, "healthy");
      },
    );
    it.each(JOURNEY_TRANSACTION_SOURCE_CATEGORIES)(
      "routes exact %s source bytes and accepts original control",
      async (category) => {
        const fault = await buildJourneyTransactionSourceFault({
          ...input,
          category,
        });
        const verified = await verify(category, fault, "fault");
        expect(
          await prepareJourneyTransactionProof({
            category,
            block: fault,
            verified,
            predecessor,
          }),
        ).toBeDefined();
        expect(fault.control.replay.trace.verdict).toBe("accepted");
        await verify(category, fault.control, "healthy");
      },
    );
    it.each(JOURNEY_TRANSACTION_CATEGORIES)(
      "installed catalogue selects %s",
      async (category) => {
        const block = await buildJourneyTransactionFault({
          ...input,
          category,
        });
        const { decision } = await classifyFullCatalogueTransactionFixture({
          block,
          predecessor,
          history: [staged.predecessor],
        });
        expect(decision).toMatchObject({
          decision: "fault_detected",
          category,
        });
        const control = await buildJourneyTransactionControl({
          ...input,
          category,
        });
        const healthy = await classifyFullCatalogueTransactionFixture({
          block: control,
          predecessor,
          history: [staged.predecessor],
        });
        expect(healthy.decision).toMatchObject({
          decision: "healthy",
          headerHash: control.headerHash,
        });
      },
    );
    it.each(JOURNEY_FORCED_TRANSACTION_CATEGORIES)(
      "installed catalogue selects wrongful %s",
      async (category) => {
        const orderKey = { transactionId: "52".repeat(32), outputIndex: 0n };
        const block = await buildJourneyForcedTransaction({
          ...input,
          category,
          orderKey,
        });
        const { decision } = await classifyFullCatalogueTransactionFixture({
          block,
          predecessor,
          history: [staged.predecessor],
          forced: {
            transaction: prepareJourneyForcedTransaction(input).transaction,
            orderKey,
          },
        });
        expect(decision).toMatchObject({
          decision: "fault_detected",
          category,
        });
        const control = await buildJourneyForcedTransaction({
          ...input,
          category,
          orderKey,
          honest: true,
        });
        const healthy = await classifyFullCatalogueTransactionFixture({
          block: control,
          predecessor,
          history: [staged.predecessor],
          forced: {
            transaction: prepareJourneyForcedTransaction(input).transaction,
            orderKey,
          },
        });
        expect(healthy.decision).toMatchObject({
          decision: "healthy",
          headerHash: control.headerHash,
        });
      },
    );
    it.each(JOURNEY_TRANSACTION_SOURCE_CATEGORIES)(
      "installed catalogue selects raw %s",
      async (category) => {
        const block = await buildJourneyTransactionSourceFault({
          ...input,
          category,
        });
        const { decision } = await classifyFullCatalogueTransactionFixture({
          block,
          predecessor,
          history: [staged.predecessor],
          originTransaction: prepareJourneyForcedTransaction(input).transaction,
        });
        expect(decision).toMatchObject({
          decision: "fault_detected",
          category,
        });
        const healthy = await classifyFullCatalogueTransactionFixture({
          block: block.control,
          predecessor,
          history: [staged.predecessor],
        });
        expect(healthy.decision).toMatchObject({
          decision: "healthy",
          headerHash: block.control.headerHash,
        });
      },
    );
    it("continues with wallet change after a protected-script successor", async () => {
      const scriptSuccessor = await buildJourneyScriptForcedSuccessor({
        ...input,
        category: "receivePurposeLanguage",
        orderKey: { transactionId: "52".repeat(32), outputIndex: 0n },
      });
      expect(scriptSuccessor.replay.trace.verdict).toBe("accepted");
      expect(scriptSuccessor.payload.block_body.utxos.length).toBeGreaterThan(
        1,
      );
      const control = await buildJourneyTransactionControl({
        ...input,
        predecessor: scriptSuccessor,
        endTime: scriptSuccessor.header.endTime + 40_000n,
        blockSlot: scriptSuccessor.header.blockSlot + 40n,
        category: "invalidSignature",
      });
      expect(control.replay.trace.verdict).toBe("accepted");
    });
  },
);
