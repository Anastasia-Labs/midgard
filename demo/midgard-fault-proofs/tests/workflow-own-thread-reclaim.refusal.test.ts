/** A cancel of our thread that the funding permit refuses to bind holds the
 * run under its own named alarm, naming the thread, its out-ref and the
 * refusal, never the generic stall of a funding failure. Nothing is signed,
 * and the alarm clears by itself: once the cause is gone, the next
 * observation binds and sends the cancel. */
import { CML, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { unsafeWorkflowFundingReservationSelectedOutRefsForTest } from "../src/workflow/funding-reservation-permit.js";
import { MemoryFraudProofWorkflowJournalStore } from "../src/workflow/journal.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
} from "../src/workflow/orchestrator.js";
import { runAdmittedFraudProofWorkflow } from "../src/workflow/orchestrator.run-admitted-fraud-proof-workflow.js";
import { threadReclaimAction } from "../src/workflow/own-thread-reclaim.js";
import { FraudProofResolvedByAnotherRecordError } from "../src/workflow/resolved-by-another-record.js";
import { readWorkflowRuntimeFundingPolicy } from "../src/workflow/runtime-funding-policy.js";
import { bindWorkflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { releaseFinality } from "./support/raw-l1-terminal-fixture.js";
import {
  makeAdapter,
  REFERENCE_OUT_REF,
  REFERENCE_SCRIPT_HASH,
  terminalVerifier,
} from "./workflow.make-adapter.js";
import {
  chain,
  decorate,
  FINAL_DEPTH,
  type Ledger,
  resolution,
} from "./workflow-own-thread-reclaim.fixtures.js";
import { DEPLOYMENT } from "./workflow-runtime.admitted-actuation.js";
import {
  runtimeFunding,
  signedFundingTransaction,
} from "./workflow-runtime.runtime-funding.js";

const TOKEN_POLICY = "e1".repeat(28);
const TOKEN = `${TOKEN_POLICY}aa`;
const THREAD_LOVELACE = 3_000_000n;
const FUNDING_LOVELACE = 15_000_000n;
const FEE = 200_000n;

/** Our thread at a governed contract, and a funded run whose permit sees
 * it as confirmed only once `confirmed` holds it. */
const scene = async () => {
  const sample = await runtimeFunding("step-one");
  const address = readWorkflowRuntimeFundingPolicy(sample.policy).contracts[0]!
    .address;
  const ours: UTxO = {
    txHash: "91".repeat(32),
    outputIndex: 0,
    address,
    assets: { lovelace: THREAD_LOVELACE, [TOKEN]: 1n },
    datum: "00",
  };
  const ourRef = `${ours.txHash}#0`;
  const confirmed: UTxO[] = [];
  const journal = new MemoryFraudProofWorkflowJournalStore();
  const runtime = await runtimeFunding("step-one", {
    begin: false,
    journal,
    action: threadReclaimAction({
      threadOutRef: ourRef,
      step: 2,
      lovelace: THREAD_LOVELACE.toString(),
    }),
    additionalInputs: [ours],
    confirmedInputs: confirmed,
  });
  // The chain as our port reads it; the cancel's hash is known once sent.
  const view: { ledger: Ledger } = { ledger: chain({ thread: ourRef }) };
  const sent: string[] = [];
  const port = {
    observe: async () => view.ledger.threads(),
    preflight: async ({ actionId }: { readonly actionId: string }) => {
      const burn = CML.Mint.new();
      burn.set(
        CML.ScriptHash.from_hex(TOKEN_POLICY),
        CML.AssetName.from_hex("aa"),
        -1n,
      );
      const signed = signedFundingTransaction({
        inputOutRefs: [
          ...unsafeWorkflowFundingReservationSelectedOutRefsForTest(
            runtime.permit,
          ).fundingOutRefs,
          ourRef,
        ],
        outputLovelace: FUNDING_LOVELACE + THREAD_LOVELACE - FEE,
        fee: FEE,
        mint: burn,
      });
      return bindWorkflowPreflightTransaction(
        Object.freeze({
          actionId,
          txHash: signed.toHash(),
          scriptExecution: "reference_scripts" as const,
          localUplcEvaluation: {
            status: "passed" as const,
            evaluator: "uplc-v1",
          },
          referenceScripts: [
            {
              role: "family-step-cancel",
              outRef: REFERENCE_OUT_REF,
              scriptHash: REFERENCE_SCRIPT_HASH,
            },
          ] as const,
        }),
        signed,
      );
    },
    submit: async ({
      preflight,
    }: {
      readonly preflight: { readonly txHash: string };
    }) => {
      sent.push(preflight.txHash);
      return preflight.txHash;
    },
  };
  const base: FraudProofFamilyWorkflowAdapter = {
    ...makeAdapter(),
    observe: async () => {
      const standing = view.ledger.threads().standing;
      throw new FraudProofResolvedByAnotherRecordError(
        standing === undefined
          ? resolution
          : { ...resolution, standingThread: standing },
      );
    },
  };
  const adapter = decorate(base, port);
  const { headerHash } = runtime.actuation;
  const run = () =>
    runAdmittedFraudProofWorkflow({
      deploymentFingerprint: DEPLOYMENT,
      category: "doubleSpend",
      headerHash,
      evidenceBinding: {
        route: "canonical_block",
        headerHash,
        payloadEnvelopeSha256: "77".repeat(32),
        payloadSha256: "78".repeat(32),
        l1BlockHash: "79".repeat(32),
        l1Slot: "1",
      },
      prepareFamilyArtifact: async () => ({}),
      registry: createFraudProofWorkflowRegistry({
        adapters: [adapter],
        launchScope: ["doubleSpend"],
      }),
      journal,
      terminalVerifier,
      releaseFinality: {
        ...releaseFinality,
        deploymentIdentityDigest: DEPLOYMENT,
      },
    });
  return { ours, ourRef, confirmed, view, sent, run };
};

describe("workflow: a cancel of our thread the funding permit refuses", () => {
  it("raises its own alarm naming the thread, and sends the cancel once the cause is gone", async () => {
    const { ours, ourRef, confirmed, view, sent, run } = await scene();
    for (let pass = 0; pass < 2; pass += 1) {
      const held = await run();
      expect(held).toMatchObject({
        kind: "pending",
        resumeOnObservation: true,
        namedState: { name: "fraud-proof-thread-reclaim-refused", alarm: true },
        reason: expect.stringContaining(
          `the funding permit refused the cancel of our thread ${ourRef}; it is built again at the next observation: funding released custody lacks confirmed workflow lineage`,
        ),
      });
      if (held.kind !== "pending") throw new Error("the run did not hold");
      expect(held.reason).not.toContain("funding reservation failed");
      expect(held.entries.map(({ event }) => event.kind)).not.toContain(
        "stalled",
      );
    }
    expect(sent).toEqual([]);

    // The thread's lineage is confirmed now: the same cancel binds.
    confirmed.push(ours);
    const sending = await run();
    expect(sending.kind).toBe("pending");
    expect(sending).not.toHaveProperty("namedState");
    expect(sent).toHaveLength(1);
    view.ledger = chain({ thread: ourRef, reclaimTx: sent[0]! });
    view.ledger.land(FINAL_DEPTH);
    const resolved = await run();
    expect(resolved).toMatchObject({
      kind: "resolved_by_another_record",
      reclaimed: [{ txHash: sent[0], threadOutRef: ourRef }],
    });
  });
});
