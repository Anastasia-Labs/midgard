import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";
import { WORKFLOW_CONTINUATION_QUIET_BOUND_MS } from "@al-ft/midgard-fault-proofs/workflow/pending-continuation";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  toUnit,
  utxoToCore,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  entry,
  expectQuietBoundRefusal,
  identity,
  pollCorrectionProgress,
} from "./correction.journey-progress-clock.test-support.js";
import { journeyProgressClock } from "./correction.js";
import { journeyTimingForCategory } from "./journey-timing.js";
/**
 * validationTraceDispute's undefended case: the watcher's workflow opens a
 * case (Init, Open, VerifySource) that nobody defends, then journals nothing
 * until it may close the case Convicted past its defense deadline,
 * `started_at + C + L_d`. The journey's progress clock comes from the
 * family's own timing at Preprod cadence and the rehearsal profile's clocks,
 * as the runner builds it.
 */
describe("the progress clock over an undefended validation-trace case", () => {
  const timing = journeyTimingForCategory("validationTraceDispute", {
    slotLengthSeconds: 1,
    activeSlotsCoeff: 0.05,
    confirmationDepth: 30,
    actionDepth: 1,
    disputeTiming: DEPLOYMENT_PROFILES["preprod-rehearsal"].timing,
  });
  const quietBoundMs = WORKFLOW_CONTINUATION_QUIET_BOUND_MS;
  const defenseWindowMs = timing.allowances.undefendedCaseMs;
  /** The case's `started_at`, chain time. */
  const startedAt = 1_800_000_000_000;
  const deadline = startedAt + defenseWindowMs;
  const address = credentialToAddress("Preprod", {
    type: "Script",
    hash: "d4".repeat(28),
  });
  const caseUnit = toUnit("c3".repeat(28), "e5".repeat(32));
  const bondUnit = toUnit("c3".repeat(28), "f6".repeat(32));
  // The VerifySource transaction that created the case, as the chain holds it.
  const verifySource = (() => {
    const outputs = CML.TransactionOutputList.new();
    for (const [unit, datum] of [
      [
        caseUnit,
        Data.to(
          {
            version: 1n,
            block_header_hash: identity.target.headerHash,
            event_key: { L2TransactionEventKey: { tx_id: "a7".repeat(32) } },
            challenger_root: "b8".repeat(32),
            step_count: 1_000n,
            challenger_terminal_hash: "c9".repeat(32),
            bounty_claimant: "da".repeat(28),
            agreed_initial_hash: "eb".repeat(32),
            started_at: BigInt(startedAt),
            open_threads: 0n,
            defenders_root: "",
            defender_won: false,
            neither_claim_valid: false,
          },
          SDK.Case,
        ),
      ],
      [bondUnit, Data.void()],
    ] as const)
      outputs.add(
        utxoToCore({
          txHash: "00".repeat(32),
          outputIndex: 0,
          address,
          assets: { lovelace: 5_000_000n, [unit]: 1n },
          datum,
        }).output(),
      );
    return CML.Transaction.new(
      CML.TransactionBody.new(
        CML.TransactionInputList.new(),
        outputs,
        200_000n,
      ),
      CML.TransactionWitnessSet.new(),
      true,
    );
  })();
  const verifySourceHash = CML.hash_transaction(verifySource.body()).to_hex();
  const transactions = {
    transaction: async (txHash: string) => {
      expect(txHash).toBe(verifySourceHash);
      return { cbor: verifySource.to_cbor_hex() };
    },
  };
  /** One stage's intent, submission and confirmation, as the watcher journals them. */
  const stage = (
    sequence: number,
    name: "init" | "open" | "verify_source",
    consumes: string | null,
    txHash: string,
    outputs: readonly (readonly [role: string, unit: string])[],
  ) => {
    const actionId = `validation-trace-${name.replaceAll("_", "-")}:${consumes ?? "first"}`;
    return [
      entry(sequence, {
        kind: "submission_intent",
        actionId,
        actionInput: {
          schemaVersion: "midgard-validation-trace-dispute-action-v1",
          stage: name,
          consumes,
        },
        durableRecovery: {
          validationTrace: {
            schemaVersion: "midgard-validation-trace-dispute-recovery-v1",
            stage: name,
            outputs: outputs.map(([role, unit], index) => ({
              role,
              outRef: `${txHash}#${index.toString()}`,
              address,
              datumCborSha256: "0f".repeat(32),
              unit,
            })),
          },
        },
        attempt: 1,
        txHash,
      }),
      entry(sequence + 1, {
        kind: "submitted",
        actionId,
        attempt: 1,
        txHash,
      }),
      entry(sequence + 2, { kind: "confirmed", actionId, txHash }),
    ];
  };
  const initHash = "1a".repeat(32);
  const openHash = "2b".repeat(32);
  const threadUnit = toUnit("c3".repeat(28), "1c".repeat(32));
  const openerUnit = toUnit("c3".repeat(28), "2d".repeat(32));
  const started = [entry(0, { kind: "started" })];
  const opened = [
    ...started,
    ...stage(1, "init", null, initHash, [["thread", threadUnit]]),
    ...stage(4, "open", `${initHash}#0`, openHash, [["opener", openerUnit]]),
  ];
  const caseOpen = [
    ...opened,
    ...stage(7, "verify_source", `${openHash}#0`, verifySourceHash, [
      ["case", caseUnit],
      ["bond", bondUnit],
    ]),
  ];
  // The gate exactly as the runner builds it from the family's timing.
  const gate = () => journeyProgressClock({ timing, transactions });

  it("catches a stall before the case opens after the watcher's quiet bound", async () => {
    // Open landed; VerifySource, which creates the case, never does.
    const openedAt = startedAt - 300_000;
    await expectQuietBoundRefusal(
      pollCorrectionProgress({
        retained: [],
        observations: [
          { at: openedAt, records: opened },
          { at: openedAt + quietBoundMs, records: opened },
          { at: openedAt + quietBoundMs + 1, records: opened },
        ],
        startAt: openedAt - 60_000,
        gate: gate(),
      }),
      {
        at: "at observation 3 of 3: ",
        last: `last action validation-trace-open:${initHash}#0`,
      },
    );
  });

  it("tolerates silence through the case's defense deadline and its Close, then catches it", async () => {
    // VerifySource's confirmation is journaled two minutes after the case
    // started; the deadline runs from the case's start, not from that record.
    const confirmedAt = startedAt + 120_000;
    await expect(
      pollCorrectionProgress({
        retained: [],
        observations: [
          { at: confirmedAt, records: caseOpen },
          { at: confirmedAt + quietBoundMs + 1, records: caseOpen },
          { at: startedAt + defenseWindowMs / 2, records: caseOpen },
          { at: deadline + quietBoundMs, records: caseOpen },
          { at: deadline + quietBoundMs + 1, records: caseOpen },
        ],
        startAt: confirmedAt - 60_000,
        gate: gate(),
      }),
    ).rejects.toThrow(
      `at observation 5 of 5: Workflow made no journaled progress within ${quietBoundMs.toString()} ms of its counterparty wait's deadline ${deadline.toString()}`,
    );
  });

  it("gives a runner resumed mid-wait the same deadline", async () => {
    const resumedAt = startedAt + defenseWindowMs / 2;
    await expect(
      pollCorrectionProgress({
        retained: caseOpen,
        observations: [
          { at: resumedAt, records: caseOpen },
          // Past the watcher's quiet bound, still inside the defense window.
          { at: deadline - 1, records: caseOpen },
          { at: deadline + quietBoundMs, records: caseOpen },
          { at: deadline + quietBoundMs + 1, records: caseOpen },
        ],
        startAt: resumedAt,
        gate: gate(),
      }),
    ).rejects.toThrow(
      `at observation 4 of 4: Workflow made no journaled progress within ${quietBoundMs.toString()} ms of its counterparty wait's deadline ${deadline.toString()}`,
    );
  });
});
