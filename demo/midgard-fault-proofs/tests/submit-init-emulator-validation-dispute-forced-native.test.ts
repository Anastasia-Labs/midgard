import { writeFileSync } from "node:fs";

import {
  buildMidgardValidationTraceTree,
  deriveMidgardForcedTxProofSourceFromCanonicalCbor,
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
} from "@al-ft/midgard-core";
import { validationTraceDescriptorDataFromCore } from "@al-ft/midgard-sdk";
import { buildValidationDisputeEvidenceBundle } from "@al-ft/midgard-validation";
import {
  nativeFaultFixture,
  type NativeFaultShape,
} from "@al-ft/midgard-validation/tests/forced-native-first-fault-fixture";
import { CML, Data } from "@lucid-evolution/lucid";
import { afterAll, expect, it } from "vitest";

import { committedValidationClaimEndpointsAndSourceAreValid } from "../src/validation-dispute/claim-endpoints.js";
import { requireStagedOneStepArgument } from "../src/validation-dispute/submit/reference-scripts.require-staged-one-step-argument.js";
import { forcedRejectionReason } from "../src/workflow/forced-rejection-reason.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { buildForcedValidationDisputeCommitments } from "./support/emulator/validation-dispute-fixtures.build-forced-validation-dispute-commitments.js";
import { runForcedValidationDisputeScenario } from "./support/submit-init-emulator-shared.js";

const measurements: {
  name: string;
  signedBytes: number;
  memory: bigint;
  cpu: bigint;
  publication: boolean;
}[] = [];
const measure: NonNullable<
  NonNullable<
    Parameters<typeof runForcedValidationDisputeScenario>[1]
  >["onSubmittedTransaction"]
> = (m, cbor) => {
  const outputs = CML.Transaction.from_cbor_hex(cbor).body().outputs();
  let publication = false;
  for (let i = 0; i < outputs.len(); i++)
    publication ||= outputs.get(i).script_ref() !== undefined;
  expect(m.completeSignedBytes).toBeLessThanOrEqual(
    publication ? 15_872 : 16_384,
  );
  expect(m.executionMemory).toBeLessThanOrEqual(13_200_000n);
  expect(m.executionSteps).toBeLessThanOrEqual(8_000_000_000n);
  measurements.push({
    name: expect.getState().currentTestName!,
    signedBytes: m.completeSignedBytes,
    memory: m.executionMemory,
    cpu: m.executionSteps,
    publication,
  });
};
afterAll(() => {
  if (process.env.MIDGARD_NATIVE_FAULT_MEASUREMENTS != null)
    writeFileSync(
      process.env.MIDGARD_NATIVE_FAULT_MEASUREMENTS,
      JSON.stringify(
        measurements,
        (_, v: unknown) => (typeof v === "bigint" ? v.toString() : v),
        2,
      ),
    );
});

/** Ordinary bisection is mandatory for Signatures; this fixture uses a genuine
 * rejected native trace with the committed ForcedTxInvalid source in both roles. */
const fixture = async (
  { operatorVkey, now }: { operatorVkey: string; now: number },
  dishonest: boolean,
  shape: NativeFaultShape,
) => {
  const { trace, canonicalTransactionCbor, phaseA } = await nativeFaultFixture(
    shape,
    now,
    0n,
  );
  const handoffCase = shape === "present" || shape === "earlierFalse";
  const low = handoffCase
    ? trace.witnesses.findIndex(
        (w, i) =>
          w.phase === "signatures" &&
          trace.states[i + 1]?.phase === "phaseANativeScripts",
      )
    : trace.witnesses.reduce(
        (last, w, i) => (w.phase === "phaseANativeScripts" ? i : last),
        -1,
      );
  if (low < 0) throw new Error("missing genuine native transition");
  let dishonestSuccessorRoot = Buffer.alloc(32, 0x7d);
  if (handoffCase) {
    // Recreate the retired producer's fabricated already-scanned handoff.
    // Keep the signatures pre-state/witness authentic so refusal reaches the
    // native successor check, rather than an unrelated source or hash guard.
    const retiredControl = Data.from(
      trace.witnesses[low + 1]!.cbor.toString("hex"),
    ) as unknown[];
    expect(retiredControl.slice(6, 8)).toEqual([-1n, 0n]);
    retiredControl[6] = shape === "earlierFalse" ? 2n : 1n;
    retiredControl[7] = retiredControl[6];
    dishonestSuccessorRoot = hashMidgardValidationWorkWitness({
      phase: trace.states[low + 1]!.phase,
      programCounter: trace.states[low + 1]!.programCounter,
      witnessCbor: Buffer.from(Data.to(retiredControl as never), "hex"),
    });
  }
  const states = trace.states.map((s, i) =>
    i <= low
      ? s
      : {
          ...s,
          workRoot:
            i === low + 1 ? dishonestSuccessorRoot : Buffer.alloc(32, 0x7d),
        },
  );
  const forged = {
    ...trace,
    states,
    tree: buildMidgardValidationTraceTree(
      states.map(hashMidgardValidationMachineState),
      trace.verdict,
      trace.tree.descriptor.rejectionCodeHash,
    ),
  };
  const operatorTrace = dishonest ? trace : forged,
    challengerTrace = dishonest ? forged : trace;
  const source = deriveMidgardForcedTxProofSourceFromCanonicalCbor(
    canonicalTransactionCbor,
  );
  const txOrderId = { transactionId: "4d".repeat(32), outputIndex: 0n };
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const root = trace.states[0]!.priorLedgerRoot.toString("hex");
  const { header, claim } = await buildForcedValidationDisputeCommitments({
    operatorVkey,
    now,
    txOrderId,
    eventKey,
    forcedTransaction: {
      tx_id: trace.states[0]!.transactionId.toString("hex"),
      submitted_source: {
        compact_cbor: source.compactCbor.toString("hex"),
        witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          source.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict: {
        ForcedTxInvalid: {
          reason: (() => {
            if ("ledgerTx" in phaseA)
              throw new Error("expected Phase A rejection");
            return forcedRejectionReason(phaseA);
          })(),
        },
      },
    },
    operatorTrace,
    preUtxosRoot: root,
    postUtxosRoot: root,
  });
  expect(
    committedValidationClaimEndpointsAndSourceAreValid(header, claim),
  ).toBe(true);
  const evidence = buildValidationDisputeEvidenceBundle({
    operatorTrace,
    challengerTrace,
    currentTime: now + 2000,
  });
  expect(evidence.finalDispute.lowIndex).toBe(low);
  const staged = requireStagedOneStepArgument(evidence.oneStepArgument);
  if (!handoffCase) {
    expect(staged.semanticResolverIndex).toBe(
      shape === "invalidChildren"
        ? 4
        : shape === "invalidThresholdChildren"
          ? 6
          : 8,
    );
  }
  return {
    header,
    claim,
    operatorTrace,
    challengerTrace,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      challengerTrace.tree.descriptor,
    ),
    evidence,
    claimedLedgerDeltaRoot: trace.states[0]!.ledgerDeltaRoot,
  };
};
it.each([
  "present",
  "earlierFalse",
  "missingKey",
  "invalidChildren",
  "invalidThresholdChildren",
  "exhaustedBoundary",
] as const)(
  "proves the exact rejecting %s native transition by bisection",
  async (shape) => {
    const result = await runForcedValidationDisputeScenario(
      (input) => fixture(input, false, shape),
      { onSubmittedTransaction: measure },
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  },
  600000,
);
it.each([
  "present",
  "earlierFalse",
  "missingKey",
  "invalidChildren",
  "invalidThresholdChildren",
  "exhaustedBoundary",
] as const)(
  "refuses a dishonest successor against the exact rejecting %s native transition",
  async (shape) => {
    const failure = await expectOnchainRefusal(() =>
      runForcedValidationDisputeScenario(
        (input) => fixture(input, true, shape),
        { onSubmittedTransaction: measure },
      ),
    );
    expect(failure).toContain("semantic-resolution");
  },
  600000,
);
