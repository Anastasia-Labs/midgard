/** The challenger's claim material for a committed transaction whose
 * PlutusV3 script fails: its trace proofs verify against its own root, and
 * its state 0 is the initial state VerifySource derives from the header, the
 * source and the step's pre-state root (N5). */
import {
  deriveMidgardValidationInitialStateV1,
  hashMidgardValidationMachineState,
  MIDGARD_VALIDATION_MACHINE_VERSION,
  MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION,
  type MidgardValidationTraceDescriptor,
  verifyMidgardValidationTraceProof,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { plutusExecutionBlockFixture } from "./helpers/plutus-execution-block-fixture.js";
import { claimMaterialOf } from "./support/validation-trace-claim-material.js";

const claimMaterial = async () => {
  const fixture = await plutusExecutionBlockFixture("failing");
  return { fixture, material: await claimMaterialOf(fixture) };
};

describe("validation-trace dispute claim material", () => {
  it("proves its Terminal Rejected leaf and its state 0 under its own root", async () => {
    const { material } = await claimMaterial();
    const descriptor: MidgardValidationTraceDescriptor = {
      schemaVersion: MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION,
      machineVersion: MIDGARD_VALIDATION_MACHINE_VERSION,
      traceRoot: Buffer.from(material.challengerRoot, "hex"),
      stepCount: Number(material.stepCount),
      initialStateHash: hashMidgardValidationMachineState(
        material.initialState,
      ),
      terminalStateHash: hashMidgardValidationMachineState(material.terminal),
      verdict: "rejected",
      rejectionCodeHash: material.terminal.rejectionCodeHash,
    };
    expect(material.stepCount).toBeGreaterThan(0n);
    expect(material.terminal).toMatchObject({
      phase: "terminal",
      verdict: "rejected",
    });
    expect(material.terminalLeafProof.stateIndex).toBe(
      Number(material.stepCount),
    );
    expect(material.terminalLeafProof.stateHash).toStrictEqual(
      hashMidgardValidationMachineState(material.terminal),
    );
    const initialProof = material.leafProof(0);
    expect(initialProof.stateHash).toStrictEqual(
      hashMidgardValidationMachineState(material.initialState),
    );
    for (const proof of [material.terminalLeafProof, initialProof])
      expect(
        verifyMidgardValidationTraceProof({
          descriptor,
          proof,
        }),
      ).toBe(true);
    const foreign = Buffer.alloc(32, 0x5c);
    expect(
      verifyMidgardValidationTraceProof({
        descriptor,
        proof: { ...material.terminalLeafProof, stateHash: foreign },
      }),
    ).toBe(false);
  });

  it("starts from the initial state derived from the header, source and pre-state root", async () => {
    const { fixture, material } = await claimMaterial();
    const header = fixture.evidence.header;
    const step = material.claimWitness.transition_step_membership.value;
    expect(
      deriveMidgardValidationInitialStateV1({
        endTimeMs: header.endTime,
        blockSlot: header.blockSlot,
        expectedNetworkId: header.expectedNetworkId,
        minFeeA: header.minFeeA,
        minFeeB: header.minFeeB,
        eventKeyCbor: Buffer.from(
          Data.to(fixture.eventKey, SDK.EventKey),
          "hex",
        ),
        source: {
          normal: { canonicalTransactionCbor: fixture.tx.canonicalCbor },
        },
        preUtxosRoot: Buffer.from(step.pre_utxos_root, "hex"),
      }),
    ).toStrictEqual(material.initialState);
  });

  it("names the event under the header's committed roots", async () => {
    const { fixture, material } = await claimMaterial();
    const header = fixture.evidence.header;
    const claim = material.claimWitness;
    expect(claim.event_to_step_membership.key).toStrictEqual(fixture.eventKey);
    expect(claim.event_to_step_membership.root).toBe(header.eventToStepRoot);
    expect(claim.transition_step_membership.root).toBe(
      header.transitionTraceRoot,
    );
    expect(claim.transition_step_membership.key).toBe(
      claim.event_to_step_membership.value.step_index,
    );
    expect(claim.source_membership).toMatchObject({
      NormalValidationSource: {
        membership: { root: header.transactionsRoot, key: fixture.tx.txId },
      },
    });
  });
});
