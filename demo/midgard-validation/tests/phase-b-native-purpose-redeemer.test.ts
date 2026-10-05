import { encodeCbor, MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core";
import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { Lambda, UPLCEncoder, UPLCProgram, UPLCVar } from "@harmoniclabs/uplc";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { buildMidgardCanonicalCekProgram } from "../src/cek-program.js";
import {
  buildDeterministicValidationMachineTrace,
  RejectCodes,
} from "../src/index.js";
import { MidgardRedeemerTag } from "../src/midgard-redeemers.js";
import {
  expectSinglePhaseBRejection,
  preState,
  runPhaseB,
} from "./phase-b.harness.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  makePhaseBCandidate,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  nativeScriptWitness,
  outRefFromByte,
  plutusV3ScriptWitness,
} from "./validation-fixtures.js";

describe("phase B validation", () => {
  it("rejects a redeemer that points at a native-script purpose, as the fault proof does", async () => {
    // A native script runs without a redeemer, so a redeemer at its pointer
    // is extraneous: Cardano refuses it as ExtraRedeemers, and the fault
    // proof's redeemer audit finds it unused. A Plutus spend carries the
    // script integrity hash, so the transaction reaches that audit.
    const program = buildMidgardCanonicalCekProgram(
      Buffer.from(
        UPLCEncoder.compile(
          new UPLCProgram([1, 1, 0], new Lambda(new UPLCVar(0))),
        ),
      ),
    );
    const plutusScript = plutusV3ScriptWitness(program.envelopeCbor);
    const sidecar = encodeMidgardCekProgramMaterialSidecar([
      ...program.material.values(),
    ]);
    const nativeScript = nativeScriptWitness({ type: "all", scripts: [] });
    const policyId = hashScriptWitness(nativeScript);
    const spent = outRefFromByte(0x2e);
    const spentOutput = makeProtectedScriptOutput(
      hashScriptWitness(plutusScript),
      FUNDED_OUTPUT_LOVELACE,
    );
    const txOptions = {
      outputs: [
        makeOutput(
          FUNDED_OUTPUT_LOVELACE,
          undefined,
          new Map([[policyId, new Map([["beef", 5n]])]]),
        ),
      ],
      scriptWitnesses: [plutusScript, nativeScript],
      mintPreimageCbor: makeMintPreimageCbor(
        new Map([
          [
            Buffer.from(policyId, "hex"),
            new Map([[Buffer.from("beef", "hex"), 5n]]),
          ],
        ]),
      ),
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Spend, index: 0n },
        { tag: MidgardRedeemerTag.Mint, index: 0n },
      ]),
      scriptLanguages: ["PlutusV3" as const],
    };

    const result = await runPhaseB(
      [
        makePhaseBCandidate({
          spent: [spent],
          programMaterialSidecarCbor: sidecar,
          ...txOptions,
        }),
      ],
      preState([[spent, spentOutput]]),
    );
    const rejection = expectSinglePhaseBRejection(
      result,
      RejectCodes.InvalidFieldType,
    );
    expect(rejection.detail).toContain("extraneous redeemer");

    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      ...txOptions,
    });
    const unchangedRoot = Buffer.alloc(32, 0x2e).toString("hex");
    const trace = await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        eventKeyCbor: encodeCbor([2n, Buffer.alloc(32, 0x41)]),
        sourceKind: "normal",
        blockEndTimeMs: 1_750_000_000_000,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        blockSlot: 100n,
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        programMaterialSidecarCbor: sidecar,
        priorUtxosRoot: unchangedRoot,
        postUtxosRoot: unchangedRoot,
        ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
        expectedLedgerOps: [],
        ledgerMutationSteps: [],
        expectedVerdict: "rejected",
        expectedRejectionCode: RejectCodes.InvalidFieldType,
      }),
    );
    expect(trace.verdict).toBe("rejected");
  }, 60_000);
});
