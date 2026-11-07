import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { RejectCodes, runPhaseAValidation } from "../src/index.js";
import type { PhaseBResultWithPatch } from "../src/phase-b.js";
import {
  expectSinglePhaseBRejection,
  preState,
  runPhaseB,
} from "./phase-b.harness.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeQueued,
  nativeScriptWitness,
  outRefFromByte,
  TEST_ADDRESS_BYTES,
} from "./validation-fixtures.js";

const txIds = (txs: PhaseBResultWithPatch["accepted"]) =>
  txs.map((tx) => tx.ledgerTx.txId.toString("hex"));

// Admission is part of this regression: each candidate comes from signed raw
// transaction bytes accepted by the production Phase A entrypoint.
describe("phase B plain conflict fallback", () => {
  it.each([
    { fault: "output network", code: RejectCodes.NetworkIdMismatch },
    { fault: "integrity hash", code: RejectCodes.InvalidFieldType },
  ])(
    "preserves plain $fault rejection when a conflict component falls back",
    async ({ fault, code }) => {
      const spent = outRefFromByte(0x2a);
      const address = Buffer.from(TEST_ADDRESS_BYTES);
      if (fault === "output network") address[0] = (address[0]! & 0xf0) | 1;
      const plain = makeNativeTx({
        spendInputs: [spent],
        outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE, address)],
        scriptLanguages: fault === "integrity hash" ? ["PlutusV3"] : [],
      });
      const native = nativeScriptWitness({ type: "all", scripts: [] });
      const scripted = makeNativeTx({
        spendInputs: [spent],
        outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
        scriptWitnesses: [native],
        requiredObserverItems: [Buffer.from(hashScriptWitness(native), "hex")],
      });
      const admitted = await Effect.runPromise(
        runPhaseAValidation(
          [
            makeQueued(plain.txId, plain.txCbor, 0n),
            makeQueued(scripted.txId, scripted.txCbor, 1n),
          ],
          {
            expectedNetworkId: 0n,
            minFeeA: 0n,
            minFeeB: 0n,
            concurrency: 1,
            strictnessProfile: "phase-a-unit",
          },
        ),
      );
      expect(admitted.rejected).toStrictEqual([]);
      expect(txIds(admitted.accepted)).toStrictEqual([
        plain.txId.toString("hex"),
        scripted.txId.toString("hex"),
      ]);
      const [plainCandidate, scriptedCandidate] = admitted.accepted;
      expect(plainCandidate.submission.txCbor).toStrictEqual(plain.txCbor);
      expect(scriptedCandidate.submission.txCbor).toStrictEqual(
        scripted.txCbor,
      );
      expect(plainCandidate.derived.requiresLocalScriptDiscovery).toBe(false);
      expect(scriptedCandidate.derived.requiresLocalScriptDiscovery).toBe(true);
      const state = preState([[spent, makeOutput(FUNDED_OUTPUT_LOVELACE)]]);
      const singleton = await runPhaseB([plainCandidate], state);
      const rejection = expectSinglePhaseBRejection(singleton, code);
      const scriptedSingleton = await runPhaseB([scriptedCandidate], state);
      expect(scriptedSingleton.rejected).toStrictEqual([]);
      expect(txIds(scriptedSingleton.accepted)).toStrictEqual([
        scripted.txId.toString("hex"),
      ]);

      // The later native observer forces their shared-spend component onto
      // the full caller after the plain probe has already rejected the first.
      const together = await runPhaseB(admitted.accepted, state);
      expect(together.rejected).toStrictEqual([rejection]);
      expect(txIds(together.accepted)).toStrictEqual([
        scripted.txId.toString("hex"),
      ]);
      expect(together.statePatch).toStrictEqual(scriptedSingleton.statePatch);
    },
  );
});
