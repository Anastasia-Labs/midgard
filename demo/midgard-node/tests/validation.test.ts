import { describe, expect, it } from "vitest";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import fs from "node:fs";
import { LedgerUtils } from "@/database/index.js";
import {
  PhaseAAccepted,
  QueuedTx,
  RejectCodes,
  runPhaseAValidation,
  runPhaseBValidation,
} from "@/validation/index.js";

type TxFixture = {
  cborHex: string;
  txId: string;
};

const phaseAConfig = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  concurrency: 4,
  strictnessProfile: "phase1_midgard",
} as const;

const fixturePath = new URL("./txs/txs_0.json", import.meta.url);
const fixtures = JSON.parse(fs.readFileSync(fixturePath, "utf8")) as TxFixture[];

const toQueuedTx = (
  tx: InstanceType<typeof CML.Transaction>,
  arrivalSeq = 1n,
): QueuedTx => {
  const txBody = tx.body();
  return {
    txId: Buffer.from(CML.hash_transaction(txBody).to_raw_bytes()),
    txCbor: Buffer.from(tx.to_cbor_bytes()),
    arrivalSeq,
    createdAt: new Date("2026-01-01T00:00:00.000Z"),
  };
};

const loadFixtureTx = (index = 0): InstanceType<typeof CML.Transaction> =>
  CML.Transaction.from_cbor_hex(fixtures[index].cborHex);

const runPhaseAOne = async (
  tx: InstanceType<typeof CML.Transaction>,
): Promise<{
  accepted: readonly PhaseAAccepted[];
  rejected: readonly { code: string; detail: string | null }[];
}> => {
  const result = await Effect.runPromise(
    runPhaseAValidation([toQueuedTx(tx)], phaseAConfig),
  );

  return {
    accepted: result.accepted,
    rejected: result.rejected.map((r) => ({
      code: r.code,
      detail: r.detail,
    })),
  };
};

const runPhaseAAcceptedFixture = async (): Promise<PhaseAAccepted> => {
  const result = await Effect.runPromise(
    runPhaseAValidation([toQueuedTx(loadFixtureTx())], phaseAConfig),
  );
  expect(result.rejected).toHaveLength(0);
  expect(result.accepted).toHaveLength(1);
  return result.accepted[0];
};

const nowWithinCandidateInterval = (candidate: PhaseAAccepted): bigint => {
  if (candidate.validityIntervalStart !== undefined) {
    return candidate.validityIntervalStart;
  }
  if (candidate.validityIntervalEnd !== undefined) {
    return candidate.validityIntervalEnd;
  }
  return 0n;
};

const buildPreStateForCandidate = (
  candidate: PhaseAAccepted,
): LedgerUtils.Entry[] => {
  const firstProduced = candidate.processedTx.produced[0];
  const firstProducedOutput = CML.TransactionOutput.from_cbor_bytes(
    firstProduced[LedgerUtils.Columns.OUTPUT],
  );
  const validInputOutput = CML.TransactionOutput.new(
    firstProducedOutput.address(),
    CML.Value.from_coin(firstProducedOutput.amount().coin() + candidate.fee),
  );

  return [
    {
      [LedgerUtils.Columns.TX_ID]: Buffer.alloc(32, 1),
      [LedgerUtils.Columns.OUTREF]: candidate.processedTx.spent[0],
      [LedgerUtils.Columns.OUTPUT]: Buffer.from(validInputOutput.to_cbor_bytes()),
      [LedgerUtils.Columns.ADDRESS]: firstProducedOutput.address().to_bech32(),
    },
  ];
};

const stripWitnesses = (candidate: PhaseAAccepted): PhaseAAccepted => ({
  ...candidate,
  witnessKeyHashes: [],
  nativeScriptHashes: [],
});

const withReferenceInput = (
  candidate: PhaseAAccepted,
  outRef: Buffer,
): PhaseAAccepted => ({
  ...candidate,
  referenceInputs: [outRef],
});

describe("phase-1 validation", () => {
  it("accepts a valid fixture in phase A", async () => {
    const result = await runPhaseAOne(loadFixtureTx());
    expect(result.rejected).toHaveLength(0);
    expect(result.accepted).toHaveLength(1);
  });

  it("allows non-empty reference_inputs in phase A shape checks", async () => {
    const tx = loadFixtureTx();
    const txBody = tx.body();

    const refInputs = CML.TransactionInputList.new();
    refInputs.add(txBody.inputs().get(0));
    txBody.set_reference_inputs(refInputs);

    const mutatedTx = CML.Transaction.new(
      txBody,
      tx.witness_set(),
      tx.is_valid(),
      tx.auxiliary_data(),
    );

    const result = await runPhaseAOne(mutatedTx);
    // We no longer reject reference_inputs as unsupported.
    // This mutated tx is unsigned for the updated body, so failure is expected,
    // but it should not fail with E_UNSUPPORTED_FIELD_NONEMPTY(reference_inputs).
    expect(
      result.rejected.some(
        (r) =>
          r.code === RejectCodes.UnsupportedFieldNonEmpty &&
          r.detail === "reference_inputs",
      ),
    ).toBe(false);
  });

  it("rejects non-empty script_data_hash in phase A", async () => {
    const tx = loadFixtureTx();
    const txBody = tx.body();

    txBody.set_script_data_hash(CML.ScriptDataHash.from_hex("00".repeat(32)));

    const mutatedTx = CML.Transaction.new(
      txBody,
      tx.witness_set(),
      tx.is_valid(),
      tx.auxiliary_data(),
    );

    const result = await runPhaseAOne(mutatedTx);
    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.UnsupportedFieldNonEmpty);
    expect(result.rejected[0].detail).toBe("script_data_hash");
  });

  it("rejects missing input in phase B", async () => {
    const candidate = await runPhaseAAcceptedFixture();
    const result = runPhaseBValidation([candidate], [], {
      nowMillis: nowWithinCandidateInterval(candidate),
    });

    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.InputNotFound);
  });

  it("rejects missing key witness for spent pubkey input in phase B", async () => {
    const candidate = await runPhaseAAcceptedFixture();
    const preState = buildPreStateForCandidate(candidate);
    const result = runPhaseBValidation([stripWitnesses(candidate)], preState, {
      nowMillis: nowWithinCandidateInterval(candidate),
    });

    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.MissingRequiredWitness);
  });

  it("rejects missing reference input in phase B", async () => {
    const candidate = await runPhaseAAcceptedFixture();
    const preState = buildPreStateForCandidate(candidate);
    const missingReferenceOutRef = Buffer.alloc(36, 0xab);

    const result = runPhaseBValidation(
      [withReferenceInput(candidate, missingReferenceOutRef)],
      preState,
      {
        nowMillis: nowWithinCandidateInterval(candidate),
      },
    );

    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.InputNotFound);
    expect(result.rejected[0].detail).toContain("reference input not found");
  });

  it("rejects double spend in phase B", async () => {
    const candidate = await runPhaseAAcceptedFixture();
    const preState = buildPreStateForCandidate(candidate);

    const result = runPhaseBValidation([candidate, candidate], preState, {
      nowMillis: nowWithinCandidateInterval(candidate),
    });

    expect(result.accepted).toHaveLength(1);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.DoubleSpend);
  });

  it("accepts value-preserving candidate in phase B", async () => {
    const candidate = await runPhaseAAcceptedFixture();
    const preState = buildPreStateForCandidate(candidate);

    const result = runPhaseBValidation([candidate], preState, {
      nowMillis: nowWithinCandidateInterval(candidate),
    });

    expect(result.rejected).toHaveLength(0);
    expect(result.accepted).toHaveLength(1);
  });
});
