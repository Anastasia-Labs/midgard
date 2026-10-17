import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { pathToFileURL } from "node:url";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import {
  deserializePhaseACandidate,
  MidgardRedeemerTag,
  type PhaseAResult,
  type PhaseAValidatedTx,
  type PhaseBResultWithPatch,
} from "@al-ft/midgard-validation";

import {
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  makeQueued,
  makeRedeemersCbor,
  outRefFromByte,
  outRefFromTxId,
  plutusV3ScriptWitness,
} from "../../midgard-validation/tests/validation-fixtures.js";
import { FixedValidationWorkerPool } from "../src/services/validation-pool.js";
import { packPhaseAJob } from "../src/workers/utils/validation-pool.js";

export const workerEntry = pathToFileURL(resolve("dist/validation.js"));

export const init = {
  config: {
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    strictnessProfile: "phase2_worker_test",
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  },
} as const;

export const nodeVerifierInit = {
  ...init,
  signatureVerifier: "node",
} as const;

const alwaysSucceedsBlueprint = JSON.parse(
  readFileSync(resolve("blueprints/always-succeeds/plutus.json"), "utf8"),
) as {
  readonly validators: readonly {
    readonly title: string;
    readonly compiledCode: string;
  }[];
};

export const alwaysSucceedsScriptBytes = Buffer.from(
  alwaysSucceedsBlueprint.validators.find(
    (validator) => validator.title === "midgard.deposit_spend.else",
  )?.compiledCode ?? "",
  "hex",
);

export const invalidTxs = (length: number) =>
  Array.from({ length }, (_, index) => ({
    txId: Buffer.alloc(32, index & 0xff),
    txCbor: Buffer.from("80", "hex"),
    arrivalSeq: BigInt(index),
    createdAt: new Date(index),
  }));

export const runWorkerPhaseA = async (
  pool: FixedValidationWorkerPool,
  queued: readonly ReturnType<typeof makeQueued>[],
): Promise<PhaseAResult> => {
  const responses = await Promise.all(
    Array.from({ length: Math.ceil(queued.length / 4) }, (_, chunk) =>
      pool.submit(
        packPhaseAJob(
          pool.allocateJobId(),
          queued.slice(chunk * 4, chunk * 4 + 4),
        ),
      ),
    ),
  );
  const accepted: PhaseAValidatedTx[] = [];
  const rejected: PhaseAResult["rejected"][number][] = [];
  for (const response of responses) {
    if (response.kind !== "phase_a") {
      throw new Error(`expected phase_a response, got ${response.kind}`);
    }
    for (const result of response.results) {
      if (result.ok) {
        accepted.push(deserializePhaseACandidate(result.candidate));
      } else {
        rejected.push({
          txId: Buffer.from(result.txId),
          code: result.code,
          detail: result.detail,
        });
      }
    }
  }
  return { accepted, rejected };
};

export const buildAdversarialCorpus = () => {
  if (alwaysSucceedsScriptBytes.length === 0) {
    throw new Error("missing always-succeeds Plutus spend fixture");
  }
  const shared = outRefFromByte(0x31);
  const parentInput = outRefFromByte(0x32);
  const invalidSignatureInput = outRefFromByte(0x33);
  const scriptInput = outRefFromByte(0x34);
  const budgetInput = outRefFromByte(0x35);
  const cycleLeftInput = outRefFromByte(0x36);
  const cycleRightInput = outRefFromByte(0x37);
  const script = plutusV3ScriptWitness(alwaysSucceedsScriptBytes);
  const scriptHash = hashScriptWitness(script);
  const parent = makeNativeTx({ spendInputs: [parentInput] });
  const fixtures = [
    makeNativeTx({ spendInputs: [shared] }),
    makeNativeTx({ spendInputs: [shared], validityIntervalEnd: 10n }),
    parent,
    makeNativeTx({ spendInputs: [outRefFromTxId(parent.txId)] }),
    makeNativeTx({
      spendInputs: [invalidSignatureInput],
      invalidVkeyWitness: true,
    }),
    makeNativeTx({
      spendInputs: [scriptInput],
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Spend, index: 0n },
      ]),
      scriptLanguages: ["PlutusV3"],
    }),
    makeNativeTx({
      spendInputs: [budgetInput],
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Spend,
          index: 0n,
          exUnits: [0n, 0n],
        },
      ]),
      scriptLanguages: ["PlutusV3"],
    }),
    makeNativeTx({ spendInputs: [cycleLeftInput] }),
    makeNativeTx({ spendInputs: [cycleRightInput] }),
  ];
  return {
    queued: fixtures.map((fixture, index) =>
      makeQueued(fixture.txId, fixture.txCbor, BigInt(index)),
    ),
    preState: new Map<string, Buffer>([
      [shared.toString("hex"), makeOutput(10n)],
      [parentInput.toString("hex"), makeOutput(10n)],
      [invalidSignatureInput.toString("hex"), makeOutput(10n)],
      [scriptInput.toString("hex"), makeProtectedScriptOutput(scriptHash, 10n)],
      [budgetInput.toString("hex"), makeProtectedScriptOutput(scriptHash, 10n)],
      [cycleLeftInput.toString("hex"), makeOutput(10n)],
      [cycleRightInput.toString("hex"), makeOutput(10n)],
    ]),
    cycleTxIds: [
      fixtures[7]!.txId.toString("hex"),
      fixtures[8]!.txId.toString("hex"),
    ] as const,
  };
};

export const injectDefensiveCycle = (
  candidates: readonly PhaseAValidatedTx[],
  cycleTxIds: readonly [string, string],
): readonly PhaseAValidatedTx[] => {
  const byId = new Map(
    candidates.map((candidate) => [
      candidate.ledgerTx.txId.toString("hex"),
      candidate,
    ]),
  );
  const left = byId.get(cycleTxIds[0]);
  const right = byId.get(cycleTxIds[1]);
  if (left === undefined || right === undefined) {
    throw new Error("cycle fixtures did not survive Phase A");
  }
  const leftConsumes = right.graph.produced[0]!.outref.toString("hex");
  const rightConsumes = left.graph.produced[0]!.outref.toString("hex");
  return candidates.map((candidate) => {
    const txIdHex = candidate.ledgerTx.txId.toString("hex");
    if (txIdHex === cycleTxIds[0]) {
      return {
        ...candidate,
        graph: {
          ...candidate.graph,
          spentOutRefHexes: [leftConsumes],
          referenceOutRefHexes: [],
        },
      };
    }
    if (txIdHex === cycleTxIds[1]) {
      return {
        ...candidate,
        graph: {
          ...candidate.graph,
          spentOutRefHexes: [rightConsumes],
          referenceOutRefHexes: [],
        },
      };
    }
    return candidate;
  });
};

export const normalizeVerdict = (
  phaseA: PhaseAResult,
  phaseB: PhaseBResultWithPatch,
) => ({
  acceptedTxIds: phaseB.accepted.map((candidate) =>
    candidate.ledgerTx.txId.toString("hex"),
  ),
  rejected: [...phaseA.rejected, ...phaseB.rejected].map((rejection) => ({
    txId: rejection.txId.toString("hex"),
    code: rejection.code,
    detail: rejection.detail,
  })),
  statePatch: phaseB.statePatch,
});
