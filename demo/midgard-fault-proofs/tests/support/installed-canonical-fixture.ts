import {
  buildMidgardValidationTraceTree,
  decodeMidgardTxOutput,
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  encodeMidgardTxOutput,
  hashMidgardValidationMachineState,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
} from "@al-ft/midgard-core";
import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  EventKey,
  validationTraceDescriptorDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  RejectCodes,
  validatePhaseASingle,
  validationSemanticResolverIndex,
} from "@al-ft/midgard-validation";
import {
  makeMinAdaFundedExactSizeOutputItem,
  makeNativeTx,
  outRefFromByte,
  outRefFromTxId,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { buildForcedValidationDisputeCommitments } from "./emulator/validation-dispute-fixtures.build-forced-validation-dispute-commitments.js";
import { buildDeterministicValidationTraceMembers } from "./node-validation-trace-export.mjs";

/** Funded, signed output sources exported by the production node trace builder. */
export const buildInstalledCanonicalFixture = async ({
  operatorVkey,
  now,
  outputSizes = [16_384],
  dishonestChallenger = false,
  invalidSignature = false,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  readonly outputSizes?: readonly number[];
  readonly dishonestChallenger?: boolean;
  readonly invalidSignature?: boolean;
}) => {
  const key = CML.PrivateKey.from_normal_bytes(
    Buffer.concat([Buffer.alloc(31), Buffer.from([1])]),
  );
  const address = Buffer.from(
    CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(key.to_public().hash()),
    )
      .to_address()
      .to_raw_bytes(),
  );
  const outputs = outputSizes.map((size) =>
    encodeMidgardTxOutput({
      ...decodeMidgardTxOutput(makeMinAdaFundedExactSizeOutputItem(size)),
      address,
    }),
  );
  outputs.forEach((output, index) =>
    expect(output.length).toBe(outputSizes[index]),
  );
  const funding = outputs.reduce(
    (total, output) => total + decodeMidgardTxOutput(output).value.lovelace,
    0n,
  );
  const output = encodeMidgardTxOutput({
    ...decodeMidgardTxOutput(makeMinAdaFundedExactSizeOutputItem(100)),
    address,
    value: { lovelace: funding, assets: new Map() },
  });
  const rejected = outputs.some(
    (output) =>
      output.length >
      MIDGARD_CONSENSUS_PROFILE.limits.maxLedgerOutputPreimageBytes,
  );
  const spent = outRefFromByte(0x11);
  const transaction = makeNativeTx({
    privateKey: key,
    version: 1n,
    ...(invalidSignature ? { invalidVkeyWitness: true as const } : {}),
    spendInputs: [spent],
    outputs,
  });
  const forcedBytes = encodeMidgardForcedTxCanonical(transaction.tx);
  const source = deriveMidgardForcedTxProofSource(transaction.tx);
  const phaseA = validatePhaseASingle(
    {
      txId: transaction.txId,
      txCbor: forcedBytes,
      sourceKind: "forced",
      programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
      arrivalSeq: 0n,
      createdAt: new Date(0),
    },
    {
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      minFeeA: 0n,
      minFeeB: 0n,
      expectedNetworkId: 0n,
      concurrency: 1,
      strictnessProfile: "phase1_midgard",
    },
  );
  if (rejected)
    expect(phaseA).toMatchObject({
      code: RejectCodes.InvalidFieldType,
      consensusPhase: "canonicalDecode",
      subject: {
        arm: "OutputNonCanonical",
        index: BigInt(
          outputs.findIndex(
            (output) =>
              output.length >
              MIDGARD_CONSENSUS_PROFILE.limits.maxLedgerOutputPreimageBytes,
          ),
        ),
      },
    });
  else expect(phaseA).not.toHaveProperty("code");
  const ledgerOps = [
    { type: "delete" as const, key: spent },
    ...outputs.map((outputCbor, index) =>
      buildValidationMachineLedgerInsertOp({
        key: outRefFromTxId(transaction.txId, BigInt(index)),
        outputCbor,
      }),
    ),
  ];
  const ledgerWitnessEntries = [{ outRef: spent, output }];
  const acceptedMutationSteps = await buildValidationMachineLedgerMutationSteps(
    {
      initialEntries: ledgerWitnessEntries,
      operations: ledgerOps,
    },
  );
  const preUtxosRoot = acceptedMutationSteps[0]!.preRoot.toString("hex");
  const postUtxosRoot = rejected
    ? preUtxosRoot
    : acceptedMutationSteps.at(-1)!.postRoot.toString("hex");
  const actualLedgerOps = rejected ? [] : ledgerOps;
  const ledgerMutationSteps = rejected ? [] : acceptedMutationSteps;
  const txOrderId = { transactionId: "5a".repeat(32), outputIndex: 0n };
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const challengerReplayInput = {
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    blockEndTimeMs: now + 1000,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    blockSlot: 0n,
    eventKeyCbor: Buffer.from(Data.to(eventKey, EventKey), "hex"),
    transactionId: transaction.txId,
    canonicalTransactionCbor: forcedBytes,
    programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
    sourceKind: "forced" as const,
    priorUtxosRoot: preUtxosRoot,
    postUtxosRoot,
    ledgerWitnessEntries,
    ledgerMutationSteps,
    expectedLedgerOps: actualLedgerOps,
    expectedVerdict: rejected ? ("rejected" as const) : ("accepted" as const),
    expectedRejectionCode: rejected ? RejectCodes.InvalidFieldType : null,
  };
  const [exported] = await Effect.runPromise(
    buildDeterministicValidationTraceMembers({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      blockEndTime: new Date(now + 1000),
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      transactions: [
        {
          ...challengerReplayInput,
          eventKey,
          ledgerOps: actualLedgerOps,
          verdict: challengerReplayInput.expectedVerdict,
          rejectionCode: challengerReplayInput.expectedRejectionCode,
        },
      ],
    }),
  );
  let challengerTrace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace(challengerReplayInput),
  );
  expect(challengerTrace.verdict).toBe(rejected ? "rejected" : "accepted");
  if (rejected)
    expect(challengerTrace.states.at(-2)?.phase).toBe("canonicalDecode");
  const challengerDescriptor = validationTraceDescriptorDataFromCore(
    challengerTrace.tree.descriptor,
  );
  expect(exported?.value).toEqual(challengerDescriptor);
  expect(exported!.witnesses.length).toBe(challengerTrace.witnesses.length + 2);
  const disputedLowIndex = challengerTrace.states.findIndex(
    (state, index) =>
      state.phase === "canonicalDecode" &&
      challengerTrace.witnesses[index]?.auxiliary?.kind ===
        "transactionFieldItem" &&
      challengerTrace.witnesses[index]!.auxiliary!.fieldIndex === 2,
  );
  if (disputedLowIndex < 0)
    throw new Error("node trace omitted the complete output item");
  const forgedTerminal = {
    ...challengerTrace.states.at(-1)!,
    workRoot: Buffer.alloc(32, 0x7e),
    verdict: "accepted" as const,
    rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  };
  const states = challengerTrace.states.map((state, index) =>
    index <= disputedLowIndex
      ? state
      : dishonestChallenger
        ? { ...state, workRoot: Buffer.alloc(32, 0x7e) }
        : forgedTerminal,
  );
  const honestTrace = challengerTrace;
  const forgedTrace = {
    ...challengerTrace,
    verdict: "accepted" as const,
    rejectionCode: null,
    states,
    tree: buildMidgardValidationTraceTree(
      states.map(hashMidgardValidationMachineState),
      "accepted",
      MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
    ),
  };
  if (dishonestChallenger && honestTrace.verdict !== "accepted")
    throw new Error(
      "manual false challenger requires an honest accepted source",
    );
  const operatorTrace = dishonestChallenger
    ? {
        ...honestTrace,
        verdict: "accepted" as const,
        rejectionCode: null,
        states: [...honestTrace.states],
      }
    : forgedTrace;
  if (dishonestChallenger) challengerTrace = forgedTrace;
  const { header, claim } = await buildForcedValidationDisputeCommitments({
    operatorVkey,
    now,
    txOrderId,
    eventKey,
    forcedTransaction: {
      tx_id: transaction.txId.toString("hex"),
      submitted_source: {
        compact_cbor: source.compactCbor.toString("hex"),
        witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          source.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict: "ForcedTxValid",
    },
    operatorTrace,
    preUtxosRoot,
    postUtxosRoot,
  });
  return {
    header,
    claim,
    operatorTrace,
    challengerTrace,
    disputedLowIndex,
    challengerReplayInput,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      challengerTrace.tree.descriptor,
    ),
    evidence: {
      oneStepArgument: {
        resolverIndex: 0,
        semanticResolverIndex: validationSemanticResolverIndex(
          challengerTrace.witnesses[disputedLowIndex]!,
        ),
      },
    },
  };
};
