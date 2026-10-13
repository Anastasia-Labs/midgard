import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { forcedVerdictForRejection } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  placeholderValidationTraceDescriptorV1,
  replayValidationVerdictEvent,
  validationMachineLedgerRoot,
  validationTransactionCommitmentV1,
} from "@al-ft/midgard-validation";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it, vi } from "vitest";

const calls = vi.hoisted(() => [] as { time: number; slot: bigint }[]);
vi.mock("@al-ft/midgard-validation", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("@al-ft/midgard-validation")>();
  return {
    ...actual,
    replayValidationVerdictEvent: (
      input: Parameters<typeof actual.replayValidationVerdictEvent>[0],
    ) => {
      calls.push({ time: input.blockTimeMs, slot: input.blockSlot });
      return actual.replayValidationVerdictEvent(input);
    },
  };
});
import { encodeForcedInclusionValueV1 } from "../src/database/forcedTransactions.js";
import { encodeTransitionStepCbor } from "../src/mpf/transition-cbor.js";
import { fixture, rebind, verdict } from "./foreign-block-import.fixture.js";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "./midgard-output-helpers.js";
import { buildNativeTx } from "./native-transaction-integration.build-native-tx.js";

it("replays a catch-up forced verdict at its authenticated commit clock instead of coverage end", async () => {
  const signer = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x17));
  const address = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(signer.to_public().hash()),
  ).to_address();
  const output = Buffer.from(
    makeMidgardTxOutput(
      address,
      CML.Value.from_coin(3_000_000n),
    ).to_cbor_bytes(),
  );
  const spent = makeOutRefCbor(0x11);
  const authored = buildNativeTx({
    spendInputOutRefs: [spent],
    referenceInputOutRefs: [],
    outputCbors: [output],
    witnessMode: "valid",
    witnessSignerPrivateKey: signer,
    validityIntervalStart: 0n,
    validityIntervalEnd: 5n,
  });
  const canonicalTransactionCbor = encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical(authored.tx),
  );
  const ledgerWitnessEntries = [{ outRef: spent, output }];
  const context = {
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    canonicalTransactionCbor,
    ledgerWitnessEntries,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    blockSlot: 0n,
    sourceKind: "forced" as const,
  };
  const coverageVerdict = await Effect.runPromise(
    replayValidationVerdictEvent({
      ...context,
      blockTimeMs: 2,
      blockSlot: 2n,
    }),
  );
  expect(coverageVerdict.verdict).toBe("accepted");
  const commitVerdict = await Effect.runPromise(
    replayValidationVerdictEvent({
      ...context,
      blockTimeMs: 8,
      blockSlot: 8n,
    }),
  );
  expect(commitVerdict.verdict).toBe("rejected");
  if (commitVerdict.rejection === undefined)
    throw new Error("expired forced transaction had no rejection reason");
  const payload = await fixture();
  const body = payload.block_body;
  const root = (
    await validationMachineLedgerRoot(ledgerWitnessEntries)
  ).toString("hex");
  const step = Data.from(body.transition_trace[0]![1], SDK.TransitionStep);
  const descriptor = SDK.validationTraceDescriptorDataFromCore(
    placeholderValidationTraceDescriptorV1(
      {
        eventKeyCbor: Buffer.from(body.validation_traces[0]![0], "hex"),
        transactionCommitment: validationTransactionCommitmentV1({
          canonicalTransactionCbor,
          sourceKind: "forced",
        }),
      },
      "rejected",
      commitVerdict.rejection.code,
    ),
  );
  const encoded = await Effect.runPromise(
    encodeForcedInclusionValueV1({
      nativeTxCbor: canonicalTransactionCbor,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      verdict: forcedVerdictForRejection(commitVerdict.rejection),
    }),
  );
  const catchup = await rebind({
    ...payload,
    block_body: {
      ...body,
      header: {
        ...body.header,
        prevUtxosRoot: root,
        utxosRoot: root,
        blockSlot: 8n,
      },
      forced_transactions: [
        [body.forced_transactions[0]![0], encoded.value.toString("hex")],
      ],
      forced_transaction_preimages: [
        [
          body.forced_transaction_preimages[0]![0],
          canonicalTransactionCbor.toString("hex"),
        ],
      ],
      transition_trace: [
        [
          body.transition_trace[0]![0],
          encodeTransitionStepCbor({
            ...step,
            pre_utxos_root: root,
            post_utxos_root: root,
          }).toString("hex"),
        ],
      ],
      validation_traces: [
        [
          body.validation_traces[0]![0],
          Data.to(descriptor, SDK.ValidationTraceDescriptor),
        ],
      ],
    },
  });
  expect(catchup.block_body.header.endTime).toBe(2n);
  const result = await verdict(catchup, {
    blockTimeMs: 8,
    blockSlot: 8n,
    parentUtxosRoot: root,
    parentEntries: [{ outref: spent, output }],
  });
  expect(calls.at(-1)).toEqual({ time: 8, slot: 8n });
  if (result._tag === "Left") throw result.left;
  expect(result._tag).toBe("Right");
});
