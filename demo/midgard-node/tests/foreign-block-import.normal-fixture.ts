import {
  encodeMidgardCekProgramMaterialDaValue,
  encodeMidgardCekProgramMaterialSidecar,
} from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildMidgardCanonicalCekProgram,
  MidgardRedeemerTag,
  placeholderValidationTraceDescriptorV1,
  replayValidationVerdictEvent,
  validationMachineLedgerRoot,
  validationTransactionCommitmentV1,
} from "@al-ft/midgard-validation";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Lambda, UPLCEncoder, UPLCProgram, UPLCVar } from "@harmoniclabs/uplc";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { encodeTransactionRootValue } from "../src/mpf/ledger-hydration.js";
import {
  encodeEventToStepValueCbor,
  encodeTransitionIntegerCbor,
  encodeTransitionStepCbor,
} from "../src/mpf/transition-cbor.js";
import { countsFromLengths, headerFor } from "./da-payload.record.js";
import { rebind, verdict } from "./foreign-block-import.fixture.js";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "./midgard-output-helpers.js";
import { buildNativeTx } from "./native-transaction-integration.build-native-tx.js";

/** A signed native transfer spending a key-locked output. */
const signedTransfer = () => {
  const signer = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x17));
  const address = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(signer.to_public().hash()),
  ).to_address();
  const output = makeMidgardTxOutput(
    address,
    CML.Value.from_coin(3_000_000n),
  ).to_cbor_bytes();
  const spent = makeOutRefCbor(0x11);
  const tx = buildNativeTx({
    spendInputOutRefs: [spent],
    referenceInputOutRefs: [],
    outputCbors: [output],
    witnessMode: "valid",
    witnessSignerPrivateKey: signer,
  });
  return { spent, output, tx, material: [] };
};

/** A PlutusV3 spend of a script-locked output under an accepting program. */
const plutusSpend = () => {
  const program = buildMidgardCanonicalCekProgram(
    Buffer.from(
      UPLCEncoder.compile(
        new UPLCProgram([1, 1, 0], new Lambda(new UPLCVar(0))),
      ),
    ),
  );
  const script = plutusV3ScriptWitness(program.envelopeCbor);
  const spent = outRefFromByte(0x1d);
  const output = makeProtectedScriptOutput(
    hashScriptWitness(script),
    FUNDED_OUTPUT_LOVELACE,
  );
  const tx = makeNativeTx({
    spendInputs: [spent],
    outputs: [
      makeOutput(
        FUNDED_OUTPUT_LOVELACE,
        Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x4d)]),
      ),
    ],
    omitVkeyWitness: true,
    scriptWitnesses: [script],
    scriptLanguages: ["PlutusV3"],
    redeemerTxWitsPreimageCbor: makeRedeemersCbor([
      {
        tag: MidgardRedeemerTag.Spend,
        index: 0n,
        // Within the per-transaction execution cap.
        exUnits: [10_000_000n, 1_000_000_000n],
      },
    ]),
  });
  return { spent, output, tx, material: [...program.material.values()] };
};

/** Real normal transaction on a fabricated funded prior ledger: a signed
 * native transfer, or a PlutusV3 spend whose CEK material rides the block. The
 * alias control regenerates every semantic commitment using the wrong event
 * identity. */
export const normalFixture = async (
  alias = false,
  kind: "native" | "plutus" = "native",
) => {
  const { spent, output, tx, material } =
    kind === "plutus" ? plutusSpend() : signedTransfer();
  const sourceKey = alias ? "a7".repeat(32) : tx.txId.toString("hex");
  const event: SDK.EventKey = { L2TransactionEventKey: { tx_id: sourceKey } };
  const eventCbor = Data.to(event, SDK.EventKey);
  const prior = (
    await validationMachineLedgerRoot([{ outRef: spent, output }])
  ).toString("hex");
  const replay = await Effect.runPromise(
    replayValidationVerdictEvent({
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      canonicalTransactionCbor: tx.txCbor,
      programMaterialSidecarCbor:
        encodeMidgardCekProgramMaterialSidecar(material),
      ledgerWitnessEntries: [{ outRef: spent, output }],
      blockEndTimeMs: 2,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      sourceKind: "normal",
    }),
  );
  if (replay.verdict !== "accepted")
    throw new Error(`normal fixture was rejected: ${replay.rejection?.code}`);
  const post = (
    await validationMachineLedgerRoot(
      replay.statePatch.upsertedOutRefs.map(([key, value]) => ({
        outRef: Buffer.from(key, "hex"),
        output: value,
      })),
    )
  ).toString("hex");
  const descriptor = SDK.validationTraceDescriptorDataFromCore(
    placeholderValidationTraceDescriptorV1(
      {
        eventKeyCbor: Buffer.from(eventCbor, "hex"),
        transactionCommitment: validationTransactionCommitmentV1({
          canonicalTransactionCbor: tx.txCbor,
          sourceKind: "normal",
        }),
      },
      "accepted",
      null,
    ),
  );
  const counts = {
    ...countsFromLengths({ transactions: 1 }),
    validationTraceCount: 1n,
  };
  const roots = Object.fromEntries(
    [
      "utxosRoot",
      "withdrawalsRoot",
      "forcedTransactionsRoot",
      "transactionsRoot",
      "depositsRoot",
      "transitionTraceRoot",
      "eventToStepRoot",
      "validationTracesRoot",
    ].map((key) => [key, SDK.EMPTY_MERKLE_TREE_ROOT]),
  ) as Parameters<typeof headerFor>[0];
  const header = { ...headerFor(roots, counts), prevUtxosRoot: prior };
  const payload: SDK.DaPayload = {
    version: 1n,
    block_body: {
      header,
      header_hash: "00".repeat(28),
      utxos: replay.statePatch.upsertedOutRefs.map(([key, value]) => [
        key,
        value.toString("hex"),
      ]),
      transactions: [
        [sourceKey, encodeTransactionRootValue(tx.txCbor).toString("hex")],
      ],
      transaction_preimages: [[sourceKey, tx.txCbor.toString("hex")]],
      withdrawals: [],
      forced_transactions: [],
      forced_transaction_preimages: [],
      deposits: [],
      cek_program_material: material.map((entry) => [
        Buffer.from(entry.root).toString("hex"),
        encodeMidgardCekProgramMaterialDaValue(entry).toString("hex"),
      ]),
      transition_trace: [
        [
          encodeTransitionIntegerCbor(0n).toString("hex"),
          encodeTransitionStepCbor({
            schema_version: 1n,
            step_index: 0n,
            event_key: event,
            phase: "L2Transaction",
            pre_utxos_root: prior,
            post_utxos_root: post,
          }).toString("hex"),
        ],
      ],
      event_to_step: [
        [
          eventCbor,
          encodeEventToStepValueCbor({
            step_index: 0n,
            phase: "L2Transaction",
          }).toString("hex"),
        ],
      ],
      validation_traces: [
        [eventCbor, Data.to(descriptor, SDK.ValidationTraceDescriptor)],
      ],
      validation_trace_witnesses: [],
      counts,
    },
  };
  const bound = await rebind(payload);
  return {
    payload: bound,
    sourceKey,
    transactionId: tx.txId.toString("hex"),
    imported: () =>
      verdict(bound, {
        parentUtxosRoot: prior,
        parentEntries: [{ outref: spent, output }],
      }),
  };
};
