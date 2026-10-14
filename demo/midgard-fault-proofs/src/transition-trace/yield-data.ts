import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerOutputMaterial,
  deriveCanonicalOriginalDepositTransitionEffect,
} from "@al-ft/midgard-validation";
import { Data, type Network } from "@lucid-evolution/lucid";

import {
  reopenTransitionDeposit,
  type TransitionDepositOpening,
} from "./history-opening.js";
import {
  readTransitionProof,
  transitionL2Replay,
  transitionProofCbor,
  type TransitionProofInput,
} from "./proof-material.js";
import { depositSourceView } from "./proof-material.js";
import { TRANSITION_TRACE_YIELD_REFERENCES } from "./yield-references.js";

type YieldKey = keyof typeof TRANSITION_TRACE_YIELD_REFERENCES;
const deriveTransitionTraceYieldData = ({
  proof: proofInput,
  network,
  depositPolicyId,
  depositOpening,
}: {
  proof: TransitionProofInput;
  network: Network;
  depositPolicyId: string;
  depositOpening?: TransitionDepositOpening;
}): readonly {
  key: YieldKey;
  redeemer: string;
  outputCbors?: readonly string[];
  /** The accepted transaction's spend-inputs field items. */
  spendInputCbors?: readonly string[];
  /** The ledger descriptor each output inserts, in output order. */
  outputDescriptors?: readonly string[];
  depositSourceCbor?: string;
  depositAssetCount?: number;
  depositAssetIndexes?: Readonly<Record<string, number>>;
  outputSummaries?: SDK.TransitionTraceOutputSummaries["summaries"];
}[] => {
  const proof = readTransitionProof(proofInput);
  if ("OmittedDueL1Event" in proof.fault)
    return [
      {
        key:
          "OmittedDueForcedTransaction" in proof.fault.OmittedDueL1Event.witness
            ? "forcedTiming"
            : "l1Event",
        redeemer: Data.void(),
      },
    ];
  if ("OutOfWindowSourceEvent" in proof.fault)
    return [
      {
        key:
          "OutOfWindowForcedTransaction" in
          proof.fault.OutOfWindowSourceEvent.witness
            ? "forcedTiming"
            : "l1Event",
        redeemer: Data.void(),
      },
    ];
  if (!("InvalidOneStepTransition" in proof.fault)) return [];
  const witness = proof.fault.InvalidOneStepTransition.witness;
  let outputCbors: Buffer[];
  let spendInputCbors: string[] | undefined;
  let outputIndex = 0;
  let depositSourceCbor: string | undefined;
  let depositAssetCount: number | undefined;
  let depositAssetIndexes: Record<string, number> | undefined;
  let openKey: YieldKey;
  if ("L2TransactionTransition" in witness) {
    const replay = transitionL2Replay(proofInput);
    if (replay === undefined)
      throw new Error(
        "Transition L2 replay requires its field items and ledger witnesses",
      );
    outputCbors = replay.outputCbors.map((item) => Buffer.from(item, "hex"));
    spendInputCbors = [...replay.spendInputCbors];
    openKey = "l2Open";
  } else if ("ValidDepositTransition" in witness) {
    const deposit = witness.ValidDepositTransition;
    if (depositOpening === undefined)
      throw new Error(
        "Transition deposit projection requires its retained opening",
      );
    const source = depositSourceView(deposit.source_membership);
    const { commitment, opening, infoCbor } = reopenTransitionDeposit(
      depositOpening,
      depositPolicyId,
      source,
    );
    const value = opening.original_assets;
    depositSourceCbor = Data.to(commitment, SDK.EventHistoryCommitment);
    depositAssetIndexes = Object.fromEntries(
      [...value]
        .filter(([policy]) => policy !== "")
        .flatMap(([policy, names]) =>
          [...names.keys()].sort().map((name, index) => [policy + name, index]),
        ),
    );
    depositAssetCount = [...value].reduce(
      (count, [policy, names]) => count + (policy === "" ? 0 : names.size),
      0,
    );
    const originalAssets = Object.fromEntries(
      [...value].flatMap(([policy, names]) =>
        [...names].map(([name, quantity]) => [
          policy === "" ? "lovelace" : policy + name,
          quantity,
        ]),
      ),
    );
    const effect = deriveCanonicalOriginalDepositTransitionEffect({
      configuredNetwork: network,
      eventId: {
        transactionId: source.key.transactionId,
        outputIndex: source.key.outputIndex,
      },
      l2NetworkId: source.value.l2_network_id,
      l2Address: source.value.l2_address,
      l2DatumCbor:
        source.value.l2_datum === null
          ? null
          : Buffer.from(plutusConstrFieldCbor(infoCbor, [2, 0]), "hex"),
      originalAssets,
    });
    const operation = effect.operations[0];
    if (operation?.type !== "insert")
      throw new Error("transition deposit did not project one insert");
    outputCbors = [operation.outputCbor];
    outputIndex = Number(source.key.outputIndex);
    openKey = "depositProjection";
  } else return [];
  // The summaries the phase-8 walk derives on chain, so the stepper can write
  // the checkpoint that commits them, and the descriptor each output inserts.
  const materials = outputCbors.map((bytes, index) =>
    buildCanonicalMidgardLedgerOutputMaterial({
      outputIndex: outputIndex + index,
      outputCbor: bytes,
    }),
  );
  const convert = (
    summary: (typeof materials)[number]["descriptor"]["cardanoTxOut"],
  ) => ({
    root: Buffer.from(summary.root).toString("hex"),
    cbor_length: summary.cborLength,
    memory: summary.memory,
  });
  return [
    {
      key: openKey,
      depositSourceCbor,
      depositAssetCount,
      depositAssetIndexes,
      // A deposit's projection names the one output's hash; the accepted
      // transaction's steps build their redeemers from the walked fields.
      redeemer:
        openKey === "depositProjection"
          ? Data.to(computeHash32(outputCbors[0]!).toString("hex"))
          : Data.void(),
      outputCbors: outputCbors.map((bytes) => bytes.toString("hex")),
      ...(spendInputCbors === undefined ? {} : { spendInputCbors }),
      outputDescriptors: materials.map((material) =>
        material.descriptorCbor.toString("hex"),
      ),
      outputSummaries: materials.map(({ descriptor }) => [
        convert(descriptor.cardanoTxOut),
        convert(descriptor.midgardTxOut),
        convert(descriptor.cardanoSpendDatum),
      ]),
    },
  ];
};

// A continuation changes only the authenticated checkpoint. Rebuilding all
// semantic output commitments at every hop is quadratic in the output size.
// Keep one content-addressed derivation; callers receive a detached value.
let cachedYieldData:
  | { key: string; value: ReturnType<typeof deriveTransitionTraceYieldData> }
  | undefined;
export const transitionTraceYieldData = (
  input: Parameters<typeof deriveTransitionTraceYieldData>[0],
): ReturnType<typeof deriveTransitionTraceYieldData> => {
  const key = JSON.stringify(
    {
      network: input.network,
      depositPolicyId: input.depositPolicyId,
      proofCbor: transitionProofCbor(input.proof),
      l2Replay: transitionL2Replay(input.proof),
      depositOpening: input.depositOpening,
    },
    (_key, value: unknown) =>
      typeof value === "bigint" ? value.toString() : value,
  );
  if (cachedYieldData?.key !== key)
    cachedYieldData = { key, value: deriveTransitionTraceYieldData(input) };
  return structuredClone(cachedYieldData.value);
};
