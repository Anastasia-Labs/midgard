import { decodeMidgardNativeByteListPreimage } from "@al-ft/midgard-core";
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
  transitionProofCbor,
  type TransitionProofInput,
} from "./proof-material.js";
import { transitionProofHistorySource } from "./proof-material.js";
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
  depositSourceCbor?: string;
  depositAssetCount?: number;
  depositAssetIndexes?: Readonly<Record<string, number>>;
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
  if ("AcceptedTransactionTransitionMismatch" in proof.fault)
    return ["claimStructure", "claimSource", "claimEndpoints"].map((key) => ({
      key: key as YieldKey,
      redeemer: Data.void(),
    }));
  if (!("InvalidOneStepTransition" in proof.fault)) return [];
  const witness = proof.fault.InvalidOneStepTransition.witness;
  let outputCbors: Buffer[];
  let spendInputKeys: string[];
  let outputIndex = 0;
  let depositSourceCbor: string | undefined;
  let depositAssetCount: number | undefined;
  let depositAssetIndexes: Record<string, number> | undefined;
  let openKey: YieldKey;
  let summaryKey: YieldKey;
  if ("L2TransactionTransition" in witness) {
    const l2 = witness.L2TransactionTransition;
    outputCbors = [
      ...decodeMidgardNativeByteListPreimage(
        Buffer.from(l2.outputs_preimage, "hex"),
        "transition outputs",
      ),
    ];
    spendInputKeys = decodeMidgardNativeByteListPreimage(
      Buffer.from(l2.spend_inputs_preimage, "hex"),
      "transition inputs",
    ).map((bytes) => Buffer.from(bytes).toString("hex"));
    openKey = "l2Open";
    summaryKey = "l2Summaries";
  } else if ("ValidDepositTransition" in witness) {
    const deposit = witness.ValidDepositTransition;
    if (depositOpening === undefined)
      throw new Error(
        "Transition deposit projection requires its retained opening",
      );
    const source = deposit.source_membership;
    const { commitment, opening, infoCbor } = reopenTransitionDeposit(
      depositOpening,
      depositPolicyId,
      {
        ...source,
        valueCbor: transitionProofHistorySource(proofInput)!.valueCbor,
      },
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
    spendInputKeys = [];
    outputIndex = Number(source.key.outputIndex);
    openKey = "depositProjection";
    summaryKey = "depositSummaries";
  } else return [];
  const opened: SDK.TransitionTraceOpenedOutputs = {
    spend_input_keys: spendInputKeys,
    output_hashes: outputCbors.map((bytes) =>
      computeHash32(bytes).toString("hex"),
    ),
  };
  const summaries: SDK.TransitionTraceOutputSummaries = {
    summaries: outputCbors.map((bytes, index) => {
      const descriptor = buildCanonicalMidgardLedgerOutputMaterial({
        outputIndex: outputIndex + index,
        outputCbor: bytes,
      }).descriptor;
      const convert = (summary: typeof descriptor.cardanoTxOut) => ({
        root: Buffer.from(summary.root).toString("hex"),
        cbor_length: summary.cborLength,
        memory: summary.memory,
      });
      return [
        convert(descriptor.cardanoTxOut),
        convert(descriptor.midgardTxOut),
        convert(descriptor.cardanoSpendDatum),
      ];
    }),
  };
  return [
    {
      key: openKey,
      depositSourceCbor,
      depositAssetCount,
      depositAssetIndexes,
      redeemer: Data.to(opened, SDK.TransitionTraceOpenedOutputs),
      outputCbors: outputCbors.map((bytes) => bytes.toString("hex")),
    },
    {
      key: summaryKey,
      redeemer: Data.to(summaries, SDK.TransitionTraceOutputSummaries),
    },
    {
      key: openKey === "l2Open" ? "l2Assembly" : "depositAssembly",
      redeemer: Data.void(),
    },
    ...(openKey === "l2Open"
      ? [{ key: "l2Replay" as const, redeemer: Data.void() }]
      : []),
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
      depositOpening: input.depositOpening,
    },
    (_key, value: unknown) =>
      typeof value === "bigint" ? value.toString() : value,
  );
  if (cachedYieldData?.key !== key)
    cachedYieldData = { key, value: deriveTransitionTraceYieldData(input) };
  return structuredClone(cachedYieldData.value);
};
