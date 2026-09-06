import { decodeMidgardNativeByteListPreimage } from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerOutputMaterial,
  deriveCanonicalDepositTransitionEffect,
} from "@al-ft/midgard-validation";
import { Data, type Network, type UTxO } from "@lucid-evolution/lucid";

import { TRANSITION_TRACE_YIELD_REFERENCES } from "./yield-references.js";

type YieldKey = keyof typeof TRANSITION_TRACE_YIELD_REFERENCES;
const deriveTransitionTraceYieldData = ({
  proof,
  network,
  depositPolicyId,
  additionalReferenceInputs,
}: {
  proof: SDK.TransitionFaultProof;
  network: Network;
  depositPolicyId: string;
  additionalReferenceInputs: readonly UTxO[];
}): readonly {
  key: YieldKey;
  redeemer: string;
  outputCbors?: readonly string[];
  depositSourceCbor?: string;
  depositAssetCount?: number;
  depositAssetIndexes?: Readonly<Record<string, number>>;
}[] => {
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
    const unit = depositPolicyId + deposit.event_asset_name;
    const candidates = additionalReferenceInputs.filter(
      (utxo) => utxo.assets[unit] === 1n,
    );
    if (candidates.length !== 1)
      throw new Error(
        "transition deposit projection requires its unique authenticated event reference",
      );
    const source = deposit.source_membership;
    const value = new Map<string, Map<string, bigint>>();
    for (const [unit, quantity] of Object.entries(candidates[0]!.assets)) {
      const policy = unit === "lovelace" ? "" : unit.slice(0, 56);
      const name = unit === "lovelace" ? "" : unit.slice(56);
      const names = value.get(policy) ?? new Map<string, bigint>();
      names.set(name, quantity);
      value.set(policy, names);
    }
    depositSourceCbor = Data.to([
      Data.from(
        Data.to(
          {
            transactionId: candidates[0]!.txHash,
            outputIndex: BigInt(candidates[0]!.outputIndex),
          },
          SDK.OutputReference,
        ),
      ),
      depositPolicyId,
      deposit.event_asset_name,
    ]);
    depositAssetIndexes = Object.fromEntries(
      [...value].flatMap(([policy, names]) =>
        [...names.keys()].sort().map((name, index) => [policy + name, index]),
      ),
    );
    depositAssetCount = Object.keys(candidates[0]!.assets).filter(
      (unit) =>
        unit !== "lovelace" &&
        unit !== depositPolicyId + deposit.event_asset_name,
    ).length;
    const effect = deriveCanonicalDepositTransitionEffect({
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
          : Buffer.from(Data.to(source.value.l2_datum), "hex"),
      l1Assets: candidates[0]!.assets,
      depositPolicyId,
      depositAssetNameHex: deposit.event_asset_name,
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
      proofCbor: Data.to(input.proof, SDK.TransitionFaultProof),
      references: input.additionalReferenceInputs,
    },
    (_key, value: unknown) =>
      typeof value === "bigint" ? value.toString() : value,
  );
  if (cachedYieldData?.key !== key)
    cachedYieldData = { key, value: deriveTransitionTraceYieldData(input) };
  return structuredClone(cachedYieldData.value);
};
