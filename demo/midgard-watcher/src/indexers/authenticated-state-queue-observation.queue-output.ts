import { computeHash28 } from "@al-ft/midgard-core/codec/hash";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput, Data } from "@lucid-evolution/lucid";

import {
  type DecodedQueueHeader,
  HEX_28,
  type LockOutput,
  type QueueOutput,
} from "./authenticated-state-queue-observation.parse-persisted-header.js";

export const queueOutput = ({
  output,
  outRef,
  stateQueueAddress,
  stateQueuePolicyId,
}: {
  output: CML.TransactionOutput;
  outRef: string;
  stateQueueAddress: string;
  stateQueuePolicyId: string;
}): QueueOutput | null => {
  const core = coreToTxOutput(output);
  const assets = Object.entries(core.assets).filter(
    ([unit, quantity]) =>
      unit.startsWith(stateQueuePolicyId) && quantity !== 0n,
  );
  if (assets.length === 0) return null;
  if (
    core.address !== stateQueueAddress ||
    assets.length !== 1 ||
    assets[0]![1] !== 1n ||
    output.script_ref() !== undefined
  ) {
    throw new Error(
      "state-queue policy output has invalid address/value/script topology",
    );
  }
  const assetName = assets[0]![0].slice(stateQueuePolicyId.length);
  const headerHash =
    assetName === SDK.STATE_QUEUE_ROOT_ASSET_NAME
      ? null
      : assetName.startsWith(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX) &&
          HEX_28.test(
            assetName.slice(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length),
          )
        ? assetName.slice(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length)
        : undefined;
  const datum = output.datum()?.as_datum();
  if (headerHash === undefined || datum === undefined) {
    throw new Error(
      "state-queue output has an unknown asset or non-inline datum",
    );
  }
  const linkedListDatumCborHex = datum.to_canonical_cbor_hex();
  const view = SDK.linkedListDatumToNodeView(
    Data.from(linkedListDatumCborHex, SDK.LinkedListDatum),
    assetName,
  );
  const datumHeaderHash = view.key === "Empty" ? null : view.key.Key.key;
  if (datumHeaderHash !== headerHash) {
    throw new Error("state-queue output asset and datum identities differ");
  }
  let header: DecodedQueueHeader | null = null;
  if (headerHash !== null) {
    const stateQueueNode = Data.castFrom(
      view.data,
      SDK.StateQueueNode,
    ) as SDK.StateQueueNode;
    const stateQueueNodeCborHex = Data.to(stateQueueNode, SDK.StateQueueNode);
    const headerCborHex = Data.to(stateQueueNode.header, SDK.Header);
    const computedHeaderHash = computeHash28(
      Buffer.from(headerCborHex, "hex"),
    ).toString("hex");
    if (computedHeaderHash !== headerHash) {
      throw new Error(
        "state-queue header bytes or DA-attestation identity differ from the node asset",
      );
    }
    header = Object.freeze({
      headerHash,
      headerCborHex,
      stateQueueNodeCborHex,
      linkedListDatumCborHex,
      daAvailability: stateQueueNode.da_attestation,
    });
  }
  return Object.freeze({
    node: Object.freeze({ headerHash, outRef }),
    nextHeaderHash: view.next === "Empty" ? null : view.next.Key.key,
    header,
  });
};

export const lockOutput = ({
  output,
  outRef,
  correctionLockAddress,
  hubOraclePolicyId,
}: {
  output: CML.TransactionOutput;
  outRef: string;
  correctionLockAddress: string;
  hubOraclePolicyId: string;
}): LockOutput | null => {
  const core = coreToTxOutput(output);
  const lockUnit = SDK.correctionLockUnit(hubOraclePolicyId);
  if ((core.assets[lockUnit] ?? 0n) === 0n) return null;
  const nonAda = Object.entries(core.assets).filter(
    ([unit, quantity]) => unit !== "lovelace" && quantity !== 0n,
  );
  const datum = output.datum()?.as_datum();
  if (
    core.address !== correctionLockAddress ||
    nonAda.length !== 1 ||
    nonAda[0]![0] !== lockUnit ||
    nonAda[0]![1] !== 1n ||
    output.script_ref() !== undefined ||
    datum === undefined
  ) {
    throw new Error(
      "CorrectionLock output has invalid address/value/datum topology",
    );
  }
  const parsed = SDK.parseStateQueueCorrectionLockDatum(
    Data.from(datum.to_canonical_cbor_hex(), SDK.CorrectionLockDatum),
  );
  if (parsed === null) throw new Error("CorrectionLock datum is non-canonical");
  return Object.freeze({ outRef, datum: parsed });
};

export const decodedQueueOutputs = ({
  body,
  transactionHash,
  stateQueueAddress,
  stateQueuePolicyId,
}: {
  body: CML.TransactionBody;
  transactionHash: string;
  stateQueueAddress: string;
  stateQueuePolicyId: string;
}): readonly QueueOutput[] => {
  const result: QueueOutput[] = [];
  const outputs = body.outputs();
  for (let index = 0; index < outputs.len(); index += 1) {
    const decoded = queueOutput({
      output: outputs.get(index),
      outRef: `${transactionHash}#${index.toString()}`,
      stateQueueAddress,
      stateQueuePolicyId,
    });
    if (decoded !== null) result.push(decoded);
  }
  if (
    new Set(result.map(({ node }) => node.headerHash)).size !== result.length ||
    new Set(result.map(({ node }) => node.outRef)).size !== result.length
  ) {
    throw new Error("state-queue transaction produced duplicate identities");
  }
  return result;
};
