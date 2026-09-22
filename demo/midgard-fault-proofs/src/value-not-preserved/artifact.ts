import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeHash28,
  computeMidgardNativeTxId,
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardTxOutput,
  deriveMidgardNativeTxProofSource,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import { parseSubmitStep01TxInclusion } from "../submit-step-01.js";
import { nativeTxFromCoreCompact } from "../submit-step-01.js";
import { commitCountedRoot } from "../transition-trace/phas.js";
import {
  type JournalJsonObject,
  normalizeJournalJson,
} from "../workflow/journal.js";
import type { ValueNotPreservedContracts } from "./contracts.js";
import { flattenMidgardValueAssets } from "./evidence.js";
import {
  conservationAcceptedSource,
  conservationForcedSource,
} from "./source-plan.js";
import { planConservationFold } from "./union-plan.js";
import { ConservationClaim } from "./union-schemas.js";

export const VALUE_CONSERVATION_ARTIFACT =
  "midgard-value-conservation-artifact-v1";
export type ValueConservationArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof VALUE_CONSERVATION_ARTIFACT;
    headerCbor: string;
    transactionCbor: string;
    claimCbor: string;
    forcedMembershipCbor: string | null;
    acceptedSourceCbor: string | null;
    acceptedPhasRoot: string | null;
    acceptedProofCbor: string | null;
    eventCbor: string;
    transitionCbor: string;
    inputs: readonly (JournalJsonObject & {
      readonly outputCbor: string;
      readonly proofCbor: string;
    })[];
  }>;

const proofSteps = (proof: SDK.Proof) =>
  proof.map((step) =>
    "Branch" in step
      ? {
          type: "branch" as const,
          skip: Number(step.Branch.skip),
          neighbors: step.Branch.neighbors,
        }
      : "Fork" in step
        ? {
            type: "fork" as const,
            skip: Number(step.Fork.skip),
            neighbor: {
              nibble: Number(step.Fork.neighbor.nibble),
              prefix: step.Fork.neighbor.prefix,
              root: step.Fork.neighbor.root,
            },
          }
        : {
            type: "leaf" as const,
            skip: Number(step.Leaf.skip),
            neighbor: { key: step.Leaf.key, value: step.Leaf.value },
          },
  );
const verifyMpf = (
  root: string,
  key: Buffer,
  value: Buffer,
  proof: SDK.Proof,
) => {
  if (
    proof.length > 64 ||
    MpfProof.fromJSON(key, value, proofSteps(proof))
      .verify(true)
      ?.toString("hex") !== root
  )
    throw new Error("value conservation: invalid authenticated membership");
};
const decode = <T>(cbor: string, schema: T): T => {
  const value = Data.from(cbor, schema);
  if (Data.to(value as never, schema as never) !== cbor)
    throw new Error("value conservation: noncanonical artifact encoding");
  return value;
};
const counted = async <K, V>(
  membership: SDK.RootMembershipProof<K, V>,
  domain: SDK.RootDomain,
  root: string,
  count: bigint,
  key: Buffer,
  value: Buffer,
) => {
  if (
    membership.domain !== domain ||
    membership.root !== root ||
    membership.count !== count ||
    count <= 0n ||
    (await commitCountedRoot({
      domain,
      phasRoot: membership.phas_root,
      count,
    })) !== root
  )
    throw new Error("value conservation: counted root identity differs");
  verifyMpf(membership.phas_root, key, value, membership.proof);
};

/** Re-verifies retained bytes, all counted roots and every input value on restart. */
export const admitValueConservationArtifact = async (
  value: JournalJsonObject,
  contracts: ValueNotPreservedContracts,
) => {
  const keys = [
    "schemaVersion",
    "headerCbor",
    "transactionCbor",
    "claimCbor",
    "forcedMembershipCbor",
    "acceptedSourceCbor",
    "acceptedPhasRoot",
    "acceptedProofCbor",
    "eventCbor",
    "transitionCbor",
    "inputs",
  ];
  if (
    Object.keys(value).sort().join() !== keys.sort().join() ||
    value.schemaVersion !== VALUE_CONSERVATION_ARTIFACT
  )
    throw new Error("value conservation: malformed artifact");
  for (const key of [
    "headerCbor",
    "transactionCbor",
    "claimCbor",
    "eventCbor",
    "transitionCbor",
  ])
    if (
      typeof value[key] !== "string" ||
      !/^(?:[0-9a-f]{2})+$/u.test(value[key])
    )
      throw new Error("value conservation: malformed artifact bytes");
  for (const key of [
    "forcedMembershipCbor",
    "acceptedSourceCbor",
    "acceptedPhasRoot",
    "acceptedProofCbor",
  ])
    if (
      value[key] !== null &&
      (typeof value[key] !== "string" ||
        !/^(?:[0-9a-f]{2})+$/u.test(value[key]))
    )
      throw new Error("value conservation: malformed source bytes");
  if (
    !Array.isArray(value.inputs) ||
    value.inputs.some(
      (input) =>
        input === null ||
        typeof input !== "object" ||
        Array.isArray(input) ||
        Object.keys(input).sort().join() !== "outputCbor,proofCbor" ||
        typeof input.outputCbor !== "string" ||
        typeof input.proofCbor !== "string" ||
        !/^(?:[0-9a-f]{2})+$/u.test(input.outputCbor) ||
        !/^(?:[0-9a-f]{2})+$/u.test(input.proofCbor),
    )
  )
    throw new Error("value conservation: malformed spent-input artifact");
  const artifact = normalizeJournalJson(value) as ValueConservationArtifact;
  const header = decode(artifact.headerCbor, SDK.Header);
  const claim = decode(artifact.claimCbor, ConservationClaim);
  const transaction = (
    claim === "ForcedConservation"
      ? decodeMidgardForcedTxFullFromCanonicalCbor
      : decodeMidgardNativeTxFullFromCanonicalCbor
  )(Buffer.from(artifact.transactionCbor, "hex"));
  const transactionId = computeMidgardNativeTxId(transaction).toString("hex");
  let accepted: ReturnType<typeof parseSubmitStep01TxInclusion> | undefined;
  let preparedSource: ReturnType<typeof conservationAcceptedSource>;
  let nativeTxCompactCbor: string;
  if (artifact.forcedMembershipCbor !== null) {
    if (
      claim !== "ForcedConservation" ||
      artifact.acceptedSourceCbor !== null ||
      artifact.acceptedPhasRoot !== null ||
      artifact.acceptedProofCbor !== null
    )
      throw new Error("value conservation: mixed source directions");
    const membership = decode(
      artifact.forcedMembershipCbor,
      SDK.ForcedTransactionSourceMembershipProof,
    );
    await counted(
      membership,
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      header.forcedTransactionsRoot,
      header.forcedTransactionCount,
      Buffer.from(Data.to(membership.key, SDK.OutputReference), "hex"),
      Buffer.from(Data.to(membership.value, SDK.ForcedInclusionTxV1), "hex"),
    );
    const prepared = conservationForcedSource({
      header,
      membership,
      transactionCbor: artifact.transactionCbor,
    });
    preparedSource = prepared;
    nativeTxCompactCbor = prepared.nativeTxCompactCbor;
  } else {
    const nativeTransaction = decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(artifact.transactionCbor, "hex"),
    );
    if (
      claim === "ForcedConservation" ||
      artifact.acceptedSourceCbor === null ||
      artifact.acceptedPhasRoot === null ||
      artifact.acceptedProofCbor === null ||
      nativeTransaction.validity !== "TxIsValid"
    )
      throw new Error("value conservation: malformed accepted direction");
    const leaf = decode(artifact.acceptedSourceCbor, SDK.L2TransactionSource);
    const proof = decode(artifact.acceptedProofCbor, SDK.Proof);
    const expected = deriveMidgardNativeTxProofSource(nativeTransaction);
    if (
      header.l2TransactionCount <= 0n ||
      leaf.tx_id !== transactionId ||
      leaf.source.compact_cbor !== expected.compactCbor.toString("hex") ||
      leaf.source.witness_set_compact_cbor !==
        expected.witnessSetCompactCbor.toString("hex") ||
      leaf.source.field_preimage_lengths_cbor !==
        expected.fieldPreimageLengthsCbor.toString("hex") ||
      (await commitCountedRoot({
        domain: SDK.ROOT_DOMAINS.transactionsV1,
        phasRoot: artifact.acceptedPhasRoot,
        count: header.l2TransactionCount,
      })) !== header.transactionsRoot
    )
      throw new Error("value conservation: accepted source identity differs");
    verifyMpf(
      artifact.acceptedPhasRoot,
      Buffer.from(transactionId, "hex"),
      Buffer.from(artifact.acceptedSourceCbor, "hex"),
      proof,
    );
    nativeTxCompactCbor = leaf.source.compact_cbor;
    accepted = parseSubmitStep01TxInclusion({
      nativeTxId: transactionId,
      nativeTx: nativeTxFromCoreCompact(nativeTransaction.compact),
      nativeTxCompactCbor,
      l2TransactionSourceCbor: artifact.acceptedSourceCbor,
      transactionsPhasRoot: artifact.acceptedPhasRoot,
      txMembershipProofCbor: artifact.acceptedProofCbor,
    });
    preparedSource = conservationAcceptedSource({
      header,
      transactionId,
      fee: transaction.body.fee,
      claim,
    });
  }
  const eventMembership = decode(
    artifact.eventCbor,
    SDK.EventToStepMembershipProof,
  );
  const traceMembership = decode(
    artifact.transitionCbor,
    SDK.IndexedTraceProof,
  );
  await counted(
    eventMembership,
    SDK.ROOT_DOMAINS.eventToStep,
    header.eventToStepRoot,
    header.totalEventCount,
    Buffer.from(Data.to(eventMembership.key, SDK.EventKey), "hex"),
    Buffer.from(Data.to(eventMembership.value, SDK.EventToStepValue), "hex"),
  );
  await counted(
    traceMembership,
    SDK.ROOT_DOMAINS.transitionTrace,
    header.transitionTraceRoot,
    header.transitionStepCount,
    Buffer.from(Data.to(traceMembership.key), "hex"),
    Buffer.from(Data.to(traceMembership.value, SDK.TransitionStep), "hex"),
  );
  const phase =
    claim === "ForcedConservation" ? "ForcedTransaction" : "L2Transaction";
  if (
    Data.to(eventMembership.key, SDK.EventKey) !==
      Data.to(preparedSource.source.event_key, SDK.EventKey) ||
    eventMembership.value.phase !== phase ||
    eventMembership.value.step_index !== traceMembership.key ||
    traceMembership.value.step_index !== traceMembership.key ||
    traceMembership.value.schema_version !== 1n ||
    traceMembership.value.phase !== phase ||
    Data.to(traceMembership.value.event_key, SDK.EventKey) !==
      Data.to(eventMembership.key, SDK.EventKey) ||
    traceMembership.key < 0n ||
    traceMembership.key >= header.transitionStepCount
  )
    throw new Error("value conservation: event coordinate differs");
  const fields = {
    0: transaction.body.spendInputsPreimageCbor.toString("hex"),
    2: transaction.body.outputsPreimageCbor.toString("hex"),
    5: transaction.body.mintPreimageCbor.toString("hex"),
  };
  const inputItems = decodeMidgardFieldPreimage(
    transaction.body.spendInputsPreimageCbor,
  );
  if (inputItems.length !== artifact.inputs.length)
    throw new Error("value conservation: omitted spent input");
  const spentInputs = artifact.inputs.map((input, index) => {
    const key = inputItems[index]!;
    const outputCbor = Buffer.from(input.outputCbor, "hex");
    const material = buildCanonicalMidgardLedgerEntryOutputMaterial({
      outRef: key,
      outputCbor,
    });
    const proof = decode(input.proofCbor, SDK.Proof);
    verifyMpf(
      traceMembership.value.pre_utxos_root,
      key,
      material.descriptorCbor,
      proof,
    );
    return {
      descriptorCbor: material.descriptorCbor.toString("hex"),
      proof,
      assets: flattenMidgardValueAssets(
        decodeMidgardTxOutput(outputCbor).value,
      ),
    };
  });
  const plan = await planConservationFold({
    contracts,
    source: preparedSource.source,
    eventMembership,
    traceMembership,
    nativeTxCompactCbor,
    fields,
    spentInputs,
  });
  return {
    artifact,
    header,
    headerHash: computeHash28(SDK.encodeHeaderCbor(header)).toString("hex"),
    source: preparedSource.source,
    nativeTxCompactCbor,
    fields,
    actions: [...preparedSource.actions, ...plan.actions],
    ...(accepted === undefined ? {} : { accepted }),
  };
};
