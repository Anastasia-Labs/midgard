import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import { computeHash28 } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { commitCountedRoot } from "../transition-trace/phas.js";
import {
  type JournalJsonObject,
  normalizeJournalJson,
} from "../workflow/journal.js";
import {
  type NonExistentInputForcedSource,
  nonExistentInputForcedSourceMaterial,
  type PreparedNonExistentInputWrongfulRejection,
} from "./wrongful-rejection.js";
const proofSteps = (proof: SDK.Proof) =>
  proof.map((step) => {
    if ("Branch" in step) {
      return {
        type: "branch" as const,
        skip: Number(step.Branch.skip),
        neighbors: step.Branch.neighbors,
      };
    }
    if ("Fork" in step) {
      return {
        type: "fork" as const,
        skip: Number(step.Fork.skip),
        neighbor: {
          nibble: Number(step.Fork.neighbor.nibble),
          prefix: step.Fork.neighbor.prefix,
          root: step.Fork.neighbor.root,
        },
      };
    }
    return {
      type: "leaf" as const,
      skip: Number(step.Leaf.skip),
      neighbor: { key: step.Leaf.key, value: step.Leaf.value },
    };
  });

const verifyMpf = ({
  root,
  key,
  value,
  proof,
  membership,
  label,
}: {
  readonly root: string;
  readonly key: Buffer;
  readonly value?: Buffer;
  readonly proof: SDK.Proof;
  readonly membership: boolean;
  readonly label: string;
}): void => {
  const actual = MpfProof.fromJSON(key, value, proofSteps(proof)).verify(
    membership,
  );
  const actualHex =
    actual === null
      ? SDK.EMPTY_MERKLE_TREE_ROOT
      : Buffer.from(actual).toString("hex");
  if (actualHex !== root) {
    throw new Error(`${label} does not open its authenticated MPF root`);
  }
};

export const NON_EXISTENT_INPUT_FORCED_ARTIFACT =
  "midgard-non-existent-input-forced-artifact-v1";
export const nonExistentInputForcedArtifact = (
  prepared: PreparedNonExistentInputWrongfulRejection,
): JournalJsonObject =>
  normalizeJournalJson({
    schemaVersion: NON_EXISTENT_INPUT_FORCED_ARTIFACT,
    headerHash: prepared.headerHash,
    source: Data.to(
      prepared.forcedSource as never,
      SDK.NonExistentInputForcedSourcePayloadSchema as never,
    ),
    transaction: prepared.fullTransactionCbor,
    event: Data.to(prepared.eventMembership, SDK.EventToStepMembershipProof),
    transition: Data.to(prepared.transitionMembership, SDK.IndexedTraceProof),
    ledger:
      prepared.ledgerMembership === null
        ? null
        : {
            value: prepared.ledgerMembership.value,
            proof: Data.to(prepared.ledgerMembership.proof, SDK.Proof),
          },
  }) as JournalJsonObject;

/** Revalidate all committed evidence on every durable admission. */
export const admitNonExistentInputForcedArtifact = async (
  input: unknown,
): Promise<PreparedNonExistentInputWrongfulRejection> => {
  if (typeof input !== "object" || input === null || Array.isArray(input))
    throw new Error("nonExistentInput: malformed artifact");
  const artifact = input as Record<string, unknown>;
  if (
    Object.keys(artifact).sort().join(",") !==
      "event,headerHash,ledger,schemaVersion,source,transaction,transition" ||
    artifact.schemaVersion !== NON_EXISTENT_INPUT_FORCED_ARTIFACT
  )
    throw new Error("nonExistentInput: artifact shape changed");
  const hex = (key: string) => {
    const value = artifact[key];
    if (typeof value !== "string" || !/^(?:[0-9a-f]{2})+$/u.test(value))
      throw new Error(`nonExistentInput: malformed ${key}`);
    return value;
  };
  const forcedSource = Data.from(
    hex("source"),
    SDK.NonExistentInputForcedSourcePayloadSchema as never,
  ) as NonExistentInputForcedSource;
  const { header, membership } = forcedSource;
  const headerHash = computeHash28(SDK.encodeHeaderCbor(header)).toString(
    "hex",
  );
  if (headerHash !== hex("headerHash") || header.protocolVersion !== 1n)
    throw new Error("nonExistentInput: header changed");
  const material = nonExistentInputForcedSourceMaterial(
    forcedSource,
    hex("transaction"),
  );
  const eventMembership = Data.from(
    hex("event"),
    SDK.EventToStepMembershipProof,
  );
  const transitionMembership = Data.from(
    hex("transition"),
    SDK.IndexedTraceProof,
  );
  const counted = async <K, V>(
    witness: SDK.RootMembershipProof<K, V>,
    domain: SDK.RootDomain,
    root: string,
    count: bigint,
    key: string,
    value: string,
  ) => {
    if (
      witness.domain !== domain ||
      witness.root !== root ||
      witness.count !== count ||
      count <= 0n ||
      (await commitCountedRoot({
        domain,
        phasRoot: witness.phas_root,
        count,
      })) !== root
    )
      throw new Error("nonExistentInput: counted root changed");
    verifyMpf({
      root: witness.phas_root,
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
      proof: witness.proof,
      membership: true,
      label: "nonExistentInput",
    });
  };
  await counted(
    membership,
    SDK.ROOT_DOMAINS.forcedTransactionsV1,
    header.forcedTransactionsRoot,
    header.forcedTransactionCount,
    Data.to(membership.key, SDK.OutputReference),
    Data.to(membership.value, SDK.ForcedInclusionTxV1),
  );
  await counted(
    eventMembership,
    SDK.ROOT_DOMAINS.eventToStep,
    header.eventToStepRoot,
    header.totalEventCount,
    Data.to(eventMembership.key, SDK.EventKey),
    Data.to(eventMembership.value, SDK.EventToStepValue),
  );
  await counted(
    transitionMembership,
    SDK.ROOT_DOMAINS.transitionTrace,
    header.transitionTraceRoot,
    header.transitionStepCount,
    Data.to(transitionMembership.key),
    Data.to(transitionMembership.value, SDK.TransitionStep),
  );
  const expectedEvent: SDK.EventKey = {
    ForcedTransactionEventKey: { tx_order_id: membership.key },
  };
  const transition = transitionMembership.value;
  if (
    Data.to(eventMembership.key, SDK.EventKey) !==
      Data.to(expectedEvent, SDK.EventKey) ||
    Data.to(transition.event_key, SDK.EventKey) !==
      Data.to(expectedEvent, SDK.EventKey) ||
    eventMembership.value.phase !== "ForcedTransaction" ||
    transition.phase !== "ForcedTransaction" ||
    transition.schema_version !== 1n ||
    transition.step_index !== transitionMembership.key ||
    transition.step_index !== eventMembership.value.step_index ||
    transition.step_index < 0n ||
    transition.step_index >= header.transitionStepCount
  )
    throw new Error("nonExistentInput: event/transition coordinate changed");
  let ledgerMembership: PreparedNonExistentInputWrongfulRejection["ledgerMembership"] =
    null;
  if (material.selectedInput === null) {
    if (artifact.ledger !== null)
      throw new Error(
        "nonExistentInput: impossible index carries unrelated witness",
      );
  } else {
    if (
      typeof artifact.ledger !== "object" ||
      artifact.ledger === null ||
      Array.isArray(artifact.ledger)
    )
      throw new Error("nonExistentInput: ledger membership absent");
    const ledger = artifact.ledger as Record<string, unknown>;
    if (
      Object.keys(ledger).sort().join(",") !== "proof,value" ||
      typeof ledger.value !== "string" ||
      !/^(?:[0-9a-f]{2})+$/u.test(ledger.value) ||
      typeof ledger.proof !== "string" ||
      !/^(?:[0-9a-f]{2})+$/u.test(ledger.proof)
    )
      throw new Error("nonExistentInput: malformed ledger witness");
    ledgerMembership = {
      value: ledger.value,
      proof: Data.from(ledger.proof, SDK.Proof),
    };
    verifyMpf({
      root: transition.pre_utxos_root,
      key: material.inputItems[Number(material.inputIndex)]!,
      value: Buffer.from(ledger.value, "hex"),
      proof: ledgerMembership.proof,
      membership: true,
      label: "nonExistentInput ledger",
    });
  }
  return {
    headerHash,
    forcedSource,
    fullTransactionCbor: hex("transaction"),
    ...material,
    eventMembership,
    transitionMembership,
    ledgerMembership,
  };
};
