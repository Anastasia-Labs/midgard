import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { asLucidSchema } from "@al-ft/midgard-core/lucid-data";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type DaPayload,
  type DaPayloadEntry,
  encodeDaPayload,
  type EventKey,
  EventKeySchema,
  hashBlockHeader,
  type RejectionReason,
  ROOT_DOMAINS,
  TransitionStep,
  ValidationTraceDescriptorSchema,
} from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  placeholderValidationTraceDescriptorV1,
  validationTransactionCommitmentV1,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildCountedRoot,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import { buildCanonicalBlockFixture } from "../helpers/canonical-block-evidence-fixture.js";
import { recordFixtureLedger } from "./fixture-ledger.js";
import { buildDecodingBlockFixture } from "./native-script-decoding-emulator.js";

/**
 * The validation-trace descriptor entry an operator commits for one event under
 * its exact claimed verdict: the P1 placeholder descriptor, derived from the
 * event key, the transaction's proof-source commitment and the verdict.
 */
export const placeholderValidationTraceEntries = ({
  eventKey,
  canonicalTransactionCbor,
  sourceKind,
  claim,
}: {
  readonly eventKey: EventKey;
  /** The canonical normal or forced bytes the event commits. */
  readonly canonicalTransactionCbor: Uint8Array;
  readonly sourceKind: "normal" | "forced";
  readonly claim:
    | { readonly verdict: "accepted" }
    | { readonly verdict: "rejected"; readonly reason: RejectionReason };
}) => {
  const eventKeyCbor = Buffer.from(
    Data.to(eventKey, asLucidSchema(EventKeySchema)),
    "hex",
  );
  const descriptor = placeholderValidationTraceDescriptorV1(
    {
      eventKeyCbor,
      transactionCommitment: validationTransactionCommitmentV1({
        canonicalTransactionCbor,
        sourceKind,
      }),
    },
    claim.verdict,
    claim.verdict === "accepted" ? null : claim.reason,
  );
  return {
    descriptorEntries: [
      {
        key: eventKeyCbor,
        value: Buffer.from(
          Data.to(
            SDK.validationTraceDescriptorDataFromCore(descriptor),
            asLucidSchema(ValidationTraceDescriptorSchema),
          ),
          "hex",
        ),
      },
    ],
  };
};

/** Commits existing small validation fixtures into the same DA/header boundary as classification. */
export const buildRetainedValidationBlockFixture = async ({
  subject,
  priorLedgerRoot,
  descriptorEntries,
  blockEndTimeMs,
  blockStartTimeMs = blockEndTimeMs - 60_000,
  blockSlot = 100n,
  minFeeA = 0n,
  minFeeB = 0n,
  operatorVkey = "b1".repeat(28),
  prevHeaderHash,
  postLedgerEntries,
}: {
  readonly subject: Parameters<typeof buildDecodingBlockFixture>[0]["subject"];
  readonly priorLedgerRoot: string;
  readonly descriptorEntries?: readonly { key: Buffer; value: Buffer }[];
  readonly blockEndTimeMs: number;
  readonly blockStartTimeMs?: number;
  readonly blockSlot?: bigint;
  readonly minFeeA?: bigint;
  readonly minFeeB?: bigint;
  readonly operatorVkey?: string;
  readonly prevHeaderHash?: string;
  readonly postLedgerEntries?: readonly { outRef: Buffer; output: Buffer }[];
}) => {
  const base = await buildDecodingBlockFixture({
    subject,
    priorLedgerRoot,
    operatorVkey,
    startTime: BigInt(blockStartTimeMs),
  });
  const descriptors =
    descriptorEntries ??
    base.reconstruction.payload.block_body.validation_traces.map(
      ([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      }),
    );
  const root = await buildCountedRoot(
    ROOT_DOMAINS.validationTraces,
    descriptors,
  );
  const postLedger =
    postLedgerEntries === undefined
      ? undefined
      : await keyValuePhasRootWithCount(
          postLedgerEntries.map(({ outRef, output }) => ({
            key: outRef,
            value: buildCanonicalMidgardLedgerEntryOutputMaterial({
              outRef,
              outputCbor: output,
            }).descriptorCbor,
          })),
        );
  const transitionEntries = base.reconstruction.transitionTrace.map(
    (entry) => ({
      key: entry.keyBytes,
      value: Buffer.from(
        Data.to(
          {
            ...entry.value,
            ...(postLedger === undefined
              ? {}
              : { post_utxos_root: postLedger.root }),
          },
          TransitionStep,
        ),
        "hex",
      ),
    }),
  );
  const transitionRoot = await buildCountedRoot(
    ROOT_DOMAINS.transitionTrace,
    transitionEntries,
  );
  const header = {
    ...base.header,
    endTime: BigInt(blockEndTimeMs),
    blockSlot,
    minFeeA,
    minFeeB,
    prevUtxosRoot: priorLedgerRoot,
    ...(prevHeaderHash === undefined ? {} : { prevHeaderHash }),
    // The header pins the validation-trace pair; the body keeps its traces.
    validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    validationTraceCount: 0n,
    transitionTraceRoot: transitionRoot.root,
    ...(postLedger === undefined ? {} : { utxosRoot: postLedger.root }),
  };
  if (postLedger !== undefined && postLedgerEntries !== undefined)
    recordFixtureLedger(
      postLedger.root,
      postLedgerEntries.map(({ outRef, output }) => ({
        key: outRef,
        value: output,
      })),
    );
  const headerHash = await Effect.runPromise(hashBlockHeader(header));
  const entries = (
    values: readonly { key: Buffer; value: Buffer }[],
  ): DaPayloadEntry[] =>
    [...values]
      .sort((a, b) => Buffer.compare(a.key, b.key))
      .map(({ key, value }) => [key.toString("hex"), value.toString("hex")]);
  const payload: DaPayload = {
    ...base.reconstruction.payload,
    block_body: {
      ...base.reconstruction.payload.block_body,
      header,
      header_hash: headerHash,
      validation_traces: entries(descriptors),
      validation_trace_witnesses: [],
      transition_trace: entries(transitionEntries),
      counts: {
        ...base.reconstruction.payload.block_body.counts,
        validationTraceCount: root.count,
      },
    },
  };
  return {
    header,
    headerHash,
    payloadEnvelopeCbor: await wrapDaPayload(encodeDaPayload(payload), {
      mode: "identity",
    }),
  };
};

export type RetainedPlutusFixtureOptions = Readonly<{
  sourceKind?: "normal" | "forced";
  /** Reuse the same identity program against an actually deposited ledger entry. */
  ledgerInput?: Readonly<{ outRef: Buffer; output: Buffer }>;
  predecessor?: Pick<
    Awaited<ReturnType<typeof buildCanonicalBlockFixture>>,
    "header" | "headerHash" | "payloadEnvelopeCbor"
  >;
  orderKey?: SDK.OutputReference;
  operatorVkey?: string;
  blockStartTimeMs?: number;
  blockEndTimeMs?: number;
  blockSlot?: bigint;
  /** The disputed transaction's native validity interval, in slots. */
  validityInterval?: Readonly<{ start: bigint; end: bigint }>;
  /** Carry the program as the created output's reference script. */
  outputReferenceScript?: boolean;
  /** Further field-6 items after the program's own witness. */
  extraScriptWitnesses?: readonly Buffer[];
  /**
   * The default predecessor's header operator and times, so it can be
   * committed as an ordinary first block on an emulator state queue.
   */
  predecessorFrame?: Readonly<{
    operatorVkey: string;
    startTime: bigint;
    endTime: bigint;
  }>;
}>;
